;======================================================================
;
function get_pfilter_script_cmd, script_file, p_stat
compile_opt idl2

;Here we will select and open a script file to getr input files etc
;
status=0
n_case=0
Log_File=''
Run_Desc=''
pulse_min_index=-1
pulse_max_index=-1
b_thresh=0.0
r_thresh=0.0
save_br=0b
exit_idl=0b
delete_input=0b
run_type='[pfilter]'

;select the file using envi_pickfile
;script_file=envi_pickfile(title='Select the Script File to Process')
if (script_file eq '') then begin
  print,'No script file selected in envi_pickfile'
  status=1
  goto,go_back
endif

;test if file exists (very protective)
if (~file_test(script_file)) then begin
  print,'Script file entered does not exist'
  print,'File name: '+strtrim(script_file,2)
  status=2
  goto,go_back
endif
;open the script file
openr,inunit,script_file,/get_lun,error=err
if (err ne 0) then begin
  print,'Error opening the script file'
  print,'File name: '+strtrim(script_file,2)
  print,!error_state.msg
  status=2
  goto,go_back
endif

if(eof(inunit)) then begin
  print,'The script file is empty!'
  print,'File name: '+strtrim(script_file,2)
  status=3
  goto,go_back
endif

;read the record into a string array
incount=0
inbuf=['']
accum=''

reading:
if (eof(inunit)) then goto,all_read
readf,inunit,inbuf,format='(a)'

print,strtrim(inbuf,2)
;do a first trim for leading & trailing blanks
inbuf=strtrim(inbuf,2)
if (strlen(inbuf) eq 0) then goto,reading

;strip off any comments, headers etc
result=strpos(inbuf,';')
if (result gt -1) then begin
  if (result eq 0) then goto,reading
  inbuf=strtrim(strmid(inbuf,0,result),2)
  if (strlen(inbuf) eq 0) then goto,reading
endif

;now see if there is a continuation of records in the one primary statement
if (strmid(inbuf,strlen(inbuf)-1,1) eq '$') then begin
  accum=accum+strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
  inbuf=accum
  goto,reading
endif else begin
  if (accum ne '') then begin
    inbuf=strtrim(accum+inbuf,2)
  endif
  accum=''
endelse

;now add the assembled record to the list
if(incount le 0) then begin
  scr_text=[inbuf]
endif else begin
  scr_text=[scr_text,inbuf]
endelse
incount=incount+1

if (eof(inunit)) then begin
  goto,all_read
endif else begin
  goto,reading
endelse

;all the records are now read in
all_read:
free_lun,inunit,/force

if (incount ne n_elements(scr_text)) then begin
  print,'Warning, read count and stored lines do not match'
endif

;set up some pointers
records=strarr(13)
records=['n_case','log_file','run_desc', $
         'datafile','ancfile','outfile', $
         'pulse_min_index','pulse_max_index','b_thresh', $
     'r_thresh','save_br','exit_idl','delete_input']
present=bytarr(13)
rec_pos=intarr(13)

script_type=strtrim(scr_text[0],2)

if (strtrim(strlowcase(script_type),2) ne strtrim(strlowcase(script_type),2)) then begin
  print, 'Warning! script type is not the same as run type'
  print, 'run type=',run_type
  print, 'script type=',script_type
endif  

for j=0,incount-1 do begin
  result=strpos(scr_text[j],'=')
  if (result gt 0) then begin
    test=strtrim(strlowcase(strmid(scr_text[j],0,result)),2)
    pos=where(records eq test,npos)
    if (npos eq 1) then begin
      present[pos]=1
      rec_pos[pos]=j
    endif
  endif
endfor

;next we need to extract the information from the records
;first get n_case and check it - return if not present or zero!
;then set up the arrays and then onto the others

;read n_case
n_case_char=''
if (not present[0]) then begin
  print,'n_case not present in the input script file !'
  status=4
  goto,go_back
endif else begin
  result=strpos(scr_text[rec_pos[0]],'=')
  if (result le 0) then begin
    print,'No = in n_pos ... bad input?'
    status=5
    goto,go_back
  endif
  n_case_char=strtrim(strmid(scr_text[rec_pos[0]],result+1),2)
  reads,n_case_char,n_case,format='(i4)'
endelse

;now the log file
log_file=''
if (present[1]) then begin
  result=strpos(scr_text[rec_pos[1]],'=')
  if (result le 0) then begin
    print,'No = in log_file record ... bad input?'
    status=5
    goto,go_back
  endif
  log_file=strtrim(strmid(scr_text[rec_pos[1]],result+1),2)
  if (strmid(log_file,0,1) eq "'") then begin
    log_file=strtrim(strmid(log_file,1,strlen(log_file)-1),2)
  endif
  if (strmid(log_file,strlen(log_file)-1,1) eq "'") then begin
    log_file=strtrim(strmid(log_file,0,strlen(log_file)-1),2)
  endif
endif

;now the run_desc
run_desc=''
if (present[2]) then begin
  result=strpos(scr_text[rec_pos[2]],'=')
  if (result le 0) then begin
    print,'No = in run_desc record ... bad input?'
    status=5
    goto,go_back
  endif
  run_desc=strtrim(strmid(scr_text[rec_pos[2]],result+1),2)
  if (strmid(run_desc,0,1) eq "'") then begin
    run_desc=strtrim(strmid(run_desc,1,strlen(run_desc)-1),2)
  endif
  if (strmid(run_desc,strlen(run_desc)-1,1) eq "'") then begin
    run_desc=strtrim(strmid(run_desc,0,strlen(run_desc)-1),2)
  endif
endif

;test for zero n_case
if (n_case le 0) then begin
  ;; ; Use this as a flag to select files manually
  ;; datafile = envi_pickfile(title='Select EVI files to filter')
  ;; ; Ancillary and Output filenames will be set to defaults, so ignore any that are
  ;; ; present in the batch file
  ;; present[4] = 0b
  ;; present[5] = 0b
  ;; n_case = n_elements(datafile)
   print, 'n_case is less than one!'
   GOTO, go_back
endif else begin
  ;n_case so set up the arrays
  datafile=strarr(n_case)

  ; in this case, get datafiles from the batchfile input
  if (present[3]) then begin
    result=strpos(scr_text[rec_pos[3]],'=')
    if (result le 0) then begin
      print,'No = in datafile list record ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[3]],result+1),2)
    if (strmid(inbuf,0,1) eq '[') then begin
      inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
    endif
    if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
      inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
    endif
    datafile=strtrim(strsplit(inbuf,',',/extract),2)
    if (n_elements(datafile) ne n_case) then begin
      print,'Number of input datafiles different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(datafile)
      status=6
      goto,go_back
    endif
    for j=0,n_case-1 do begin
      if (strmid(datafile[j],0,1) eq "'") then begin
        datafile[j]=strtrim(strmid(datafile[j],1,strlen(datafile[j])-1),2)
      endif
      if (strmid(datafile[j],strlen(datafile[j])-1,1) eq "'") then begin
        datafile[j]=strtrim(strmid(datafile[j],0,strlen(datafile[j])-1),2)
      endif
    endfor
  endif
endelse

; Set up arrays for other file lists
ancfile=strarr(n_case)
outfile=strarr(n_case)

;now the ancillary file
if (present[4]) then begin
  result=strpos(scr_text[rec_pos[4]],'=')
  if (result le 0) then begin
    print,'No = in ancillary file list record ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[4]],result+1),2)
  if (strmid(inbuf,0,1) eq '[') then begin
    inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
  endif
  if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
    inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
  endif
  ancfile=strtrim(strsplit(inbuf,',',/extract),2)
  if (n_elements(ancfile) ne n_case) then begin
    print,'Number of ancillary file names different from number of cases'
    print,'n_case= ',n_case,' n_elements= ',n_elements(ancfile)
    status=6
    goto,go_back
  endif
  for j=0,n_case-1 do begin
    if (strmid(ancfile[j],0,1) eq "'") then begin
      ancfile[j]=strtrim(strmid(ancfile[j],1,strlen(ancfile[j])-1),2)
    endif
    if (strmid(ancfile[j],strlen(ancfile[j])-1,1) eq "'") then begin
      ancfile[j]=strtrim(strmid(ancfile[j],0,strlen(ancfile[j])-1),2)
    endif
  endfor
endif

;now the output image(s)
if (present[5]) then begin
  result=strpos(scr_text[rec_pos[5]],'=')
  if (result le 0) then begin
    print,'No = in output image list record ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[5]],result+1),2)
  if (strmid(inbuf,0,1) eq '[') then begin
    inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
  endif
  if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
    inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
  endif
  outfile=strtrim(strsplit(inbuf,',',/extract),2)
  if (n_elements(outfile) ne n_case) then begin
    print,'Number of output images different from number of cases'
    print,'n_case= ',n_case,' n_elements= ',n_elements(outfile)
    status=6
    goto,go_back
  endif
  for j=0,n_case-1 do begin
    if (strmid(outfile[j],0,1) eq "'") then begin
      outfile[j]=strtrim(strmid(outfile[j],1,strlen(outfile[j])-1),2)
    endif
    if (strmid(outfile[j],strlen(outfile[j])-1,1) eq "'") then begin
      outfile[j]=strtrim(strmid(outfile[j],0,strlen(outfile[j])-1),2)
    endif
  endfor
endif

if (present[6]) then begin
  pulse_min_index=-1
  result=strpos(scr_text[rec_pos[6]],'=')
  if (result le 0) then begin
    print,'No = in pulse_min_index flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[6]],result+1),2)
  reads,inbuf,pulse_min_index,format='(i6)'
endif

if (present[7]) then begin
  pulse_max_index=-1
  result=strpos(scr_text[rec_pos[7]],'=')
  if (result le 0) then begin
    print,'No = in pulse_max_index flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[7]],result+1),2)
  reads,inbuf,pulse_max_index,format='(i6)'
endif

if (present[8]) then begin
  b_thresh=0.0
  result=strpos(scr_text[rec_pos[8]],'=')
  if (result le 0) then begin
    print,'No = in b_thresh flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[8]],result+1),2)
  reads,inbuf,b_thresh,format='(i6)'
endif

if (present[9]) then begin
  r_thresh=0.0
  result=strpos(scr_text[rec_pos[9]],'=')
  if (result le 0) then begin
    print,'No = in r_thresh flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[9]],result+1),2)
  reads,inbuf,r_thresh,format='(f5.3)'
endif

if (present[10]) then begin
  save_br=0b
  result=strpos(scr_text[rec_pos[10]],'=')
  if (result le 0) then begin
    print,'No = in save_br flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[10]],result+1),2)
  reads,inbuf,save_br,format='(i2)'
endif

if (present[11]) then begin
  exit_idl=0b
  result=strpos(scr_text[rec_pos[11]],'=')
  if (result le 0) then begin
    print,'No = in exit_idl flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[11]],result+1),2)
  reads,inbuf,exit_idl,format='(i2)'
endif

if (present[12]) then begin
  delete_input=0b
  result=strpos(scr_text[rec_pos[12]],'=')
  if (result le 0) then begin
    print,'No = in delete_input flag ... bad input?'
    status=5
    goto,go_back
  endif
  inbuf=strtrim(strmid(scr_text[rec_pos[12]],result+1),2)
  reads,inbuf,delete_input,format='(i2)'
endif

print,'Input script file information:'
print,' '

print,'n_case= ',n_case
if(present[1]) then begin
  print,'log file= ',log_file
endif else begin
  print,'Log File not present'
endelse
if(present[2]) then begin
  print,'Run Description= ',run_desc
endif else begin
  print,'Run Description not present'
endelse
if(present[3]) then begin
  print,'datafile List:'
  for j=0,n_case-1 do begin
    print,datafile[j]
  endfor
endif else begin
  print,'datafile List not present'
endelse
if(present[4]) then begin
  print,'Ancillary File List:'
  for j=0,n_case-1 do begin
    print,ancfile[j]
  endfor
endif else begin
  print,'Ancillary File List not present'
endelse
if(present[5]) then begin
  print,'Output Image List:'
  for j=0,n_case-1 do begin
    print,outfile[j]
  endfor
endif else begin
  print,'Output Image List not present'
endelse
if(present[6]) then begin
  print,'Pulse Min Index= ',pulse_min_index
endif else begin
  print,'Pulse Min Index Flag not present'
endelse
if(present[7]) then begin
  print,'Pulse Max Index= ',pulse_max_index
endif else begin
  print,'Pulse Max Index Flag not present'
endelse
if(present[8]) then begin
  print,'B Threshold= ',b_thresh
endif else begin
  print,'B Threshold Flag not present'
endelse
if(present[9]) then begin
  print,'R Threshold= ',r_thresh
endif else begin
  print,'R Threshold Flag not present'
endelse
if(present[10]) then begin
  print,'Save B & R= ',save_br
endif else begin
  print,'Save B & R Flag not present'
endelse
if(present[11]) then begin
  print,'Exit IDL= ',Exit_IDL
endif else begin
  print,'Exit IDL Flag not present'
endelse
if(present[12]) then begin
  print,'Delete_input= ',delete_input
endif else begin
  print,'Delete Input Flag not present'
endelse

sav={ $
     Script_File:Script_File,$
     n_case:n_case,$
     Run_Desc:Run_Desc,$
     script_type:script_type,$
     Log_File:Log_File,$
     datafile:datafile,$
     ancfile:ancfile,$
     outfile:outfile,$
     pulse_min_index:pulse_min_index,$
     pulse_max_index:pulse_max_index,$
     b_thresh:b_thresh,$
     r_thresh:r_thresh,$
     save_br:save_br,$
     exit_idl:exit_idl,$
     delete_input:delete_input $
     }

;now locate the data on the heap with a pointer
p_stat=ptr_new(sav,/no_copy)

go_back:
sav=0b
return, status

end

;-------------------------------

pro dwel_pfilter_batch_doit_cmd, script_file
compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window

RESOLVE_ROUTINE, 'apply_evi_filter_nu'

;doit for the EVI derived ENVI file write utility procedure that

outfile=['']
ofile=101
tfile=109
p_list=ptr_new()
exit_idl=0b
delete_input=0b
in_batch=0b
run_type='[pfilter]'

;First setup protective aunty-Catch to watch out for errors
error_status=0
catch, error_status
if (error_status ne 0) then begin
  catch,/cancel
  help,/last,output=out
  info_text=[strtrim(string('Catch trapped an Error !'),2),$
             strtrim('Error Name : '+strtrim(!err_string,2),2),$
             strtrim(string('Error Number: ',error_status,$
             format='(a,i5)'),2),$
             strtrim('Last Message: '+strtrim(out,2),2)]
;ie write this out to the log file when all is well
  for j=0,n_elements(info_text)-1 do begin
    print,info_text[j]
  endfor
;now check if pointer is still valid
  result=ptr_valid(p_stat)
  if (result) then begin
    ptr_free,p_stat
  endif
  result=ptr_valid(p_list)
  pos=where(result ne 0,count)
  if (count gt 0) then begin
    ptr_free,p_list
  endif
  
;  if (~in_batch) then begin
;;get state pointer to close up the widgets
;    widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;    result=ptr_valid(pstate)
;    if (result) then ptr_free, pstate
;    widget_control,event.top,/destroy
;  endif
  goto, out
endif

print,' '
print,'Starting evi_pfilter_batch_doit'
print,' '

;clean up any fids which are no longer where they were!
;ENVI issue that is annoying and leads to confusion
clean_envi_file_fids

;initialise a few things

datafile=''
o_name=''
ancfile=''
ofile=101
tfile=109
envi_set_path,path_to,/no_set
log_file=strtrim(path_to,2)+'\default_pfilter_log.log'
Log_file_set=0b
;script_file='Hard Coded'
Run_Desc='Default Title'
min_index=-1
max_index=-1
b_thresh=0.0
r_thresh=0.0
save_br=0b
exit_idl=0b
delete_input=0b

day=['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday']
month=['January','February','March','April','May','June','July', $
                 'August','September','October','November','December']

;select the input script file
input:

n_case=1
datafile=strarr(n_case)
ancfile=strarr(n_case)
outfile=strarr(n_case)

status=get_pfilter_script_cmd(script_file, p_stat)

if(status ne 0) then begin
  result=ptr_valid(p_stat)
  if (result) then begin
    ptr_free,p_stat
  endif
  print,'EVI Script file error number '+strtrim(string(status,format='(i8)'),2)
  print,'The Script File has problems - check it !'
  if(status eq 1 or status eq 2) then begin
    Info_text=['EVI pfilter script file does not exist',$
    'or has major problems - check it!',$
    '',$
    'If ENVI/IDL is running some more information',$
    'can be found in the IDL log window']
    print, Info_text
    GOTO, out
;;     result=dialog_message(info_text,/error,title='Error in script_file')
  endif else begin
    Info_text=['EVI pfilter script file has syntax',$
    'or other problems - check it!',$
    '',$
    'If ENVI/IDL is running some more information',$
    'can be found in the IDL log window']
    print, Info_text
    GOTO, out
;;     result=dialog_message(info_text,/error,title='Error in script_file')
  endelse
;;get state pointer to close up the widgets
;  widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;  result=ptr_valid(pstate)
;  if (result) then ptr_free, pstate
;  widget_control,event.top,/destroy
  goto,out
endif

Script_File=(*p_stat).Script_File
n_case=(*p_stat).n_case
Run_Desc=(*p_stat).Run_Desc
script_type=(*p_stat).script_type
Log_File=(*p_stat).Log_File
datafile=(*p_stat).datafile
ancfile=(*p_stat).ancfile
outfile=(*p_stat).outfile
min_ind=(*p_stat).pulse_min_index
max_ind=(*p_stat).pulse_max_index
b_thresh=(*p_stat).b_thresh
r_thresh=(*p_stat).r_thresh
save_br=(*p_stat).save_br
exit_idl=(*p_stat).exit_idl
delete_input=(*p_stat).delete_input
ptr_free, p_stat

;========================================================================
;Start the input checking Process

if (strtrim(log_file,2) eq '') then begin
  print,'The Log file name is missing or blank - check the script file !'
  Info_text=['EVI PulseFilter log file name is missing',$
  'Check the script file!',$
  '',$
  'If ENVI/IDL is running some more information',$
  'can be found in the IDL log window']
  print, Info_text
;;   result=dialog_message(info_text,/error,title='Error in script_file')
;;get state pointer to close up the widgets
;  widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;  result=ptr_valid(pstate)
;  if (result) then ptr_free, pstate
;  widget_control,event.top,/destroy
  goto,out
endif

;see if the log file exists & remove if it does!
if(file_test(log_file)) then begin
  fids=envi_get_file_ids()
  if(fids[0] eq -1) then begin
    file_delete, log_file
  endif else begin
    for i=0,n_elements(fids)-1 do begin
      envi_file_query,fids[i],fname=tname
      if (strtrim(strlowcase(log_file),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
      endif
    endfor
    file_delete, log_file
  endelse
endif

;Open Log file
text_err=0
openw, tfile, log_file,/get_lun,error=text_err
if (text_err ne 0) then begin
  print,' '
  print,'error opening the log file - check the script file !'
  print,'Logfile name='+strtrim(log_file,2)
  print,'evi_pfilter_batch terminating'
  print,' '
  Info_text=['EVI PulseFilter log file cannot be opened',$
  'Check the script file or a system issue',$
  '',$
  'If ENVI/IDL is running some more information',$
  'can be found in the IDL log window']
  print, Info_text
;;   result=dialog_message(info_text,/error,title='Error in script_file')
;;get state pointer to close up the widgets
;  widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;  result=ptr_valid(pstate)
;  if (result) then ptr_free, pstate
;  widget_control,event.top,/destroy
  goto, out
endif

printf,tfile,strtrim('EVI PulseFilter Batch Mode Log File',2)
printf,tfile,strtrim('Run made at: '+systime(),2)
printf,tfile,'Script File used: '+strtrim(script_file,2)
printf,tfile,strtrim('Script file type: '+script_type,2)
if (strtrim(run_desc,2) eq '') then begin
  printf,tfile,'No Description Record was provided'
endif else begin
  printf,tfile,'Run description: '+strtrim(run_desc,2)
endelse
flush, tfile

; Check for sensible index flag values
if (min_ind ne -1 or max_ind ne -1) then begin
  if (min([min_ind, max_ind]) lt 0) then begin
      printf, tfile,'Error setting pulse index range, '+strtrim(min_ind,2)+' - '+strtrim(max_ind,2)
      printf, tfile,'Beginning and end indices must be set if default values are not used.'
      printf, tfile, ' '
      flush, tfile
      info_text=['There were errors found in the checking', $
      ' ',$
      'Check the output log file for the details and re-run',$
      'Log File Name: '+strtrim(log_file,2), $
      ' ',$
      'evi_pfilter_batch terminating']
      for j=0,n_elements(info_text)-1 do begin
        print,info_text[j]
     ENDFOR
;;      result=dialog_message(info_text,/error,title='Errors in evi_pfilter_batch input')
;      ;get state pointer to close up the widgets
;      widget_control,event.top,get_uvalue=pstate
;      ;clean up pointers
;      result=ptr_valid(pstate)
;      if (result) then ptr_free, pstate
;      widget_control,event.top,/destroy
      goto,out
  endif else if (min_ind gt max_ind) then begin
    tempind = min_ind
    min_ind = max_ind
    max_ind = tempind
  endif
endif

log_file_set=1b

;; ;first check you are doing the right process!
;; if (strtrim(strlowcase(script_type),2) ne strtrim(strlowcase(run_type),2)) then begin
;;   Info_Text=[$
;;   'Warning: Script Type is '+strtrim(script_type),$
;;   'However: Run Type is '+strtrim(run_type),$
;;   '',$
;;   'Are you sure you wish to make this run (Yes/No)?']
;;   response=dialog_message(Info_Text,$
;;          /question,title='Incorrect Script File?')
;;   if(response eq 'No') then begin
;; ;;get state pointer to close up the widgets
;; ;    widget_control,event.top,get_uvalue=pstate
;; ;;clean up pointers
;; ;    result=ptr_valid(pstate)
;; ;    if (result) then ptr_free, pstate
;; ;    widget_control,event.top,/destroy
;;     goto, out
;;   endif
;; endif

; Initialise a variable to keep track of disk space required
disk_needed = 0.0

;Now we need to test that the input is OK
print,'Checking the input information for validity'
run_stat=bytarr(n_case)

for j_case=0,n_case-1 do begin

  case_add=strtrim(string(j_case,format='(i8)'),2)

  ;first check the input files exist
  if (~file_test(datafile[j_case])) then begin
    printf,tfile,strtrim('Input file does not exist',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif

  ; Check file can be opened and get dimensions
  envi_open_file, datafile[j_case], r_fid=fid,/no_realize
  ;check if operation cancelled
  if (fid eq -1) then begin
    printf,tfile,strtrim('Error opening input file',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif
  envi_file_query, fid, nb=nb, nl=nl, ns=ns

  ;get path and file name as separate strings and store some useful numbers
  last=strpos(datafile[j_case],path_sep(),/reverse_search)
  f_path=strmid(datafile[j_case],0,last+1)
  f_base=strtrim(strmid(datafile[j_case],last+1,strlen(datafile[j_case])-last-1),2)
  n_base=strlen(f_base)
  n_dot=strpos(f_base,'.',/reverse_search)

  ; Now check header records and make sure it is a valid base-fixed file
  ;set up a base structure for the EVI headers
    evi_headers={ $
       f_base:f_base $
     }
  ;find all of the EVI headers in the hdr file as defined by FID
    status=get_headers(fid,evi_headers)

  if (~status ) then begin
    printf,tfile,strtrim('Invalid FID in EVI get_headers',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  if (evi_headers.headers_present le 0s or ~evi_headers.run_present) then begin
    printf,tfile,strtrim('No EVI headers present, may not be a valid EVI file',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  ; Check file has been base fixed
  if (~evi_headers.base_present) then begin
    printf,tfile,strtrim('No EVI base_fix header present',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif
  envi_file_mng, id=fid, /remove

  ; Check if file has been projected
  if (evi_headers.proj_present) then $
    projected = 1b $
  else $
    projected = 0b

  ; Check ancillary file
  ; First, if ancillary file names weren't given, we need to build them.
  if (ancfile[j_case] eq '') then begin
    if (~projected) then begin
      pos = strpos(f_base, 'cube_bil',/reverse_search)
      ancfile[j_case]=strtrim(f_path,2)+strmid(f_base,0,pos+8)+'_ancillary.img'
    endif else begin
      if (n_dot le 0) or (n_base-n_dot ne 4) then begin
        ancfile[j_case]=strtrim(f_path,2)+f_base+'_extrainfo_bsq'
      endif else begin
        ancfile[j_case]=strtrim(f_path,2)+strmid(f_base,0,n_dot)+'_extrainfo_bsq.img'
      endelse
    endelse
  endif
  if (~file_test(ancfile[j_case])) then begin
    printf,tfile,strtrim('Ancillary file does not exist',2)
    printf,tfile,'Ancillary File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif
  envi_open_file, ancfile[j_case], r_fid=ancillaryfile_fid,/no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
    printf,tfile,strtrim('Error opening ancillary file',2)
    printf,tfile,'Ancillary File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif
  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc
  envi_file_mng, id=ancillaryfile_fid, /remove
  if ((nl_anc ne nl) or (ns_anc ne ns) or (nb_anc lt 3)) then begin
    printf,tfile,strtrim('Ancillary data does not conform with input data',2)
    printf,tfile,'Ancillary File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif

  ;Test the output files
  ; First, if output file names weren't given, we need to build them.
  if (outfile[j_case] eq '') then begin
    if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      outfile[j_case]=f_base+'_pulsefilter.img'
    endif else begin
      outfile[j_case]=strmid(f_base,0,n_dot)+'_pulsefilter.img'
    endelse
    outfile[j_case]=strtrim(f_path,2)+outfile[j_case]
  endif
    hfile = strmid(outfile[j_case],0,strlen(outfile[j_case])-3)+'hdr'

  ; Create the output directory (if it already exists, this code with do nothing)
  o_dir = file_dirname(outfile[j_case])
  file_mkdir, o_dir

  ;The output should NOT be the input file
  if (strtrim(outfile[j_case],2) eq $
    strtrim(datafile[j_case],2)) then begin
    printf,tfile,strtrim('File name conflict encountered',2)
    printf,tfile,strtrim('Output cannot be the Input !',2)
    printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
    goto, end_loop
  endif

  ;see if the output file exists
  ;if so it is either a mistake or user wishes to replace
  ;it by the new file
  ;replacing it involves checking for open files in ENVI
  ;and closing it or them
  if(file_test(outfile[j_case])) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, outfile[j_case],/quiet
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(outfile[j_case]),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
        endif
      endfor
      file_delete, outfile[j_case],/quiet
    endelse
  endif

  ;Open output file
  text_err=0
  openw, ofile, outfile[j_case],/get_lun,error=text_err
  if (text_err ne 0) then begin
    printf,tfile,'Error in case '+case_add
    printf,tfile,'Error opening output file '+strtrim(outfile,2)
      run_stat[j_case]=1
    free_lun,ofile,/force
    goto, end_loop
  endif
  free_lun,ofile,/force
  file_delete, outfile[j_case], /quiet
  file_delete, hfile, /quiet

if (save_br) then begin
  ; Clean up other output files if they exist
  odot = strpos(outfile[j_case],'.',/reverse_search)
  if (odot lt 0) then odot = strlen(outfile[j_case])
  bdatafile = strmid(outfile[j_case], 0, odot)+'_Bvalues.img'
  rdatafile = strmid(outfile[j_case], 0, odot)+'_Rvalues.img'
  bhfile = strmid(outfile[j_case], 0, odot)+'_Bvalues.hdr'
  rhfile = strmid(outfile[j_case], 0, odot)+'_Rvalues.hdr'
  file_delete, bdatafile, rdatafile, bhfile, rhfile, /quiet
endif

; Work out disk space required.
; NB integer data, double precision B and R.
; image=No of samples * No of lines * No of bands * 2
; bdatafile=No of samples * No of lines * No of bands * 4
; rdatafile=No of samples * No of lines * No of bands * 4
; headers ~ 50kB
  if (~save_br) then $
    disk_needed = disk_needed + $
      2.0*1.01*(float(ns)*float(nl)*float(nb)) + 50.0*1024.0 $
  else $
    disk_needed = disk_needed + $
      10.0*1.01*(float(ns)*float(nl)*float(nb)) + 50.0*1024.0

end_loop:
  flush, tfile
endfor

; Get disk required in GB
gbsize = float(disk_needed)/(1024.0^3)

;tell the IDL output checking complete
print,'Input Script File validity checking complete'

;now report to the user and act accordingly
pos_err=where(run_stat gt 0,count)

;report how many cases in error in the log file
if (count gt 0) then begin
  printf,tfile,'There were '+strtrim(string(count,format='(i8)'),2)+' cases in error'
endif
flush, tfile

;now get the user to initiate the batch processing or exit
if (count gt 0) then begin
  info_text=['There were errors found in the checking', $
  ' ',$
  'Check the output log file for the details and re-run',$
  'Log File Name: '+strtrim(log_file,2), $
  ' ',$
  'evi_pfilter_batch terminating']
  for j=0,n_elements(info_text)-1 do begin
    print,info_text[j]
  endfor
;;  result=dialog_message(info_text,/error,title='Errors in evi_pfilter_batch input')
;;get state pointer to close up the widgets
;  widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;  result=ptr_valid(pstate)
;  if (result) then ptr_free, pstate
;  widget_control,event.top,/destroy
  goto,out
endif else begin
  info_text=['Input script information checked',$
  'There are '+strtrim(string(n_case,format='(i8)'),2)+' cases and all are feasible',$
  'Approximate disk space required for output is '+strtrim(string(gbsize,format='(f6.2)'),2)+'GB']
  nlist=2
  if (delete_input gt 0) then begin
    nlist=3
    info_text=[info_text, $
    'The '+strtrim(string(n_case,format='(i8)'),2)+' Input Files will be deleted to conserve space']
  endif
  info_text=[info_text,' ',$
  'OK to start off the batch run? (Yes/No)']
  printf,tfile,''
  printf,tfile,'********************************'
  for j=0,nlist do begin
    printf,tfile,info_text[j]
  endfor

;now list the input information to the log file
  printf,tfile,'********************************'
  printf,tfile,'Run Information:'
  printf,tfile,'n_case='+strtrim(string(n_case,format='(i8)'),2)
  printf,tfile,'log file='+strtrim(log_file,2)
  printf,tfile,'Run Description: '+strtrim(run_desc,2)
  printf,tfile,' '
  printf,tfile,'Data file List:'
  for j=0,n_case-1 do begin
    printf,tfile,strtrim(datafile[j],2)
  endfor
  printf,tfile,' '
  printf,tfile,'Ancillary file List:'
  for j=0,n_case-1 do begin
    printf,tfile,strtrim(ancfile[j],2)
  endfor
  printf,tfile,' '
  printf,tfile,'Output Image List:'
  for j=0,n_case-1 do begin
    printf,tfile,strtrim(outfile[j],2)
  endfor
  printf,tfile,' '
  if (min_ind ne -1 or max_ind ne -1) then begin
    printf,tfile,'User has specified an index range of ' $
      +strtrim(min_ind,2)+' - '+strtrim(max_ind,2)+' for pulse model'
    printf,tfile,'This range will be used for processing all files.'
  endif else begin
    printf, tfile, 'Default pulse model will be used.'
    min_ind = 16
    max_ind = 236
  endelse
  printf,tfile,' '
  if (b_thresh ne 0.0) then begin
    printf,tfile,'User has specified a B threshold of '+strtrim(b_thresh,2)
    printf,tfile,'This value will be used for processing all files.'
  endif else begin
     printf, tfile, 'Default B threshold will be used.'
     b_thresh = 9.5
  endelse
  printf,tfile,' '
  if (r_thresh ne 0.0) then begin
    printf,tfile,'User has specified an R threshold of '+strtrim(r_thresh,2)
    printf,tfile,'This value will be used for processing all files.'
  endif else begin
      printf, tfile, 'Default R threshold will be used.'
      r_thresh = 0.05
  endelse
  printf, tfile,' '
  if (save_br) then begin
      printf,tfile, 'B and R images will be saved.'
  endif else begin
      printf,tfile, 'B and R images will NOT be saved.'
  endelse
  printf,tfile,' '
  flush, tfile

;;all ready so ask if the user really wants to start it all off now?!
;  result=dialog_message(info_text,/question, $
;  title='Batch input feasible - Start the Run ?',$
;  dialog_parent=event.top)
;  if(result eq 'No') then begin
;    printf,tfile,' '
;    printf,tfile,'User Decided NOT to make the run'
;;;get state pointer to close up the widgets
;;    widget_control,event.top,get_uvalue=pstate
;;;clean up pointers
;;    result=ptr_valid(pstate)
;;    if (result) then ptr_free, pstate
;;    widget_control,event.top,/destroy
;    goto, out
;  endif
endelse

;;get state pointer to close up the widgets
;widget_control,event.top,get_uvalue=pstate
;;clean up pointers
;result=ptr_valid(pstate)
;if (result) then ptr_free, pstate
;widget_control,event.top,/destroy

;Now everything is ready to run the cases of the batch run
in_batch=1b

T_start=systime(1)
run_stat=bytarr(n_case)
print,' '
printf,tfile,''

for j_case=0,n_case-1 do begin

  case_add=strtrim(string(j_case+1),2)
  print,'Starting Case '+case_add
  printf,tfile,''
  if (j_case eq 0) then printf,tfile,'********************************'
  printf,tfile,'Case Number '+case_add
  printf,tfile,''
  printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
  flush, tfile

  ; This is where the processing guts go ****

  ; Set up filenames for sat B and R images, if required
  if (save_br) then begin
    odot = strpos(outfile[j_case],'.',/reverse_search)
    if (odot lt 0) then odot = strlen(outfile[j_case])
    bdatafile = strmid(outfile[j_case], 0, odot)+'_Bvalues.img'
    rdatafile = strmid(outfile[j_case], 0, odot)+'_Rvalues.img'
  endif

  ; Open input and ancillary files
  envi_open_file, datafile[j_case], r_fid=fid,/no_realize
  envi_open_file, ancfile[j_case], r_fid=ancillaryfile_fid,/no_realize

  ; Get the file dimensions etc
  envi_file_query, fid, fname=infile, nb=nbands, nl=nlines, $
  ns=nsamples, bnames=bnames, wl=range, data_type=dt
  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, $
          data_type=anc_type, bnames=anc_bnames

  ;get path and file name as separate strings and store some useful numbers
  last=strpos(datafile[j_case],path_sep(),/reverse_search)
  f_path=strmid(datafile[j_case],0,last+1)
  f_base=strtrim(strmid(datafile[j_case],last+1,strlen(datafile[j_case])-last-1),2)
  n_base=strlen(f_base)
  n_dot=strpos(f_base,'.',/reverse_search)
  
  ;set up a base structure for the EVI headers
    evi_headers={ $
       f_base:f_base $
     }
  ;find all of the EVI headers in the hdr file as defined by FID
    status=get_headers(fid,evi_headers)

  ; Get the scale factor from the Base_fix info.
  ;scale_factor = ceil(max_val/184.0)
  info = evi_headers.EVI_base_fix_info
  match = -1
  for i=0,n_elements(info)-1 do if (strmatch(info[i],'*scale*')) then match=i
  if (match ge 0) then begin
    sf = strsplit(info[match],'=',/extract)
    scale_factor = float(sf[1])
  endif else scale_factor = 1.0

;Get date and time of the acquisition
evi_date_time=''
 match = -1
 for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
   if (strmatch(evi_headers.evi_scan_info[i],'*Data End Time*')) then match=i
 endfor
 if (match ge 0) then begin
   sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
   evi_date_time = strtrim(strcompress(sf[1]),2)
 endif else begin
   evi_date_time = ''
 endelse

;Get the site description
evi_description_record=''
 match = -1
 for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
   if (strmatch(evi_headers.evi_scan_info[i],'*Scan Description*')) then match=i
 endfor
 if (match ge 0) then begin
   sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
   if (n_elements(sf) gt 1) then begin
     evi_description_record = strtrim(sf[1],2)
   endif else begin
     evi_description_record = ''
   endelse
 endif else begin
   evi_description_record = ''
 endelse
 
printf,tfile,''
printf,tfile,'EVI run Date & Time='+strtrim(evi_date_time,2)
printf,tfile,'EVI run Description='+strtrim(evi_description_record,2)
flush,tfile 

  ; Check if file has been projected
  if (evi_headers.proj_present) then $
    projected = 1b $
  else $
    projected = 0b

  ; Get the non-trigger and sun-sensor masks, combine and zero first line
  dims = [-1, 0, nsamples-1, 0, nlines-1]
  if (~projected) then begin
    mask = envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=6)
  endif else mask = envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=3)

;get the power information for the data
;range_pos=where(range gt 0.1,nrpos)
;dden=double(nrpos)*double(nsamples)*double(nlines)
;acc_p=0.0d0
;for j=0,nlines-1 do begin
;  slice=envi_get_slice(/bil,fid=fid,line=j,pos=range_pos,xs=0,xe=nsamples-1)
;  spos=where(reform(mask[*,j]) gt 0,nspos)
;  if (nspos gt 0) then begin
;    acc_p=acc_p+total(double(reform(slice[spos,*]))^2)/dden
;  endif
;  slice=0b
;  spos=0b
;endfor
;range_pos=0b
;T_power=float(acc_p)

  ; now temporarily get wavelength from the filename
  ; later need to add a header information of laser wavelength to the hdr file
  ; the wavelength will be extracted from the header information. 
  if (strpos(evi_headers.f_base, '1064') ne -1) then begin
    wavelength = 1064
  endif
  if (strpos(evi_headers.f_base, '1548') ne -1) then begin
    wavelength = 1548
  endif
  ; Call routine to set up pulse model
  dwel_pulse_model_dual, wavelength, i_val, t_val, r_val, p_range, p_time, pulse
  i_val = i_val[[0,1,2,5,6]]
  t_val = t_val[[0,1,2,5,6]]
  r_val = r_val[[0,1,2,5,6]]

  ; Resample pulse onto time base of data
  w = where(range ge p_range[i_val[0]] and range le p_range[i_val[4]],numw)
  base_range=range[w[0]:w[numw-1]]
  p = interpol(pulse, p_range, base_range)

    value=min(abs(base_range-r_val[1]),min_ind)
    value=min(abs(base_range-r_val[3]),max_ind)

  ; Make sure we aren't asking for elements beyond the end of pulse array
  use_max_ind = max_ind < (n_elements(p)-1)
  if (use_max_ind ne max_ind) then begin
    printf, tfile, 'Requested pulse array range truncated to element ',+strtrim(use_max_ind,2)
    printf, tfile, ' '
    flush, tfile
  endif
  ; Subset the pulse array
  max_ind=use_max_ind
  p = p[min_ind:max_ind]

range_pos=where(range gt 0.1,nrpos)
T_Power=0.0d0
F_Power=0.0d0

;set up a state structure
  state={ $
  T_Power:T_Power, $
  F_Power:F_Power, $
  nrpos:nrpos, $
  range_pos:range_pos, $
  mask:mask $
  }
  pstate=ptr_new(state,/no_copy)

; ***********************
  ierr=0
  apply_evi_filter_nu, fid, p, outfile[j_case], b_thresh, r_thresh, $
    dt, nsamples, nlines, nbands,  $
    save_br, bdatafile, rdatafile, pstate, ierr, /quiet
    dt_br=dt_br
dt_br=2
print, 'b_thresh=', b_thresh
print, 'r_thresh=', r_thresh
; ***********************

T_Power=(*pstate).T_Power
F_Power=(*pstate).F_Power
ptr_free,pstate

E_power=T_power-F_power
NSR_p=float(100.0d0*sqrt(E_power/F_power))
F_power=float(sqrt(F_power))

  ; Load the output into ENVI
  for i=0,nbands-1 do bnames[i] = bnames[i]+'_PulseFilter'
  descrip = 'EVI_PULSE_FILTER applied to '+strtrim(datafile[j_case],2)
  envi_setup_head, data_type=2, fname=outfile[j_case], nb = nbands, nl = nlines, $
  ns = nsamples, interleave=1, bnames=bnames, wl=range, $
  zplot_titles=['Range (m)','Intensity'], $
  descrip=descrip, /write, r_fid=out_fid
  envi_open_file, outfile[j_case], r_fid=out_fid,/no_interactive_query,/no_realize

;get output_file and ancillary file names without path ; Zhan's note: actually only output waveform file here, NO ancillary file from pulse filter processing!
last=strpos(outfile[j_case],path_sep(),/reverse_search)
out_base=strtrim(strmid(outfile[j_case],last+1,strlen(outfile[j_case])-last-1),2)

;get the power information for the output data
;range_pos=where(range gt 0.1,nrpos)
;dden=double(nrpos)*double(nsamples)*double(nlines)
;acc_p=0.0d0
;for j=0,nlines-1 do begin
;  slice=envi_get_slice(/bil,fid=out_fid,line=j,pos=range_pos,xs=0,xe=nsamples-1)
;  spos=where(reform(mask[*,j]) gt 0,nspos)
;  if (nspos gt 0) then begin
;    acc_p=acc_p+total(double(reform(slice[spos,*]))^2)/dden
;  endif
;  slice=0b
;  spos=0b
;endfor
;range_pos=0b
;F_power=float(acc_p)
;E_power=T_power-F_power
;NSR_p=100.0*sqrt(E_power/F_power)
;F_power=sqrt(F_power)

  ; Write out user header records
  status = put_headers(out_fid, evi_headers)

; Write pulse filter parameters to header
  evi_pfilter_info = [$
          'Title=Filter parameters and NSR removed from Data',$
          'Min index='+strtrim(min_ind,2), $
          'Max index='+strtrim(max_ind,2), $
          'B thresh='+strtrim(b_thresh,2), $
          'R thresh='+strtrim(r_thresh,2), $
          'F_power='+strtrim(string(F_power),2), $
          'NSR(%)='+strtrim(string(NSR_p),2), $
          'Output_File='+strtrim(out_base,2)]
  evi_pfilter_info=strtrim(evi_pfilter_info,2)

  envi_assign_header_value, fid=out_fid, keyword='EVI_pfilter_info', $
      value=evi_pfilter_info
  ; Update header file
  envi_write_file_header, out_fid
  envi_file_mng, id=out_fid, /remove
  out_fid=0b

  if (save_br) then begin
    save_bnames=bnames
    for i=0,nbands-1 do bnames[i] = strtrim(save_bnames[i],2)+'_Bvalues'
    descrip = 'Pulse filter intensity values used by EVI_PULSE_FILTER to threshold '+datafile[j_case]
    envi_setup_head, data_type=dt_br, fname=bdatafile, nb = nbands, nl = nlines, $
    ns = nsamples, interleave=1, bnames=bnames, wl=range, $
    descrip=descrip, /write, r_fid=out_fid, $
    zplot_titles=['Range (m)','Intensity']
    envi_open_file, bdatafile, r_fid=out_fid,/no_interactive_query,/no_realize

    ; Write out user header records
    status = put_headers(out_fid, evi_headers)

    ; Write pulse filter parameters to header
    envi_assign_header_value, fid=out_fid, keyword='EVI_pfilter_info', $
        value=evi_pfilter_info

    ; Update header file
    envi_write_file_header, out_fid
    envi_file_mng, id=out_fid, /remove
    out_fid=0b

    for i=0,nbands-1 do bnames[i] = strtrim(save_bnames[i],2)+'_Rvalues'
    descrip = 'Pulse filter correlation values used by EVI_PULSE_FILTER to threshold '+datafile[j_case]
    envi_setup_head, data_type=dt_br, fname=rdatafile, nb = nbands, nl = nlines, $
    ns = nsamples, interleave=1, bnames=bnames, wl=range, $
    descrip=descrip, /write, r_fid=out_fid, $
    zplot_titles=['Range (m)','Intensity']
    envi_open_file, rdatafile, r_fid=out_fid,/no_interactive_query,/no_realize

    ; Write out user header records
    status = put_headers(out_fid, evi_headers)

    ; Write pulse filter parameters to header
    envi_assign_header_value, fid=out_fid, keyword='EVI_pfilter_info', $
        value=evi_pfilter_info

    ; Update header file
    envi_write_file_header, out_fid
    envi_file_mng, id=out_fid, /remove
    out_fid=0b
  endif

  ; Remove all remaining files from ENVI
  envi_file_mng, id=ancillaryfile_fid, /remove
  envi_file_mng, id=fid, /remove
  ancillaryfile_fid=0b
  fid=0b
  ; ***
  print,'Completed pfilter for case '+strtrim(string(j_case+1,format='(i8)'),2)
  print,''

  printf,tfile,'Completed pfilter for case '+strtrim(string(j_case+1,format='(i8)'),2)
  printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
  printf,tfile,' '
  printf,tfile,'EVI Processing Info written to Output File Headers:
  for j=0,n_elements(EVI_pfilter_info)-1 do begin
    printf,tfile,strtrim(EVI_pfilter_info[j],2)
  endfor
  printf,tfile,' '
  printf,tfile,'*************************************'
  printf,tfile,' '
  flush, tfile
;Here is the end of the input file Loop!!!!!
endfor

T_end=systime(1)
printf,tfile,' '
printf,tfile,'Pfilter batch Run finished at '+strtrim(systime(),2)
printf,tfile,'Elapsed Time was '+strtrim(string(float(T_end-T_start)/60.0,format='(f12.3)'),2)+' minutes'
flush,tfile

print,'pfilter complete'

if (delete_input gt 0) then begin
  for j=0,n_case-1 do begin
;see if the input file still exists & remove if it does!
    if(file_test(datafile[j])) then begin
      fids=envi_get_file_ids()
      if(fids[0] eq -1) then begin
        file_delete, datafile[j],/quiet
      endif else begin
        for i=0,n_elements(fids)-1 do begin
          envi_file_query,fids[i],fname=tname
          if (strtrim(strlowcase(datafile[j]),2) eq $
              strtrim(strlowcase(tname),2)) then begin
              envi_file_mng,id=fids[i],/remove
          endif
        endfor
        file_delete, datafile[j],/quiet
      endelse
    endif
  endfor
  printf,tfile,'Input files deleted'
endif
datafile=0b

;now free the lun for the log file
free_lun, tfile,/force

;exit the batch file options enforced
if (exit_idl) then begin
  envi_batch_exit,/exit_idl,/no_confirm
endif

out:

;free the log file Lun
free_lun, tfile,/force

if (log_file_set) then begin
  print,'Processing information written to:'
  print,strtrim(log_file,2)
  print,' '
endif

datafile=0b
anc_name=0b
outfile=0b
run_stat=0b
info_text=0b

heap_gc,/verbose

return

end
