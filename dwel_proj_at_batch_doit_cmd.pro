;======================================================================
pro val_block_cmd

  compile_opt idl2
  
  ;set a common block for some important constants
  common evi_constants,pi_val,rad,deg,eta
  
  pi_val=4.0*atan(1.0)
  rad=pi_val/180.0
  deg=1.0/rad
  eta=1.0e-7
  
  return
  
end

function get_project_at_script_cmd, script_file, p_stat

  compile_opt idl2
  
  ;Here we will select and open a script file to getr input files etc
  ;
  status=0
  n_case=0
  Log_File=''
  Run_Desc=''
  exit_idl=0b
  run_type='[project_at]'
  
  ;select the file using envi_pickfile
  ;script_file=envi_pickfile(title='Select the Project_AT Script File to Process')
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
  ;see if it is empty
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
  
  ;do a first trim for leading & trailing blanks
  inbuf=strtrim(inbuf,2)
  
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
  records=strarr(11)
  records=['n_case','log_file','run_desc', $
    'image','anc_name','outfile', $
    'output_resolution', $
    'max_zenith_angle', $
    'exit_idl','delete_input', $
    'overlap']
  present=bytarr(11)
  rec_pos=intarr(11)
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

  ;test for zero n_case
  if (n_case le 0) then begin
    print,'n_case is ZERO! Bad input'
    status=6
    goto,go_back
  endif

  ;all is well so set up the arrays
  image=strarr(n_case)
  anc_name=strarr(n_case)
  outfile=strarr(n_case)
  output_resolution=fltarr(n_case)
  Max_Zenith_Angle=fltarr(n_case)

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

  ;now the image(s)
  if (present[3]) then begin
    result=strpos(scr_text[rec_pos[3]],'=')
    if (result le 0) then begin
      print,'No = in image list record ... bad input?'
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
    image=strtrim(strsplit(inbuf,',',/extract),2)
    if (n_elements(image) ne n_case) then begin
      print,'Number of input images different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(image)
      status=6
      goto,go_back
    endif
    for j=0,n_case-1 do begin
      if (strmid(image[j],0,1) eq "'") then begin
        image[j]=strtrim(strmid(image[j],1,strlen(image[j])-1),2)
      endif
      if (strmid(image[j],strlen(image[j])-1,1) eq "'") then begin
        image[j]=strtrim(strmid(image[j],0,strlen(image[j])-1),2)
      endif
    endfor
  endif

  ;now the ancillary image(s)
  if (present[4]) then begin
    result=strpos(scr_text[rec_pos[4]],'=')
    if (result le 0) then begin
      print,'No = in ancillary image list record ... bad input?'
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
    anc_name=strtrim(strsplit(inbuf,',',/extract),2)
    if (n_elements(anc_name) ne n_case) then begin
      print,'Number of input ancillary images different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(anc_name)
      status=6
      goto,go_back
    endif
    for j=0,n_case-1 do begin
      if (strmid(anc_name[j],0,1) eq "'") then begin
        anc_name[j]=strtrim(strmid(anc_name[j],1,strlen(anc_name[j])-1),2)
      endif
      if (strmid(anc_name[j],strlen(anc_name[j])-1,1) eq "'") then begin
        anc_name[j]=strtrim(strmid(anc_name[j],0,strlen(anc_name[j])-1),2)
      endif
    endfor
  endif

  ;now the output image(s)
  if (present[5]) then begin
    result=strpos(scr_text[rec_pos[5]],'=')
    if (result le 0) then begin
      print,'No = in ancillary image list record ... bad input?'
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

  ;now the output resolution
  if (present[6]) then begin
    result=strpos(scr_text[rec_pos[6]],'=')
    if (result le 0) then begin
      print,'No = in output_resolution record ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[6]],result+1),2)
    if (strmid(inbuf,0,1) eq '[') then begin
      inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
    endif
    if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
      inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
    endif
    output_resolution=float(strsplit(inbuf,',',/extract))
    if (n_elements(output_resolution) ne n_case) then begin
      print,'Number of output resolutions different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(output_resolution)
      status=6
      goto,go_back
    endif
  endif

  ;now the max zenith angle
  if (present[7]) then begin
    result=strpos(scr_text[rec_pos[7]],'=')
    if (result le 0) then begin
      print,'No = in max_zenith_angle record ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[7]],result+1),2)
    if (strmid(inbuf,0,1) eq '[') then begin
      inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
    endif
    if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
      inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
    endif
    max_zenith_angle=float(strsplit(inbuf,',',/extract))
    if (n_elements(max_zenith_angle) ne n_case) then begin
      print,'Number of maximum zeniths different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(max_zenith_angle)
      status=6
      goto,go_back
    endif
  endif

  ; now if exit idl after finishing processing
  if (present[8]) then begin
    exit_idl=0b
    result=strpos(scr_text[rec_pos[8]],'=')
    if (result le 0) then begin
      print,'No = in exit_idl flag ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[8]],result+1),2)
    reads,inbuf,exit_idl,format='(i2)'
  endif

  ; now if deleting input files
  if (present[9]) then begin
    delete_input=0b
    result=strpos(scr_text[rec_pos[9]],'=')
    if (result le 0) then begin
      print,'No = in delete_input flag ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[9]],result+1),2)
    reads,inbuf,delete_input,format='(i2)'
  endif

  ;now the azimuth overlap range
  if (present[10]) then begin
    result=strpos(scr_text[rec_pos[10]],'=')
    if (result le 0) then begin
      print,'No = in overlap record ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[10]],result+1),2)
    if (strmid(inbuf,0,1) eq '[') then begin
      inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
    endif
    if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
      inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
    endif
    overlap=float(strsplit(inbuf,',',/extract))
    if (n_elements(overlap) ne n_case) then begin
      print,'Number of overlaps different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(overlap)
      status=6
      goto,go_back
    endif
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
    print,'Image List:'
    for j=0,n_case-1 do begin
      print,image[j]
    endfor
  endif else begin
    print,'Image List not present'
  endelse
  if(present[4]) then begin
    print,'Ancillary Image List:'
    for j=0,n_case-1 do begin
      print,anc_name[j]
    endfor
  endif else begin
    print,'Ancillary Image List not present'
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
    print,'Output Resolution= ',output_resolution
  endif else begin
    print,'Output Resolution not present'
  endelse
  if(present[7]) then begin
    print,'Max Zenith Angle= ',max_zenith_angle
  endif else begin
    print,'Max Zenith Angle not present'
  endelse
  if(present[8]) then begin
    print,'Exit IDL= ',Exit_IDL
  endif else begin
    print,'Exit IDL Flag not present'
  endelse
  if(present[9]) then begin
    print,'Delete_input= ',delete_input
  endif else begin
    print,'Delete Input Flag not present'
  endelse
  if (present[10]) then begin
    print, 'Overlap= ', overlap
  endif else begin
    print, 'Overlap not present'
    overlap = make_array(n_case, value=-1, /integer)
  endelse 

  sav={ $
    Script_File:Script_File,$
    n_case:n_case,$
    Run_Desc:Run_Desc,$
    script_type:script_type,$
    Log_File:Log_File,$
    image:image,$
    anc_name:anc_name,$
    outfile:outfile,$
    output_resolution:output_resolution,$
    Max_Zenith_Angle:Max_Zenith_Angle,$
    exit_idl:exit_idl,$
    delete_input:delete_input, $
    overlap:overlap $
  }

  ;now locate the data on the heap with a pointer
  p_stat=ptr_new(sav,/no_copy)

  go_back:
  sav=0b
  return, status

end

;======================================================================
pro dwel_proj_at_batch_doit_cmd, script_file, MinRange=minrange
  
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_wind
  
  ;doit for the EVI derived ENVI file write utility procedure that
  
  common evi_constants,pi_val,rad,deg,eta
  
  outfile=['']
  ofile=101
  tfile=109
  p_list=ptr_new()
  exit_idl=0b
  in_batch=0b
  run_type='[project_at]'
  delete_input=0b
  
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
    ;now see if the output file has a handle
    if(outfile[0] ne '') then begin
      fids=envi_get_file_ids()
      if(fids[0] ne -1) then begin
        for i=0,n_elements(fids)-1 do begin
          envi_file_query,fids[i],fname=tname
          if (strtrim(strlowcase(outfile[0]),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
          endif
        endfor
      endif
    endif
    free_lun,ofile,/force
    ;  if (~in_batch) then begin
    ;;get state pointer to close up the widgets
    ;  widget_control,event.top,get_uvalue=pstate
    ;;clean up pointers
    ;  result=ptr_valid(pstate)
    ;  if (result) then ptr_free, pstate
    ;  widget_control,event.top,/destroy
    ;  endif
    goto, out
  endif
  
  print,' '
  print,'Starting evi_proj_at_batch_doit'
  print,' '
  
  ;clean up any fids which are no longer where they were!
  ;ENVI issue that is annoying and leads to confusion
  clean_envi_file_fids
  
  ;initialise a few things
  initiate:
  
  val_block_cmd
  image=''
  o_name=''
  ofile=101
  tfile=109
  def_ifov_x=5.0
  ifov_min=0.0
  ifov_max=60.0
  tmax_min=0.0
  tmax_max=180.0
  def_tmax=108.0
  projection_index=0
  type=4
  scale=1.0d0
  r2mr=1000.0
  Proj_name=['Hemispherical','Andrieu Normal','Andrieu Transpose']
  envi_set_path,path_to,/no_set
  log_file=strtrim(path_to,2)+PATH_SEP()+'default_proj_log.log'
  Log_file_set=0b
  ;script_file='Hard Coded'
  Run_Desc='Default Title'
  exit_idl=0b
  delete_input=0b
  
  ;select the input script file
  input:
  
  n_case=1
  image=strarr(n_case)
  anc_name=strarr(n_case)
  outfile=strarr(n_case)
  output_resolution=fltarr(n_case)
  Max_Zenith_Angle=fltarr(n_case)
  
  status=get_project_at_script_cmd(script_file, p_stat)
  
  if(status ne 0) then begin
    result=ptr_valid(p_stat)
    if (result) then begin
      ptr_free,p_stat
    endif
    print,'EVI Script file error number '+strtrim(string(status,format='(i8)'),2)
    print,'The Script File has problems - check it !'
    if(status eq 1 or status eq 2) then begin
      Info_text=['EVI Projection script file does not exist',$
        'or has major problems - check it!',$
        '',$
        'If ENVI/IDL is running some more information',$
        'can be found in the IDL log window']
      print, Info_text
      ;;     result=dialog_message(info_text,/error,title='Error in script_file')
    endif else begin
      Info_text=['EVI Project_AT script file has  syntax',$
        'or other problems - check it!',$
        '',$
        'If ENVI/IDL is running some more information',$
        'can be found in the IDL log window']
      print, Info_text
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
  image=(*p_stat).image
  anc_name=(*p_stat).anc_name
  outfile=(*p_stat).outfile
  output_resolution=(*p_stat).output_resolution
  Max_Zenith_Angle=(*p_stat).Max_Zenith_Angle
  exit_idl=(*p_stat).exit_idl
  delete_input=(*p_stat).delete_input
  overlap = (*p_stat).overlap
  ptr_free, p_stat
  
  ;========================================================================
  ;Start the input checking Process
  
  if (strtrim(log_file,2) eq '') then begin
    print,'The Log file name is missing or blank - check the script file !'
    Info_text=['EVI Project_AT log file is missing - check it!',$
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
    print,'evi_proj_at_batch terminating'
    print,' '
    Info_text=['EVI Project_AT log file does not open - check it!',$
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
  
  printf,tfile,strtrim('EVI_Project_AT Batch Mode Log File',2)
  printf,tfile,strtrim('Run made at: '+systime(),2)
  printf,tfile,'Script File used: '+strtrim(script_file,2)
  printf,tfile,strtrim('Script file type: '+script_type,2)
  if (strtrim(run_desc,2) eq '') then begin
    printf,tfile,'No Description Record was provided'
  endif else begin
    printf,tfile,'Run description: '+strtrim(run_desc,2)
  endelse
  flush, tfile
  
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
  
  ;Now we need to test that the input is OK
  print,'Checking the input information for validity'
  run_stat=bytarr(n_case)
  
  for j_case=0,n_case-1 do begin
  
    case_add=strtrim(string(j_case,format='(i8)'),2)
    
      ;first check the input images
      if (~file_test(image[j_case])) then begin
        printf,tfile,strtrim('Input file does not exist',2)
        printf,tfile,'Input File: '+strtrim(image[j_case],2)
        printf,tfile,'Error occurred for case '+case_add
        run_stat[j_case]=1
        goto, end_loop
      endif
      envi_open_file, image[j_case],r_fid=fid,/no_interactive_query,/no_realize
      if(fid eq -1) then begin
        printf,tfile,'error opening input file in run '+case_add
        run_stat[j_case]=1
        goto, end_loop
      endif
      ;get the input image dimensions and other info
      envi_file_query, fid, ns=samples, nl=lines, file_type=f_type
      
      result=envi_file_type(f_type)
      if (result ne 'ENVI Standard') then begin
        printf,tfile,'Error in input for case '+case_add
        printf,tfile,'Input file not ENVI Standard'
        run_stat[j_case]=1
        envi_file_mng,id=fid,/remove
        goto, end_loop
      endif
      ;now get the EVI headers that are present
      
      ;get path and image name as separate strings
      last=strpos(image[j_case],path_sep(),/reverse_search)
      f_path=strmid(image[j_case],0,last+1)
      f_base=strtrim(strmid(image[j_case],last+1,strlen(image[j_case])-last-1),2)
      
      ;set up a base structure for the EVI headers
      evi_headers={ $
        f_base:f_base $
      }
      ;find all of the EVI headers in the hdr file as defined by FID
      status=get_headers(fid,evi_headers)
      if (not status) then begin
        printf,tfile,'bad FID in EVI get_headers on input file in case '+case_add
        flush,tfile
        envi_file_mng,id=fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      if (evi_headers.headers_present le 0s or ~evi_headers.run_present) then begin
        printf,tfile,'Input File is NOT a valid EVI Cube file in case '+case_add
        flush,tfile
        envi_file_mng,id=fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      if (evi_headers.proj_present) then begin
        printf,tfile,'Input File is projected (not allowed!) in case '+case_add
        flush,tfile
        envi_file_mng,id=fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      envi_file_mng,id=fid,/remove
      
      ;Now to check the ancillary file
      ;Now get the ancillary file for information
      if (anc_name[j_case] eq '') then begin
        if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
          anc_name[j_case]=strtrim(image[j_case],2)+'_ancillary'
        endif else begin
          anc_name[j_case]=strmid(image[j_case],0,n_dot)+'_ancillary.img'
        endelse
      endif
      
      if(not file_test(anc_name[j_case])) then begin
        printf,tfile,'Ancillary file not present or not correct - going to next case'
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      text_err=0
      envi_open_file, anc_name[j_case],r_fid=anc_fid,/no_interactive_query,/no_realize
      if (anc_fid eq -1) then begin
        printf,tfile,'Error opening ancillary data file '+strtrim(anc_name[j_case],2)
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      ;get the input image dimensions and other info
      envi_file_query, anc_fid, ns=Nshots, nl=Nscans, nb=nb_anc
      
      if ((Nshots ne Samples) or (Nscans ne lines)) then begin
        printf,tfile,'Ancillary File '+strtrim(anc_name[j_case],2)+' does NOT Conform with input !'
        envi_file_mng,id=anc_fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      ;now get the EVI headers that are present for the ancillary file
      ;set up a base structure for the EVI headers
      evi_anc_headers={ $
        f_base:anc_name[j_case] $
      }
      
      ;find all of the EVI headers in the hdr file as defined by FID
      status=get_headers(anc_fid,evi_anc_headers)
      
      if (not status) then begin
        printf,tfile,'Bad FID in get_headers for Ancillary File in case '+case_add
        envi_file_mng,id=anc_fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      if (evi_anc_headers.headers_present le 0s or not evi_anc_headers.run_present) then begin
        printf,tfile,'Ancillary file NOT a valid EVI Cube file in Case '+case_add
        envi_file_mng,id=anc_fid,/remove
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      envi_file_mng,id=anc_fid,/remove
      
      ;Test output resolution
      if (output_resolution[j_case] lt 0.0 or $
        output_resolution[j_case] gt 50.0) then begin
        printf,tfile,'Invalid output resolution in Case '+case_add
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      ;Test max_zenith_angle
      if (Max_Zenith_Angle[j_case] le 0.0 or Max_Zenith_Angle[j_case] gt 180.0) then begin
        printf,tfile,'Invalid Maximum Zenith Angle in Case '+case_add
        run_stat[j_case]=1
        goto, end_loop
      endif
      
      ;Get the beam divergence
      buf=''
      match = -1
      for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
        if (strmatch(evi_headers.evi_scan_info[i],'*Beam Divergence*')) then match=i
      endfor
      if (match ge 0) then begin
        ;   print,'match=',evi_headers.evi_scan_info[match]
        sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
        if (n_elements(sf) gt 1) then begin
          buf = strtrim(strcompress(sf[1]),2)
        endif else begin
          buf = ''
        endelse
      endif else begin
        buf = ''
      endelse
      l=strpos(buf,'mrad')
      if (l gt 0) then buf=strtrim(strmid(buf,0,l-1),2) else buf=''
      ; print,'buf=',buf
      val=float(buf)
      beam_div=val[0]
      
      ;Get the scan_step
      buf=''
      match = -1
      for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
        if (strmatch(evi_headers.evi_scan_info[i],'*Scans per Complete Rotation*')) then match=i
      endfor
      if (match ge 0) then begin
        ;   print,'match=',evi_headers.evi_scan_info[match]
        sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
        if (n_elements(sf) gt 1) then begin
          buf = strtrim(strcompress(sf[1]),2)
        endif else begin
          buf = ''
        endelse
      endif else begin
        buf = ''
      endelse
      val=float(buf)
      if (val[0] gt 0.0) then scan_step=2000.0*!pi/val[0] else scan_step=0.0
      
      ;if none in file set the default ifov
      if(output_resolution[j_case] le 0.0) then output_resolution[j_case]=float(round(100.0*scan_step))/100.0
      
      ;get the Ifov and Maximum Theta
      ifov_x=output_resolution[j_case]
      t_max=Max_Zenith_Angle[j_case]*!pi/180.0
      ptype='AT'
      pname='Andrieu Transpose'
      
      ;Finally test the output files
      n_base=strlen(f_base)
      n_dot=strpos(f_base,'.',/reverse_search)
      if (outfile[j_case] eq '') then begin
        if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
          outfile[j_case]=f_base+'_'+ptype+'_project.img'
        endif else begin
          outfile[j_case]=strmid(f_base,0,n_dot)+'_'+ptype+'_project.img'
        endelse
        outfile[j_case]=strtrim(f_path,2)+outfile[j_case]
      endif
      
      ; Check string length of full output path - error if >255 on some platforms
      if (strlen(outfile[j_case]) gt 255) then begin
        printf,tfile,strtrim('Output file path is too long',2)
        printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
        printf,tfile,'Error occurred for case '+case_add
        run_stat[j_case]=1
        goto, end_loop
      endif else begin
      
        ; Create the output directory (if it already exists, this code will do nothing)
        o_dir = file_dirname(outfile[j_case])
        file_mkdir, o_dir
        
        ;The output should NOT be the input file
        if (strtrim(outfile[j_case],2) eq $
          strtrim(image[j_case],2)) then begin
          printf,tfile,strtrim('File name conflict encountered',2)
          printf,tfile,strtrim('Output cannot be the Input !',2)
          printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
          printf,tfile,'Input File: '+strtrim(image[j_case],2)
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
            file_delete, outfile[j_case]
          endif else begin
            for i=0,n_elements(fids)-1 do begin
              envi_file_query,fids[i],fname=tname
              if (strtrim(strlowcase(outfile[j_case]),2) eq $
                strtrim(strlowcase(tname),2)) then begin
                envi_file_mng,id=fids[i],/remove
              endif
            endfor
            file_delete, outfile[j_case]
          endelse
        endif
        
        ;Open output file
        text_err=0
        openw, ofile, outfile[j_case],/get_lun,error=text_err
        if (text_err ne 0) then begin
          printf,tfile,'Error in case '+case_add
          printf,tfile,'Error opening output file '+strtrim(outfile[j_case],2)
          run_stat[j_case]=1
          free_lun,ofile,/force
          goto, end_loop
        endif
        free_lun,ofile,/force
      endelse
      
    end_loop:
    flush, tfile
  endfor
  
  ;tell the IDL output checking complete
  print,'Input Script File validity checking complete'
  
  ;now report to the user and act accordingly
  pos_err=where(run_stat gt 0,count)
  
  ;report how many cases in error in the log file
  if (count gt 0) then begin
    printf,tfile,'There were '+strtrim(string(count,format='(i8)'),2)+' cases in error'
  endif
  
  ;now get the user to initiate the batch processing or exit
  if (count gt 0) then begin
    info_text=['There were errors found in the checking', $
      ' ',$
      'Check the output log file for the details and re-run',$
      'Log File Name: '+strtrim(log_file,2), $
      ' ',$
      'evi_proj_at_batch terminating']
    for j=0,n_elements(info_text)-1 do begin
      print,info_text[j]
    ENDFOR
    ;;  result=dialog_message(info_text,/error,title='Errors in evi_proj_at_batch input')
    ;;get state pointer to close up the widgets
    ;  widget_control,event.top,get_uvalue=pstate
    ;;clean up pointers
    ;  result=ptr_valid(pstate)
    ;  if (result) then ptr_free, pstate
    ;  widget_control,event.top,/destroy
    goto,out
  endif else begin
    info_text=['Input script information checked',$
      'There are '+strtrim(string(n_case,format='(i8)'),2)+' cases and all are feasible']
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
    printf,tfile,''
    for j=0,nlist do begin
      printf,tfile,info_text[j]
    endfor
    
    ;now list the input information to the log file
    printf,tfile,'********************************'
    printf,tfile,'Run Information:'
    printf,tfile,'n_case='+strtrim(string(n_case),2)
    printf,tfile,'log file='+strtrim(log_file,2)
    printf,tfile,'Run Description: '+strtrim(run_desc,2)
    printf,tfile,'Image List:'
    for j=0,n_case-1 do begin
      printf,tfile,strtrim(image[j],2)
    endfor
    printf,tfile,'Ancillary Image List:'
    for j=0,n_case-1 do begin
      printf,tfile,strtrim(anc_name[j],2)
    endfor
    printf,tfile,'Output Image List:'
    for j=0,n_case-1 do begin
      printf,tfile,strtrim(outfile[j],2)
    endfor
    buf=strjoin(strtrim(string(output_resolution,format='(f10.2)'),2),',',/single)
    printf,tfile,'Output Resolution(mrad)='+strtrim(buf,2)
    buf=strjoin(strtrim(string(max_zenith_angle,format='(f10.2)'),2),',',/single)
    printf,tfile,'Max Zenith Angle(deg)='+strtrim(buf,2)
    buf=strjoin(strtrim(string(overlap,format='(f10.2)'),2),',',/single)
    printf,tfile,'Azimuth overlap(deg)='+strtrim(buf,2)
    flush, tfile
    
    ;;all ready so ask if the user really wants to start it all off now?!
    ;  result=dialog_message(info_text,/question, $
    ;  title='Batch input feasible - Start the Run ?')
    ;  if(result eq 'No') then begin
    ;    printf,tfile,'User Decided NOT to make the run'
    ;;;get state pointer to close up the widgets
    ;;    widget_control,event.top,get_uvalue=pstate
    ;;;clean up pointers
    ;;    result=ptr_valid(pstate)
    ;;    if (result) then ptr_free, pstate
    ;;    widget_control,event.top,/destroy
    ;    goto, out
    ;  endif
    print, 'Start the Run! ...'
  endelse
  
  ;;get state pointer to close up the widgets
  ;widget_control,event.top,get_uvalue=pstate
  ;;clean up pointers
  ;result=ptr_valid(pstate)
  ;if (result) then ptr_free, pstate
  ;widget_control,event.top,/destroy
  
  in_batch=1b
  
  ;Now everything is ready to run the cases of the batch run
  
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
      printf,tfile,'Input File: '+strtrim(image[j_case],2)
      flush, tfile
      
      envi_open_file, image[j_case],r_fid=fid,/no_interactive_query,/no_realize
      if(fid eq -1) then begin
        printf,tfile,'error opening input file in run '+strtrim(string(j_case,format='(i8)'),2)
        print,'error opening input file - going to next case'
        run_stat[j_case]=1
        goto, cleanup
      endif
      
      ;get the input image dimensions and other info
      envi_file_query, fid, ns=ns, nl=nl, nb=nb, $
        byte_swap=order, data_type=type, interleave=ftype, $
        bnames=band_name, dims=dims, file_type=f_type, $
        xstart=xstart, ystart=ystart,wl=wl
        
      result=envi_file_type(f_type)
      
      print,'input file type = ',result
      
      if (result ne 'ENVI Standard') then begin
        printf,tfile,'input file not ENVI Standard - going to next case!'
        goto, cleanup
      endif
      
      samples=ns
      lines=nl
      bands=nb
      
      band_pos=indgen(nb)
      
      x_range=[dims[1],dims[2]]
      y_range=[dims[3],dims[4]]
      
      ;set the type of file
      ft_nam='Unknown'
      case ftype of
        0: ft_nam='BSQ'
        1: ft_nam='BIL'
        2: ft_nam='BIP'
      endcase
      
      ;set the data type
      dt_nam='Unknown'
      case type of
        1: dt_nam='Byte'
        2: dt_nam='Int'
        3: dt_nam='Long Int'
        4: dt_nam='Floating Point'
        5: dt_nam='DP Floating Point'
        6: dt_nam='Complex'
        9: dt_nam='DP Complex'
        12: dt_nam='Unsigned Int'
        13: dt_nam='Unsigned Long Int'
        14: dt_nam='64-bit Int'
        15: dt_nam='64-bit Long Int'
      endcase
      
      ;get path and image name as separate strings
      last=strpos(image[j_case],path_sep(),/reverse_search)
      f_path=strmid(image[j_case],0,last+1)
      f_base=strtrim(strmid(image[j_case],last+1,strlen(image[j_case])-last-1),2)
      
      ;check for the ancillary data file
      n_base=strlen(image[j_case])
      n_dot=strpos(image[j_case],'.',/reverse_search)
      
      ;now get the EVI headers that are present
      ;set up a base structure for the EVI headers
      evi_headers={ $
        f_base:f_base $
      }
      
      ;find all of the EVI headers in the hdr file as defined by FID
      status=get_headers(fid,evi_headers)
      
      if (not status) then begin
        printf,tfile,'bad FID in EVI get_headers on input file! Going to next case'
        goto, cleanup
      endif
      
      if (evi_headers.headers_present le 0s or not evi_headers.run_present) then begin
        printf,tfile,'Input File is NOT a valid EVI Cube file - going to next case'
        goto, cleanup
      endif
      
      ;locate and set the scale factor for the average images
      ;; if (not evi_headers.base_present) then begin
      ;;   base_scale=1.0
      ;;   if (type le 1) then begin
      ;;     scale=1000.0d0
      ;;   endif else begin
      ;;     scale=100.0d0
      ;;   endelse
      ;; endif else begin
      ;;   text=strtrim(evi_headers.EVI_base_fix_info[8],2)
      ;;   l=strlen(text)
      ;;   k=strpos(text,'=')
      ;;   if (strtrim(strmid(text,0,k),2) ne 'scale') then begin
      ;;     base_scale=1.0
      ;;     if (type le 1) then begin
      ;;       scale=1000.0d0
      ;;     endif else begin
      ;;       scale=100.0d0
      ;;     endelse
      ;;   endif else begin
      ;;     reads,strtrim(strmid(text,k+1,l),2),base_scale
      ;;     if (type le 1) then begin
      ;;       scale=1000.0d0/double(base_scale)
      ;;     endif else begin
      ;;       scale=100.0d0/double(base_scale)
      ;;     endelse
      ;;   endelse
      ;; endelse
      
      ;; get rid of the complication of different scale factor for mean image
      ;; b/c it causes many confusions and overflow troubles sometimes. 
      ;; simply just use scale factor of 1.0
      scale = 1.0
      
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
      
      ;Now get the ancillary file for information
      if (anc_name[j_case] eq '') then begin
        if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
          anc_name[j_case]=strtrim(image[j_case],2)+'_ancillary'
        endif else begin
          anc_name[j_case]=strmid(image[j_case],0,n_dot)+'_ancillary.img'
        endelse
      endif
      
      if(not file_test(anc_name[j_case])) then begin
        printf,tfile,'Ancillary file not present or not correct - going to next case'
        goto, cleanup
      endif
      
      text_err=0
      envi_open_file, anc_name[j_case],r_fid=anc_fid,/no_interactive_query,/no_realize
      if (anc_fid eq -1) then begin
        printf,tfile,'Error opening ancillary data file '+strtrim(anc_name[j_case],2)
        goto, cleanup
      endif
      
      ;get the input image dimensions and other info
      envi_file_query, anc_fid, ns=Nshots, nl=Nscans, nb=nb_anc
      
      if ((Nshots ne Samples) or (Nscans ne lines)) then begin
        printf,tfile,'Ancillary File '+strtrim(anc_name[j_case],2)+' does NOT Conform with input !'
        goto, cleanup
      endif
      
      ;now get the EVI headers that are present for the ancillary file
      ;set up a base structure for the EVI headers
      evi_anc_headers={ $
        f_base:anc_name[j_case] $
      }
      
      ;find all of the EVI headers in the hdr file as defined by FID
      status=get_headers(anc_fid,evi_anc_headers)
      
      if (not status) then begin
        printf,tfile,'Bad FID in get_headers for Ancillary File'
        goto, cleanup
      endif
      
      if (evi_anc_headers.headers_present le 0s or not evi_anc_headers.run_present) then begin
        printf,tfile,'Ancillary file NOT a valid EVI Cube file'
        goto, cleanup
      endif
      
      print,'Input information from input files complete'
      
      ;set output band number
      nb_out=n_elements(where(band_pos gt -1))
      
      ;get the mask
      Mask_all=bytarr(Nshots,Nscans)+1b
      dims=[-1,0,Nshots-1,0,Nscans-1]
      Mask_all=envi_get_data(fid=anc_fid,dims=dims,pos=6)
      
      ;get encoder positions!
      ShotZen=fltarr(Nshots,Nscans)
      ShotAZim=fltarr(Nshots,Nscans)
      
      ShotZen=float(envi_get_data(fid=anc_fid,dims=dims,pos=2))
      ShotAZim=float(envi_get_data(fid=anc_fid,dims=dims,pos=3))
      
      envi_file_mng,id=anc_fid,/remove
      
      ;set up a structure and push it onto the heap
      sav={ $
        Nshots:Nshots,$
        Nscans:Nscans,$
        ShotZen:ShotZen,$
        ShotAzim:ShotAzim $
      }

      ;; set the mask for no overlapping
      ;; here we are using ENCODER values, NOT actual angular values. 
      if overlap[j_case] ge 0 then begin
        ; any negative values of overlap means to retain the whole overlap area. 
        ;; set mask according to azimuth angles
        ;; the last scan line to be included, its azimuth is calculated as following
        ;;from the whole first line. This avoids the first pixel in the first line
        ;;is an invalid pixel. 
        tmpazim = lonarr(Nscans)
        for i=0,Nscans-1 do begin
          tmp = ShotAzim[where(Mask_all[*, i]), i]
          tmpazim[i] = mean(tmp)
        endfor 
        ;; tmpazim = ShotAzim[where(Mask_all)]
        ;; tmpazim = long(tmpazim) ;; convert the azimuth values to integer so that
        ;; ;; unique function can act properly.
        ;; tmpazim = tmpazim[uniq(tmpazim)]
        ;; if n_elements(tmpazim) ne Nscans then begin
        ;;   print, 'Azimuth values of scan lines are not expected'
        ;;   return
        ;; endif 
        ;; tmpazim = reform(tmpazim, 2, Nscans)
        ;; tmpazim = tmpazim[0, *]
        tmpdiff = tmpazim[0:Nscans-2] - tmpazim[1:Nscans-1]
        tmppos = where(tmpdiff lt -180.0, tmpcount)
        while tmpcount gt 0 do begin
          tmpazim = tmpazim[0:tmppos[0]] + 524288
          tmpdiff = tmpazim[0:Nscans-2] - tmpazim[1:Nscans-1]
          tmppos = where(tmpdiff lt -524288/2, tmpcount)
        endwhile
        ;; we've found that the azimuth encoders of the first few scan lines
        ;; of a scan could be of no change possibly due to the inertial of the
        ;;instrument rotation or lack of lock-in of rotary encoder. Thus here we
        ;;retain the last 180 degrees of scan lines and discard the first few scan
        ;;lines with possibly bad azimuth values. 

        firstazim = tmpazim[Nscans-1] + 524288/2 + float(overlap[j_case])/360.0*524288
        tmppos = where(tmpazim gt firstazim, tmpcount)
        if tmpcount gt 0 then begin
          Mask_all[*, 0:tmppos[tmpcount-1]] = 0
        endif 

        ;; lastazim = tmpazim[0] - 524288/2
        ;; tmppos = where(tmpazim lt lastazim, tmpcount)
        ;; if tmpcount gt 0 then begin
        ;;   Mask_all[*, tmppos[0]:Nscans-1] = 0
        ;; endif


        ;; tmpazim = ShotAzim[*, 0]
        ;; tmpazim = tmpazim[Mask_all[*, 0]]
        ;; tmpazim = tmpazim[uniq(tmpazim)]
        ;; lastazim = tmpazim[0]
      endif    
      
      ;now put the data on the heap with a pointer
      p_stat=ptr_new(sav,/no_copy)
      
      status = dwel_set_theta_phi(p_stat)
      
      ;put the results into the local arrays
      ShotZen=(*p_stat).ShotZen
      ShotAzim=(*p_stat).ShotAzim
      ptr_free, p_stat
      
      ;set the mask for angles outside range
      pos=where(Shotzen gt Max_Zenith_Angle[j_case],npos)
      print,'npos angles above max=',npos
      
      if (npos gt 0) then mask_all[pos]=0
      pos=0b
      
      srate_set=0b
      ;set the default sampling rate
      match = -1
      for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
        if (strmatch(evi_headers.evi_scan_info[i],'*Sampling Rate*')) then match=i
      endfor
      if (match ge 0) then begin
        text=strtrim(evi_headers.evi_scan_info[match],2)
        k=strpos(text,'=')
        l=strpos(text,'smp/ns')
        var2=float(strtrim(strmid(text,k+1,l-k-1),2))
        if (var2 gt 0.0) then begin
          srate=var2
          srate_set=1
        endif else begin
          srate=2.0
          srate_set=0b
        endelse
      endif else begin
        srate=2.0
        srate_set = 0b
      endelse
      
      if (~srate_set) then print,'Sampling rate NOT read from headers!!!'
      print,'sampling rate=',srate
      
      ;Get the FWHM
      buf=''
      match = -1
      for i=0,n_elements(evi_headers.EVI_base_fix_info)-1 do begin
        if (strmatch(evi_headers.EVI_base_fix_info[i],'*casing_fwhm(nsec)*')) then match=i
      endfor
      if (match ge 0) then begin
        sf = strsplit(evi_headers.EVI_base_fix_info[match],'=',/extract)
        if (n_elements(sf) gt 1) then begin
          buf = strtrim(strcompress(sf[1]),2)
        endif else begin
          buf = ''
        endelse
      endif else begin
        buf = ''
      endelse
      casing_fwhm=float(buf)*srate
      
      print,'casing fwhm=',casing_fwhm
      
      ;Get the beam divergence
      buf=''
      match = -1
      for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
        if (strmatch(evi_headers.evi_scan_info[i],'*Beam Divergence*')) then match=i
      endfor
      if (match ge 0) then begin
        ;   print,'match=',evi_headers.evi_scan_info[match]
        sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
        if (n_elements(sf) gt 1) then begin
          buf = strtrim(strcompress(sf[1]),2)
        endif else begin
          buf = ''
        endelse
      endif else begin
        buf = ''
      endelse
      l=strpos(buf,'mrad')
      if (l gt 0) then buf=strtrim(strmid(buf,0,l-1),2) else buf=''
      ; print,'buf=',buf
      val=float(buf)
      beam_div=val[0]
      
      ;Get the scan_step
      buf=''
      match = -1
      for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
        if (strmatch(evi_headers.evi_scan_info[i],'*Scans per Complete Rotation*')) then match=i
      endfor
      if (match ge 0) then begin
        ;   print,'match=',evi_headers.evi_scan_info[match]
        sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
        if (n_elements(sf) gt 1) then begin
          buf = strtrim(strcompress(sf[1]),2)
        endif else begin
          buf = ''
        endelse
      endif else begin
        buf = ''
      endelse
      val=float(buf)
      if (val[0] gt 0.0) then scan_step=2000.0*!pi/val[0] else scan_step=0.0 ; unit of scan step: mrad
      sampling_ratio=beam_div/scan_step
      
      ;if none in file set the default ifov
      if(output_resolution[j_case] le 0.0) then output_resolution[j_case]=float(round(100.0*scan_step))/100.0
      
      ;get the Ifov and Maximum Theta
      set_ifov:
      
      ifov_x=output_resolution[j_case]
      t_max=Max_Zenith_Angle[j_case]*!pi/180.0
      ptype='AT'
      pname='Andrieu Transpose'
      
      print,'ifov_x=',ifov_x
      
      ;now convert the data to (x,y) coordinates
      
      ;print, 'Andrieu_transpose'
      status=t_Andrieu_tp2xy(Nshots*Nscans,ShotZen,ShotAzim,x_proj,y_proj)
      
      ShotZen=0b
      ShotAzim=0b
      
      ;define image geometry - linear size is (2*k_inc+1) cells
      
      ;set output dimensions
      ns_out=fix(r2mr*t_max/ifov_x)+1
      nl_out=fix(r2mr*2.0*!pi/ifov_x)+1
      
      ;set the step size in (x,y) space corresponding to the resolution
      h=(t_max/float(ns_out))*!radeg
      h21=1.1*(h/2.0)
      h22=1.1*((!radeg*scan_step)/r2mr)/2.0
      h2=max([h21,h22])
      
      counter=0L
      Tot_Count=0L
      gotind=0b
      a_ref=[Nshots,Nscans]
      
      ;Now set up the pointer array to the cell data and all is ready
      
      p_list=make_array(nl_out,/ptr)
      num_val=make_array(ns_out,nl_out,/integer)
      theta=make_array(ns_out,nl_out,/integer)
      phi=make_array(ns_out,nl_out,/integer)
      mask=make_array(ns_out,nl_out,/integer)+1s
      
      A_theta=make_array(ns_out,/double)
      A_phi=make_array(nl_out,/double)
      pos_ind=0b
      
      ;now loop over the cells and set up pointers to the original image cells
      ;involved in the output spatial cells
      for i=0, nl_out-1 do begin
        y=float(i)*h
        y_min=y-h2
        y_max=y+h2
        pos_y=where(((y_proj ge y_min) and (y_proj le y_max) and Mask_all),count_y)
        
        if (count_y gt 0) then begin
          for j=0,ns_out-1 do begin
            x=float(j)*h
            ;get four corners of the pixel
            x_min=x-h2
            x_max=x+h2
            pos_x=where(x_proj[pos_y] ge x_min and x_proj[pos_y] le x_max,count_x)
            if(count_x gt 0) then begin
              num_val[j,i]=fix(count_x)
              temp=array_indices(A_ref,reform(pos_y[pos_x]),/dimensions)
              jj=size(temp,/dimensions)
              if (n_elements(jj) ne 2) then begin
                a_add=j
                temp=[temp,a_add]
              endif else begin
                a_add=intarr(jj[1])+j
                temp=[temp,transpose(a_add)]
              endelse
              if (gotind) then begin
                pos_ind=[[pos_ind],[temp]]
              endif else begin
                pos_ind=[[temp]]
                gotind=1b
              endelse
              counter=counter+1L
              Tot_Count=Tot_Count+count_x
              temp=0b
            endif else begin
              mask[j,i]=0s
            endelse
            status=t_Andrieu_xy2tp(1,x,y,th,ph)
            A_theta[j]=th
            A_phi[i]=ph
            theta[j,i]=fix(round(10.0*th))
            phi[j,i]=fix(round(10.0*ph))
          endfor
          if (total(reform(num_val[*,i])) gt 0) then begin
            temp=sort(reform(pos_ind[1,*]))
            pos_ind=pos_ind[*,temp]
          endif
        endif else begin
          for j=0,ns_out-1 do begin
            x=float(j)*h
            num_val[j,i]=0s
            mask[j,i]=0s
            status=t_Andrieu_xy2tp(1,x,y,th,ph)
            A_theta[j]=th
            A_phi[i]=ph
            theta[j,i]=fix(round(10.0*th))
            phi[j,i]=fix(round(10.0*ph))
          endfor
          ;          print,'Hit an empty line at i=',i
        endelse
        p_list[i]=ptr_new(pos_ind,/no_copy)
        pos_ind=0b
        gotind=0b
        temp=0b
      endfor
      
      a_zen=strtrim(string(A_theta,format='(f12.4)'),2)
      a_azm=strtrim(string(A_phi,format='(f12.4)'),2)
      
      EVI_Andrieu_zenith=a_zen
      EVI_Andrieu_azimuth=a_azm
      
      a_zen=0b
      a_azm=0b
      A_theta=0b
      A_phi=0b
      
      if (type le 1) then begin
        scaler=100.0d0
      endif else begin
        scaler=1.0d0
      endelse
      
      EVI_Projection_info=[ $
        'type='+ptype,$
        'name='+pname,$
        'Beam_Divergence_(mrad)='+strtrim(string(beam_div,format='(f14.3)'),2),$
        'Scan_Step_(mrad)='+strtrim(string(scan_step,format='(f14.3)'),2),$
        'Sampling_Ratio='+strtrim(string(sampling_ratio,format='(f14.3)'),2),$
        'output_resolution_(mrad)='+strtrim(string(ifov_x,format='(f10.2)'),2),$
        'max_zenith_angle_(deg)='+strtrim(string(deg*t_max,format='(f10.2)'),2),$
        'Mean image scale='+strtrim(string(scale,format='(f10.2)'),2),$
        'Output scale='+strtrim(string(scaler,format='(f10.2)'),2) $
        ]
      EVI_Projection_info=strtrim(EVI_Projection_info,2)
      
      ;all ready to go ... so get output file name[s]
      output_envi:
      
      ;Open output file
      text_err=0
      openw, ofile, outfile[j_case],/get_lun,error=text_err
      if (text_err ne 0) then begin
        printf,tfile,'Error opening output file '+strtrim(outfile,2)
        ptr_free,p_list
        goto, cleanup
      endif
      
      print,'pre-processing done - projecting the image!'
      
      ;do the processing in BSQ structure
      ft_out=1
      ft_str=['BSQ','BIL','BIP']
      
      ;Set up the tiling
      tile_id=envi_init_tile(fid,band_pos,num_tiles=num_tiles,$
        interleave=ft_out,xs=x_range[0],xe=x_range[1],ys=y_range[0],ye=y_range[1])
        
      ;check that tiling and dimensions match
      if (num_tiles ne Nscans) then begin
        printf,tfile,'Number of Tiles is unexpected'
        printf,tfile,'Output File Type is '+strtrim(ft_str[ft_out],2)
        printf,tfile,'Number of tiles is '+strtrim(string(num_tiles,format='(i5)'),2)
        printf,tfile,'Expected number of tiles = '+strtrim(string(nb_out,format='(i5)'),2)
        flush,tfile
        envi_tile_done, tile_id
        free_lun, ofile,/force
        ptr_free, p_list
        goto, cleanup
      endif
      
      ;set the output data type
      if (type lt 4 or type gt 9) then begin
        out_type=2
      endif else begin
        out_type=4
      endelse
      
      ;set up the arrays for data and output
      ;set up the arrays for data and output
      data=make_array(Nshots,nb_out,/double)
      accum=make_array(ns_out,nl_out,/double)
      accum_abs=make_array(ns_out,nl_out,/double)
      temp=make_array(Nshots,nb_out,/double)
      accum_r=make_array(ns_out,nl_out,/double)
      accum_r2=make_array(ns_out,nl_out,/double)
      wfmax = make_array(ns_out, nl_out, /double)
      
      ;set up the range scaling
      sc_one = dblarr(nb_out)
      sc_r=dblarr(nb_out)
      sc_r2=dblarr(nb_out)

      ;; set up a minimum range to be projected
      if n_elements(minrange) ne 0 or arg_present(minrange) then begin
        
      endif else begin
        minrange = 0.0
      endelse  
      
      ;pos_nz=where(wl ge 0.0,n_pos)
      pos_nz=where(wl ge minrange,n_pos)
      step=wl[1]-wl[0]
      
      if (n_pos gt 0) then begin
        sc_one[pos_nz] = 1.0
        sc_r[pos_nz]=double(step)*(dindgen(n_pos)+1.0d)
        sc_r2[pos_nz]=(double(step)*(dindgen(n_pos)+1.0d))^2
      endif
      pos_nz=0b
      num_avg=make_array(ns_out,nl_out,/long)
      sc_one = transpose(cmreplicate(sc_one, ns_out))
      
      ;do the processing over the output tiles
      ;BIL Tile
      NegRangePos = where(wl le 0)
      for k=0L, nl_out-1 do begin
        current=-1
        count=0L
        temp=make_array(ns_out,nb_out,/double)
        ;help,*(p_list[k])
        pos_nz=where(num_val[*,k] gt 0,n_pos)
        if (n_pos gt 0) then begin
          pos_ind=*(p_list[k])
          kk=n_elements(reform(pos_ind[0,*]))
          point=0L
          while (point lt kk) do begin
            lin=pos_ind[1,point]
            if (lin ne current) then begin
              data=0s
              data=double(envi_get_tile(tile_id,lin))
              count=count+1L
              current=lin
            endif
            ;do something!
            temp[pos_ind[2,point],*]=temp[pos_ind[2,point],*]+ $
              data[pos_ind[0,point],*]
            num_avg[pos_ind[2,point],k]=num_avg[pos_ind[2,point],k]+1L
            point=point+1L
          endwhile
          temp[pos_nz,*] = cmreplicate((1.0/float(reform(num_avg[pos_nz,k]))), $
            nb_out) * temp[pos_nz,*]
        endif
        pos_ind=0b
        pos_nz=0b
        data=0b
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Zhan's change, suppress all bins within zero range to zero
        temp[*, NegRangePos]=0
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        writeu,ofile,fix(round(scaler*temp))
        
        ;now get the statistics of the image for the extra info file
        accum[*,k]=total(temp*sc_one,2,/double)/float(casing_fwhm)
        accum_abs[*,k]=total(abs(transpose(temp)##sc_one),2,/double)/float(nb_out)
        accum_r[*,k]=transpose(temp)##sc_r
        accum_r2[*,k]=transpose(temp)##sc_r2
        wfmax[*, k] = max(temp*sc_one, dimension=2)
        temp=0b
      endfor
      
      envi_tile_done, tile_id
      free_lun,ofile,/force
      ptr_free, p_list
      envi_file_mng,id=fid,/remove
      fid=0b
      data=0b
      temp=0b
      
      if (total(abs(num_avg-num_val)) gt 0) then begin
        print,'numbers of averaged cells do NOT agree!'
        print,'total counted in first loop=',total(num_val)
        print,'total counted in second loop=',total(num_avg)
      endif
      
      ;compute the stars
      star_r=make_array(ns_out,nl_out,/double)
      star_r2=make_array(ns_out,nl_out,/double)
      pos=where(abs(accum_abs) gt 1.0e-5,count)
      if (count gt 0) then begin
        star_r[pos]=abs(accum_r[pos])/abs(accum_abs[pos])
        star_r2[pos]=sqrt(abs(accum_r2[pos])/abs(accum_abs[pos]))
      endif
      accum_abs=0b
      pos=0b
      
      accum_r=accum_r/total(sc_r)
      accum_r2=accum_r2/total(sc_r2)
      
      sc_r=0b
      sc_r2=0b
      
      image_statistics,accum,mask=mask,minimum=amin,maximum=amax,mean=amean,stddev=asdev
      scaler=4095.0/(amax-amin)
      accum=fix(round(scaler*(accum-amin)))
      
      print,'accum mean=',amean
      
      EVI_projection_info=[EVI_projection_info, $
        'Stats_Format=(Min,Mean,Max,Stddev)', $
        'accum_Stats=['+strtrim(string(amin),2)+',' $
        +strtrim(string(amean),2)+',' $
        +strtrim(string(amax),2)+',' $
        +strtrim(string(asdev),2)+']' $
        ]
        
      image_statistics,accum_r,mask=mask,minimum=amin,maximum=amax,mean=amean,stddev=asdev
      scaler=4095.0/(amax-amin)
      accum_r=fix(round(scaler*(accum_r-amin)))
      
      EVI_projection_info=[EVI_projection_info, $
        'accum_r_Stats=['+strtrim(string(amin),2)+',' $
        +strtrim(string(amean),2)+',' $
        +strtrim(string(amax),2)+',' $
        +strtrim(string(asdev),2)+']' $
        ]
        
      image_statistics,accum_r2,mask=mask,minimum=amin,maximum=amax,mean=amean,stddev=asdev
      scaler=4095.0/(amax-amin)
      accum_r2=fix(round(scaler*(accum_r2-amin)))
      
      EVI_projection_info=[EVI_projection_info, $
        'accum_r2_Stats=['+strtrim(string(amin),2)+',' $
        +strtrim(string(amean),2)+',' $
        +strtrim(string(amax),2)+',' $
        +strtrim(string(asdev),2)+']' $
        ]
        
      image_statistics,star_r,mask=mask,minimum=amin,maximum=amax,mean=amean,stddev=asdev
      scaler=4095.0/(amax-amin)
      star_r=fix(round(scaler*(star_r-amin)))
      
      EVI_projection_info=[EVI_projection_info, $
        'star_r_Stats=['+strtrim(string(amin),2)+',' $
        +strtrim(string(amean),2)+',' $
        +strtrim(string(amax),2)+',' $
        +strtrim(string(asdev),2)+']' $
        ]
        
      image_statistics,star_r2,mask=mask,minimum=amin,maximum=amax,mean=amean,stddev=asdev
      scaler=4095.0/(amax-amin)
      star_r2=fix(round(scaler*(star_r2-amin)))
      
      EVI_projection_info=[EVI_projection_info, $
        'star_r2_Stats=['+strtrim(string(amin),2)+',' $
        +strtrim(string(amean),2)+',' $
        +strtrim(string(amax),2)+',' $
        +strtrim(string(asdev),2)+']' $
        ]
        
      ;set up the ENVI header for the output image and open in the
      ;available files list
      
      descrip=pname+' Projected BIL version of '+strtrim(image[j_case],2)
      
      ;get output_file name without path
      last=strpos(outfile[j_case],path_sep(),/reverse_search)
      out_base=strtrim(strmid(outfile[j_case],last+1,strlen(outfile[j_case])-last-1),2)
      
      EVI_projection_info=[EVI_projection_info, $
        'Input_File='+strtrim(f_base,2),$
        'Output_projected_file='+strtrim(out_base,2)]
        
      envi_setup_head,fname=outfile[j_case],ns=ns_out,nl=nl_out,nb=nb_out,$
        xstart=0,ystart=0,$
        data_type=2, interleave=1, $
        descrip=descrip, wl=wl[band_pos], bnames=band_name[band_pos]+'_'+ptype+'_Projected',/write
        
      envi_open_file,outfile[j_case],r_fid=out_fid,/no_interactive_query,/no_realize
      
      ;write out the previous header records
      status=put_headers(out_fid,evi_headers)
      
      envi_assign_header_value, fid=out_fid, keyword='EVI_projection_info', $
        value=EVI_projection_info
      envi_assign_header_value, fid=out_fid, keyword='EVI_Andrieu_zenith', $
        value=EVI_Andrieu_zenith
      envi_assign_header_value, fid=out_fid, keyword='EVI_Andrieu_azimuth', $
        value=EVI_Andrieu_azimuth
        
      envi_write_file_header, out_fid
      envi_file_mng,id=out_fid,/remove
      
      ;now write out the extra information image
      n_base=strlen(outfile[j_case])
      n_dot=strpos(outfile[j_case],'.',/reverse_search)
      if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
        outextra=outfile[j_case]+'_extrainfo.img'
      endif else begin
        outextra=strmid(outfile[j_case],0,n_dot)+'_extrainfo.img'
      endelse
      ;Open output file
      text_err=0
      openw, ofile, outextra,/get_lun,error=text_err
      if (text_err ne 0) then begin
        print,'Error opening output file '+strtrim(outextra,2)
        goto, cleanup
      endif
      
      writeu,ofile,fix(num_val)
      writeu,ofile,fix(theta)
      writeu,ofile,fix(phi)
      writeu,ofile,fix(mask)
      ;writeu,ofile,fix(round(accum))
      writeu, ofile, fix(round(scale*wfmax))
      writeu,ofile,fix(round(accum_r))
      writeu,ofile,fix(round(accum_r2))
      writeu,ofile,fix(round(star_r))
      writeu,ofile,fix(round(star_r2))
      free_lun, ofile,/force
      
      descrip='Numbers and info for '+strtrim(image[j_case],2)
      bnames=['Number Averaged','Zenith','Azimuth','Mask','Mean','Mean_r','Mean_r2','Star_r','Star_RMS']
      envi_setup_head,fname=outextra,ns=ns_out,nl=nl_out,nb=9,$
        xstart=0,ystart=0,$
        data_type=2, interleave=0, bnames=bnames, $
        descrip=descrip, /write
        
      envi_open_file,outextra,r_fid=anc_fid,/no_interactive_query,/no_realize
      
      ;write out the previous header records
      status=put_headers(anc_fid,evi_headers)
      
      envi_assign_header_value, fid=anc_fid, keyword='EVI_projection_info', $
        value=EVI_projection_info
      envi_assign_header_value, fid=anc_fid, keyword='EVI_Andrieu_zenith', $
        value=EVI_Andrieu_zenith
      envi_assign_header_value, fid=anc_fid, keyword='EVI_Andrieu_azimuth', $
        value=EVI_Andrieu_azimuth
        
      envi_write_file_header, anc_fid
      envi_file_mng,id=anc_fid,/remove
      anc_fid=0b
      
      cleanup:
      
      num_val=0b
      theta=0b
      phi=0b
      mask=0b
      accum=0b
      accum_abs=0b
      pos=0b
      accum_r=0b
      accum_r2=0b
      star_r=0b
      star_r2=0b
      
      envi_tile_done, tile_id
      free_lun, ofile,/force
      
      result=ptr_valid(p_stat)
      if (result) then begin
        ptr_free,p_stat
      endif
      
      result=ptr_valid(p_list[0])
      if (result) then begin
        ptr_free,p_list
      endif
      
      p_list=0b
      
      print,'Completed writing projected image - now for summary data'
      
      ;write this out to the log file when all set up
      
      flush, tfile
      
      printf,tfile,'Completed Project_AT runs for case '+strtrim(string(j_case+1,format='(i8)'),2)
      printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
      printf,tfile,' '
      printf,tfile,'EVI_Projection_Info written to Output File Headers:'
      for index=0,n_elements(evi_projection_info)-1 do begin
        printf,tfile,strtrim(evi_projection_info[index],2)
      endfor
      printf,tfile,' '
      printf,tfile,'*************************************'
      printf,tfile,' '
      flush,tfile
      
      ;Here is the end of the input file Loop!!!!!
    endfor
    
    T_end=systime(1)
    printf,tfile,' '
    printf,tfile,'Batch Run finished at '+strtrim(systime(),2)
    printf,tfile,'Elapsed Time was '+strtrim(string(float(T_end-T_start)/60.0,format='(f12.3)'),2)+' minutes'
    flush,tfile
    
    if (delete_input gt 0) then begin
      for j=0,n_case-1 do begin
        ;see if the input file still exists & remove if it does!
        if(file_test(image[j])) then begin
          fids=envi_get_file_ids()
          if(fids[0] eq -1) then begin
            file_delete, image[j],/quiet
          endif else begin
            for i=0,n_elements(fids)-1 do begin
              envi_file_query,fids[i],fname=tname
              if (strtrim(strlowcase(image[j]),2) eq $
                strtrim(strlowcase(tname),2)) then begin
                envi_file_mng,id=fids[i],/remove
              endif
            endfor
            file_delete, image[j],/quiet
          endelse
        endif
      endfor
      printf,tfile,'Input files deleted'
      flush,tfile
    endif
    image=0b
    
    free_lun, tfile,/force
    
    if (exit_idl) then begin
      envi_batch_exit,/exit_idl,/no_confirm
    endif
    
    out:
    
    free_lun, tfile,/force
    
    if (log_file_set) then begin
      print,'Processing information written to:'
      print,strtrim(log_file,2)
      print,' '
    endif
    
    image=0b
    anc_name=0b
    outfile=0b
    output_resolution=0b
    max_zenith_angle=0b
    projection_type=0b
    run_stat=0b
    info_text=0b
    
    heap_gc,/verbose
    
    return
    ;
  end
  ;======================================================================
