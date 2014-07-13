;======================================================================
;
function get_point_cloud_script_cmd, script_file, p_stat
  compile_opt idl2
  
  ;Here we will select and open a script file to get input files etc
  ;
  status=0
  n_case=0
  Log_File=''
  Run_Desc=''
  pulse_min_index=-1
  pulse_max_index=-1
  b_thresh=6.0
  ;r_thresh=0.0
  save_br=0b
  exit_idl=0b
  delete_input=0b
  run_type='[point_cloud]'
  
  ;select the file using envi_pickfile
  ;script_file=envi_pickfile(title='Select the Script File to Process')
  ;; if (script_file eq '') then begin
  ;;   print,'No script file selected in envi_pickfile'
  ;;   status=1
  ;;   goto,go_back
  ;; endif
  
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
  records=strarr(11)
  records=['n_case','log_file','run_desc', $
    'datafile','ancfile','outfile', $
    'add_evi','save_zero_hits','exit_idl','calib',$
    'evi_az_n']
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
    print, 'The number of cases in the script file is zero. Check the script!'
    GOTO, out
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
  calib=bytarr(n_case)
  evi_az_n=fltarr(n_case)+180.0
  
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
  
  ;now the calibration setting
  if (present[9]) then begin
    result=strpos(scr_text[rec_pos[9]],'=')
    if (result le 0) then begin
      print,'No = in calib record ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[9]],result+1),2)
    if (strmid(inbuf,0,1) eq '[') then begin
      inbuf=strtrim(strmid(inbuf,1,strlen(inbuf)-1),2)
    endif
    if (strmid(inbuf,strlen(inbuf)-1,1) eq ']') then begin
      inbuf=strtrim(strmid(inbuf,0,strlen(inbuf)-1),2)
    endif
    ;
    calib=strsplit(inbuf,',',/extract)
    calib=byte(fix(calib))
    if (n_elements(calib) ne n_case) then begin
      print,'Number of calibration flags different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(calib)
      status=6
      goto,go_back
    endif
  endif
  
  help,calib
  print,'calib=',calib
  
  ;now the EVI azimuth to north setting
  if (present[10]) then begin
    result=strpos(scr_text[rec_pos[10]],'=')
    if (result le 0) then begin
      print,'No = in evi_az_n record ... bad input?'
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
    ;
    evi_az_n=strsplit(inbuf,',',/extract)
    evi_az_n=float(evi_az_n)
    if (n_elements(evi_az_n) ne n_case) then begin
      print,'Number of evi_az_n elements different from number of cases'
      print,'n_case= ',n_case,' n_elements= ',n_elements(calib)
      status=6
      goto,go_back
    endif
  endif
  
  help,evi_az_n
  print,'evi_az_n=',evi_az_n
  
  ;check for add_evi flag
  if (present[6]) then begin
    add_evi=-1
    result=strpos(scr_text[rec_pos[6]],'=')
    if (result le 0) then begin
      print,'No = in add_evi flag ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[6]],result+1),2)
    reads,inbuf,add_evi,format='(i6)'
  endif
  
  if (present[7]) then begin
    save_zero_hits=-1
    result=strpos(scr_text[rec_pos[7]],'=')
    if (result le 0) then begin
      print,'No = in save_zero_hits flag ... bad input?'
      status=5
      goto,go_back
    endif
    inbuf=strtrim(strmid(scr_text[rec_pos[7]],result+1),2)
    reads,inbuf,save_zero_hits,format='(i6)'
  endif
  
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
  if(present[9]) then begin
    print,'Calibration Flags=',calib
  endif else begin
    print,'Calibration Flags not present'
  endelse
  if(present[10]) then begin
    print,'EVI_Az_North=',evi_az_n
  endif else begin
    print,'Default Evi_Az_North used (180)'
  endelse
  if(present[6]) then begin
    print,'Add EVI= ',add_evi
  endif else begin
    print,'Add EVI Flag not present'
    add_evi=0b
  endelse
  if(present[7]) then begin
    print,'Save zero hits= ',save_zero_hits
  endif else begin
    print,'Save zero hits Flag not present'
    save_zero_hits=0b
  endelse
  if(present[8]) then begin
    print,'Exit IDL= ',Exit_IDL
  endif else begin
    print,'Exit IDL Flag not present'
    exit_idl=0b
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
    calib:calib,$
    evi_az_n:evi_az_n,$
    add_evi:add_evi,$
    save_zero_hits:save_zero_hits,$
    exit_idl:exit_idl $
    }
    
  ;now locate the data on the heap with a pointer
  p_stat=ptr_new(sav,/no_copy)
  
  go_back:
  sav=0b
  return, status
  
  out:
  sav=0b
  return, status
  
end

;-------------------------------
pro apply_ptcl_filter_batch_cmd, p, pb_stats, pb_meta, pb_info, error=error

  compile_opt idl2
  
  ;set data
  zero=0.0
  error=0b
  point_file=''
  mdata_file=''
  mfile=99
  tfile=101
  tempfile=103
  ovdebug=0b
  
  evi_num=long((*pb_meta).run_number)
  b_thresh=(*pb_meta).threshold
  i_scale=(*pb_meta).i_scale
  fid=(*pb_stats).fid
  if ((*pb_meta).Zero_Hit_Option) then add='_addgap' else add=''
  if ((*pb_meta).Add_EVI) then add=add+'_addevi' else add=add+''
  
  ;retrieve information about the input file
  envi_file_query, fid, fname=infile,nb=nbands,nl=nlines,ns=nsamples
  
  ;now set up the point and metadata files
  point_file=strtrim((*pb_stats).outfile,2)
  n_base=strlen(point_file)
  n_dot=strpos(point_file,'.',/reverse_search)
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
    point_file=strtrim(point_file,2)+add+'_points.txt'
    mdata_file=strtrim(point_file,2)+add+'_metadata.txt'
  endif else begin
    point_file=strtrim(strmid(point_file,0,n_dot),2)+add+'_points.txt'
    mdata_file=strtrim(strmid(point_file,0,n_dot),2)+add+'_metadata.txt'
  endelse
  
  ;see if the points file exists & remove if it does!
  if(file_test(point_file)) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, point_file,/quiet
      print,'old points file deleted'
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(point_file),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
          print,'old points file removed from ENVI'
        endif
      endfor
      file_delete, point_file,/quiet
      print,'old points file deleted'
    endelse
  endif
  text_err=0
  openw, tfile, point_file,/get_lun,error=text_err
  if (text_err ne 0) then begin
    info_text=[strtrim(string('Error opening points file !'),2),$
      strtrim('File Name ='+strtrim(point_file,2),2),$
      strtrim('Error Number ='+strtrim(text_err,2),2),$
      strtrim('Error Type ='+strtrim(string(!ERROR_STATE.MSG),2),2)]
    ;;result=dialog_message(info_text,/error,title='Error opening points file in evi_point_cloud')
    print, info_text
    print,'Error opening points file in point cloud!!'
    print,'File Name =',strtrim(point_file,2)
    print,'text_err=',text_err
    print,'Error Type =',strtrim(string(!ERROR_STATE.MSG),2)
    print,'Sys_Error Type =',strtrim(string(!ERROR_STATE.SYS_MSG),2)
    error=1b
    goto, cleanup
  endif
  
  time_date=strtrim(systime(),2)
  
  printf,tfile,strtrim('[EVI Point Cloud Data]',2)
  printf,tfile,strtrim('Run made at: '+time_date,2)
  printf,tfile,strtrim('X,Y,Z,d_I,Return_Number,Number_of_Returns,Shot_Number,Run_Number,range,theta,phi,Sample,Line,Band',2)
  flush,tfile
  
  ;see if the metadata file exists & remove if it does!
  if(file_test(mdata_file)) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, mdata_file,/quiet
      print,'old meta data file deleted'
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(mdata_file),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
          print,'old meta data file removed from ENVI'
        endif
      endfor
      file_delete, mdata_file,/quiet
      print,'old meta data file deleted'
    endelse
  endif
  text_err=0
  openw, mfile, mdata_file,/get_lun,error=text_err
  if (text_err ne 0) then begin
    info_text=[strtrim(string('Error opening metadata file !'),2),$
      strtrim('File Name ='+strtrim(mdata_file,2),2),$
      strtrim('Error Number ='+strtrim(text_err,2),2),$
      strtrim('Error Type ='+strtrim(string(!ERROR_STATE.MSG),2),2)]
    ;;result=dialog_message(info_text,/error,title='Error opening
    ;;metadata file in evi_point_cloud')
    print, info_text
    print,'Error opening metadata file in point cloud!!'
    print,'File Name =',strtrim(mdata_file,2)
    print,'text_err=',text_err
    print,'Error Type =',strtrim(string(!ERROR_STATE.MSG),2)
    print,'Sys_Error Type =',strtrim(string(!ERROR_STATE.SYS_MSG),2)
    error=1b
    goto, cleanup
  endif
  
  (*pb_meta).Processing_Date_Time=time_date
  
  printf,mfile,strtrim('[EVI Point Cloud Metadata]',2)
  
  ;write out the metadata that you have set
  printf,mfile,'Processing_Date_Time='+strtrim((*pb_meta).Processing_Date_Time,2)
  printf,mfile,'Run_Number='+strtrim(string((*pb_meta).Run_Number),2)
  printf,mfile,'Description='+strtrim((*pb_meta).Description,2)
  printf,mfile,'Input_Path='+strtrim((*pb_meta).Input_Path,2)
  printf,mfile,'Input_File='+strtrim((*pb_meta).Input_File,2)
  printf,mfile,'Acquisition_Date_Time=',strtrim((*pb_meta).Acquisition_Date_Time,2)
  printf,mfile,'Ancillary_Path='+strtrim((*pb_meta).Ancillary_Path,2)
  printf,mfile,'Ancillary_File='+strtrim((*pb_meta).Ancillary_File,2)
  printf,mfile,'Projection='+strtrim((*pb_meta).Projection,2)
  printf,mfile,'EVI_Calibration='+strtrim(string((*pb_meta).EVI_Calibration),2)
  printf,mfile,'EVI_Height(m)='+strtrim(string((*pb_meta).EVI_Height,format='(f10.2)'),2)
  printf,mfile,'EVI_Az_North='+strtrim(string((*pb_meta).EVI_Az_North,format='(f10.2)'),2)
  printf,mfile,'Max_Zenith_Angle='+strtrim(string((*pb_meta).Max_Zenith_Angle,format='(f10.2)'),2)
  printf,mfile,'Range_Step(m)='+strtrim(string((*pb_meta).Range_Step,format='(f10.4)'),2)
  printf,mfile,'Threshold='+strtrim(string((*pb_meta).Threshold,format='(f10.4)'),2)
  printf,mfile,'Zero_Hit_Option='+strtrim(string((*pb_meta).Zero_Hit_Option,format='(i10)'),2)
  printf,mfile,'Add_EVI='+strtrim(string((*pb_meta).Add_EVI,format='(i10)'),2)
  printf,mfile,'X_scale='+strtrim(string((*pb_meta).X_scale,format='(f10.3)'),2)
  printf,mfile,'Y_scale='+strtrim(string((*pb_meta).Y_scale,format='(f10.3)'),2)
  printf,mfile,'Z_scale='+strtrim(string((*pb_meta).Z_scale,format='(f10.3)'),2)
  printf,mfile,'X_offset='+strtrim(string((*pb_meta).X_offset,format='(f10.3)'),2)
  printf,mfile,'Y_offset='+strtrim(string((*pb_meta).Y_offset,format='(f10.3)'),2)
  printf,mfile,'Z_offset='+strtrim(string((*pb_meta).Z_offset,format='(f10.3)'),2)
  printf,mfile,'I_Scale='+strtrim(string((*pb_meta).I_Scale,format='(f10.3)'),2)
  
  flush,mfile
  
  ; Get path and file name as separate strings
  last=strpos(point_file,path_sep(),/reverse_search)
  in_path = file_dirname(point_file)
  in_base=strtrim(strmid(point_file,last+1,strlen(point_file)-last-1),2)
  
  (*pb_meta).point_file_Path=strtrim(in_path,2)
  (*pb_meta).point_file_Name=strtrim(in_base,2)
  
  flush,mfile
  
  printf,mfile,'Ptcl_File_Path='+strtrim((*pb_meta).point_file_Path,2)
  printf,mfile,'Ptcl_File_Name='+strtrim((*pb_meta).point_file_Name,2)
  
  ;===============================================================
  ;code to save solutions for d for debug
  if (ovdebug)then begin
    temp_name=strtrim(in_path,2)+path_sep()+'temp_file_ptcl_sol.txt'
    print,strtrim(temp_name,2)
    ;see if the TEMP file exists & remove if it does!
    if(file_test(temp_name)) then begin
      fids=envi_get_file_ids()
      if(fids[0] eq -1) then begin
        file_delete, temp_name,/quiet
        print,'old temp file deleted'
      endif else begin
        for i=0,n_elements(fids)-1 do begin
          envi_file_query,fids[i],fname=tname
          if (strtrim(strlowcase(temp_name),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
            print,'old temp file removed from ENVI'
          endif
        endfor
        file_delete, temp_name,/quiet
        print,'old temp file deleted'
      endelse
    endif
    text_err=0
    openw, tempfile, temp_name,/get_lun,error=text_err
    if (text_err ne 0) then begin
      info_text=[strtrim(string('Error opening temp file !'),2),$
        strtrim('File Name ='+strtrim(temp_name,2),2),$
        strtrim('Error Number ='+strtrim(text_err,2),2),$
        strtrim('Error Type ='+strtrim(string(!ERROR_STATE.MSG),2),2)]
      ;;result=dialog_message(info_text,/error,title='Error opening temp
      ;;file in evi_point_cloud')
      print, info_text
      print,'Error opening temp file in point cloud!!'
      print,'File Name =',strtrim(temp_name,2)
      print,'text_err=',text_err
      print,'Error Type =',strtrim(string(!ERROR_STATE.MSG),2)
      print,'Sys_Error Type =',strtrim(string(!ERROR_STATE.SYS_MSG),2)
      error=1b
      goto, cleanup
    endif
    num_test=4
    test_num=11
    printf,tempfile,'File for selected outputs to monitor overlap correction'
    printf,tempfile,strtrim('Run made at: '+time_date,2)
    printf,tempfile,'num_test='+strtrim(string(num_test),2)
    printf,tempfile,'test_num='+strtrim(string(test_num),2)
    flush,tempfile
  endif
  ;
  ;=======================================================
  
  ;use only positive range (greater than focal length)
  pos_r=where((*pb_stats).range lt 0.25,nzero)
  h=(*pb_stats).range[1]-(*pb_stats).range[0]
  if (h le 0.0) then h=1.0
  rmax=float(round((*pb_stats).range[n_elements((*pb_stats).range)-1]+1.0))
  
  print,'rmax=',rmax
  
  ; Calculate some values needed in the convolution
  p=float(p)
  psum=total(p*p)
  pnorm=total(abs(p))
  pmax=max(p)
  spfac=psum/(pnorm*pmax)
  m = max(p,mpos)
  nlead = mpos
  ntrail = n_elements(p)-nlead-1
  
  ;NOTE: this is a change of one value (it was ntrail = n_elements(p)-nlead)
  ;print,'max p=',m
  ;print,'fwhm p=',pnorm/m
  ;print,'nlead,ntrail=',nlead,ntrail
  ;
  ;now put in code to ensure the result if there is truncation
  ; Pad pulse to make symmetrical about the peak
  if (ntrail gt nlead) then begin
    ppad = [replicate(0.0,ntrail-nlead),p]
  endif else begin
    ppad = [p,replicate(0.0,nlead-ntrail)]
  endelse
  
  value=max(ppad,max_point)
  ipscal=float(ppad[max_point])/mean(ppad[max_point-1:max_point+1])
  print,'ipscal=',ipscal
  ;print,'elements in ppad=',n_elements(ppad)
  ;print,'mid point=',n_elements(ppad)/2
  ;print,'max_point=',max_point
  
  num_pad=n_elements(ppad)
  fnum=float(num_pad)
  ;
  ppad2=[replicate(0,num_pad),ppad,replicate(0,num_pad)]
  c2=convol(ppad2,ppad,pnorm)
  c2=c2[num_pad:2*num_pad-1]/pmax
  value2=max(c2,max_point_2)
  
  print,'num_pad='+strtrim(string(num_pad),2)
  print,'size of c2='+strtrim(string(n_elements(c2)),2)
  print,'max in ppad=',value
  print,'max in c2=',value2
  print,'max point in ppad=',max_point
  print,'max point in c2=',max_point_2
  
  ;save the pulse information
  ord=(findgen(num_pad)-float(max_point))*h
  min_ord=min(ord)
  max_ord=max(ord)
  value0=float(ppad)/pnorm
  value1=float(ppad)/pmax
  value2=float(c2)
  d_fwhm=1.0/max(value0)
  p_fwhm=d_fwhm*h
  l_file=strtrim((*pb_stats).outfile,2)
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
    pp_file=strtrim(l_file,2)+add+'_pulse.txt'
  endif else begin
    pp_file=strtrim(strmid(l_file,0,n_dot),2)+add+'_pulse.txt'
  endelse
  ;see if the log file exists & remove if it does!
  if(file_test(pp_file)) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, pp_file,/quiet
      print,'old pulse file deleted'
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(pp_file),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
          print,'old pulse file removed from ENVI'
        endif
      endfor
      file_delete, pp_file,/quiet
      print,'old pulse file deleted'
    endelse
  endif
  text_err=0
  openw, ppfile, pp_file,/get_lun,error=text_err
  if (text_err ne 0) then begin
    info_text=[strtrim(string('Error opening pulse file !'),2),$
      strtrim('File Name ='+strtrim(pp_file,2),2),$
      strtrim('Error Number ='+strtrim(text_err,2),2),$
      strtrim('Error Type ='+strtrim(string(!ERROR_STATE.MSG),2),2)]
    ;;result=dialog_message(info_text,/error,title='Error opening pulse
    ;;file in evi_point_cloud'
    print, info_text
    print,'Error opening pulse file in point cloud!!'
    print,'File Name =',strtrim(pp_file,2)
    print,'text_err=',text_err
    print,'Error Type =',strtrim(string(!ERROR_STATE.MSG),2)
    print,'Sys_Error Type =',strtrim(string(!ERROR_STATE.SYS_MSG),2)
    error=1b
    goto, cleanup
  endif
  time_date=strtrim(systime(),2)
  printf,ppfile,strtrim('Point Cloud Pulse Filter Information File',2)
  printf,ppfile,strtrim('Run made at: '+time_date,2)
  printf,ppfile,strtrim('num_pad=',2)+strtrim(string(num_pad),2)
  printf,ppfile,strtrim('d_fwhm(samp)=',2)+strtrim(string(d_fwhm),2)
  printf,ppfile,strtrim('p_fwhm(m)=',2)+strtrim(string(p_fwhm),2)
  printf,ppfile,strtrim('num,h,Pulse,I,I2',2)
  for k=0,num_pad-1 do begin
    buf=string(k+1)+','+string(ord[k])+','+string(value0[k])+','+string(value1[k])+','+string(value2[k])
    buf=strcompress(buf,/remove_all)
    printf,ppfile,strtrim(buf,2)
  endfor
  free_lun,ppfile,/force
  
  ;open the image file to save info
  ;Open output file
  text_err=0
  openw, oifile, (*pb_stats).oi_name,/get_lun,error=text_err
  if (text_err ne 0) then begin
    print,'Error opening output file '+strtrim((*pb_stats).oi_name,2)
    goto, cleanup
  endif
  
  ;set up accumulators for the ancillary image file
  oi_accum=lonarr(nsamples,nlines)
  gaps=bytarr(nsamples,nlines)
  sum_accum=fltarr(nsamples,nlines)
  range_mean=fltarr(nsamples,nlines)
  i2_accum=fltarr(nsamples,nlines)
  d0_accum=fltarr(nsamples,nlines)
  d_accum=fltarr(nsamples,nlines)
  resid=fltarr(nsamples,nlines)
  
  ;normalisation factor for correlation
  psum=sqrt(psum/fnum)
  
  ;set up and go over the image extracting the point cloud
  
  Zero_Hit_Number=0L
  Shot_Hit_Number=0L
  Total_Hit_Number=0L
  Min_Intensity=1.0e7
  Max_Intensity=-1.0e7
  Min_X=1.0e7
  Max_X=-1.0e7
  Min_Y=1.0e7
  Max_Y=-1.0e7
  Min_Z=1.0e7
  Max_Z=-1.0e7
  Shot_Num=0L
  
  num_spec=0L
  
  ;check if you are going to add the EVI position records
  ;these are just the top and bottom of the EVI so they are they only for geometry
  if ((*pb_meta).Add_EVI gt 0) then begin
    ;      buf=string(0.0,0.0,0.0,0.0,0,0,0.0,0.0,0.0,0,0,0,format='(3f14.3,f14.4,2i10,3f14.2,3i10)')
    buf=string(0.0,0.0,0.0,0.0,0,0,0,evi_num,0.0,0.0,0.0,0,0,0,format='(3f14.3,f14.4,2i10,2i14,3f14.2,3i10)')
    buf=strtrim(strcompress(buf),2)
    while (((ii = strpos(buf, ' '))) ne -1) do $
      strput, buf, ',', ii
    printf,tfile,buf
    ;      buf=string(0.0,0.0,(*pb_meta).EVI_Height,0.0,0,0,0.0,0.0,0.0,0,0,0,format='(3f14.3,f14.4,2i10,3f14.2,3i10)')
    buf=string(0.0,0.0,(*pb_meta).EVI_Height,0.0,0,0,0,evi_num,0.0,0.0,0.0,0,0,0,format='(3f14.3,f14.4,2i10,2i14,3f14.2,3i10)')
    buf=strtrim(strcompress(buf),2)
    while (((ii = strpos(buf, ' '))) ne -1) do $
      strput, buf, ',', ii
    printf,tfile,buf
    flush,tfile
    Total_Hit_Number=2L
  endif
  
  accumn=0.0d0
  accumd=0.0d0
  iprint=0L
  
  sample_interval = 0.5 ;; unit: ns
  ;; create a Gaussian filter.
  ;; the FWHM of outgoing pulse, ns
  outgoing_fwhm = 5.1
  ;; get a normalized Gaussian filter (integral is one)
  outgoing_sigma = outgoing_fwhm / (2 * sqrt(2*alog(2)))
  pulse_half_range = outgoing_fwhm * sqrt(alog(0.001)/alog(0.5)) / 2.0
  gp_half_len = fix(pulse_half_range / sample_interval)
  tmpx = (indgen(2*gp_half_len+1) - gp_half_len) * sample_interval
  tmpy = 1.0 / (outgoing_sigma*sqrt(2*!pi)) * exp(-1*tmpx*tmpx/(2*outgoing_sigma^2))
  gaussian_pulse = fltarr(1, size(tmpy, /n_elements))
  gaussian_pulse[0, *] = tmpy
  gp_len = size(gaussian_pulse, /n_elements)
  ; Loop through each line, calculate B and apply filter.
  ; Write to output file.
  for i=0,nlines-1 do begin
    line = envi_get_slice(fid=fid, /bil, pos=pos, line=i, xs=0, xe=nsamples-1)
    b=fltarr(nbands)
    db=fltarr(nbands)
    d2b=fltarr(nbands)
    r=fltarr(nbands)
    dr=fltarr(nbands)
    d2r=fltarr(nbands)
    
    ;now go over the BIL slice for each sample and find points
    for j=0, nsamples-1 do begin
      if ((*pb_stats).mask[j,i] le 0b) then begin
        oi_accum[j,i]=0l
        sum_accum[j,i]=0.0
        range_mean[j,i]=0.0
        i2_accum[j,i]=0.0
        d0_accum[j,i]=0.0
        d_accum[j,i]=0.0
        resid[j,i]=0.0
        gaps[j,i]=0b
        goto,skip
      endif
      shot_num=shot_num+1L
      ;Pad data to allow convolution to the ends of the original array
      ;note using mean in padding areas
      temp=float(reform(line[j,*]))
      ;; use a Gaussian filter to reduce the noise in DWEL waveforms.
      t=[replicate(mean(temp[0:19]),gp_Len),temp,replicate(mean(temp[nbands-20:nbands-1]),gp_len)]
      c = convol(t, gaussian_pulse)
      temp = c[gp_len:gp_len+nbands-1]
      c = 0b
      
      t=[replicate(mean(temp[0:19]),num_pad),temp,replicate(mean(temp[nbands-20:nbands-1]),num_pad)]
      c = convol(t,ppad,pnorm)
      dc=deriv(c)
      d2c=deriv(dc)
      b = c[num_pad:num_pad+nbands-1]
      db=dc[num_pad:num_pad+nbands-1]/h
      d2b = d2c[num_pad:num_pad+nbands-1]/(h^2)
      if (nzero gt 0) then begin
        b[pos_r] = 0.0
        db[pos_r]=0.0
        d2b[pos_r] = 0.0
      endif
      
      ;; because DWEL pulse has both double peaks and one trough, i.e. real
      ;; signal passes zero level. This thresholding could throw away real
      ;;data and thus not working for DWEL here.
      ;; ;check for areas of noise
      ;;       bs1 = shift(b,1)
      ;;       bsm1 = shift(b,-1)
      ;;       test1 = (bs1 ge b_thresh)
      ;;       testm1 = (bsm1 ge b_thresh)
      ;;       test = (b ge b_thresh)
      ;;       noise = where(~(test and (test1 or testm1)), nump)
      ;;       if (nump gt 0) then begin
      ;;         b[noise] = 0.0
      ;;         db[noise]=0.0
      ;;         d2b[noise]=0.0
      ;;       endif
      
      ;get moving average value variance to compute correlation
      temp = sqrt(float(smooth(t^2,num_pad)))
      temp = temp[num_pad:num_pad+nbands-1]
      t = t[num_pad:num_pad+nbands-1]
      ;; w = where(temp lt b_thresh, nw, compl=fok, ncompl=nfok)
      ;;  if (nw gt 0) then begin
      ;;    r[w] = 0.0
      ;;    dr[w]=0.0
      ;;    d2r[w]=0.0
      ;;    if (nfok gt 0) then begin
      ;;      r[fok] = b[fok]*psum/temp[fok]
      ;;      dr[fok] = db[fok]*psum/temp[fok]
      ;;      d2r[fok] = d2b[fok]*psum/temp[fok]
      ;;    endif
      ;;  endif else begin
      ;;    r = b*psum/temp
      ;;    dr=db*psum/temp
      ;;    d2r=d2b*psum/temp
      ;; ENDELSE
      r = b*psum/temp
      dr=db*psum/temp
      d2r=d2b*psum/temp
      
      ; Check neighbourhoods of derivative and second derivative of correlation for peaks
      bs1 = shift(dr,1)
      bs2=shift(dr,2)
      bsm1 = shift(dr,-1)
      bsm2=shift(dr,-2)
      test1 = (bs1 gt 0.0001 and bs2 gt 0.0001)
      testm1 = (bsm1 lt -0.0001 and bsm2 lt -0.0001)
      test = (d2r lt -0.01) ; and (r gt 0.1) ; remove the r thresholding here, DWEL's r threshold can't be as large as 0.1
      peaks = where(((test) and (test1 and testm1)), nump)
      
      ;; DWEL, remove peaks with intensity lower than b_thresh
      tmpind = where(temp[peaks] GE b_thresh, tmpcount)
      IF tmpcount GT 0 THEN BEGIN
        peaks = peaks[tmpind]
        nump=tmpcount
      ENDIF
      ;; DWEL, check if the peak is just the secondary peak of a
      ;; return pulse. Search a preceding peak within a given range,
      ;;calculate its amplitude of secondary peak. If the candidate
      ;;peak is stronger than this calculated peak, then there is a
      ;;real return pulse. Otherwise this candidate peak is just the
      ;;secondary peak of the preceding peak.
      if nump gt 0 then begin
        newpeaks = make_array(dimension=size(peaks, /dimensions), type=size(peaks, /type))
        newpeaks[0] = peaks[0]
        nump_new = 1
        if (nump gt 1) then begin
          for k = 1, nump-1 do begin
            ;; first see if there is a preceding peak within a certain
            ;; distance
            if peaks[k] - peaks[k-1] lt 20 then begin
              ;; 0.1838 below is the ratio of secondary peak to the
              ;; primary peak.
              if (t[peaks[k]] - t[peaks[k-1]]*0.1838) ge 3*11*0.1838 then begin
                newpeaks[nump_new] = peaks[k]
                nump_new = nump_new + 1
              endif
            endif
          endfor
        endif
      endif
      
      
      nump_new=nump
      
      ph=(*pb_stats).azimuth[j,i]
      th=(*pb_stats).zenith[j,i]
      offset=0s
      ;check for runs of peaks and simplify (can happen)
      if (nump gt 0) then begin
        if (nump gt 1) then begin
          for k=0,nump-2 do begin
            nn=nump-k-1
            if (peaks[nn-1] eq peaks[nn]-1) then begin
              if(r[peaks[nn-1]] ge r[peaks[nn]]) then begin
                peaks[nn]=peaks[nn-1]
              endif
              peaks[nn-1:nump-2]=peaks[nn:nump-1]
              nump_new=nump_new-1
            endif
          endfor
          peaks=peaks[0:nump_new-1]
        endif
        ;
        ;Now look over the selected peaks and get (x,y,z,I) values
        ;
        esum=0.0
        bsum=0.0
        rg=fltarr(nump_new)
        intensity=fltarr(nump_new)
        I2=fltarr(nump_new)
        for k=0,nump_new-1 do begin
          val=r[peaks[k]-1:peaks[k]+1]
          rg_loc=(*pb_stats).range[peaks[k]-1:peaks[k]+1]
          istat=peak_int(rg_loc,val,rg_peak,intensity_peak,offset)
          rg[k]=rg_peak
          peaks[k]=peaks[k]+offset
          intensity[k]=ipscal*mean(t[peaks[k]-1:peaks[k]+1])
          I2[k]=b[peaks[k]]
          esum=esum+float(intensity[k])
          bsum=bsum+float(I2[k])
          if ((nump_new eq 1) and (abs(I2[k]) gt 0.0001)) then begin
            accumn=accumn+double(I2[k])^2
            accumd=accumd+double(intensity[k])^2
          endif
        endfor
        ;
        ;Next correct for overlap and use I and I2 to estimate stable values
        ls_res=0.0
        if (nump_new gt 0) then begin
          difmat=cmreplicate(rg,nump_new)
          difmat=difmat-transpose(difmat)
          pos_out=where((difmat lt min_ord) or (difmat gt max_ord),num_out)
          ;here interpolate I and I2 with dismat, set zeros and then you are close.
          ;two matrices are done and vectors are I and I2 then multiply
          I_mat=interpol(value1,ord,difmat)
          I2_mat=interpol(value2,ord,difmat)
          if (num_out gt 0) then begin
            I_mat[pos_out]=0.0
            I2_mat[pos_out]=0.0
          endif
          I_mat=reform(I_mat,nump_new,nump_new)
          I2_mat=reform(I2_mat,nump_new,nump_new)
          ls_mat=fltarr(nump_new,2*nump_new)
          ls_rhs=fltarr(2*nump_new)
          ls_mat=[[I_mat],[I2_mat]]
          ls_rhs=[intensity,I2]
          d_out=la_least_squares(ls_mat,ls_rhs,residual=ls_res)
          ls_res=sqrt(abs(float(ls_res))/float(2*nump_new))
          
          ;
          ;Code to save information for debugging the linear solution for overlap
          ;======================================================================
          saving=0b
          if (ovdebug) then begin
            if ((nump_new eq num_test) and (iprint le test_num) and (ls_res gt 3.5) $
              and (j gt 100) and (j lt 200) and (i gt 50) $
              ) then begin
              printf,tempfile,''
              printf,tempfile,'Test Case Number='+strtrim(string(iprint+1),2)
              printf,tempfile,'Sample='+strtrim(string(j+1),2)
              printf,tempfile,'Line='+strtrim(string(i+1),2)
              flush,tempfile
              printf,tempfile,'rg range vector'
              buf=strtrim(string(rg),2)
              buf=strcompress(buf,/remove_all)
              buf=strtrim(strjoin(buf,' '),2)
              printf,tempfile,buf
              buf=''
              printf,tempfile,'difmat'
              for jj=0,nump_new-1 do begin
                buf=strtrim(string(reform(difmat[*,jj])),2)
                buf=strcompress(buf,/remove_all)
                buf=strtrim(strjoin(buf,' '),2)
                printf,tempfile,buf
                buf=''
              endfor
              flush,tempfile
              printf,tempfile,'I_mat'
              for jj=0,nump_new-1 do begin
                buf=strtrim(string(reform(I_mat[*,jj])),2)
                buf=strcompress(buf,/remove_all)
                buf=strtrim(strjoin(buf,' '),2)
                printf,tempfile,buf
                buf=''
              endfor
              flush,tempfile
              printf,tempfile,'I2_mat'
              for jj=0,nump_new-1 do begin
                buf=strtrim(string(reform(I2_mat[*,jj])),2)
                buf=strcompress(buf,/remove_all)
                buf=strtrim(strjoin(buf,' '),2)
                printf,tempfile,buf
                buf=''
              endfor
              flush,tempfile
              printf,tempfile,'ls_mat'
              for jj=0,2*nump_new-1 do begin
                buf=strtrim(string(reform(ls_mat[*,jj])),2)
                buf=strcompress(buf,/remove_all)
                buf=strtrim(strjoin(buf,' '),2)
                printf,tempfile,buf
                buf=''
              endfor
              flush,tempfile
              printf,tempfile,'ls_rhs'
              buf=strtrim(string(ls_rhs),2)
              buf=strcompress(buf,/remove_all)
              buf=strtrim(strjoin(buf,' '),2)
              printf,tempfile,buf
              buf=''
              flush,tempfile
              printf,tempfile,'d_out'
              buf=strtrim(string(d_out),2)
              buf=strcompress(buf,/remove_all)
              buf=strtrim(strjoin(buf,' '),2)
              printf,tempfile,buf
              buf=''
              flush,tempfile
              printf,tempfile,'ls_res'
              buf=strtrim(string(ls_res),2)
              buf=strtrim(strjoin(buf,' '),2)
              printf,tempfile,buf
              buf=''
              flush,tempfile
              if (~(*pb_stats).cal_dat) then printf,tempfile,''
              iprint=iprint+1L
              saving=1b
            endif
          endif
          ;
          ;======================================================================
          
          difmat=0b
          I_mat=0b
          I2_mat=0b
          ls_mat=0b
          ls_rhs=0b
        endif
        
        d0sum=0.0
        for k=0,nump_new-1 do begin
          d0sum=d0sum+float(d_out[k])
        endfor
        ;
        ;Now calibrate the d values and get the ls_res (residual of LS) if required
        if ((*pb_stats).cal_dat) then begin
          eff=evi_eff_nu((*pb_stats).evi_div,rg,(*pb_stats).Rtef)
          temp=(*pb_stats).s_Factor*(rg^(*pb_stats).rpow)*d_out/eff
          if (n_elements(temp) ne n_elements(d_out)) then begin
            print,'Bad error! temp and d_out do NOT conform!'
            print,'number of elements in temp=',n_elements(temp)
            print,'number of elements in d_out=',n_elements(d_out)
          endif
          d_out=temp
          temp=0b
          if (ovdebug and saving) then begin
            printf,tempfile,'d_out'
            buf=strtrim(string(d_out),2)
            buf=strtrim(strjoin(buf,' '),2)
            printf,tempfile,buf
            buf=''
            flush,tempfile
            printf,tempfile,''
          endif
        endif
        ;
        rsum=0.0
        vsum=0.0
        for k=0,nump_new-1 do begin
          rsum=rsum+rg[k]*float(d_out[k])
          vsum=vsum+float(d_out[k])
        endfor
        ;
        if ((*pb_stats).cal_dat and ((vsum le -0.0001) or (vsum ge 1.5))) then begin
          oi_accum[j,i]=0l
          sum_accum[j,i]=0.0
          range_mean[j,i]=0.0
          i2_accum[j,i]=0.0
          d0_accum[j,i]=0.0
          d_accum[j,i]=0.0
          resid[j,i]=0.0
          gaps[j,i]=0b
          (*pb_stats).mask[j,i]=0b
          num_spec=num_spec+1L
          goto,skip
        endif else begin
          Shot_Hit_Number=Shot_Hit_Number+1L
          Total_Hit_Number=Total_Hit_Number+nump_new
          oi_accum[j,i]=long(nump_new)
          sum_accum[j,i]=float(esum)
          i2_accum[j,i]=float(bsum)
          d0_accum[j,i]=float(d0sum)
          d_accum[j,i]=float(vsum)
          range_mean[j,i]=rsum/vsum
          gaps[j,i]=0b
          resid[j,i]=ls_res
        endelse
        ;Now go over the final point cloud model and write out the records to the point cloud file
        for k=0,nump_new-1 do begin
          x=rg[k]*sin(th*!dtor)*sin(ph*!dtor)
          y=rg[k]*sin(th*!dtor)*cos(ph*!dtor)
          z=rg[k]*cos(th*!dtor)+(*pb_meta).EVI_Height
          buf=string(x,y,z,i_scale*d_out[k],k+1,nump_new,shot_num,evi_num,rg[k],th,ph,j+1,i+1,peaks[k]+1,format='(3f14.3,f14.4,2i10,2i14,3f14.2,3i10)')
          buf=strtrim(strcompress(buf),2)
          while (((ii = strpos(buf, ' '))) ne -1) do $
            strput, buf, ',', ii
          printf,tfile,buf
          if (i_scale*d_out[k] gt max_intensity) then max_intensity=i_scale*d_out[k]
          if (i_scale*d_out[k] lt min_intensity) then min_intensity=i_scale*d_out[k]
          if (x gt max_x) then max_x=x
          if (x lt min_x) then min_x=x
          if (y gt max_y) then max_y=y
          if (y lt min_y) then min_y=y
          if (z gt max_z) then max_z=z
          if (z lt min_z) then min_z=z
        endfor
      endif else begin
        ; Otherwise it is a gap
        Zero_Hit_Number=Zero_Hit_Number+1L
        oi_accum[j,i]=0l
        sum_accum[j,i]=0.0
        i2_accum[j,i]=0.0
        d0_accum[j,i]=0.0
        d_accum[j,i]=0.0
        range_mean[j,i]=rmax
        gaps[j,i]=1b
        resid[j,i]=0.0
        if ((*pb_meta).zero_hit_option gt 0) then begin
          buf=string(0.0,0.0,0.0,0.0,0,0,shot_num,evi_num,0.0,th,ph,j+1,i+1,0,format='(3f14.3,f14.4,2i10,2i14,3f14.2,3i10)')
          buf=strtrim(strcompress(buf),2)
          while (((ii = strpos(buf, ' '))) ne -1) do $
            strput, buf, ',', ii
          printf,tfile,buf
        endif
      endelse
      skip:
    endfor
    
    c=0b
    dc=0b
    d2c=0b
    bs1=0b
    bsm1=0b
    bs2=0b
    bsm2=0b
    
  endfor
  flush,tfile
  
  if (ovdebug) then begin
    if (iprint le 0) then begin
      printf,tempfile,'There were NO such cases detected!'
      flush,tempfile
    endif
    flush,tempfile
    free_lun,tempfile,/force
  endif
  
  print,'num_spec=',num_spec
  
  ;compute the actual ratio to compare with theroy
  ratio_th=spfac
  ratio_est=sqrt(double(accumn)/double(accumd))
  pos_mask=where((*pb_stats).mask ne 0b,npos)
  tmask=(*pb_stats).mask
  
  ;get d-stats
  image_statistics,d_accum,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  d_accum[pos_mask]=4095.0*(d_accum[pos_mask]-emin)/(emax-emin)
  print,'mean corrected intensity=',emean
  
  pb_info=[pb_info,$
    'Stats_Format=(Min,Mean,Max,Stddev)',$
    'd_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;get d-stats
  image_statistics,d0_accum,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  d0_accum[pos_mask]=4095.0*(d0_accum[pos_mask]-emin)/(emax-emin)
  print,'mean corrected uncalibrated intensity=',emean
  
  pb_info=[pb_info,$
    'd0_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;get I stats
  image_statistics,sum_accum,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  sum_accum[pos_mask]=4095.0*(sum_accum[pos_mask]-emin)/(emax-emin)
  print,'mean I sum=',emean
  
  pb_info=[pb_info,$
    'I_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;get I2 stats
  image_statistics,i2_accum,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  i2_accum[pos_mask]=4095.0*(i2_accum[pos_mask]-emin)/(emax-emin)
  print,'mean I2 sum=',emean
  
  pb_info=[pb_info,$
    'I2_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;get Range stats
  image_statistics,range_mean,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  print,'mean Range=',emean
  
  pb_info=[pb_info,$
    'Range_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;get residual stats
  image_statistics,resid,mask=tmask,minimum=emin,maximum=emax,mean=emean,stddev=esdev
  resid[pos_mask]=4095.0*(resid[pos_mask]-emin)/(emax-emin)
  print,'mean Residual=',emean
  
  pb_info=[pb_info,$
    'Residual_Stats=['+strtrim(string(emin),2)+',' $
    +strtrim(string(emean),2)+',' $
    +strtrim(string(emax),2)+',' $
    +strtrim(string(esdev),2)+']' $
    ]
    
  ;write out the ancillary file in the at-project geometry
  writeu,oifile,fix(oi_accum)
  writeu,oifile,fix(round(d_accum))
  writeu,oifile,fix(round(d0_accum))
  writeu,oifile,fix(round(sum_accum))
  writeu,oifile,fix(round(i2_accum))
  writeu,oifile,fix(round(100.0*range_mean))
  writeu,oifile,fix(round(10.0*(*pb_stats).zenith))
  writeu,oifile,fix(round(10.0*(*pb_stats).azimuth))
  writeu,oifile,fix(round(resid))
  writeu,oifile,fix(gaps)
  writeu,oifile,fix((*pb_stats).mask)
  free_lun, oifile,/force
  pos_mask=0b
  tmask=0b
  
  oi_accum=0b
  d_accum=0b
  d0_accum=0b
  sum_accum=0b
  i2_accum=0b
  
  if ((*pb_meta).zero_hit_option gt 0) then begin
    Nrecs=Total_Hit_Number+Zero_Hit_Number
  endif else begin
    Nrecs=Total_Hit_Number
  endelse
  
  (*pb_meta).Zero_Hit_Number=Zero_Hit_Number
  (*pb_meta).Shot_Hit_Number=Shot_Hit_Number
  (*pb_meta).Total_Hit_Number=Total_Hit_Number
  (*pb_meta).Total_Hit_Number=Total_Hit_Number
  (*pb_meta).Nrecs=Nrecs
  (*pb_meta).Min_X=Min_X
  (*pb_meta).Max_X=Max_X
  (*pb_meta).Min_Y=Min_Y
  (*pb_meta).Max_Y=Max_Y
  (*pb_meta).Min_Z=Min_Z
  (*pb_meta).Max_Z=Max_Z
  (*pb_meta).Min_Intensity=Min_Intensity
  (*pb_meta).Max_Intensity=Max_Intensity
  
  printf,mfile,'Zero_Hit_Number='+strtrim(string((*pb_meta).Zero_Hit_Number,format='(i10)'),2)
  printf,mfile,'Shot_Hit_Number='+strtrim(string((*pb_meta).Shot_Hit_Number,format='(i10)'),2)
  printf,mfile,'Total_Hit_Number='+strtrim(string((*pb_meta).Total_Hit_Number,format='(i10)'),2)
  printf,mfile,'Nrecs='+strtrim(string((*pb_meta).Nrecs,format='(i10)'),2)
  printf,mfile,'Max_X='+strtrim(string((*pb_meta).Max_X,format='(f10.3)'),2)
  printf,mfile,'Min_X='+strtrim(string((*pb_meta).Min_X,format='(f10.3)'),2)
  printf,mfile,'Max_Y='+strtrim(string((*pb_meta).Max_Y,format='(f10.3)'),2)
  printf,mfile,'Min_Y='+strtrim(string((*pb_meta).Min_Y,format='(f10.3)'),2)
  printf,mfile,'Max_Z='+strtrim(string((*pb_meta).Max_Z,format='(f10.3)'),2)
  printf,mfile,'Min_Z='+strtrim(string((*pb_meta).Min_Z,format='(f10.3)'),2)
  printf,mfile,'Max_Intensity='+strtrim(string((*pb_meta).Max_Intensity,format='(f10.3)'),2)
  printf,mfile,'Min_Intensity='+strtrim(string((*pb_meta).Min_Intensity,format='(f10.3)'),2)
  
  flush,mfile
  
  perc_hit=round(100.0*float((*pb_meta).Shot_Hit_Number)/float((*pb_meta).Total_Hit_Number))
  
  pb_info=[pb_info,$
    'Percent_hits='+strtrim(string(perc_hit,format='(f10.3)'),2),$
    'Theory_Ratio='+strtrim(string(ratio_th,format='(f10.4)'),2),$
    'Estimated_Ratio='+strtrim(string(ratio_est,format='(f10.4)'),2) $
    ]
    
  print,'Point Cloud Case Finished - clean up'
  
  cleanup:
  
  ; Close output files
  free_lun, tfile,/force
  free_lun, mfile,/force
  free_lun,tempfile,/force
  
  return
end

;------------------------------------------------------------------

pro dwel_point_cloud_batch_doit_cmd, script_file
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_wind
  
  ;doit for the EVI derived ENVI file write utility procedure that
  
  outfile=['']
  ofile=101
  tfile=109
  p_list=ptr_new()
  add_evi=0b
  save_zero_hits=0b
  exit_idl=0b
  delete_input=0b
  in_batch=0b
  run_type='[point_cloud]'
  
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
  print,'Starting evi_point_cloud_batch_doit'
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
  log_file=strtrim(path_to,2)+'\default_point_cloud_log.log'
  Log_file_set=0b
  ;script_file='Hard Coded'
  Run_Desc='Default Title'
  add_evi=0b
  save_zero_hits=0b
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
  
  status=get_point_cloud_script_cmd(script_file, p_stat)
  
  if(status ne 0) then begin
    result=ptr_valid(p_stat)
    if (result) then begin
      ptr_free,p_stat
    endif
    print,'EVI Script file error number '+strtrim(string(status,format='(i8)'),2)
    print,'The Script File has problems - check it !'
    if(status eq 1 or status eq 2) then begin
      Info_text=['EVI point_cloud script file does not exist',$
        'or has major problems - check it!',$
        '',$
        'If ENVI/IDL is running some more information',$
        'can be found in the IDL log window']
      ;; result=dialog_message(info_text,/error,title='Error in
      ;; script_file')
      print, info_text
    endif else begin
      Info_text=['EVI point_cloud script file has syntax',$
        'or other problems - check it!',$
        '',$
        'If ENVI/IDL is running some more information',$
        'can be found in the IDL log window']
      ;; result=dialog_message(info_text,/error,title='Error in
      ;;script_file')
      print, info_text
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
  calib=(*p_stat).calib
  evi_az_n=(*p_stat).evi_az_n
  add_evi=(*p_stat).add_evi
  save_zero_hits=(*p_stat).save_zero_hits
  exit_idl=(*p_stat).exit_idl
  ptr_free, p_stat
  
  print,'evi_az_n returned =',evi_az_n
  
  ;========================================================================
  ;Start the input checking Process
  
  if (strtrim(log_file,2) eq '') then begin
    print,'The Log file name is missing or blank - check the script file !'
    Info_text=['EVI PulseFilter log file name is missing',$
      'Check the script file!',$
      '',$
      'If ENVI/IDL is running some more information',$
      'can be found in the IDL log window']
    ;; result=dialog_message(info_text,/error,title='Error in
    ;; script_file')
    print, info_text
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
      file_delete, log_file,/quiet
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(log_file),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
        endif
      endfor
      file_delete, log_file,/quiet
    endelse
  endif
  
  ;Open Log file
  text_err=0
  openw, tfile, log_file,/get_lun,error=text_err
  if (text_err ne 0) then begin
    print,' '
    print,'error opening the log file - check the script file !'
    print,'Logfile name='+strtrim(log_file,2)
    print,'evi_point_cloud_batch terminating'
    print,' '
    Info_text=['EVI PulseFilter log file cannot be opened',$
      'Check the script file or a system issue',$
      '',$
      'If ENVI/IDL is running some more information',$
      'can be found in the IDL log window']
    ;;result=dialog_message(info_text,/error,title='Error in
    ;;script_file')
    print, info_text
    ;;get state pointer to close up the widgets
    ;  widget_control,event.top,get_uvalue=pstate
    ;;clean up pointers
    ;  result=ptr_valid(pstate)
    ;  if (result) then ptr_free, pstate
    ;  widget_control,event.top,/destroy
    goto, out
  endif
  
  printf,tfile,strtrim('EVI Point_Cloud Batch Mode Log File',2)
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
  
  ;first check you are doing the right process!
  if (strtrim(strlowcase(script_type),2) ne strtrim(strlowcase(run_type),2)) then begin
    Info_Text=[$
      'Warning: Script Type is '+strtrim(script_type),$
      'However: Run Type is '+strtrim(run_type),$
      'Incorrect Script File?']
    print, Info_Text
    GOTO, out
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
  endif
  
  ; Initialise a variable to keep track of disk space required
  disk_needed = 0.0
  
  ;Now we need to test that the input is OK
  print,'Checking the input information for validity'
  run_stat=bytarr(n_case)
  
  if (n_elements(calib) ne n_case) then begin
    print,'calib NOT set up properly - default to none'
    calib=bytarr(n_case)
  endif
  
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
    
    envi_file_mng, id=fid, /remove
    
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
    
    ; Check if file is apparent reflectance
    if (evi_headers.apprefl_present) then begin
      app_refl=1b
      calib[j_case]=0b
    endif else app_refl=0b
    
    ; Check if file is pfiltered
    if (evi_headers.pfilter_present) then begin
      pfiltered=1b
    endif else pfiltered=0b
    
    ;Get date and time of the acquisition
    Date_Time=''
    match = -1
    for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
      if (strmatch(evi_headers.evi_scan_info[i],'*Data End Time*')) then match=i
    endfor
    if (match ge 0) then begin
      sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
      Date_Time = strtrim(sf[1],2)
    endif else begin
      Date_Time = ''
    endelse
    ; Check date can be retrieved
    if (Date_time eq '') then begin
      printf,tfile,strtrim('Date and Time cannot be retrieved',2)
      printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
      printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
      goto, end_loop
    endif
    evi_year=fix(strtrim(strmid(date_time,strlen(date_time)-4),2))
    
    ; Check year is OK!
    ;  if ((evi_year lt 2000) or (evi_year gt 2012)) then begin
    ;    printf,tfile,strtrim('EVI year is invalid (',2)+strtrim(string(evi_year),2)+')'
    ;    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    ;    printf,tfile,'Error occurred for case '+case_add
    ;      run_stat[j_case]=1
    ;    goto, end_loop
    ;  endif
    
    ;Get the site description
    Description_record=''
    match = -1
    for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
      if (strmatch(evi_headers.evi_scan_info[i],'*Scan Description*')) then match=i
    endfor
    if (match ge 0) then begin
      sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
      if (n_elements(sf) gt 1) then begin
        Description_record = strtrim(sf[1],2)
      endif else begin
        Description_record = ''
      endelse
    endif else begin
      Description_record = ''
    endelse
    
    ;Locate the EVI Height
    EVI_Height=-1.0
    match = -1
    for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
      if (strmatch(evi_headers.evi_scan_info[i],'*EVI Height*')) then match=i
    endfor
    if (match ge 0) then begin
      sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
      EVI_Height = float(sf[1])
    endif else begin
      EVI_Height = -1.0
    endelse
    ; Check evi height can be retrieved
    if (evi_height le -1.0) then begin
      printf,tfile,strtrim('EVI Height cannot be retrieved',2)
      printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
      printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
      goto, end_loop
    endif
    
    ;Read the beam divergence from the scan info
    match = -1
    for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
      if (strmatch(evi_headers.evi_scan_info[i],'*Beam Divergence*')) then match=i
    endfor
    if (match ge 0) then begin
      sf = strtrim(strcompress(strsplit(evi_headers.evi_scan_info[match],'=',/extract)),2)
      t = strsplit(sf[1],' ',/extract)
      evi_div = float(t[0])
    endif else begin
      evi_div = -1.0
    endelse
    if (evi_div lt 0.0) then begin
      printf,tfile,strtrim('EVI beam divergence cannot be retrieved',2)
      printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
      printf,tfile,'Error occurred for case '+case_add
      run_stat[j_case]=1
      goto, end_loop
    endif
    
    ; Check if file has been projected
    if (evi_headers.proj_present) then $
      projected = 1b $
    else $
      projected = 0b
      
    ; Read the scale factor and ND filter name from base_fix
    match = -1
    for i=0,n_elements(evi_headers.evi_base_fix_info)-1 do if (strmatch(evi_headers.evi_base_fix_info[i],'*scale*')) then match=i
    if (match ge 0) then begin
      sf = strsplit(evi_headers.evi_base_fix_info[match],'=',/extract)
      scale_factor = float(sf[1])
    endif else scale_factor = 1.0
    match = -1
    for i=0,n_elements(evi_headers.evi_base_fix_info)-1 do if (strmatch(evi_headers.evi_base_fix_info[i],'*Filter*')) then match=i
    if (match ge 0) then begin
      f_name = strsplit(evi_headers.evi_base_fix_info[match],'=',/extract)
      filter_name = f_name[1]
    endif else filter_name = 'unknown'
    
    if (strlowcase(filter_name) eq 'unknown') then begin
      printf,tfile,strtrim('Warning: EVI ND filter name is Unknown',2)
      printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    endif
    
    got_cm=0b
    ; Get the casing max from the Base_fix info.
    info = evi_headers.evi_base_fix_info
    match = -1
    for i=0,n_elements(info)-1 do if (strmatch(info[i],'*casing_max*')) then match=i
    if (match ge 0) then begin
      sf = strsplit(info[match],'=',/extract)
      CM = float(sf[1])
      got_cm=1b
    endif else begin
      print,'casing_max not found in evi header'
      print,'Output calibration may be invalid!'
      CM=132.77
      case filter_name of
        'ND0' : F_casing = 0.6858
        'ND015' : F_casing = 1.0
        'ND030' : F_casing = 2.25
        'ND1' : F_casing = 8.0
      else : F_casing = 3.4
    endcase
    CM=CM/F_casing
  endelse
  F_casing=132.77/CM
  ;
  if (~got_cm) then begin
    printf,tfile,strtrim('EVI file has NO casing information!',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  
  ; Check ancillary file
  ; First, if ancillary file names weren't given, we need to build them.
  if (ancfile[j_case] eq '') then begin
    if (~projected) then begin
      pos = strpos(f_base, 'cube_bil',/reverse_search)
      ancfile[j_case]=strtrim(f_path,2)+strmid(f_base,0,pos+8)+'_ancillary.img'
    endif else begin
      if (n_dot le 0) or (n_base-n_dot ne 4) then begin
        ancfile[j_case]=strtrim(f_path,2)+f_base+'_extrainfo'
      endif else begin
        ancfile[j_case]=strtrim(f_path,2)+strmid(f_base,0,n_dot)+'_extrainfo.img'
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
  
  if ((nl_anc ne nl) or (ns_anc ne ns) or (nb_anc lt 3)) then begin
    printf,tfile,strtrim('Ancillary data does not conform with input data',2)
    printf,tfile,'Ancillary File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  
  Mask=bytarr(ns,nl)
  ; Get the mask band
  dims = [-1, 0, ns-1, 0, nl-1]
  if (~projected) then begin
    Mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=6))
  endif else begin
    Mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=3))
  endelse
  envi_file_mng,id=ancillaryfile_fid,/remove
  pos_mask=where(Mask,num_mask)
  if (num_mask le 0) then begin
    printf,tfile,strtrim('The Mask band from an ancillary file is all zero !',2)
    printf,tfile,'Ancillary File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  Mask=0b
  pos_mask=0b
  
  ;Test the output files
  ; First, if output file names weren't given, we need to build them.
  if (outfile[j_case] eq '') then begin
    if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      outfile[j_case]=f_base+'_ptcld.txt'
    endif else begin
      outfile[j_case]=strmid(f_base,0,n_dot)+'_ptcld.txt'
    endelse
    outfile[j_case]=strtrim(f_path,2)+outfile[j_case]
  endif
  
  ; Create the output directory (if it already exists, this code will do nothing)
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
  
  ;check azimuth to north correct
  if ((evi_az_n[j_case] lt 0.0) or (evi_az_n[j_case] gt 360.0)) then begin
    printf,tfile,strtrim('EVI azimuth to North is out of bounds',2)
    printf,tfile,'Input File: '+strtrim(datafile[j_case],2)
    printf,tfile,'Error occurred for case '+case_add
    run_stat[j_case]=1
    goto, end_loop
  endif
  
  ; end of checking
  
  end_loop:
  flush, tfile
endfor

; Get disk required in GB
;gbsize = float(disk_needed)/(1024.0^3)

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
    'evi_point_cloud_batch terminating']
  for j=0,n_elements(info_text)-1 do begin
    print,info_text[j]
  endfor
  ;; result=dialog_message(info_text,/error,title='Errors in
  ;;evi_point_cloud_batch input')
  print, info_text
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
  nlist=1
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
  printf,tfile,'Calibration Flag List=',calib
  printf,tfile,' '
  printf,tfile,'evi_az_n List=',evi_az_n
  printf,tfile,' '
  if (add_evi gt 0) then begin
    printf,tfile,'Points for EVI base and top added for ALL output pt clouds'
  endif else begin
    printf,tfile,'Points for EVI base and top NOT added to output pt clouds'
  endelse
  printf,tfile,' '
  if (save_zero_hits gt 0) then begin
    printf,tfile,'Zero hits (pure gaps) saved for ALL output pt clouds'
  endif else begin
    printf,tfile,'Zero hits (pure gaps) NOT saved in output pt clouds'
  endelse
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
  
  cal_dat=calib[j_case]
  infile=datafile[j_case]
  anc_name=ancfile[j_case]
  ; Open input and ancillary files
  envi_open_file, infile, r_fid=fid,/no_realize
  envi_open_file, anc_name, r_fid=ancillaryfile_fid,/no_realize
  
  ; Get the file dimensions etc
  envi_file_query, fid, fname=infile, nb=nbands, nl=nlines, $
    ns=nsamples, bnames=bnames, wl=range, data_type=dt
  envi_file_query, ancillaryfile_fid, fname=anc_name, $
    nb=nb_anc, nl=nl_anc, ns=ns_anc, $
    data_type=anc_type, bnames=anc_bnames
    
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
  evi_year=fix(strtrim(strmid(evi_date_time,strlen(evi_date_time)-4),2))
  
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
  
  ;Locate the EVI Height
  EVI_Height=-1.0
  match = -1
  for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
    if (strmatch(evi_headers.evi_scan_info[i],'*EVI Height*')) then match=i
  endfor
  if (match ge 0) then begin
    sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
    EVI_Height = float(sf[1])
  endif else begin
    EVI_Height = -1.0
  endelse
  
  ; Check if file is pfiltered
  if (evi_headers.pfilter_present) then begin
    pfiltered=1b
  endif else pfiltered=0b
  
  evi_pointcloud_info=['Title=Point Cloud Information',$
    'EVI run Date & Time='+strtrim(evi_date_time,2),$
    'EVI run Description='+strtrim(evi_description_record,2),$
    'EVI Height='+strtrim(string(evi_height,format='(f10.3)'),2) $
    ]
    
  printf,tfile,'EVI run Date & Time='+strtrim(evi_date_time,2)
  printf,tfile,'EVI run Description='+strtrim(evi_description_record,2)
  printf,tfile,'EVI Height='+strtrim(string(evi_height,format='(f10.3)'),2)
  flush,tfile
  
  ; Check if file has been projected
  if (~evi_headers.proj_present) then begin
    projected = 0b
    projection_type='None'
  endif else begin
    projected = 1b
    match = -1
    for i=0,n_elements(evi_headers.evi_projection_info)-1 do begin
      if (strmatch(evi_headers.evi_projection_info[i],'*type*')) then match=i
    endfor
    if (match ge 0) then begin
      sf = strsplit(evi_headers.evi_projection_info[match],'=',/extract)
      Projection_Type = strtrim(sf[1],2)
    endif else begin
      Projection_Type='None'
    endelse
  endelse
  
  ; Check if file is apparent reflectance
  if (evi_headers.apprefl_present) then begin
    app_refl=1b
    cal_dat=0b
  endif else app_refl=0b
  
  ;Read the beam divergence from the scan info
  match = -1
  for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
    if (strmatch(evi_headers.evi_scan_info[i],'*Beam Divergence*')) then match=i
  endfor
  if (match ge 0) then begin
    sf = strtrim(strcompress(strsplit(evi_headers.evi_scan_info[match],'=',/extract)),2)
    t = strsplit(sf[1],' ',/extract)
    evi_div = float(t[0])
  endif else begin
    evi_div = -1.0
  endelse
  
  ;now get the filter name
  match = -1
  for i=0,n_elements(evi_headers.evi_base_fix_info)-1 do if (strmatch(evi_headers.evi_base_fix_info[i],'*Filter*')) then match=i
  if (match ge 0) then begin
    f_name = strsplit(evi_headers.evi_base_fix_info[match],'=',/extract)
    filter_name = f_name[1]
  endif else filter_name = 'unknown'
  
  ; Get the casing max from the Base_fix info.
  info = evi_headers.evi_base_fix_info
  match = -1
  for i=0,n_elements(info)-1 do if (strmatch(info[i],'*casing_max*')) then match=i
  if (match ge 0) then begin
    sf = strsplit(info[match],'=',/extract)
    CM = float(sf[1])
  endif else begin
    print,'casing_max not found in evi header'
    print,'Output calibration may be invalid!'
    CM=132.77
    case filter_name of
      'ND0' : F_casing = 0.6858
      'ND015' : F_casing = 1.0
      'ND030' : F_casing = 2.25
      'ND1' : F_casing = 8.0
    else : F_casing = 3.4
    endcase
    CM=CM/F_casing
  endelse
  F_casing=132.77/CM
  ;
  ;the calibration is now known for 2006 and 2009 but others
  ;are interpolated or extrapolated
  if (evi_year le 2006) then begin
    cal=270948.2
    rpow = 2.1543
    Rtef = 8.7148
  endif else if (evi_year ge 2009) then begin
    cal=273504.1
    rpow = 2.2651
    Rtef = 8.6975
  endif else begin
    cal=282696.2
    rpow = 2.2464
    Rtef = 8.7158
  endelse
  
  printf,tfile,'EVI beam divergence='+strtrim(string(evi_div,format='(f10.3)'),2)
  printf,tfile,'EVI casing factor='+strtrim(string(F_casing,format='(f10.3)'),2)
  printf,tfile,'EVI calibration Const='+strtrim(string(cal,format='(f10.3)'),2)
  printf,tfile,'EVI calibration range power='+strtrim(string(rpow,format='(f10.3)'),2)
  printf,tfile,'EVI calibration Teff='+strtrim(string(Rtef,format='(f10.3)'),2)
  printf,tfile,'EVI calibration Scale='+strtrim(string(scale_factor,format='(f10.3)'),2)
  flush,tfile
  
  evi_pointcloud_info=[evi_pointcloud_info,$
    'EVI beam divergence='+strtrim(string(evi_div,format='(f10.3)'),2),$
    'EVI casing factor='+strtrim(string(F_casing,format='(f10.3)'),2),$
    'EVI calibration Const='+strtrim(string(cal,format='(f10.3)'),2),$
    'EVI calibration range power='+strtrim(string(rpow,format='(f10.3)'),2),$
    'EVI calibration Teff='+strtrim(string(Rtef,format='(f10.3)'),2),$
    'EVI calibration Scale='+strtrim(string(scale_factor,format='(f10.3)'),2) $
    ]
    
  ;set up the calibration as far as possible
  if (cal_dat) then begin
    s_Factor=F_casing/(cal*scale_factor)
    i_scale=1000.0
    evi_cal=1b
  endif else begin
    s_Factor=1.0
    i_scale=1.0
    evi_cal=0b
  endelse
  
  mask=bytarr(nsamples,nlines)
  zenith=fltarr(nsamples,nlines)
  azimuth=fltarr(nsamples,nlines)
  
  ; Get the Mask, Zenith, Azimuth from the ancillary file
  dims = [-1, 0, nsamples-1, 0, nlines-1]
  if (~projected) then begin
    Mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=6))
    Zenith = float(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=7))/10.0
    Azimuth = float(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=8))/10.0
  endif else begin
    mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=3))
    zenith = float(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=1))/10.0
    azimuth = float(envi_get_data(fid=ancillaryfile_fid, dims=dims,pos=2))/10.0
  endelse
  envi_file_mng,id=ancillaryfile_fid,/remove
  
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
  i_val=i_val[[0,1,2,5,6]]
  t_val=t_val[[0,1,2,5,6]]
  r_val=r_val[[0,1,2,5,6]]
  
  ;S0 is a scale factor that relates the FWHM of the standard pulse and the mean FWHM of the data
  ;it has been chosen to be best for data in between ND015 (maybe 30% saturated) and ND100 (no saturated)
  ;
  S0=1.06  ;best for all but ND100
  
  evi_pointcloud_info=[evi_pointcloud_info,$
    'S0='+strtrim(string(s0,format='(f12.4)'),2) $
    ]
    
  ; Resample pulse onto time base of data
  w = where(range ge p_range[i_val[0]] and range le p_range[i_val[4]],numw)
  base_range=range[w[0]:w[numw-1]]/S0
  
  print,'number of elements in base range=',n_elements(base_range)
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
  
  ;options here are fixed for now
  ;; change accordingly to DWEL scans based on empirical tests.
  if (app_refl) then begin
    if (pfiltered) then b_thresh=0.002 else b_thresh=0.005
    i_scale=1000.0
    evi_cal=1b
  endif else begin
    if (pfiltered) then b_thresh=25 else b_thresh=25
    i_scale=1.0
  ;  evi_cal=0b
  endelse
  ;evi_az_north=0.0
  max_zen_ang=125.0
  azimuth=azimuth-evi_az_n[j_case] ;EVI, rotation direction is clockwise, the same as the direction that true azimuth is count
  ;azimuth=evi_az_n[j_case]-azimuth ;DWEL? rotation direction is counter-clockwise
  pos=where(azimuth lt 0.0,npos)
  if (npos gt 0) then azimuth[pos]=azimuth[pos]+360.0
  pos=0b
  pos=where(zenith gt max_zen_ang,num_zen)
  if (num_zen gt 0) then begin
    mask[pos]=0b
  endif
  pos=0b
  pos_mask=where(Mask,num_mask)
  max_zenith=max(zenith[pos_mask])
  pos_mask=0b
  
  evi_pointcloud_info=[evi_pointcloud_info,$
    'B_Thresh='+strtrim(string(b_thresh),2) $
    ]
    
  ; ***********************
  ; Get path and file name as separate strings
    
  last=strpos(infile,path_sep(),/reverse_search)
  in_path=file_dirname(infile)
  in_base=strtrim(strmid(infile,last+1,strlen(infile)-last-1),2)
  
  last=strpos(anc_name,path_sep(),/reverse_search)
  anc_path = file_dirname(anc_name)
  anc_base=strtrim(strmid(anc_name,last+1,strlen(anc_name)-last-1),2)
  
  ;Set up the metadata elements
  
  Processing_Date_Time=''
  Run_Number=j_case+1
  Description=evi_description_record
  Input_Path=in_path
  Input_File=in_base
  Acquisition_Date_Time=Evi_Date_Time
  Ancillary_Path=anc_path
  Ancillary_File=anc_base
  Projection=Projection_Type
  EVI_Calibration=evi_cal
  EVI_Height=EVI_Height
  EVI_Az_North=evi_az_n[j_case]
  Max_Zenith_Angle=Max_Zenith
  Range_Step=abs(range[1]-range[0])
  Threshold=b_thresh
  Zero_Hit_Option=save_zero_hits
  Add_EVI=Add_EVI
  X_scale=1.0
  Y_scale=1.0
  Z_scale=1.0
  X_offset=0.0
  Y_offset=0.0
  Z_offset=0.0
  I_Scale=i_scale
  Point_File_Path=''
  Point_File_name=''
  Zero_Hit_Number=0L
  Shot_Hit_Number=0L
  Total_Hit_Number=0L
  Nrecs=0L
  Max_X=0.0
  Min_X=0.0
  Max_Y=0.0
  Min_Y=0.0
  Max_Z=0.0
  Min_Z=0.0
  Min_Intensity=0.0
  Max_Intensity=0.0
  
  ;***************
  ;now set up structures and push onto the stack
  
  meta_data={ $
    Processing_Date_Time:Processing_Date_Time,$
    Run_Number:Run_Number,$
    Description:Description,$
    Input_Path:Input_Path,$
    Input_File:Input_File,$
    Acquisition_Date_Time:Acquisition_Date_Time,$
    Ancillary_Path:Ancillary_Path,$
    Ancillary_File:Ancillary_File,$
    Projection:Projection,$
    EVI_Calibration:EVI_Calibration,$
    EVI_Height:EVI_Height,$
    EVI_Az_North:EVI_Az_North,$
    Max_Zenith_Angle:Max_Zenith_Angle,$
    Range_Step:Range_Step,$
    Threshold:Threshold,$
    Zero_Hit_Option:Zero_Hit_Option,$
    Add_EVI:Add_EVI,$
    X_scale:X_scale,$
    Y_scale:Y_scale,$
    Z_scale:Z_scale,$
    X_offset:X_offset,$
    Y_offset:Y_offset,$
    Z_offset:Z_offset,$
    I_Scale:I_Scale,$
    Point_File_Path:point_file_Path,$
    Point_File_name:point_file_name,$
    Zero_Hit_Number:Zero_Hit_Number,$
    Shot_Hit_Number:Shot_Hit_Number,$
    Total_Hit_Number:Total_Hit_Number,$
    Nrecs:Nrecs,$
    Max_X:Max_X,$
    Min_X:Min_X,$
    Max_Y:Max_Y,$
    Min_Y:Min_Y,$
    Max_Z:Max_Z,$
    Min_Z:Min_Z,$
    Min_Intensity:Min_Intensity,$
    Max_Intensity:Max_Intensity $
    }
    
  pb_meta=ptr_new(meta_data,/no_copy)
  
  out_file=strtrim(outfile[j_case],2)
  n_base=strlen(out_file)
  n_dot=strpos(out_file,'.',/reverse_search)
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
    oi_name=out_file+'_pcinfo.img'
  endif else begin
    oi_name=strmid(out_file,0,n_dot)+'_pcinfo.img'
  endelse
  oi_name=strtrim(oi_name,2)
  
  ;see if the output image file exists & remove if it does!
  if(file_test(oi_name)) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, oi_name,/quiet
      print,'old image file deleted'
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(oi_name),2) eq $
          strtrim(strlowcase(tname),2)) then begin
          envi_file_mng,id=fids[i],/remove
          print,'old image file removed from ENVI'
        endif
      endfor
      file_delete, oi_name,/quiet
      print,'old image file deleted'
    endelse
  endif
  
  base_stats={ $
    fid:fid,$
    outfile:out_file,$
    oi_name:oi_name,$
    range:range,$
    mask:mask,$
    zenith:zenith,$
    azimuth:azimuth,$
    cal_dat:cal_dat,$
    evi_div:evi_div,$
    rpow:rpow,$
    Rtef:Rtef,$
    s_Factor:s_Factor $
    }
    
  pb_stats=ptr_new(base_stats,/no_copy)
  pb_info=''
  
  ;Apply the point cloud processing
  ;********************************************
  err=0b
  apply_ptcl_filter_batch_cmd, p, pb_stats, pb_meta, pb_info, error=err
  ;********************************************
  
  if (err ne 0b) then begin
    printf,tfile,'Error in point cloud processing at case='+strtrim(string(j_case+1,format='(i8)'),2)
    printf,tfile,'Error called from apply_ptcl_filter'
    flush,tfile
    print,'Error in point cloud processing at case='+strtrim(string(j_case+1,format='(i8)'),2)
    print,'Error called from apply_ptcl_filter'
    ptr_free,pb_stats
    ptr_free,pb_meta
    goto,out
  endif
  
  evi_pointcloud_info=[evi_pointcloud_info,pb_info]
  pb_info=''
  evi_pointcloud_info=[evi_pointcloud_info,$
    'PointCloud Info File='+strtrim(oi_name,2),$
    'Output PointCloud File='+strtrim(out_file,2) $
    ]
    
  tname=(*pb_stats).oi_name
  ;write out header for info image
  descrip='Info image for Point Cloud '+strtrim(out_file,2)
  bnames=['Nhits','Total d','Total d0','Total I','Total I2','Mean_Range*100','Zenith*10','Azimuth*10','Residual','Gap','Mask']
  envi_setup_head,fname=tname,ns=nsamples,nl=nlines,nb=11,$
    xstart=0,ystart=0,$
    data_type=2, interleave=0, bnames=bnames, $
    descrip=descrip, /write
    
  envi_open_file,(*pb_stats).oi_name,r_fid=anc_fid,/no_interactive_query,/no_realize
  ;write out the previous header records
  status=put_headers(anc_fid,evi_headers)
  envi_assign_header_value, fid=anc_fid, keyword='EVI_pointcloud_info', $
    value=evi_pointcloud_info
    
  envi_write_file_header, anc_fid
  envi_file_mng,id=anc_fid,/remove
  anc_fid=0b
  
  ptr_free,pb_stats
  ptr_free,pb_meta
  
  ; **************
  ; Remove all remaining file handles from ENVI
  envi_file_mng, id=ancillaryfile_fid, /remove
  envi_file_mng, id=fid, /remove
  ancillaryfile_fid=0b
  fid=0b
  if(ptr_valid(pb_stats)) then ptr_free,pb_stats
  if(ptr_valid(pb_meta)) then ptr_free,pb_meta
  if(ptr_valid(p_stat)) then ptr_free,p_stat
  pb_stats=0b
  pb_meta=0b
  p_stat=0b
  ; **************
  print,'Completed point_cloud for case '+strtrim(string(j_case+1,format='(i8)'),2)
  print,''
  
  printf,tfile,'Completed point_cloud for case '+strtrim(string(j_case+1,format='(i8)'),2)
  printf,tfile,'Output File: '+strtrim(outfile[j_case],2)
  printf,tfile,' '
  printf,tfile,'*************************************'
  printf,tfile,' '
  flush, tfile
  ;Here is the end of the input file Loop!!!!!
  endfor
  
  T_end=systime(1)
  printf,tfile,' '
  printf,tfile,'point_cloud batch Run finished at '+strtrim(systime(),2)
  printf,tfile,'Elapsed Time was '+strtrim(string(float(T_end-T_start)/60.0,format='(f12.3)'),2)+' minutes'
  flush,tfile
  
  print,'point_cloud complete'
  
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
  
  if(ptr_valid(pb_stats)) then ptr_free,pb_stats
  if(ptr_valid(pb_meta)) then ptr_free,pb_meta
  if(ptr_valid(p_stat)) then ptr_free,p_stat
  pb_stats=0b
  pb_meta=0b
  p_stat=0b
  
  datafile=0b
  anc_name=0b
  outfile=0b
  run_stat=0b
  info_text=0b
  
  heap_gc,/verbose
  
  return

end

;======================================================================
