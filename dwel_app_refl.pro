;======================================================================
;  The following set of procedures is part of the EVI processing chain.
;  They follow the structure of the CSIRO EOC Hyperion Workshop module set.
;  The routines all have a similar structure.
;  They can be used stand-alone or as part of a Project structure with
;  an ENVI main menu button.
;
;  The general structure for a procedure called "name" is:
;
;  [Procedures called from name-doit]
;    These may be called anything
;    they are arranged so that every procedure occurs before its first
;    reference. This is good IDL practice. They all use the compile_opt idl2
;    convention.
;  pro name_doit
;    This is the main area of action for the algorithm - the DOIT!
;  [Procedures supporting the general top level procedure]
;      These procedures are not part of the algorithm - just the framework.
;      The workshop routines have a common top level look and feel.
;    pro name_banner_text
;      Top level widget Help and how to use the module
;    pro name_disclaimer
;      General disclaimer to all responsibility and blame accessed
;      from main widget
;    pro name_action_go
;      Main widget event handler
;    pro name_action_exit
;      Another event handler
;    pro name_resize
;      Yet another event handler
;    pro name_cleanup
;      Will they never end?
;    pro name
;      This is the common top level procedure that simply sets up an
;      IDL widget structure that allows the user to look at the main
;      help and if desired hit GO and start the module. It handles
;      exit nicely.
;======================================================================
; History
; 28/4/11 Modified input widget to allow power of range to be set interactively (i.e. non 1/r^2)

pro button_value, event
  compile_opt idl2

  common button, tick
  tick=event.select
end

pro cbutton_value, event
  compile_opt idl2

  common cbutton, ctick
  ctick=event.select
end

function set_apprefl_params, pstat
compile_opt idl2

; Set up a common block to return the value of a tickbox in the widget.
; Value must be set to zero to default values is (zero=unticked).
common button, tick
common cbutton, ctick
tick=1
ctick=0

;first set up the widget base and user help
wb_set_params=widget_auto_base( $
  title='Set parameters for apparent reflectance calculation', $
  group=(*pstat).top)

Info_Text=[ $
'This widget reports values read from the header', $
'and requests user input on various calibration and', $
'parameter constants used in the processing',$
' ',$
'Items on the LHS have been read from the header.', $
'Beam divergence has been placed in an editable box,', $
'as the user may wish to round it. This will affect,', $
'the K(r) values.', $
' ', $
'Items on the RHS are parameter values set to defaults:', $
'Calibration constant: empirical value of CIshot obtained from trunk data', $
'Pulse FWHM: based on standard pulse model', $
'Vegetation reflectance: assumed value allowing calculation of obscuration extent', $
'Pwer of R used in calculation', $
'Output scale: scale factor to be applied to output data for integer conversion.' $
]

;put in the help and some next level bases to use
;for input info
w_param_txt=widget_slabel(wb_set_params,prompt=Info_Text,$
  /frame,xsize=60,ysize=5)

wb_lower=widget_base(wb_set_params,/row,/frame)

wb_set_par_1=widget_base(wb_lower,/column,/frame)
wb_set_par_2=widget_base(wb_lower,/column,/frame)
wb_set_par_3=widget_base(wb_lower,/column,/frame)

; display the relevant data parameters
w_div=widget_param(wb_set_par_1,/auto_manage,default=(*pstat).def_div,dt=4,$
         Prompt='Beam Divergence (mrad): ', $
         uvalue='div',floor=(*pstat).div_floor,ceil=(*pstat).div_ceil,field=2)
w_fname=widget_label(wb_set_par_1,/align_left, $
     value=(*pstat).f_name)

;set the processing parameters
w_cal=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).def_cal,dt=4,field=2,$
         Prompt='Calibration constant: ', $
         uvalue='cal',floor=(*pstat).cal_floor,ceil=(*pstat).cal_ceil)
w_fwhm=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).def_fwhm,dt=4,$
         field=4, Prompt='Pulse FWHM: ', $
         uvalue='fwhm',floor=(*pstat).fwhm_floor,ceil=(*pstat).fwhm_ceil)
w_refl=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).def_refl,dt=4,$
         Prompt='Vegetation reflectance: ', $
         uvalue='refl',floor=(*pstat).refl_floor,ceil=(*pstat).refl_ceil,field=2)
w_rpow=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).def_rpow,dt=4,$
         Prompt='Power of R: ', $
         uvalue='rpow',floor=(*pstat).rpow_floor,ceil=(*pstat).rpow_ceil,field=2)
w_outscale=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).outscale,dt=2, $
     Prompt='Output scale factor: ',$
     uvalue='outscale',floor=(*pstat).outscale_floor,ceil=(*pstat).outscale_ceil,field=2)
w_pstart=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).pstart,dt=2, $
     Prompt='Start Index for pulse: ',$
     uvalue='pstart',floor=0,ceil=1000,field=2)
w_pstop=widget_param(wb_set_par_2,/auto_manage,default=(*pstat).pstop,dt=2, $
     Prompt='Stop Index for pulse: ',$
     uvalue='pstop',floor=1,ceil=1000,field=2)
wb_set_par_2_lower=widget_base(wb_set_par_2,/column,/nonexclusive)
w_clip=widget_button(wb_set_par_2_lower,value='Clip output to 1.0',$
      event_pro='button_value',uvalue='clip')
widget_control, w_clip, /set_button
w_conv=widget_button(wb_set_par_2_lower,value='Convolve K(R) with pulse?',$
      event_pro='cbutton_value',uvalue='conv')
widget_control, w_conv, sensitive=0 ; disable the use of K(R)


;write a warning about calibration
warning=['Default calibration constant is for upgraded EVI.', $
         'values are correct for filters: ', $
         '   ND0', $
         '   ND015', $
         '   ND030', $
         '   ND1 ', $
         'other filters have defaulted to the old calibration', $
         'and should be set by the user']
w_warn=widget_slabel(wb_set_par_3,prompt=warning)

;realise the widget using ENVI auto-manage
result=auto_wid_mng(wb_set_params)

;result is a structure - see if setup cancelled
if (result.accept eq 0) then return,0b

(*pstat).div=result.div
(*pstat).cal=result.cal
(*pstat).fwhm=result.fwhm
(*pstat).refl=result.refl
(*pstat).rpow=result.rpow
(*pstat).outscale=result.outscale
(*pstat).pstart=result.pstart ; added by Zhan on 05102013
(*pstat).pstop=result.pstop ; added by Zhan on 05102013
(*pstat).conv=ctick
(*pstat).clip=tick
return,1b

end

pro evi_app_refl_doit, event

  compile_opt idl2

;clean up any fids which are no longer where they were!
;ENVI issue that is annoying and leads to confusion
clean_envi_file_fids

  ; Select the input file - either from within ENVI or by opening the file
  infile_select:
  envi_select, /file_only, fid=fid, file_type=0,/no_dims, /no_spec, $
    title='Select EVI file for processing to apparent reflectance'
  if (fid eq -1) then begin
      result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='No file selected or operation cancelled !')
      if(result eq 'No') then goto, cleanup else goto, infile_select
  endif

  ; Get the file dimensions etc
  envi_file_query, fid, fname=infile, nb=nbands, nl=nlines, $
  ns=nsamples, bnames=bnames, wl=range

  n_base=strlen(infile)
  n_dot=strpos(infile,'.',/reverse_search)
  f_path = file_dirname(infile)

;get image name as separate string
last=strpos(infile,path_sep(),/reverse_search)
f_base=strtrim(strmid(infile,last+1,n_base-last-1),2)

;set up a base structure for the EVI headers
  evi_headers={ $
     f_base:f_base $
     }
;find all of the EVI headers in the hdr file as defined by FID
  status=get_headers(fid,evi_headers)

;respond to major problems!
  if (not status) then begin
    result=dialog_message('Bad FID for selected file - Try again ? (Yes.No)',/question,$
      title='bad FID in EVI get_headers on input file')
    envi_file_mng,id=fid,/remove
    if (result eq 'Yes') then begin
      goto, infile_select
    endif else begin
      goto, cleanup
    endelse
  endif
  if (evi_headers.headers_present le 0s or not evi_headers.run_present) then begin
    result=dialog_message('Selected file invalid EVI file - Try again ? (Yes.No)',/question,$
      title='Input File is NOT a valid EVI file')
    envi_file_mng,id=fid,/remove
    if (result eq 'Yes') then begin
      goto, infile_select
    endif else begin
      goto, cleanup
    endelse
  endif

; Check if file is apparent reflectance
if (evi_headers.apprefl_present) then begin
  info_text=[ $
  'File header indicates EVI data are apparent reflectance data.',$
  'Apparent Reflectance data have already been calibrated and',$
  'attempting to use these data for calibration will not work.',$
  '',$
  'Really use this file for Calibration ? (No/Yes) ' $
  ]
  result=dialog_message(info_text,/question,$
  title='File is already calibrated!')
  if(result eq 'No') then goto, infile_select
endif

;Get date and time of the acquisition
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
 
 print,'evi year=',evi_year

;Get the site description
 match = -1
 for i=0,n_elements(evi_headers.evi_scan_info)-1 do begin
   if (strmatch(evi_headers.evi_scan_info[i],'*Scan Description*')) then match=i
 endfor
 if (match ge 0) then begin
   sf = strsplit(evi_headers.evi_scan_info[match],'=',/extract)
   if (n_elements(sf) gt 1) then begin
     Description_record = strtrim(strcompress(sf[1]),2)
   endif else begin
     Description_record = ''
   endelse
 endif else begin
   Description_record = ''
 endelse
 
;check for processing Level
level1='File has been baseline fixed'
level2='File has been saturation fixed'
level3='File has been filtered for noise'
if (~(evi_headers.base_present) or ~(evi_headers.sat_present) or $
    ~(evi_headers.pfilter_present)) then begin
  if (~(evi_headers.base_present)) then level1='File has NOT been baseline fixed'
  if (~(evi_headers.sat_present)) then level2='File has NOT been saturation fixed'
  if (~(evi_headers.pfilter_present)) then level3='File has NOT been noise filtered'
  info_text=[ $
  'File header indicates EVI data may not have been processed to the',$
  'recommended Level for Calibration. The recommended Level is for',$
  'EVI data to be baseline fixed, saturation fixed and filtered.',$
  'For the File '+f_base+':',$
  level1,level2,level3,$
  '',$
  'Really use this file for Calibration ? (No/Yes) ' $
  ]
  result=dialog_message(info_text,/question,$
  title='File has NOT been processed to recommended level')
  if(result eq 'No') then goto, infile_select
endif

;help,evi_headers,/structure
;print,'listing of diagnostics'
;print,'number of diagnostic entries=',n_elements(evi_headers.evi_diagnostic_info)
;print,''
;for i=0,n_elements(evi_headers.evi_diagnostic_info)-1 do begin
;  print,evi_headers.evi_diagnostic_info[i]
;endfor

;check if file has been projected
if (not evi_headers.proj_present) then begin
     projected=0b
     ; Find ancillary file and read mask
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      anc_name=strtrim(infile,2)+'_ancillary'
  endif else begin
      anc_name=strmid(infile,0,n_dot)+'_ancillary.img'
  endelse

  if(not file_test(anc_name)) then begin
    get_anc:
      message_text=[ $
        'Default ancillary file is not present',$
        'default name: '+strtrim(anc_name,2),$
        'Find an ancillary File ? (Yes) or Re-start (No)?' $
        ]
      result=dialog_message(message_text,/question,$
        title='Default ancillary file is NOT present')
      if(result eq 'No') then goto,infile_select
      file = dialog_pickfile(title='Select ancillary evi_file', $
            file=anc_name,path=f_path, /must_exist)
    ;check for error or cancel button hit
      if (file eq '') then begin
        result=dialog_message('Try again ? (No/Yes) ',/question,$
        title='No file selected or operation cancelled !')
        if(result eq 'No') then goto, cleanup else goto, get_anc
      endif
      anc_name=file
  endif

  envi_open_file, anc_name, r_fid=ancillaryfile_fid,/no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
      result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='Error Opening Ancillary Data File !')
      if(result eq 'No') then goto, cleanup else goto, get_anc
  endif

  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, $
          data_type=anc_type, bnames=anc_bnames

  if ((nl_anc ne nlines) or (ns_anc ne nsamples) or (nb_anc lt 3)) then begin
    result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='Ancillary Data File does NOT conform with current EVI Cube !')
      envi_file_mng,id=ancillaryfile_fid,/remove
      if(result eq 'No') then goto, cleanup else goto, infile_select
  endif

  ; Get the mask
  dims = [-1, 0, nsamples-1, 0, nlines-1]
  mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=6))
endif else begin
     projected=1b
     ; check this isn't a cyllindrical projection
     cyl = envi_get_header_value(in_fid, 'EVI_cylindrical_projection_info',undefined=undef)
     if (not undef) then begin
      result=dialog_message('Selected file has an invalid projection - Try again ? (Yes/No)',/question,$
        title='Input File is NOT a valid EVI file')
      envi_file_mng,id=fid,/remove
      if (result eq 'Yes') then begin
          goto, infile_select
      endif else begin
          goto, cleanup
      endelse
     endif
     ; Find extrainfo file and read mask
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      anc_name=strtrim(infile,2)+'_extrainfo'
  endif else begin
      anc_name=strmid(infile,0,n_dot)+'_extrainfo.img'
  endelse

  if(not file_test(anc_name)) then begin
    get_xinfo:
      message_text=[ $
        'Default extrainfo file is not present',$
        'default name: '+strtrim(anc_name,2),$
        'Find an extrainfo File ? (Yes) or Re-start (No)?' $
        ]
      result=dialog_message(message_text,/question,$
        title='Default extrainfo file is NOT present')
      if(result eq 'No') then goto,infile_select
      file = dialog_pickfile(title='Select extrainfo evi_file', $
            file=anc_name,path=f_path, /must_exist)
    ;check for error or cancel button hit
      if (file eq '') then begin
        result=dialog_message('Try again ? (No/Yes) ',/question,$
        title='No file selected or operation cancelled !')
        if(result eq 'No') then goto, cleanup else goto, get_xinfo
      endif
      anc_name=file
  endif

  envi_open_file, anc_name, r_fid=ancillaryfile_fid,/no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
      result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='Error Opening Ancillary Data File !')
      if(result eq 'No') then goto, cleanup else goto, get_xinfo
  endif

  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, $
          data_type=anc_type, bnames=anc_bnames

  if ((nl_anc ne nlines) or (ns_anc ne nsamples) or (nb_anc lt 3)) then begin
    result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='Ancillary Data File does NOT conform with current EVI Cube !')
      envi_file_mng,id=ancillaryfile_fid,/remove
      if(result eq 'No') then goto, cleanup else goto, infile_select
  endif

  ; Get the mask
  dims = [-1, 0, nsamples-1, 0, nlines-1]
  mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=dims, pos=3))
endelse

; check range values to see if data have been resampled. If so, issue warning.
  bl_correct=0b
  test = mean(range[1:*]-shift(range[0:nbands-2],1))
  samp_rate_text = evi_headers.evi_scan_info[17]
  t = strsplit(samp_rate_text, ' ', /extract)
  sr = float(t[4])
  rstep = 0.5*2.998e8/(sr*1.0e9)
  if (abs(rstep-test) gt 0.001) then begin
    result=dialog_message('Really use this file ? (No/Yes) ',/question,$
    title='File header indicates EVI data have been resampled.')
    if(result eq 'No') then goto, infile_select
    rstep=test
  endif else bl_correct=1b
  
; Read the beam divergence from the scan info
; bd_text = evi_headers.evi_scan_info[9]
; t = strsplit(bd_text,' ',/extract)
; evi_div = float(t[3])

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

  print,'evi_div=',evi_div

if (evi_div lt 0.0) then begin
  print,'evi divergence problem! evi_div=',evi_div
endif

if (not bl_correct) then begin
  scale_factor=1.0
  filter_name='unknown'
endif else begin
  ; Read the scale factor and ND filter name from base_fix
  ; It is possible (but unlikely) for scale_factor and ND filter name to absent
  ; from the header therefore search for them rather than using an explicit
  ; index into the array.
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
endelse

print,'scale_factor=',scale_factor
print,'filter name=',filter_name

  match = -1
  for i=0,n_elements(evi_headers.evi_base_fix_info)-1 do if (strmatch(evi_headers.evi_base_fix_info[i],'*casing_fwhm(m)*')) then match=i
  if (match ge 0) then begin
    f_name = strsplit(evi_headers.evi_base_fix_info[match],'=',/extract)
    casing_fwhm = float(f_name[1])
  endif else begin
    casing_fwhm=2.1623
    print,'casing fwhm NOT found!'
  endelse

print,'casing fwhm(m)=',casing_fwhm
print,'local fwhm(samples)=',casing_fwhm/rstep

if (strlowcase(filter_name) eq 'unknown') then begin
  info_text=[$
  'The ND Filter has not been set during processing',$
  'of the current input file.',$
  'The Apparent Reflectance processing needs this to be set',$
  '',$
  'Try another file ? (No/Yes) ',$
  '' $
  ]
  result=dialog_message(info_text,/question,$
  title='The ND Filter has NOT been set')
  if(result eq 'Yes') then goto, infile_select
  goto,cleanup
endif

got_cm=0b

if (~(evi_headers.base_present) or (evi_year lt 2006)) then begin
  CM=132.77
  F_casing=1.0
endif else begin
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
    'ND030' : F_casing = 1.86
    'ND1' : F_casing = 5.897
    else : F_casing = 3.4
    endcase
    CM=CM/F_casing
  endelse
  F_casing=132.77/CM
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;change for DWEL
  F_casing=1.0
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
print,'casing max=',CM
print,'DWEL casing_scale, constant now=',F_casing

if (~got_cm) then begin
info_text=[$
'The Casing Maximum not found in headers',$
'File must be an old version (before 2006) or not correct',$
'Calibration may be meaningless.',$
'',$
'Try a new input file ? (Yes/No)']
  result=dialog_message(info_text,/question,$
    title='Casing Maximum not found in headers')
  envi_file_mng,id=fid,/remove
  envi_file_mng,id=ancillaryfile_fid,/remove
  if (result eq 'Yes') then begin
    goto, infile_select
  endif else begin
    goto, cleanup
  endelse
endif
endelse

; Set default parameter values
  div_floor=1.0
  div_ceil=20.0
  def_div = evi_div
  cal_floor = 1
  cal_ceil = 1e7
; This cal value for original EVI data
; def_cal = 107931.0
; These cal values for EVI data after receiver upgrade and using standard filters.
; case filter_name of
; 'ND0' : def_cal = 540178.0
; 'ND015' : def_cal = 370462.0
; 'ND030' : def_cal = 164990.0
; 'ND1' : def_cal = 45835.0
; else : def_cal = 107931.0
; endcase

;the calibration is now known for 2006 and 2009 but others
;are interpolated or extrapolated
if (evi_year le 2006) then begin
  def_cal=270948.2
  def_rpow = 2.1543
  def_Rtef = 8.7148
endif else if (evi_year ge 2009) then begin
  def_cal=273504.1
  def_rpow = 2.2651
  def_Rtef = 8.6975
endif else begin
  def_cal=282696.2
  def_rpow = 2.2464
  def_Rtef = 8.7158
endelse
  cal=def_cal
  rpow=def_rpow
  Rtef=def_Rtef
  const=cal
;NOTE: fwhm is in samples and not in metres
  def_fwhm = casing_fwhm/rstep
  fwhm=def_fwhm
  def_refl = 0.45
  refl=def_refl
  def_outscale = 1000.0

;set some basic limits
  f_name = 'Filter: '+filter_name
  fwhm_floor = 0.01
  fwhm_ceil = 100.0
  refl_floor = 0.01
  refl_ceil = 1.01
  rpow_floor = 1.0
  rpow_ceil = 3.0
  outscale_floor = 1.0
  outscale_ceil = 1.0e6
  conv = 0
  clip = 0
  pstart = 16
  pstop = 240

  ; set up processing parameters with user input
  set_params:

  ;set up a structure and push it onto the heap
  ;NOTE: R_tef is currently not able to be changed
  sav={ $
       div:def_div,$
      div_floor:div_floor,$
      div_ceil:div_ceil,$
      def_div:def_div,$
      f_name:f_name, $
      cal:def_cal,$
      cal_floor:cal_floor,$
      cal_ceil:cal_ceil,$
      def_cal:def_cal,$
      fwhm:def_fwhm,$
      fwhm_floor:fwhm_floor,$
      fwhm_ceil:fwhm_ceil,$
      def_fwhm:def_fwhm,$
      refl:def_refl,$
      refl_floor:refl_floor,$
      refl_ceil:refl_ceil,$
      def_refl:def_refl,$
      rpow:def_rpow,$
      rpow_floor:rpow_floor,$
      rpow_ceil:rpow_ceil,$
      def_rpow:def_rpow,$
      outscale:def_outscale,$
      outscale_floor:outscale_floor,$
      outscale_ceil:outscale_ceil,$
      def_outscale:def_outscale,$
      conv:conv,$
      clip:clip,$
      pstart:pstart,$
      pstop:pstop,$
      top:event.top $
      }

  ;now locate the data on the heap with a pointer
  p_stat=ptr_new(sav,/no_copy)

  ;call to widget menu function to set parameter values
  istat = set_apprefl_params(p_stat)

  if (not istat) then begin
      ptr_free, p_stat
      result=dialog_message('No Parameters set - Restart ? ',/question,$
      title='Parameter setup cancelled', $
      dialog_parent=event.top)
      if(result eq 'No') then begin
        goto, cleanup
      endif else goto, infile_select
  endif

  ;set up the parameter variables and store other necessary data from pstat
  beam_div = (*p_stat).div
  const = (*p_stat).cal
  pulse_fwhm = (*p_stat).fwhm
  out_scale = float((*p_stat).outscale)
  def_refl = (*p_stat).refl
  rpow = (*p_stat).rpow
  conv_eff = (*p_stat).conv
  clip_output = (*p_stat).clip
  pstart = (*p_stat).pstart
  pstop = (*p_stat).pstop
  ptr_free, p_stat
  ; Swap pstart and pstop if their sizes are inconsistent
  if (pstart gt pstop) then begin
    temp = pstart
    pstart = pstop
    pstop = temp
  endif

  ; Find pulse file if needed for convolution
  if (conv_eff) then begin
    ; Choose pulse file for convolution with efficiency function
    choose_file:

    envi_set_path,save_add_path,/no_set
    save_add_path=save_add_path+'\save_add'

    pfile_name = 'MeanPulse.txt'
    pfile_name=dialog_pickfile(title='Select EVI Pulse (.txt) file for convolution with K(R)',$
          filter='*.txt',file=pfile_name, $
          dialog_parent=event.top, path=save_add_path)
    if (pfile_name eq '') then begin
        result=dialog_message('Try again ? (No/Yes) ',/question,/default_no, $
        title='No EVI Pulse file selected or selection cancelled !',$
        dialog_parent=event.top)
        if(result eq 'No') then begin
          goto, cleanup
      endif else goto, choose_file
    endif
  endif

  ; Construct default names for output files
  ; Get path and file name as separate strings and construct output
  ; file name
  last=strpos(infile,path_sep(),/reverse_search)
  in_path=strmid(infile,0,last+1)
  in_base=strtrim(strmid(infile,last+1,strlen(infile)-last-1),2)
  dot = strpos(in_base,'.',/reverse_search)
  if (dot gt 0) then begin
    o_base = strmid(in_base,0,dot)+'_apprefl.img'
  endif else begin
    o_base = in_base+'_apprefl.img'
  endelse
  outfile = in_path+path_sep()+o_base
  
  last=strpos(infile,path_sep(),/reverse_search)
  in_path=strmid(infile,0,last+1)
  in_base=strtrim(strmid(infile,last+1,strlen(infile)-last-1),2)
  n_base=strlen(in_base)
  n_dot=strpos(in_base,'.',/reverse_search)
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
    o_base = in_base+'_apprefl.img'
  endif else begin
    o_base=strmid(in_base,0,dot)+'_apprefl.img'
  endelse
  outfile = strtrim(in_path+o_base,2)
  
  ; Select output file name
  outfile_select:
  outfile = dialog_pickfile(title='Select name for output Apparent Reflectance file', $
          file=o_base,path=in_path,  /overwrite_prompt, /write)
  ;check for error or cancel button hit
  if (outfile eq '') then begin
      result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='No output file selected or operation cancelled !')
      if(result eq 'No') then goto, cleanup else goto, outfile_select
;  endif
;The output should NOT be the input file
  endif else if (strtrim(strlowcase(outfile),2) eq $
    strtrim(strlowcase(infile),2)) then begin
    info_text=[strtrim('File name conflict',2),$
               strtrim('Output cannot be the Input !',2),$
               strtrim('Try another selection',2)]
    result=dialog_message(info_text,/error,title='Invalid Input')
    outfile=''
    goto, outfile_select
  endif

; check if we need to close and delete outfile
if(file_test(outfile)) then begin
    fids=envi_get_file_ids()
    if(fids[0] eq -1) then begin
      file_delete, outfile,/quiet
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(outfile),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
        endif
      endfor
      file_delete, outfile,/quiet
    endelse
endif

  ; Construct default names for the rest of the output files
  ; Get path and file name as separate strings and construct output
  ; file name

  last=strpos(outfile,path_sep(),/reverse_search)
  out_path=strmid(outfile,0,last+1)
  out_base=strtrim(strmid(outfile,last+1,strlen(outfile)-last-1),2)
  dot = strpos(out_base,'.',/reverse_search)
  if (dot gt 0) then begin
    pg_base = strmid(out_base,0,dot)+'_pgap.img'
    mask_base = strmid(out_base,0,dot)+'_bad_mask.img'
  endif else begin
    pg_base = out_base+'_pgap.img'
    mask_base = out_base+'_bad_mask.img'
  endelse
  pgapfile = strtrim(out_path+pg_base,2)
  maskfile = strtrim(out_path+mask_base,2)

; check if we need to close and delete pgapfile
if(file_test(pgapfile)) then begin
    fids=envi_get_file_ids()
;    print,'pgap fids=',fids
    if(fids[0] eq -1) then begin
      file_delete, pgapfile,/quiet
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(pgapfile),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
;              print,'pgapfile fid removed'
        endif
      endfor
      file_delete, pgapfile,/quiet
    endelse
endif

; check if we need to remove maskfile fid
if(file_test(maskfile)) then begin
    fids=envi_get_file_ids()
;    print,'mask fids=',fids
    if(fids[0] eq -1) then begin
      file_delete, maskfile,/quiet
    endif else begin
      for i=0,n_elements(fids)-1 do begin
        envi_file_query,fids[i],fname=tname
        if (strtrim(strlowcase(maskfile),2) eq $
            strtrim(strlowcase(tname),2)) then begin
            envi_file_mng,id=fids[i],/remove
;              print,'maskfile fid removed'
        endif
      endfor
      file_delete, maskfile,/quiet
    endelse
endif

  ; now temporarily get wavelength from the filename
  ; later need to add a header information of laser wavelength to the hdr file
  ; the wavelength will be extracted from the header information. 
  if (strpos(evi_headers.f_base, '1064') ne -1) then begin
    wavelength = 1064
  endif
  if (strpos(evi_headers.f_base, '1548') ne -1) then begin
    wavelength = 1548
  endif
;get telescope efficiency function
  eff=dwel_eff_nu(wavelength, beam_div,range,rtef)

;if needed, Convolve telescope efficiency function with model pulse
  if (conv_eff) then begin
    ; Read pulse
    envi_read_cols, pfile_name, pdata
    p_range = reform(pdata[0,*])
    n = n_elements(p_range)
    ; Sort out values of pstart and pstop that we can't cope with and subset pulse
    if (pstop gt n-1) then pstop = n-1
    if (pstop-pstart gt 290) then pstop = pstart+290
    pulse = reform(pdata[1,pstart:pstop])
    ; Pad pulse for symmetry about peak and normalise to sum to 1
    m = max(pulse, nlead)
    ntrail = n_elements(pulse-nlead)
    p = [replicate(0.0,ntrail-nlead),pulse/total(pulse)]
    ; Pad efficiency function data
    e = [replicate(0.0,300),eff,replicate(1.0,150)]
    ; Apply convolution and subset data back to original size
    ec = convol(e,p,total(p))
    eff = ec[300:300+nbands]
  endif

  ;####
  ; If eff=0 we get NaN results, so flag these values to be
  ; set to zero in output
  z = where((eff lt 1.0e-7) or (range lt 0.01), nz,compl=zcomp,ncompl=ncomp)
  ; There will usually be some values at small (or negative) ranges that
  ; result in eff=0, but just in case the file has been clipped, we need
  ; to make sure the z array exists so we don't have to test nz every time
  ; we want to use z. In the unlikely event of no elements in z, we
  ; set it to the single element [0] and thereby sacrifice the first
  ; data point in each waveform. [This will probably never happen!].
  if (nz eq 0) then z = 0
  
  factor=fltarr(n_elements(eff))

  print, 'Setting up matrix '
; Set up matrix of factors for apparent reflectance calculation of one line of data 
; factor = (F*range^n)/(const*scale_factor*eff)
; Code modified to allow the power of range to be set interactively
  if (ncomp gt 0) then factor[zcomp] = (F_casing*(range[zcomp]^rpow))/(const*scale_factor*eff[zcomp])
  factor[z] = 0.0

;save the factor vector to check it!
save_fac=file_dirname(outfile,/mark_directory)+'save_factor.txt'
;Open Log file
text_err=0
openw, tfile, save_fac,/get_lun,error=text_err
printf,tfile,'Range,Eff,Factor*100'
for i=0,n_elements(eff)-1 do begin
  buf=strtrim(strcompress(strtrim(string(range[i],format='(f14.4)'),2)+','+ $
  strtrim(string(eff[i],format='(f14.5)'),2)+','+ $
  strtrim(string(100.0*factor[i],format='(f14.6)'),2)),2)
  printf,tfile,buf
endfor
free_lun,tfile,/force

  f = cmreplicate(factor,nsamples)
  factor = 0b
  factor_matrix = transpose(f)
  f = 0b
  
;set up the progress bar
info_text=[file_basename(infile),'Correcting each waveform to apparent reflectance']
envi_report_init,info_text,title='Processing Waveforms to AppRefl',base=wb_report,/interrupt

  openw,ofile,outfile,/get_lun
  openw,ofile1,pgapfile,/get_lun
  
  widget_control,/hourglass
  
  mask_1=bytarr(nsamples,nlines)
  mask_2=bytarr(nsamples,nlines)
  mean_image=fltarr(nsamples,nlines)

; Loop through each waveform and calculate apparent reflectance,
  ;***
  pos = lindgen(nbands)
  count_bad1=0L
  count_bad2=0L
  imax=fix(out_scale)
  help,imax
  print, 'Starting main loop '
  for i=0,nlines-1 do begin
    line = envi_get_slice(fid=fid, /bil, pos=pos, line=i, xs=0, xe=nsamples-1)
    refl_data = float(line)*factor_matrix*cmreplicate(float(reform(mask[*,i])),nbands)
    line = 0b

    badappmask=(min(refl_data,dimension=2) le -0.1) or (max(refl_data,dimension=2) ge 1.1)
;    writeu,bfile,badappmask

    posbad=where(badappmask gt 0,ntemp)
    if (ntemp gt 0) then begin
      count_bad1=count_bad1+ntemp
      mask_1[posbad,i]=1b
    endif
      
    posbad=0b
    badappmask=0b

    ; Outputs may still be >1 or < 0, so if we've chosen the clip output option,
    ; then clip reflectance output to between 0.0 and 1.0
    ; Very small values of div_factor cause problems. This often occurs
    ; when it becomes negative (due to incorrect calibration). Find these
    ; cases, store as a mask and default to non-negative if requested.
    if (clip_output) then begin
      refl_data = float(refl_data) < replicate(1.0,nsamples,nbands)
      refl_data = float(refl_data) > replicate(0.0,nsamples,nbands)
    endif
    writeu, ofile, refl_data

    mean_image[*,i]=total(refl_data,2)

;now get the Pgap data
    div_factor = 1.0 - total(refl_data,2,/cum,/nan)/(pulse_fwhm*def_refl)
    refl_data = 0b
    pgap = fix(round(float(out_scale)*div_factor))
    if (clip_output) then begin
      pgap = pgap < replicate(imax,nsamples,nbands)
      pgap = pgap > replicate(0s,nsamples,nbands)
    endif
;write out the pgap data
    writeu, ofile1, pgap
    pgap = 0b
    negmask = (div_factor[*,nbands-1]) le -0.25
    div_factor = 0b
;    writeu, mfile, negmask
    posbad=where(negmask gt 0,ntemp)
    if (ntemp gt 0) then begin
      count_bad2=count_bad2+ntemp
      mask_2[posbad,i]=1b
    endif
    posbad=0b
    negmask = 0b

    envi_report_stat,wb_report,i,nlines,cancel=cancel
    if (cancel) then begin
      result=dialog_message('Cancelling apparent reflectance processing',$
        title='Cancelling Job')
      envi_report_init, base=wb_report,/finish
        goto, cleanup
        endif
  endfor
  envi_report_init, base=wb_report,/finish
  ;***
  print, 'Finished main loop ',systime()

; Close primary output files
  free_lun,ofile,/force
  free_lun,ofile1,/force

print,'count_bad1=',count_bad1
print,'count_bad2=',count_bad2

;write out the mask file
  openw,mfile,maskfile,/get_lun
  writeu,mfile,mask_1
  writeu,mfile,mask_2
  posbad=where((mask_1 ne 0b) or (mask_2 ne 0b),ntemp)
  if (ntemp gt 0) then mask[posbad]=0b
;now scale mean image
image_statistics,mean_image,mask=mask,maximum=amax,minimum=amin
mean_image=byte(((255.0*(mean_image-amin)/(amax-amin))>0.0)<255.0)
  writeu,mfile,mask
  writeu,mfile,mean_image
  free_lun,mfile,/force
  posbad=0b
  mask_1=0b
  mask_2=0b
  mask=0b
  mean_image=0b

  ; Write header for output file
  envi_setup_head, fname=outfile, nb=nbands, nl=nlines, ns=nsamples, wl=range, $
    interleave=1, data_type=4, /write, /open, r_fid=out_fid, $
    bnames=bnames+'_AppRefl'
  ; Get user header values from previous processing. This is done 'by hand'
  ; for processes we expect - probably should use the automatic read and

;write out the previous header records
status=put_headers(out_fid,evi_headers)

  ; Write apparent reflectance parameters to header
  evi_apprefl_info = ['Beam Divergence='+strtrim(beam_div,2), $
                  'Cal const='+strtrim(const,2), $
          'Pulse FWHM='+strtrim(pulse_fwhm,2), $
          'Scale='+strtrim(out_scale,2), $
          'Reflectance='+strtrim(def_refl,2), $
          'R_Power='+strtrim(rpow,2),$
          'Clip output='+strtrim(clip_output,2), $
          'Convolve efficiency='+strtrim(conv_eff,2), $
          'Start Position='+strtrim(pstart,2), $
          'Stop Position='+strtrim(pstop,2), $
          'Input file='+strtrim(infile,2), $
          'Apprefl file='+strtrim(outfile,2), $
          'Pgap file='+strtrim(pgapfile,2), $
          'Mask file='+strtrim(maskfile,2), $
          'Number of bad refl='+strtrim(string(count_bad1),2),$
          'Number of bad pgap='+strtrim(string(count_bad2),2) $
          ]
  envi_assign_header_value, fid=out_fid, keyword='EVI_apprefl_info', $
      value=evi_apprefl_info

  ; Update header file
  envi_write_file_header, out_fid

  ; Write header for pgap file
  envi_setup_head, fname=pgapfile, nb=nbands, nl=nlines, ns=nsamples, wl=range, $
    interleave=1, data_type=2, /write, /open, r_fid=pg_fid, $
    bnames=bnames+'_PGap'

;write out the previous header records
  status=put_headers(pg_fid,evi_headers)
  envi_assign_header_value, fid=pg_fid, keyword='EVI_apprefl_info', $
      value=evi_apprefl_info
; Update header file
  envi_write_file_header, pg_fid

  mnames=['Bad Refl','Bad Pgap','Alt Mask','Mean_app']
  ; Write header for mask file
  descstr = 'Mask file from apparent reflectance processing.'+ $
    'Apparent reflectance file is '+strtrim(outfile,2)
  envi_setup_head, fname=maskfile, nb=4, nl=nlines, ns=nsamples, data_type=1, $
    /write, /open, bnames=mnames,interleave=0, descrip=descstr, r_fid=m_fid

;write out the previous header records
  status=put_headers(m_fid,evi_headers)
  envi_assign_header_value, fid=m_fid, keyword='EVI_apprefl_info', $
      value=evi_apprefl_info
; Update header file
  envi_write_file_header, m_fid

cleanup:
if (n_elements(ofile) ne 0) then free_lun, ofile, /force
if (n_elements(ofile1) ne 0) then free_lun, ofile1, /force
if (n_elements(mfile) ne 0) then free_lun, mfile, /force

widget_control,event.top,/destroy
heap_gc,/verbose
return
end

;======================================================================

function evi_app_refl_bannertext
compile_opt idl2

;Provides the banner text as a string array

text=[ $
'Module Description:',$
' ',$
'Procedure evi_app_refl calculates apparent reflectance.', $
'Apparent reflectance is the reflectance a target would',$
'have to give the same power return at the given range.',$
' ',$
'The routine also computes an estimate for Pgap as the sum',$
'of the Apparent Reflectance normalised by target reflectance',$
'Target orientation is assumed to be normal to the beam.', $
' ',$
'Apparent reflectance of noise increases as range^2.',$
'Best results are obtained by applying the pulse filter', $
'routine to the data prior to this routine.',$
' ',$
'The output file is the same format as the input file.',$
'The output Pgap file is integer with scaled probabilities.' $
]

return,text

end

pro evi_app_refl_action_go, event
compile_opt idl2

;Event handler for hitting the "GO" button
;on the top level main widget
;simply calls the main action routine

widget_control,event.top,get_uvalue=pstate

(*pstate).go=1

evi_app_refl_doit, event

return

end

pro evi_app_refl_action_exit, event
compile_opt idl2

;Event handler for hitting the "EXIT" button
;on the top level main widget
;cleans up and destroys the widget hierarchy

widget_control,event.top,get_uvalue=pstate
ptr_free, pstate
widget_control,event.top,/destroy

;some protective programming for now
;when all has been well for a while it
;can go away
heap_gc,/verbose

return

end

pro evi_app_refl_resize, event
compile_opt idl2

;Window resize event handler
;for the top level main widget

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
   result=dialog_message(info_text,/error,title='Error in evi_app_refl_resize', $
   dialog_parent=event.top)
   goto, cleanup
endif

;get the information from event - a structure and
;the uvalue - a pointer

 widget_control,event.top,get_uvalue=pstate
 widget_control,event.top, xsize=event.x,ysize=event.y

 xs=max([53*(event.x/(*pstate).xy_old[0]),1])
 ys1=max([10*(event.y/(*pstate).xy_old[1]),1])

 if((event.y/(*pstate).xy_old[1]) eq 1) then begin
  ys2=1
 endif else begin
  ys2=max([2*(event.y/(*pstate).xy_old[1]),1])
 endelse

 xs_act=max([(*pstate).xy_act[0]*(event.x/(*pstate).xy_old[0]),1])
 ys_act=min([max([(*pstate).xy_act[1]*(event.y/(*pstate).xy_old[1]),1]),35])

 widget_control,(*pstate).w_2,xsize=xs,ysize=ys1
 widget_control,(*pstate).w_3,xsize=xs,ysize=ys2
 widget_control,(*pstate).wb_action,xsize=xs_act,ysize=ys_act

 widget_control,event.top,/realize

cleanup:

return

end

pro evi_app_refl_cleanup,top
compile_opt idl2

;The cleanup routine for Xmanager
;This is part of the IDL style top widget

;get state pointer
widget_control,top,get_uvalue=pstate

;clean up pointers
ptr_free, pstate

heap_gc,/verbose

return

end


pro dwel_app_refl, event
compile_opt idl2

;The main routine that sets up the widget hierarchy and
;manages the events
;There are only two main events here so it is not so hard
;Providing the overall procedure description in a slider
;menu and the disclaimer are the main purposes
;it is a general top level routine and uses IDL widgets
;for the most part and no auto-manage

; Setup aunty-Catch to watch out for nasty errors
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
   result=dialog_message(info_text,/error,title='Error in evi_app_refl')
   goto, cleanup
endif

;now set up the top level base widget
envi_center,xoff,yoff
wb_base=widget_base(title='Calculating Apparent Reflectance',/column,$
xoffset=xoff,yoffset=yoff,/frame,$
/tlb_size_events)

rev=1s

;get the procedure description and disclaimer to display
intro_text=evi_app_refl_bannertext()
dis_claim=evi_disclaimer(rev)

;Set up a base widget to hold the description and disclaimer
wb_info=widget_base(wb_base,/column)

;Use two slider label box routines from the ENVI library
w_2=widget_slabel(wb_info,prompt=intro_text,/frame,xsize=53,ysize=10)
w_3=widget_slabel(wb_info,prompt=dis_claim,/frame,xsize=53,ysize=1)

;Now set up another base widget for the action buttons
wb_action=widget_base(wb_base,/row)

;Action buttons are just "go" and "exit"
w_go=widget_button(wb_action,value='Go',uvalue='evi_app_refl_action_go',$
event_pro='evi_app_refl_action_go',/dynamic_resize)
w_exit=widget_button(wb_action,Value='Exit',uvalue='evi_app_refl_action_exit',$
event_pro='evi_app_refl_action_exit',/dynamic_resize)

;realise the widget hierarchy
widget_control,wb_base,/realize

go=0

widget_geometry_1=widget_info(wb_base,/geometry)
widget_geometry_2=widget_info(wb_action,/geometry)

xy_old=[widget_geometry_1.xsize,widget_geometry_1.ysize]
xy_act=[widget_geometry_2.xsize,widget_geometry_2.ysize]

;set up a state structure
state={ $
tlb:wb_base,$
w_2:w_2, $
w_3:w_3, $
wb_action:wb_action, $
go:go, $
xy_old:xy_old, $
xy_act:xy_act, $
wb_info:wb_info $
}

pstate=ptr_new(state,/no_copy)
widget_control,wb_base,set_uvalue=pstate

;call xmanager
xmanager,'evi_app_refl',wb_base, $
event_handler='evi_app_refl_resize',$
cleanup='evi_app_refl_cleanup',/no_block

;exit back to ENVI
;leave a bit of a trace for bad exits for now
;this can go when things stabilise - along with
;much protective programming and aunty catch
;in all the many places it is at this time!
cleanup:
heap_gc,/verbose
return

end

