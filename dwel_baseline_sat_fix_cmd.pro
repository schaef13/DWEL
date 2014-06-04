;; baseline fix and saturation fix of NSF DWEL data
;; Zhan Li, zhanli86@bu.edu
;; Created in 2013 by Zhan Li
;; Last modified: 20140603 by Zhan Li

function dwel_header_parse, headerstr, tag_name

end

function apply_sat_fix, basefixed_satwf, pulse_model, p_troughloc, p_scdpeakloc, satfixedwf=satfixedwf
  ; maximum peak location
  tmp = max(basefixed_satwf, maxpeakloc)
  maxpeakloc = maxpeakloc[0]
  ; find the three zero-cross points after the maximum peak
  wflen = size(basefixed_satwf, /n_elements)
  zero_xloc = where(basefixed_satwf[maxpeakloc:wflen-2]*basefixed_satwf[maxpeakloc+1:wflen-1] le 0, tmpcount) + maxpeakloc
  if (size(zero_xloc, /n_elements) lt 3) then begin
    return, 0
  endif
  ; find the minimum and maximum between the first zero-cross point and the third zero-cross point
  tmp = min(basefixed_satwf[zero_xloc[0]:zero_xloc[2]], tmploc)
  troughloc = fix(mean(tmploc[0])) + zero_xloc[0]
  tmp = max(basefixed_satwf[zero_xloc[0]:zero_xloc[2]], tmploc)
  scdpeakloc = fix(mean(tmploc[0])) + zero_xloc[0]
  satfix_scale = (basefixed_satwf[troughloc[0]]/pulse_model[p_troughloc] + basefixed_satwf[scdpeakloc[0]]/pulse_model[p_scdpeakloc])/2.0
  
  plen = size(pulse_model, /n_elements)
  satfixedwf = basefixed_satwf
  satfixedwf[troughloc-p_troughloc:troughloc+plen-p_troughloc-1] = pulse_model*satfix_scale
  return, 1
end

pro DWEL_Baseline_Sat_Fix_Cmd, DWELCubeFile, Casing_Range ;, AsciiCasingMeanWfFile, AsciiSkyMeanWfFile
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window
  
  lun=99
  ofile=101
  tfile=98
  fname=''
  o_name=''
  debug=1b

  ;; distance from casing (edge of casing) to the true Tzero position
  casing2Tzero = 0.2 ; unit: meters
  ;; the FWHM of outgoing pulse, ns
  outgoing_fwhm = 5.1
  ;; the full width of outgoing pulse where intensity is below 0.01 of
  ;;maximum
  pulse_width_range = 5.1 * sqrt(alog(0.01)/alog(0.5))  
  
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
     print, strtrim('Error in DWEL_Baseline_Fix_Cmd', 2)
     print, strtrim(info_text, 2)
     goto, cleanup
  endif
  
  ;clean up any fids which are no longer where they were!
  ;ENVI issue that is annoying and leads to confusion
  clean_envi_file_fids
  
  ;set speed of light metres per nsec /2
  c=0.299792458
  c2=c/2.0
  
  ;Setup of the ND here is no actual of use for DWEL data since DWEL does NOT use ND anymore
  ;It's just to fake a EVI file so that the DWEL file can go through the current EVI programs.  
  ;set current estimates for the ND filter factors
  ;this should be set up better with an instrument data base - one day
  ND_Nam=strarr(11)
  ND_Val=fltarr(11)
  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  ND_Val=[0.31327,0.17798,0.090430,0.045602,0.03469,0.02294,0.06366,0.03556,0.02166,0.01242,0.17798]
  
  ; Open DWEL cube file
  envi_open_file, DWELCubeFile, r_fid=infile_fid,/no_realize
  
  if (infile_fid eq -1) then begin
      print,strtrim('Error opening input file',2)
      print,'Input File: '+strtrim(DWELCubeFile,2)
      goto, cleanup
  endif
  
  envi_file_query, infile_fid, ns=ns, nl=nl, nb=nb, wl=wl, $
    xstart=xstart, ystart=ystart, data_type=type, $
    interleave=ftype, fname=fname, dims=dims
  
  x_range=[dims[1],dims[2]]
  y_range=[dims[3],dims[4]]
  
  ;set the type of file
  ft_nam='Unknown'
  case ftype of
  0: ft_nam='BSQ'
  1: ft_nam='BIL'
  2: ft_nam='BIP'
  endcase
  
  ;get path and evi_file name as separate strings
  f_base=file_basename(fname)
  f_path=file_dirname(fname)
  
  ; Open Ancillary file
  n_base=strlen(fname)
  n_dot=strpos(fname,'.',/reverse_search)
  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
    anc_name=strtrim(fname,2)+'_ancillary'
  endif else begin
    ancillaryfile_name=strmid(fname,0,n_dot)+'_ancillary.img'
  endelse
  
  if(not file_test(ancillaryfile_name)) then begin
    message_text=[ $
    'Default ancillary file is not present',$
    'default name: '+strtrim(ancillaryfile_name,2)$
    ]
    print, message_text
    GOTO, cleanup
  endif
  
  envi_open_file, ancillaryfile_name, r_fid=ancillaryfile_fid, $
                  /no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
      print,strtrim('Error or No opening ancillary file',2)
      print,'Ancillary File: '+strtrim(ancillaryfile_name,2)
      goto, cleanup
  endif
  
  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, data_type=type_anc
  
  if ((nl_anc ne nl) or (ns_anc ne ns) or (nb_anc lt 3)) then begin
      envi_file_mng,id=ancillaryfile_fid,/remove
      print,strtrim('Ancillary Data File does NOT conform with current EVI Cube !',2)
      print,'Input File: '+strtrim(DWELCubeFile,2)
      print,'Ancillary File: '+strtrim(ancillaryfile_name,2)
      goto, cleanup
  endif
  
  ;now get the EVI headers that are present
  ;set up a base structure for the EVI headers
  evi_headers={ $
       f_base:f_base $
       }
  
  ;find all of the EVI headers in the hdr file as defined by FID
  status=get_headers(infile_fid,evi_headers)
  
  if (not status) then begin
    print,strtrim('Bad FID in get_headers! EVI Header setup cancelled!',2)
    print,'Input File: '+strtrim(DWELCubeFile,2)
    goto, cleanup
  endif
  
  if (evi_headers.headers_present le 0s or not evi_headers.run_present) then begin
    print,strtrim('NOT a valid DWEL Cube file!',2)
    print,'Input File: '+strtrim(DWELCubeFile,2)
    goto, cleanup
  endif
  
  info = evi_headers.EVI_scan_info
  
  ;check for ND filter in Cube (may not be set)
  match = -1
  for i=0,n_elements(info)-1 do if (strmatch(info[i],'*ND_Filter*')) then match=i
  if (match ge 0) then begin
    ndf = strtrim(strsplit(info[match],'=',/extract),2)
    ND_Filter = fix(ndf[1])
    ;ND_Nam[ND_Filter] is used
    ;ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
    ;ND_Val=[0.31327,0.17798,0.090430,0.045602,0.03469,0.02294,0.06366,0.03556,0.02166,0.01242,0.17798]
    ND_def_set = 1b
  endif else begin
    ND_Filter=0
    ND_def_set = 0b
  endelse
  if (match ge 0) then print,'info match for ND Filter= ',info[match]
  if (~ND_def_set) then print,'ND filter not set'
  print,'ND Filter=',ND_Filter
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; these parameters are of no use for DWEL. just to fake EVI files
  ;set default baseplate limits
  low=140.0
  high=144.0
  ;fix run-in for now
  nref=20
  ;ratio of casing peak to outgoing peak
  casing_outgoing_factor=0.026172
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;also scale
  scale=10.0
  ;also encoder zero zenith
  enc_zero_zenith=double(262144.0)
  
  ;set the default sampling rate
  match = -1
  for i=0,n_elements(info)-1 do if (strmatch(info[i],'*Sampling Rate*')) then match=i
  if (match ge 0) then begin
    text=strtrim(info[match],2)
    print,'text=',text
    k=strpos(text,'=')
    l=strpos(text,'smp/ns')
    print,'extract=',strtrim(strmid(text,k+1,l-k-1),2)
  ;  reads,strtrim(strmid(text,k+1,l-k-1),2),var2
    var2=float(strtrim(strmid(text,k+1,l-k-1),2))
    if (var2 gt 0.0) then begin
      srate=var2
      srate_set=1
    endif else begin
      srate=2.0
      srate_set=0
    endelse
  endif else begin
    srate=2.0
    srate_set = 0b
  endelse
  if (match ge 0) then print,'info match for sampling rate= ',strtrim(info[match],2)
  if (~srate_set) then print,'sampling rate not set'
  print,'sampling rate=',srate
  
  ;input some planes of data from the ancillary file
  anc_data=lon64arr(ns,nl,9)
  for j=0,8 do begin
    anc_data[*,*,j]=fix(ENVI_GET_DATA(fid=ancillaryfile_fid, dims=[-1L,0,ns-1,0,nl-1], pos=j), type=14)
  endfor
  
  DWEL_Adaptation = ENVI_GET_HEADER_VALUE(ancillaryfile_fid, 'DWEL_Adaptation', undefined=undef)
  if undef then begin
    DWEL_Adaptation = ''
    if (strpos(evi_headers.f_base, '1064') ne -1) then begin
      wavelength = 1064
    endif
    if (strpos(evi_headers.f_base, '1548') ne -1) then begin
      wavelength = 1548
   ENDIF
 ENDIF ELSE BEGIN
    match = -1
    info = DWEL_Adaptation
    for i=0,n_elements(info)-1 do BEGIN
       if (strmatch(info[i],'*Wavelength*', /fold_case)) then match=i
    ENDFOR 
    IF match GE 0 THEN BEGIN
       text=strtrim(info[match],2)
       print,'text=',text
       k=strpos(text,'=')
       print,'extract=',strtrim(strmid(text,k+1,4),2)
       wavelength=float(strtrim(strmid(text,k+1,4),2))
    ENDIF ELSE BEGIN
       if (strpos(evi_headers.f_base, '1064') ne -1) then begin
          wavelength = 1064
       endif
       if (strpos(evi_headers.f_base, '1548') ne -1) then begin
          wavelength = 1548
       ENDIF
    ENDELSE 
 ENDELSE

  envi_file_mng, id=ancillaryfile_fid,/remove
  
  ;compute the mask from the ancillary file
  m=bytarr(ns,nl)
  mask_all=bytarr(ns,nl)
  m = byte(anc_data[*,*,6])
  if (max(m) gt 1b) then begin
    w = where(m lt 255, nbad)
  ;Create the binary mask
    m = replicate(1b,ns,nl)
    if (nbad gt 0) then m[w] = 0b
  endif
  mask_all=m
  m=0b
  
  scan_encoder=fltarr(ns,nl)
  rotary_encoder=fltarr(ns,nl)
  zeniths=fltarr(ns,nl)
  azimuths=fltarr(ns,nl)
  
  scan_encoder=float(anc_data[*,*,2])
  rotary_encoder=float(anc_data[*,*,3])
  
  ;compute the zeniths and azimuths
  zeniths=(enc_zero_zenith-scan_encoder)*(360.0/524288.0)
  azimuths=rotary_encoder*(360.0/524288.0)
  
  index = where(zeniths lt 0.0, count)
  if count gt 0 then begin
    Zeniths[index] = - Zeniths[index]
    Azimuths[index] = Azimuths[index] + 180.0
  endif
  index = where(Azimuths gt 360.0, count)
  if count gt 0 then begin
    Azimuths[index] = Azimuths[index] - 360.0
  endif
  
  ;------------------------------------------------------------------------------
  ;now get the output file name
  
  output:
  
  n_base=strlen(fname)
  n_dot=strpos(fname,'.',/reverse_search)
  
  if (o_name eq '') then begin
    if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      o_name=fname+'_nu_basefix.img'
    endif else begin
      o_name=strmid(fname,0,n_dot)+'_nu_basefix.img'
    endelse
  endif
  
  ;============================================
  ;get sat fix output file name
    if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
      out_satfix_name=fname+'_nu_basefix_satfix.img'
    endif else begin
      out_satfix_name=strmid(fname,0,n_dot)+'_nu_basefix_satfix.img'
    endelse
  ;============================================
  
  out_name=o_name
    
  ;; ; read the mean waveform of marked casing area from an ascii file
  ;; ; get the initial T0 from this mean waveform 
  ;; CasingMeanWf = Get_AsciiWf(AsciiCasingMeanWfFile)
  ;; p_time = CasingMeanWf.wf_x
  ;; pulse = CasingMeanWf.wf_y
  
  ;; ; get the mean waveform of marked sky area from an ascii file
  ;; ; get the background noise (baseline) from this mean waveform
  ;; SkyMeanWf = Get_AsciiWf(AsciiSkyMeanWfFile)
  ;; baseline = SkyMeanWf.wf_y
  
  ;; get mean pulse and baseline from the given casing area designated
  ;;by the zenith angles. 
  sum = dblarr(nb)
  sum2 = dblarr(nb)
  n = 0
  for i=0L,nl-1L do BEGIN     
    index=where((mask_all[*,i] ne 0) and (zeniths[*,i] ge Casing_Range[0] and zeniths[*,i] LE Casing_Range[1]), count)
    if (count gt 0L) then begin
      data = envi_get_slice(fid=infile_fid, line=i, /bil)
      d = double(data[index,*])
      data=0b
      n = n + count
      sum = sum + total(d, 1, /double)
      sum2 = sum2 + total(d^2, 1,/double) ; sum of square waveform over the casing area in this scan line, still a vector
    endif else BEGIN
       
    endelse
    index=0b
    data=0b
    d=0b
 ENDFOR
  ; initial time from current data cube before baseline fix
  time = wl 
  p_time = time
  pulse = sum / double(n)
  sig = sqrt((sum2 / double(n) - pulse^2)*double(n)/double(n-1))
  tmpmax = max(pulse, tmppos)
  print, 'Initial Tzero before baseline fix = ', time[tmppos], ' ns'  
  baseline = dblarr(nb)
  tmpind = where(time LT time[tmppos] - pulse_width_range/2.0, count)
  IF count GT 0 THEN BEGIN
     baseline[tmpind] = pulse[tmpind]
  ENDIF 
  tmpind = where(time GT time[tmppos] + 5*pulse_width_range, count)
  IF count GT 0 THEN BEGIN
     baseline[where(time GE time[tmppos] - pulse_width_range/2.0)] = total(pulse[tmpind])/double(count)
  ENDIF ELSE BEGIN
     baseline[where(time GE time[tmppos] - pulse_width_range/2.0)] = pulse[nb-1]
  ENDELSE 
  
  pulse = pulse - baseline
  
  CasingMeanWfMax = max(pulse, Tzero_I) 
  Tzero=time[Tzero_I]
  print,'Initial Tzero after baseline fix = ',Tzero,' ns'

  ;; interpolate peak location
  istat = peak_int(time[[Tzero_I-1, Tzero_I, Tzero_I+1]], pulse[[Tzero_I-1, Tzero_I, Tzero_I+1]], time_int, pulse_int, offset)
  Tzero = time_int
  print, 'Initial Tzero after intepolation = ', Tzero, ' ns'

  delta= -casing2Tzero/c2 ; 0.2 meter is about the distance between the rotating mirror and the base. This is an old measurement and needs be updated. 
  print,'delta=',delta, ' ns'
  print,''

  Tzero=Tzero+delta
  print,'Shifted Tzero',Tzero, ' ns'
  
  time=time-Tzero
  
  ;set up the EVI header information for the base fixing
  ;this header is just a dummy thing for this output file to be accomodated by following EVI routines.
  ;values of some properties in this header is fabricated and fake.
  data_max=0
  data_max=1.0
  data_sig=0
  cv=0
  casing_fwhm=0
  casing_fwhm=(p_time[1]-p_time[0])*total(pulse,/double)/max(pulse)
  model_fwhm=casing_fwhm
  
  EVI_base_fix_info=strarr(20)
  EVI_base_fix_info=[$
  'Descr=EVI New Base Fix Settings with casing power',$
  'Pulse='+'Pulse_Model',$
  'Comment=Tzero is the time at which the output peak occurs',$
  'Tzero='+strtrim(string(Tzero,format='(f10.3)'),2),$
  'srate='+strtrim(string(srate,format='(f10.2)'),2),$
  'ND_Filter='+strtrim(string(ND_Filter,format='(i6)'),2),$
  'Filter_Name='+strtrim(ND_Nam[ND_Filter],2),$
  'Nref='+strtrim(string(nref,format='(i10)'),2),$
  'scale='+strtrim(string(scale,format='(f10.2)'),2),$
  'Low='+strtrim(string(low,format='(f10.2)'),2),$
  'High='+strtrim(string(high,format='(f10.2)'),2),$
  'enc_zero_zenith='+strtrim(string(enc_zero_zenith,format='(f10.2)'),2),$
  'delta(ns)='+strtrim(string(delta,format='(f10.4)'),2),$
  'casing_max='+strtrim(string(data_max,format='(f10.3)'),2),$
  'casing_sig='+strtrim(string(data_sig,format='(f10.3)'),2),$
  'Casing_CV(%)='+strtrim(string(cv,format='(f10.2)'),2),$
  'casing_fwhm(nsec)='+strtrim(string(casing_fwhm,format='(f10.4)'),2),$
  'casing_fwhm(m)='+strtrim(string(casing_fwhm*c2,format='(f10.4)'),2),$
  'model_fwhm(nsec)='+strtrim(string(model_fwhm,format='(f10.4)'),2),$
  'model_fwhm(m)='+strtrim(string(model_fwhm*c2,format='(f10.4)'),2) $
  ]
  EVI_base_fix_info=strtrim(EVI_base_fix_info,2)
  
  ;=====================================================================
  ;now apply the correction to the image
  
  nb_out=nb
  nl_out=nl
  ns_out=ns
  wl_range=c2*time
  
  ;Open the output file for BIL tiling
  text_err=0
  openw, ofile, out_name,/get_lun,error=text_err
  if (text_err ne 0) then begin
    print, strtrim('Halting evi_baseline_fix', 2)
    print, strtrim(['Error opening output file '+strtrim(out_name,2)], 2)
    goto, cleanup
  endif
  
  ;=====================================================================
  openw, osatfile, out_satfix_name,/get_lun,error=text_err
  if (text_err ne 0) then begin
    print, strtrim('Halting evi_baseline_sat_fix', 2)
    print, strtrim(['Error opening output file '+strtrim(out_satfix_name,2)], 2)
    goto, cleanup
  endif
  ;=====================================================================
  
  band_pos=indgen(nb_out)
  
  ft_out=1
  ft_str=['BSQ','BIL','BIP']
  
  ;Set up the tiling
  tile_id=envi_init_tile(infile_fid,band_pos,num_tiles=num_tiles,$
    interleave=ft_out,xs=x_range[0],xe=x_range[1],ys=y_range[0],ye=y_range[1])
  
  ;check that tiling and dimensions match
  if (num_tiles ne nl_out) then begin
    print, strtrim('Number of Tiles is unexpected', 2)
    print,strtrim([$
      'Output File Type is '+strtrim(ft_str[ft_out],2),$
      'Number of tiles is '+strtrim(string(num_tiles,format='(i5)'),2),$
      'Expected number of tiles = '+strtrim(string(nl_out,format='(i5)'),2)],2)
    goto, cleanup
  endif
  
  ;; ;set up the odometer
  ;; Info_Text=['Input file : '+strtrim(fname,2),$
  ;;       'Output baseline fix file : '+strtrim(out_name,2),$
  ;;       'Output saturation fix file : '+strtrim(out_satfix_name,2),$
  ;;       ' ',$
  ;;       'Applying EVI Base and Sat fix in '$
  ;;       +strtrim(string(n_elements(where(band_pos gt -1))),2)+' Bands']
  ;; envi_report_init,Info_Text,title='Applying EVI Base and Sat fix',$
  ;; base=wb_report,/interupt
  ;; envi_report_inc,wb_report,num_tiles
  
  ;set up some things outside the loop
  info_text=[strtrim(string('Processing Cancelled'),2),$
             strtrim('Exiting EVI Base and Sat fixing',2)]
  
  ;do the processing over the tiles
  ;BIL Tile
  ;zero=fltarr(ns_out,nb_out)
  temp=fltarr(ns_out,nb_out)
  one_ns=fltarr(ns_out)+1.0
  one_nb=fltarr(nb_out)+1.0
  
  mean_image=fltarr(ns_out,nl_out)
  max_image=fltarr(ns_out,nl_out)
  ;==========================================
  sat_mean_image=fltarr(ns_out,nl_out)
  sat_max_image=fltarr(ns_out,nl_out)
  satfixeddata = fltarr(ns_out,nb_out)
  ;==========================================
  pos_pos=where(wl_range gt 0.0,npos_r)
  if(npos_r le 0) then begin
    print,'bad range, npos_r=',npos_r
  endif
  fln=1.0/float(npos_r)  
  
  DWEL_pulse_model_dual, wavelength, i_val, t_val, r_val, p_range, p_time, pulse
  p_troughloc = i_val[3]
  p_scdpeakloc = i_val[4]
  
  for i=0, num_tiles-1 do begin
  ;first get the data tile
    data=envi_get_tile(tile_id,i)
    pos_z=where(mask_all[*,i] eq 0,count_z)
    temp=float(data)
    ;============================================
    ;saturation detection
    ;check if the waveform maximum is equal or larger 1023. If so the waveform is identified saturated
    maxtemp = max(temp, dimension=2)
    sat_pos = where(maxtemp ge 1023, count_sat)
    ;============================================
    
    temp=temp-transpose(baseline)##one_ns
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; check if the mean of the first 100 waveform bins is abnormal
    tmpmean = mean(temp[*, 0:99], dimension=2)
    abnormalpos = where(tmpmean gt 10, tmpcount)
    if (tmpcount gt 0) then begin
      temp[abnormalpos, *] = temp[abnormalpos, *] - cmreplicate(tmpmean[abnormalpos], [1, nb_out])
    endif
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (count_z gt 0L) then begin
       temp[pos_z,*]=0.0
    endif
    
    satfixeddata = temp
    ;============================================
    ;saturation fix, replace the saturated waveform part with pulse model and corresponding peak intensity
    if (count_sat gt 0) then begin
      for si=0,count_sat-1 do begin
        satflag = apply_sat_fix(temp[sat_pos[si], *], pulse, p_troughloc, p_scdpeakloc, satfixedwf=unsattemp)
        if (~satflag) then begin
          print, 'line=', i, ', sample=', sat_pos[si]
          continue
        endif else begin
          satfixeddata[sat_pos[si], *] = unsattemp
        endelse
      endfor
    endif
    ;============================================
    
  ;round if integer else set back in data
    if (type lt 4 or type gt 9) then begin
      data=fix(round(scale*temp), type=2)
    endif else begin
      data=scale*temp
    endelse
  ;write out the resulting tile
    writeu,ofile,data
    mean_image[*,i]=fln*total(data[*,pos_pos],2)
    max_image[*,i]=float(max(data[*,pos_pos],DIMENSION=2))
    
    ;=================================================
    ;round if integer else set back in data
    if (type lt 4 or type gt 9) then begin
      satfixeddata=fix(round(scale*satfixeddata), type=2)
    endif else begin
      satfixeddata=scale*satfixeddata
    endelse
  ;write out the resulting tile
    writeu,osatfile,satfixeddata
    sat_mean_image[*,i]=fln*total(satfixeddata[*,pos_pos],2)
    sat_max_image[*,i]=float(max(satfixeddata[*,pos_pos],DIMENSION=2))
    ;=================================================
    
;;  ;update the odometer
;;    envi_report_stat,wb_report,i,num_tiles,cancel=cancel
  ;; ;act on cancel
  ;;   if(cancel) then begin
  ;;     result=dialog_message(info_text,$
  ;;     title='EVI Base and Sat Fix cancelled');, $
  ;;     ;dialog_parent=event.top)
  ;;     envi_tile_done, tile_id
  ;;     envi_report_init, base=wb_report,/finish
  ;;     goto,cleanup
  ;;   endif
    data=0
    satfixeddata = 0
    temp=0b
  endfor
  
  ;set the output data type
  if (type lt 4 or type gt 9) then begin
    out_type=2
  endif else begin
    out_type=4
  endelse
  
  ;clear up and complete the action
  envi_tile_done, tile_id
;;  envi_report_init, base=wb_report,/finish
  free_lun, ofile,/force
  data=0
  temp=0b
  one_ns=0b
  one_nb=0b
  
  out_head:
  ;write out header for the output file
  descrip='EVI Base Fix applied to '+strtrim(fname,2)
  band_names=strarr(nb_out)+'Range_Sample_(m)_'
  band_names=band_names+strtrim(string(indgen(nb_out)+1),2)+'_basefix'
  
  ;get output_file name without path
  last=strpos(out_name,path_sep(),/reverse_search)
  out_base=strtrim(strmid(out_name,last+1,strlen(out_name)-last-1),2)
  EVI_base_fix_info=[EVI_base_fix_info,'Base_Fixed_File='+out_base]
  
  envi_setup_head,fname=out_name,ns=ns_out,nl=nl_out,nb=nb_out,$
  xstart=xstart+dims[1],ystart=ystart+dims[3],$
  data_type=out_type, interleave=ft_out, $
  wl=wl_range, inherit=inherit, $
  bnames=band_names,descrip=descrip, $
  zplot_titles=['Range (m)','Intensity'], $
  /write,/open, r_fid=out_fid
  
  ;write out the previous header records
  status=put_headers(out_fid,evi_headers)
  ;
  ;write the new header(s) into the HDR file
  envi_assign_header_value, fid=out_fid, keyword='EVI_base_fix_info', $
      value=EVI_base_fix_info
  envi_write_file_header, out_fid
  
  ;===================================================
  ;Set up the ancillary file
  dot = strpos(out_name,'.',/reverse_search)
  if ((dot lt 0) or ((strlen(out_name)-dot-1) ne 3)) then dot = strlen(out_name)
  ancfile = strmid(out_name, 0, dot)+'_ancillary.img'
  
  print,ancfile
  
  ; check if we need to close and delete ancfile
  fids=envi_get_file_ids()
  if(fids[0] ne -1) then begin
    for i=0,n_elements(fids)-1 do begin
      envi_file_query,fids[i],fname=name
      if (strtrim(strlowcase(ancfile),2) eq $
        strtrim(strlowcase(name),2)) then begin
        envi_file_mng,id=fids[i],/remove,/delete
      endif
    endfor
  endif
  
  ;now write out the new ancillary file
  text_err=0
  openw, ofile, ancfile,/get_lun,error=text_err
  if (text_err ne 0) then begin
     print, strtrim('Halting evi_baseline_fix', 2)
     print, strtrim(['Error opening output file '+strtrim(ancname,2)], 2)
    goto, cleanup
  endif
  
  pos_pos=where(mask_all ne 0)
  meanmean=mean(mean_image[pos_pos])
  stddevmean=stddev(mean_image[pos_pos])
  mlow=meanmean-4.0*stddevmean
  mhigh=meanmean+4.0*stddevmean
  minmean=min(mean_image[pos_pos]) > mlow
  maxmean=max(mean_image[pos_pos]) < mhigh
  mean_image[pos_pos]=((4095.0*(mean_image[pos_pos]-minmean)/(maxmean-minmean) > 0.0) < 4095.0)
  
  print,'scale range=',minmean,maxmean
  
  for j=0,4 do begin
    writeu,ofile,anc_data[*,*,j]
  endfor
  writeu,ofile,fix(round(max_image), type=14)
  writeu,ofile,fix(mask_all, type=14)
  writeu,ofile,fix(round(10.0*zeniths), type=14)
  writeu,ofile,fix(round(10.0*azimuths), type=14)
  free_lun, ofile,/force
  ;anc_data=long64(0)
  mean_image=0b
  
  ENVI_SETUP_HEAD, fname=ancfile, $
    ns=ns_out, nl=nl_out, nb=9, $
    interleave=0, data_type=size(anc_data, /type), $
    /write, /open, r_fid=anc_fid, $
    bnames=['Non Triggers','Sun Sensor','Scan Encoder','Rotary Encoder', $
    'Laser Power','Waveform Mean','Mask','Zenith','Azimuth']
  
  ;write out the previous header records
  status=put_headers(anc_fid,evi_headers)
  ;
  ;write the new header(s) into the HDR file
  DWEL_Adaptation=[DWEL_Adaptation, 'Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift in HDF raw data']
  envi_assign_header_value, fid=anc_fid, $
    keyword='DWEL_Adaptation', $
    value=DWEL_Adaptation
  envi_assign_header_value, fid=anc_fid, keyword='EVI_base_fix_info', $
      value=EVI_base_fix_info
  envi_write_file_header, anc_fid
  
  ;=====================================================================
  out_satfix_head:
  ;get output_file name without path
  last=strpos(out_satfix_name,path_sep(),/reverse_search)
  out_base=strtrim(strmid(out_satfix_name,last+1,strlen(out_satfix_name)-last-1),2)
  ;Set up the ancillary file
  dot = strpos(out_satfix_name,'.',/reverse_search)
  if ((dot lt 0) or ((strlen(out_satfix_name)-dot-1) ne 3)) then dot = strlen(out_satfix_name)
  ancfile = strmid(out_satfix_name, 0, dot)+'_ancillary.img'
  last=strpos(ancfile,path_sep(),/reverse_search)
  anc_base=strtrim(strmid(ancfile,last+1,strlen(ancfile)-last-1),2)
  
  print,ancfile
  ; Write saturation fixing parameters to header
  evi_sat_info = [$
          'Title=Parameters for Saturation Fixing',$
          'Pulse r0='+strtrim(0,2), $
          'Pulse n='+strtrim(0,2), $
          'Pulse rp='+strtrim(0,2), $
          'Pulse W='+strtrim(0,2), $
          'Pulse pos intercept='+strtrim(0,2), $
          'Pulse pos slope='+strtrim(0,2), $
          'Sat test value='+strtrim(0,2), $
          'Sat test width='+strtrim(0,2), $
          'Sat width test height='+strtrim(0,2), $
          'Sat test depth='+strtrim(0,2), $
          'Saturated_(pixels)='+strtrim(0,2), $
          'Saturated_(%)='+strtrim(string(0,format='(f14.2)'),2), $
          'Stats_Format=(Num,Min,Max,Mean,RMS)',$
          'Range_Stats=('+'0'+')',$
          'Sat Fixed File='+out_base, $
          'Sat Fix Stats File='+'NaN', $
          'Updated Ancillary File='+anc_base]
  evi_sat_info=strtrim(evi_sat_info,2)
  
  evi_sat_info=[evi_sat_info,'Sat_Fixed_File='+out_base]
  
  ;write out header for the output file
  descrip='EVI Sat Fix applied to '+strtrim(out_base,2)
  band_names=strarr(nb_out)+'Range_Sample_(m)_'
  band_names=band_names+strtrim(string(indgen(nb_out)+1),2)+'_satfix'
  
  envi_setup_head,fname=out_satfix_name,ns=ns_out,nl=nl_out,nb=nb_out,$
  xstart=xstart+dims[1],ystart=ystart+dims[3],$
  data_type=out_type, interleave=ft_out, $
  wl=wl_range, inherit=inherit, $
  bnames=band_names,descrip=descrip, $
  zplot_titles=['Range (m)','Intensity'], $
  /write,/open, r_fid=out_fid
  
  ;write out the previous header records
  status=put_headers(out_fid,evi_headers)
  ;
  ;write the new header(s) into the HDR file
  envi_assign_header_value, fid=out_fid, keyword='EVI_base_fix_info', $
      value=EVI_base_fix_info
  envi_assign_header_value, fid=out_fid, keyword='evi_sat_info', $
      value=evi_sat_info
  envi_write_file_header, out_fid
  
  ;===================================================
  
  ; check if we need to close and delete ancfile
  fids=envi_get_file_ids()
  if(fids[0] ne -1) then begin
    for i=0,n_elements(fids)-1 do begin
      envi_file_query,fids[i],fname=name
      if (strtrim(strlowcase(ancfile),2) eq $
        strtrim(strlowcase(name),2)) then begin
        envi_file_mng,id=fids[i],/remove,/delete
      endif
    endfor
  endif
  
  ;now write out the new ancillary file
  text_err=0
  openw, ofile, ancfile,/get_lun,error=text_err
  if (text_err ne 0) then begin
     print, strtrim('Halting evi_baseline_sat_fix', 2)
     print, strtrim(['Error opening output file '+strtrim(ancname,2)], 2)
     goto, cleanup
  endif
  
  pos_pos=where(mask_all ne 0)
  sat_meanmean=mean(sat_mean_image[pos_pos])
  sat_stddevmean=stddev(sat_mean_image[pos_pos])
  sat_mlow=sat_meanmean-4.0*sat_stddevmean
  sat_mhigh=sat_meanmean+4.0*sat_stddevmean
  sat_minmean=min(sat_mean_image[pos_pos]) > mlow
  sat_maxmean=max(sat_mean_image[pos_pos]) < mhigh
  sat_mean_image[pos_pos]=((4095.0*(sat_mean_image[pos_pos]-sat_minmean)/(sat_maxmean-sat_minmean) > 0.0) < 4095.0)
  
  print,'saf fixed data scale range=',sat_minmean,sat_maxmean
  
  for j=0,4 do begin
    writeu,ofile,anc_data[*,*,j]
  endfor
  writeu,ofile,fix(round(sat_max_image), type=14)
  writeu,ofile,fix(mask_all, type=14)
  writeu,ofile,fix(round(10.0*zeniths), type=14)
  writeu,ofile,fix(round(10.0*azimuths), type=14)
  free_lun, ofile,/force
  anc_data=long64(0)
  sat_mean_image=0b
  
  ENVI_SETUP_HEAD, fname=ancfile, $
    ns=ns_out, nl=nl_out, nb=9, $
    interleave=0, data_type=size(anc_data, /type), $
    /write, /open, r_fid=anc_fid, $
    bnames=['Non Triggers','Sun Sensor','Scan Encoder','Rotary Encoder', $
    'Laser Power','Waveform Mean','Mask','Zenith','Azimuth']
  
  ;write out the previous header records
  status=put_headers(anc_fid,evi_headers)
  ;
  ;write the new header(s) into the HDR file
  DWEL_Adaptation=[DWEL_Adaptation, 'Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift in HDF raw data']
  envi_assign_header_value, fid=anc_fid, $
    keyword='DWEL_Adaptation', $
    value=DWEL_Adaptation
  envi_assign_header_value, fid=anc_fid, keyword='EVI_base_fix_info', $
      value=EVI_base_fix_info
  envi_assign_header_value, fid=anc_fid, keyword='evi_sat_info', $
      value=evi_sat_info
  envi_write_file_header, anc_fid  
  ;=====================================================================
  ;=====================================================================
  
  ; Do the final cleanup
  cleanup:
  free_lun, lun,/force
  free_lun, ofile,/force
  free_lun, tfile,/force
  heap_gc,/verbose
  
  ;;get state pointer
  ;widget_control,event.top,get_uvalue=pstate
  ;;clean up pointers
  ;widget_control,event.top,/destroy
  
  return
end