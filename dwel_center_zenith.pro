pro dwel_center_zenith, DWEL_AncFile, Casing_MaskFile
  COMPILE_OPT IDL2
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, /NO_STATUS_WINDOW
  
  ;open the range mask file
  envi_open_file, Casing_MaskFile, r_fid=maskfile_fid,/no_realize
  if (maskfile_fid eq -1) then begin
      print,strtrim('Error opening input mask file',2)
      print,'Input File: '+strtrim(Casing_MaskFile,2)
      return
  endif
  envi_file_query, maskfile_fid, ns=mask_ns, nl=mask_nl, nb=mask_nb, $
    xstart=xstart, ystart=ystart, data_type=mask_type, $
    interleave=mask_ftype, fname=mask_fname, dims=mask_dims

  envi_open_file, DWEL_AncFile, r_fid=anc_fid, /no_realize
  if (anc_fid eq -1) then begin
      print,strtrim('Error opening input ancillary file',2)
      print,'Input File: '+strtrim(DWEL_AncFile,2)
      return
  endif
  envi_file_query, anc_fid, ns=anc_ns, nl=anc_nl, nb=anc_nb, $
    xstart=xstart, ystart=ystart, data_type=anc_type, $
    interleave=anc_ftype, fname=anc_fname, dims=anc_dims

  f_base = file_basename(DWEL_AncFile)
  ;now get the EVI headers that are present
  ;set up a base structure for the EVI headers
  evi_headers={ $
       f_base:f_base $
       }  
  ;find all of the EVI headers in the hdr file as defined by FID
  status=get_headers(anc_fid,evi_headers)
    
  mask = envi_get_data(dims=anc_dims, fid=maskfile_fid, pos=0)
  
  ; find the position of the two edges of the case and store them in an array
  CaseEdgePos = intarr(2, mask_nl)
  ; here we assume the casing are split and at the sides of a scanning image. 
  ; we start from the center pixel of a line and search the first mask pixel to the two sides. 
  for il=0, mask_nl-1 do begin
    tmpind = where(mask[0:mask_ns/2, il], tmpnum)
    if tmpnum le 0 then begin
      CaseEdgePos[0, il] = -1
    endif else begin
      CaseEdgePos[0, il] = tmpind[tmpnum-1]
    endelse
    
    tmpind = where(mask[mask_ns/2+1:mask_ns-1, il], tmpnum)
    if tmpnum le 0 then begin
      CaseEdgePos[1, il] = -1
    endif else begin
      CaseEdgePos[1, il] = mask_ns/2+1+tmpind[0]
    endelse
    
  endfor
  ; synthesize the scan encoder according to the edges of the case
  Syn_ScanEncoder = make_array(anc_ns, anc_nl, type=anc_type)
  for il=1, anc_nl-1 do begin
    if CaseEdgePos[0, il] eq -1 or CaseEdgePos[1, il] eq -1 then begin
      continue
    endif 
    out_interval = 220.0/360.0*524288.0/float(CaseEdgePos[1,il]-1-CaseEdgePos[0,il])
    casing_interval = (360.0-220.0)/360.0*524288.0/float(anc_ns-(CaseEdgePos[1,il]-1-CaseEdgePos[0,il]))
    zenith_pos = (CaseEdgePos[0,il]+CaseEdgePos[1,il])/2.0
    tmppos = fix((CaseEdgePos[0,il]+CaseEdgePos[1,il])/2)
    Syn_ScanEncoder[CaseEdgePos[0,il]+1:CaseEdgePos[1,il]-1, il] = $
      (indgen(CaseEdgePos[1,il]-1-CaseEdgePos[0,il])+CaseEdgePos[0,il]+1-zenith_pos)*out_interval+262144.0
    Syn_ScanEncoder[0:CaseEdgePos[0,il], il] = $
      (indgen(CaseEdgePos[0,il]+1)-(CaseEdgePos[0,il]+1))*casing_interval+Syn_ScanEncoder[CaseEdgePos[0,il]+1, il]
    Syn_ScanEncoder[CaseEdgePos[1,il]:anc_ns-1, il] = $
      (indgen(anc_ns-CaseEdgePos[1,il])+CaseEdgePos[1,il]-(CaseEdgePos[1,il]-1))*casing_interval+Syn_ScanEncoder[CaseEdgePos[1,il]-1, il]
    tmpind = where(Syn_ScanEncoder[*,il] lt 0, tmpnum)
    if tmpnum gt 0 then begin
      Syn_ScanEncoder[tmpind, il] = Syn_ScanEncoder[tmpind, il] + 524288.0 
    endif
    tmpind = where(Syn_ScanEncoder[*,il] gt 524288.0, tmpnum)
    if tmpnum gt 0 then begin
      Syn_ScanEncoder[tmpind, il] = Syn_ScanEncoder[tmpind, il] - 524288.0 
    endif
  endfor
  
  
  anc_data = make_array(anc_ns, anc_nl, anc_nb, type=anc_type)
   for j=0,anc_nb-1 do begin
    anc_data[*,*,j]=ENVI_GET_DATA(fid=anc_fid, dims=[-1L,0,anc_ns-1,0,anc_nl-1], pos=j)
  endfor
  Syn_ScanEncoder[*,0] = anc_data[*,0,2]
  
  ; update the zenith and azimuth angle
  ShotZen = double(262144 - Syn_ScanEncoder) / double(524288) * 2 * 180.0
  ShotAzim = double(anc_data[*,*,3]) / double(524288) * 2 * 180.0
  tmpind = where(ShotZen lt 0, tmpcount, /L64)
  if tmpcount gt 0 then begin
    ShotZen[tmpind] = ShotZen[tmpind]*(-1)
    ShotAzim[tmpind] = ShotAzim[tmpind]+180.0 
  endif
  tmpind = where(ShotAzim gt 360.0, tmpcount, /L64)
  if tmpcount gt 0 then begin
    ShotAzim[tmpind] = ShotAzim[tmpind]-360.0
  endif
  
  tmpind = where(anc_data[*,*,6], tmpcount, COMPLEMENT=badind, /L64, NCOMPLEMENT=badnum)
  if badnum gt 0 then begin
    Syn_ScanEncoder[badind]=0
    ShotZen[badind] = 0
    ShotAzim[badind] = 0
  endif
  anc_data[*,*,2] = Syn_ScanEncoder
  anc_data[*,*,7]=ShotZen
  anc_data[*,*,8]=ShotAzim
  
  close, anc_fid
  
  openw, ofid, DWEL_AncFile, /get_lun,error=text_err
  if (text_err ne 0) then begin
     print, strtrim('Halting dwel_center_zenith', 2)
     print, strtrim(['Error opening output file '+strtrim(DWEL_AncFile,2)], 2)
     return
  endif
  for j=0,anc_nb-1 do begin
    writeu, ofid, anc_data[*,*,j]
  endfor
  
 ENVI_SETUP_HEAD, fname=DWEL_AncFile, $
   ns=anc_ns, nl=anc_nl, nb=9, $
   interleave=0, data_type=size(anc_data, /type), $
   /write, /open, r_fid=ofid, $
   bnames=['Non Triggers','Sun Sensor','Scan Encoder','Rotary Encoder', $
   'Laser Power','Waveform Mean','Mask','Zenith','Azimuth']
  
 ;write out the previous header records
 status=put_headers(ofid,evi_headers)

 close, ofid
end