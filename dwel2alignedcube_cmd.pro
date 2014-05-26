; Convert DWEL HDF5 data format to ENVI data cube (.img files)
; Running in commandline mode

function AlignedCheckDWEL, DWEL_H5File, Wavelength
  compile_opt idl2

  scanenc_ind = 0
  rotateenc_ind = 1
  
  fileid=h5f_open(DWEL_H5File)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)
    
  ;; find the first valid and last valid shot. 
  interval_diff = encoders[0, 0:dim_encoders[1]-2] - encoders[0, 1:dim_encoders[1]-1] ; the difference between two consecutive shots, the early one - the later one
  tmpind = where(interval_diff ne 0, tmpcount, ncomplement=count, complement=dummyind)
  if (tmpcount gt 0) then begin
    shotstart = tmpind[0]+1
    shotend = tmpind[size(tmpind, /n_elements)-1]
  endif else begin
    print, 'No valid scan encoder value! Processing is terminated!'
    h5d_close, encoderset
    h5f_close, fileid
    H5_CLOSE
    return, -1
 ENDELSE

  ;; find the average azimuth interval
  az_diff = encoders[rotateenc_ind, shotstart:shotend-1] - encoders[rotateenc_ind, shotstart+1:shotend]
  mean_az_diff = mean(abs(az_diff))
  
  ;; ; Find out the number of shots in each scan. 
  ;; ; The largest number will be used as the dimension (number of pixels) along the elevation or zenith axis.
  ;; ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  ;; NegInd = where(interval_diff lt -262144, NegCount, /L64)
  ;; TotalNoScans = NegCount + 1
  ;; ShotStartVec = [shotstart, NegInd+1]
  ;; ShotEndVec = [NegInd, shotend]
  ;; ShotNumVec = ShotEndVec - ShotStartVec + 1
  
  ;; NoScanPerRotation = fix(TotalNoScans * 524288.0 / abs(encoders[1,0]-encoders[1,shotend]))
  
  ;; ; TotalNoScans is the dimension of the azimuth axis
  ;; ; get the largest number of shots in a scan. It is the dimension of the zenith axis
  ;; NoShotsPerScan = max(ShotNumVec)
  
  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid,Waveset_Name)
  wave_type = h5d_get_type(waveset)
  wave_class = h5t_get_class(wave_type)
  if strcmp(wave_class, 'H5T_INTEGER', /fold_case) then begin
    wavespace = h5d_get_space(waveset)
    tmpdims = H5S_GET_SIMPLE_EXTENT_DIMS(wavespace)
    NoSamplesPerShot = tmpdims[0]
  endif else begin
    print, Waveset_Name + ': data type is not H5T_INTEGER as expected. Please check the HDF5 file.'
    h5t_close, wave_type
    h5d_close, waveset
    h5d_close, encoderset
    h5f_close, fileid
    H5_CLOSE
    return, -1
  endelse
  h5t_close, wave_type
  
  h5d_close, waveset
  h5d_close, encoderset
  h5f_close, fileid  
  ; This is the crucial step - release all of HDF5's memory
  H5_CLOSE
  return, {DWELFileName:DWEL_H5File, $
    TotalNoScans:-1, NoShotsPerScan:-1, NoSamplesPerShot:NoSamplesPerShot, $
    FirstShotInd:shotstart, LastShotInd:shotend, $
    NoScanPerRotation:-1, AzInterval:mean_az_diff}
end

function AlignedDataCube, DWEL_MetaInfo, Flag_H5File, DataCube_File, AlignedMaskFile, Wavelength
  compile_opt idl2

  ;; some critical constant values
  casing_zenith_span = 123.2

; read the mask of aligned scanning image
  AlignedMask = read_tiff(AlignedMaskFile)
  tmp = size(AlignedMask, /dimensions)
  aligned_ns = tmp[0]
  aligned_nl = tmp[1]
; update the meta information in the structural variable DWEL_MetaInfo
  DWEL_MetaInfo.NoScanPerRotation = fix(DWEL_MetaInfo.NoScanPerRotation * aligned_nl / DWEL_MetaInfo.TotalNoScans)
  DWEL_MetaInfo.TotalNoScans = aligned_nl
  DWEL_MetaInfo.NoShotsPerScan = aligned_ns
  DWEL_MetaInfo.NoScanPerRotation = fix(DWEL_MetaInfo.TotalNoScans * 524288.0 / DWEL_MetaInfo.AzInterval)

  AncillaryFile = strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'

  fileid=h5f_open(DWEL_MetaInfo.DWELFileName)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
;; NOTE HERE: SCAN ENCODERS ARE NOT USED IN THIS ROUTINE. INSTEAD WE
;;FAKE SCAN ENCODERS HERE FOR ALIGNED SCANNING IMAGE!!!
  encoders = h5d_read(encoderset)
;; virtually reverse the rotation direction of DWEL by changing the rotatary encoder values
;encoders[1, *] =  524288 - encoders[1, *]

  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid,Waveset_Name)
  wave_space = h5d_get_space(waveset)
  memspace = h5s_create_simple([DWEL_MetaInfo.NoSamplesPerShot, 1])

  openw, DataCubeFID, DataCube_File, /get_lun
  openw, AncillaryFID, AncillaryFile, /get_lun

  DataArray = intarr(aligned_ns, DWEL_MetaInfo.NoSamplesPerShot)
  AncillaryArray = lonarr(aligned_ns,9)
  Trigger = 0B
  SunSensor = 0B
  ScanEncoder = 0LL
  RotaryEncoder = 0LL
  LaserPower = 0E
  Pos = 0LL
  mask=0b
  ShotZen=0.0
  ShotAzim=0.0

;; read the dataset of "Flag" which labels each shot inside or
;; outside the case
  flag_fid = h5f_open(Flag_H5File)
  flagset = h5d_open(flag_fid, '/Flag')
  shotflag = h5d_read(flagset)

  shotind = DWEL_MetaInfo.FirstShotInd
  ZeroWaveform = intarr(DWEL_MetaInfo.NoSamplesPerShot)
  for i = 0, aligned_nl-1, 1 do BEGIN
;; the flag of one scan line
     flag_line = intarr(DWEL_MetaInfo.NoShotsPerScan)
     for j = 0, aligned_ns-1, 1  do begin
        if (AlignedMask[j, i] eq 0) or (shotind ge DWEL_MetaInfo.LastShotInd+1) then begin
           mask = 0
           AncillaryArray[j,*] = [fix(Trigger), fix(SunSensor), 0, 0, $
                                    0, 0, fix(mask), 0, $
                                    0]
           DataArray[j,*] = ZeroWaveform
           continue
        endif
        H5S_SELECT_HYPERSLAB, wave_space, [[shotind], [0]], [[1], [DWEL_MetaInfo.NoSamplesPerShot]], /reset
        Waveform = h5d_read(waveset, file_space=wave_space, MEMORY_SPACE = memspace)
        WaveformMean = mean(waveform)
        WaveformMax = max(waveform, max_I)

        DataArray[j,*] = Waveform
; fake scan encoder values and zenith angles from the aligned image
        ScanEncoder = 524288*(1 - (j+0.5)/double(aligned_ns))
        ShotZen = double(262144 - ScanEncoder) / double(524288) * 2 * 180.0
        RotaryEncoder = encoders[1, shotind]
        ShotAzim = double(RotaryEncoder) / double(524288) * 2 * 180.0
;      ;;;;;; temporarily fake azimuth angle b/c of the wrong azimuth encoders. 
;      ShotAzim = 2*(i+0.5)*0.001/!pi*180.0
;      RotaryEncoder = fix(ShotAzim/360.0*double(524288), type=14)

        if (ShotZen lt 0.0) then begin
           ShotZen=-ShotZen
           ShotAzim=ShotAzim+180.0
        endif
        if (ShotAzim gt 360.0) then ShotAzim=ShotAzim-360.0

        mask = 1

        ;; get the flags of shots in this line and label the casing
        ;; shots       
        IF shotflag[shotind] EQ 1 THEN BEGIN
           flag_line[j] = 1
        ENDIF 
        IF shotflag[shotind] EQ 2 THEN BEGIN
           flag_line[j] = 0
        ENDIF 

        AncillaryArray[j,*] = [fix(Trigger), fix(SunSensor), ScanEncoder, RotaryEncoder, $
        fix(round(LaserPower*100.0)), fix(round(WaveformMax)), fix(mask), fix(round(10.0*ShotZen)), $
                                 fix(round(10.0*ShotAzim))]

        shotind = shotind + 1
     ENDFOR
    
    ;; synthesize the scan encoder according to the edges of the case
    Syn_ScanEncoder = lonarr(aligned_ns)
    ;; search casing edge in a narrower area rather than the whole
    ;; line to avoid noise casing pixels from casing mask outside
    ;;actual casing area. The casing mask can never be perfect without
    ;;any error casing pixel. 
    leftendpos = fix(aligned_ns * (casing_zenith_span/2.0 + 5)/360.0) ;; 5 is a buffer area to make sure the searching area includes the casing edge. 
    ;; find the position of the two edges of the case
    tmpind = where(flag_line[0:leftendpos], tmpnum)
    IF tmpnum LE 0 THEN BEGIN
       CaseEdgePos1 = -1
    ENDIF ELSE BEGIN
       CaseEdgePos1 = tmpind[tmpnum-1]
    ENDELSE 
    rightstartpos = fix(aligned_ns - aligned_ns * (casing_zenith_span/2.0 + 5)/360.0)
    tmpind = where(flag_line[rightstartpos:aligned_ns-1], tmpnum)
    IF tmpnum LE 0 THEN BEGIN
       CaseEdgePos2 = -1
    ENDIF ELSE BEGIN
       CaseEdgePos2 = rightstartpos+tmpind[0]
    ENDELSE    
    IF CaseEdgePos1 NE -1 AND CaseEdgePos2 NE -1 THEN BEGIN
       out_interval = (360.0 - casing_zenith_span)/360.0*524288.0/float(CaseEdgePos2 - CaseEdgePos1)
       casing_interval = casing_zenith_span/360.0*524288.0/float(aligned_ns - (CaseEdgePos2 - CaseEdgePos1))
       zenith_pos = (CaseEdgePos1 + CaseEdgePos2)/2.0
       Syn_ScanEncoder[CaseEdgePos1+1:CaseEdgePos2-1] = (indgen(CaseEdgePos2-1-CaseEdgePos1)+CaseEdgePos1+1-zenith_pos)*(-1)*out_interval + 262144.0
       Syn_ScanEncoder[0:CaseEdgePos1] = (indgen(CaseEdgePos1+1)-(CaseEdgePos1+1))*(-1)*casing_interval + Syn_ScanEncoder[CaseEdgePos1+1]
       Syn_ScanEncoder[CaseEdgePos2:aligned_ns-1] = (indgen(aligned_ns-CaseEdgePos2)+1)*(-1)*casing_interval + Syn_ScanEncoder[CaseEdgePos2-1]
       tmpind = where(Syn_ScanEncoder LT 0, tmpnum)
       IF tmpnum GT 0 THEN BEGIN
          Syn_ScanEncoder[tmpind] = Syn_ScanEncoder[tmpind] + 524288.0
       ENDIF 
       tmpind = where(Syn_ScanEncoder GT 524288.0, tmpnum)
       IF tmpnum GT 0 THEN BEGIN
          Syn_ScanEncoder[tmpind] = Syn_ScanEncoder[tmpind] - 524288.0
       ENDIF 
       ;; update the scan encoder
       AncillaryArray[*, 2] = Syn_ScanEncoder
       ;; update the zenith angle
       ShotZen = double(262144 - Syn_ScanEncoder)/524288.0 * 2* 180.0
       ShotAzim = double(AncillaryArray[*, 3])/524288.0 * 2 * 180.0
       tmpind = where(ShotZen lt 0, tmpcount)
       if tmpcount gt 0 then begin
          ShotZen[tmpind] = ShotZen[tmpind]*(-1)
          ShotAzim[tmpind] = ShotAzim[tmpind]+180.0 
       endif
       tmpind = where(ShotAzim gt 360.0, tmpcount)
       if tmpcount gt 0 then begin
          ShotAzim[tmpind] = ShotAzim[tmpind]-360.0
       ENDIF
       ;; update the zenith and azimuth angle
       AncillaryArray[*, 7] = fix(ShotZen*10)
       AncillaryArray[*, 8] = fix(ShotAzim*10)

       ;; ;; debug
       ;; ;; make mask as the casing mask
       ;; AncillaryArray[CaseEdgePos1:CaseEdgePos2, 6] = 0
    ENDIF 
    
    writeu, DataCubeFID, DataArray
    writeu, AncillaryFID, AncillaryArray
  endfor
  
  h5s_close, wave_space
  h5s_close, memspace
  h5d_close, waveset
  h5d_close, encoderset
  h5f_close, fileid
  h5d_close, flagset
  h5f_close, flag_fid
  ; This is the crucial step - release all of HDF5's memory
  H5_CLOSE
  
  close, DataCubeFID
  close, AncillaryFID
  free_lun, DataCubeFID,/force
  free_lun, AncillaryFID,/force
  
  data_dims = size(DataArray)
  ancillary_dims = size(AncillaryArray)
  
  return, {samples:data_dims[1], lines:aligned_nl, $
    databands:data_dims[2], ancillarybands:ancillary_dims[2], offset:0, $
    filetype:'ENVI Data Cube', datatype:data_dims[3], $
    ancillarydatatype:ancillary_dims[3], interleave:1, sensortype:'DWEL', $
    byteorder:0, wavelengthunit:'metres', range:120.0}
  
end

pro DWEL2AlignedCube_cmd, DWEL_H5File, Flag_H5File, AlignedMaskFile, DataCube_File, Wavelength, Wavelength_Label, DWEL_Height, $
  beam_div, srate

  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /NO_STATUS_WINDOW

  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  DWEL_ND_Filter=0

  DWEL_MetaInfo = AlignedCheckDWEL(DWEL_H5File, Wavelength)
  HeaderInfo = AlignedDataCube(DWEL_MetaInfo, Flag_H5File, DataCube_File, AlignedMaskFile, Wavelength)
  
  ;get path and evi_file name as separate strings
  last=strpos(DWEL_H5File,path_sep(),/reverse_search)
  f_path=strmid(DWEL_H5File,0,last+1)
  f_base=strtrim(strmid(DWEL_H5File,last+1,strlen(DWEL_H5File)-last-1),2)
  
  ;Name_Info=['Original DWEL File =  '+strtrim(f_base,2)]
  Name_Info=['Original EVI File =  '+strtrim(f_base,2)]
  Site_Info=[ $
  'Data Start Time = '+'24h00m00s  Monday  August  13 9999',$
  'Data End Time =  '+'24h00m00s  Monday  August  13 9999',$
  'Scan Duration = '+'9999'+' minutes',$
  'Latitude = '+strtrim(string(-9999,format='(f10.2)'),2)+' Degrees',$
  'Longitude = '+strtrim(string(-9999,format='(f10.2)'),2)+' Degrees',$
  'Elevation = '+strtrim(string(-9999,format='(f10.2)'),2)+' Metres',$
  'Bearing = '+strtrim(string(-9999,format='(f10.2)'),2)+' Degrees',$
  'Scan Description = '+strtrim('DWEL Test Scan',2)]
  Scan_Info=[ $
  'Beam Divergence = '+strtrim(string(beam_div,format='(f10.2)'),2)+' mrad',$
  'Scan Configuration = '+strtrim(string(-9999),2),$
  'Number of Hemispheres = '+strtrim(string(1),2),$
  'Scans per Complete Rotation = '+strtrim(string(DWEL_MetaInfo.NoScanPerRotation),2),$
  'Number of Scans selected = '+strtrim(string(DWEL_MetaInfo.TotalNoScans),2),$
  'Number of Shots per Scan = '+strtrim(string(DWEL_MetaInfo.NoShotsPerScan),2),$
  'Number of Samples per Shot = '+strtrim(string(DWEL_MetaInfo.NoSamplesPerShot),2),$
  'Digitised Range = '+strtrim(string(120.0,format='(f10.2)'),2)+' Metres',$
  'Digitiser Sampling Rate = '+strtrim(string(srate,format='(f10.2)'),2)+' smp/ns']
  Post_Info=[ $
  'Data Start = '+strtrim(string(0),2),$
  'Actual scans completed = '+strtrim(string(DWEL_MetaInfo.TotalNoScans),2)]
  
  DWEL_Scan_Info=[Name_Info,Site_Info,Scan_Info,Post_Info]
  if (DWEL_ND_Filter le 10) then $
  DWEL_Scan_Info = [DWEL_Scan_Info,'ND_Filter='+strtrim(DWEL_ND_Filter,2), $
    'Filter Name='+strtrim(ND_Nam[DWEL_ND_Filter],2)]
  if (DWEL_Height ge 0) then $
  DWEL_Scan_Info = [DWEL_Scan_Info,'EVI Height='+strtrim(DWEL_Height,2)]
  
  DSR = srate
  wl_out=findgen(HeaderInfo.databands)/DSR
  band_names=strarr(HeaderInfo.databands)+'Time_Sample_'
  band_names=band_names+strtrim(string(indgen(HeaderInfo.databands)+1),2)
  
  ;get output_file name without path
  last=strpos(DataCube_File,path_sep(),/reverse_search)
  out_base=strtrim(strmid(DataCube_File,last+1,strlen(DataCube_File)-last-1),2)
  DWEL_Scan_Info=[DWEL_Scan_Info,'Output Cube File = '+out_base]
  
  ENVI_SETUP_HEAD, fname=DataCube_File, $
    ns=HeaderInfo.samples, nl=HeaderInfo.lines, nb=HeaderInfo.databands, $
    interleave=HeaderInfo.interleave, data_type=HeaderInfo.datatype, $
    offset=HeaderInfo.offset, zplot_titles=['Time (nsec)','Intensity'], $
    bnames=band_names, $
    wl=wl_out, /write, /open, r_fid=out_fid
  
  envi_assign_header_value, fid=out_fid, keyword='EVI_Scan_Info', $ ;keyword='DWEL_Scan_Info', $
      value=DWEL_Scan_Info
  envi_write_file_header, out_fid
  
  anc_name=strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'
  
  ENVI_SETUP_HEAD, fname=anc_name, $
    ns=HeaderInfo.samples, nl=HeaderInfo.lines, nb=HeaderInfo.ancillarybands, $
    interleave=HeaderInfo.interleave, data_type=HeaderInfo.ancillarydatatype, $
    offset=HeaderInfo.offset, /write, /open, r_fid=anc_fid, $
    bnames=['Non Triggers','Sun Sensor','Scan Encoder','Rotary Encoder', $
    'Laser Power','Waveform Mean','Mask','Zenith','Azimuth']
  
  DWEL_Adaptation=['Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift in HDF raw data']
  DWEL_Adaptation=[DWEL_Adaptation, 'Wavelength='+strtrim(Wavelength_Label, 2)]
  envi_assign_header_value, fid=anc_fid, $
    keyword='DWEL_Adaptation', $
    value=DWEL_Adaptation
  envi_assign_header_value, fid=anc_fid, $   
    keyword='EVI_Scan_Info', $ ;keyword='DWEL_Scan_Info', $
    value=DWEL_Scan_Info
    
  envi_write_file_header, anc_fid
end
