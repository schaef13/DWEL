; Convert DWEL HDF5 data format to ENVI data cube (.img files)
; Running in commandline mode

function AlignedCheckDWEL, DWEL_H5File, Wavelength
  compile_opt idl2
  
  fileid=h5f_open(DWEL_H5File)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)
    
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
  endelse
  
  ; Find out the number of shots in each scan. 
  ; The largest number will be used as the dimension (number of pixels) along the elevation or zenith axis.
  ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1
  ShotStartVec = [shotstart, NegInd+1]
  ShotEndVec = [NegInd, shotend]
  ShotNumVec = ShotEndVec - ShotStartVec + 1
  
  NoScanPerRotation = fix(TotalNoScans * 524288.0 / abs(encoders[1,0]-encoders[1,shotend]))
  
  ; TotalNoScans is the dimension of the azimuth axis
  ; get the largest number of shots in a scan. It is the dimension of the zenith axis
  NoShotsPerScan = max(ShotNumVec)
  
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
    TotalNoScans:TotalNoScans, NoShotsPerScan:NoShotsPerScan, NoSamplesPerShot:NoSamplesPerShot, $
    FirstShotInd:shotstart, LastShotInd:shotend, $
    NoScanPerRotation:NoScanPerRotation}
end

function AlignedDataCube, DWEL_MetaInfo, DataCube_File, AlignedMaskFile, Wavelength
  compile_opt idl2
  
  ; read the mask of aligned scanning image
  AlignedMask = read_tiff(AlignedMaskFile)
  tmp = size(AlignedMask, /dimensions)
  aligned_ns = tmp[0]
  aligned_nl = tmp[1]
  ; update the meta information in the structural variable DWEL_MetaInfo
  DWEL_MetaInfo.NoScanPerRotation = fix(DWEL_MetaInfo.NoScanPerRotation * aligned_nl / DWEL_MetaInfo.TotalNoScans)
  DWEL_MetaInfo.TotalNoScans = aligned_nl
  DWEL_MetaInfo.NoShotsPerScan = aligned_ns
  
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
  AncillaryArray = lon64arr(aligned_ns,9)
  Trigger = 0B
  SunSensor = 0B
  ScanEncoder = 0LL
  RotaryEncoder = 0LL
  LaserPower = 0E
  Pos = 0LL
  mask=0b
  ShotZen=0.0
  ShotAzim=0.0
  
  shotind = DWEL_MetaInfo.FirstShotInd
  ZeroWaveform = intarr(DWEL_MetaInfo.NoSamplesPerShot)
  for i = 0, aligned_nl-1, 1 do begin    
    for j = 0L, aligned_ns-1, 1  do begin
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
      AncillaryArray[j,*] = [fix(Trigger), fix(SunSensor), ScanEncoder, RotaryEncoder, $
        fix(round(LaserPower*100.0)), fix(round(WaveformMax)), fix(mask), fix(round(10.0*ShotZen)), $
        fix(round(10.0*ShotAzim))]
        
      shotind = shotind + 1
    endfor
    
    writeu, DataCubeFID, DataArray
    writeu, AncillaryFID, AncillaryArray
  endfor
  
  h5s_close, wave_space
  h5s_close, memspace
  h5d_close, waveset
  h5d_close, encoderset
  h5f_close, fileid
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

pro DWEL2AlignedCube_cmd, DWEL_H5File, AlignedMaskFile, DataCube_File, Wavelength, Wavelength_Label, DWEL_Height, $
  beam_div, srate

  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init

  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  DWEL_ND_Filter=0

  DWEL_MetaInfo = AlignedCheckDWEL(DWEL_H5File, Wavelength)
  HeaderInfo = AlignedDataCube(DWEL_MetaInfo, DataCube_File, AlignedMaskFile, Wavelength)
  
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
