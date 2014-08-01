; Convert DWEL HDF5 data format to ENVI data cube (.img files).
; Running in command line mode.
;
; Zhan Li, zhanli86@bu.edu
; Adapted from heritage EVI's code, 2012
; Revison: March 2014

function CheckDWEL, DWEL_H5File, Wavelength, nadirelevshift
  compile_opt idl2
  
  scanenc_ind = 0
  rotateenc_ind = 1
  
  ;; open the HDF5 file
  fileid=h5f_open(DWEL_H5File)
  ;; read encoder dataset.
  encoderset = h5d_open(fileid,'/Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)
  
  ;; correct the shift of nadir
  encoders[scanenc_ind, *] = ((encoders[scanenc_ind, *] - nadirelevshift) + 524288) mod 524288
  
  ;; use the difference between every two scan encoder values to
  ;; determine the actual start and ending shots and to remove the
  ;;dummy shots at the beginning and the end.
  interval_diff = encoders[scanenc_ind, 0:dim_encoders[1]-2] - encoders[scanenc_ind, 1:dim_encoders[1]-1] ; the difference between two consecutive shots, the early one - the later one
  tmpind = where(interval_diff ne 0, tmpcount, ncomplement=count, complement=dummyind)
  if (tmpcount gt 0) then begin
    shotstart = tmpind[0]
    shotend = tmpind[size(tmpind, /n_elements)-1]
  endif else begin
    print, 'No valid scan encoder value! Processing is terminated!'
    h5d_close, encoderset
    h5f_close, fileid
    h5_close
    return, -1
  endelse
  
  ;; Find out the number of shots in each scan by identifying the
  ;;start and ending of a scan line.
  ;; If the difference is negative (actually comparing the difference
  ;; with -2^18 instead of zero to avoid possible positive differences
  ;;due to wiggles in the encoder values), it indicates the start of a
  ;;new scan.
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1   ; TotalNoScans is the dimension of the azimuth axis
  ShotStartVec = [shotstart, NegInd+1] ; the indexes of start positions of each scan line
  ShotEndVec = [NegInd, shotend] ; the indexes of ending positions of each scan line
  ShotNumVec = ShotEndVec - ShotStartVec + 1 ; the number of shots per each scan line
  
  ;; number of scan lines per a whole 360-degree rotation.
  NoScanPerRotation = fix(TotalNoScans * 524288.0 / abs(encoders[rotateenc_ind,shotstart]-encoders[rotateenc_ind,shotend]))
  
  ;; Get the largest number of shots in a scan. It is the dimension of the zenith axis
  NoShotsPerScan = max(ShotNumVec)
  
  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid, Waveset_Name)
  
  wave_type = h5d_get_type(waveset)
  wave_class = h5t_get_class(wave_type)
  if strcmp(wave_class, 'H5T_INTEGER', /fold_case) then begin
    wavespace = h5d_get_space(waveset) ; space is the actual place where the data can be read out.
    tmpdims = H5S_GET_SIMPLE_EXTENT_DIMS(wavespace)
    NoSamplesPerShot = tmpdims[0] ; number of bins per each waveform.
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
  H5_CLOSE  ; This is the crucial step - release all of HDF5's memory
  return, {DWELFileName:DWEL_H5File, NadirScanEncoder:nadirelevshift, $
    TotalNoScans:TotalNoScans, NoShotsPerScan:NoShotsPerScan, NoSamplesPerShot:NoSamplesPerShot, $
    FirstShotInd:shotstart, LastShotInd:shotend, $
    ShotStart:ShotStartVec, ShotEnd:ShotEndVec, ShotNum:ShotNumVec, $
    NoScanPerRotation:NoScanPerRotation}
end

function DataCube, DWEL_MetaInfo, DataCube_File, Wavelength
  compile_opt idl2
  
  scanenc_ind = 0
  rotateenc_ind = 1
  
  ;; create the name of the ancillary file from the given waveform
  ;;data cube file name.
  AncillaryFile = strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'
  
  fileid=h5f_open(DWEL_MetaInfo.DWELFileName)
  encoderset = h5d_open(fileid, '/Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  ;; virtually reverse the rotation direction of DWEL by changing the rotatary encoder values
  ;encoders[1, *] =  524288 - encoders[1, *]
  ; correct the shift of nadir
  encoders[scanenc_ind, *] = ((encoders[scanenc_ind, *] - DWEL_MetaInfo.NadirScanEncoder) + 524288) mod 524288
  
  ;; open the dataset of waveform and get the space of the dataset.
  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid,Waveset_Name)
  wave_space = h5d_get_space(waveset)
  memspace = h5s_create_simple([DWEL_MetaInfo.NoSamplesPerShot, 1])
  
  openw, DataCubeFID, DataCube_File, /get_lun
  openw, AncillaryFID, AncillaryFile, /get_lun
  
  ; get the smallest and largest elevation encoder values.
  ; the zenith angles of pixels in each scan line (a row in the produced image) are assumed evenly distributed between the lower and upper elevation encoders.
  ; each shot in one scan is sequently put into the pixel where its zenith angle is closest to the zenith of the shot.
  ;; LBScanEncoder = min(encoders[0, DWEL_MetaInfo.FirstShotInd:DWEL_MetaInfo.LastShotInd])
  ;; UBScanEncoder = max(encoders[0, DWEL_MetaInfo.FirstShotInd:DWEL_MetaInfo.LastShotInd])
  
  DataArray = intarr(DWEL_MetaInfo.NoShotsPerScan, DWEL_MetaInfo.NoSamplesPerShot)
  AncillaryArray = lonarr(DWEL_MetaInfo.NoShotsPerScan,9)
  Trigger = 0B
  SunSensor = 0B
  ScanEncoder = 0L
  RotaryEncoder = 0L
  LaserPower = 0E
  Pos = 0L
  mask=0b
  ShotZen=0.0
  ShotAzim=0.0
  
  shotind = DWEL_MetaInfo.FirstShotInd
  ;; PixelLoc = indgen(DWEL_MetaInfo.NoShotsPerScan)
  ;; PixelScanEncoder = LBScanEncoder + PixelLoc*(UBScanEncoder-LBScanEncoder)/(DWEL_MetaInfo.NoShotsPerScan-1)
  NumBlank = 0
  BlankVec = bytarr(DWEL_MetaInfo.NoShotsPerScan)
  ZeroWaveform = intarr(DWEL_MetaInfo.NoSamplesPerShot)
  for i = 0, DWEL_MetaInfo.TotalNoScans-1, 1 do begin
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; if the number of shots in a scan line is less than the given
    ;; NoShotsPerScan (number of column in a row), the following
    ;;generates a vector BlankPixelLoc which gives where we need some
    ;;dummy pixels to fill the extra space in this row.
    ScanEncoderVec =  encoders[scanenc_ind, DWEL_MetaInfo.ShotStart[i]:DWEL_MetaInfo.ShotEnd[i]]
    ;; check how many blank pixels there will be. Insert the blank
    ;; pixels to the largest gaps between neighboring scan encoder values.
    NumBlank = DWEL_MetaInfo.NoShotsPerScan - size(ScanEncoderVec,/n_elements)
    if NumBlank gt 0 then begin
      ;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;      ; Gap between neighboring scan encoder values (early minus later, only positive difference is accepted as gap since the scan encoder values are supposed decreasing)
      ;      DiffScanEncoderVec = ScanEncoderVec[0:DWEL_MetaInfo.ShotNum[i]-2]-ScanEncoderVec[1:DWEL_MetaInfo.ShotNum[i]-1]
      ;      ; add the gap between the first shot in this scan line and the upper boundary of all elevation encoder,
      ;      ; and the gap between the last shot in this scan line and the lower boundary of all elevation encoder values.
      ;      DiffScanEncoderVec = [UBScanEncoder-ScanEncoderVec[0], DiffScanEncoderVec, ScanEncoderVec[DWEL_MetaInfo.ShotNum[i]-1]-LBScanEncoder]
      ;      DescendingInd = reverse(sort(DiffScanEncoderVec))
      ;      ; check how many blank pixels each gap can accomodate when the angular size of each pixel is given by LB, UB and ShotNum.
      ;      NumPotentialBlankVec = round(DiffScanEncoderVec[DescendingInd]/(UBScanEncoder-LBScanEncoder)*(DWEL_MetaInfo.NoShotsPerScan) + 0.5) - 1
      ;      NumPotentialBlankVec[where(NumPotentialBlankVec lt 0)]=0
      ;      CumPotentialBlankVec = total(NumPotentialBlankVec[0:(NumBlank lt size(NumPotentialBlankVec, /n_elements) ? NumBlank : size(NumPotentialBlankVec, /n_elements))-1], /cumulative)
      ;      NumGap = where(CumPotentialBlankVec ge NumBlank) + 1
      ;      NumGap = NumGap[0]
      ;      if NumGap gt 1 then begin
      ;        NumActualBlankVec = [NumPotentialBlankVec[0:NumGap-2], NumBlank-CumPotentialBlankVec[NumGap-2]]
      ;      endif else begin
      ;        NumActualBlankVec = NumBlank
      ;        if NumGap eq 0 then begin
      ;          NumGap = NumBlank
      ;        endif
      ;      endelse
      ;      BlankInd = DescendingInd[0:NumGap-1] ; the blank locations in the ShotNum of this scan line
      ;      AscendingIndBlankInd = sort(BlankInd)
      ;      BlankInd = BlankInd[AscendingIndBlankInd]
      ;      NumActualBlankVec = NumActualBlankVec[AscendingIndBlankInd]
      ;      BlankPixelLoc = indgen(NumActualBlankVec[0])+BlankInd[0] ; the blank locations in the NoShotsPerScan
      ;      for bi = 1, NumGap-1, 1 do begin
      ;        BlankPixelLoc = [BlankPixelLoc, $
      ;          indgen(NumActualBlankVec[bi])+ $
      ;          BlankInd[bi]+total(NumActualBlankVec[0:bi-1]) ]
      ;      endfor
      ;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; simply insert blanck pixels at the end of each scan line
      BlankPixelLoc = indgen(NumBlank) + DWEL_MetaInfo.NoShotsPerScan-NumBlank
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    endif else begin ; if no blank pixel is needed
      BlankPixelLoc=-1
    endelse
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    for j = 0L, DWEL_MetaInfo.NoShotsPerScan-1, 1  do begin
      if (total((BlankPixelLoc eq j)) gt 0) or (shotind ge DWEL_MetaInfo.LastShotInd+1) then begin
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
      ScanEncoder = encoders[scanenc_ind, shotind]
      RotaryEncoder = encoders[rotateenc_ind, shotind]
      ShotZen = double(262144 - ScanEncoder) / double(524288) * 2 * 180.0
      ShotAzim = double(RotaryEncoder) / double(524288) * 2 * 180.0
      ;      ;;;;;; temporarily fake azimuth angle b/c of the wrong azimuth encoders.
      ;      ShotAzim = 2*(i+0.5)*0.001/!pi*180.0
      ;      RotaryEncoder = fix(ShotAzim/360.0*double(524288), type=size(ScanEncoder, /type))
      
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
  free_lun, DataCubeFID, /force
  free_lun, AncillaryFID, /force
  
  data_dims = size(DataArray)
  ancillary_dims = size(AncillaryArray)
  
  return, {samples:data_dims[1], lines:DWEL_MetaInfo.TotalNoScans, $
    databands:data_dims[2], ancillarybands:ancillary_dims[2], offset:0, $
    filetype:'ENVI Data Cube', datatype:data_dims[3], $
    ancillarydatatype:ancillary_dims[3], interleave:1, sensortype:'DWEL', $
    byteorder:0, wavelengthunit:'metres', range:120.0}
    
end

pro dwel2cube_cmd, DWEL_H5File, DataCube_File, Wavelength, Wavelength_Label, $
  DWEL_Height, beam_div, srate, nadirelevshift
  ;;
  ;; Because the names of the waveform datasets in HDF5 files were
  ;;incorrectly labeled as of March 2014, including all data from CA
  ;;Sierra June 2013 and Brisbane August 2013, two numbers of wavelength
  ;;are required here.
  ;; Wavelength: a number used to create a dataset name and get waveform
  ;; data from HDF5 file.
  ;; Wavelength_Label: a number to be put in the header file. This is
  ;;the CORRECT wavelength number.
  
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window
  
  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  DWEL_ND_Filter=0
  
  DWEL_MetaInfo = CheckDWEL(DWEL_H5File, Wavelength, nadirelevshift)
  HeaderInfo = DataCube(DWEL_MetaInfo, DataCube_File, Wavelength)
  
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
  
  DWEL_Adaptation=['Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift']
  DWEL_Adaptation=[DWEL_Adaptation, 'Wavelength='+strtrim(Wavelength_Label, 2)]
  DWEL_Adaptation=[DWEL_Adaptation, 'Nadir shift of scan encoder='+strtrim(nadirelevshift, 2)]
  
  ENVI_SETUP_HEAD, fname=DataCube_File, $
    ns=HeaderInfo.samples, nl=HeaderInfo.lines, nb=HeaderInfo.databands, $
    interleave=HeaderInfo.interleave, data_type=HeaderInfo.datatype, $
    offset=HeaderInfo.offset, zplot_titles=['Time (nsec)','Intensity'], $
    bnames=band_names, $
    wl=wl_out, /write, /open, r_fid=out_fid
    
  envi_assign_header_value, fid=out_fid, keyword='EVI_Scan_Info', $ ;keyword='DWEL_Scan_Info', $
    value=DWEL_Scan_Info
  envi_assign_header_value, fid=out_fid, $
    keyword='DWEL_Adaptation', $
    value=DWEL_Adaptation
  envi_write_file_header, out_fid
  
  anc_name=strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'
  
  ENVI_SETUP_HEAD, fname=anc_name, $
    ns=HeaderInfo.samples, nl=HeaderInfo.lines, nb=HeaderInfo.ancillarybands, $
    interleave=HeaderInfo.interleave, data_type=HeaderInfo.ancillarydatatype, $
    offset=HeaderInfo.offset, /write, /open, r_fid=anc_fid, $
    bnames=['Non Triggers','Sun Sensor','Scan Encoder','Rotary Encoder', $
    'Laser Power','Waveform Mean','Mask','Zenith','Azimuth']
    
  envi_assign_header_value, fid=anc_fid, $
    keyword='EVI_Scan_Info', $ ;keyword='DWEL_Scan_Info', $
    value=DWEL_Scan_Info
  envi_assign_header_value, fid=anc_fid, $
    keyword='DWEL_Adaptation', $
    value=DWEL_Adaptation
  envi_write_file_header, anc_fid
  
end
