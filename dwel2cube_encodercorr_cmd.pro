; Convert DWEL HDF5 data format to ENVI data cube (.img files)
; Running in commandline mode

function CheckDWEL_EncoderCorr, DWEL_H5File, Wavelength, nadirelevshift, ScanEncoderCorrection
  compile_opt idl2
  
  ; test
  ;shotend = 1000000
  
  fileid=h5f_open(DWEL_H5File)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)
  
  ;dummyind = where(encoders[0, *] eq 0, count)
  dummyind = where(encoders[0, *] eq encoders[0, dim_encoders[1]-1], count)
  if (count gt 0) then begin
    shotend = dummyind[0]-1
  endif else begin
    shotend = dim_encoders[1]-1
  endelse
  
  ; test
  ;if dim_encoders[0] lt shotend then shotend=dim_encoders[0]
  
  ;test
  ;shotend = 729777
  
  ; Find out the number of shots in each scan. 
  ; The largest number will be used as the dimension (number of pixels) along the elevation or zenith axis.
  descending = 0b ; descending flag
  ScanNum = 0u ; records the number of scans
  shotnum = 0
;  ScanNumChunk = 1000u;
;  ShotNumVec = uintarr(ScanNumChunk) ; record the number of shots in each scan, the number of scans here is an estimate from estimate scan resolution.
;  ShotStartVec = lon64arr(ScanNumChunk) ; record the shot index of the first shot in each scan
;  ShotEndVec = lon64arr(ScanNumChunk) ; record the shot index of the first shot in each scan
  
  interval_diff = encoders[0, 0:shotend-1] - encoders[0, 1:shotend] ; the difference between two consecutive shots, the early one - the later one
  ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1
  ShotStartVec = [0, NegInd+1]
  ShotEndVec = [NegInd, shotend]
  ShotNumVec = ShotEndVec - ShotStartVec + 1
  for i=0,TotalNoScans-1,1 do begin
    encoders[0, ShotStartVec[i]:ShotEndVec[i]] = $
      encoders[0, ShotStartVec[i]:ShotEndVec[i]] + ScanEncoderCorrection[i]
  endfor
  tmpind = where(encoders[0, *] lt 0, tmpcount)
  if (tmpcount gt 0) then begin
    encoders[0, tmpind] = encoders[0, tmpind] + 524288
  endif
  tmpind = where(encoders[0, *] gt 524288, tmpcount)
  if (tmpcount gt 0) then begin
    encoders[0, tmpind] = encoders[0, tmpind] - 524288
  endif
  
  ; correct the shift of nadir
  encoders[0, *] = ((encoders[0, *] - nadirelevshift) + 524288) mod 524288
  
  ; recalculate the number of shots per each scan line
  interval_diff = encoders[0, 0:shotend-1] - encoders[0, 1:shotend] ; the difference between two consecutive shots, the early one - the later one
  ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1
  ShotStartVec = [0, NegInd+1]
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
  if strcmp(wave_class, 'H5T_INTEGER', /fold_case) then begin ; if strcmp(wave_class, 'H5T_ARRAY', /fold_case) then begin
    ;NoSamplesPerShot = h5t_get_array_dims(wave_type)
    wavespace = h5d_get_space(waveset)
    tmpdims = H5S_GET_SIMPLE_EXTENT_DIMS(wavespace)
    NoSamplesPerShot = tmpdims[0]
  endif else begin
    h5t_close, wave_type
    h5d_close, waveset
    h5d_close, encoderset
    h5f_close, fileid
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
    LastShotInd:shotend, $
    ShotStart:ShotStartVec, ShotEnd:ShotEndVec, ShotNum:ShotNumVec, $
    NoScanPerRotation:NoScanPerRotation, $
    CorrectedScanEncoder:encoders[0,*]}
end

function DataCube_EncoderCorr, DWEL_MetaInfo, DataCube_File, Wavelength, nadirelevshift, ScanEncoderCorrection
  compile_opt idl2
  
  AncillaryFile = strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'
  
  fileid=h5f_open(DWEL_MetaInfo.DWELFileName)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
;  ; correct the shift of nadir
;  encoders[0, *] = ((encoders[0, *] - nadirelevshift) + 524288) mod 524288
  ;; virtually reverse the rotation direction of DWEL by changing the rotatary encoder values
  ;encoders[0, *] =  524288 - encoders[0, *]
  
  encoders[0, *] = DWEL_MetaInfo.CORRECTEDSCANENCODER
  
  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid,Waveset_Name)
  wave_space = h5d_get_space(waveset)
  memspace = h5s_create_simple([DWEL_MetaInfo.NoSamplesPerShot, 1])
;  memspace = h5s_create_simple([1])
  ;h5s_select_elements, memspace, [0], /reset
  
  ;
  ; wave_datatype = h5d_get_type(waveset)
;  name = H5T_GET_CLASS(wave_datatype)
;  ndims = H5T_GET_ARRAY_DIMS(wave_datatype)
;  fields = H5T_GET_NMEMBERS(wave_datatype)
;print, H5T_GET_SIZE(wave_datatype)
;print, H5T_get_class(H5T_GET_SUPER(wave_datatype))
;print, H5T_get_size(H5T_GET_SUPER(wave_datatype))
;print, H5T_get_sign(H5T_GET_SUPER(wave_datatype))
  
  openw, DataCubeFID, DataCube_File, /get_lun
  openw, AncillaryFID, AncillaryFile, /get_lun
  
  ; get the smallest and largest elevation encoder values.
  ; the zenith angles of pixels in each scan line (a row in the produced image) are assumed evenly distributed between the lower and upper elevation encoders. 
  ; each shot in one scan is sequently put into the pixel where its zenith angle is closest to the zenith of the shot. 
  LBScanEncoder = min(encoders[0, 0:DWEL_MetaInfo.LastShotInd])
  UBScanEncoder = max(encoders[0, 0:DWEL_MetaInfo.LastShotInd])
  
  DataArray = intarr(DWEL_MetaInfo.NoShotsPerScan, DWEL_MetaInfo.NoSamplesPerShot)
  AncillaryArray = lonarr(DWEL_MetaInfo.NoShotsPerScan,9)
  Trigger = 0B
  SunSensor = 0B
  ScanEncoder = 0LL
  RotaryEncoder = 0LL
  LaserPower = 0E
  Pos = 0LL
  mask=0b
  ShotZen=0.0
  ShotAzim=0.0
  
  shotind = 0L
  PixelLoc = indgen(DWEL_MetaInfo.NoShotsPerScan)
  PixelScanEncoder = LBScanEncoder + PixelLoc*(UBScanEncoder-LBScanEncoder)/(DWEL_MetaInfo.NoShotsPerScan-1)
  NumBlank = 0
  BlankVec = bytarr(DWEL_MetaInfo.NoShotsPerScan)
  ZeroWaveform = intarr(DWEL_MetaInfo.NoSamplesPerShot)
  for i = 0, DWEL_MetaInfo.TotalNoScans-1, 1 do begin
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; if the number of shots in a scan line is less than the given NoShotsPerScan (number of column in a row), the following generates
    ; a vector BlankPixelLoc which gives where we need some dummy pixels to fill the extra space in this row. 
    ScanEncoderVec =  encoders[0, DWEL_MetaInfo.ShotStart[i]:DWEL_MetaInfo.ShotEnd[i]]
    ; check how many blank pixels there will be. Insert the blank pixels to the largest gaps between neighboring scan encoder values. 
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
;      NumPotentialBlankVec = round(DiffScanEncoderVec[DescendingInd]/(UBScanEncoder-LBScanEncoder)*(DWEL_MetaInfo.NoShotsPerScan-1))-1
;      NumPotentialBlankVec[where(NumPotentialBlankVec lt 0)]=0
;      CumPotentialBlankVec = total(NumPotentialBlankVec[0:(NumBlank lt size(NumPotentialBlankVec, /n_elements) ? NumBlank : size(NumPotentialBlankVec, /n_elements))-1], /cumulative)
;      NumGap = where(CumPotentialBlankVec ge NumBlank) + 1
;      NumGap = NumGap[0]
;      if NumGap gt 1 then begin
;        NumActualBlankVec = [NumPotentialBlankVec[0:NumGap-2], NumBlank-CumPotentialBlankVec[NumGap-2]]
;      endif else begin
;        NumActualBlankVec = NumBlank
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
      if total((BlankPixelLoc eq j)) gt 0 then begin
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
      ScanEncoder = encoders[0, shotind]
      ShotZen = double(262144 - ScanEncoder) / double(524288) * 2 * 180.0
      RotaryEncoder = encoders[1, shotind]
;      ;;;;;;;;;;;;;;;specific correction for site 305 NE;;;;;;
;      RotaryEncoder = encoders[1, shotind]+40.5/360.0*double(524288)
;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ShotAzim = double(RotaryEncoder) / double(524288) * 2 * 180.0
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
  
  ;h5t_close, wave_datatype
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
  
  return, {samples:data_dims[1], lines:DWEL_MetaInfo.TotalNoScans, $
    databands:data_dims[2], ancillarybands:ancillary_dims[2], offset:0, $
    filetype:'ENVI Data Cube', datatype:data_dims[3], $
    ancillarydatatype:ancillary_dims[3], interleave:1, sensortype:'DWEL', $
    byteorder:0, wavelengthunit:'metres', range:120.0}
  
end
 
pro DWEL2Cube_EncoderCorr_cmd, DWEL_H5File, oldancillaryfile_name, DWEL_Casing_Mask, BenchLineInd, DataCube_File, $
  Wavelength, Wavelength_Label, DWEL_Height, beam_div, srate, nadirelevshift 
;;
;; Because the names of the waveform datasets in HDF5 files were
;;incorrectly labeled as of March 2014, including all data from CA
;;Sierra June 2013 and Brisbane August 2013, two numbers of wavelength
;;are required here. 
;; Wavelength: a number used to create a dataset name and get waveform
;; data from HDF5 file.
;; Wavelength_Label: a number to be put in the header file. This is
;;the CORRECT wavelength number. 
  
  ;resolve_all
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init  

  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  DWEL_ND_Filter=0

;  beam_div = 2.5 ; mrad
;  srate = 2.0 ; smp/ns,
  
  ;; create a struct variable to store the instrument information, such a the encoder values of case.
  ;DWEL_InstInfo = {CaseElEncoder:[116508,145636], NadirElEncoder:128000}
  
  ; Open Ancillary file
;  n_base=strlen(OldDataCube_File)
;  n_dot=strpos(OldDataCube_File,'.',/reverse_search)
;  if((n_dot le 0) or (n_base-n_dot ne 4)) then begin
;    anc_name=strtrim(OldDataCube_File,2)+'_ancillary'
;  endif else begin
;    ancillaryfile_name=strmid(OldDataCube_File,0,n_dot)+'_ancillary.img'
;  endelse

  if(not file_test(oldancillaryfile_name)) then begin
    message_text=[ $
    'Default ancillary file is not present',$
    'default name: '+strtrim(oldancillaryfile_name,2),$
    'Find an ancillary File ? (Yes) or Re-start (No)?' $
    ]
    result=dialog_message(message_text,/question,$
    title='Default ancillary file NOT present')
    if(result eq 'No') then return
  get_anc:
    outfile = dialog_pickfile(title='Select ancillary DWEL_file', $
            file=oldancillaryfile_name,path=f_path, /must_exist)
    ;check for error or cancel button hit
    if (outfile eq '') then begin
      result=dialog_message('Try again ? (No/Yes) ',/question,$
      title='No file selected or operation cancelled !')
      if(result eq 'No') then begin
        return
      endif else goto, get_anc
    endif
    anc_name=outfile
  endif

  ; get the correction of scan encoder values from old ancillary file and a casing mask
   Correction= Get_ScanEncoderCorrection(oldancillaryfile_name, DWEL_Casing_Mask, BenchLineInd)
   ScanEncoderCorrection = Correction.ScanEncoderCorrection
   ;nadirelevshift = Correction.NadirScanEncoder+double(7 / 360.0 * 524288) ; specifically corrected for site 305 NE
  
  ; start import HDF5 file to data cube with correction of scan encoder values
  DWEL_MetaInfo = CheckDWEL_EncoderCorr(DWEL_H5File, Wavelength, nadirelevshift, ScanEncoderCorrection)
  HeaderInfo = DataCube_EncoderCorr(DWEL_MetaInfo, DataCube_File, Wavelength, nadirelevshift, ScanEncoderCorrection)
;  HeaderInfo = {samples:1452, lines:490, $
;    databands:1594, ancillarybands:9, offset:0, $
;    filetype:'ENVI Data Cube', datatype:12, $
;    ancillarydatatype:14, interleave:1, sensortype:'DWEL', $
;    byteorder:0, wavelengthunit:'metres', range:120.0}
  
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
  ;List the file details and see that all is OK
  ;dwel_height = 1.75
  
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

  DWEL_Adaptation=['Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift in HDF raw data']
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