;; dwel2cube_encodercorr_cmd.pro
;; Convert DWEL HDF5 data format to ENVI data cube (.img files)
;; Running in command-line mode
;;
;; Zhan Li, zhanli86@bu.edu
;; 
;; Introduced in 2013
;; 
;; Revision history:
;; 20140322, unwarp the encoder drift and correct zenith with a casing
;;mask file at the same time. Don't have to provide the nadir
;;shift of elevation encoder anymore to correct the zenith. The
;;program will find the zenith encoder correction of each scan line
;;from the casing mask file and apply the changes. 

function CheckDWEL_EncoderCorr, DWEL_H5File, Wavelength, Correction
  compile_opt idl2
  
  fileid=h5f_open(DWEL_H5File)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)
  
  ;; use the difference between every two scan encoder values to
  ;; determine the actual start and ending shots and to remove the
  ;;dummy shots at the beginning and the end. 
  interval_diff = encoders[0, 0:dim_encoders[1]-2] - encoders[0, 1:dim_encoders[1]-1] ; the difference between two consecutive shots, the early one - the later one
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
  
  ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1
  ShotStartVec = [shotstart, NegInd+1]
  ShotEndVec = [NegInd, shotend]
  ShotNumVec = ShotEndVec - ShotStartVec + 1
  for i=0,TotalNoScans-1,1 do BEGIN
     encoders[0, ShotStartVec[i]:ShotEndVec[i]] = $
        encoders[0, ShotStartVec[i]:ShotEndVec[i]] + Correction.NadirCorrection[i]
  endfor
  tmpind = where(encoders[0, *] lt 0, tmpcount)
  if (tmpcount gt 0) then begin
    encoders[0, tmpind] = encoders[0, tmpind] + 524288
  endif
  tmpind = where(encoders[0, *] gt 524288, tmpcount)
  if (tmpcount gt 0) then begin
    encoders[0, tmpind] = encoders[0, tmpind] - 524288
  endif
  
  ; recalculate the number of shots per each scan line
  interval_diff = encoders[0, 0:shotend-1] - encoders[0, 1:shotend] ; the difference between two consecutive shots, the early one - the later one
  ; if the difference is negative (compare with -2^18 to avoid the positive difference due to wiggles in the encoder values), here is a start of new scan
  NegInd = where(interval_diff lt -262144, NegCount, /L64)
  TotalNoScans = NegCount + 1
  ShotStartVec = [shotstart, NegInd+1]
  ShotEndVec = [NegInd, shotend]
  ShotNumVec = ShotEndVec - ShotStartVec + 1
  
  NoScanPerRotation = fix(TotalNoScans * 524288.0 / abs(encoders[1,shotstart]-encoders[1,shotend]))
  
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
             FirstShotInd:shotstart, LastShotInd:shotend, $
             ShotStart:ShotStartVec, ShotEnd:ShotEndVec, ShotNum:ShotNumVec, $
             NoScanPerRotation:NoScanPerRotation, $
             CorrectedScanEncoder:encoders[0,*]}
end

function DataCube_EncoderCorr, DWEL_MetaInfo, DataCube_File, Wavelength
  compile_opt idl2
  
  AncillaryFile = strmid(DataCube_File,0,strpos(DataCube_File, '.', /reverse_search))+'_ancillary.img'
  
  fileid=h5f_open(DWEL_MetaInfo.DWELFileName)
  encoderset = h5d_open(fileid,'Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  ;; virtually reverse the rotation direction of DWEL by changing the rotatary encoder values
  ;encoders[1, *] =  524288 - encoders[1, *]
  
  ;; get the scan encoder with nadir position corrected.
  encoders[0, *] = DWEL_MetaInfo.CorrectedScanEncoder
  
  ;; open the dataset of waveform and prepare to read the waveform data.
  Waveset_Name = '/'+strtrim(string(Wavelength), 2)+' Waveform Data'
  waveset = h5d_open(fileid,Waveset_Name)
  wave_space = h5d_get_space(waveset)
  memspace = h5s_create_simple([DWEL_MetaInfo.NoSamplesPerShot, 1])
  
  openw, DataCubeFID, DataCube_File, /get_lun
  openw, AncillaryFID, AncillaryFile, /get_lun

  ;;=========================================================
  ;; temporary fix of Oz BFP scans
  ;; Oz BFP East
  DWEL_MetaInfo.NoShotsPerScan = 2709
  ;; ;; Oz BFP Aug2_C2
  ;; DWEL_MetaInfo.NoShotsPerScan = 5362
  ;;=========================================================
    
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
  PixelLoc = indgen(DWEL_MetaInfo.NoShotsPerScan)
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
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; simply insert blank pixels at the end of each scan line
      BlankPixelLoc = indgen(NumBlank) + DWEL_MetaInfo.NoShotsPerScan-NumBlank
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    endif else begin ; if no blank pixel is needed
      BlankPixelLoc=-1
   endelse
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;;=========================================================
      ;; temporary fix of Oz BFP scans
      ;; Oz BFP East
      IF i EQ 124 THEN BEGIN
         shotind = shotind + DWEL_MetaInfo.ShotNum[i]
         CONTINUE
      ENDIF
      ;; ;; Oz BFP Aug2_C2
      ;; IF i EQ 569 THEN BEGIN
      ;;    shotind = shotind + DWEL_MetaInfo.ShotNum[i]
      ;;    CONTINUE 
      ;; ENDIF
      ;;=========================================================
    
    for j = 0L, DWEL_MetaInfo.NoShotsPerScan-1, 1  do begin
      if (total((BlankPixelLoc eq j)) gt 0) or (shotind ge DWEL_MetaInfo.LastShotInd+1) gt 0 then begin
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

  ;;=========================================================
  ;; temporary fix of Oz BFP scans
  ;; Oz BFP East, Oz BFP Aug2_C2
  DWEL_MetaInfo.TotalNoScans = DWEL_MetaInfo.TotalNoScans - 1
  ;;=========================================================
  
  return, {samples:data_dims[1], lines:DWEL_MetaInfo.TotalNoScans, $
    databands:data_dims[2], ancillarybands:ancillary_dims[2], offset:0, $
    filetype:'ENVI Data Cube', datatype:data_dims[3], $
    ancillarydatatype:ancillary_dims[3], interleave:1, sensortype:'DWEL', $
    byteorder:0, wavelengthunit:'metres', range:120.0}
  
end
 
pro DWEL2Cube_EncoderCorr_cmd, DWEL_H5File, oldancillaryfile_name, DWEL_Casing_Mask, DataCube_File, $
  Wavelength, Wavelength_Label, DWEL_Height, beam_div, srate 
;;
;; Because the names of the waveform datasets in HDF5 files were
;;incorrectly labeled as of March 2014, including all data from CA
;;Sierra June 2013 and Brisbane August 2013, two numbers of wavelength
;;are required here. 
;; Wavelength: a number used to create a dataset name and get waveform
;; data from HDF5 file.
;; Wavelength_Label: a number to be put in the header file. This is
;;the CORRECT wavelength number. 
;; DWEL_Casing_Mask is simply from thresholding the range image, 
;; NOT filtered by morphology windows. 
  
  ;resolve_all
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /NO_STATUS_WINDOW  

  ND_Nam=['ND0','ND015','ND030','ND1','ND2','ND3','ND045','ND115','ND130','ND145','Unknown']
  DWEL_ND_Filter=0

  if(not file_test(oldancillaryfile_name)) then BEGIN
     message_text=[ $
                     'Input ancillary file is not present\n',$
                     'Input ancillary file  name: '+strtrim(oldancillaryfile_name,2)]
     print, message_text
     return    
  endif

  ;; DWEL_Casing_Mask is simply from thresholding the range image. We
  ;; first filter the mask with morph filters to remove noises and
  ;;fill holes in the mask and output the filtered mask to a new
  ;;file. 
  ;; Let's create the file name of the new mask. 
  last = strpos(DWEL_Casing_Mask, '.img', /reverse_search)
  newRangeMask = strmid(DWEL_Casing_Mask, 0, last) + '_morphopenclose.img'
  FilterRangeMask, DWEL_Casing_Mask, oldancillaryfile_name, newRangeMask

  ; get the correction of scan encoder values from old ancillary file and a casing mask
   Correction= Get_ScanEncoderCorrection(oldancillaryfile_name, newRangeMask)
   ;;ScanEncoderCorrection = Correction.ScanEncoderCorrection
  
  ; start import HDF5 file to data cube with correction of scan encoder values
  DWEL_MetaInfo = CheckDWEL_EncoderCorr(DWEL_H5File, Wavelength, Correction)
  HeaderInfo = DataCube_EncoderCorr(DWEL_MetaInfo, DataCube_File, Wavelength)
;  HeaderInfo = {samples:1452, lines:490, $
;    databands:1594, ancillarybands:9, offset:0, $
;    filetype:'ENVI Data Cube', datatype:12, $
;    ancillarydatatype:14, interleave:1, sensortype:'DWEL', $
;    byteorder:0, wavelengthunit:'metres', range:120.0}

  ;;=========================================================
  ;; temporary fix of Oz BFP scans
  ;; Oz BFP East
  DWEL_MetaInfo.NoShotsPerScan = 2709
  DWEL_MetaInfo.TotalNoScans = DWEL_MetaInfo.TotalNoScans - 1
  ;; ;; Oz BFP Aug2_C2
  ;; DWEL_MetaInfo.NoShotsPerScan = 5362
  ;; DWEL_MetaInfo.TotalNoScans = DWEL_MetaInfo.TotalNoScans - 1
  ;;=========================================================
  
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

  DWEL_Adaptation=['Band "Waveform Mean" is actually "Waveform Max"', 'Band "Scan Encoder" is value corrected for nadir shift in HDF raw data']
  DWEL_Adaptation=[DWEL_Adaptation, 'Wavelength='+strtrim(Wavelength_Label, 2)]
  
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