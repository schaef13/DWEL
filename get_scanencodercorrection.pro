;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; correct scan encoder values (elevation encoder) according to the casing region.
; cassing region of the scan to be corrected is input from a binary image where 1 is casing region. 
; the binary image of casing region has the same dimensions with the scan's ancillary image which provides
; the uncorrected  scan encoder values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; By default, the casing mask is the left edge of the casing. 
; When the keyword parameter RightCasing is set, the casing mask is the right edge of the casing area
function Get_ScanEncoderCorrection, ancillaryfile_name, DWEL_Casing_Mask, BenchLineInd

  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init
  
  envi_open_file, ancillaryfile_name, r_fid=ancillaryfile_fid, $
                  /no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
      print,strtrim('Error or No opening ancillary file',2)
      print,'Ancillary File: '+strtrim(ancillaryfile_name,2)
      return, NULL
  endif
  
  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, data_type=type_anc, interleave=anc_ftype, $
    bnames=anc_bnames
  
  ; open the casing mask file
  envi_open_file, DWEL_Casing_Mask, r_fid=casingmask_fid, /no_realize
  if (casingmask_fid eq -1) then begin
    print, strtrim('Error opening casing mask file', 2)
    print, 'Casing mask file: '+strtrim(DWEL_Casing_Mask, 2)
    return, NULL
  endif
  
  envi_file_query, casingmask_fid, nb=nb_casing, nl=nl_casing, ns=ns_casing, data_type=type_casing
  
  if ( (nl_casing ne nl_anc) or (ns_casing ne ns_anc) or (nb_casing ne 1) ) then begin
    envi_file_mng, id=casingmask_fid, /remove
    print, strtrim('Casing mask file does NOT conform with current DWEL cube!', 2)
    print,'Input File: '+strtrim(DWELCubeFile,2)
    print,'Casing Mask File: '+strtrim(DWEL_Casing_Mask,2)
    return, NULL
  endif
  
  ScanEncoderBandInd = -1
  MaskBandInd = -1
  ScanEncoderBandInd = where(strcmp(strtrim(anc_bnames,2),'Scan Encoder'))
  MaskBandInd = where(strcmp(strtrim(anc_bnames,2),'Mask'))
  ; read scan encoder
  ScanEncoderImg = envi_get_data(fid=ancillaryfile_fid, dims=[-1L,0,ns_anc-1,0,nl_anc-1], pos=ScanEncoderBandInd)
  ; read mask in the ancillary file
  mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=[-1L,0,ns_anc-1,0,nl_anc-1], pos=MaskBandInd))
  ; read image of casing mask
  CasingMask = envi_get_data(fid=casingmask_fid, dims=[-1L,0,ns_casing-1,0,nl_casing-1], pos=0)
  ; the combined mask
  commask = mask*CasingMask
  
  ; an array to store the correction of scan encoder of each scan line
  ScanEncoderCorrection = fltarr(nl_anc)
  ; because we've seen some unexpected pattern of encoder values in the first scan line
  ; we will use the encoder values in the second scan line as a benchamrk to which all other scan lines will be aligned
  ; BenchLineInd = 1 
  CasingInd = where(commask[*,BenchLineInd], NCasingPixel)
  tmpind = where( ScanEncoderImg[CasingInd[0:NCasingPixel-2],BenchLineInd]-ScanEncoderImg[CasingInd[1:NCasingPixel-1],BenchLineInd] gt 262144, tmpcount )
  if (tmpcount ge 1) then begin ; the casing region is split into two parts due to extreme warpping
    ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], BenchLineInd] = $
      ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], BenchLineInd] + 524288
  endif
  ;BenchScanEncoder = mean(ScanEncoderImg[CasingInd, BenchLineInd])
  BenchScanEncoder = (ScanEncoderImg[CasingInd[0], BenchLineInd]+ScanEncoderImg[CasingInd[NCasingPixel-1], BenchLineInd])/2.0
  ; correction of the first scan line
  CasingInd = where(commask[*,0], NCasingPixel)
  if (NCasingPixel eq 0) then begin
    ScanEncoderCorrection[0] = 0L
  endif else begin
    tmpind = where( ScanEncoderImg[CasingInd[0:NCasingPixel-2],0]-ScanEncoderImg[CasingInd[1:NCasingPixel-1],0] gt 262144, tmpcount )
    if (tmpcount ge 1) then begin ; the casing region is split into two parts due to extreme warpping
      ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], 0] = $
        ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], 0] + 524288
    endif
    ;ScanEncoderCorrection[0]=BenchScanEncoder-mean(ScanEncoderImg[CasingInd, 0])
    ScanEncoderCorrection[0]=BenchScanEncoder - (ScanEncoderImg[CasingInd[0], 0]+ScanEncoderImg[CasingInd[NCasingPixel-1], 0])/2.0
  endelse
  ; correction of the remaining scan lines
  for l=1,nl_anc-1,1 do begin
    CasingInd = where(commask[*,l], NCasingPixel)
    if (NCasingPixel eq 0) then begin
      ScanEncoderCorrection[l] = ScanEncoderCorrection[l-1]
      continue
    endif
    if (NCasingPixel ge 2) then begin
      tmpind = where( ScanEncoderImg[CasingInd[0:NCasingPixel-2],l]-ScanEncoderImg[CasingInd[1:NCasingPixel-1],l] gt 262144, tmpcount )
      if (tmpcount ge 1) then begin ; the casing region is split into two parts due to extreme warpping
        ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], l] = $
          ScanEncoderImg[CasingInd[tmpind[0]+1]:CasingInd[NCasingPixel-1], l] + 524288
      endif
    endif
    ScanEncoderCorrection[l]=BenchScanEncoder-mean(ScanEncoderImg[CasingInd, l])
    ;ScanEncoderCorrection[l]=BenchScanEncoder - (ScanEncoderImg[CasingInd[0], l]+ScanEncoderImg[CasingInd[NCasingPixel-1], l])/2.0
  endfor
  
;  ; using benchmark scan encoder from the casing region to calculate the nadir shift
;  CasingExtScanEncoder = 0.0
;  CasingExtScanEncoder = double(CasingExtZenith / 360.0 * 524288)
;  if keyword_set(right) then begin ; the casing mask is the right edge of the casing region
;    CasingScanEncoder = BenchScanEncoder + CasingExtScanEncoder
;  endif else begin ; the casing mask is the left edge of the casing region
;    CasingScanEncoder = BenchScanEncoder - CasingExtScanEncoder
;  endelse
;  NadirScanEncoder = (BenchScanEncoder + CasingScanEncoder)/2.0
  
  ;NadirScanEncoder = BenchScanEncoder
  ;NadirScanEncoder = 0
  
;  ; debug, output the correction of the scan encoders
;  openw, CorrFid, '', /get_lun
;  printf, CorrFid, ScanEncoderCorrection
;  close, CorrFid
  
  return, {ScanEncoderCorrection:ScanEncoderCorrection}
end

