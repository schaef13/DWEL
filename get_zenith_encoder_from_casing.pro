; get_zenith_encoder_from_casing.pro
; Estimate the encoder of the zenith point from the mask of whole
; casing area derived from the range image.
;
; Zhan Li, zhanli86@bu.edu
; 2014, March 10th

FUNCTION get_zenith_encoder_from_casing, anc_file, casing_mask_file
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window
  
  ;; open the ancillary file of the data cube that contains the
  ;; encoder values
  envi_open_file, anc_file, r_fid=anc_fid, /no_realize
  envi_file_query, anc_fid, nb=anc_nb, nl=anc_nl, ns=anc_ns, dims=anc_dims, data_type=anc_dtype
  
  ;; open the mask of casing area
  envi_open_file, casing_mask_file, r_fid=casing_mask_fid, /no_realize
  envi_file_query, casing_mask_fid, nb=mask_nb, nl=mask_nl, ns=mask_ns, dims=mask_dims
  
  ;; read the scan encoder
  scan_encoder = envi_get_data(fid=anc_fid, dims=anc_dims, pos=2)
  ;; read the casing mask
  casing_mask = envi_get_data(fid=casing_mask_fid, dims=mask_dims, pos=0)
  
  casing_zenith_array = make_array(anc_nl, type=anc_dtype) ; an array to store the casing zenith from the casing area in each scan line
  ;; for each line, get the start and ending positions of the casing
  ;; area and get the mid-point as the zenith point
  FOR il=0,anc_nl-1 DO BEGIN
    tmpmask = casing_mask[*, il]
    maskind = where(tmpmask, maskcount)
    casing_zenith_array[il] = (scan_encoder[maskind[0], il] + scan_encoder[maskind[maskcount-1], il])/2
  ENDFOR
  
  return, mean(casing_zenith_array)
  
END

; I don't understand why emacs backup doesn't work on this .pro
; files!!! Write some junks here to test it out again!
