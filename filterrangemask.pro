pro FilterRangeMask, oldRangeMask, ancfile, newRangeMask
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window

  ;open the old range mask file
  envi_open_file, oldRangeMask, r_fid=infile_fid,/no_realize
  
  if (infile_fid eq -1) then begin
      print,strtrim('Error opening input mask file',2)
      print,'Input File: '+strtrim(oldRangeMask,2)
      return
  endif
  
  envi_file_query, infile_fid, ns=ns, nl=nl, nb=nb, $
    xstart=xstart, ystart=ystart, data_type=type, $
    interleave=ftype, fname=fname, dims=dims
    
  if (nb ne 1) then begin
    print, strtrim('Input image contains more than one band, not a mask image', 2)
    print,'Input File: '+strtrim(oldRangeMask,2)
    return
  endif
  
  mask = envi_get_data(dims=dims, fid=infile_fid, pos=0)
;;  maskopen = morph_open(mask, [1,1,1,1,1,1,1,1,1,1,1])
  maskopen = morph_open(mask, [1,1,1])
  maskopenclose = morph_close(maskopen, [1,1,1])
  
  envi_open_file, ancfile, r_fid=anc_fid, /no_realize
  basicmask = byte(envi_get_data(dims=dims, fid=anc_fid, pos=6))
  
  newmask = byte(maskopenclose) * basicmask
  
  openw, out_fid, newRangeMask, /get_lun
  writeu, out_fid, (newmask)
  
  ; set up header for output filtered image file
  envi_setup_head, fname=newRangeMask, $
    ns=ns, nl=nl, nb=1, $
    data_type=size(1b, /type), $
    interleave=0, $
    bname = 'Range mask after opening and closing', $
    /write
  
  close, out_fid, infile_fid, anc_fid
   
end