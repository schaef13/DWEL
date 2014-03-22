;; make an image of range from the data cube.
;; the range image might help us to find a uniform casing area
;;
;; Zhan Li, zhanli86@bu.edu
;; Created in 2013
;; Revision, March 2014

pro DataCube2RangeImage, DataCubeFile, Tzero, RangeImageFile
  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /no_status_window

  ; Open DWEL cube file
  envi_open_file, DataCubeFile, r_fid=infile_fid, /no_realize
  
  if (infile_fid eq -1) then begin
      print,strtrim('Error opening input data cube file',2)
      print,'Input File: '+strtrim(DWELCubeFile,2)
      return
  endif
  
  envi_file_query, infile_fid, ns=ns, nl=nl, nb=nb, wl=wl, $
    xstart=xstart, ystart=ystart, data_type=type, $
    interleave=ftype, fname=fname, dims=dims
  
  band_pos = indgen(nb)
    
  tile_id=envi_init_tile(infile_fid,band_pos,num_tiles = ntiles)
  if (ntiles ne nl) then begin
    print, 'Number of tiles are not expected. It should be the number of lines'
    return
  endif
  
  openw, RangeImageFID, RangeImageFile, /get_lun
  RangeArray = fltarr(ns, 2)
  for t=0, ntiles-1 do begin
    data = envi_get_tile(tile_id, t, band_index=band_pos, ys=0, ye=ns-1)
    tmpmax = max(data, dimension=2, maxI)
    tmppos = array_indices(data, maxI)
    RangeArray[*, 0] = (wl[tmppos[1, *]] - Tzero)*0.15 ; for raw data cube with waveforms in time 
    ;RangeArray[*, 0] = wl[tmppos[1, *]] ; for data cube with waveforms in range
    RangeArray[*, 1] = tmpmax
    writeu, RangeImageFID, RangeArray
  endfor
  envi_tile_done, tile_id
  
  ; set up header for output range image file
  envi_setup_head, fname=RangeImageFile, $
    ns=ns, nl=nl, nb=2, $
    data_type=size(RangeArray, /type), $
    interleave=ftype, $
    bname = ['Range', 'Waveform Max'], $
    /write
  
  close, RangeImageFID, infile_fid
end
