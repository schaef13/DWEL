; calculate the waveforms of the difference index and create an ENVI image cube.
pro DWEL_Diff_Index_Cube, DataCubeFile_1064, AncFile_1064, DataCubeFile_1548, AncFile_1548, DiffIndexCubeFile, BandWindow, Scale
  ; open the ENVI image
  envi_open_file, DataCubeFile_1064, r_fid=infile_fid_1064, /no_realize
  ; ask for information about this image
  envi_file_query, infile_fid_1064, ns=ns_1064, nl=nl_1064, nb=nb_1064, wl=wl_1064, $
    xstart=xs_1064, ystart=ys_1064, data_type=type_1064, $
    interleave=ftype_1064, fname=fname_1064, dims=dims_1064, bname=bname_1064
    
  ; open the ancillary ENVI image where we can get the mask image
  envi_open_file, AncFile_1064, r_fid=ancfile_fid_1064, /no_realize
  ; ask for information about this ancillary image
  envi_file_query, ancfile_fid_1064, ns=anc_ns_1064, nl=anc_nl_1064, nb=anc_nb_1064, $
    xstart=anc_xs_1064, ystart=anc_ys_1064, data_type=anc_type_1064, $
    interleave=anc_ftype_1064, fname=anc_fname_1064, dims=anc_dims_1064, bname=anc_bname_1064
    
  ; open the ENVI image
  envi_open_file, DataCubeFile_1548, r_fid=infile_fid_1548, /no_realize
  ; ask for information about this image
  envi_file_query, infile_fid_1548, ns=ns_1548, nl=nl_1548, nb=nb_1548, wl=wl_1548, $
    xstart=xs_1548, ystart=ys_1548, data_type=type_1548, $
    interleave=ftype_1548, fname=fname_1548, dims=dims_1548, bname=bname_1548
    
  ; open the ancillary ENVI image where we can get the mask image
  envi_open_file, AncFile_1548, r_fid=ancfile_fid_1548, /no_realize
  ; ask for information about this ancillary image
  envi_file_query, ancfile_fid_1548, ns=anc_ns_1548, nl=anc_nl_1548, nb=anc_nb_1548, $
    xstart=anc_xs_1548, ystart=anc_ys_1548, data_type=anc_type_1548, $
    interleave=anc_ftype_1548, fname=anc_fname_1548, dims=anc_dims_1548, bname=anc_bname_1548
    
  if (nb_1064 ne nb_1548) then begin
    print, 'Data error: the number of bands of the two data cube is not the same!'
    return
  endif
  
  tmplogic = strcmp(anc_bname_1064, 'Mask', /fold_case)
  tmppos = where(tmplogic)
  ; read the mask
  mask_1064 = envi_get_data(fid=ancfile_fid_1064, dims=anc_dims_1064, pos=tmppos[0])
  mask_1064 = byte(mask_1064)
  
  tmplogic = strcmp(anc_bname_1548, 'Mask', /fold_case)
  tmppos = where(tmplogic)
  ; read the mask
  mask_1548 = envi_get_data(fid=ancfile_fid_1548, dims=anc_dims_1548, pos=tmppos[0])
  mask_1548 = byte(mask_1548)
  
  mask = mask_1064
  mask[where(mask_1548 eq 0)] = 0b
  imagepos = where(mask eq 1)
  
  ; open the output file for writing data
  openw, diffindex_ofid, DiffIndexCubeFile, /get_lun
  if (diffindex_ofid lt 0) then begin
    print, 'failed to open the output file'
    return
  endif
  
  ns_out = ns_1064
  nl_out = nl_1064
  nb_out = nb_1064
  diffindex_array = intarr(ns_out, nb_out)
  
  tile_id_1064 = envi_init_tile(infile_fid_1064, indgen(nb_1064), num_tiles=ntiles_1064)
  tile_id_1548 = envi_init_tile(infile_fid_1548, indgen(nb_1548), num_tiles=ntiles_1548)
  if (ntiles_1064 ne nl_1064) or (ntiles_1548 ne nl_1548) then begin
    print, 'RAM for ENVI is not big enough to create proper tiles for data processing'
    return
  endif
  if (ntiles_1064 ne ntiles_1548) then begin
    print, 'processing error: tiles of the two wavelengths are different.'
    return
  endif
  for nt=0, ntiles_1064-1 do begin
    tmp1064 = envi_get_tile(tile_id_1064, nt)
    tmp1548 = envi_get_tile(tile_id_1548, nt)
    tmp1064 = smooth(tmp1064, [1,BandWindow])
    tmp1548 = smooth(tmp1548, [1,BandWindow])
    tmppos = where(tmp1064+tmp1548 ne 0, COMPLEMENT=tmpzeropos, NCOMPLEMENT=ntmpzero)
    diffindex_array[tmppos] = round(float(tmp1064[tmppos]-tmp1548[tmppos])/float(tmp1064[tmppos]+tmp1548[tmppos])*Scale)
    if (ntmpzero gt 0) then begin
      diffindex_array[tmpzeropos] = -32768
    endif
    writeu, diffindex_ofid, diffindex_array
  endfor
  
  envi_tile_done, tile_id_1064
  envi_tile_done, tile_id_1548
  
  free_lun, diffindex_ofid, /force
  
  tmpind = strpos(DiffIndexCubeFile, path_sep(), /reverse_search)
  out_name = strtrim(strmid(DiffIndexCubeFile, tmpind+1, strlen(DiffIndexCubeFile)-1),2)
  out_type = size(diffindex_array, /type)
  ft_out = 1 ; BIL
  wl_out = wl_1064
  bname_out = bname_1064
  descrip = '(1064-1548)/(1064+1548)'
  ; setup header info
  envi_setup_head,fname=out_name,ns=ns_out,nl=nl_out,nb=nb_out,$
    data_type=out_type, interleave=ft_out, $
    wl=wl_out, $
    bnames=bname_out,descrip=descrip, $
    zplot_titles=['Range (m)','Diff. Index'], $
    /write,/open, r_fid=diffindex_ofid
end