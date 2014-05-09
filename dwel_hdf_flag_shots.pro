;; dwel_hdf_flag_shots.pro
;; Label whether a shot is inside casing area or not in the HDF5 file. 
;; To do so, create a dataset called "Flag" in the HDF5 file of the
;; scanning data. 

PRO dwel_hdf_flag_shots, DWEL_H5File, ancillaryfile_name, Filtered_Casing_Mask_File, Flag_H5File

  compile_opt idl2
  envi, /restore_base_save_files
  envi_batch_init, /NO_STATUS_WINDOW

  scanenc_ind = 0
  rotateenc_ind = 1

  ;; ;; first find out whether the hdf5 file is backed up. 
  ;; backupflag = file_test(DWEL_H5File+'.backup')
  ;; IF backupflag EQ 0 THEN BEGIN
  ;;    ;; if the hdf5 file is not backed up, then create a backup file
  ;;    file_copy, DWEL_H5File, DWEL_H5File+'.backup'
  ;; ENDIF 

  DWEL_H5D_Names = ['1064 Waveform Data', $
                          '1548 Waveform Data', $
                          'Interpolated angles (Alt, Azm)', $
                          'Time']

  ;; get the information about the hdf5 file
  h5info = h5_parse(DWEL_H5File)
  h5info_tags = tag_names(h5info)
  h5info_n = n_tags(h5info)
  h5d_id = replicate(-1, size(DWEL_H5D_Names, /n_elements))
  FOR i=6, h5info_n-1, 1 DO BEGIN
     tmp = strcmp(h5info.(i)._NAME, DWEL_H5D_Names, /fold_case)
     tmpind = where(tmp, tmpcount)
     IF tmpcount EQ 1 THEN BEGIN
        h5d_id[tmpind] = i
     ENDIF 
  ENDFOR 

  ;; open the HDF5 file.
  fileid = h5f_open(DWEL_H5File, /write)
  ;; read encoder dataset.
  encoderset = h5d_open(fileid, '/Interpolated angles (Alt, Azm)')
  encoders = h5d_read(encoderset)
  dim_encoders = size(encoders, /dimensions)

  ;; use the difference between every two scan encoder values to
  ;; determine the actual start and ending shots and to remove the
  ;;dummy shots at the beginning and the end. 
  interval_diff = encoders[scanenc_ind, 0:dim_encoders[1]-2] - encoders[scanenc_ind, 1:dim_encoders[1]-1] ; the difference between two consecutive shots, the early one - the later one
  tmpind = where(interval_diff ne 0, tmpcount, ncomplement=count, complement=dummyind)
  IF (tmpcount gt 0) then begin
    shotstart = tmpind[0]
    shotend = tmpind[size(tmpind, /n_elements)-1]
  ENDIF  else begin
    print, 'No valid scan encoder value! Processing is terminated!'
    h5d_close, encoderset
    h5f_close, fileid
    h5_close    
    return
  ENDELSE
  
  envi_open_file, ancillaryfile_name, r_fid=ancillaryfile_fid, $
                  /no_realize
  ;check if operation cancelled
  if (ancillaryfile_fid eq -1) then begin
      print,strtrim('Error or No opening ancillary file',2)
      print,'Ancillary File: '+strtrim(ancillaryfile_name,2)
      return
  endif
  
  envi_file_query, ancillaryfile_fid, nb=nb_anc, nl=nl_anc, ns=ns_anc, data_type=type_anc, interleave=anc_ftype, $
    bnames=anc_bnames

  ;; open the mask file
  envi_open_file, Filtered_Casing_Mask_File, r_fid=casingmask_fid, /no_realize
  if (casingmask_fid eq -1) then begin
    print, strtrim('Error opening casing mask file', 2)
    print, 'Casing mask file: '+strtrim(DWEL_Casing_Mask, 2)
    return
  endif
  
  envi_file_query, casingmask_fid, nb=nb_casing, nl=nl_casing, ns=ns_casing, data_type=type_casing

  MaskBandInd = where(strcmp(strtrim(anc_bnames,2),'Mask'))
  ; read mask in the ancillary file
  mask = byte(envi_get_data(fid=ancillaryfile_fid, dims=[-1L,0,ns_anc-1,0,nl_anc-1], pos=MaskBandInd))
  ; read image of casing mask
  CasingMask = envi_get_data(fid=casingmask_fid, dims=[-1L,0,ns_casing-1,0,nl_casing-1], pos=0)
  ; the combined mask
  commask = mask*CasingMask

  flag = make_array(dim_encoders[1], /integer)
  ;; before shotstart and after shotend, flag of -1 means invalid
  ;; shots that will be ignored in the scanning image.
  flag[0:shotstart] = -1
  flag[shotend:dim_encoders[1]-1] = -1
  shotind = shotstart
  FOR i = 0, nl_casing-1, 1 DO BEGIN
     FOR j = 0, ns_casing-1, 1 DO BEGIN
        ;; if this is a casing shot
        IF commask[j, i] THEN BEGIN
           flag[shotind] = 1 ;; flag of 1: casing shot
           shotind = shotind + 1
        ENDIF ELSE BEGIN
           IF mask[j, i] THEN BEGIN
              flag[shotind] = 2 ;; flag of 2: valid shot outside casing area
              shotind = shotind + 1
           ENDIF 
        ENDELSE         
     ENDFOR 
  ENDFOR 

  ;; write flag to a new hdf5 file, name given by the last input
  ;; argument.
  ;; first find out whether the flag hdf5 file exists
  fileexist = file_test(Flag_H5File)
  IF fileexist EQ 1 THEN BEGIN
     ;; if the flag hdf5 file exist, delete the old one
     file_delete, Flag_H5File, /quiet, /verbose
  ENDIF
  flag_fid = h5f_create(Flag_H5File)
  flag_datatype_id = h5t_idl_create(flag)
  flag_dataspace_id = h5s_create_simple(size(flag, /dimensions))
  flag_dataset_id = h5d_create(flag_fid, 'Flag', flag_datatype_id, flag_dataspace_id)
  h5d_write, flag_dataset_id, flag

  att_str = 'invalid=-1;casing=1;valid outside casing=2'  
  n_attrs = h5a_get_num_attrs(flag_dataset_id)
  att_id = -1
  FOR i = 0, n_attrs-1, 1 DO BEGIN 
     att_id = h5a_open_idx(flag_dataset_id, i)
     IF strcmp(h5a_get_name(att_id), 'Flag Meaning', /fold_case) THEN BEGIN
        continue
     ENDIF
     att_id = -1
  ENDFOR 
  IF att_id LT 0 THEN BEGIN
     ;; create a reference attribute attached to the dataset
     flagatt_datatype_id = h5t_idl_create(att_str)
     flagatt_dataspace_id = h5s_create_scalar()
     att_id = h5a_create(flag_dataset_id, 'Flag Meaning', flagatt_datatype_id, flagatt_dataspace_id)
     h5a_write, att_id, att_str
     h5a_close, att_id
     h5s_close, flagatt_dataspace_id
     h5t_close, flagatt_datatype_id
  ENDIF ELSE BEGIN
     h5a_write, att_id, att_str
     h5a_close, att_id
  ENDELSE

  h5d_close, encoderset
  h5f_close, fileid
  h5s_close, flag_dataspace_id
  h5t_close, flag_datatype_id
  h5d_close, flag_dataset_id
  h5f_close, flag_fid

  ;; ;; write flag to a dataset in the DWEL hdf5 file
  ;; ;; see if the dataset 'Flag' exists
  ;; FOR i=6, h5info_n-1, 1 DO BEGIN
  ;;    flag_ind = strcmp(h5info.(i)._NAME, 'Flag', /fold_case)
  ;;    IF flag_ind THEN BEGIN
  ;;       flag_ind = i
  ;;       break
  ;;    ENDIF 
  ;; ENDFOR 
  
  ;; ;; if no dataset of 'Flag' exists, create a new one
  ;; IF flag_ind EQ 0 THEN BEGIN
  ;;    ;; create a HDF5 datatype
  ;;    datatype_id = h5t_idl_create(flag)
  ;;    ;; create a dataspace
  ;;    dataspace_id = h5s_create_simple(size(flag, /dimensions), max_dimensions = -1)
  ;;    ;; create a HDF5 dataset
  ;;    dataset_id = h5d_create(fileid, 'Flag', datatype_id, dataspace_id, chunk_dimensions=ns_casing)
  ;;    ;; extend the size of the dataset to fit the data
  ;;    h5d_extend, dataset_id,size(flag,/dimensions)
  ;;    ;; write the data to the dataset
  ;;    h5d_write, dataset_id, flag
  ;;    ;; close some id
  ;;    h5s_close, dataspace_id
  ;;    h5t_close, datatype_id
  ;; ENDIF ELSE BEGIN
  ;;    dataset_id = h5d_open(fileid, '/Flag')
  ;;    ;; write the data to the dataset
  ;;    h5d_write, dataset_id, flag
  ;; ENDELSE 

  ;; att_str = 'invalid=-1;casing=1;valid outside casing=2'  
  ;; n_attrs = h5a_get_num_attrs(dataset_id)
  ;; att_id = -1
  ;; FOR i = 0, n_attrs-1, 1 DO BEGIN 
  ;;    att_id = h5a_open_idx(dataset_id, i)
  ;;    IF strcmp(h5a_get_name(att_id), 'Flag Meaning', /fold_case) THEN BEGIN
  ;;       continue
  ;;    ENDIF
  ;;    att_id = -1
  ;; ENDFOR 
  ;; IF att_id LT 0 THEN BEGIN
  ;;    ;; create a reference attribute attached to the dataset
  ;;    datatype_id = h5t_idl_create(att_str)
  ;;    dataspace_id = h5s_create_simple(1, max_dimensions=-1)
  ;;    att_id = h5a_create(dataset_id, 'Flag Meaning', datatype_id, dataspace_id)
  ;;    h5a_write, att_id, att_str
  ;;    h5a_close, att_id
  ;;    h5s_close, dataspace_id
  ;;    h5t_close, datatype_id
  ;; ENDIF ELSE BEGIN
  ;;    h5a_write, att_id, att_str
  ;;    h5a_close, att_id
  ;; ENDELSE 

  ;; h5d_close, encoderset
  ;; h5d_close, dataset_id
  ;; h5f_close, fileid
  
END

