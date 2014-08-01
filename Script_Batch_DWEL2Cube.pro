; a script to do a batch job of importing DWEL HDF5 files to ENVI cube

; DWEL2Cube_Cmd2, DWEL_H5File, DataCube_File, Wavelength, DWEL_Height, DWEL_ND_Filter, nadirelevshift
pro Script_Batch_DWEL2Cube

  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_E/Aug1_Kara5_E.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_E/Aug1_Kara5_E_1548_Cube.img', $
    1064, 1.23, 2.5, 2.0, 0
    
  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_E/Aug1_Kara5_E.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_E/Aug1_Kara5_E_1064_Cube.img', $
    1548, 1.23, 2.5, 2.0, 0
    
  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_N/Aug1_Kara5_N1.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_N/Aug1_Kara5_N_1548_Cube.img', $
    1064, 1.21, 2.5, 2.0, 0
    
  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_N/Aug1_Kara5_N1.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_N/Aug1_Kara5_N_1064_Cube.img', $
    1548, 1.21, 2.5, 2.0, 0
    
  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_W/Aug1_Kara5_W1.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_W/Aug1_Kara5_W_1548_Cube.img', $
    1064, 1.14, 2.5, 2.0, 0
    
  DWEL2Cube_cmd, '/projectnb/echidna/lidar/Data_2013OzBrisbane/DWEL/Aug1_Kara5/Aug1_Kara5_W/Aug1_Kara5_W1.hdf5', $
    '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_W/Aug1_Kara5_W_1064_Cube.img', $
    1548, 1.14, 2.5, 2.0, 0
end