pro myBatch

; dwel2cube_cmd, DWEL_H5File, DataCube_File, Wavelength, Wavelength_Label, 
; DWEL_Height, beam_div, srate, nadirelevshift

dwel2cube_cmd, '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/waveform_2014-04-30-15-24.hdf5', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1064_cube.img', 1064, 1064, 1.0, 2.5, 2.0, 0

dwel_cube2at, '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1064_cube.img', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1064_cube_ancillary.img', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1064_cube_at_project.img', 180, 2.0

dwel2cube_cmd, '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/waveform_2014-04-30-15-24.hdf5', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1556_cube.img', 1548, 1548, 1.0, 2.5, 2.0, 0

; dwel_cube2at, DWEL_Cube_File, DWEL_Anc_File, DWEL_AT_File, Max_Zenith_Angle, output_resolution
dwel_cube2at, '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1556_cube.img', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1556_cube_ancillary.img', $
  '/projectnb/echidna/lidar/Data_DWEL_TestCal/April30LabTest/April30_1556_cube_at_project.img', 180, 2.0  
end