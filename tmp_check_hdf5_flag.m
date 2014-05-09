% a quick check on the flag dataset in DWEL hdf5 file created from
% the casing mask. 
% 
% zhanli86@bu.edu, Zhan Li
% April 29, 2014

clear;

inputfile = '/projectnb/echidna/lidar/Data_2013CASierra/DWEL/June14_01_305_NE/June14_01.hdf5';
flag = h5read(inputfile, '/Flag');
encoders = h5read(inputfile, '/Interpolated angles (Alt, Azm)');
diff = encoders(1, 1:end-1) - encoders(1, 2:end);
tmpind = find(diff > 262144);

