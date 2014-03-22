% Align scan images on geo server

datacubefile1064 = '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_C/Aug1_Kara5_C_1064_Cube_ancillary.img';
datacubefile1548 = '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_C/Aug1_Kara5_C_1548_Cube_ancillary.img';
ns = 2703;
nl = 1670;
gapopen = -512^2*2;
startline = 2;
AlignedMaskFile = '/projectnb/echidna/lidar/DWEL_Processing/Brisbane2013Aug/Brisbane2013_Kara05/Aug1_Kara05_C/Aug1_Kara5_C_AlignMask.tif';

AlignedWfMax_dual = AlignScanMask(datacubefile1064, datacubefile1548, ns, nl, gapopen, startline, AlignedMaskFile);
