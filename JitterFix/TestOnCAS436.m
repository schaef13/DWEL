% test on computer CAS436

clear;

% ns = 1571;
% nl = 3142;
% datacubefilename = 'E:\data\Processed\Aug1_Kara5_C\CorrLabelATAligned\Aug1_Kara5_C_1548_Cube_nu_basefix_satfix_pfilter_at_project_extrainfo.img';
% WfMax_1548 = multibandread(datacubefilename, [nl, ns, 9], 'int16', 0, 'bsq', 'ieee-le', {'Band', 'Direct', 5});
% datacubefilename = 'E:\data\Processed\Aug1_Kara5_C\CorrLabelATAligned\Aug1_Kara5_C_1064_Cube_nu_basefix_satfix_pfilter_at_project_extrainfo.img';
% WfMax_1064 = multibandread(datacubefilename, [nl, ns, 9], 'int16', 0, 'bsq', 'ieee-le', {'Band', 'Direct', 5});
% ns = 2699;
% nl = 1669;
% datacubefilename = 'C:\WorkSpace\Data\Oz_July_2013\Aug1_Kara5_C\SatfixAppReflCorrLabel\Aug1_Kara5_C_1548_Cube_ancillary.img';
% WfMax_1548 = multibandread(datacubefilename, [nl, ns, 9], 'int64', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});
% datacubefilename = 'C:\WorkSpace\Data\Oz_July_2013\Aug1_Kara5_C\SatfixAppReflCorrLabel\Aug1_Kara5_C_1064_Cube_ancillary.img';
% WfMax_1064 = multibandread(datacubefilename, [nl, ns, 9], 'int64', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});
% 
% % gapopen = -512;
% % AlignedWfMax_1064 = NWAlignScanImage(WfMax_1064, gapopen);
% % AlignedWfMax_1548 = NWAlignScanImage(WfMax_1548, gapopen);
% 
% %ns = 1064;
% WfMax_dual = zeros(nl, ns, 2);
% WfMax_dual(:,:,1) = WfMax_1064(:,1:ns);
% WfMax_dual(:,:,2) = WfMax_1548(:,1:ns);
% gapopen = -1024^2*2;
% [AlignedWfMax_dual, AlignedMask] = NWAlignScanImage_nd(WfMax_dual, gapopen);
% imwrite(AlignedMask, 'C:\WorkSpace\Data\Oz_July_2013\Aug1_Kara5_C\SatfixAppReflCorrLabel\Aug1_Kara5_C_AlignedMask.tif', 'tif')


clear;
ns = 2556;
nl = 1636;
datacubefilename = 'F:\ProcessedDWEL\June14_01_305_NE\June14_01_1064_EncoderCorr_Cube2_ancillary.img';
WfMax_1548 = multibandread(datacubefilename, [nl, ns, 9], 'int64', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});
datacubefilename = 'F:\ProcessedDWEL\June14_01_305_NE\June14_01_1548_EncoderCorr_Cube2_ancillary.img';
WfMax_1064 = multibandread(datacubefilename, [nl, ns, 9], 'int64', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});

% gapopen = -512;
% AlignedWfMax_1064 = NWAlignScanImage(WfMax_1064, gapopen);
% AlignedWfMax_1548 = NWAlignScanImage(WfMax_1548, gapopen);

WfMax_dual = zeros(nl, ns, 2);
WfMax_dual(:,:,1) = WfMax_1064;
WfMax_dual(:,:,2) = WfMax_1548;
gapopen = -1024^2*2;
[AlignedWfMax_dual, AlignedMask] = NWAlignScanImage_nd(WfMax_dual, gapopen);
imwrite(AlignedMask, 'F:\ProcessedDWEL\June14_01_305_NE\June14_01_EncoderCorr_Cube2_AlignedMask.tif', 'tif')