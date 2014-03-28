function AlignedWfMax_dual = AlignScanMask(datacubefile1064, datacubefile1548, ns, nl, gapopen, startline, AlignedMaskFile)
WfMax_1064 = multibandread(datacubefile1064, [nl, ns, 9], 'int32', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});
WfMax_1548 = multibandread(datacubefile1548, [nl, ns, 9], 'int32', 0, 'bil', 'ieee-le', {'Band', 'Direct', 6});

WfMax_dual = zeros(nl, ns, 2);
WfMax_dual(:,:,1) = WfMax_1064(1:nl,:);
WfMax_dual(:,:,2) = WfMax_1548(1:nl,:);
% gapopen = -1024^2*2;
[AlignedWfMax_dual, AlignedMask] = NWAlignScanImage_nd(WfMax_dual, gapopen, startline);
imwrite(AlignedMask, AlignedMaskFile, 'tif');
end