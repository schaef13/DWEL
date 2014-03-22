; extract saturated and unsaturated waveform pairs from the 6-part panel scan
function SatCalAnalyze, wf
  wfpeak = max(wf, wfpeakloc)
  tmpind = where(wf ge wfpeak*0.5, tmpcount)
  ;tmpind[1:tmpcount-1] - tmpind[0:tmpcount-2]
  return, {PeakI:wfpeak, PeakLoc:wfpeakloc, Width:(tmpind[tmpcount-1]-tmpind[0])}
end

pro PanelScan2SatWfPair, PanelScanCubeFile, PanelScanAncFile, PanelBoundingBox, Tzero, RangeThreshold, UnSatThreshold, $
  OutCubeFile, OutAncFile, $
  BkgWfFile, OutAsciiFile
  ; open the data cube
  envi_open_file, PanelScanCubeFile, r_fid=panelscan_fid, /no_realize
  
  envi_file_query, panelscan_fid, ns=ns, nl=nl, nb=nb, wl=wl, $
    xstart=xstart, ystart=ystart, data_type=type, $
    interleave=ftype, fname=fname, dims=dims, bname=bname
    
  envi_open_file, PanelScanAncFile, r_fid=anc_fid, /no_realize
  
  envi_file_query, anc_fid, ns=anc_ns, nl=anc_nl, nb=anc_nb, wl=anc_wl, $
    xstart=anc_xstart, ystart=anc_ystart, data_type=anc_type, $
    interleave=anc_ftype, fname=anc_fname, dims=anc_dims, bname=anc_bname
    
  tmplogic = strcmp(anc_bname, 'Mask', /fold_case)
  tmppos = where(tmplogic) 
  topmask = envi_get_data(dims=anc_dims, fid=anc_fid, pos=tmppos[0])
  topmask = byte(topmask)
  
  band_pos = indgen(nb)
  
  linenum = PanelBoundingBox[3]-PanelBoundingBox[2]+1
  samplenum = PanelBoundingBox[1]-PanelBoundingBox[0]+1
  
  linemask = bytarr(linenum)
  mask = intarr(samplenum, linenum)
  wfmax = fltarr(samplenum, linenum)
  range = fltarr(samplenum, linenum)
  
  wf_array = make_array(samplenum, nb, type=type)
  
  baseline = get_asciiwf(BkgWfFile)
  satcalpair = make_array(6, fix(samplenum/2)+1, /float)
  
  openw, OutFid, OutCubeFile, /get_lun
  openw, OutAsciiFid, OutAsciiFile, /get_lun
  printf, OutAsciiFid, format='(%"PatchID,Unsat_PeakI,Unsat_PeakLoc,Unsat_Width,Sat_PeakI,Sat_PeakLoc,Sat_Width")'
  PatchId = 1
  tmpbyte = 1b
  for l=PanelBoundingBox[2],PanelBoundingBox[3] do begin
    wf_array[*, *] = 0
    
    data = envi_get_slice(/bil, fid=panelscan_fid, line=l, pos=band_pos, xe=PanelBoundingBox[1], xs=PanelBoundingBox[0])
    tmpmax = max(data, dimension=2, maxI)
    tmppos = array_indices(data, maxI)
    tmprange = (tmppos[1, *]*0.5 - Tzero)*0.15
    tmpmask = (tmprange gt RangeThreshold[0]) and (tmprange lt RangeThreshold[1]) and topmask[PanelBoundingBox[0]:PanelBoundingBox[1], l]
    tmpsatmask = tmpmask and (tmpmax ge 1023)
    if total(tmpsatmask) eq 0 then begin
      linemask[l-PanelBoundingBox[2]]=0b
      continue
    endif
    tmpunsatmask = tmpmask and (tmpmax lt UnSatThreshold)
    if total(tmpunsatmask) eq 0 then begin
      linemask[l-PanelBoundingBox[2]]=0b
      continue
    endif
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; test
    print, [total(tmpunsatmask), total(tmpsatmask), total(tmpunsatmask)-total(tmpsatmask)]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (abs(total(tmpunsatmask)-total(tmpsatmask)) gt 10) then begin
      linemask[l-PanelBoundingBox[2]]=0b
      if (abs(total(tmpunsatmask)-total(tmpsatmask)) gt 100) then begin
        if tmpbyte then begin
          PatchID = PatchID + 1
          tmpbyte = 0
        endif
      endif
      continue
    endif
    tmpbyte = 1
    tmppairnum = min([total(tmpunsatmask), total(tmpsatmask)])
    if (tmppairnum eq total(tmpunsatmask)) then begin
      wf_array[0:tmppairnum-1, *] = data[where(tmpunsatmask), *]
      tmpsub = where(tmpsatmask, tmpcount)
      wf_array[tmppairnum:2*tmppairnum-1, *] = data[tmpsub[tmpcount-tmppairnum:tmpcount-1], *]
    endif
    if (tmppairnum eq total(tmpsatmask)) then begin
      tmpsub = where(tmpunsatmask, tmpcount)
      wf_array[0:tmppairnum-1, *] = data[tmpsub[0:tmppairnum-1], *]
      wf_array[tmppairnum:2*tmppairnum-1, *] = data[where(tmpsatmask), *]
    endif
    mask[0:tmppairnum-1, l-PanelBoundingBox[2]]=1
    mask[tmppairnum:2*tmppairnum-1, l-PanelBoundingBox[2]]=2
    wfmax[*, l-PanelBoundingBox[2]] = max(wf_array, dimension=2, maxI)
    tmppos = array_indices(wf_array, maxI)
    range[*, l-PanelBoundingBox[2]] = (tmppos[1, *]*0.5 - Tzero)*0.15
    linemask[l-PanelBoundingBox[2]]=1b
    writeu, OutFid, wf_array
    
    ; get peak intensity, location and pulse width of the sat. and unsat. waveform pair
    for s=0, tmppairnum-1 do begin
      tmp = SatCalAnalyze(wf_array[s, *]-baseline.wf_y)
      satcalpair[0:2, s]=[tmp.PeakI, tmp.PeakLoc, tmp.Width]
      tmp = SatCalAnalyze(wf_array[2*tmppairnum-1-s, *]-baseline.wf_y)
      satcalpair[3:5, s]=[tmp.PeakI, tmp.PeakLoc, tmp.Width]
    endfor
    printf, OutAsciiFid, format='(%"%d,%f,%f,%f,%f,%f,%f")', [replicate(PatchID,1,tmppairnum), satcalpair[*, 0:tmppairnum-1]]
  endfor
  
  mask = mask[*, where(linemask)]
  wfmax = wfmax[*, where(linemask)]
  range = range[*, where(linemask)]
  
  ; set up header for output image file
  envi_setup_head, fname=OutCubeFile, $
    ns=samplenum, nl=total(linemask), nb=nb, $
    data_type=size(wf_array, /type), $
    interleave=1, $
    wl=wl, $
    bname=bname, $
    /write
    
  openw, OutAncFid, OutAncFile, /get_lun
  writeu, OutAncFid, float(wfmax)
  writeu, OutAncFid, float(range)
  writeu, OutAncFid, float(mask)
  
  envi_setup_head, fname=OutAncFile, $
    ns=samplenum, nl=total(linemask), nb=3, $
    data_type=size(wfmax, /type), $
    interleave=0, $
    bname=['Waveform Max', 'Range', 'Cover Type'], $
    /write
    
  close, panelscan_fid, OutFid, OutAncFid, OutAsciiFid
end