; Zhan Li's notes about this routine
; Input: 
;   fid: the file id of the waveform cube to be processed
;   p: normalized transmitted pulse model
;   b_thresh: b is the filtered waveform (waveform is convolved with pulse model p, it's actually a moving average of the 
;     waveform weighted by the pulse model). b_thresh is to zero those filtered bins b/c they are thought to be total noise.
;     Thus to choose the right b_thresh, check out the waveforms, get a sense of the backgrouund noise level and use this 
;     noise level a guide to set b_thresh. For example, get a mean value of noise bins, or the mean value of positive noise
;     bins as a more constrained threshold. 
;   r_thresh: it is not in use anymore. it was corrlation. wiki correlation to get a clearer definition in signal processing. 
;   dt: data type to be written to the outfile
;   pstate:   state={ $
;               T_Power:T_Power, $ ; 0.0, the mean square of bins in all waveforms before being filtered
;               F_Power:F_Power, $ ; 0.0, the mean square of bins in all waveforms after being filtered
;               nrpos:nrpos, $ ; number of waveform samples at distance further than 0.1m
;               range_pos:range_pos, $ ; positions where waveform samples are at distance further than 0.1m
;               mask:mask $
;               }
;   ierr:
;
pro apply_evi_filter_nu, fid, p, outfile, b_thresh, r_thresh, $
  dt, nsamples, nlines, nbands,  $
  save_br, bdatafile, rdatafile, pstate, ierr, quiet=quiet, $
  dt_br=dt_br

  compile_opt idl2
  
  print, 'in apply_evi_filter_nu, b_thresh=', b_thresh
  print, 'in apply_evi_filter_nu, r_thresh=', r_thresh

  sample_interval = 0.5 ;; unit: ns
  ;; create a Gaussian filter. 
  ;; the FWHM of outgoing pulse, ns
  outgoing_fwhm = 5.1
  ;; get a normalized Gaussian filter (integral is one)
  outgoing_sigma = outgoing_fwhm / (2 * sqrt(2*alog(2)))
  pulse_half_range = outgoing_fwhm * sqrt(alog(0.001)/alog(0.5)) / 2.0
  gp_half_len = fix(pulse_half_range / sample_interval)
  tmpx = (indgen(2*gp_half_len+1) - gp_half_len) * sample_interval
  tmpy = 1.0 / (outgoing_sigma*sqrt(2*!pi)) * exp(-1*tmpx*tmpx/(2*outgoing_sigma^2))
  gaussian_pulse = fltarr(1, size(tmpy, /n_elements))
  gaussian_pulse[0, *] = tmpy
  
  ierr=0
  T_Power=0.0d0
  F_Power=0.0d0
  dden=double(nsamples)*double(nlines)*double((*pstate).nrpos)  ; number of bins in all the waveforms
  ;set data type (based on input file)
    if (dt lt 4 or dt gt 9) then begin
      zero=0s
    endif else zero=0.0

  ; Open output files
  openw, ofile, outfile, /get
  if (save_br) then begin
    openw, bfile, bdatafile, /get
    openw, rfile, rdatafile, /get
    openw, blocfile, bdatafile+'bloc.img', /get
    openw, rlocfile, rdatafile+'rloc.img', /get
    openw, locfile, outfile+'loc.img', /get
  endif

  if ~keyword_set(quiet) then begin
  ;set up the progress bar
    envi_file_query, fid, fname=infile
    info_text=[file_basename(infile),'Filtering each waveform based on model pulse']
    envi_report_init,info_text,title='Processing Waveforms',base=wb_report,/interupt
  endif

  ; Calculate some values needed in the convolution
  m = max(p,mpos)
  nlead = mpos
  ntrail = n_elements(p)-nlead-1

;print,'max p=',m
;print,'fwhm p=',pnorm/m
;print,'nlead,ntrail=',nlead,ntrail

;now put in code to ensure the result if there is truncation

; Pad pulse to make symmetrical about the peak
    if (ntrail gt nlead) then begin
      ppad = [replicate(0,ntrail-nlead),p]
    endif else begin
        ppad = [p,replicate(0,nlead-ntrail)]
    endelse

    value=max(ppad,max_point)
;print,'elements in ppad=',n_elements(ppad)
;print,'mid point=',n_elements(ppad)/2
;print,'max_point=',max_point

  num_pad=n_elements(ppad)  ; number of bins after zero padding
  fnum=float(num_pad)
  psum=total(ppad^2)  ; sum of square intensity
  pnorm=total(abs(ppad))  ; sum of waveform intensity
  spfac=psum/pnorm  ; ??
  spsum=sqrt(psum/fnum) ; root of mean square
  pp_filter=fltarr(1,num_pad)
  pp_filter[0,*]=ppad
  
;  av_filter=fltarr(1,num_pad)+1.0
;  av_den=float(num_pad)
  print,'num_pad=',num_pad

;s_thresh finds areas with very low signal after smoothing so that
;correlation coefficient is stable and not spurious
  s_thresh=b_thresh/100.0 ; s_thresh is of no use in this function. b_thresh is the one to select signal
  pos=indgen(nbands)
; Loop through each line, calculate B and R (correlation) and apply filter.
; Write to output file.
  gp_len = size(gaussian_pulse, /n_elements)
  for i=0,nlines-1 do begin
    line = envi_get_slice(fid=fid, /bil, pos=pos, line=i, xs=0, xe=nsamples-1)  ; line is ns by nb
    spos=where(reform((*pstate).mask[*,i]) gt 0,nspos)
    if (nspos gt 0) then begin
      temp=reform(line[spos,*]) ; temp is still 2D array, n_sample_valid(mask=1) by nb
      T_Power=T_power+total(double(reform(temp[*,(*pstate).range_pos]))^2)/dden
    endif

    b=fltarr(nsamples,nbands)
    r=fltarr(nsamples,nbands)

    ;; smooth the return waveform with a Gaussian filter created
    ;;according to the outgoing pulse width. The objective is to
    ;;filter out noises in the return waveform before correlating it
    ;;with outgoing pulse and searching for signals. 
    ;; store the original waveform in another variable for later use
    ;; and use variable 'line' to store a Gaussian-filtered waveform
    oldline = line
    T_Pad=total(float(line[*,0:19]),2)/20.0 ; 1 by ns array, each element is an average of the first 20 bins of each waveform
    T_pad=cmreplicate(T_pad,gp_len)  ; num_pad by ns array, each row is the averages of all columns in this line
    B_pad=total(float(line[*,nbands-20:nbands-1]),2)/20.0 ; average of the last 20 bins
    B_pad=cmreplicate(B_pad,gp_len)
    t=[[T_pad],[float(line)],[B_pad]] ; use the average of first and last 20 bins to pad the leading and trail part of all the waveforms in this line 
    T_pad=0b
    B_pad=0b
    c = convol(t, gaussian_pulse)
    line = c[*, gp_len:gp_len+nbands-1]
    c = 0b

    T_Pad=total(float(line[*,0:19]),2)/20.0 ; 1 by ns array, each element is an average of the first 20 bins of each waveform
    T_pad=cmreplicate(T_pad,num_pad)  ; num_pad by ns array, each row is the averages of all columns in this line
    B_pad=total(float(line[*,nbands-20:nbands-1]),2)/20.0 ; average of the last 20 bins
    B_pad=cmreplicate(B_pad,num_pad)
    t=[[T_pad],[float(line)],[B_pad]] ; use the average of first and last 20 bins to pad the leading and trail part of all the waveforms in this line 
    T_pad=0b
    B_pad=0b
    c=convol(t,pp_filter,psum)
    b=spfac*c[*,num_pad:num_pad+nbands-1] ; b is actually the moving average of waveform bins weighted by the pulse model.
    c=0b
    temp=sqrt(smooth(t^2,[1,num_pad])) ; root mean square of the return waveform
    temp = temp[*,num_pad:num_pad+nbands-1]
    ;; w = where(temp lt s_thresh, nw, compl=fok, ncompl=nfok)
    ;; if (nw gt 0) then begin
    ;;   r[w] = 0.0
    ;;   if (nfok gt 0) then begin
    ;;     r[fok] = b[fok]*spsum/temp[fok]
    ;;   endif
    ;; endif else r = b*spsum/temp 
    r = b*spsum/temp
    ; r is actually the normalized moving average such that the moving averaged waveform is at a comparable level with unit peak.
    temp=0b
    w=0b
    fok=0b

    ; Find data that meets several criteria in b and r
    ; Complementary data will be zeroed in output.
    ;; due to the nature of asymmetry of DWEL pulse, the moving
    ;; average of return waveform with pulse model will give us a
    ;;symmetric and sinc-function-like b and r. Where there is a real
    ;;signal, in b and r we should see one large peak in the middle,
    ;;two deep valleys on both sides and then two small peaks on both
    ;;sides. The criteria here is to find a section of waveform bins
    ;;that have such a pattern in the b and r. Then this section of
    ;;waveform bins will be retained as a return signal. Otherwise the
    ;;waveform bins will be changed to zeros. 
    ;; criterion 1: absolute of b is above a given threshold
;;    index_b = abs(b) GE b_thresh
    ;; criterion 2: absolute of r is above a given threshold
;;    index_r = abs(r) GE r_thresh
    ;; criterion 3: find the peaks and troughs in the b and r that
    ;; also meet criteria 1 and 2
    db = b - shift(b, 0, 1)
    dr = r - shift(r, 0, 1)
    index_bpeak = (shift(db, 0, 1) GT 0) AND (shift(db, 0, -1) LE 0)
    index_btrough = (shift(db, 0, 1) LE 0) AND (shift(db, 0, -1) GT 0)
    index_rpeak = (shift(dr, 0, 1) GT 0) AND (shift(dr, 0, -1) LE 0)
    index_rtrough = (shift(dr, 0, 1) LE 0) AND (shift(dr, 0, -1) GT 0)
    index_bloc = (index_bpeak AND (b GE b_thresh)) OR (index_btrough AND (b LE -1*b_thresh))
    index_rloc = (index_rpeak AND (r GE r_thresh)) OR (index_rtrough AND (r LE -1*r_thresh))
;;    index_bloc = (index_bpeak OR index_btrough) AND index_b
;;    index_rloc = (index_rpeak OR index_rtrough) AND index_r
    index_bloc[*, 0] = 0b
    index_rloc[*, nbands-1] = 0b
    ;; criterion 4: find those bins that have at least four neighbors
    ;;meeting the criterion 3 within a given search window of which
    ;;size is determined from the pulse width
    search_width = 51 ; in unit of number of bins, from the observation of self cross-correlation of DWEL pusle model. 
    min_peaknum = 3 ; minimum number of detected peaks or troughs
    ;; index_peak = (smooth(float(index_bloc), [1, search_width], /EDGE_TRUNCATE) GE float(min_peaknum*2)/float(search_width)) AND (smooth(float(index_rloc), [1, search_width], /EDGE_TRUNCATE) GE float(min_peaknum*2)/float(search_width)) ;; 60 is from the observation of self cross-correlation of DWEL pusle model. 
    index_loc = index_bloc AND index_rloc
    index_peak = (smooth(float(index_loc), [1, search_width], /EDGE_TRUNCATE) GE float(min_peaknum*2)/float(search_width)) AND index_loc
    ;; ;; ****debug****
    ;; print, 'b peak #: ', total(smooth(float(index_bloc), [1, search_width]) GE 5.0/float(search_width))
    ;; print, 'r peak #: ', total(smooth(float(index_rloc), [1, search_width]) GE 5.0/float(search_width))
    ;; ;; ****
    pos_peak = where( transpose(index_peak), peakcount)
    IF peakcount GT 0 THEN BEGIN
       pos_signal = lonarr(search_width*size(pos_peak, /n_elements))
       half_width = fix(search_width/2)
       ;; retain bins within windows of given size centered at identified
       ;;peak locations.  
       FOR w = -1*half_width, half_width DO BEGIN
          pos_signal[(w+half_width)*peakcount:(w+half_width+1)*peakcount-1] = pos_peak + w
       ENDFOR 
       tmppos = where(pos_signal GE 0 AND pos_signal LT size(b, /n_elements))
       pos_signal = pos_signal[tmppos]
       tline = transpose(oldline)
       outdata = make_array(dimension=size(tline, /dimensions), type=size(tline, /type))
       outdata[pos_signal] = tline[pos_signal]
       outdata = transpose(outdata)
       pos_signal = 0
    ENDIF ELSE BEGIN
       outdata = make_array(dimension=size(oldline, /dimensions), type=size(oldline, /type))
    ENDELSE  

    if (nspos gt 0) then begin
      temp=reform(outdata[spos,*])
      F_Power=F_power+total(double(reform(temp[*,(*pstate).range_pos]))^2)/dden
    endif

    writeu, ofile, outdata
    if (save_br) then begin
      writeu, bfile,fix(round(b))
      writeu, rfile,fix(round(r*1000))
      writeu, blocfile, index_bloc
      writeu, rlocfile, index_rloc
      writeu, locfile, index_peak
    endif
    outdata=0b
    b=0b
    temp=0b
    spos=0b
    r=0b

    if ~keyword_set(quiet) then begin
      envi_report_stat,wb_report,i,nlines,cancel=cancel
      if (cancel) then begin
          result=dialog_message('Cancelling convolution filter processing',$
          title='Cancelling Job')
          envi_report_init, base=wb_report,/finish
          ierr=1
          goto, cleanup
      endif
    endif

  endfor
  if ~keyword_set(quiet) then envi_report_init, base=wb_report,/finish

  (*pstate).T_Power=T_Power
  (*pstate).F_Power=F_Power

  dt_br=2 ; integer

  cleanup:
  ; Close output files
  free_lun, ofile,/force
  if (save_br) then begin
    free_lun, bfile,/force
    free_lun, rfile,/force
    envi_setup_head, data_type=size(index_bloc, /type), fname=bdatafile+'bloc.img', nb = nbands, nl = nlines, $
    ns = nsamples, interleave=1, $
    /write, r_fid=blocfile, $
    zplot_titles=['bin','bloc']
    envi_setup_head, data_type=size(index_rloc, /type), fname=rdatafile+'rloc.img', nb = nbands, nl = nlines, $
    ns = nsamples, interleave=1, $
    /write, r_fid=rlocfile, $
    zplot_titles=['bin','rloc']
    envi_setup_head, data_type=size(index_peak, /type), fname=outfile+'loc.img', nb = nbands, nl = nlines, $
    ns = nsamples, interleave=1, $
    /write, r_fid=locfile, $
    zplot_titles=['bin','loc']
    free_lun, blocfile, /force
    free_lun, rlocfile, /force
    free_lun, locfile, /force
 ENDIF

  return
end
