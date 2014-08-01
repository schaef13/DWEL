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
  for i=0,nlines-1 do begin
    line = envi_get_slice(fid=fid, /bil, pos=pos, line=i, xs=0, xe=nsamples-1)  ; line is ns by nb
    spos=where(reform((*pstate).mask[*,i]) gt 0,nspos)
    if (nspos gt 0) then begin
      temp=reform(line[spos,*]) ; temp is still 2D array, n_sample_valid(mask=1) by nb
      T_Power=T_power+total(double(reform(temp[*,(*pstate).range_pos]))^2)/dden
    endif
    
    b=fltarr(nsamples,nbands)
    r=fltarr(nsamples,nbands)
    
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
    w = where(temp lt s_thresh, nw, compl=fok, ncompl=nfok)
    if (nw gt 0) then begin
      r[w] = 0.0
      if (nfok gt 0) then begin
        r[fok] = b[fok]*spsum/temp[fok]
      endif
    endif else r = b*spsum/temp
    ;    b = b*spfac
    temp=0b
    w=0b
    fok=0b
    
    ; Find data that exceeds the thresholds in B and r and is non-negative
    ; Also test for exceeding threshold in at least one neighbour.
    ; Complementary data will be zeroed in output.
    ;    test=(b lt b_thresh)
    ;;    w=where((test or (r lt r_thresh)) and (shift(test,0,1) or shift(test,0,-1)),num)
    ;    w=where(test and (shift(test,0,1) or shift(test,0,-1)),num)
    ;    test=0b
    
    ; Zhan's change: both B and R have to be above the threshold and at least one neighbour does too.
    test1=(b lt b_thresh)
    test2=(r lt r_thresh)
    w=where(test1 or test2 or (shift(test1, 0, -1) and shift(test1, 0, 1)) or (shift(test2, 0, -1) and shift(test2, 0, 1)), num)
    test1=0b
    test2=0b
    ;
    ;    bs1 = shift(b,0,1)
    ;    bs2 = shift(b,0,-1)
    ;    test1 = (bs1 lt b_thresh)
    ;    test2 = (bs2 lt b_thresh)
    ;    bs1=0b
    ;    bs2=0b
    ;    test = (b lt b_thresh) or (r lt r_thresh)
    ;    w = where(test and (test1 or test2), num)
    ;    test1=0b
    ;    test2=0b
    ;    test=0b
    ;
    outdata = abs(line)
    if (num gt 0) then outdata[w] = zero
    w=0b
    
    if (nspos gt 0) then begin
      temp=reform(outdata[spos,*])
      F_Power=F_power+total(double(reform(temp[*,(*pstate).range_pos]))^2)/dden
    endif
    ;
    writeu, ofile, outdata
    if (save_br) then begin
      writeu, bfile,fix(round(b))
      writeu, rfile,fix(round(r*1000))
    endif
    outdata=0b
    b=0b
    temp=0b
    spos=0b
    r=0b
    ;
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
    ;
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
  endif
  return
end
