; This function reads the ASCII file of mean waveform from a statistical average over a designated area
; and return the time/range and the intensity of the waveform

; just test emacs backup

function Get_AsciiWf, ENVIAsciiWfFile
  openr, wffid, ENVIAsciiWfFile, /get_lun
  
  ; check the lines in the file
  nlines = file_lines(ENVIAsciiWfFile)
  
  nb = nlines - 3 ; the first three lines are header information
  wf_x = fltarr(nb)
  wf_y = fltarr(nb)
  tmpline = ''
  i=0b
  for i=1,3 do begin ; skip the first three lines of header
    readf, wffid, tmpline
  endfor
  for i=0,nb-1 do begin
    readf, wffid, tmpline
    reads, tmpline, var1, var2
    wf_x[i]=var1
    wf_y[i]=var2
  endfor
  
  close, wffid
  free_lun, wffid
  
  return, {wf_x:wf_x, wf_y:wf_y}
end