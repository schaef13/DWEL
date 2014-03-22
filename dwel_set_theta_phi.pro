function dwel_set_theta_phi, pstat
compile_opt idl2

ShotZen=fltarr((*pstat).NShots,(*pstat).Nscans)
ShotAzim=fltarr((*pstat).Nshots,(*pstat).Nscans)

;compute the zenith [-180,180] and azimuth [0,360]
;for every shot from the encoder values
ShotZen = (262144.0-(*pstat).ShotZen)*360.0/524288.0
ShotAzim = (*pstat).ShotAzim*360.0/524288.0
index = where(ShotZen lt 0, count)

;reset negative zeniths to positive and account in phase
if count gt 0 then begin
  ShotZen[index] = - ShotZen[index]
  ShotAzim[index] = ShotAzim[index] + 180.0
endif

;correct azimuth to [0,360]
index = where(ShotAzim[*] gt 360.0, count)
if count gt 0 then begin
  ShotAzim[index] = ShotAzim[index] - 360.0
endif

;Put the results back into the structure
(*pstat).ShotZen=ShotZen
(*pstat).shotAzim=ShotAzim

ShotZen=0b
ShotAzim=0b

return,1b

end

