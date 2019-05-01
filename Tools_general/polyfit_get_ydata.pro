FUNCTION polyfit_get_ydata, fct_dgr, xdata, parms

  ydata = DBLARR(N_ELEMENTS(xdata))

  CASE fct_dgr OF
    1: ydata[*]=parms[0]+parms[1]*xdata[*]
    2: ydata[*]=parms[0]+parms[1]*xdata[*]+parms[2]*xdata[*]^2
    3: ydata[*]=parms[0]+parms[1]*xdata[*]+parms[2]*xdata[*]^2+parms[3]*xdata[*]^3
    4: ydata[*]=parms[0]+parms[1]*xdata[*]+parms[2]*xdata[*]^2+parms[3]*xdata[*]^3+parms[4]*xdata[*]^4
  ENDCASE

  RETURN, ydata

END