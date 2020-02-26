;+
; FUNCTION dp_interpol_cal
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: core function; interpolates calibration points to positions of sample measurements based on the selection
;          of interpolation method and cal treatment method.
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_interpol_cal, xdata, ydata, vd_cal, sel_calip, sel_caltreat, sequence

  COMMON dp_data

  calblock_rsd = DBLARR(N_ELEMENTS(sequence.id))*!Values.D_NAN
  cal_devtofit = !Values.D_NAN
  ix_vd_cals = !NULL
  vout = !NULL

  ip_mthd = cal_ip_mthd[sel_calip] ; select from common variables
  treat_mthd = cal_treat_mthd[sel_caltreat]

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  xout = xdata

; --------------> determination of calibration points (indices) <--------------

  CASE treat_mthd OF ; determine cal points for interpolation based on cal treatment method
    'bracketing': $ ; determine cal indices...
      BEGIN
        ix_brack_bef = (sequence.ix_init_samblock)[WHERE(sequence.ix_init_samblock NE -1)]-1
        ix_brack_aft = (sequence.ix_end_samblock)[WHERE(sequence.ix_end_samblock NE -1)]+1
        ix_bracks_all = ([ix_brack_bef, ix_brack_aft])[SORT([ix_brack_bef, ix_brack_aft])]
        ix_bracks = ix_bracks_all[uniq(ix_bracks_all)] ; remove double indices if only one cal point between samples

        ix_vd_cals = ix_bracks

        IF sequence.miss_1stcal EQ 1 THEN BEGIN
          ix_bracks = [ix_bracks[0], ix_bracks] ; missing 1st cal: use cal after first sample
          ix_vd_cals = ix_vd_cals[1:-1]
        ENDIF

        IF sequence.miss_lastcal EQ 1 THEN BEGIN
          ix_bracks = [ix_bracks, ix_bracks[-1]] ; missing last cal: use cal before last sample
          ix_vd_cals = ix_vd_cals[0:-2]
        ENDIF


        v = DBLARR(N_ELEMENTS(ix_vd_cals))
        x = DBLARR(N_ELEMENTS(ix_vd_cals))

        FOR i=0, N_ELEMENTS(ix_vd_cals)-1 DO BEGIN
          w = WHERE(vd_cal EQ ix_vd_cals[i])
          IF w NE -1 THEN BEGIN
            v[i] = ydata[ix_vd_cals[i]]
            x[i] = xdata[ix_vd_cals[i]]
          ENDIF ELSE BEGIN
            v[i] = !VALUES.D_NAN
            x[i] = !VALUES.D_NAN
          ENDELSE
        ENDFOR

      END

    'block_mean': $ ; v and x have to be calculated here
      BEGIN
        ix_calinit = (sequence.ix_init_calblock)[WHERE(sequence.ix_init_calblock NE -1)]
        ix_calend = (sequence.ix_end_calblock)[WHERE(sequence.ix_end_calblock NE -1)]
        ix_saminit = (sequence.ix_init_samblock)[WHERE(sequence.ix_init_samblock NE -1)]
        ix_samend = (sequence.ix_end_samblock)[WHERE(sequence.ix_end_samblock NE -1)]

        n_perblock = ix_calend-ix_calinit+1
        n_blocks = N_ELEMENTS(n_perblock)
        n_typical = MEDIAN(n_perblock)
        seq_ix = LINDGEN(N_ELEMENTS(sequence.id))
        seq_ix[WHERE(sequence.id NE id_cal)] = -1

        IF (sequence.miss_1stcal NE 1 AND n_perblock[0] GT n_typical) THEN BEGIN ; use typical block length at beginning of series
          seq_ix[0:ix_calend[0]-n_typical] = -1
          n_perblock[0] = n_typical
        ENDIF ; missing 1st cal is not treated explicitly -> interpol function is then used to estimate the values that are missing

        IF (sequence.miss_lastcal NE 1 AND n_perblock[-1] GT n_typical) THEN BEGIN ; use typical block length at end of series
          seq_ix[ix_calinit[-1]+n_typical:-1] = -1
          n_perblock[-1] = n_typical
        ENDIF

        ix_vd_cals = seq_ix[WHERE(seq_ix NE -1)]
        v_cals = DBLARR(N_ELEMENTS(ix_vd_cals))
        x_cals = DBLARR(N_ELEMENTS(ix_vd_cals))

        FOR i=0, N_ELEMENTS(ix_vd_cals)-1 DO BEGIN
          w = WHERE(vd_cal EQ ix_vd_cals[i])
          IF w NE -1 THEN BEGIN
            v_cals[i] = ydata[ix_vd_cals[i]]
            x_cals[i] = xdata[ix_vd_cals[i]]
          ENDIF ELSE BEGIN
            v_cals[i] = !VALUES.D_NAN
            x_cals[i] = !VALUES.D_NAN
          ENDELSE
        ENDFOR

        v = DBLARR(n_blocks)
        x = DBLARR(n_blocks)

        FOR i=0, n_blocks-1 DO BEGIN
          v[i] = mean(v_cals[(TOTAL(n_perblock[0:i])-n_perblock[i]):(TOTAL(n_perblock[0:i])-1)], /DOUBLE, /NAN)
          x[i] = mean(x_cals[(TOTAL(n_perblock[0:i])-n_perblock[i]):(TOTAL(n_perblock[0:i])-1)], /DOUBLE, /NAN)
          calblock_rsd[ix_calend[i]] = stddev(v_cals[(TOTAL(n_perblock[0:i])-n_perblock[i]):(TOTAL(n_perblock[0:i])-1)], /DOUBLE, /NAN)/v[i]
        ENDFOR
      END

    'preceding': $ ; determine cal indices...
      BEGIN
        ix_init = (sequence.ix_init_calblock)[WHERE(sequence.ix_init_calblock NE -1)]
        ix_end = (sequence.ix_end_calblock)[WHERE(sequence.ix_end_calblock NE -1)]
        ix_brack_bef = (sequence.ix_init_samblock)[WHERE(sequence.ix_init_samblock NE -1)]-1
        ix_brack_aft = (sequence.ix_end_samblock)[WHERE(sequence.ix_end_samblock NE -1)]+1 ; needed for last cal
        n_perblock = ix_end-ix_init+1
        n_typical = MEDIAN(n_perblock)
        ix_prec = ix_brack_bef

        IF sequence.miss_1stcal EQ 1 THEN ix_prec = [ix_prec[0], ix_prec]
        IF sequence.miss_lastcal EQ 1 THEN ix_prec = [ix_prec, ix_prec[-1]] $
            ELSE BEGIN ; last cal not missing; check last block length and select a point that would correspond...
              IF n_perblock[-1] GE n_typical THEN $ ; ...to a preceding cal point before the next sample block
                ix_prec = [ix_brack_bef, (ix_brack_aft[-1]+n_typical-1)] $
                  ELSE ix_prec = [ix_brack_bef, ix_brack_aft[-1]]
            ENDELSE

        ix_vd_cals = ix_prec
        v = DBLARR(N_ELEMENTS(ix_vd_cals))
        x = DBLARR(N_ELEMENTS(ix_vd_cals))

        FOR i=0, N_ELEMENTS(ix_vd_cals)-1 DO BEGIN
          w = WHERE(vd_cal EQ ix_vd_cals[i], nw)
          IF nw NE 0 THEN BEGIN
            v[i] = ydata[ix_vd_cals[i]]
            x[i] = xdata[ix_vd_cals[i]]
          ENDIF ELSE BEGIN
            v[i] = !VALUES.D_NAN
            x[i] = !VALUES.D_NAN
          ENDELSE
        ENDFOR

        IF sequence.miss_1stcal THEN x[0] = xdata[0]
        IF sequence.miss_lastcal THEN x[-1] = xdata[-1]

      END

  ENDCASE

; --------------> interpolation <--------------

  CASE ip_mthd OF ; apply interpolation method using the selected/calculated cal points
    'p2p': vout = interpol(V, X, XOUT, /NAN);, /QUADRATIC)

    'calsmean': vout = MAKE_ARRAY(N_ELEMENTS(ydata), VALUE=mean(v, /NAN, /DOUBLE), /DOUBLE)

    'linear_fit': $
      BEGIN
        w_finite = WHERE(FINITE(v) NE 0)
        x = x[w_finite]
        v = v[w_finite]
        IF N_ELEMENTS(v) GT 2 THEN BEGIN ; perform fit if enough data
          params = linfit(x, v, /DOUBLE) ; y = a+bx; params[0] = a; params[1] = b
          vout = DBLARR(N_ELEMENTS(xdata))
          vout[*] = (params[0]+params[1]*xdata[*])
        ENDIF ELSE BEGIN ; not enough data: skip fit
          vout = DBLARR(N_ELEMENTS(xdata))*!Values.D_NAN
        ENDELSE
      END

    'polyfit_dg2': $ ; requires modification of xdata: subtract starting value to reduce absolute value
      BEGIN
        w_finite = WHERE(FINITE(v) NE 0)
        x = x[w_finite]
        v = v[w_finite]
        IF N_ELEMENTS(v) GT 2 THEN BEGIN ; perform fit if enough data
          params = poly_fit(x-x[0], v, 2, /DOUBLE) ; y = a+bx+cx2; params[0] = a; params[1] = b; params[2] = c
          vout = DBLARR(N_ELEMENTS(xdata))
          vout[*] = (params[0]+params[1]*(xdata-(xdata[ix_vd_cals])[0])[*]+params[2]*(xdata-(xdata[ix_vd_cals])[0])[*]^2.)
          cal_devtofit = mean(ABS(ydata[ix_vd_cals]-vout[ix_vd_cals]), /DOUBLE)
        ENDIF $
          ELSE vout = DBLARR(N_ELEMENTS(xdata))*!Values.D_NAN
      END
  ENDCASE



  IF ip_mthd NE 'polyfit_dg2' THEN BEGIN ; perform polyfit through cals to get cal series rsd
    w_finite = WHERE(FINITE(v) NE 0)
    x = x[w_finite]
    v = v[w_finite]
    IF N_ELEMENTS(v) GT 2 THEN BEGIN ; perform fit if enough data
      params = poly_fit(x-x[0], v, 2, /DOUBLE) ; y = a+bx+cx2; params[0] = a; params[1] = b; params[2] = c
      vout_temp = DBLARR(N_ELEMENTS(xdata))
      vout_temp[*] = (params[0]+params[1]*(xdata-(xdata[ix_vd_cals])[0])[*]+params[2]*(xdata-(xdata[ix_vd_cals])[0])[*]^2.)
      cal_devtofit = mean(ABS(ydata[ix_vd_cals]-vout_temp[ix_vd_cals]), /DOUBLE)
    ENDIF
  ENDIF



  strct = { ix_vd_cals:   ix_vd_cals, $
          cal_interpol: vout, $
          calblock_rsd: calblock_rsd, $
          cal_devtofit: cal_devtofit }



  RETURN, strct

END