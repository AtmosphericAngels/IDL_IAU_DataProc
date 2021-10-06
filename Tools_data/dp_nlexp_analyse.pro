;+
; FUNCTION: dp_nlexp_analyse
;
; AUTHOR: F. Obersteiner, florian.obersteiner@kit.edu, June 2017.
;
; OPERATION:
; - use cal mrs to calculate tgt mrs
; - compare cal-based mrs to tgt mrs
; - use difference to get NL function parameters
; - if no cal mrs are specified, the function tries to estimate a cal mr based on the tgts specified.
;
; USAGE OF THE OUTPUT:
; - linear-proportional MR put into NL function gives a correction term
; - subtract the correction term to the linear-proportional MR to correct detector non-linearity
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_nlexp_analyse, fct_dgr, sel_exp, sel_subst, FORCE_ZERO=force_zero, $
                           LOUD=loud, SHOW_PLOTS=show_plots, ESTIMATE_CAL=estimate_cal, $
                           VERBOSE=verbose, SAVEPLOT=saveplot, PIC_FILETYPE=pic_filetype

  COMMON dp_data

  IF NOT KEYWORD_SET(loud) THEN loud = 0
  IF NOT KEYWORD_SET(verbose) THEN verbose = 0
  IF NOT KEYWORD_SET(force_zero) THEN force_zero = 0
  IF NOT KEYWORD_SET(show_plots) THEN show_plots = 0
  IF NOT KEYWORD_SET(estimate_cal) THEN estimate_cal = 0
  IF NOT KEYWORD_SET(saveplot) THEN saveplot = 0
  IF NOT KEYWORD_SET(pic_filetype) THEN pic_filetype = '.ps'
  status = 0

  ; check if cal and tgt mrs loaded
  IF PTR_VALID((dp_expcfg[sel_exp]).tgt_mrs.mr_ppt) THEN BEGIN
    IF *(dp_expcfg[sel_exp]).tgt_mrs.mr_ppt EQ !NULL THEN BEGIN
      IF loud THEN msg=DIALOG_MESSAGE('Please load Tgt MRs first.', /INFORMATION)
      RETURN, !NULL
    ENDIF
  ENDIF ELSE BEGIN
    IF loud THEN msg=DIALOG_MESSAGE('Please load Tgt MRs first.', /INFORMATION)
    RETURN, !NULL
  ENDELSE

  IF *(dp_expcfg[sel_exp]).cal_mrs.mr_ppt EQ !NULL $
    THEN BEGIN
      IF loud THEN msg=DIALOG_MESSAGE('No Cal MRs found, switching to Cal-MR estimation.', /INFORMATION)
      estimate_cal = 1
    ENDIF ; ELSE estimate_cal = estimate_cal

  ; check if sample treatment is 'individual'
  treat_arr = (dp_chrom[sel_exp]).subst.rres.sam_treat
  w_not_indiv = WHERE(treat_arr NE 'individual', nw)
  IF nw GT 0 THEN BEGIN
    IF loud THEN msg=DIALOG_MESSAGE('Please use sample treatment method "individual".', /INFORMATION)
    RETURN, !NULL
  ENDIF

  exp_fname = ((dp_chrom[sel_exp]).exp_fname)[0]

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  eval_mode = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]

  subst = (dp_chrom[sel_exp])[0].subst[sel_subst].name

  IF estimate_cal THEN $
    cal_substs = (dp_chrom[sel_exp])[0].subst.name $
  ELSE BEGIN
    cal_substs = *(dp_expcfg[sel_exp]).cal_mrs.substance
    cal_substs = cal_substs[uniq(cal_substs[SORT(cal_substs)])]
  ENDELSE

  tgt_substs = *(dp_expcfg[sel_exp]).tgt_mrs.substance
  tmp = tgt_substs[SORT(tgt_substs)]
  tgt_substs = tmp[uniq(tmp)]

  w_s_cal = WHERE(cal_substs EQ subst)
  w_s_tgt = WHERE(tgt_substs EQ subst)

  CASE estimate_cal OF
    0: $ ; unit from cal mr table
      BEGIN
      IF (*((dp_expcfg[sel_exp]).cal_mrs.unit)) NE !NULL THEN $
        unit = (*(dp_expcfg[sel_exp]).cal_mrs.unit)[w_s_cal] ELSE unit = 'NA'
    END
    1: $ ; unit from tgt mr table
      BEGIN
      IF (*((dp_expcfg[sel_exp]).tgt_mrs.unit)) NE !NULL THEN $
        unit = (*(dp_expcfg[sel_exp]).tgt_mrs.unit)[w_s_tgt] ELSE unit = 'NA'
    END
  ENDCASE

  ; check if the selected substance (dataproc widget) is also found in
  ; tgt_mrs and cal_mrs table. abort if not.
  IF w_s_cal[0] NE -1 AND w_s_tgt[0] NE -1 THEN BEGIN
    match_substs = subst ; override loop over all substances

    ; get parameters: number of measurements, name of each measurement
    n_meas = N_ELEMENTS(dp_expcfg[sel_exp].sequence.id)
    s_names = dp_expcfg[sel_exp].expinfo.s_name
    use_flag = dp_expcfg[sel_exp].expinfo.use
    inv_ix = WHERE(use_flag EQ 0, n_inv)
    s_tgt = s_names[WHERE((dp_expcfg[sel_exp].sequence.id) EQ id_tgt)]
    u_tgt = s_tgt[SORT(s_tgt)]
    u_tgt = u_tgt[uniq(u_tgt)]

    ; define some empty arrays to hold data: mixing ratios and corresponding sample names
    mrs = DBLARR(n_meas, N_ELEMENTS(match_substs))*!VALUES.D_NAN
    mrs_rsd = DBLARR(n_meas, N_ELEMENTS(match_substs))*!VALUES.D_NAN
    tgt_mrs = DBLARR(n_meas, N_ELEMENTS(match_substs))*!VALUES.D_NAN
    tgt_unc_rel = DBLARR(n_meas, N_ELEMENTS(match_substs))*!VALUES.D_NAN
    delta_mrs = DBLARR(n_meas, N_ELEMENTS(match_substs))*!VALUES.D_NAN
    s_names_arr = STRARR(n_meas, N_ELEMENTS(match_substs))

    ; calcualte "linear" target mixing ratios relative to the used cal
    FOR i=0, N_ELEMENTS(match_substs)-1 DO BEGIN ; substances loop

        sel_name = WHERE((dp_chrom[sel_exp])[0].subst.name EQ match_substs[i]) ; substance name

        IF NOT estimate_cal THEN BEGIN
          mrs[*,i]=dp_calc_mrs(match_substs[i], sel_name, sel_exp, dp_chrom, dp_expcfg, eval_mode)
          w_subst=WHERE(*(dp_expcfg[sel_exp]).cal_mrs.substance EQ match_substs[i], n_match_mr)
          cal_MR_spec = (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w_subst[0]]
          cal_MR_err = (*(dp_expcfg[sel_exp]).cal_mrs.unc_ppt)[w_subst[0]]
        ENDIF ELSE BEGIN
          cal_MR_spec = !VALUES.D_NAN
          cal_MR_err = !VALUES.D_NAN
        ENDELSE

        ; generate a corresponding array with "actual" target mixing ratios (as defined)
        w_sel_subst = WHERE(*(dp_expcfg[sel_exp]).tgt_mrs.substance EQ match_substs[i])
        sel_subst_mrs = (*(dp_expcfg[sel_exp]).tgt_mrs.mr_ppt)[w_sel_subst]
        sel_subst_unc = (*(dp_expcfg[sel_exp]).tgt_mrs.unc_rel)[w_sel_subst]
        sel_subst_tgts = (*(dp_expcfg[sel_exp]).tgt_mrs.tgt_name)[w_sel_subst]

        FOR j=0, n_meas-1 DO BEGIN ; targets loop
          w_mr = WHERE(sel_subst_tgts EQ s_names[j], nw)
          IF nw GT 0 THEN BEGIN
            tgt_mrs[j,i] = sel_subst_mrs[w_mr]
            tgt_unc_rel[j,i] = sel_subst_unc[w_mr]
            s_names_arr[j,i] = s_names[j]
          ENDIF
        ENDFOR ; end targets loop

        ; targtes: implement use flag and determine range
        tmp = (tgt_mrs[*,i])
        tmp[WHERE(use_flag EQ 0)] = !VALUES.D_NAN
        (tgt_mrs[*,i]) = TEMPORARY(tmp)

        ; get relative responses of samples (targets)
        sequence = (dp_expcfg[sel_exp]).sequence
        s_ix = (sequence.ix_init_samblock)[WHERE(sequence.ix_init_samblock NE -1)]
        e_ix = (sequence.ix_end_samblock)[WHERE(sequence.ix_end_samblock NE -1)]
        CASE eval_mode OF
          0: samples_rR = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.sam_rrsp)
          1: samples_rR = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.sam_rrsp)
        ENDCASE
        IF n_inv GT 0 THEN samples_rR[inv_ix] = !VALUES.D_NAN

        FOR k=0, N_ELEMENTS(u_tgt)-1 DO BEGIN
          w_tgt = WHERE(s_names_arr[*,i] EQ u_tgt[k], nw)
          IF nw GT 0 THEN $
            mrs_rsd[w_tgt,i] = stddev(samples_rR[w_tgt], /NAN) / mean(samples_rR[w_tgt], /NAN)
        ENDFOR

        delta_mrs[*,i] = mrs[*,i] - tgt_mrs[*,i]

        ; sort stuff out for fitting...
        IF estimate_cal THEN BEGIN
            w_fin = WHERE(FINITE(tgt_mrs[*,i]) EQ 1)
            w_fin_rR = WHERE(FINITE(samples_rR) EQ 1)
            match_fin = arr1D_get_matchIX(w_fin, w_fin_rR)
            w_fin = w_fin[match_fin]
            n_fin = N_ELEMENTS(w_fin)
        ENDIF ELSE $
            w_fin = WHERE(FINITE(delta_mrs[*,i]) EQ 1, n_fin)

        IF n_fin LT 2 THEN status = -1

        sort_ix = SORT((tgt_mrs[*,i])[w_fin])
        tgt_mr_range = [MIN(((tgt_mrs[*,i])[w_fin])[sort_ix], /NAN),MAX(((tgt_mrs[*,i])[w_fin])[sort_ix], /NAN)]

        ; get measurement precision and write to cal_norm_resp variable
        measure_errors = ((mrs_rsd[*,i])[w_fin])[sort_ix]
        invd_ix = WHERE(FINITE(measure_errors) EQ 0, n_invd)
        IF n_invd GT 0 THEN measure_errors[invd_ix] = mean(measure_errors, /NAN)

        cal_norm_resp=[1D, $ ; if relative response is used, cal is 1 by definition
                       mean(measure_errors, /NAN)] ;  + mean((*(dp_expcfg[sel_exp]).tgt_mrs.unc_rel)[w_sel_subst], /NAN)

        ; calculate error estimates for fit: measurement precision + uncertainty given for the targets
        measure_errors = measure_errors+((tgt_unc_rel[*,i])[w_fin])[sort_ix]
        measure_errors[WHERE(measure_errors LT 0.0001)] = 0.0001

        IF estimate_cal THEN BEGIN
          XTITLE = 'relative detector response'
          YTITLE = 'target MR'
          p2_name = 'est_Cal_MR'
          x = ((samples_rR)[w_fin])[sort_ix]
          y=((tgt_mrs[*,i])[w_fin])[sort_ix]
        ENDIF ELSE BEGIN
          XTITLE = 'linear MR'
          YTITLE = 'delta MR'
          p2_name = 'corr. values'
          x=((mrs[*,i])[w_fin])[sort_ix]
          y=((delta_mrs[*,i])[w_fin])[sort_ix]
        ENDELSE

        ; derive mean values, different number of measurements per target are accounted for
        ; through the error of the mean SD/SQRT(n)
        snames=((s_names_arr[*,i])[w_fin])[sort_ix]
        x_means = DBLARR(N_ELEMENTS(u_tgt))*!VALUES.D_NAN
        y_means = DBLARR(N_ELEMENTS(u_tgt))*!VALUES.D_NAN
        tgt_mrs_means = DBLARR(N_ELEMENTS(u_tgt))*!VALUES.D_NAN
        e_means = DBLARR(N_ELEMENTS(u_tgt))*!VALUES.D_NAN
        e_plot = DBLARR(N_ELEMENTS(u_tgt))*!VALUES.D_NAN
        FOR k=0, N_ELEMENTS(u_tgt)-1 DO BEGIN
          w_tgt = WHERE(snames EQ u_tgt[k], nw)
          IF nw GT 0 THEN BEGIN
            x_means[k]=mean(x[w_tgt], /NAN)
            y_means[k]=mean(y[w_tgt], /NAN)
            e_plot[k]=mean(measure_errors[w_tgt], /NAN)
            e_means[k]=mean(measure_errors[w_tgt], /NAN)/SQRT(nw)
          ENDIF
        ENDFOR

  ; +++ MOD
  ;      IF match_substs[0] EQ 'SF6' THEN force_zero = 1
  ;      measure_errors[*] = 0.01D
        w_fin_x=WHERE(FINITE(x_means) EQ 1, n_fin_x)
        IF n_fin_x GT 0 THEN BEGIN
          x = x_means[w_fin_x]
          y = y_means[w_fin_x]
          measure_errors = e_means[w_fin_x]
          e_plot = e_plot[w_fin_x]
        ENDIF
  ; +++

        IF force_zero THEN BEGIN
          x=[0D,x]
          y=[0D,y]
          measure_errors=[0.000001D, measure_errors] ; add a nice 10^-6 error
          e_plot=[0.000001D,e_plot]
          e_means=[0.000001D, e_plot]
        ENDIF


        IF status NE -1 THEN $
            parms = poly_fit(x, y, fct_dgr, MEASURE_ERRORS=measure_errors,$
                             SIGMA=sigma, STATUS=status)


        IF status EQ 0 OR status EQ 2 THEN BEGIN ; polyfit successful completed?

            fit_delta = polyfit_get_ydata(fct_dgr, x, parms)

  ; +++ MOD
  ;        IF match_substs[0] EQ 'N2O' THEN fit_x = interpol([MIN([280D, x], /NAN),MAX(x, /NAN)], N_ELEMENTS(x)) $
  ;          ELSE fit_x = interpol([MIN(x, /NAN),MAX(x, /NAN)], N_ELEMENTS(x))
  ; +++

            fit_x = interpol([MIN(x, /NAN),MAX(x, /NAN)], 200)
            fit_smooth = polyfit_get_ydata(fct_dgr, fit_x, parms)
            dev_to_fit = polyfit_get_ydata(fct_dgr, x, parms) - y
            n_fold_sd = 1D

  ; +++ MOD
  ;        ; derive ascension angle alpha from polyfit parms
  ;        c = SQRT(parms[1]^2 + 1D)
  ;        q = (parms[1]^2)/c
  ;        p = c - q
  ;        h = SQRT(p*q)
  ;        alpha = ATAN(h/p)
  ;        ; from the ascension angel, derive x- and y-offset to fit boundary lines
  ;        alpha = (!DPI/2D) - ABS(alpha)
  ;        b=n_fold_sd*stddev(dev_to_fit,/NAN)
  ;        a = b*TAN(alpha)
  ;        c = SQRT(a^2 + b^2)
  ;        q = a^2/c
  ;        x_off = c - q
  ;        y_off = SQRT(x_off*q)
  ;
  ;        x_bound_0=[MIN(x,ix_min,/NAN)-x_off, MAX(x,ix_max,/NAN)+x_off]
  ;        y_bound_0=polyfit_get_ydata(1, x_bound_0, parms)
  ;        y_bound_0=[y_bound_0[0]-y_off, y_bound_0[1]+y_off]
  ;        x_bound_1=[MIN(x,ix_min,/NAN)+x_off, MAX(x,ix_max,/NAN)-x_off]
  ;        y_bound_1=polyfit_get_ydata(1, x_bound_1, parms)
  ;        y_bound_1=[y_bound_1[0]+y_off, y_bound_1[1]-y_off]
  ;
  ;        p_upper=poly_fit(x_bound_0, y_bound_0, 1)
  ;        p_lower=poly_fit(x_bound_1, y_bound_1, 1)
  ;
  ;        y_upper=polyfit_get_ydata(1, [280D,x_bound_0], p_upper)
  ;        y_lower=polyfit_get_ydata(1, [280D,x_bound_1], p_lower)

  ;        p_upper=poly_fit([MIN(x,ix_min),MAX(x,ix_max)], $
  ;                         [mean(y-fit_delta)+n_fold_sd*stddev(y-fit_delta,/NAN),mean(y-fit_delta)-n_fold_sd*stddev(y-fit_delta,/NAN)], 1)
  ;        p_lower=poly_fit([MIN(x,ix_min),MAX(x,ix_max)], $
  ;                         [mean(y-fit_delta)-n_fold_sd*stddev(y-fit_delta,/NAN),mean(y-fit_delta)+n_fold_sd*stddev(y-fit_delta,/NAN)], 1)
  ;        x_bound_0 = x
  ;        x_bound_1 = x
  ;        y_upper=polyfit_get_ydata(1, [280D,x_bound_0], p_upper)
  ;        y_lower=polyfit_get_ydata(1, [280D,x_bound_1], p_lower)
  ; +++

            w_cal = WHERE((sequence.id) EQ id_cal, n_cal)
            cal_name = s_names[w_cal[0]]

            IF estimate_cal THEN BEGIN
                x_corr = [!VALUES.D_NAN, cal_norm_resp[0], !VALUES.D_NAN]
                y_corr = [!VALUES.D_NAN, polyfit_get_ydata(fct_dgr, cal_norm_resp[0], parms), !VALUES.D_NAN]
                fit_x = interpol([MIN([x,x_corr], /NAN), MAX([x,x_corr], /NAN)], 200)
                fit_smooth = polyfit_get_ydata(fct_dgr, fit_x, parms)

                cal_est_MR = y_corr[1]
                cal_est_sig = y_corr[1]*cal_norm_resp[1]

                IF cal_est_MR LT tgt_mr_range[0] OR cal_est_MR GT tgt_mr_range[1] $
                  THEN calMR_inTGTrange = 0 ELSE calMR_inTGTrange = 1

                e_plot=[!VALUES.D_NAN,cal_est_sig,!VALUES.D_NAN]

                msg = [cal_name, $
                       'estimated Cal MR: ', $
                       STRCOMPRESS(STRING(y_corr[1], FORMAT='(D25.5)'), /REMOVE_ALL),$
                       'abs. precision: ', $
                       STRCOMPRESS(STRING(cal_est_sig, FORMAT='(D25.8)'), /REMOVE_ALL),$
                       'in TGT MR range: '+STRING(calMR_inTGTrange)]

                IF loud THEN !NULL=DIALOG_MESSAGE(msg, /INFORMATION)

            ENDIF ELSE BEGIN
                x_corr = x
                y_corr = (y-fit_delta)
                cal_est_MR = !VALUES.D_NAN
                cal_est_sig = !VALUES.D_NAN
                IF cal_MR_spec LT tgt_mr_range[0] OR cal_MR_spec GT tgt_mr_range[1] $
                  THEN calMR_inTGTrange=0 ELSE calMR_inTGTrange=1
            ENDELSE

            ; fill nl structure, create if first loop iteration
            IF i EQ 0 THEN nl_strct=REPLICATE(create_ref_nl(fct_dgr, N_ELEMENTS(((mrs[*,i])[w_fin])[sort_ix])), N_ELEMENTS(match_substs))
              nl_strct[i].species = match_substs[i]
              nl_strct[i].fct_parms = parms
              nl_strct[i].fct_sigma = sigma
              IF estimate_cal THEN nl_strct[i].max_dev_to_fit = MAX(dev_to_fit)
              IF NOT estimate_cal THEN nl_strct[i].corr_max_dev_to_zero = MAX(ABS(y_corr))
              nl_strct[i].cal_in_tgt_range = calMR_inTGTrange
              nl_strct[i].cal_MR_spec = cal_MR_spec
              nl_strct[i].cal_MR_err = cal_MR_err
              nl_strct[i].cal_est_MR = cal_est_MR
              nl_strct[i].cal_est_sig = cal_est_sig
              nl_strct[i].unit = unit
              nl_strct[i].lin_mrs = ((mrs[*,i])[w_fin])[sort_ix]
              nl_strct[i].lin_mrs_rsd = ((mrs_rsd[*,i])[w_fin])[sort_ix]
              nl_strct[i].tgt_names = ((s_names_arr[*,i])[w_fin])[sort_ix]
              nl_strct[i].tgt_mrs = ((tgt_mrs[*,i])[w_fin])[sort_ix]
              nl_strct[i].delta_mrs = ((delta_mrs[*,i])[w_fin])[sort_ix]

            IF show_plots THEN BEGIN

                min_x=MIN([MIN(x),MIN(fit_x),MIN(x_corr,/NAN)])
                max_x=MAX([MAX(x),MAX(fit_x),MAX(x_corr,/NAN)])
                min_y=MIN([MIN(y),MIN(fit_smooth),MIN(y_corr,/NAN)])
                max_y=MAX([MAX(y),MAX(fit_smooth),MAX(y_corr,/NAN)])
                xrange=[min_x-0.04*(max_x-min_x),max_x+0.04*(max_x-min_x)]
                yrange=[min_y-0.1*(max_y-min_y), max_y+0.1*(max_y-min_y)]

                p0 = plot(x, y, XRANGE=xrange, YRANGE=yrange, $
                          DIMENSIONS=[1400,900], MARGIN=[0.1,0.1,0.1,0.1], $
                          SYMBOL='td',SYM_FILLED=1, $ ;TITLE=match_substs[i]
                          LINESTYLE=6, YTITLE=ytitle, XTITLE=xtitle, $
                          NAME='TGTs (avg.)', SYM_SIZE=2.5)
                p1 = plot(fit_x,fit_smooth,$
                          'r', /OVERPLOT, NAME='fit, dgr: '+STRCOMPRESS(STRING(fct_dgr)))
                p2 = errorplot(x_corr,y_corr,e_plot,$
                          SYMBOL='*',SYM_FILLED=1,SYM_SIZE=3,LINESTYLE=6,$
                          'b', /OVERPLOT, NAME=p2_name, ERRORBAR_COLOR='b')
      ; +++ MOD
      ;          p3 = plot([280D,x_bound_0], y_upper,'g', /OVERPLOT)
      ;          p4 = plot([280D,x_bound_1], y_lower,'g', /OVERPLOT)
      ; +++
                l = legend(TARGET=[p0,p1,p2], /DATA, /AUTO_TEXT_COLOR)
                l.position = [0.99,0.99]

                textfontsize = 14
                t0 = text(0.02,0.95, match_substs[i], TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                t1 = text(0.26,0.95, exp_fname, TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                t2 = text(0.52,0.95, 'Cal: '+cal_name, TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')

                IF estimate_cal THEN BEGIN
                  t3 = text(0.02,0.915, 'est_Cal_MR: '+STRCOMPRESS(STRING(y_corr[1], FORMAT='(D25.4)'), /REMOVE_ALL), $
                                        TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                  t4 = text(0.26,0.915, 'est_MR+-: '+STRCOMPRESS(STRING(cal_est_sig, FORMAT='(D25.4)'), /REMOVE_ALL), $
                                        TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                  t5 = text(0.52,0.915, 'avg_dev_2_fit: '+STRCOMPRESS(STRING(mean(ABS(dev_to_fit)), FORMAT='(D25.4)'), /REMOVE_ALL), $
                                        TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                  t3.FONT_COLOR = 'b'
                  t4.FONT_COLOR = 'b'
                  t5.FONT_COLOR = 'r'
                ENDIF ELSE BEGIN
                  t3 = text(0.02,0.915, 'corrected max dev-2-zero: '+STRCOMPRESS(STRING(MAX(ABS(y_corr), ix_max), FORMAT='(D25.4)'), /REMOVE_ALL), $
                                        TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                  t4 = text(0.26,0.915, '('+STRCOMPRESS(STRING(MAX(ABS(y_corr))/x[ix_max]*100D, FORMAT='(D25.4)'), /REMOVE_ALL)+' % of MR)', $
                                        TARGET=p0, FONT_SIZE=textfontsize, FONT_STYLE='bf')
                  t3.FONT_COLOR = 'b'
                  t4.FONT_COLOR = 'b'
                ENDELSE

                IF KEYWORD_SET(saveplot) THEN BEGIN
                  IF estimate_cal THEN suffix = '_'+match_substs[i]+'_cal_MR'+pic_filetype $
                    ELSE suffix='_'+match_substs[i]+'_NL'+pic_filetype
                  p0.save, saveplot+suffix, resolution=300
                  p0.close
                ENDIF

            ENDIF ; end: show plots

            IF verbose THEN BEGIN
              print, '+++'
              print, match_substs[i], ' fct dgr: ', STRING(fct_dgr)
              print, 'mean delta MR: ', mean(ABS(y))
              print, 'mean, 3-fold sigma fit: ', mean(sigma)*3D
              print, 'mean delta before correction: ', mean(ABS(y))
              print, 'mean delta after correction: ', mean(ABS(y-fit_delta))
              print, 'corrected delta vs. mean MR [%]: ', 100D*mean(ABS(y-fit_delta))/mean(x)
            ENDIF

        ENDIF ELSE BEGIN ; end: fit successful
            IF loud THEN msg=DIALOG_MESSAGE('Fit failed.', /ERROR)
            RETURN, !NULL
        ENDELSE

    ENDFOR ; end substances loop

    RETURN, nl_strct

  ENDIF ELSE BEGIN
      msg = 'No target MRs found for selected species.'
      IF loud THEN !NULL=DIALOG_MESSAGE(msg, /INFORMATION)
      RETURN, !NULL
  ENDELSE

END