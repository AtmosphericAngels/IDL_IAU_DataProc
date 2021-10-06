;+
; PRO: dp_plot_rres_diag
;
; AUTHOR: S.Sala
; MODIFIED: F. Obersteiner, Sep-2016
; MODIFIED: T. Schuck, Feb-2016

;
; PURPOSE: generate data for response plot and diagnostics plot.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_plot_rres_diag, sel_exp, sel_subst, DIAGNOSTIC=diagnostic, RRES=rres, SAVEPATH=savepath, $
                                           PIC_FILETYPE=pic_filetype
; diagnostic = 0 (none), 1 (normalised ret. time, height/area and S/N), 2 (delta t enrich, enriched Volume)


  IF NOT KEYWORD_SET(rres) THEN rres = 0
  IF NOT KEYWORD_SET(diagnostic) THEN diagnostic = 0
  IF NOT KEYWORD_SET(pic_filetype) THEN pic_filetype = '.ps'

  COMMON dp_data

  eval_mode = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]
  i_flag = (dp_chrom[sel_exp]).subst[sel_subst].ires.flag ; integration flag
  r_flag = (dp_chrom[sel_exp]).subst[sel_subst].rres.use_flag ; results flag
  s_name = (dp_expcfg[sel_exp]).expinfo.s_name
  s_id   = (dp_expcfg[sel_exp]).expinfo.s_id
  time = (dp_chrom[sel_exp]).jdate
  name = ((dp_chrom[sel_exp]).subst[sel_subst].name)[0]
  exp_name = file_basename((dp_chrom[sel_exp].exp_fname)[0])
  title = exp_name+': '+name + " (" + ((dp_chrom[sel_exp]).subst[sel_subst].formula)[0] + ")"

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  vd_data= WHERE(i_flag GE 1 AND r_flag EQ 1)
  vd_sam = WHERE(s_id EQ id_sam[0] AND i_flag GE 1 AND r_flag NE 0) ; r_flag = 0 -> not evaluated
  vd_tgt = WHERE(s_id EQ id_tgt[0] AND i_flag GE 1 AND r_flag NE 0)
  vd_cal = WHERE(s_id EQ id_cal[0] AND i_flag GE 1 AND r_flag NE 0)

  n_av = (dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.normalised
  n_hv = (dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.normalised


  CASE eval_mode OF
    0 : BEGIN
          signal = n_av    ; response, normalised to cal measurements' response
          cal_interpol = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.cal_rsp)[vd_data]
        END
    1 : BEGIN
          signal = n_hv
          cal_interpol = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.cal_rsp)[vd_data]
        END
  ENDCASE

  t_ret = (dp_chrom[sel_exp]).subst[sel_subst].ires.rt
  s_n = (dp_chrom[sel_exp]).subst[sel_subst].rres.s_n

  mp = REPLICATE(!VALUES.D_NAN, N_ELEMENTS(time))
  IF ((dp_expcfg[sel_exp]).instr_prc.instrument) NE '' AND *(dp_expcfg[sel_exp]).instr_prc.substance NE !NULL $
    THEN BEGIN
      w = WHERE(*(dp_expcfg[sel_exp]).instr_prc.substance EQ name)
      IF w[0] NE -1 THEN $
        mp[*]=(*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w]
    ENDIF

  mean_cal=mean(signal[vd_cal], /NAN)

  t_ret = (dp_chrom[sel_exp]).subst[sel_subst].ires.rt
  s_n = (dp_chrom[sel_exp]).subst[sel_subst].rres.s_n


  vol = (dp_expcfg[sel_exp]).expinfo.s_vol
  mfc_vol = (dp_expcfg[sel_exp]).expinfo.mfc_vol
  delta_t = (dp_expcfg[sel_exp]).expinfo.dt *86400


;++++++++++++++ relative response plotten
  IF rres THEN BEGIN

    IF vd_data[0] NE -1 THEN BEGIN
      p = dp_modify_plot(p = p)
      p.p1.title = title
      skip_save = 0
    ENDIF ELSE BEGIN
;      msg=DIALOG_MESSAGE(exp_name+': Please run calculations first.', /INFORMATION)
      skip_save = 1
    ENDELSE

    ; Cal-Messungen, welche zur Auswertung verwendet werden
    IF vd_cal[0] NE -1 THEN BEGIN
      x1 = time[vd_cal]
      y1=signal[vd_cal];/mean(signal[vd_cal], /nan)
      p = dp_modify_plot(p=p, x1=x1, y1=y1)
    ENDIF

    ; Sample - Messungen, welche ausgewertet werden
    IF vd_sam[0] NE -1 THEN BEGIN
      x2 = time[vd_sam]
      y2=signal[vd_sam];/mean(signal[vd_cal],/nan)
      yerr2 = mp[vd_sam]
      p = dp_modify_plot(p=p, x2=x2, y2=y2, yerr2=yerr2)
    ENDIF

    ; Target-Messungen
    IF vd_tgt[0] NE -1 THEN BEGIN
      x4 = time[vd_tgt]
      y4=signal[vd_tgt];/mean(signal[vd_cal],/nan)
      yerr4 = mp[vd_tgt]
      p = dp_modify_plot(p=p, x4=x4, y4=y4, yerr4=yerr4)
    ENDIF

    ; cal interpolation
    IF vd_data[0] NE -1 THEN BEGIN
      x7 = time[vd_data]
      y7 = cal_interpol
      p = dp_modify_plot(p=p, x7=x7, y7=y7)
    ENDIF

    dp_refr_status, MESSAGE='Created results plot.'

    IF KEYWORD_SET(savepath) AND NOT skip_save THEN BEGIN
      p.p1.save, savepath+'\'+exp_name+'_'+name+pic_filetype, resolution=300
      p.p1.close
    ENDIF
  ENDIF

;++++++++++++++ diagnostic plot 1
  IF diagnostic EQ 1 THEN BEGIN

    IF SIZE(p2,/type) EQ 8 THEN p2.p1.refresh,/disable

    ; Retentionszeit plotten
    x1 = INDGEN(N_ELEMENTS(time))+1
    y1 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y1[vd_data] = (t_ret[vd_data]/mean(t_ret[vd_data],/NAN))

    p2=dp_modify_plot2_multi(p2=p2, x1=x1,y1=y1 )
    p2.p1.title = "Diagnostic plot for: " + title


    ; Height vs. Area plotten
    x2 = x1
    y2 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y2[vd_data] = (n_hv/n_av)[vd_data]

    p2.p2.select
    p2=dp_modify_plot2_multi(p2=p2,x2=x2,y2=y2)


    ; S/N plotten
    x3 = x1
    y3 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y3[vd_data] = s_n[vd_data]

    p2.p3.select
    p2=dp_modify_plot2_multi(p2=p2,x3=x3,y3=y3)
    p2.p1.refresh

    dp_refr_status, MESSAGE='Created diagnostics plot.'

  ENDIF

  ;++++++++++++++ diagnostic plot 2
  IF diagnostic EQ 2 THEN BEGIN

    IF SIZE(p2,/type) EQ 8 THEN p2.p1.refresh,/disable

    p2=dp_modify_plot2_multi( ytitle1= "t precon", ytitle2= "dp Vol", ytitle3='MFC Vol')


    ; Retentionszeit plotten
    x1 = INDGEN(N_ELEMENTS(time))+1
    y1 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y1[vd_data] = delta_t[vd_data]

    p2=dp_modify_plot2_multi(p2=p2, x1=x1,y1=y1 )
    p2.p1.title = "Diagnostic info from: " + dp_expcfg[sel_exp].expinfo[0].expinfo_fname


    ; delta t enrich plotten
    x2 = x1
    y2 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y2[vd_data] = vol[vd_data]

    p2.p2.select
    p2=dp_modify_plot2_multi(p2=p2,x2=x2,y2=y2)


    ; enrich volume (p difference) plotten
    x3 = x1
    y3 = DBLARR(N_ELEMENTS(time))*!Values.D_NAN
    y3[vd_data] = mfc_vol[vd_data]

    p2.p3.select
    p2=dp_modify_plot2_multi(p2=p2,x3=x3,y3=y3)
    p2.p1.refresh

    dp_refr_status, MESSAGE='Created diagnostics plot.'

  ENDIF

END