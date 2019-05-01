;+
; PRO: call_relresp_calc
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: core routine. gathers data from dp_chrom, interpolates calibration points and
;          calculate relative responses of samples. writes data back into dp_chrom.
;
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, CURRENT=current, $
                          SEL_EXP_SUBST=sel_exp_subst, OVERWRITE=overwrite, $
                          EVAL_MODE=eval_mode, AUTO_SAMTREAT=auto_samtreat, $
                          CALTREATS=caltreats, SAMTREATS=samtreats, CALIPMTHDS=calipmthds, $
                          VERBOSE=verbose


COMMON DP_DATA
COMMON DP_WIDID


  IF NOT KEYWORD_SET(overwrite) THEN overwrite=0
  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  IF NOT KEYWORD_SET(current) THEN current=0 ; 0: try using treatcfg / 1: use settings on the widget
  IF NOT KEYWORD_SET(eval_mode) THEN eval_mode=0 ; default: area
    vd_evm = [0,1] ; valid eval modes

  IF verbose THEN print, 'Initialized calculation of relative responses...'

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  IF NOT KEYWORD_SET(caltreats) THEN BEGIN
    ID_caltreat = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='caltreat_dl'); get valid treat options from droplists
    WIDGET_CONTROL, ID_caltreat, GET_VALUE = vd_caltreat
    w_ne0=WHERE(STRLEN(vd_caltreat) NE 0)
    IF w_ne0[0] NE -1 THEN vd_caltreat=vd_caltreat[w_ne0]
  ENDIF ELSE vd_caltreat=caltreats

  IF NOT KEYWORD_SET(samtreats) THEN BEGIN
    ID_samtreat = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='samtreat_dl')
    WIDGET_CONTROL, ID_samtreat, GET_VALUE = vd_samtreat
    w_ne0=WHERE(STRLEN(vd_samtreat) NE 0)
    IF w_ne0[0] NE -1 THEN vd_samtreat=vd_samtreat[w_ne0]
  ENDIF ELSE vd_samtreat=samtreats

  IF NOT KEYWORD_SET(calipmthds) THEN BEGIN
    ID_calip = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='calip_dl')
    WIDGET_CONTROL, ID_calip, GET_VALUE = vd_calip
    w_ne0=WHERE(STRLEN(vd_calip) NE 0)
    IF w_ne0[0] NE -1 THEN vd_calip=vd_calip[w_ne0]
  ENDIF ELSE vd_calip=calipmthds

  IF KEYWORD_SET(sel_exp_subst) $
;+++++++++
;+++++++++
;+++++++++
  THEN BEGIN ;+++++++++++++++++ call caluculations for selected experiment and substance.

    IF SIZE(sel_exp_subst, /TYPE) NE 3 THEN RETURN ; variable not specified correctly (vector of 2 long integers)

    exp_nbr=sel_exp_subst[0]
    subst_nbr=sel_exp_subst[1]

    tmp_chrom=(dp_chrom[exp_nbr]) ; move dataset out of LIST
    tmp_expcfg=(dp_expcfg[exp_nbr])

    time = (dp_chrom[exp_nbr]).jdate ; gather experiment-specific info
    sequence = (dp_expcfg[exp_nbr]).sequence

    vol = (dp_expcfg[exp_nbr]).expinfo.s_vol

    w0 = WHERE(vol LE 0.)
    IF w0[0] NE -1 THEN vol[w0] = !Values.D_NAN

    id_vector = (dp_expcfg[exp_nbr]).expinfo.s_id

    tmp_chrom.subst[subst_nbr].rres.dp_timestamp = SYSTIME(/JULIAN)

    tmp_chrom.subst[subst_nbr].rres.cal_ip_mthd = cal_ip_mthd[sel_calip];       selection on the processing dialog widget
    tmp_chrom.subst[subst_nbr].rres.cal_treat = cal_treat_mthd[sel_caltreat];   use this as default...

    IF NOT KEYWORD_SET(auto_samtreat) THEN $
      tmp_chrom.subst[subst_nbr].rres.sam_treat = sam_treat_mthd[sel_samtreat] $
    ELSE BEGIN
      name_vector=(dp_expcfg[exp_nbr]).expinfo.s_name
      smpls=name_vector[WHERE(sequence.id EQ id_sam OR sequence.id EQ id_tgt)]
      count=1 ; there is at least one sample...
      FOR s=0, N_ELEMENTS(smpls)-2 DO IF smpls[s+1] NE smpls[s] THEN count=count+1
      IF count GT sequence.n_sam_blocks THEN $
        tmp_chrom.subst[subst_nbr].rres.sam_treat = sam_treat_mthd[1] $
      ELSE tmp_chrom.subst[subst_nbr].rres.sam_treat = sam_treat_mthd[sel_samtreat]
    ENDELSE

    ix_quant = tmp_chrom.subst[subst_nbr].quant
    noise = tmp_chrom.subst[subst_nbr].ires.noise
    height = tmp_chrom.subst[subst_nbr].ires.height
    area = tmp_chrom.subst[subst_nbr].ires.area

    tmp_chrom.subst[subst_nbr].rres.s_n  = height/noise[ix_quant[0],*]

    i_flag = tmp_chrom.subst[subst_nbr].ires.flag ; integrated?

    IF overwrite THEN BEGIN
      use_def = (dp_expcfg[exp_nbr]).expinfo.use          ; overwrite, use defaults from expinfo
      tmp_chrom.subst[subst_nbr].rres.use_flag = use_def  ; overwrite use_flag in rres strct
      ds=MAKE_ARRAY(N_ELEMENTS(tmp_chrom.subst[subst_nbr].rres.data_select), /STRING, VALUE='auto')
      (tmp_chrom.subst[subst_nbr].rres.data_select)=ds
    ENDIF ELSE BEGIN
      use_def = tmp_chrom.subst[subst_nbr].rres.use_flag           ; no overwrite, use eval_flag from rres strct
      ds=MAKE_ARRAY(N_ELEMENTS(tmp_chrom.subst[subst_nbr].rres.data_select), /STRING, VALUE='manual')
      (tmp_chrom.subst[subst_nbr].rres.data_select)=ds
    ENDELSE

    vd_cal = WHERE((use_def EQ 1) AND (i_flag EQ 1) AND (id_vector EQ id_cal), nvd)
    IF nvd LT 1 AND verbose EQ 1 $
        THEN print, ((dp_chrom[exp_nbr]).subst[subst_nbr].name)[0]+': No calibration values found.'

    vd_sam = WHERE((use_def EQ 1) AND (i_flag EQ 1) AND ((id_vector EQ id_sam) OR (id_vector EQ id_tgt)), nvd)
    IF nvd LT 1 AND verbose EQ 1 $
        THEN PRINT, ((dp_chrom[exp_nbr]).subst[subst_nbr].name)[0]+': No sample values found.'

    tmp_chrom.subst[subst_nbr].rres.ha_rat = height/area

    a_to_vol = area/vol
    n_av = a_to_vol/mean(a_to_vol[vd_cal], /DOUBLE, /NAN)
    corr_n_av = DBLARR(N_ELEMENTS(n_av))+!VALUES.D_NAN

    IF (tmp_chrom.subst[subst_nbr].rres.active_corr[0])[0] EQ !TRUE THEN $
      ydata = dp_calc_carryover(n_av, sequence, tmp_expcfg.carryover, (tmp_chrom.subst[subst_nbr].name)[0]) $
    ELSE ydata = n_av

    interpol_a = dp_interpol_cal(time, ydata, vd_cal, sel_calip, sel_caltreat, sequence)
    rres_a = dp_calc_relresp(time, ydata, vd_sam, interpol_a.cal_interpol, sel_samtreat, sequence)

    tmp_chrom.subst[subst_nbr].rres.rsp_area.normalised = n_av
    tmp_chrom.subst[subst_nbr].rres.rsp_area.norm_corrected = corr_n_av
    tmp_chrom.subst[subst_nbr].rres.rsp_area.cal_rsp = interpol_a.cal_interpol
    tmp_chrom.subst[subst_nbr].rres.rsp_area.sam_rrsp = rres_a.sam_rrsp
    tmp_chrom.subst[subst_nbr].rres.rsp_area.block_rrsp = rres_a.block_rrsp
    tmp_chrom.subst[subst_nbr].rres.rsp_area.block_rsd = rres_a.block_rsd
    tmp_chrom.subst[subst_nbr].rres.rsp_area.cal_block_rsd = interpol_a.calblock_rsd

    h_to_vol = height/vol
    n_hv = h_to_vol/mean(h_to_vol[vd_cal], /DOUBLE, /NAN)
    corr_n_hv = DBLARR(N_ELEMENTS(n_hv))+!VALUES.D_NAN

    IF (tmp_chrom.subst[subst_nbr].rres.active_corr[0])[0] EQ !TRUE THEN $
      ydata = dp_calc_carryover(n_hv, sequence, tmp_expcfg.carryover, (tmp_chrom.subst[subst_nbr].name)[0]) $
    ELSE ydata = n_hv

    interpol_h = dp_interpol_cal(time, ydata, vd_cal, sel_calip, sel_caltreat, sequence)
    rres_h = dp_calc_relresp(time, ydata, vd_sam, interpol_h.cal_interpol, sel_samtreat, sequence)

    tmp_chrom.subst[subst_nbr].rres.rsp_height.normalised = n_hv
    tmp_chrom.subst[subst_nbr].rres.rsp_height.norm_corrected = corr_n_hv
    tmp_chrom.subst[subst_nbr].rres.rsp_height.cal_rsp = interpol_h.cal_interpol
    tmp_chrom.subst[subst_nbr].rres.rsp_height.sam_rrsp = rres_h.sam_rrsp
    tmp_chrom.subst[subst_nbr].rres.rsp_height.block_rrsp = rres_h.block_rrsp
    tmp_chrom.subst[subst_nbr].rres.rsp_height.block_rsd = rres_h.block_rsd
    tmp_chrom.subst[subst_nbr].rres.rsp_height.cal_block_rsd = interpol_h.calblock_rsd

    cal_min2max_a = MAX(n_av[interpol_a.ix_vd_cals])-MIN(n_av[interpol_a.ix_vd_cals])
    cal_min2max_h = MAX(n_hv[interpol_h.ix_vd_cals])-MIN(n_hv[interpol_h.ix_vd_cals])
    tmp_chrom.subst[subst_nbr].rres.rsp_area.cal_mintomax = cal_min2max_a
    tmp_chrom.subst[subst_nbr].rres.rsp_height.cal_mintomax = cal_min2max_h
    tmp_chrom.subst[subst_nbr].rres.rsp_area.cal_devtofit = interpol_a.cal_devtofit
    tmp_chrom.subst[subst_nbr].rres.rsp_height.cal_devtofit = interpol_h.cal_devtofit

    tmp_chrom.subst[subst_nbr].rres.rsp_select = eval_mode

    (dp_chrom[exp_nbr])=tmp_chrom ; put dataset back into LIST
    IF ((dp_expcfg[exp_nbr]).instr_prc.instrument) NE '' THEN $
      dp_apply_instrprc, exp_nbr, VERBOSE=verbose

;+++++++++
;+++++++++
;+++++++++
  ENDIF ELSE BEGIN ;+++++++++++++++++ call caluculations for all loaded experiments

    n_exp = N_ELEMENTS(dp_chrom)
    n_subst = LONARR(n_exp)
    FOR i=0, n_exp-1 DO n_subst[i]=N_ELEMENTS(((dp_chrom[i])[0].subst.name))
  ;++++++++++++
    FOR sel_exp=0, n_exp-1 DO BEGIN ; loop over all loaded experiments
      tmp_chrom=(dp_chrom[sel_exp]) ; move dataset out of LIST
      tmp_expcfg=(dp_expcfg[sel_exp])

      time = (dp_chrom[sel_exp]).jdate ; gather experiment-specific info
      sequence = (dp_expcfg[sel_exp]).sequence
      vol = (dp_expcfg[sel_exp]).expinfo.s_vol
      id_vector = (dp_expcfg[sel_exp]).expinfo.s_id
  ;+++++++++
      FOR sel_subst=0, n_subst[sel_exp]-1 DO BEGIN ; loop over all substances in loaded experiments

        tmp_chrom.subst[sel_subst].rres.dp_timestamp = SYSTIME(/JULIAN)

        tmp_chrom.subst[sel_subst].rres.cal_ip_mthd = cal_ip_mthd[sel_calip]
        tmp_chrom.subst[sel_subst].rres.cal_treat = cal_treat_mthd[sel_caltreat]
        tmp_chrom.subst[sel_subst].rres.sam_treat = sam_treat_mthd[sel_samtreat]

        IF NOT current THEN BEGIN ; try to find settings for the selected substance in dp_expcfg.treatcfg
          pvd=PTR_VALID((tmp_expcfg.treatcfg.substance)) ; check pointer...
          IF *(tmp_expcfg.treatcfg.substance) EQ !NULL THEN p_filled = 0 ELSE p_filled = 1
          IF pvd AND p_filled THEN BEGIN
            w=WHERE(STRUPCASE(*(tmp_expcfg.treatcfg.substance)) EQ STRUPCASE((substlist[sel_exp])[sel_subst]))

            IF w[0] NE -1 THEN BEGIN
              ix_cal_ip=WHERE(vd_calip EQ ((*(tmp_expcfg.treatcfg.cal_ip))[w])[0])
              IF ix_cal_ip[0] NE -1 THEN BEGIN
                tmp_chrom.subst[sel_subst].rres.cal_ip_mthd = vd_calip[ix_cal_ip[0]]
                sel_calip = ix_cal_ip[0]
              ENDIF

              ix_cal_treat=WHERE(vd_caltreat EQ ((*(tmp_expcfg.treatcfg.cal_treat))[w])[0])
              IF ix_cal_treat[0] NE -1 THEN BEGIN
                tmp_chrom.subst[sel_subst].rres.cal_treat = vd_caltreat[ix_cal_treat[0]]
                sel_caltreat = ix_cal_treat[0]
              ENDIF

              ix_sam_treat=WHERE(vd_samtreat EQ ((*(tmp_expcfg.treatcfg.sam_treat))[w])[0])
              IF ix_sam_treat[0] NE -1 THEN BEGIN
                tmp_chrom.subst[sel_subst].rres.sam_treat = vd_samtreat[ix_sam_treat[0]]
                sel_samtreat = ix_sam_treat[0]
              ENDIF

              ix_evm=WHERE(vd_evm EQ ((*(tmp_expcfg.treatcfg.eval_mode))[w])[0])
              IF ix_sam_treat[0] NE -1 THEN $
                eval_mode = ((*(tmp_expcfg.treatcfg.eval_mode))[w])[0]

            ENDIF ; matching
          ENDIF ; pointer valid
        ENDIF ; current keyword set


        IF KEYWORD_SET(auto_samtreat) THEN BEGIN
          name_vector=(dp_expcfg[sel_exp]).expinfo.s_name
          smpls=name_vector[WHERE(sequence.id EQ id_sam OR sequence.id EQ id_tgt)]
          vd_smpls=smpls[WHERE((dp_expcfg[sel_exp]).expinfo.use EQ 1)]
          count=1 ; there is at least one sample...
          FOR s=0, N_ELEMENTS(vd_smpls)-2 DO IF vd_smpls[s+1] NE vd_smpls[s] THEN count=count+1
          IF count GT sequence.n_sam_blocks THEN BEGIN
            sel_samtreat = 1
            tmp_chrom.subst[sel_subst].rres.sam_treat = vd_samtreat[sel_samtreat]
          ENDIF
        ENDIF


        ix_quant = tmp_chrom.subst[sel_subst].quant
        noise = tmp_chrom.subst[sel_subst].ires.noise
        height = tmp_chrom.subst[sel_subst].ires.height
        area = tmp_chrom.subst[sel_subst].ires.area

        tmp_chrom.subst[sel_subst].rres.s_n  = height /noise[ix_quant[0],*]

        i_flag = tmp_chrom.subst[sel_subst].ires.flag ; integrated?

        IF overwrite THEN BEGIN
          use_def = (dp_expcfg[sel_exp]).expinfo.use          ; overwrite, use defaults from expinfo
          tmp_chrom.subst[sel_subst].rres.use_flag = use_def  ; overwrite use_flag in rres strct
          ds=MAKE_ARRAY(N_ELEMENTS(tmp_chrom.subst[sel_subst].rres.data_select), /STRING, VALUE='auto')
          (tmp_chrom.subst[sel_subst].rres.data_select)=ds ; lable data as 'autoselect'
        ENDIF ELSE BEGIN
          use_def = tmp_chrom.subst[sel_subst].rres.use_flag           ; no overwrite, use eval_flag from rres strct
          ds=MAKE_ARRAY(N_ELEMENTS(tmp_chrom.subst[sel_subst].rres.data_select), /STRING, VALUE='manual')
          (tmp_chrom.subst[sel_subst].rres.data_select)=ds
        ENDELSE

        vd_cal = WHERE((use_def EQ 1) AND (i_flag GE 1) AND (id_vector EQ id_cal), nvd)
        IF nvd LT 1 AND verbose EQ 1 $
          THEN PRINT, ((dp_chrom[sel_exp]).subst[sel_subst].name)[0]+': No calibration points found.'

        vd_sam = WHERE((use_def EQ 1) AND (i_flag GE 1) AND ((id_vector EQ id_sam) OR (id_vector EQ id_tgt)), nvd)
        IF nvd LT 1 AND verbose EQ 1 $
          THEN PRINT, ((dp_chrom[sel_exp]).subst[sel_subst].name)[0]+': No sample points found.'

        tmp_chrom.subst[sel_subst].rres.ha_rat = height/area

        a_to_vol = area/vol
        n_av = a_to_vol/mean(a_to_vol[vd_cal], /DOUBLE, /NAN)
        corr_n_av = DBLARR(N_ELEMENTS(n_av))+!VALUES.D_NAN

        IF (tmp_chrom.subst[sel_subst].rres.active_corr[0])[0] EQ !TRUE THEN $
          ydata = dp_calc_carryover(n_av, sequence, tmp_expcfg.carryover, (tmp_chrom.subst[sel_subst].name)[0]) $
        ELSE ydata = n_av

        interpol_a = dp_interpol_cal(time, ydata, vd_cal, sel_calip, sel_caltreat, sequence)
        rres_a = dp_calc_relresp(time, ydata, vd_sam, interpol_a.cal_interpol, sel_samtreat, sequence)

        tmp_chrom.subst[sel_subst].rres.rsp_area.normalised = n_av
        tmp_chrom.subst[sel_subst].rres.rsp_area.norm_corrected = corr_n_av
        tmp_chrom.subst[sel_subst].rres.rsp_area.cal_rsp = interpol_a.cal_interpol
        tmp_chrom.subst[sel_subst].rres.rsp_area.sam_rrsp = rres_a.sam_rrsp
        tmp_chrom.subst[sel_subst].rres.rsp_area.block_rrsp = rres_a.block_rrsp
        tmp_chrom.subst[sel_subst].rres.rsp_area.block_rsd = rres_a.block_rsd
        tmp_chrom.subst[sel_subst].rres.rsp_area.cal_block_rsd = interpol_a.calblock_rsd

        h_to_vol = height/vol
        n_hv = h_to_vol/mean(h_to_vol[vd_cal], /DOUBLE, /NAN)
        corr_n_hv = DBLARR(N_ELEMENTS(n_hv))+!VALUES.D_NAN

        IF (tmp_chrom.subst[sel_subst].rres.active_corr[0])[0] EQ !TRUE THEN $
          ydata = dp_calc_carryover(n_hv, sequence, tmp_expcfg.carryover, (tmp_chrom.subst[sel_subst].name)[0]) $
        ELSE ydata = n_hv

        interpol_h = dp_interpol_cal(time, ydata, vd_cal, sel_calip, sel_caltreat, sequence)
        rres_h = dp_calc_relresp(time, ydata, vd_sam, interpol_h.cal_interpol, sel_samtreat, sequence)

        tmp_chrom.subst[sel_subst].rres.rsp_height.normalised = n_hv
        tmp_chrom.subst[sel_subst].rres.rsp_height.norm_corrected = corr_n_hv
        tmp_chrom.subst[sel_subst].rres.rsp_height.cal_rsp = interpol_h.cal_interpol
        tmp_chrom.subst[sel_subst].rres.rsp_height.sam_rrsp = rres_h.sam_rrsp
        tmp_chrom.subst[sel_subst].rres.rsp_height.block_rrsp = rres_h.block_rrsp
        tmp_chrom.subst[sel_subst].rres.rsp_height.block_rsd = rres_h.block_rsd
        tmp_chrom.subst[sel_subst].rres.rsp_height.cal_block_rsd = interpol_h.calblock_rsd

        cal_min2max_a = MAX(n_av[interpol_a.ix_vd_cals])-MIN(n_av[interpol_a.ix_vd_cals])
        cal_min2max_h = MAX(n_hv[interpol_h.ix_vd_cals])-MIN(n_hv[interpol_h.ix_vd_cals])
        tmp_chrom.subst[sel_subst].rres.rsp_area.cal_mintomax = cal_min2max_a
        tmp_chrom.subst[sel_subst].rres.rsp_height.cal_mintomax = cal_min2max_h
        tmp_chrom.subst[sel_subst].rres.rsp_area.cal_devtofit = interpol_a.cal_devtofit
        tmp_chrom.subst[sel_subst].rres.rsp_height.cal_devtofit = interpol_h.cal_devtofit

        tmp_chrom.subst[sel_subst].rres.rsp_select = eval_mode

      ENDFOR ; end loop: substances in loaded experiments
  ;+++++++++
      (dp_chrom[sel_exp])=tmp_chrom ; put dataset back into LIST
      IF ((dp_expcfg[sel_exp]).instr_prc.instrument) NE '' THEN $
        dp_apply_instrprc, sel_exp, VERBOSE=verbose

    ENDFOR ; end loop: loaded experiments
  ;++++++++++++

  ENDELSE

  IF verbose THEN print, 'Calculation of relative responses done.'

END