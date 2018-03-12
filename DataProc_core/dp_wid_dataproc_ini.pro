;+
; PRO: dp_wid_dataproc_handle/dp_wid_dataproc_ini
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: dialog window to configure calculations / handle the core features of the software (rR calculation etc.)
;-
;------------------------------------------------------------------------------------------------------------------------
@dp_wid_dataproc_upd
@dp_plot_tools
;------------------------------------------------------------------------------------------------------------------------
PRO dp_wid_dataproc_handle, event

  COMMON DP_DATA
  COMMON DP_WIDID
  
  IF error_handler_IO EQ 1 THEN BEGIN
    CATCH, Error_status
    IF Error_status NE 0 THEN BEGIN
      e_ix = '(!) Error '+STRING(Error_status)
      e_msg = ', '+STRING(!ERROR_STATE.MSG)
      msg = STRCOMPRESS(STRTRIM(e_ix+e_msg+' Returning.'))
      dlg = DIALOG_MESSAGE(msg, /ERROR)
      CATCH, /CANCEL
      dp_refr_status, MESSAGE='Returned from error.'
      RETURN
    ENDIF
  END
  
  ID_caltreat   = WIDGET_INFO(event.top, FIND_BY_UNAME='caltreat_dl')
  sel_caltreat  = WIDGET_INFO(ID_caltreat, /DROPLIST_SELECT)
  ID_samtreat   = WIDGET_INFO(event.top, FIND_BY_UNAME='samtreat_dl')
  sel_samtreat  = WIDGET_INFO(ID_samtreat, /DROPLIST_SELECT)
  ID_calip      = WIDGET_INFO(event.top, FIND_BY_UNAME='calip_dl')
  sel_calip     = WIDGET_INFO(ID_calip, /DROPLIST_SELECT)
  ID_experiment = WIDGET_INFO(event.top, FIND_BY_UNAME='exp_dl')
  sel_exp       = WIDGET_INFO(ID_experiment, /DROPLIST_SELECT)
  ID_substance  = WIDGET_INFO(event.top, FIND_BY_UNAME='subst_dl')
  sel_subst     = WIDGET_INFO(ID_substance, /DROPLIST_SELECT)
  ID_eval_mode  = WIDGET_INFO(event.top, FIND_BY_UNAME='eval_mode')
  eval_mode     = WIDGET_INFO(ID_eval_mode, /DROPLIST_SELECT)
  ID_ovwr       = WIDGET_INFO(event.top, FIND_BY_UNAME='ovwr')
  ovwr          = WIDGET_INFO(ID_ovwr, /BUTTON_SET)
  ID_ovwr_sel   = WIDGET_INFO(event.top, FIND_BY_UNAME='ovwr_sel')
  ovwr_sel      = WIDGET_INFO(ID_ovwr_sel, /BUTTON_SET)
  ID_espec      = WIDGET_INFO(dp_widid.dp_mainwid, find_by_UNAME='expspec_dl')
  sel_espec     = WIDGET_INFO(ID_espec, /DROPLIST_SELECT)
  
  ID_instr = WIDGET_INFO(dp_widid.dp_mainwid, FIND_BY_UNAME='instr_dl')
  WIDGET_CONTROL, ID_instr, GET_VALUE=instruments
  instrument = instruments[WIDGET_INFO(ID_instr, /DROPLIST_SELECT)]
  
  UNAME = WIDGET_INFO(event.id, /UNAME)
  
  verbose = 0 
    
  CASE UNAME OF   
    ;**********************************### DROPLISTS/BUTTONS ###******************************* 
    'exp_dl' : dp_wid_dataproc_upd, event 
    ;*****************************    
    'subst_dl' : dp_wid_dataproc_upd, event 
    ;*****************************
    'eval_mode' : eval_mode = WIDGET_INFO(ID_eval_mode, /DROPLIST_SELECT)
    ;*****************************
    'caltreat_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_caltreat, GET_VALUE=dl_val
        IF dl_val[sel_caltreat] EQ '' THEN BEGIN
          !NULL=DIALOG_MESSAGE('Please select a valid option.')
          sel_caltreat=0
          WIDGET_CONTROL, ID_caltreat, SET_DROPLIST_SELECT=sel_caltreat
        ENDIF
      END
    ;*****************************
    'samtreat_dl' : $
        BEGIN
        WIDGET_CONTROL, ID_samtreat, GET_VALUE=dl_val
        IF dl_val[sel_samtreat] EQ '' THEN BEGIN
          !NULL=DIALOG_MESSAGE('Please select a valid option.')
          sel_samtreat=0
          WIDGET_CONTROL, ID_samtreat, SET_DROPLIST_SELECT=sel_caltreat
        ENDIF
      END
    ;*****************************      
    'calip_dl' :
    ;*****************************      
    'edit_useflag' : $
      BEGIN
        dp_sel_meas_table_ini, sel_exp, sel_subst
        WIDGET_CONTROL, ID_ovwr, SET_BUTTON = 0
      END
    ;*****************************     
    'calc_individual' : $
      BEGIN
        IF sel_espec NE 3 THEN $
          dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, $
                                sel_exp_subst=[sel_exp,sel_subst], OVERWRITE=ovwr_sel, $
                                /CURRENT, EVAL_MODE=eval_mode, VERBOSE=verbose
          dp_wid_dataproc_upd, event
          dp_refr_status, MESSAGE='Ran calculations: selected data.'
      END
    ;*****************************     
    'show_restable_brief' : dp_show_restable, sel_exp, sel_subst, /BRIEF
    ;*****************************   
    'show_resplot' : dp_plot_rres_diag, sel_exp, sel_subst, /RRES  
    ;*****************************    
    'calc_all' : $
      BEGIN
        quest=DIALOG_MESSAGE('Use loaded cal/sample treatment config if available?', /QUESTION)
        IF quest EQ 'Yes' THEN current=0 ELSE current=1
        dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, OVERWRITE=ovwr, $
                              CURRENT=current, EVAL_MODE=eval_mode, VERBOSE=verbose
        dp_wid_dataproc_upd, event
        dp_refr_status, MESSAGE='Run calculations: all loaded data.'
      END
    ;*****************************
    ;*********************************### MENU: Config ###*************************************
    'load_prc' : $
      BEGIN
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Precision: Load for selected experiment only?', /QUESTION, /CANCEL)
          IF quest EQ 'No' THEN sel_only=0
          IF quest EQ 'Cancel' THEN RETURN
        ENDIF
        prc_strct=dp_read_instrprc(sel_exp, chromlist[sel_exp], substlist, $
                                   PATH=path_wd, VERBOSE=verbose)
        IF prc_strct EQ !NULL THEN RETURN
        dp_apply_instrprc, sel_exp, SEL_ONLY=sel_only, PRC_STRCT=prc_strct, VERBOSE=verbose
      END
    ;*****************************
    'load_mr' : $
      BEGIN
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Cal MRs: Load for selected experiment only?', /QUESTION, /CANCEL)
          IF quest EQ 'No' THEN sel_only=0
          IF quest EQ 'Cancel' THEN RETURN
        ENDIF
        mrs_strct=dp_read_calmrs(PATH=path_wd, VERBOSE=verbose)
        IF mrs_strct EQ !NULL THEN RETURN
        dp_apply_calmrs, sel_exp, SEL_ONLY=sel_only, mrs_strct, PATH=path_wd, VERBOSE=verbose
      END
    ;*****************************
    'load_mr_tgt' : $
      BEGIN
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Target MRs: Load for selected experiment only?', /QUESTION, /CANCEL)
          IF quest EQ 'No' THEN sel_only=0
          IF quest EQ 'Cancel' THEN RETURN
        ENDIF
        tgt_strct=dp_read_tgtmrs(PATH=path_wd, VERBOSE=verbose)
        IF tgt_strct EQ !NULL THEN RETURN       
        dp_apply_tgtmrs, sel_exp, SEL_ONLY=sel_only, tgt_strct, PATH=path_wd, VERBOSE=verbose
      END
    ;*****************************
    'load_treatcfg' : $
      BEGIN
        sel_only=1
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Load for selected experiment only?', /QUESTION, /CANCEL)
          IF quest EQ 'No' THEN sel_only=0
          IF quest EQ 'Cancel' THEN RETURN
        ENDIF
        dp_read_treatcfg, sel_exp, SEL_ONLY=sel_only, VERBOSE=verbose
        dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, VERBOSE=verbose
        dp_wid_dataproc_upd, event
      END
    ;*****************************
    'load_co_corr' : $
      BEGIN
        sel_only=1
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Load for selected experiment only?', /QUESTION, /CANCEL)
          IF quest EQ 'No' THEN sel_only=0
          IF quest EQ 'Cancel' THEN RETURN
        ENDIF
        dp_read_cocorrparms, sel_exp, SEL_ONLY=sel_only, PATH=path_wd, $
                            DEF_FILE=def_file, VERBOSE=verbose
        dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, VERBOSE=verbose
        dp_wid_dataproc_upd, event
      END
    ;*****************************
    'reset_prc' : dp_remv_instrprc, sel_exp, SEL_ONLY=sel_only, /LOUD
    ;*****************************
    'reset_mr' : dp_remv_calmrs, sel_exp, SEL_ONLY=sel_only, /LOUD
    ;*****************************
    'reset_mr_tgt' : dp_remv_tgtmrs, sel_exp, SEL_ONLY=sel_only, /LOUD
    ;*****************************
    'reset_treatcfg' : $
      BEGIN
        sel_only=1
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          quest=DIALOG_MESSAGE('Remove for selected experiment only?', /QUESTION)
          IF quest EQ 'No' THEN sel_only=0
        ENDIF
        remv=dp_remv_treatcfg(sel_exp, SEL_ONLY=sel_only, /LOUD)
        IF remv THEN BEGIN
          dp_call_relresp_calc, 0, 0, 0, VERBOSE=verbose
          dp_wid_dataproc_upd, event
        ENDIF
      END
    ;*****************************
    'reset_co_corr' : $
      BEGIN
        remv=dp_remv_cocorr(sel_exp, /LOUD)
        IF remv THEN BEGIN
          dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, VERBOSE=verbose
          dp_wid_dataproc_upd, event
        ENDIF
      END
    ;*****************************
    'exp_treatcfg' : dp_export_treatconfig, sel_exp, PATH=path_wd
    ;*****************************
    ;******************************### MENU: Diagnostics ###***********************************
    'show_restable' : dp_show_restable, sel_exp, sel_subst
    ;*****************************      
    'show_diaplot' : dp_plot_rres_diag, sel_exp, sel_subst, DIAGNOSTIC=1
    ;*****************************
    'show_diaplot2' : dp_plot_rres_diag, sel_exp, sel_subst, DIAGNOSTIC=2
    ;*****************************
    'show_prcplot' : dp_plot_mrs_prc, sel_exp, sel_subst, /SHOW_PRC
    ;*****************************
    'show_mrsplot' : dp_plot_mrs_prc, sel_exp, sel_subst, /SHOW_MRs  
    ;*****************************
    ;**********************************### MENU: Show ###**************************************
    'show_prctbl' : dp_show_prctable, sel_exp
    ;***************************** 
    'show_mrtbl' : dp_show_mrtable, sel_exp
    ;*****************************     
    'show_tgtmrtbl' : dp_show_tgtmrtable, sel_exp
    ;*****************************      
    'show_treattbl' : dp_show_treatcfgtable, sel_exp
    ;*****************************
    'show_co_def_tbl' : dp_show_carryover_def, sel_exp
    ;*****************************
    'show_svolselect' : $
      BEGIN
        s_vol_select=((dp_expcfg[sel_exp]).expinfo.s_vol_select)[0]
        s_vol_origins=['Pressure Difference', 'Mass Flow Controller']
        info='Sample volume determined by: '+s_vol_origins[s_vol_select]
        !NULL=DIALOG_MESSAGE(info, /INFORMATION)
      END
    ;*****************************
    ;**********************************### MENU: Tools ###*************************************
    'chg_substnames' : $
      BEGIN
        dp_replace_substnames, PATH=path_wd
        dp_wid_dataproc_upd, event
      END
    ;***************************** 
    'ana_mp' : $
      BEGIN
        mpexp_cfg = dp_prcexp_analyse_seq(sel_exp, dp_expcfg, sid_name, VERBOSE=verbose)
        dp_prcexp_wid_ini,  mpexp_cfg   
      END
    ;*****************************
    'ana_nl' : dp_nlexp_wid_ini
    ;*****************************
    ;**********************************### MENU: Report ###************************************
    'rep_all' : dp_res2txt, sel_exp, sel_subst, PATH=path_wd, /ALL, /BRIEF, VERBOSE=verbose   
    ;*****************************
    'rep_sel' : dp_res2txt, sel_exp, sel_subst, PATH=path_wd, /BRIEF, VERBOSE=verbose
    ;*****************************
    'rep_sel_det' : dp_res2txt, sel_exp, sel_subst, PATH=path_wd, VERBOSE=verbose
    ;*****************************
    'rep_exp_mean' : dp_mean2txt, sel_exp, PATH=path_wd
    ;*****************************
    'rep_subst_list' : dp_report_list, sel_exp, PATH=path_wd
    ;*****************************
    ELSE:
  ENDCASE
END

;#####################################################################################################################

PRO dp_wid_dataproc_ini

  COMMON DP_DATA
  COMMON DP_WIDID
  

;++++++++++++ WIDGET BASE
  dpbase=WIDGET_BASE(title='Processing Dialog', MBAR=dp_men, COLUMN=1, /BASE_ALIGN_CENTER, $
                     XOFFSET=5, YOFFSET=330)


;++++++++++++ MENUS
  cnfgID=WIDGET_BUTTON(dp_men, VALUE='Config', /MENU)
    load_sub=WIDGET_BUTTON(cnfgID, VALUE='Apply...', /MENU)
      ID=WIDGET_BUTTON(load_sub, VALUE='Precision Def.', UNAME='load_prc')
      ID=WIDGET_BUTTON(load_sub, VALUE='CAL Mixing Ratios', UNAME='load_mr')
      ID=WIDGET_BUTTON(load_sub, VALUE='TGT Mixing Ratios', UNAME='load_mr_tgt')
      ID=WIDGET_BUTTON(load_sub, VALUE='Treatment Config', UNAME='load_treatcfg')
      ID=WIDGET_BUTTON(load_sub, VALUE='Carry-Over Config', UNAME='load_co_corr')
    rset_sub=WIDGET_BUTTON(cnfgID, VALUE='Reset...', /MENU)
      ID=WIDGET_BUTTON(rset_sub, VALUE='Precision Def.', UNAME='reset_prc')
      ID=WIDGET_BUTTON(rset_sub, VALUE='CAL Mixing Ratios', UNAME='reset_mr')
      ID=WIDGET_BUTTON(rset_sub, VALUE='TGT Mixing Ratios', UNAME='reset_mr_tgt')
      ID=WIDGET_BUTTON(rset_sub, VALUE='Treatment Config', UNAME='reset_treatcfg')
      ID=WIDGET_BUTTON(rset_sub, VALUE='Carry-Over Config', UNAME='reset_co_corr')
    ID=WIDGET_BUTTON(cnfgID, VALUE='Export Treat. Config', UNAME='exp_treatcfg', /SEPARATOR)
    
  showID=WIDGET_BUTTON(dp_men, VALUE='Show', /MENU)
    ID=WIDGET_BUTTON(showID, VALUE='Precision Table', UNAME='show_prctbl')
    ID=WIDGET_BUTTON(showID, VALUE='Cal MR Table', UNAME='show_mrtbl')
    ID=WIDGET_BUTTON(showID, VALUE='Tgt MR Table', UNAME='show_tgtmrtbl')
    ID=WIDGET_BUTTON(showID, VALUE='Treatment Config', UNAME='show_treattbl')
    ID=WIDGET_BUTTON(showID, VALUE='Carry-Over Config', UNAME='show_co_def_tbl')
    ID=WIDGET_BUTTON(showID, VALUE='Sample Volume Info', UNAME='show_svolselect')
    
  toolsID=WIDGET_BUTTON(dp_men, VALUE='Tools', /MENU)
    ID=WIDGET_BUTTON(toolsID, VALUE='Update Subst. Names', UNAME='chg_substnames')
    ID=WIDGET_BUTTON(toolsID, VALUE='Analyse PRC Exp.', UNAME='ana_mp', /SEPARATOR)
    ID=WIDGET_BUTTON(toolsID, VALUE='Analyse NL Exp.', UNAME='ana_nl', /SEPARATOR)
    
  diaID=WIDGET_BUTTON(dp_men, VALUE='Diagnostics', /MENU)
    ID=WIDGET_BUTTON(diaID, VALUE='Results Table (detailed)', UNAME='show_restable')
    ID=WIDGET_BUTTON(diaID, VALUE='PreCon. Diagnostics Plot', UNAME='show_diaplot2', /SEPARATOR)
    ID=WIDGET_BUTTON(diaID, VALUE='Chrom. Diagnostics Plot', UNAME='show_diaplot', /SEPARATOR)
    ID=WIDGET_BUTTON(diaID, VALUE='Block PRC Plot', UNAME='show_prcplot')
    ID=WIDGET_BUTTON(diaID, VALUE='MRs Plot', UNAME='show_mrsplot')
    
  repID=WIDGET_BUTTON(dp_men, VALUE='Report', /MENU)
    ID=WIDGET_BUTTON(repID, VALUE='All (sel. Exp.)', UNAME='rep_all')
    ID=WIDGET_BUTTON(repID, VALUE='Selected', UNAME='rep_sel', /SEPARATOR)
    ID=WIDGET_BUTTON(repID, VALUE='Selected (detailed)', UNAME='rep_sel_det')
    ID=WIDGET_BUTTON(repID, VALUE='Means (sel. Exp.)', UNAME='rep_exp_mean', /SEPARATOR)
    ID=WIDGET_BUTTON(repID, VALUE='Exp. Subst. List (sel. Exp.)', UNAME='rep_subst_list', /SEPARATOR)


;++++++++++++ DROPLISTS & BUTTONS
  subbase3=WIDGET_BASE(dpbase, col=2) 
    TXT=WIDGET_LABEL(subbase3, VALUE='Experiment:', /ALIGN_LEFT)
    CDL=WIDGET_DROPLIST(subbase3, VALUE=chromlist, UNAME='exp_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
    SEP=WIDGET_LABEL(subbase3, VALUE='   ')
    TXT=WIDGET_LABEL(subbase3, VALUE='Substance:', /ALIGN_LEFT)
    EDL=WIDGET_DROPLIST(subbase3, VALUE=substlist[0], UNAME='subst_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
    SEP=WIDGET_LABEL(subbase3, VALUE='   ')

  edit_tab=WIDGET_TAB(dpbase)
    subbase1=WIDGET_BASE(edit_tab, col=3, TITLE='Selected Data: Edit...')    
      TXT=WIDGET_LABEL(subbase1, VALUE='CAL Treat Mthd:       ', /ALIGN_LEFT)
      caltreatvals=((dp_expcfg)[0]).sequence.cal_treat
      ID_caltreat=WIDGET_DROPLIST(subbase1, VALUE=caltreatvals, UNAME='caltreat_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')
      TXT=WIDGET_LABEL(subbase1, VALUE='SAM Treat Mthd:       ', /ALIGN_LEFT)
      samtreatvals=((dp_expcfg)[0]).sequence.sam_treat
      ID_samtreat=WIDGET_DROPLIST(subbase1, VALUE=samtreatvals, UNAME='samtreat_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
      ID_mw_etype=WIDGET_INFO(dp_widid.dp_mainwid, FIND_BY_UNAME='exptype_dl')
      IF WIDGET_INFO(ID_mw_etype, /DROPLIST_SELECT) EQ 1 THEN $ ; if continous, select individual sample treatment
        WIDGET_CONTROL, ID_samtreat, SET_DROPLIST_SELECT=(WHERE(sam_treat_mthd EQ 'individual'))[0]       
      TXT=WIDGET_LABEL(subbase1, VALUE='CAL Interpol Mthd:    ', /ALIGN_LEFT)
      interpolvals=cal_ip_mthd
      ID_calip=WIDGET_DROPLIST(subbase1, VALUE=interpolvals, UNAME='calip_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')    
      TXT=WIDGET_LABEL(subbase1, VALUE='Eval Mode:            ', /ALIGN_LEFT)
      ID_eval_mode=WIDGET_DROPLIST(subbase1, VALUE=['signal_area', 'signal_height'], uname='eval_mode')    
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')
      SEP=WIDGET_LABEL(subbase1, VALUE='   ')
      ID=WIDGET_BUTTON(subbase1, VALUE='use_flag', UNAME='edit_useflag')    

  subbase7=WIDGET_BASE(dpbase, col=3)  
    ID=WIDGET_BUTTON(subbase7, VALUE='(Re)Calculate !', UNAME='calc_individual')
    chkb2 = WIDGET_BASE(subbase7, column=1, /NONEXCLUSIVE)
    ID=WIDGET_BUTTON(chkb2, value='overwrite use_flag', UNAME='ovwr_sel', /ALIGN_LEFT)
    WIDGET_CONTROL, ID, SET_BUTTON = 0

  subbase4=WIDGET_BASE(dpbase, col=1)
    SEP=WIDGET_LABEL(subbase4, VALUE='   ')
    TXT=WIDGET_LABEL(subbase4, VALUE='Selection: Generate...', /ALIGN_LEFT)
    subbase5=WIDGET_BASE(dpbase, col=2)
    ID=WIDGET_BUTTON(subbase5, VALUE='Results Table', UNAME='show_restable_brief')
    ID=WIDGET_BUTTON(subbase5, VALUE='Results Plot', UNAME='show_resplot')

  subbase8=WIDGET_BASE(dpbase, col=1)
    SEP=WIDGET_LABEL(subbase8, VALUE='   ', /ALIGN_LEFT)
    TXT=WIDGET_LABEL(subbase8, VALUE='All loaded Data...', /ALIGN_LEFT)

  subbase2=WIDGET_BASE(dpbase, col=2)
    ID=WIDGET_BUTTON(subbase2, VALUE='Run Calculations !', UNAME='calc_all')
    SEP=WIDGET_LABEL(subbase2, VALUE='')
    chkb1 = WIDGET_BASE(subbase2, column=1, /NONEXCLUSIVE)
    ID=WIDGET_BUTTON(chkb1, value='overwrite ALL use_flags', UNAME='ovwr', /ALIGN_LEFT)
    WIDGET_CONTROL, ID, SET_BUTTON = 0

  dp_widid.dp_dataproc = dpbase
  WIDGET_CONTROL, dpbase, /REALIZE
  
  sel_exp=0       ; update droplist settings in case an experiment is restored
  sel_subst=0     ; and settings are not default.
  cal_ip=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_ip_mthd
  caltreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_treat
  samtreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.sam_treat
  new_sel_calip=(WHERE(cal_ip_mthd EQ cal_ip))[0]
  new_sel_caltreat=(WHERE(cal_treat_mthd EQ caltreat))[0]
  new_sel_samtreat=(WHERE(sam_treat_mthd EQ samtreat))[0]
  WIDGET_CONTROL, ID_calip, SET_DROPLIST_SELECT=new_sel_calip
  WIDGET_CONTROL, ID_caltreat, SET_DROPLIST_SELECT=new_sel_caltreat
  WIDGET_CONTROL, ID_samtreat, SET_DROPLIST_SELECT=new_sel_samtreat
  WIDGET_CONTROL, ID_eval_mode, SET_DROPLIST_SELECT=((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]


  XMANAGER, 'dp_wid_dataproc_handle', dpbase, /NO_BLOCK, EVENT_HANDLER='dp_wid_dataproc_handle'


END