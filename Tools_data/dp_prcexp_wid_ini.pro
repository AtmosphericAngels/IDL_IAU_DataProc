;+
; PRO: dp_wid_prcexp_handle/dp_wid_prcexp_ini
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: handle settings for the analysis of a precision ('repro') experiment.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_prcexp_wid_handle, event

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
      RETURN
    ENDIF
  END

  ID_exp = WIDGET_INFO(dp_widid.dp_dataproc, find_by_uname='exp_dl')
  sel_exp = WIDGET_INFO(ID_exp, /DROPLIST_SELECT)

  ID_espec      = WIDGET_INFO(dp_widid.dp_mainwid, find_by_uname='expspec_dl')
  sel_espec     = WIDGET_INFO(ID_espec, /DROPLIST_SELECT)
  ID_experiment = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='exp_dl')
  sel_exp       = WIDGET_INFO(ID_experiment, /DROPLIST_SELECT)
  ID_min_blsz   = WIDGET_INFO(event.top, FIND_BY_UNAME='set_min_blsz')
  min_blsz_txt  = WIDGET_INFO(ID_min_blsz, /COMBOBOX_GETTEXT)
  min_blsz      = FIX(min_blsz_txt, TYPE=3)
  ID_max_blsz   = WIDGET_INFO(event.top, FIND_BY_UNAME='set_max_blsz')
  max_blsz_txt  = WIDGET_INFO(ID_max_blsz, /COMBOBOX_GETTEXT)
  max_blsz      = FIX(max_blsz_txt, TYPE=3)
  ID_cal_blsz   = WIDGET_INFO(event.top, FIND_BY_UNAME='set_cal_blsz')
  cal_blsz_txt  = WIDGET_INFO(ID_cal_blsz, /COMBOBOX_GETTEXT)
  cal_blsz      = FIX(cal_blsz_txt, TYPE=3)

  uname = WIDGET_INFO(event.id, /UNAME)

;  verbose = 1

  CASE uname OF
    ;******************************************************************************************
    'set_min_blsz' : $
      BEGIN
        IF min_blsz GT max_blsz THEN BEGIN
          WIDGET_CONTROL, ID_min_blsz, SET_COMBOBOX_SELECT=0
        ENDIF
      END
    ;******************************************************************************************
    'set_max_blsz' : $
      BEGIN
        IF min_blsz GT max_blsz THEN BEGIN
          WIDGET_CONTROL, ID_min_blsz, SET_COMBOBOX_SELECT=0
        ENDIF
      END
    ;******************************************************************************************
    'run_ana' : $
      BEGIN
        dp_refr_status, MESSAGE='Called PRC analyser.'
        prc_res = dp_prcexp_analyse_prc(sel_exp, min_blsz, max_blsz, cal_blsz, VERBOSE=verbose)
        dp_prcexp_res2txt, prc_res, sel_exp
      END
    ;******************************************************************************************
    ELSE:
  ENDCASE


END

;#####################################################################################################################

PRO dp_prcexp_wid_ini,  prcexp_cfg

  IF prcexp_cfg.abort EQ 1 THEN RETURN

  COMMON DP_DATA
  COMMON DP_WIDID


  ;++++++++++++ WIDGET BASE
  mpbase=WIDGET_BASE(TITLE='Prc. Exp. Config', MBAR=mp_men, column=1,$
                     XOFFSET=330, YOFFSET=350)


  lable_base=WIDGET_BASE(mpbase, col=1, /BASE_ALIGN_CENTER)
    SEP=WIDGET_LABEL(lable_base, VALUE='***', /ALIGN_CENTER)
    LBL=WIDGET_LABEL(lable_base, VALUE='PRC Analyser', /ALIGN_CENTER)
    SEP=WIDGET_LABEL(lable_base, VALUE='***', /ALIGN_CENTER)

  ;++++++++++++ DROPLISTS & BUTTONS
  subbase1=WIDGET_BASE(mpbase, col=3)
    LBL=WIDGET_LABEL(subbase1, VALUE='Set min. Blocksz:      ', /ALIGN_LEFT)
    CBX=WIDGET_COMBOBOX(subbase1, VALUE=prcexp_cfg.min_bl_str, UNAME='set_min_blsz', /DYNAMIC_RESIZE, /ALIGN_LEFT)
    LBL=WIDGET_LABEL(subbase1, VALUE='Set max. Blocksz:      ', /ALIGN_LEFT)
    CBX=WIDGET_COMBOBOX(subbase1, VALUE=prcexp_cfg.max_bl_str, UNAME='set_max_blsz', /DYNAMIC_RESIZE, /ALIGN_LEFT)
    WIDGET_CONTROL, CBX, SET_COMBOBOX_SELECT=N_ELEMENTS(prcexp_cfg.max_bl_str)-1
    LBL=WIDGET_LABEL(subbase1, VALUE='Set Cal Blocksz:      ', /ALIGN_LEFT)
    CBX=WIDGET_COMBOBOX(subbase1, VALUE=['1','2','3'], UNAME='set_cal_blsz', /DYNAMIC_RESIZE, /ALIGN_LEFT)

  subbase2=WIDGET_BASE(mpbase, col=1)
       BTN=WIDGET_BUTTON(subbase2, VALUE='Analyse!', UNAME='run_ana')


  dp_widid.dp_prcexp = mpbase
  WIDGET_CONTROL, mpbase, /REALIZE
  XMANAGER, 'dp_prcexp_wid_handle', mpbase, /NO_BLOCK, EVENT_HANDLER='dp_prcexp_wid_handle'

END