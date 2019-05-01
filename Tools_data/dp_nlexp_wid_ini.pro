;+
; PRO: dp_nlexp_wid_ini
;
; AUTHOR: F. Obersteiner, June 2017
;
; PURPOSE: widget to select nl function and trigger calculation.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_nlexp_wid_handle, event

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

  ID_subst = WIDGET_INFO(dp_widid.dp_dataproc, find_by_uname='subst_dl')
  sel_subst = WIDGET_INFO(ID_subst, /DROPLIST_SELECT)

  ID_fct = WIDGET_INFO(event.top, find_by_uname='fct_dgr')
  fct_dgr = WIDGET_INFO(ID_fct, /DROPLIST_SELECT)+1

  ID_fzr = WIDGET_INFO(event.top, find_by_uname='force_zero')
  force_zero = WIDGET_INFO(ID_fzr, /BUTTON_SET)

  ID_dpl = WIDGET_INFO(event.top, find_by_uname='dump_plots')
  dump_plots = WIDGET_INFO(ID_dpl, /BUTTON_SET)

  ID_dpr = WIDGET_INFO(event.top, find_by_uname='dump_report')
  dump_rep = WIDGET_INFO(ID_dpr, /BUTTON_SET)

  uname = WIDGET_INFO(event.id, /UNAME)

  CASE uname OF
    ;******************************************************************************************
    'run_nl_ana' : $
      BEGIN

        nl_strct = dp_nlexp_analyse(fct_dgr, sel_exp, sel_subst, $
                                    FORCE_ZERO=force_zero, SHOW_PLOTS=dump_plots, /LOUD)

        IF dump_rep EQ 1 AND nl_strct NE !NULL $
          THEN dp_nlexp_res2txt, nl_strct, sel_exp, sel_subst;, dir='C:\Users\va6504\Downloads\'

      END
    ELSE:
  END

END
;------------------------------------------------------------------------------------------------------------------------
PRO dp_nlexp_wid_ini

  COMMON DP_DATA
  COMMON DP_WIDID

  ;++++++++++++ WIDGET BASE
  nlbase=WIDGET_BASE(TITLE='NL Exp. Config', MBAR=mp_men, column=1,$
                     XOFFSET=330, YOFFSET=500)

  lable_base=WIDGET_BASE(nlbase, col=1, /BASE_ALIGN_CENTER)
    SEP=WIDGET_LABEL(lable_base, VALUE='***', /ALIGN_CENTER)
    LBL=WIDGET_LABEL(lable_base, VALUE='NL Analyser', /ALIGN_CENTER)
    SEP=WIDGET_LABEL(lable_base, VALUE='***', /ALIGN_CENTER)

  ;++++++++++++ DROPLISTS & BUTTONS
  CHKB = WIDGET_BASE(lable_base, column=1, /NONEXCLUSIVE)
    FZR = WIDGET_BUTTON(chkb, VALUE='Force Zero-Crossing?', uname='force_zero', /ALIGN_CENTER)
    WIDGET_CONTROL, FZR, SET_BUTTON = 0
    DPL = WIDGET_BUTTON(chkb, VALUE='Dump Plot?', uname='dump_plots', /ALIGN_CENTER)
    WIDGET_CONTROL, DPL, SET_BUTTON = 0

  subbase1=WIDGET_BASE(nlbase, col=2)
    LBL=WIDGET_LABEL(subbase1, VALUE='FCT degree:', /ALIGN_LEFT)
    CBX=WIDGET_DROPLIST(subbase1, VALUE=STRING(INDGEN(4)+1), UNAME='fct_dgr', /ALIGN_LEFT)
    LBL=WIDGET_LABEL(subbase1, VALUE='                                 ', /ALIGN_LEFT)
    BTN=WIDGET_BUTTON(subbase1, VALUE='Analyse!', UNAME='run_nl_ana')

  lable_base2=WIDGET_BASE(nlbase, col=1, /BASE_ALIGN_CENTER)
    SEP=WIDGET_LABEL(lable_base2, VALUE='***', /ALIGN_CENTER)

  CHKB2 = WIDGET_BASE(lable_base2, column=1, /NONEXCLUSIVE)
    DRP = WIDGET_BUTTON(chkb2, VALUE='Dump Report?', uname='dump_report', /ALIGN_CENTER)
    WIDGET_CONTROL, DRP, SET_BUTTON = 0



  dp_widid.dp_nlexp = nlbase
  WIDGET_CONTROL, nlbase, /REALIZE
  XMANAGER, 'dp_nlexp_wid_handle', nlbase, /NO_BLOCK, EVENT_HANDLER='dp_nlexp_wid_handle'

END