;+
; PRO: dp_show_treatcfgtable
;
; AUTHOR: F. Obersteiner, Oct-2016
;
; PURPOSE: visualise loaded sample/cal treatment configuration (selected experiment).
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_treatcfgtable, sel_exp

  COMMON DP_DATA

  IF NOT PTR_VALID((dp_expcfg[sel_exp]).treatcfg.substance) THEN BEGIN
    msg = DIALOG_MESSAGE('No loaded treatment config found.')
    RETURN
  ENDIF

  IF (*(dp_expcfg[sel_exp]).treatcfg.substance) EQ !NULL THEN BEGIN
    msg = DIALOG_MESSAGE('No loaded treatment config found.')
    RETURN
  ENDIF


  width   = 4
  n_subst = N_ELEMENTS(*(dp_expcfg[sel_exp]).treatcfg.substance)

  hdr = ['Substance','Cal_Treat','Sam_Treat','Cal_Interpol']

  value = STRARR(width, n_subst)


  value[0,0:-1]= *((dp_expcfg[sel_exp]).treatcfg.substance)
  value[1,0:-1]= *((dp_expcfg[sel_exp]).treatcfg.cal_treat)
  value[2,0:-1]= *((dp_expcfg[sel_exp]).treatcfg.sam_treat)
  value[3,0:-1]= *((dp_expcfg[sel_exp]).treatcfg.cal_ip)

  column_width=[150,100,100,100]

  mainbase = WIDGET_BASE(title='Treatment Config Table')
  ID=WIDGET_TABLE(mainbase, VALUE=value, COLUMN_WIDTH=column_width, COLUMN_LABELS=hdr, ALIGNMENT=0)

  WIDGET_CONTROL, ID, /REALIZE

  dp_refr_status, MESSAGE='Created treat. config table.'

END