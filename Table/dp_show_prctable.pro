;+
; PRO: dp_show_prctable
;
; AUTHOR: F. Obersteiner, Oct-2016
;
; PURPOSE: visualise loaded precision table (selected experiment)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_prctable, sel_exp

  COMMON DP_DATA

  IF ((dp_expcfg[sel_exp]).instr_prc.instrument) EQ '' THEN BEGIN
    msg = DIALOG_MESSAGE('No loaded PRC data found.')
    RETURN
  ENDIF

  nl_hdr  = 4
  width   = 6
  n_subst = N_ELEMENTS(*((dp_expcfg[sel_exp]).instr_prc.substance))

  value=STRARR(width,nl_hdr+n_subst)

  value[0,0]= 'Reference Measurement Precisions' ; header...
  value[0,1]= 'Instrument/Detector: '
  value[1,1]= ((dp_expcfg[sel_exp]).instr_prc.instrument)
  value[0,2]= '***'
  value[0,3]= 'Substance'
  value[1,3]= 'MP_abs'
  value[2,3]= 'MP_rel'
  value[3,3]= 'LOD'
  value[4,3]= 'Unit'
  value[5,3]= 'Comment'

  value[0,4:-1]= *((dp_expcfg[sel_exp]).instr_prc.substance) ; data...
  value[1,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).instr_prc.mp_ppt), TYPE=7), /REMOVE_ALL)
  value[2,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).instr_prc.mp_rel), TYPE=7), /REMOVE_ALL)
  value[3,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).instr_prc.lod), TYPE=7), /REMOVE_ALL)

  IF (*((dp_expcfg[sel_exp]).instr_prc.unit)) NE !NULL THEN $
    value[4,4:-1]= *((dp_expcfg[sel_exp]).instr_prc.unit)

  value[5,4:-1]= *((dp_expcfg[sel_exp]).instr_prc.comment)

  column_width=[150,100,100,100,100,150]

  mainbase = WIDGET_BASE(title = 'Loaded Prc Table')
  ID=WIDGET_TABLE(mainbase, VALUE=value, COLUMN_WIDTH=column_width, ALIGNMENT=0)

  WIDGET_CONTROL, ID, /REALIZE

  dp_refr_status, MESSAGE='Created PRC table.'

END