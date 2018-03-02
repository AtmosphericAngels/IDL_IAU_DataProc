;+
; PRO: dp_show_carryover_def
;
; AUTHOR: F. Obersteiner, 2018-02
;
; PURPOSE: visualise loaded carry over definition (selected experiment)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_carryover_def, sel_exp

  COMMON DP_DATA

  IF *(dp_expcfg[sel_exp]).carryover.substance EQ !NULL THEN BEGIN
    msg=DIALOG_MESSAGE('No carry-over config found for selected experiment.')
    RETURN
  ENDIF

  nl_hdr  = 4
  width   = 7
  n_subst = N_ELEMENTS(*(dp_expcfg[sel_exp]).carryover.substance)

  value=STRARR(width,nl_hdr+n_subst)
  
  comments = (dp_expcfg[sel_exp]).carryover.comment
  
  value[0,0]= 'Carry-Over correction parameters' ; header...
  value[0,1]= 'Instrument: '
  value[1,1]= (strsplit(comments[1], ';', /EXTRACT, /PRESERVE_NULL))[1]
  value[0,2]= '***'
  value[0,3]= 'Substance' ; column header...
  value[1,3]= 'cal_to_sam_UP'
  value[2,3]= 'cal_to_sam_DOWN'
  value[3,3]= 'sam_to_sam_UP'
  value[4,3]= 'sam_to_sam_DOWN'
  value[5,3]= 'sam_to_cal_UP'
  value[6,3]= 'sam_to_cal_DOWN'
  
  comments = (dp_expcfg[sel_exp]).carryover.comment
  cal_to_sam = *(dp_expcfg[sel_exp]).carryover.cal_to_sam
  sam_to_sam = *(dp_expcfg[sel_exp]).carryover.sam_to_sam
  sam_to_cal = *(dp_expcfg[sel_exp]).carryover.sam_to_cal
  
  value[0,4:-1]= *(dp_expcfg[sel_exp]).carryover.substance ; data...
  value[1,4:-1]= STRCOMPRESS(FIX(cal_to_sam[0,*], TYPE=7), /REMOVE_ALL)
  value[2,4:-1]= STRCOMPRESS(FIX(cal_to_sam[1,*], TYPE=7), /REMOVE_ALL)
  value[3,4:-1]= STRCOMPRESS(FIX(sam_to_sam[0,*], TYPE=7), /REMOVE_ALL)
  value[4,4:-1]= STRCOMPRESS(FIX(sam_to_sam[1,*], TYPE=7), /REMOVE_ALL)
  value[5,4:-1]= STRCOMPRESS(FIX(sam_to_cal[0,*], TYPE=7), /REMOVE_ALL)
  value[6,4:-1]= STRCOMPRESS(FIX(sam_to_cal[1,*], TYPE=7), /REMOVE_ALL)

  column_width=[160,100,100,100,100,100,100]

  mainbase=WIDGET_BASE(title='Carry-Over Table')
  ID=WIDGET_TABLE(mainbase, VALUE=value, COLUMN_WIDTH=column_width, ALIGNMENT=0)

  WIDGET_CONTROL, ID, /REALIZE

  dp_refr_status, MESSAGE='Created Carry-Over table.'

END
