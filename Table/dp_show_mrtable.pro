;+
; PRO: dp_show_mrtable
;
; AUTHOR: F. Obersteiner, Oct-2016
;
; PURPOSE: visualise loaded mixing ratio table (selected experiment)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_mrtable, sel_exp

  COMMON DP_DATA

  IF ((dp_expcfg[sel_exp]).cal_mrs.canister) EQ '' THEN BEGIN
    msg=DIALOG_MESSAGE('No Cal MR data found.')
    RETURN
  ENDIF
  
  nl_hdr  = 4
  width   = 6
  n_subst = N_ELEMENTS(*((dp_expcfg[sel_exp]).cal_mrs.substance))
  
  value=STRARR(width,nl_hdr+n_subst)
  
  value[0,0]= 'Cal MRs' ; header...
  value[0,1]= 'Canister: '
  value[1,1]= ((dp_expcfg[sel_exp]).cal_mrs.canister)
  value[0,2]= '***'
  value[0,3]= 'Substance'     
  value[1,3]= 'MR'
  value[2,3]= 'UNC_abs'
  value[3,3]= 'UNC_rel'
  value[4,3]= 'Scale'
  value[5,3]= 'comment'
  
  value[0,4:-1]= *((dp_expcfg[sel_exp]).cal_mrs.substance) ; data...
  value[1,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).cal_mrs.mr_ppt), TYPE=7), /REMOVE_ALL)
  value[2,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).cal_mrs.unc_ppt), TYPE=7), /REMOVE_ALL)
  value[3,4:-1]= STRCOMPRESS(FIX(*((dp_expcfg[sel_exp]).cal_mrs.unc_rel), TYPE=7), /REMOVE_ALL)
  value[4,4:-1]= *((dp_expcfg[sel_exp]).cal_mrs.scale)
  value[5,4:-1]= *((dp_expcfg[sel_exp]).cal_mrs.comment)
  
  column_width=[150,100,100,100,100,100]
  
  mainbase=WIDGET_BASE(title='Cal MR Table')
  ID=WIDGET_TABLE(mainbase, VALUE=value, COLUMN_WIDTH=column_width, ALIGNMENT=0)

  WIDGET_CONTROL, ID, /REALIZE
  
  dp_refr_status, MESSAGE='Created Cal MR table.'
  
END