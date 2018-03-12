;+
; CREATED
; 17-08, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE: visualise target mixing ratio table (selected experiment)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_tgtmrtable, sel_exp

  COMMON DP_DATA
    
  IF PTR_VALID((dp_expcfg[sel_exp]).tgt_mrs.tgt_name) THEN BEGIN
    IF *((dp_expcfg[sel_exp]).tgt_mrs.tgt_name) NE !NULL THEN BEGIN
      TGT_NAME=*((dp_expcfg[sel_exp]).tgt_mrs.tgt_name)
      SUBSTANCE=*((dp_expcfg[sel_exp]).tgt_mrs.SUBSTANCE)
      MR=*((dp_expcfg[sel_exp]).tgt_mrs.MR_PPT)
      UNC_abs=*((dp_expcfg[sel_exp]).tgt_mrs.UNC_PPT)
      UNC_REL=*((dp_expcfg[sel_exp]).tgt_mrs.UNC_REL)
      SCALE=*((dp_expcfg[sel_exp]).tgt_mrs.SCALE)
      COMMENT=*((dp_expcfg[sel_exp]).tgt_mrs.COMMENT)
      IF (*((dp_expcfg[sel_exp]).tgt_mrs.UNIT)) NE !NULL THEN $
        UNIT=*((dp_expcfg[sel_exp]).tgt_mrs.UNIT) $
      ELSE UNIT=STRARR(N_ELEMENTS(COMMENT))
    ENDIF ELSE BEGIN
      msg=DIALOG_MESSAGE('No Tgt MR data found.')
      RETURN
    ENDELSE
    
    column_labels=['Substance','MR','UNC_abs','UNC_rel','Unit','Scale','comment']
    row_labels=TGT_NAME
  
    nl_hdr  = 0
    width   = 7
    format  = '(D25.5)'
    n_tgt = N_ELEMENTS(TGT_NAME)
  
    value=STRARR(width,nl_hdr+n_tgt)

    value[0,0:-1]= SUBSTANCE
    value[1,0:-1]= STRCOMPRESS(STRING(MR, FORMAT=format), /REMOVE_ALL)
    value[2,0:-1]= STRCOMPRESS(STRING(UNC_abs, FORMAT=format), /REMOVE_ALL)
    value[3,0:-1]= STRCOMPRESS(STRING(UNC_REL, FORMAT=format), /REMOVE_ALL)
    value[4,0:-1]= UNIT
    value[5,0:-1]= SCALE
    value[6,0:-1]= COMMENT
     
    column_width=[150,100,100,100,100,150,150]
  
    mainbase=WIDGET_BASE(title='Tgt MR Table')
    ID=WIDGET_TABLE(mainbase, VALUE=value, COLUMN_WIDTH=column_width, ALIGNMENT=0, $
                    COLUMN_LABELS=column_labels, ROW_LABELS=row_labels)
  
    WIDGET_CONTROL, ID, /REALIZE
  
    dp_refr_status, MESSAGE='Created Tgt MR table.'
    
  ENDIF ELSE $; end if: pointer valid and tgts loaded.
    msg=DIALOG_MESSAGE('No target MR data found.')
  
END