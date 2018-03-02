;+
; PRO: dp_destroy_wids
;
; AUTHOR: F. Obersteiner, Sep-2016. Modified Aug-2017 to use WIDGET_CONTROL -> bad_id
;
; PURPOSE: closes widget(s) if identifier returns bad_id=0 (=not bad)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_destroy_wids, ALL=all, ID=id

  COMMON DP_WIDID
  
  IF NOT KEYWORD_SET(ID) THEN ID=0
  
  IF ID EQ -1 THEN RETURN  
  IF NOT KEYWORD_SET(ALL) AND NOT KEYWORD_SET(ID) THEN RETURN
  
  
  IF KEYWORD_SET(ALL) THEN BEGIN    
    
    WIDGET_CONTROL, dp_widid.dp_mainwid, BAD_ID=bad_id    
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_mainwid, /CLEAR_EVENTS, /DESTROY
    
    WIDGET_CONTROL, dp_widid.dp_dataproc, BAD_ID=bad_id 
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_dataproc, /CLEAR_EVENTS, /DESTROY
       
    WIDGET_CONTROL, dp_widid.dp_restablewid, BAD_ID=bad_id
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_restablewid, /CLEAR_EVENTS, /DESTROY
       
    WIDGET_CONTROL, dp_widid.dp_selmeas, BAD_ID=bad_id
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_selmeas, /CLEAR_EVENTS, /DESTROY
       
    WIDGET_CONTROL, dp_widid.dp_prcexp, BAD_ID=bad_id
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_prcexp, /CLEAR_EVENTS, /DESTROY
       
    WIDGET_CONTROL, dp_widid.dp_nlexp, BAD_ID=bad_id
    IF bad_id EQ 0 THEN WIDGET_CONTROL, dp_widid.dp_nlexp, /CLEAR_EVENTS, /DESTROY
       
  ENDIF ELSE BEGIN
      
    WIDGET_CONTROL, ID, BAD_ID=bad_id
    IF bad_id EQ 0 THEN WIDGET_CONTROL, ID, /CLEAR_EVENTS, /DESTROY
    
  ENDELSE
  
END