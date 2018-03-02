PRO dp_refr_status, MESSAGE=message, CLEAR=clear
  
  key1_set=KEYWORD_SET(message) ;+++ check if keywords set, return if neither is set
  key2_set=KEYWORD_SET(clear)
  IF key1_set+key2_set EQ 0 THEN RETURN

  COMMON DP_WIDID
  
  IF dp_widid.dp_mainwid EQ -1 THEN RETURN ; widget not used yet
  ID=WIDGET_INFO(dp_widid.dp_mainwid, FIND_BY_UNAME='status')

  IF KEYWORD_SET(message) THEN BEGIN
    IF SIZE(message, /TYPE) NE 7 THEN RETURN ; keyword message not of type string
    WIDGET_CONTROL, ID, SET_VALUE=message
  ENDIF

  IF KEYWORD_SET(clear) THEN $
    WIDGET_CONTROL, ID, SET_VALUE='Idle'
    
END