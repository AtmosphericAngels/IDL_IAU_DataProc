;+
; FUNCTION: time2jultime
;
; AUTHOR: F. Obersteiner, June 2017 - modified & commented version of S.Salas function.
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION time2jultime, DATE=date, TIME=time, INPUT_IS_MDY=input_is_mdy, $
                       DEL_DATE=del_date, DEL_TIME=del_time

  IF NOT KEYWORD_SET(date) THEN BEGIN
    date = '01.01.1900'
    no_date = 1
  ENDIF ELSE no_date = 0

  IF NOT KEYWORD_SET(time) THEN BEGIN
    time = '00:00:00'
    no_time = 1
  ENDIF ELSE no_time = 0

  IF NOT KEYWORD_SET(input_is_mdy) THEN ix = [1,0,2,0,1,2] $
    ELSE ix = [0,1,2,0,1,2]

  ; neither date nor time given...
  IF no_date+no_time EQ 2 THEN RETURN, !VALUES.D_NAN

  IF NOT KEYWORD_SET(del_date) THEN del_date = '.'
  IF NOT KEYWORD_SET(del_time) THEN del_time = ':'

  ; given string too short, return NaN
  IF STRLEN(date) LE 3 OR STRLEN(time) LE 3 THEN RETURN, !VALUES.D_NAN

  ; given string too long, assume a timestamp "dd.mm.yyyy hh:mn:ss"
  IF STRLEN(date) GT 10 OR STRLEN(time) GT 10 THEN BEGIN
    CASE 1 OF
      (no_date EQ 1): v = time
      (no_time EQ 1): v = date
      ELSE: RETURN, !VALUES.D_NAN
    ENDCASE
    date = STRMID(v, 0, 10)
    time = STRMID(v, 11, 8)
  ENDIF

  ; in case NaN string is transferred, return NaN variable,
  ; else perform conversion of string to floating point representation.
  IF STRUPCASE(date) EQ STRUPCASE('NaN') OR STRUPCASE(time) EQ STRUPCASE('NaN') $
    THEN RETURN, !VALUES.D_NAN $
      ELSE BEGIN
        sdate = strsplit(STRTRIM(STRCOMPRESS(date),2), del_date, /EXTRACT)
        stime = strsplit(STRTRIM(STRCOMPRESS(time),2), del_time, /EXTRACT)
        jultime = julday(sdate[ix[0]],sdate[ix[1]],sdate[ix[2]],stime[ix[3]],stime[ix[4]],stime[ix[5]])
        RETURN, jultime
      ENDELSE

END