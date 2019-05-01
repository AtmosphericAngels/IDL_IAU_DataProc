;+
; FUNCTION: find_fnumber
;
; AUTHOR: F. Obersteiner, Oct-2016
;
; PURPOSE: perform a reversed search on a string (or string array) to find a file number. the filename and file extension
;          are expected to be separated by "sep_ext". file number and rest of filename are expected to be separated
;          by "sep_nbr".
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION find_fnumber, fname, SEP_EXT=sep_ext, SEP_NBR=sep_nbr, REST_STR=rest_str

  IF NOT KEYWORD_SET(sep_ext) THEN sep_ext = '.'
  IF NOT KEYWORD_SET(sep_nbr) THEN sep_nbr = '_'

  IF SIZE(fname, /TYPE) NE 7 OR $ ; all input variables must be of type string (7).
       SIZE(sep_ext, /TYPE) NE 7 OR $
         SIZE(sep_nbr, /TYPE) NE 7 $
           THEN RETURN, !NULL

  fnumber = LONARR(N_ELEMENTS(fname))
  rest_str = STRARR(N_ELEMENTS(fname))


  FOR i=0L, N_ELEMENTS(fname)-1 DO BEGIN

    n_chars=STRLEN(fname[i])
    ix_sep_ext=STRPOS(fname[i], sep_ext, /REVERSE_SEARCH)
    ix_sep_nbr=STRPOS(fname[i], sep_nbr, /REVERSE_SEARCH)


    IF ix_sep_ext EQ -1 OR ix_sep_nbr EQ -1 THEN BEGIN

      fnumber[i] = -1
      rest_str[i] = fname[i]

    ENDIF ELSE BEGIN

      nbr_str=STRMID(fname[i], ix_sep_nbr+1, ix_sep_ext-ix_sep_nbr-1)
      fnumber[i]=FIX(nbr_str, TYPE=3)
      rest_str[i]=STRMID(fname[i], 0, ix_sep_nbr)

    ENDELSE

  ENDFOR


  RETURN, fnumber

END