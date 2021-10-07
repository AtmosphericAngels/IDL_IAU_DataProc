;+
; FUNCTION strreplace_iter
;
; AUTHOR: -THE INTERNET-
; MODIFIED: F. Obersteiner, Nov. 2016, n_iter keyword added to replace multiple occurances and
;           no_case keyword to make the search non - case-sensitive.
;
; PURPOSE: replace one or more occurances of "Find1" with "Replacement1" in a string or strarr "Strings"
;-
;--------------------------------------------------------------------------------------------------------------------
FUNCTION strreplace_iter, Strings, Find1, Replacement1, N_ITER=n_iter, NO_CASE=no_case

  IF NOT KEYWORD_SET(n_iter) THEN n_iter = 1
  IF n_iter LT 1 THEN n_iter = 1

  NP        = N_PARAMS()  ;   Check integrity of input parameter
  IF (NP NE 3) THEN msg = DIALOG_MESSAGE('Must be called with 3 parameters: '+$
                                       'Strings, Find, Replacement.', /ERROR)

  sz        = SIZE(Strings)
  ns        = N_ELEMENTS(sz)
  IF (sz(ns-2) NE 7) THEN msg=DIALOG_MESSAGE('Parameter must be of string type.', /ERROR)

  Find        = STRING(Find1)
  Replacement = STRING(Replacement1)
  Flen        = STRLEN(Find)

  FOR iter=0L, n_iter-1 DO BEGIN ; loop iterations
    IF KEYWORD_SET(no_case) THEN pos = STRPOS(STRUPCASE(Strings), STRUPCASE(Find)) $
      ELSE pos = STRPOS(Strings, Find)
    here      = WHERE(pos ne -1, nreplace)

    IF (nreplace EQ 0) THEN BREAK

    FOR i=0, nreplace-1 DO BEGIN ; loop in case Strings is an array of strings
      j         = here(i)
      prefix    = STRMID(Strings(j), 0, pos(j))
      suffix    = STRMID(Strings(j), pos(j) + Flen, $
                         STRLEN(Strings(j)) -(pos(j) + Flen))
      Strings(j) = prefix + replacement + suffix
    ENDFOR
  ENDFOR

  RETURN, Strings;(j)
END