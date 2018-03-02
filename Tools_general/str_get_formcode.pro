FUNCTION str_get_formcode, typestr, n
  
  IF SIZE(typestr, /TYPE) NE 7 THEN RETURN, !NULL
  IF STRLEN(typestr) GT 6 THEN RETURN, !NULL
  IF SIZE(n, /TYPE) GT 3 THEN RETURN, !NULL

  prefix='('
  suffix=')'
  
  str=typestr+','
  
  formatstr = REPLICATE(str,n)
  formatstr = STRJOIN(formatstr)
  
  formatstr = prefix+formatstr
  STRPUT, formatstr, suffix, STRLEN(formatstr)-1

  RETURN, formatstr

END