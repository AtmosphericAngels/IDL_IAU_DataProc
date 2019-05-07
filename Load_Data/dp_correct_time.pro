;
; correct for time "jumps" in e.g. AGILENT .cdfs
; MS Chemstation sometimes uses the current time of the PC instead of the actual experiment timestamp
;
; 2017 04 - added keyword FORCE_MONO_INC to enforce a monotonically increasing, equidistant timestamp
;           based on the median of all found timestamps.
;
FUNCTION dp_correct_time, dp_chrom, FORCE_MONO_INC=force_mono_inc, VERBOSE=verbose, LOUD=loud

    IF NOT KEYWORD_SET(verbose) THEN verbose = 0
    IF NOT KEYWORD_SET(loud) THEN loud = 0
    IF NOT KEYWORD_SET(force_mono_inc) THEN force_mono_inc = 0

    FOR i=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN
      
      tmp_strct=(dp_chrom[i])
      
      IF N_ELEMENTS(tmp_strct.fname) GT 1 THEN BEGIN
        exp_jdate=tmp_strct.jdate
        w = WHERE(exp_jdate[1:-1]-exp_jdate[0:-2] LT 0., n_jumps)
  
        FOR k=0, n_jumps-1 DO $
          IF (exp_jdate[w[k]+1]-exp_jdate[w[k]-1]) LE 0. THEN BEGIN
            force_mono_inc = 1
            IF loud THEN $
              msg = DIALOG_MESSAGE('Detected error in timeline: ' + FILE_BASENAME((tmp_strct.fname)[w[k]]))
          ENDIF
  
        delta = MEDIAN(exp_jdate[1:-1]-exp_jdate[0:-2])
  
        IF force_mono_inc THEN j_max = N_ELEMENTS(dp_chrom[i])-1 $
          ELSE j_max = N_ELEMENTS(dp_chrom[i])-2
  
        FOR j=0, j_max DO BEGIN
  
          IF KEYWORD_SET(force_mono_inc) THEN  $
            exp_jdate[j]=exp_jdate[0]+j*delta $; "override" option
          ELSE BEGIN
            IF exp_jdate[j] GT exp_jdate[j+1] THEN $
              exp_jdate[j]=exp_jdate[j+1]-delta
          ENDELSE
  
          IF verbose THEN BEGIN
            print, '+++'
            print, 'dp_correct_time: timestamp error corrected between'
            print, FILE_BASENAME((tmp_strct.fname)[j])+' and'
            print, FILE_BASENAME((tmp_strct.fname)[j+1])
            print, '+++'
          ENDIF
  
        ENDFOR
  
        tmp_strct.jdate=exp_jdate
        (dp_chrom[i])=tmp_strct
        
      ENDIF
      
    ENDFOR

  RETURN, dp_chrom

END