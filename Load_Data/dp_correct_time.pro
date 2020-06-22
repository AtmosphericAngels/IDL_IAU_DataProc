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
      
        tmp_strct = (dp_chrom[i])
        
        IF N_ELEMENTS(tmp_strct.fname) GT 1 THEN BEGIN
            exp_jdate = tmp_strct.jdate
            
            ; check timestamps for "jumps" (Chemstation export fail)
            w = WHERE(exp_jdate[1:-1]-exp_jdate[0:-2] LT 0., n_jumps)
            FOR k=0, n_jumps-1 DO $
                IF (exp_jdate[w[k]+1]-exp_jdate[w[k]-1]) LE 0. THEN BEGIN
                    force_mono_inc = 1
                    IF loud THEN $
                        msg = DIALOG_MESSAGE('Detected error in timeline: ' + FILE_BASENAME((tmp_strct.fname)[w[k]]))
                ENDIF

            ; get median time delta between measurements
            delta = MEDIAN(exp_jdate[1:-1]-exp_jdate[0:-2])

            IF force_mono_inc THEN BEGIN  
                j_max = N_ELEMENTS(dp_chrom[i])-1
                ; enforce constant time delta
                FOR j=0, j_max DO exp_jdate[j] = exp_jdate[0] + j*delta
              
                IF verbose THEN BEGIN
                  print, '+++'
                  print, 'dp_correct_time: timestamp overwritten with constant increment (median of measurement series)'
                  print, '+++'
                ENDIF
                
            ENDIF ELSE BEGIN ; end if force_mono_inc == True
                j_max = N_ELEMENTS(dp_chrom[i])-2
                ; use median t delta if there are jumps
                FOR j=0, j_max DO BEGIN
                    IF exp_jdate[j] GT exp_jdate[j+1] THEN BEGIN
                        exp_jdate[j] = exp_jdate[j+1] - delta
                        IF verbose THEN BEGIN
                          print, '+++'
                          print, 'dp_correct_time: timestamp error corrected between'
                          print, FILE_BASENAME((tmp_strct.fname)[j])+' and'
                          print, FILE_BASENAME((tmp_strct.fname)[j+1])
                          print, '+++'
                        ENDIF
                    ENDIF
                ENDFOR
            ENDELSE ; end else force_mono_inc == False

            tmp_strct.jdate = exp_jdate
            (dp_chrom[i]) = tmp_strct

        ENDIF ; end if number of chromatograms gt 1

    ENDFOR ; end loop over chrom files

  RETURN, dp_chrom

END