;
; correct for time "jumps" in e.g. AGILENT .cdfs
; MS Chemstation sometimes uses the current time of the PC instead of the actual experiment timestamp
;
; 2017 04 - added keyword FORCE_MONO_INC to enforce a monotonically increasing, equidistant timestamp
;           based on the median of all found timestamps.
;           
; 2020 06 - revised and simplified.          
;
FUNCTION dp_correct_time, dp_chrom, FORCE_MONO_INC=force_mono_inc, VERBOSE=verbose, LOUD=loud

    IF NOT KEYWORD_SET(verbose) THEN verbose = 0
    IF NOT KEYWORD_SET(loud) THEN loud = 0
    IF NOT KEYWORD_SET(force_mono_inc) THEN force_mono_inc = 0

    FOR i=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN

        tmp_strct = (dp_chrom[i])

        IF N_ELEMENTS(tmp_strct.fname) GT 1 THEN BEGIN ; only if more than one measurement...
            exp_jdate = tmp_strct.jdate

            ; get median time delta between measurements as reference
            delta = MEDIAN(exp_jdate[1:-1]-exp_jdate[0:-2])

            ; check timestamps for "jumps" backwards (Chemstation export fail)
            w = WHERE(exp_jdate[1:-1]-exp_jdate[0:-2] LT 0., n_jumps)

            ; if a jump was found, trigger re-calculation of the timestamps
            IF n_jumps GT 0 THEN force_mono_inc = 1 

            IF force_mono_inc THEN BEGIN
                IF loud THEN $
                  msg = DIALOG_MESSAGE('error in timeline, re-calculating...')
                ; the actual recalculation...
                FOR j=0, N_ELEMENTS(dp_chrom[i])-1 DO exp_jdate[j] = exp_jdate[0] + j*delta
                ; write back to list
                tmp_strct.jdate = exp_jdate
                (dp_chrom[i]) = tmp_strct
            ENDIF

        ENDIF ; endif, more than one chromatogram

    ENDFOR ; endloop over all measurement series

    RETURN, dp_chrom

END
