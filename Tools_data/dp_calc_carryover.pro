;+
; FUNCTION dp_read_cdp_calc_carryoveralmrs
;
; AUTHOR: F. Obersteiner, Feb-2018
;
; PURPOSE: applies carry over correction to normalised response. Returns normalised corrected response.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_calc_carryover, norm_resp, sequence, carryover_strct, subst_name

  corr_norm_resp = DBLARR(N_ELEMENTS(norm_resp))+!VALUES.D_NAN
  
  w_subst = WHERE(*carryover_strct.substance EQ subst_name, n_match)
  IF n_match EQ 0 THEN RETURN, corr_norm_resp $
    ELSE BEGIN
      id_cal = 3
      id_sam = 1
      id_tgt = 2 ; tgt treated as sample in calulations here!

      corr_norm_resp=norm_resp ; set equal to input so that no additional NaNs are introduced
      FOR n=1, N_ELEMENTS(norm_resp)-1 DO BEGIN
        sect_id = (sequence.id)[n-1:n]
        sect_rsp = norm_resp[n-1:n]
        IF (WHERE(FINITE(sect_rsp) EQ 0))[0] NE -1 THEN CONTINUE ; skip iteration if NaN in response vector
        
        CASE 1 OF ; select correction factor
          sect_id[0] EQ 3 AND sect_id[1] LE 2: $ ; cal->sam
            BEGIN
;              print, 'cal->sam'
              IF sect_rsp[1] GT sect_rsp[0] THEN f_corr = (*carryover_strct.cal_to_sam)[0,w_subst] $ ; UP
                ELSE f_corr = (*carryover_strct.cal_to_sam)[1,w_subst] ; DOWN
            END
          sect_id[0] LE 2 AND sect_id[1] LE 2: $ ; sam->sam
            BEGIN
;              print, 'sam->sam'
              IF sect_rsp[1] GT sect_rsp[0] THEN f_corr = (*carryover_strct.sam_to_sam)[0,w_subst] $ ; UP
                ELSE f_corr = (*carryover_strct.sam_to_sam)[1,w_subst] ; DOWN
            END
          sect_id[0] LE 2 AND sect_id[1] EQ 3: $ ; sam->cal
            BEGIN
;              print, 'sam->cal'
              IF sect_rsp[1] GT sect_rsp[0] THEN f_corr = (*carryover_strct.sam_to_cal)[0,w_subst] $ ; UP
                ELSE f_corr = (*carryover_strct.sam_to_cal)[1,w_subst] ; DOWN
            END
          ELSE: f_corr = 0D
        ENDCASE

        corr_norm_resp[n] = norm_resp[n] + (norm_resp[n] - norm_resp[n-1])*f_corr

      ENDFOR
    ENDELSE 
  
  RETURN, corr_norm_resp
  
END