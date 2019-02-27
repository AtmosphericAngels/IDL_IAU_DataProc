;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 16-10, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; analyse a series of cal measurements (precision experiment) to determine
; the maximum/minimum block size in the prec. exp. iteration
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_prcexp_analyse_seq, sel_exp, dp_expcfg, sid_name, MIN_N_BLOCKS=min_n_blocks, $
                                VERBOSE=verbose
                                
  
  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  IF NOT KEYWORD_SET(min_n_blocks) THEN min_n_blocks=5.
  
  seq = (dp_expcfg[sel_exp]).sequence
  
  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1
  
  
  IF seq.n_cal_blocks NE 1 THEN BEGIN
    
    msg=DIALOG_MESSAGE('Number of Cal Blocks NE 1; Prerequisite for Precision Experiment Analysis!')
    abort=1
    n_cals=-1
    blsz_min=-1
    blsz_max=-1

    min_bl_val= [-1]
    max_bl_val= [-1]
    
  ENDIF ELSE BEGIN
    
    abort=0
    n_cals=seq.ix_end_calblock[0]-seq.ix_init_calblock[0]+1
    blsz_min=1
    blsz_max=FLOOR(n_cals/FIX(min_n_blocks, TYPE=5))

    min_bl_val=LINDGEN(blsz_max-blsz_min+1)+blsz_min
    max_bl_val=min_bl_val
    
  ENDELSE
                                                  
    strct={ abort : abort, $
            n_dp  : n_cals, $
            blsz_min  : blsz_min, $
            blsz_max  : blsz_max, $
            min_bl_str  : STRING(min_bl_val), $
            max_bl_str  : STRING(max_bl_val), $
            seq : seq, $
            cal_ix : WHERE(seq.id EQ id_cal) }
           
  
  RETURN, strct
END