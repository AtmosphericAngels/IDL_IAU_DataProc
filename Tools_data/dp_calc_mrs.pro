;+
; FUNCTION: dp_calc_mrs
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: calculates MRs for a given substance of an experiment, provided that cal MRs are stored in dp_expcfg.
;         
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_calc_mrs, subst_name, sel_name, sel_exp, dp_chrom, dp_expcfg, $
                      EVAL_MODE=eval_mode

  n_chrom = N_ELEMENTS((dp_chrom[sel_exp]))
  
  sam_treat = (dp_chrom[0].subst[0].rres.sam_treat)[0]  
  
  IF NOT KEYWORD_SET(eval_mode) THEN $ 
    eval_mode = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_select)[0] $
      ELSE eval_mode=0 ; default area 

  mrs_array = MAKE_ARRAY(n_chrom, /DOUBLE, VALUE=!Values.D_NAN)
  mrs=mrs_array
 
 
  IF PTR_VALID((dp_expcfg[sel_exp]).cal_mrs.substance) THEN BEGIN
    w_subst=WHERE(*(dp_expcfg[sel_exp]).cal_mrs.substance EQ subst_name)
    IF w_subst[0] EQ -1 THEN RETURN, mrs_array
  ENDIF ELSE RETURN, mrs_array
  
  
  CASE eval_mode OF ; 0 area / 1 height // bl stands for "block" here...
    0: $
        BEGIN
          IF sam_treat EQ 'individual' THEN $
            mrs = (dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.sam_rrsp  * (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w_subst[0]] $
          ELSE BEGIN
            w_bl_fin=WHERE(FINITE((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rrsp) EQ 1, n_bl_fin)
            IF n_bl_fin GT 0 THEN $ ; prefer block rresp; use individual sample responses if no block values
              mrs = (dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rrsp * (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w_subst[0]]
          ENDELSE              
        END
        
    1: $
        BEGIN
          IF sam_treat EQ 'individual' THEN $
            mrs = (dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.sam_rrsp  * (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w_subst[0]] $
          ELSE BEGIN
            w_bl_fin=WHERE(FINITE((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rrsp) EQ 1, n_bl_fin)
            IF n_bl_fin GT 0 THEN $
              mrs = (dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rrsp * (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w_subst[0]]
          ENDELSE
        END
  ENDCASE
  
      
  w_mr_fin=WHERE(FINITE(mrs) EQ 1, n_finite) 
  IF n_finite GT 0 THEN mrs_array[w_mr_fin] = mrs[w_mr_fin]

   FOR i=0, n_chrom-1 DO BEGIN
     IF FINITE(mrs_array[i]) THEN $
       IF (mrs_array[i] LT 0.) THEN mrs_array[i] = 'NaN' 
   END
  
  RETURN, mrs_array
  
END