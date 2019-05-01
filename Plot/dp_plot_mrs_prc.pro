;+
; PRO: dp_plot_mrs_prc
;
; AUTHOR: F. Obersteiner, Nov-2016
;
; PURPOSE: plot block precisions and mixing ratios for each sample.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_plot_mrs_prc, sel_exp, sel_subst, SHOW_PRC=show_prc, SHOW_MRS=show_mrs, EVAL_MODE=eval_mode

 COMMON dp_data

  IF NOT KEYWORD_SET(show_prc) THEN show_prc=0
  IF NOT KEYWORD_SET(show_mrs) THEN show_mrs=0
  IF KEYWORD_SET(show_prc) AND KEYWORD_SET(show_mrs) THEN RetuRn
  IF NOT KEYWORD_SET(eval_mode) THEN eval_mode=0 ; default: area

  IF ((dp_expcfg[sel_exp]).cal_mrs.canister) NE '' THEN cal_mrs_ok = 1 ELSE cal_mrs_ok = 0
  IF ((dp_expcfg[sel_exp]).instr_prc.instrument) NE '' THEN ins_prc_ok = 1 ELSE ins_prc_ok = 0

  w_sam_init = ((dp_expcfg[sel_exp]).sequence.ix_init_samblock)[WHERE(((dp_expcfg[sel_exp]).sequence.ix_init_samblock) NE -1, n_sam)]
  w_sam_end = ((dp_expcfg[sel_exp]).sequence.ix_end_samblock)[WHERE(((dp_expcfg[sel_exp]).sequence.ix_end_samblock) NE -1)]

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1
  sam_treat = (dp_chrom[sel_exp])[0].subst[sel_subst].rres.sam_treat

  IF sam_treat EQ 'individual' THEN BEGIN
    w_sam_end=WHERE((dp_expcfg[sel_exp]).sequence.id EQ id_sam OR (dp_expcfg[sel_exp]).sequence.id EQ id_tgt , n_sam)
    sam_names=((dp_expcfg[sel_exp]).expinfo.s_name)[w_sam_end]
  ENDIF ELSE BEGIN
    sam_names=((dp_expcfg[sel_exp]).expinfo.s_name)[w_sam_init]
  ENDELSE


  IF n_sam LT 1 THEN rETUrN

  FOR i=0, n_sam-1 DO sam_names[i]=STRING(9b)+sam_names[i]+STRING(9b)
  x=INDGEN(n_sam)+1

  subst_name=(substlist[sel_exp])[sel_subst]



;+++++++++
;+++++++++
;+++++++++ Call precision plot
  IF show_prc THEN BEGIN

    CASE eval_mode OF
      0: prc=(((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.block_rsd)[w_sam_end])*100D
      1: prc=(((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.block_rsd)[w_sam_end])*100D
    ENDCASE

    w_fin=WHERE(FINITE(prc) EQ 1, n_fin)
    IF n_fin GT 0 THEN BEGIN
      x=x[w_fin]
      prc=prc[w_fin]
      sam_names=sam_names[w_fin]
      yrange=[MIN(prc)-(MAX(prc)-MIN(prc))*0.1, MAX(prc)+(MAX(prc)-MIN(prc))*0.1]

      p_prc = plot($
                   x, prc, $
                   XRANGE=[0.5, n_sam+0.5], $
                   YRANGE=yrange, $
                   XTICKNAME=sam_names, $
                   XTEXT_ORIENTATION=45, $
                   XTICKVALUES=x, $
                   TITLE=subst_name+': block precisions', $
                   YTITLE='PRC [%]', $
                   LINESTYLE=6, $
                   SYMBOL='D', $
                   SYM_FILLED=1, $
                   SYM_SIZE=3.0, $
                   SYM_COLOR='g' $
                   )

       dp_refr_status, MESSAGE='Created precision plot.'
     ENDIF ELSE !NULL=DIALOG_MESSAGE('No displayable results found.', /INFORMATION)
  ENDIF


;+++++++++
;+++++++++
;+++++++++ Call mixing ratio plot
  IF show_mrs THEN BEGIN
    IF NOT cal_mrs_ok THEN BEGIN
      !NULL=DIALOG_MESSAGE('Please load calibration MRs first.', /INFORMATION)
      rETUrN
    ENDIF ELSE BEGIN
      prelim_MRs = dp_calc_mrs(subst_name, sel_subst, sel_exp, dp_chrom, dp_expcfg)
      sel_MRs = prelim_MRs[w_sam_end]
      w_fin=WHERE(FINITE(sel_MRs) EQ 1, n_fin)
      IF n_fin GT 0 THEN BEGIN
        x=x[w_fin]
        sel_MRs=sel_MRs[w_fin]
        sam_names=sam_names[w_fin]
        yrange=[MIN(sel_MRs)-(MAX(sel_MRs)-MIN(sel_MRs))*0.05, MAX(sel_MRs)+(MAX(sel_MRs)-MIN(sel_MRs))*0.05]

        p_mr = plot($
                    x, sel_MRs, $
                    XRANGE=[0.5, n_sam+0.5], $
                    YRANGE=yrange, $
                    XTICKNAME=sam_names, $
                    XTEXT_ORIENTATION=45, $
                    XTICKVALUES=x, $
                    TITLE=subst_name+': preliminary mixing ratios', $
                    YTITLE='MR', $
                    LINESTYLE=6, $
                    SYMBOL='D', $
                    SYM_FILLED=1, $
                    SYM_SIZE=3.0, $
                    SYM_COLOR='b' $
                    )
        dp_refr_status, MESSAGE='Created mixing ratio plot.'
      ENDIF ELSE !NULL=DIALOG_MESSAGE('No displayable results found.', /INFORMATION)
    ENDELSE
  ENDIF



END