;+
; PRO: dp_dp_mean2txt
;
; AUTHOR: T.J. Schuck, Jan-2017
;
; MODIFICATIONS:
;   FO, 170510, determination of targets
;
; PURPOSE: exports mean values for all subtances to txt file
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_mean2txt, sel_exp,  PATH=path

  COMMON DP_DATA


  tmp_data = (dp_chrom[sel_exp])
  n_chrom = N_ELEMENTS(tmp_data)
  sel_chrom = FLOOR(n_chrom/2.)
  n_subst = N_ELEMENTS(tmp_data[sel_chrom].subst.name)

  fname = ''
  sep = STRING(9B)

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  exp_ts = jultime2timestring(((dp_chrom[sel_exp]).subst[0].rres.dp_timestamp)[0])
  cal = ((dp_expcfg[sel_exp]).expinfo.s_name)[(WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_cal))[0]]

  w_tgt = WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_tgt, n_tgt)

  IF n_tgt GT 0 THEN BEGIN
    targets = ((dp_expcfg[sel_exp]).expinfo.s_name)[w_tgt]
    targets = targets[uniq(targets[SORT(targets)])]
    target = ''
    FOR i=0, N_ELEMENTS(targets)-1 DO target = target + targets[i] + STRING(9B)
    target = STRMID(target, 0, STRLEN(target)-1)
  ENDIF ELSE BEGIN
    target = 'NA'
  ENDELSE

  datestr = jultime2timestring(SYSTIME(/JULIAN), /YMD_CLEAN)

  instr = instr.replace('/', '_')
  fname = DIALOG_PICKFILE(PATH=path, /WRITE, FILE=instr+'_dp_subst_mean.txt')
  IF STRLEN(fname) EQ 0 THEN RETURN

  colheader = [ 'Substance',sep,'m/Q',sep,'eval_mode',sep,'Cal_Interpol',sep,'Cal_Treat',sep, $
                'Sample_Treat',sep,'Instr_rel_MP',sep,'Cal_mintomax',sep,'Cal_devtofit',sep, $
                'Cal_block_rsd',sep,'Sam_mean_rsd',sep, 'n_block', sep,'mean_target_mxr'         ]


  OPENW, lun, fname, /GET_LUN

  PRINTF, lun, '*** IAU_DP_v'+dp_vers+' REPORT ***', FORMAT='(A)'
  PRINTF, lun, 'Experiment:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), FORMAT='(A,A,A)'
  PRINTF, lun, 'Processing_Timestamp:', sep, exp_ts, FORMAT='(A,A,A)'
  PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
  PRINTF, lun, 'Cal_Gas:', sep, cal, FORMAT='(A,A,A)'
  PRINTF, lun, 'Target_Gas:', sep, target, FORMAT='(A,A,A)'
  PRINTF, lun, 'Separator:', sep, 'TAB', FORMAT='(A,A,A)'
  PRINTF, lun, '*** End of Header ***', FORMAT='(A)'

  PRINTF, lun, colheader, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'


  FOR i=0, n_subst-1 DO BEGIN
    name = tmp_data[sel_chrom].subst[i].name
    mass = STRCOMPRESS(STRING((tmp_data[sel_chrom].subst[i].mass[(dp_chrom[sel_exp]).subst[i].quant])[0], $
      FORMAT='(D25.3)'), /REMOVE_ALL)
    eval_mode = ((tmp_data[sel_chrom]).subst[i].rres.rsp_select)[0]
    mode_string = (['signal_area', 'signal_height'])[eval_mode]
    cal_ip = tmp_data[sel_chrom].subst[i].rres.cal_ip_mthd
    cal_treat = tmp_data[sel_chrom].subst[i].rres.cal_treat
    sam_treat = tmp_data[sel_chrom].subst[i].rres.sam_treat

    IF *(dp_expcfg[sel_exp]).instr_prc.mp_rel NE !NULL THEN BEGIN
      w=WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).instr_prc.substance) EQ STRUPCASE(name))
      IF w[0] NE -1 THEN $
        cal_prc = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
    ENDIF ELSE cal_prc= 'NaN'


    CASE eval_mode OF
      0: $
        BEGIN
          cal_mintomax = STRCOMPRESS(STRING((tmp_data[sel_chrom].subst[i].rres.rsp_area.cal_mintomax)[0], $
            FORMAT='(D25.6)'), /REMOVE_ALL)
          cal_devtofit = STRCOMPRESS(STRING((tmp_data[sel_chrom].subst[i].rres.rsp_area.cal_devtofit)[0], $
            FORMAT='(D25.6)'), /REMOVE_ALL)

          cal_block_rsd = (dp_chrom[sel_exp].subst[i].rres.rsp_area.cal_block_rsd)
          w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
          IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
          ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
            ((dp_chrom[sel_exp]).subst[i].rres.rsp_area.cal_block_rsd), $
            /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

          sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[i].rres.rsp_area.block_rsd)
          w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
          IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
          ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
            ((dp_chrom[sel_exp]).subst[i].rres.rsp_area.block_rsd), $
            /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
        END

      1: $
        BEGIN
          cal_mintomax = STRCOMPRESS(STRING((tmp_data[sel_chrom].subst[i].rres.rsp_height.cal_mintomax)[0], $
            FORMAT='(D25.6)'), /REMOVE_ALL)
          cal_devtofit = STRCOMPRESS(STRING((tmp_data[sel_chrom].subst[i].rres.rsp_height.cal_devtofit)[0], $
            FORMAT='(D25.6)'), /REMOVE_ALL)

          cal_block_rsd = (dp_chrom[sel_exp].subst[i].rres.rsp_height.cal_block_rsd)
          w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
          IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
          ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
            ((dp_chrom[sel_exp]).subst[i].rres.rsp_height.cal_block_rsd), $
            /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

          sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[i].rres.rsp_height.block_rsd)
          w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
          IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
          ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
            ((dp_chrom[sel_exp]).subst[i].rres.rsp_height.block_rsd), $
            /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
        END
    ENDCASE

    n_sam_blocks = STRCOMPRESS(STRING(n_finite, FORMAT='(I3)'), /REMOVE_ALL)


    IF ((dp_expcfg[sel_exp]).cal_mrs.canister) EQ '' THEN call_mr_calc=0 $
      ELSE call_mr_calc=1

    subst_name=(substlist[sel_exp])[i]

    IF call_mr_calc THEN prelim_MRs=dp_calc_mrs(name, i, sel_exp, dp_chrom, dp_expcfg, eval_mode) $
      ELSE prelim_MRs=MAKE_ARRAY(n_chrom, /DOUBLE, VALUE=!Values.D_NAN)

    IF n_tgt GT 0 THEN BEGIN
      tgt_mxr = prelim_MRs[w_tgt]
      w_finite = WHERE(FINITE(prelim_MRs[w_tgt]) EQ 1, n_finite)
      tgt_mxr = STRCOMPRESS(STRING(MEAN(tgt_mxr[w_finite]), FORMAT = '(D25.3)'), /REMOVE_ALL)
    ENDIF ELSE $
      tgt_mxr ='NaN'


    PRINTF, lun, name, sep, mass, sep, mode_string, sep, Cal_IP, sep, Cal_Treat, sep, Sam_Treat, sep, $
            cal_prc, sep, cal_mintomax, sep, cal_devtofit, sep, cal_block_rsd, sep, sam_blocks_rsd, sep, n_sam_blocks, $
            sep, tgt_mxr, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

  ENDFOR

  CLOSE, lun
  FREE_LUN, lun

  dp_refr_status, MESSAGE='Mean values exported.'

END