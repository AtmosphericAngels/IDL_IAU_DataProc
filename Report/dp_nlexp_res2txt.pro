;+
; PRO: dp_nlexp_res2txt
;
; AUTHOR: F. Obersteiner, florian.obersteiner@kit.edu, June 2017.
;
; PURPOSE: dump nl analyser results to a txt file.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_nlexp_res2txt, nl_strct, sel_exp, sel_subst, DIR=dir

  COMMON DP_DATA

  ; prompt directory if not set
  IF NOT KEYWORD_SET(dir) THEN $
      dir = DIALOG_PICKFILE(TITLE='Please select a directory to store results.', /DIRECTORY)

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  subst = nl_strct[0].species
  sep = STRING(9B)
  tgts = nl_strct[0].tgt_names
  eval_mode = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]
  mode_string = (['area', 'height'])[eval_mode]

  ; FO 2020-03-01: use cal name from MR table if loaded
  IF ((dp_expcfg[sel_exp]).cal_mrs.canister) EQ '' THEN $
      cal = ((dp_expcfg[sel_exp]).expinfo.s_name)[(WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_cal))[0]] $
  ELSE $
      cal = ((dp_expcfg[sel_exp]).cal_mrs.canister)

  ; FO 2020-03-02: included scale info
  w = WHERE(*((dp_expcfg[sel_exp]).tgt_mrs.SUBSTANCE) EQ subst)
  scale = (*((dp_expcfg[sel_exp]).tgt_mrs.SCALE))[w[-1]]

  mass = STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_subst].mass[(dp_chrom[sel_exp]).subst[sel_subst].quant])[0], $
                          FORMAT='(D25.3)'), /REMOVE_ALL)
  fct_dgr_str = STRCOMPRESS(STRING(nl_strct.fct_dgr, FORMAT='(I)'), /REMOVE_ALL)
  ix = INDGEN(nl_strct.fct_dgr+1, INCREMENT=2)
  fct_parms_str = STRARR((nl_strct.fct_dgr+1)*2)
  fct_parms_str[*] = sep
  fct_parms_str[ix] = STRCOMPRESS(STRING(nl_strct.fct_parms, FORMAT='(D25.9)'), /REMOVE_ALL)
  fct_sigma_str = STRARR((nl_strct.fct_dgr+1)*2)
  fct_sigma_str[*] = sep
  fct_sigma_str[ix] = STRCOMPRESS(STRING(nl_strct.fct_sigma, FORMAT='(D25.9)'), /REMOVE_ALL)
  corr_max_dev_to_zero = STRCOMPRESS(STRING(nl_strct.corr_max_dev_to_zero, FORMAT='(D25.9)'), /REMOVE_ALL)


  cal_est_MR = STRCOMPRESS(STRING(nl_strct.cal_est_MR, FORMAT='(D25.5)'), /REMOVE_ALL)
  cal_est_sig = STRCOMPRESS(STRING(nl_strct.cal_est_sig, FORMAT='(D25.6)'), /REMOVE_ALL)
  unit = nl_strct.unit
  max_dev_to_fit = STRCOMPRESS(STRING(nl_strct.max_dev_to_fit, FORMAT='(D25.9)'), /REMOVE_ALL)
  cal_in_tgt_range = STRCOMPRESS(STRING(nl_strct.cal_in_tgt_range, FORMAT='(I)'), /REMOVE_ALL)

  cal_mr_spec = STRCOMPRESS(STRING(nl_strct.cal_MR_spec, FORMAT='(D25.5)'), /REMOVE_ALL)
  cal_mr_err = STRCOMPRESS(STRING(nl_strct.cal_MR_err, FORMAT='(D25.6)'), /REMOVE_ALL)

  prefix = 'iaudp_nl_'
  IF *(dp_expcfg[sel_exp]).cal_mrs.mr_ppt EQ !NULL THEN suffix = '_cal_estimate.txt' $
    ELSE suffix = '.txt'

  fname = dir + prefix+subst+suffix

  IF NOT FILE_TEST(dir) THEN FILE_MKDIR, dir

  OPENW, lun, fname, /GET_LUN
  PRINTF, lun, '*** IAU_DP_v'+dp_vers+' NL REPORT ***', FORMAT='(A)'

  ; FO 2020-03-01 add chrom date:
  PRINTF, lun, 'Experiment/Date:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), sep, $
               jultime2timestring(mean(dp_chrom[sel_exp].jdate)), FORMAT='(A,A,A,A,A)'
  PRINTF, lun, 'Processing_Timestamp:', sep, jultime2timestring(SYSTIME(/JULIAN)), FORMAT='(A,A,A)'
  PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
  PRINTF, lun, 'Cal_Gas:', sep, cal, FORMAT='(A,A,A)'
  PRINTF, lun, 'Cal_Gas spec. MR:', sep, cal_mr_spec, FORMAT='(A,A,A)'
  PRINTF, lun, 'Cal_Gas spec. abs. error:', sep, cal_mr_err, FORMAT='(A,A,A)'
  PRINTF, lun, 'Substance:', sep, subst, sep, 'm/Q:', sep, mass, sep, 'Eval_Mode:', sep, mode_string, FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
  PRINTF, lun, '*** NL function parameters ***', FORMAT='(A)'
  PRINTF, lun, 'Polynomial Degree:', sep, fct_dgr_str, FORMAT='(A,A,A)'
  PRINTF, lun, 'Parameters:', sep, fct_parms_str
  PRINTF, lun, 'Param. Sigma:', sep, fct_sigma_str
  PRINTF, lun, 'Max. dev. to zero of corrected values:', sep, corr_max_dev_to_zero
  PRINTF, lun, 'Cal_MR est. from TGTs:', sep, cal_est_MR, FORMAT='(A,A,A)'
  PRINTF, lun, 'Cal_MR est. abs. precision:', sep, cal_est_sig, FORMAT='(A,A,A)'
  PRINTF, lun, 'Max. dev. between fit and measured values:', sep, max_dev_to_fit
  PRINTF, lun, 'Cal in MR range of targets:', sep, cal_in_tgt_range
  PRINTF, lun, 'MR_unit:', sep, unit, sep, 'MR_scale:', sep, scale, FORMAT='(A,A,A,A,A,A,A)'
  PRINTF, lun, '*** Results per individual measurement ***', FORMAT='(A)'

  colheader = ['tgt_name', sep, 'tgt_mr_def', sep, 'lin_MR', sep, 'lin_MR_rsd', sep, 'delta_MR']
  ch_format = str_get_formcode('A', N_ELEMENTS(colheader))
  PRINTF, lun, colheader, FORMAT=ch_format

  data_format = str_get_formcode('A', 9)
  FOR i=0, N_ELEMENTS(tgts)-1 DO $ ; begin loop: target measurements
      PRINTF, lun, nl_strct[0].tgt_names[i], sep, nl_strct[0].tgt_mrs[i], sep, $
                   nl_strct[0].lin_mrs[i], sep, nl_strct[0].lin_mrs_rsd[i], sep, $
                   nl_strct[0].delta_mrs[i], FORMAT=data_format

  CLOSE, lun
  FREE_LUN, lun

END