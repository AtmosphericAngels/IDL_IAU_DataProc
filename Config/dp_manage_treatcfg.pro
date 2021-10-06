;+
; PRO: dp_read_treatcfg
;
; AUTHOR: F. Obersteiner, Oct-2016
;
; PURPOSE: read config file to configure cal and sample treatment methods for each substance.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_read_treatcfg, sel_exp, SEL_ONLY=sel_only, FILE=file, VERBOSE=verbose

  COMMON DP_DATA

  IF NOT KEYWORD_SET(verbose) THEN verbose = 0

  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))

  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    finvals = FINITE((dp_chrom[exps[i]]).subst[*].rres.rsp_area.normalised)
    finvals=REFORM(finvals, N_ELEMENTS(finvals))
    w = WHERE(finvals NE 0)
    IF w[0] NE -1 THEN BEGIN
      quest=DIALOG_MESSAGE('Results found. These will be overwritten. Continue?', /QUESTION)
      IF quest EQ 'No' THEN RETURN
    ENDIF
  ENDFOR

  IF NOT KEYWORD_SET(file) THEN $
    file=DIALOG_PICKFILE(PATH=path_wd, TITLE='Please select dp_treatmthd.info file.', FILTER='*.info')

  IF file EQ '' THEN RETURN
  IF FILE_TEST(file) EQ 0 THEN RETURN

  sep = STRING(9B)
  nl = FILE_LINES(file)
  data = STRARR(nl)
  nl_hdr = 5
  n_subst = nl-nl_hdr

  OPENR, LUN, file, /GET_LUN
  READF, LUN, data
  CLOSE, LUN
  FREE_LUN, LUN

  substance = STRARR(n_subst)
  treat_cal = STRARR(n_subst)
  treat_sam = STRARR(n_subst)
  ip_cal = STRARR(n_subst)
  eval_mode = INTARR(n_subst)

  FOR i=0, n_subst-1 DO BEGIN
    tmp = strsplit(STRTRIM(data[i+nl_hdr]), sep, /EXTRACT)
    substance[i] = tmp[0]
    treat_cal[i] = tmp[1]
    treat_sam[i] = tmp[2]
    ip_cal[i] = tmp[3]
    IF N_ELEMENTS(tmp) GE 5 THEN BEGIN
      CASE STRUPCASE(tmp[4]) OF
        'AREA': eval_mode[i] = 0
        'HEIGHT': eval_mode[i] = 1
        ELSE: eval_mode[i] = -1
      ENDCASE
    ENDIF
  ENDFOR

  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    tmp_expcfg = (dp_expcfg[exps[i]])
    *tmp_expcfg.treatcfg.substance = substance
    *tmp_expcfg.treatcfg.cal_treat = treat_cal
    *tmp_expcfg.treatcfg.sam_treat = treat_sam
    *tmp_expcfg.treatcfg.cal_ip = ip_cal
    *tmp_expcfg.treatcfg.eval_mode = eval_mode
    (dp_expcfg[exps[i]]) = TEMPORARY(tmp_expcfg)
  ENDFOR

  dp_refr_status, MESSAGE='Imported treatment config.'

END
;------------------------------------------------------------------------------------------------------------------------
;+
; FUNCTION dp_remv_treatcfg
;
; AUTHOR: F. Obersteiner, Nov 2017
;
; PURPOSE: resets loaded treatcfg to default.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_remv_treatcfg, sel_exp, SEL_ONLY=sel_only, LOUD=loud

  COMMON DP_DATA

  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))
  IF NOT KEYWORD_SET(loud) THEN loud = 0

  IF loud THEN BEGIN
    FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
      finvals = FINITE((dp_chrom[exps[i]]).subst[*].rres.rsp_area.normalised)
      finvals=REFORM(finvals, N_ELEMENTS(finvals))
      w = WHERE(finvals NE 0)
      IF w[0] NE -1 THEN BEGIN
        quest=DIALOG_MESSAGE('Results found. These will be overwritten. Continue?', /QUESTION)
        IF quest EQ 'No' THEN RETURN
      ENDIF
    ENDFOR
  ENDIF

  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    tmp_expcfg = (dp_expcfg[exps[i]])
    *tmp_expcfg.treatcfg.substance = !NULL
    *tmp_expcfg.treatcfg.cal_treat = !NULL
    *tmp_expcfg.treatcfg.sam_treat = !NULL
    *tmp_expcfg.treatcfg.cal_ip = !NULL
    *tmp_expcfg.treatcfg.eval_mode = !NULL
    (dp_expcfg[exps[i]]) = TEMPORARY(tmp_expcfg)
  ENDFOR

END
;------------------------------------------------------------------------------------------------------------------------
;+
; NAME: dp_export_treatconfig
;
; AUTHOR: F.Obersteiner, Oct-2016
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_export_treatconfig, sel_exp, PATH=path

  COMMON dp_data

  tmp_data = (dp_chrom[sel_exp])
  n_chrom = N_ELEMENTS(tmp_data)
  sel_chrom = FLOOR(n_chrom/2.)
  n_subst = N_ELEMENTS(tmp_data[sel_chrom].subst.name)
  evm_strings = ['area', 'height']

  caldat, SYSTIME(/JULIAN), mm,dd,yy,hh,mn,ss
  datestr = STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)') $
           +STRING(hh,format='(I02)')+STRING(mn,format='(I02)')
  fname = DIALOG_PICKFILE(PATH=path, /WRITE, /OVERWRITE_PROMPT, FILE=datestr+'_dp_treatmthd.info')
  IF STRLEN(fname) EQ 0 THEN RETURN

  sep = STRING(9B)

  OPENW, lun, fname, /GET_LUN
  PRINTF,lun, 'Configuration of cal and sample treatment within IAU_DataProc', FORMAT='(A)'
  PRINTF,lun, 'Cal_Treat options:', sep, 'bracketing, block_mean, preceding', FORMAT='(A,A,A)'
  PRINTF,lun, 'Sam_Treat options:', sep, 'block_mean, individual, block_last_n (n depending on block size)', FORMAT='(A,A,A)'
  PRINTF,lun, 'Cal_IP options:', sep, 'p2p, calsmean, linear_fit, polyfit_dg2', FORMAT='(A,A,A)'
  PRINTF,lun, 'Substance', sep, 'Cal_Treat', sep, 'Sam_Treat', sep, 'Cal_IP', sep, 'Eval_Mode', FORMAT='(A,A,A,A,A,A,A,A,A)'

  FOR i=0, n_subst-1 DO BEGIN
    name = tmp_data[sel_chrom].subst[i].name
    cal_ip = tmp_data[sel_chrom].subst[i].rres.cal_ip_mthd
    cal_treat = tmp_data[sel_chrom].subst[i].rres.cal_treat
    sam_treat = tmp_data[sel_chrom].subst[i].rres.sam_treat
    eval_mode = evm_strings[tmp_data[sel_chrom].subst[i].rres.rsp_select]
    PRINTF,lun, name, sep, Cal_Treat, sep, Sam_Treat, sep, Cal_IP, sep, eval_mode, FORMAT='(A,A,A,A,A,A,A,A,A)'
  ENDFOR

  CLOSE, lun
  FREE_LUN, lun

  dp_refr_status, MESSAGE='Treat. config exported.'

END
