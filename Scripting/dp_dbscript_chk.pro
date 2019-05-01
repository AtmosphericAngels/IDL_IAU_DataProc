;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-08, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; check a iau_dataproc database script for consistency. no news are good news.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_dbscript_chk, dp_dbstrct, LOUD=loud, N_TESTED=n_tested

  ; file approved if n_tested=n_passed
  n_tested = 0
  n_passed = 0

  ; argument type ok?
  n_tested = n_tested + 1
  IF dp_dbstrct EQ !NULL OR SIZE(dp_dbstrct, /TYPE) NE 8 THEN RETURN, n_passed
  n_passed = n_passed + 1

  ; set up some variables...
  msg = []
  no_exp = 0
  no_data = 0
  no_expinfo = 0
  no_treat = 0
  no_calmrs = 0
  no_tgtmrs = 0
  no_prc = 0
  no_namedef = 0
  n_exp = N_ELEMENTS(dp_dbstrct.data)

  ; header enough lines?
  n_tested = n_tested + 1
  IF N_ELEMENTS(dp_dbstrct.header) LT 6 THEN $
    msg = [msg, '- not enough header lines.'] $
      ELSE  n_passed = n_passed + 1

  ; experiment defined?
  n_tested = n_tested + 1
  IF n_exp EQ 0 OR dp_dbstrct.data[0].active EQ -1 THEN BEGIN
    msg = [msg, '- no experiment defined.']
    no_exp = 1
  ENDIF ELSE n_passed = n_passed + 1

  IF NOT no_exp THEN BEGIN
    ; exp_id unique?
    n_tested = n_tested + 1
    str=dp_dbstrct.data.exp_id
    str[uniq(str)]='-unique-'
    w_not_uniq=WHERE(str NE '-unique-', nvd)
    IF nvd GT 0 THEN BEGIN
      msg = [msg, '- found double experiment id(s):']
      FOR i=0, nvd-1 DO msg = [msg, '--> '+str[w_not_uniq[i]]]
    ENDIF ELSE n_passed = n_passed + 1

    ; data_import_fct is allowed?
    n_tested = n_tested + 1
    str=dp_dbstrct.data.EXPINFO_IMPORT_FCT
    def_fct=['Lab_BenchTOF','Lab_QP/SFMS','FASTOF','GhOST_MS','GhOST_ECD','AED','GHGGC_ECD/FID']
    str_match = LONARR(N_ELEMENTS(str))
    FOR i=0, N_ELEMENTS(str)-1 DO BEGIN
      str_match[i]=MAX(STRMATCH(def_fct, str[i]))
      IF NOT str_match[i] THEN $
        msg = [msg, '- found not allowed import function(s):', '--> '+str[i]]
    ENDFOR
    IF MIN(str_match) THEN n_passed = n_passed + 1

    n_tested = n_tested + 7
    FOR n=0, n_exp-1 DO BEGIN
      IF dp_dbstrct.data[n].active THEN BEGIN
        ; chrom file at spec. destination?
        exp_data = dp_dbstrct.data[n].CHROMDATA_PATH
        IF NOT FILE_TEST(exp_data) THEN BEGIN
          msg = [msg, '- chrom file not found:', '--> '+exp_data]
          no_data = no_data + 1
        ENDIF

        ; expinfo_path points to file?
        exp_cnfg = dp_dbstrct.data[n].EXPINFO_PATH
        IF NOT FILE_TEST(exp_cnfg) THEN BEGIN
          msg = [msg, '- expinfo file not found:', '--> '+exp_cnfg]
          no_expinfo = no_expinfo + 1
        ENDIF

        ; file found at DP_TREATCFG_PATH?
        treat_cfg_path = dp_dbstrct.data[n].DP_TREATCFG_PATH
        IF STRLEN(treat_cfg_path) NE 0 THEN BEGIN
          IF NOT FILE_TEST(treat_cfg_path) THEN BEGIN
            msg = [msg, '- treat_cfg not found:', '--> '+treat_cfg_path]
            no_treat = no_treat + 1
          ENDIF
        ENDIF

        ; file found at DP_CALMRS_PATH?
        cal_mrs_path = dp_dbstrct.data[n].DP_CALMRS_PATH
        IF STRLEN(cal_mrs_path) NE 0 THEN BEGIN
          IF NOT FILE_TEST(cal_mrs_path) THEN BEGIN
            msg = [msg, '- cal_mrs not found:', '--> '+cal_mrs_path]
            no_calmrs = no_calmrs + 1
          ENDIF
        ENDIF

        ; file found at DP_TGTMRS_PATH?
        tgt_mrs_path = dp_dbstrct.data[n].DP_TGTMRS_PATH
        IF STRLEN(tgt_mrs_path) NE 0 THEN BEGIN
          IF NOT FILE_TEST(tgt_mrs_path) THEN BEGIN
            msg = [msg, '- tgt_mrs not found:', '--> '+tgt_mrs_path]
            no_tgtmrs = no_tgtmrs + 1
          ENDIF
        ENDIF

        ; file found at DP_INSTR_PRC_PATH?
        instr_prc_path = dp_dbstrct.data[n].DP_INSTR_PRC_PATH
        IF STRLEN(instr_prc_path) NE 0 THEN BEGIN
          IF NOT FILE_TEST(instr_prc_path) THEN BEGIN
            msg = [msg, '- instr_prc not found:', '--> '+instr_prc_path]
            no_prc = no_prc + 1
          ENDIF
        ENDIF

  	  ; file found at SUBST_NAMEDEF_PATH?
        subst_namedef_path = dp_dbstrct.data[n].subst_namedef_path
        IF STRLEN(subst_namedef_path) NE 0 THEN BEGIN
          IF NOT FILE_TEST(subst_namedef_path) THEN BEGIN
            msg = [msg, '- subst_namedef not found:', '--> '+subst_namedef_path]
            no_namedef = no_namedef + 1
          ENDIF
        ENDIF
      ENDIF ; end if: active
    ENDFOR

    IF no_data EQ 0 THEN  n_passed += 1
    IF no_expinfo EQ 0 THEN  n_passed += 1
    IF no_treat EQ 0 THEN  n_passed += 1
    IF no_calmrs EQ 0 THEN  n_passed += 1
    IF no_tgtmrs EQ 0 THEN  n_passed += 1
    IF no_prc EQ 0 THEN  n_passed += 1
    IF no_namedef EQ 0 THEN  n_passed += 1

  ENDIF

  ; error reporting
  IF N_ELEMENTS(msg) GT 0 THEN $
    IF NOT KEYWORD_SET(loud) THEN FOR i=0, N_ELEMENTS(msg)-1 DO print, msg[i] $
      ELSE popup=DIALOG_MESSAGE(msg, /ERROR)

  RETURN, n_passed

END