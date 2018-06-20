;+
; PRO dp_read_cocorrparms
;
; AUTHOR: F. Obersteiner, Feb. 2018
;
; PURPOSE: implements carry over correction parameters in dp_expcfg.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_read_cocorrparms, sel_exp, SEL_ONLY=sel_only, PATH=path, DEF_FILE=def_file, VERBOSE=verbose

  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  
  COMMON dp_data

  IF *(dp_expcfg[sel_exp]).carryover.substance NE !NULL $
    THEN quest=DIALOG_MESSAGE('Carry-over correction found: overwrite?', /QUESTION) $
  ELSE quest='Yes'
  IF quest EQ 'No' THEN RETURN
  
  IF NOT KEYWORD_SET(def_file) THEN $
    file=DIALOG_PICKFILE(TITLE='Please select a Carry-Over config file.', PATH=path, FILTER='*.csv', /READ) $
  ELSE file=def_file

  IF file EQ '' THEN RETURN
  IF FILE_TEST(file) EQ 0 THEN RETURN
  
  sep = ';'
  n_table_header = 7
  nl = FILE_LINES(file)
  n_subst = nl-n_table_header
  str = STRARR(nl)
  
  substance = STRARR(n_subst)
  cal_to_sam = DBLARR(2,n_subst)
  sam_to_sam = DBLARR(2,n_subst)
  sam_to_cal = DBLARR(2,n_subst)
    
  OPENR, LUN, file, /GET_LUN
  READF, LUN, str
  CLOSE, LUN
  FREE_LUN, LUN
  
  header = str[0:n_table_header-1]
  parms = str[n_table_header:-1]
  
  FOR i=0, n_subst-1 DO BEGIN
    tmp=strsplit(STRTRIM(parms[i]), sep, /EXTRACT, /PRESERVE_NULL)
    substance[i] = tmp[0]
    cal_to_sam[*,i] = [FIX(tmp[1], TYPE=5), FIX(tmp[2], TYPE=5)]
    sam_to_sam[*,i] = [FIX(tmp[3], TYPE=5), FIX(tmp[4], TYPE=5)]
    sam_to_cal[*,i] = [FIX(tmp[5], TYPE=5), FIX(tmp[6], TYPE=5)]
  ENDFOR
    
  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))
  
  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    tmp_expcfg=(dp_expcfg[exps[i]])
    *tmp_expcfg.carryover.substance = substance
    tmp_expcfg.carryover.comment = header[0:-2]
    *tmp_expcfg.carryover.cal_to_sam = cal_to_sam
    *tmp_expcfg.carryover.sam_to_sam = sam_to_sam
    *tmp_expcfg.carryover.sam_to_cal = sam_to_cal
    (dp_expcfg[exps[i]])=TEMPORARY(tmp_expcfg)
  
    tmp_chrom=(dp_chrom[exps[i]])
    ; check for which species carry over correction is defined
      exp_subst = tmp_chrom[0].subst.name[0]
      def_subst = substance
      match_ix = arr1D_get_matchIX(exp_subst, def_subst)
      IF match_ix[0] NE -1 THEN $
        tmp_chrom.subst[match_ix].rres.active_corr[0] = !TRUE
    (dp_chrom[exps[i]])=TEMPORARY(tmp_chrom)
  ENDFOR
  
END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_remv_cocorr
;
; AUTHOR: F. Obersteiner, Feb-2018
;
; PURPOSE: removes carry over correction (config and correction flag in rres.active_corr[0].
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_remv_cocorr, sel_exp, SEL_ONLY=sel_only, LOUD=loud

  COMMON dp_data

  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))

  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    IF *(dp_expcfg[sel_exp]).carryover.substance NE !NULL $
      THEN BEGIN
        IF loud THEN quest=DIALOG_MESSAGE('Remove carry-over correction from selected experiment: are you sure?', /QUESTION) $
          ELSE  quest='Yes'
        CASE quest OF
          'Yes': $
            BEGIN
              tmp_chrom = (dp_chrom[sel_exp])
              tmp_expcfg = (dp_expcfg[sel_exp])
              
              empty_strct = create_ref_cocorr()
              tmp_expcfg.carryover.substance = empty_strct.substance
              tmp_expcfg.carryover.comment = empty_strct.comment
              tmp_expcfg.carryover.cal_to_sam = empty_strct.cal_to_sam
              tmp_expcfg.carryover.sam_to_sam = empty_strct.sam_to_sam
              tmp_expcfg.carryover.sam_to_cal = empty_strct.sam_to_cal
              
              tmp_chrom.subst[*].rres[*].active_corr[0] = !FALSE
                  
              (dp_expcfg[sel_exp]) = TEMPORARY(tmp_expcfg)
              (dp_chrom[sel_exp]) = TEMPORARY(tmp_chrom)
            END
          'No': RETURN
          ELSE:
        ENDCASE
    ENDIF ELSE $
      IF loud THEN BEGIN
        msg=DIALOG_MESSAGE('No carry-over config found for selected experiment.', /INFORMATION)
        RETURN
      ENDIF
  ENDFOR
  
END
