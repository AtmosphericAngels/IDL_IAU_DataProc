; ++++++ FUNCTION dp_read_tgtmrs ++++++ PRO dp_apply_tgtmrs ++++++ PRO dp_remv_tgtmrs
;+
; FUNCTION dp_read_tgtmrs
;
; AUTHOR: F. Obersteiner, June 2017
;
; PURPOSE: reads a target gas mixing ratio table (.csv) into an IDL structure that is returned.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_read_tgtmrs, PATH=path, DEF_FILE=def_file, VERBOSE=verbose

  IF NOT KEYWORD_SET(verbose) THEN verbose=0

  IF NOT KEYWORD_SET(def_file) THEN $
    file=DIALOG_PICKFILE(TITLE='Please select a Mixing Ratio Table.', PATH=path, FILTER='*.csv', /READ) $
  ELSE file=def_file

  IF file EQ '' THEN RETURN, !NULL
  IF FILE_TEST(file) EQ 0 THEN RETURN, !NULL

  sep = ';'
  n_table_header=7
  nl = FILE_LINES(file)
  count = nl-n_table_header
  data = STRARR(nl)

  OPENR, LUN, file, /GET_LUN
  READF, LUN, data
  CLOSE, LUN
  FREE_LUN, LUN

  tgt_name  = STRARR(count)
  substance = STRARR(count)
  mr_abs    = DBLARR(count)
  unc_abs   = DBLARR(count)
  unc_rel   = DBLARR(count)
  scale     = STRARR(count)
  comment   = STRARR(count)

  FOR i=0, count-1 DO BEGIN
    tmp=strsplit(STRTRIM(data[i+n_table_header]), ';', /EXTRACT)
    tgt_name[i] = tmp[1]
    substance[i] = tmp[2]
    mr_abs[i]    = FIX(tmp[3], TYPE=5)
    unc_abs[i]   = FIX(tmp[4], TYPE=5)
    unc_rel[i]   = FIX(tmp[5], TYPE=5)
    scale[i]     = tmp[6]
    comment[i]   = tmp[7]
  ENDFOR

  strct={ tgt_name: tgt_name, $
          substance : substance, $
          mr_abs : mr_abs, $
          unc_abs : unc_abs, $
          unc_rel : unc_rel, $
          scale :   scale, $
          comment : comment }

  RETURN, strct

END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_apply_tgtmrs
;
; AUTHOR: F. Obersteiner, June 2017
;
; PURPOSE: integrates target gas mixing ratio table into dp_data.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_apply_tgtmrs, sel_exp, tgt_strct, SEL_ONLY=sel_only, PATH=path, VERBOSE=verbose

  COMMON dp_data

  IF NOT KEYWORD_SET(verbose) THEN verbose=0

  IF KEYWORD_SET(sel_only) THEN exps = sel_exp $
    ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))

  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
    tmp_expcfg=(dp_expcfg[exps[i]])

    *tmp_expcfg.tgt_mrs.tgt_name = tgt_strct.tgt_name
    *tmp_expcfg.tgt_mrs.substance = tgt_strct.substance
    *tmp_expcfg.tgt_mrs.mr_ppt = tgt_strct.mr_abs
    *tmp_expcfg.tgt_mrs.unc_ppt = tgt_strct.unc_abs
    *tmp_expcfg.tgt_mrs.unc_rel = tgt_strct.unc_rel
    *tmp_expcfg.tgt_mrs.scale = tgt_strct.scale
    *tmp_expcfg.tgt_mrs.comment = tgt_strct.comment

    (dp_expcfg[exps[i]])=TEMPORARY(tmp_expcfg)
  ENDFOR

  IF verbose THEN BEGIN
    print, '+++'
    print, 'imported tgt mixing ratios.'
    def_subst=substlist[sel_exp]
    w=REPLICATE(-1L, N_ELEMENTS(def_subst))
    FOR i=0L, N_ELEMENTS(def_subst)-1 DO $
      IF (WHERE(substance EQ def_subst[i]))[0] NE -1 THEN w[i]=(WHERE(substance EQ def_subst[i]))[0]
    w1=WHERE(w EQ -1, nvd, NCOMPLEMENT=ncomplement)
    print, 'found n matches: ', ncomplement
    missing=STRARR(N_ELEMENTS(w1))
    missing=def_subst[w1]
    print, 'missing: '
    print, missing
    print, '+++'
    print, 'Target gas MRs integrated.'
  ENDIF

  dp_refr_status, MESSAGE='Target gas MRs integrated.'

END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_remv_tgtmrs
;
; AUTHOR: F. Obersteiner, Nov-2017
;
; PURPOSE: removes loaded tgt mixing ratio values into dp_data.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_remv_tgtmrs, sel_exp, LOUD=loud

  COMMON dp_data

  IF *(dp_expcfg[sel_exp]).tgt_mrs.mr_ppt NE !NULL $
    THEN BEGIN
    quest='Yes'
    IF loud THEN quest=DIALOG_MESSAGE('Remove tgt MR values: are you sure?', /QUESTION)
    CASE quest OF
      'Yes': $
        BEGIN
        tmp_expcfg=(dp_expcfg[sel_exp])
        *tmp_expcfg.tgt_mrs.tgt_name = !NULL
        *tmp_expcfg.tgt_mrs.substance = !NULL
        *tmp_expcfg.tgt_mrs.mr_ppt = !NULL
        *tmp_expcfg.tgt_mrs.unc_ppt = !NULL
        *tmp_expcfg.tgt_mrs.unc_rel = !NULL
        *tmp_expcfg.tgt_mrs.scale = !NULL
        *tmp_expcfg.tgt_mrs.comment = !NULL
        (dp_expcfg[sel_exp])=TEMPORARY(tmp_expcfg)
      END
      'No':
      ELSE:
    ENDCASE
  ENDIF

END