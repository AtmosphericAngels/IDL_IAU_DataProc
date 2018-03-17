; ++++++ FUNCTION dp_read_calmrs ++++++ PRO dp_apply_calmrs ++++++ PRO dp_remv_calmrs
;+
; FUNCTION dp_read_calmrs
;
; AUTHOR: F. Obersteiner, Sep. 2016
;
; PURPOSE: reads a calibration gas mixing ratio table (.csv) into an IDL structure that is returned.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_read_calmrs, PATH=path, DEF_FILE=def_file, VERBOSE=verbose

  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  
  IF NOT KEYWORD_SET(def_file) THEN $
    file=DIALOG_PICKFILE(TITLE='Please select a Mixing Ratio Table.', PATH=path, FILTER='*.csv', /READ) $
      ELSE file=def_file
      
  IF file EQ '' THEN RETURN, !NULL
  IF FILE_TEST(file) EQ 0 THEN RETURN, !NULL
  
  sep = ';'
  n_table_header = 7
  nl = FILE_LINES(file)
  count = nl-n_table_header
  data = STRARR(nl)

  OPENR, LUN, file, /GET_LUN
  READF, LUN, data
  CLOSE, LUN
  FREE_LUN, LUN

  canister=((strsplit(STRTRIM(data[1]), ';', /EXTRACT))[1])

  substance = STRARR(count)
  mr_ppt    = DBLARR(count)
  unc_ppt   = DBLARR(count)
  unc_rel   = DBLARR(count)
  scale     = STRARR(count)
  comment   = STRARR(count)
  unit      = STRARR(count)

  FOR i=0, count-1 DO BEGIN
    tmp=strsplit(STRTRIM(data[i+n_table_header]), ';', /EXTRACT, /PRESERVE_NULL)
    substance[i] = tmp[0]
    mr_ppt[i]    = FIX(tmp[1], TYPE=5)
    unc_ppt[i]   = FIX(tmp[2], TYPE=5)
    unc_rel[i]   = FIX(tmp[3], TYPE=5)
    scale[i]     = tmp[4]
    comment[i]   = tmp[5]
    IF N_ELEMENTS(tmp) GT 6 THEN unit[i] = tmp[6]
  ENDFOR
  
  strct={ canister  : canister, $ 
          substance : substance, $
          mr_ppt    : mr_ppt, $
          unc_ppt   : unc_ppt, $
          unc_rel   : unc_rel, $
          scale     :   scale, $
          comment   : comment, $
          unit      : unit }
  
  RETURN, strct
  
END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_apply_calmrs
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: integrates loaded cal mixing ratio values into dp_data.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_apply_calmrs, sel_exp, mrs_strct, SEL_ONLY=sel_only, PATH=path, VERBOSE=verbose

  COMMON dp_data

  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  
  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))
  
  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1

  cal_names=[]
  FOR i=0, N_ELEMENTS(exps)-1 DO $
    cal_names = [cal_names, $
        ((dp_expcfg[exps[i]]).expinfo.s_name)[WHERE((dp_expcfg[exps[i]]).expinfo.s_id EQ id_cal AND $
                                                    (dp_expcfg[exps[i]]).expinfo.use EQ 1)]]

  ; check for uniq cal name
  IF N_ELEMENTS(uniq(STRUPCASE(cal_names))) GT 1 THEN BEGIN
    msg=[((dp_expcfg[sel_exp]).expinfo.expinfo_fname)[0], $
         'Multiple Cal names specified in the experiment. Cannot apply values.']
    !NULL=DIALOG_MESSAGE(msg, /ERROR)
    RETURN
  ENDIF
  
  canister=(mrs_strct.canister)[0]
  quest='Yes'
  IF cal_names[0] NE canister THEN $
    quest=DIALOG_MESSAGE('Calibration gas specification found in experiment-info file ('+cal_names[0]+') '+$
                          'and specification in MR Table ('+canister+') do not match. Continue?', /QUESTION, /DEFAULT_NO)
  IF quest EQ 'No' THEN $
    IF verbose THEN PRINT, 'aborted: calibration gas MRs integration.'
    
  IF quest EQ 'Yes' THEN BEGIN
    FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN
      tmp_expcfg=(dp_expcfg[exps[i]])
      tmp_expcfg.cal_mrs.canister = canister
      *tmp_expcfg.cal_mrs.substance = mrs_strct.substance
      *tmp_expcfg.cal_mrs.mr_ppt = mrs_strct.mr_ppt
      *tmp_expcfg.cal_mrs.unc_ppt = mrs_strct.unc_ppt
      *tmp_expcfg.cal_mrs.unc_rel = mrs_strct.unc_rel
      *tmp_expcfg.cal_mrs.scale = mrs_strct.scale
      *tmp_expcfg.cal_mrs.comment = mrs_strct.comment
      *tmp_expcfg.cal_mrs.unit = mrs_strct.unit
      (dp_expcfg[exps[i]])=TEMPORARY(tmp_expcfg)
    ENDFOR
    
    IF verbose THEN BEGIN
      print, '+++'
      print, 'imported cal mixing ratios.'
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
      print, 'Calibration gas MRs integrated.'
    ENDIF
    
    dp_refr_status, MESSAGE='Calibration gas MRs integrated.'
    
  ENDIF
      
END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_remv_calmrs
;
; AUTHOR: F. Obersteiner, Nov-2017
;
; PURPOSE: removes loaded cal mixing ratio values into dp_data.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_remv_calmrs, sel_exp, SEL_ONLY=sel_only, LOUD=loud

  COMMON dp_data
  
  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))
  
  FOR i=0, N_ELEMENTS(exps)-1 DO BEGIN  
    IF *(dp_expcfg[sel_exp]).cal_mrs.mr_ppt NE !NULL $
      THEN BEGIN
        quest='Yes'
        IF loud THEN quest=DIALOG_MESSAGE('Remove Cal MR values: are you sure?', /QUESTION)
        CASE quest OF
          'Yes': $
            BEGIN
              tmp_expcfg=(dp_expcfg[sel_exp])
              tmp_expcfg.cal_mrs.canister = ''
              *tmp_expcfg.cal_mrs.substance = !NULL
              *tmp_expcfg.cal_mrs.mr_ppt = !NULL
              *tmp_expcfg.cal_mrs.unc_ppt = !NULL
              *tmp_expcfg.cal_mrs.unc_rel = !NULL
              *tmp_expcfg.cal_mrs.scale = !NULL
              *tmp_expcfg.cal_mrs.comment = !NULL
              *tmp_expcfg.cal_mrs.unit = !NULL
              (dp_expcfg[sel_exp])=TEMPORARY(tmp_expcfg)
            END
          'No':
          ELSE:
        ENDCASE
      ENDIF 
  ENDFOR
  
END
