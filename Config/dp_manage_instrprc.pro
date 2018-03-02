; ++++++ FUNCTION dp_read_instrprc ++++++ PRO dp_apply_instrprc ++++++ PRO dp_remv_instrprc
;+
; FUNCTION dp_read_instrprc
;
; AUTHOR: F. Obersteiner, Sep. 2016
;
; PURPOSE: reads an instrument precision table (.csv) into an IDL structure that is returned.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_read_instrprc, sel_exp, exp_name, substlist, PATH=path, DEF_FILE=def_file, VERBOSE=verbose

  IF NOT KEYWORD_SET(verbose) THEN verbose=0

  IF NOT KEYWORD_SET(def_file) THEN $
    file=DIALOG_PICKFILE(TITLE='Please select a Precision Table.', PATH=path, FILTER='*.csv', /READ) $
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

  instrument=((strsplit(STRTRIM(data[1]), ';', /EXTRACT))[1])

  substance = STRARR(count)
  mp_abs    = DBLARR(count)
  mp_rel    = DBLARR(count)
  lod       = DBLARR(count)
  comment   = STRARR(count)

  FOR i=0, count-1 DO BEGIN
    tmp=strsplit(STRTRIM(data[i+n_table_header]), ';', /EXTRACT)
    substance[i] = tmp[0]
    mp_abs[i]    = FIX(tmp[1], TYPE=5)
    mp_rel[i]    = FIX(tmp[2], TYPE=5)
    lod[i]       = FIX(tmp[3], TYPE=5)
    comment[i]   = tmp[4]
  ENDFOR

  strct={ instrument : instrument, $
    substance : substance, $
    mp_abs : mp_abs, $
    mp_rel : mp_rel, $
    lod : lod, $
    comment : comment }

  IF verbose THEN BEGIN
    print, '+++'
    print, 'imported instrument precision values for: '+exp_name
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
  ENDIF

  RETURN, strct

END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_apply_instrprc
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: integrates loaded instrument precision values into dp_data and calculates precision flags.
;
; MODIFIED: 17-08-14 (FO), - added keyword QUIET to suppress dialog message and use supplied variable
;                            'prc_strct' in any case
;                          - corrected if statement 'IF KEYWORD_SET(prc_strct)...'
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_apply_instrprc, PRC_STRCT=prc_strct, sel_exp, SEL_ONLY=sel_only, QUIET=quiet, VERBOSE=verbose

  COMMON dp_data

  ok = prc_limits[0]; range definition
  bad = prc_limits[1]

  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  
  IF KEYWORD_SET(sel_only) THEN exps = sel_exp ELSE exps = LINDGEN(N_ELEMENTS(dp_chrom))

  FOR n=0, N_ELEMENTS(exps)-1 DO BEGIN
    tmp_chrom=(dp_chrom[exps[n]]) ; move data out of list...
    tmp_expcfg=(dp_expcfg[exps[n]])
  
    IF KEYWORD_SET(prc_strct) THEN BEGIN ; if variable supplied, update values in expcfg struct
  
      quest='Yes'
  
      IF NOT KEYWORD_SET(quiet) THEN BEGIN
        IF STRUPCASE(instr) NE STRUPCASE(prc_strct.instrument) THEN $
          quest=DIALOG_MESSAGE('Instrument specification does not match. Continue?', /QUESTION, /DEFAULT_CANCEL)
      ENDIF
  
      IF quest EQ 'Yes' THEN BEGIN
        
        tmp_expcfg.instr_prc.instrument = prc_strct.instrument
        *tmp_expcfg.instr_prc.substance = prc_strct.substance
        *tmp_expcfg.instr_prc.mp_ppt = prc_strct.mp_abs
        *tmp_expcfg.instr_prc.mp_rel = prc_strct.mp_rel
        *tmp_expcfg.instr_prc.lod = prc_strct.lod
        *tmp_expcfg.instr_prc.comment = prc_strct.comment
        IF verbose THEN print, 'instrument precision values: integrated.'
        
      ENDIF
  
    ENDIF
  
    def_subst=tmp_chrom[0].subst.name ; begin to apply...
    n_subst=N_ELEMENTS(def_subst)
    n_chrom=N_ELEMENTS(tmp_chrom.subst[0].rres.rsp_area.block_rsd)
    tmp_prc_flag=LONARR(n_chrom)
  
    FOR i=0, n_subst-1 DO BEGIN ; check values for each substance
      eval_mode = (tmp_chrom.subst[i].rres.rsp_select)[0]
      w=(WHERE(*tmp_expcfg.instr_prc.substance EQ def_subst[i]))[0]
      IF w NE -1 THEN BEGIN
        CASE eval_mode OF
          0:  BEGIN
            tmp_chrom.subst[i].rres.rsp_area.sys_prc = (*tmp_expcfg.instr_prc.mp_rel)[w]
            rsd_ix = WHERE(FINITE(tmp_chrom.subst[i].rres.rsp_area.block_rsd) EQ 1, n_finite)
            rsd = (tmp_chrom.subst[i].rres.rsp_area.block_rsd)[rsd_ix]
            tgt = ((*tmp_expcfg.instr_prc.mp_rel)[w])[0]
            w_ok = WHERE(rsd LE ok*tgt, n_ok)
            w_bad = WHERE(rsd LE bad*tgt AND rsd GT ok*tgt, n_bad)
            w_poor = WHERE(rsd GT bad*tgt, n_poor)
            IF w_ok[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_ok]] = 1
            IF w_bad[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_bad]] = -1
            IF w_poor[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_poor]] = -2
            tmp_chrom.subst[i].rres.rsp_area.prc_flag = tmp_prc_flag
          END
          1:  BEGIN
            tmp_chrom.subst[i].rres.rsp_height.sys_prc = (*tmp_expcfg.instr_prc.mp_rel)[w]
            rsd_ix = WHERE(FINITE(tmp_chrom.subst[i].rres.rsp_height.block_rsd) EQ 1, n_finite)
            rsd = (tmp_chrom.subst[i].rres.rsp_height.block_rsd)[rsd_ix]
            tgt = ((*tmp_expcfg.instr_prc.mp_rel)[w])[0]
            w_ok = WHERE(rsd LE ok*tgt, n_ok)
            w_bad = WHERE(rsd LE bad*tgt AND rsd GT ok*tgt, n_bad)
            w_poor = WHERE(rsd GT bad*tgt, n_poor)
            IF w_ok[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_ok]] = 1
            IF w_bad[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_bad]] = -1
            IF w_poor[0] NE -1 THEN tmp_prc_flag[rsd_ix[w_poor]] = -2
            tmp_chrom.subst[i].rres.rsp_height.prc_flag = tmp_prc_flag
          END
          ELSE:
        ENDCASE
      ENDIF
    ENDFOR
  
    (dp_expcfg[exps[n]])=TEMPORARY(tmp_expcfg)
    (dp_chrom[exps[n]])=TEMPORARY(tmp_chrom) ; ...move data back into list.
  ENDFOR
  
  IF verbose THEN print, 'Instrument precision values: applied.'
  dp_refr_status, MESSAGE='Instrument precision values: applied.'

END
;------------------------------------------------------------------------------------------------------------------------
;+
; PRO: dp_remv_instrprc
;
; AUTHOR: F. Obersteiner, Nov-2017
;
; PURPOSE: removes loaded cal mixing ratio values into dp_data.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_remv_instrprc, sel_exp, LOUD=loud

  COMMON dp_data

  IF *(dp_expcfg[sel_exp]).instr_prc.mp_ppt NE !NULL $
    THEN BEGIN
    quest='Yes'
    IF loud THEN quest=DIALOG_MESSAGE('Remove instrument prc values: are you sure?', /QUESTION)
    CASE quest OF
      'Yes': $
        BEGIN
        n_subst=N_ELEMENTS((dp_chrom[sel_exp])[0].subst.name)

        tmp_chrom=(dp_chrom[sel_exp])
        n_chrom=N_ELEMENTS(tmp_chrom.subst[0].rres.rsp_area.block_rsd)

        tmp_expcfg=(dp_expcfg[sel_exp])
        tmp_expcfg.instr_prc.instrument = ''
        *tmp_expcfg.instr_prc.substance = !NULL
        *tmp_expcfg.instr_prc.mp_ppt = !NULL
        *tmp_expcfg.instr_prc.mp_rel = !NULL
        *tmp_expcfg.instr_prc.lod = !NULL
        *tmp_expcfg.instr_prc.comment = !NULL

        FOR n=0, n_subst-1 DO BEGIN
          tmp_chrom.subst[n].rres.rsp_area.sys_prc = DBLARR(n_chrom)*!VALUES.D_NAN
          tmp_chrom.subst[n].rres.rsp_area.prc_flag = INTARR(n_chrom)
          tmp_chrom.subst[n].rres.rsp_height.sys_prc = DBLARR(n_chrom)*!VALUES.D_NAN
          tmp_chrom.subst[n].rres.rsp_height.prc_flag = INTARR(n_chrom)
        ENDFOR

        (dp_expcfg[sel_exp])=TEMPORARY(tmp_expcfg)
        (dp_chrom[sel_exp])=TEMPORARY(tmp_chrom) ; ...move data back into list.
      END
      'No':
      ELSE:
    ENDCASE
  ENDIF

END
