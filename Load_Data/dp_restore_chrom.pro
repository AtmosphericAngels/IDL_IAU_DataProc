;------------------------------------------------------------------------------------------------------------------------
;+
; AUTHORS
; S.Sala, F.Obersteiner, STRCT_Redefine_Tag: H.Boenisch
;
; PURPOSE
; restore an IAU_Chrom experiment, remove raw data (only keep integration results) and add dataproc-specific
; (sub-)structures.
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_restore_chrom, dp_chrom, current_version, PATH=path_wd, FNAME=fname, VERBOSE=verbose

  dp_refr_status, MESSAGE='Restoring IAU_Chrom data...'
  IF NOT KEYWORD_SET(verbose) THEN verbose = 0
  IF verbose THEN print, 'beginning to restore experiment(s)...'

  IF NOT KEYWORD_SET(fname) THEN $
    fname = DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path_wd, FILTER='*.dat', /FIX_FILTER, $
                            TITLE='Please select CHROM-file(s) to process.')

  IF fname[0] EQ '' THEN BEGIN
    dp_refr_status, /CLEAR
    IF SIZE(dp_chrom, /TYPE) EQ 11 THEN RETURN, dp_chrom ELSE RETURN, !NULL
  ENDIF

  dp_chrom = !NULL
  instr_type = !NULL
  exp_fname = STRARR(N_ELEMENTS(fname))

  FOR i=0, N_ELEMENTS(fname)-1 DO BEGIN             ; i-loop: number of selected chrom-files
    exp_fname(i) = FILE_BASENAME(fname[i])

    RESTORE, fname[i] ; gives structure "chrom"...

    IF chrom EQ !NULL THEN BEGIN
      msg=DIALOG_MESSAGE('Unable to restore. Please select valid IAU_Chrom save file.', /INFORMATION)
      dp_refr_status, /CLEAR
      RETURN, dp_chrom
    ENDIF

    m_chrom = !NULL ; empty temporary structures; "_m" = modified
    new_chrom = !NULL
                                                      ; modify the restored chrom structure...
    FOR j=0, N_ELEMENTS(chrom)-1 DO BEGIN           ; j-loop: elements of chrom, i.e. number of chromatograms
      instr_type=[instr_type, chrom[j].instr_type]
      tmp=DICTIONARY(chrom[j]) ; convert element j of chrom to dictionary to remove tags 'time','mass','intensity'
      tmp.remove, ['time','mass','intensity']
      m_chrom=[m_chrom, tmp.tostruct()]         ; convert back to structure / =modified chrom, "raw data" removed

      tmp0=STRCT_Redefine_Tag(m_chrom[j], TAG_NAME="exp_fname", TAG_DEF=exp_fname(i))
      tmp0=STRCT_Redefine_Tag(tmp0, TAG_NAME="iaudp_vers", TAG_DEF=FIX(current_version, TYPE=4))

      new_subst = !NULL

      ref_rres = create_ref_rres()

        FOR k=0, N_ELEMENTS(m_chrom[j].subst)-1 DO BEGIN        ; k-loop elements of subst in each chrom
          tmp1=STRCT_Redefine_Tag(tmp0.subst[k], Tag_Name="rres", Tag_Def=ref_rres) ; tmp1: subst level [n_chrom, n_subst]
          new_subst=[new_subst,tmp1]
        ENDFOR

      tmp0=STRCT_Redefine_Tag(tmp0, Tag_Name="subst", Tag_Def=new_subst)

      new_chrom=[new_chrom, tmp0]

    ENDFOR ; end loop: chromatograms in restored .sav file

    IF i EQ 0 THEN chromlist=LIST(new_chrom) ELSE chromlist.add, new_chrom

    IF verbose THEN print, 'restored experiment: ', exp_fname(i)

  ENDFOR ; end loop: selected .sav files


  IF N_ELEMENTS(instr_type[uniq(instr_type)]) GT 1 THEN BEGIN
    msg=DIALOG_MESSAGE('Instrument types of experiments do not match. Aborted.', /ERROR)
    chromlist = !NULL
  ENDIF

  dp_refr_status, MESSAGE='Data restored.'

  RETURN, chromlist; return a list of all loaded experiments

END