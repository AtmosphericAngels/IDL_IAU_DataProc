;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED:
; 17-06, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE of functions:
; dp_version_check: check the version of dataproc that was used to generate the save-file
; dp_gen_empty_rres: generate an emtpy list of structures using current reference structures.
; dp_strct2current_chrom: assign the restored structure (old version) to the new structure.
; dp_gen_empty_expcfg: same as dp_gen_empty_chrom, but for expcfg list.
; dp_strct2current_expcfg: same as dp_strct2current_chrom, but for expcfg list.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_version_check, dp_chrom_list, VCHECK_VERSION=vcheck_version, VERS_TAG=vers_tag

  IF NOT KEYWORD_SET(vcheck_version) THEN vcheck_version = 1.27
  IF NOT KEYWORD_SET(vers_tag) THEN vers_tag = 'IAUDP_VERS'

  version = 0.
  vcheck = 0
  tmp_chrom0 = dp_chrom_list[0]
  
  w_verstag = WHERE(STRUPCASE(TAG_NAMES(tmp_chrom0)) EQ vers_tag, w_no_verstag)

  IF w_no_verstag EQ 0 THEN vcheck = -1 $ ; not even a version tag found...
    ELSE version = FIX(tmp_chrom0[0].iaudp_vers, TYPE=4)

  IF version LE vcheck_version THEN vcheck = vcheck ELSE vcheck = 1
  
  RETURN, vcheck

END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_gen_empty_rres, old_dp_chrom_strct, vcheck, current_version

  ; get some stats...
  n_chroms = N_ELEMENTS(old_dp_chrom_strct)
  n_subst = N_ELEMENTS(old_dp_chrom_strct[0].subst)
  
  ; the sub-structure thats actually redefined empty...
  ref_rres=create_ref_rres()
  new_subst=!NULL

  tmp1=[]
  
  IF vcheck EQ -1 THEN BEGIN
    FOR i=0, n_chroms-1 DO BEGIN
      tmp0=STRCT_Redefine_Tag(old_dp_chrom_strct[i], TAG_NAME="iaudp_vers", TAG_DEF=FIX(current_version, TYPE=4))
      tmp1=[tmp1, tmp0]
    ENDFOR
  ENDIF ELSE tmp1 = old_dp_chrom_strct
  
  dp_chrom_empty_rres = []
  
  FOR i=0, n_chroms-1 DO BEGIN ; for each chromatogram...
    
    new_subst=[]
    
    FOR j=0, n_subst-1 DO BEGIN ; for each species...
      tmp2=STRCT_Redefine_Tag(tmp1[i].subst[j], Tag_Name="rres", Tag_Def=ref_rres)
      new_subst=[new_subst,tmp2]
    ENDFOR
    
    tmp3=STRCT_Redefine_Tag(tmp1[i], Tag_Name="subst", Tag_Def=new_subst)
    
    dp_chrom_empty_rres = [dp_chrom_empty_rres, tmp3]
  ENDFOR

  RETURN, dp_chrom_empty_rres

END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_strct2current_chrom, old_dp_chrom, vcheck, current_version
  
  n_exp = N_ELEMENTS(old_dp_chrom)
  
  FOR exp_nbr=0, n_exp-1 DO BEGIN ; loop through list (experiments)
    ; move dataset out of list
    tmp_chrom = old_dp_chrom[exp_nbr]
    ; generate an empty reference structure
    dp_chrom_empty_rres = dp_gen_empty_rres(tmp_chrom, vcheck, current_version)
    ; assign the old dataset to the new
    STRUCT_ASSIGN, tmp_chrom, dp_chrom_empty_rres
    ; rename...
    new_dp_chrom = TEMPORARY(dp_chrom_empty_rres)
    new_dp_chrom.iaudp_vers = MAKE_ARRAY(N_ELEMENTS(new_dp_chrom.iaudp_vers), VALUE=current_version, /FLOAT)
    ; recreate list
    IF exp_nbr EQ 0 THEN exp_list=LIST(new_dp_chrom) ELSE exp_list.add, new_dp_chrom
  ENDFOR

  RETURN, exp_list

END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_gen_empty_expcfg, old_dp_expcfg

  n_chroms = N_ELEMENTS(old_dp_expcfg.expinfo)
  
  ref_expinfo = REPLICATE(create_ref_expinfo(), n_chroms)
  ref_its = create_ref_ITS()
  ref_sequence = create_ref_sequence(n_chroms)
  ref_calmrs = create_ref_calmrs()
  ref_tgtmrs = create_ref_tgtmrs()
  ref_instrmp = create_ref_instrprc()
  ref_treatcfg = create_ref_treatcfg()
  ref_cocorr = create_ref_cocorr()

  empty_dp_expcfg = { expinfo:    ref_expinfo, $
                      setup:      ref_its, $
                      sequence:   ref_sequence, $
                      cal_mrs:    ref_calmrs, $
                      tgt_mrs:    ref_tgtmrs, $
                      instr_prc:  ref_instrmp, $
                      treatcfg:   ref_treatcfg, $
                      carryover:  ref_cocorr }

  RETURN, empty_dp_expcfg

END
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_strct2current_expcfg, old_dp_expcfg
  
  n_exp = N_ELEMENTS(old_dp_expcfg)
  
  FOR exp_nbr=0, n_exp-1 DO BEGIN ; loop through list (experiments)
    ; move dataset out of list
    tmp_expcfg = old_dp_expcfg[exp_nbr]
    ; generate an empty reference structure
    empty_dp_expcfg = dp_gen_empty_expcfg(tmp_expcfg)
    ; assign the old dataset to the new
    STRUCT_ASSIGN, tmp_expcfg, empty_dp_expcfg, /NOZERO
    ; rename...
    new_dp_expinfo = TEMPORARY(empty_dp_expcfg)
    ; recreate list
    IF exp_nbr EQ 0 THEN expinfo_list=LIST(new_dp_expinfo) ELSE expinfo_list.add, new_dp_expinfo
  ENDFOR
  
  RETURN, expinfo_list

END
;------------------------------------------------------------------------------------------------------------------------