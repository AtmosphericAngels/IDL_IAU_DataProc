;+
; PRO: dp_call_expinfo
;
; AUTHOR: T. Schuck, Nov 2015
; REVISED: F. Obersteiner; Sep. 2016, June 2017
;
; PURPOSE:  common frame for reading experiment info files
;
; INFO: * import functions -> def_offset *
; - specifies default header size excluding column header. individual import routines can contain a tag that specifies a
;   different header size. therefore it is possible to extend or reduce header information in the future and ensure some
;   downward compatibility.
;
; INFO: * import functions -> search_tags *
; - principal note: IDL structure 'ref_expinfo' defines, what can be extracted from the expinfo file; therefore the
;   content of the expinfo file is determined by the IDL structure. Changing the IDL structure will result in downward
;   compatibility issues.
; - possible variables to be extracted from expinfo file:
;     opr:      not essential
;     com:      not essential
;     rv_vol:   not essential
;     fname:    essential
;     nr:       not essential
;     s_name:   essential
;     s_date:   not essential
;     s_lat:    not essential
;     s_lon:    not essential
;     s_alt:    not essential
;     s_id:     essential
;     s_vol:    not essential, given e.g. as dp or mfc
;     rv_ps:    essential if no mfc else not essential
;     rv_pe:    essential if no mfc else not essential
;     rv_dp:    not essential, calculated as pe-ps
;     mfc_flow: not essential
;     mfc_vol:  essential if no ps and pe
;     Ts_cldhd: not essential
;     Te_cldhd: not essential
;     ts:       not essential
;     te:       not essential
;     dt:       not essential
;     use:      not essential
;
;  -> default search tags are as follows.
;  ***
;   search_tags_def=['opr','com','rv_vol','fname','nr','s_name','s_date','s_lat','s_lon','s_alt','s_id','s_vol','rv_ps', $
;                    'rv_pe','rv_dp','mfc_flow','mfc_vol','Ts_cldhd','Te_cldhd','ts','te','dt','use']
;   ess_tags_ix=[0,0,0,1,0,1,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0] / w_ess=[3, 5, 10, 12, 13, 16]
;  ***
;  -> modifiy these to suite specific expinfo-file.
;  -> make sure that all essential tags are available in expinfo-file
;  -> do not change order of def search tags
;  -> do not remove def search tags if they are not actually specified in expinfo-file, just leave them default
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_call_expinfo, FNAME=fname, OVERWRITE=overwrite, VERBOSE=verbose

  dp_refr_status, MESSAGE='loading exp-info...'
  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  IF NOT KEYWORD_SET(overwrite) THEN overwrite=0
  IF verbose THEN print, 'beginning to load experiment-info(s)...'

  COMMON DP_DATA

  IF SIZE(dp_chrom, /TYPE) NE 11 THEN BEGIN
    msg=DIALOG_MESSAGE('Please load Experiment(s) first..', /ERROR)
    dp_refr_status, /CLEAR
    RETURN
  ENDIF

  filter=['*.csv', '*.txt', '*.chrom']
  IF NOT KEYWORD_SET(fname) THEN $
    fname=DIALOG_PICKFILE(/MULTIPLE_FILES, PATH=path_wd, TITLE='Please select ExpInfo-file(s).', FILTER=filter, /FIX_FILTER)
  IF fname[0] EQ '' THEN BEGIN
    dp_refr_status, /CLEAR
    RETURN
  ENDIF

  n_files=N_ELEMENTS(fname)
  IF N_ELEMENTS(dp_chrom) NE n_files THEN BEGIN
    msg=DIALOG_MESSAGE('Please select the correct number of ExpInfo-files.', /ERROR)
    dp_refr_status, /CLEAR
    RETURN
  ENDIF

; *********************** INSTRUMENT-SPECIFIC CONFIG *********************************************************************

  FOR n=0, n_files-1 DO BEGIN ; begin loop over all selected expinfo-files

    CASE 1 OF
      (instr EQ 'Lab_BenchTOF' OR instr EQ 'Lab_QP/SFMS'): $
        BEGIN
          def_offset=2 ; old header size up to 2016-11; use header_size tag in header for newer files
          sep=';'
          name='ChromInfo'
          search_tags=['opr','Comment','rv_vol','Fname','Run_No','Sample_Name','s_date','PosLat','PosLon','PosAlt', $
                       'Sample_ID','s_vol','Vol0','Vol1','rv_dp','MFC.flow.set','MFC.cts','Ts_cldhd','Te_cldhd', $
                       'enrich_start','enrich_stop','dt','use']
          import = dp_read_expinfofile(fname[n], n, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                                       SEARCH_TAGS=search_tags, NL=nl, INSTRUMENT=instr)
        END

      (instr EQ 'FASTOF'): $
        BEGIN
          def_offset=5
          sep=STRING(9b)
          name=''
          search_tags=['Operator','com','rv_vol','fname','Run_No','Sample_Name','s_date','s_lat','s_lon','s_alt',$
                       'Sample_ID','s_vol','p0[hPa]', 'p1[hPa]','dp[hPa]','F.Set[mL]','Counter[mL]','TCH_p0', $
                       'TCH_p1','t_p0','t_p1','dt','use']
          import = dp_read_expinfofile(fname[n], n, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                                       SEARCH_TAGS=search_tags, NL=nl, INSTRUMENT=instr)
        END

      (instr EQ 'GhOST_MS' OR instr EQ 'GhOST_ECD'): $
        BEGIN
          def_offset=0
          sep=STRING(9b)
          name='Chrom'
          search_tags=['opr','com','rv_vol','fname','nr','s_name','s_date','s_lat','s_lon','s_alt','s_id','s_vol','p0', $
                       'p1','dp','mfc_flow','mfc_vol','Ts_cldhd','Te_cldhd','ts','te','dt','use']
          import = dp_read_expinfofile(fname[n], n, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                                       SEARCH_TAGS=search_tags, NL=nl, INSTRUMENT=instr)
        END

      (instr EQ 'AED'): $
        BEGIN
          def_offset=9
          sep=';'
          name='expinfo'
          search_tags=['opr','Comment','rv_vol','Fname','Run_No','Sample_Name','s_date','PosLat','PosLon','PosAlt','Sample_ID','s_vol','rv_ps', $
                       'rv_pe','rv_dp','mfc_flow','SVol.ml','Ts_cldhd','Te_cldhd','ts','te','dt','Use']
          import = dp_read_expinfofile(fname[n], n, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                                       SEARCH_TAGS=search_tags, NL=nl, INSTRUMENT=instr)
        END

      (instr EQ 'GHGGC_ECD/FID'): $
        BEGIN
          import = !NULL
          def_offset=7
          sep=';'
          name='expinfo'
          search_tags=['OPERATOR','SAMPLER','FLIGHT','FNAME','NR','S_NAME','CHEMSTATION_START','LAT_dgr_mean',$
                       'LON_dgr_mean','ALT_pstatic_hPa','S_ID','s_vol','P0','P1','dp','mfc_flow','mfc_vol','Ts_cldhd',$
                       'Te_cldhd','t0_collection','t1_collection','dt','USE']
          import = dp_read_expinfofile(fname[n], n, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                                       SEARCH_TAGS=search_tags, NL=nl, INSTRUMENT=instr)
        END

      ELSE: import = !NULL

    ENDCASE


    IF import EQ !NULL THEN BEGIN
      dp_refr_status, MESSAGE='Error: exp.info import failed.'
      RETURN
    ENDIF

    ; +++ remove some elements from the filename string to simplify comparison...
    remove_strings=['QP_','TOF_', '_mc_recal', '_recal']

    fnames_chrom = FILE_BASENAME(((dp_chrom[n]).fname))
    fnames_expin = FILE_BASENAME(import.fname)

    FOR i=0, N_ELEMENTS(remove_strings)-1 DO BEGIN
      fnames_chrom = strreplace_iter(fnames_chrom, remove_strings[i], '')
      fnames_expin = strreplace_iter(fnames_expin, remove_strings[i], '')
    ENDFOR

    ; +++ get the file numbers specifid in the filename...
    fnumbers_chrom = find_fnumber(fnames_chrom, SEP_EXT=sep_ext, SEP_NBR=sep_nbr, REST_STR=fnames_chrom_rest)
    fnumbers_expin = find_fnumber(fnames_expin, SEP_EXT=sep_ext, SEP_NBR=sep_nbr, REST_STR=fnames_expin_rest)

    w_num_match=WHERE(fnumbers_chrom EQ fnumbers_expin, n_num_match)
    w_str_match=WHERE(STRUPCASE(fnames_chrom_rest) EQ STRUPCASE(fnames_expin_rest), n_str_match)

    IF (n_num_match+n_str_match) NE 2*(nl-(def_offset+1)) THEN BEGIN
      msg=DIALOG_MESSAGE(FILE_BASENAME(fname[n])+': Filenames do not match. Please select correct ExpInfo-file.', /ERROR)
      dp_refr_status, /CLEAR
      RETURN
    ENDIF


    ref_sequence = create_ref_sequence(N_ELEMENTS(fnames_chrom))
    ref_its = create_ref_ITS()
    ref_calmrs = create_ref_calmrs()
    ref_tgtmrs = create_ref_tgtmrs()
    ref_instrmp = create_ref_instrprc()
    ref_treatcfg = create_ref_treatcfg()
    ref_cocorr = create_ref_cocorr()

    tmp_strct = { expinfo:    import, $
                  setup:      ref_its, $
                  sequence:   ref_sequence, $
                  cal_mrs:    ref_calmrs, $
                  tgt_mrs:    ref_tgtmrs, $
                  instr_prc:  ref_instrmp, $
                  treatcfg:   ref_treatcfg, $
                  carryover:  ref_cocorr }

    IF n EQ 0 THEN BEGIN
      dp_expcfg = !NULL ; import ok so far, overwrite previous version of expcfg
      dp_expcfg = LIST(tmp_strct) ; create the dp_expcfg list...
    ENDIF ELSE dp_expcfg.add, tmp_strct ; add elements to the list

    use_def = import.use
    tmp_chrom=(dp_chrom[n])
    n_subst=N_ELEMENTS(tmp_chrom[0].subst.name)
    FOR k=0, n_subst-1 DO tmp_chrom.subst[k].rres.use_flag = use_def  ; overwrite use_flag in rres strct
    (dp_chrom[n])=tmp_chrom

  ENDFOR ; loop over selected files


  IF overwrite THEN BEGIN ; delete results if previous expinfo is overwritten by the current operation
    ref_rres=create_ref_rres()
    FOR i=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN ; overwrite eval_flag
      tmp_chrom=(dp_chrom[i])
      n_chrom=N_ELEMENTS(dp_chrom[i])
      n_subst=N_ELEMENTS(tmp_chrom[i].subst.name)
      FOR j=0, n_chrom-1 DO BEGIN
        FOR k=0, n_subst-1 DO tmp_chrom[j].subst[k].rres=ref_rres
      ENDFOR
      (dp_chrom[i])=tmp_chrom
    ENDFOR
  ENDIF

  IF verbose THEN print, 'expinfo file(s) loaded.'

  dp_refr_status, MESSAGE='Expinfo file(s) loaded.'

END