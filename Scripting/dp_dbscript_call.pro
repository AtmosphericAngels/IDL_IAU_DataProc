;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-08, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; run a database script to process multiple experiments automatically.
;
; INFO
; can only process one experiment at a time, i.e. each line in the script is one experiment
; with one chrom-file etc.
;
;-
;------------------------------------------------------------------------------------------------------------------------
@dp_plot_tools
@dp_dbscript_rw
@dp_plot_rres_diag
;------------------------------------------------------------------------------------------------------------------------
PRO dp_dbscript_call, event, LOAD_1ST_ONLY=load_1st_only, VERBOSE=verbose

  ; destroy widgets if opened
  dp_destroy_wids, ID=dp_dataproc
  dp_destroy_wids, ID=dp_restablewid
  dp_destroy_wids, ID=dp_selmeas
  dp_destroy_wids, ID=dp_prcexp
  dp_destroy_wids, ID=dp_nlexp

  COMMON dp_data

  IF NOT KEYWORD_SET(load_1st_only) THEN load_1st_only=0

  ; load database table
  db_info = dp_dbscript_read(FILE=db_file, PATH=path_wd)
  IF db_info EQ !NULL THEN RETURN

  ; run db checker
  dp_refr_status, message='db script: checking...'
  db_check = dp_dbscript_chk(db_info, N_TESTED=n_tested, /LOUD)

  IF n_tested EQ db_check THEN BEGIN
    ; get number of experiments + active experiments from info structure
    vd_exp = WHERE(db_info.data.active EQ 1, n_exp)
    IF n_exp EQ 0 THEN RETURN

    IF load_1st_only THEN BEGIN
      n_exp=1
      vd_exp=vd_exp[0]
    ENDIF

    FOR n=0, n_exp-1 DO BEGIN ; begin loop: N EXPERIMENTS
      ; clear references to avoid heap memory overload
      IF dp_chrom NE !NULL THEN HEAP_FREE, dp_chrom
      IF dp_expcfg NE !NULL THEN HEAP_FREE, dp_expcfg
      IF chromlist NE !NULL THEN HEAP_FREE, chromlist
      IF expinflist NE !NULL THEN HEAP_FREE, expinflist
      IF substlist NE !NULL THEN HEAP_FREE, substlist
      HEAP_GC

      current_exp = ' ('+STRCOMPRESS(STRING(n+1), /REMOVE_ALL)+' of '+STRCOMPRESS(STRING(n_exp), /REMOVE_ALL)+')'
      dp_refr_status, message='db script: loading...'+current_exp

      ; restore the chrom file
      dp_chrom=dp_restore_chrom(dp_chrom, dp_vers, FNAME=db_info.data[vd_exp[n]].chromdata_path, VERBOSE=verbose)
      dp_chrom=dp_correct_time(dp_chrom, VERBOSE=verbose)
      chromlist=((dp_chrom[0]).exp_fname)
      substlist=LIST(((dp_chrom[0]).subst.name)[*,0])

      ; load expinfo
      instr=db_info.data[vd_exp[n]].EXPINFO_IMPORT_FCT
      dp_call_expinfo, FNAME=db_info.data[vd_exp[n]].expinfo_path, /OVERWRITE, VERBOSE=verbose

      ; update setup info structure
      ITS = {instrument: instr, type: 'undef', spec: 'undef'}
      tmp_strct=(dp_expcfg)[0]  ; move structure out of list
      tmp_strct.setup=ITS
      (dp_expcfg)[0]=TEMPORARY(tmp_strct)  ; put strct with loaded values back into list

      ; analyse the sequence
      dp_analyse_seq, /QUIET, VERBOSE=verbose
      samtreats=((dp_expcfg)[0]).sequence.sam_treat
      samtreats=samtreats[WHERE(STRLEN(samtreats) NE 0)]
      caltreats=((dp_expcfg)[0]).sequence.cal_treat
      caltreats=caltreats[WHERE(STRLEN(caltreats) NE 0)]

      ; apply default substance names if specified
      IF STRLEN(db_info.data[vd_exp[n]].subst_namedef_path) NE 0 THEN $
        dp_replace_substnames, DEF_FILE=db_info.data[vd_exp[n]].subst_namedef_path

      ; load dp_treatcfg if specified
      IF STRLEN(db_info.data[vd_exp[n]].dp_treatcfg_path) NE 0 THEN $
        dp_read_treatcfg, 0, /SEL_ONLY, FILE=db_info.data[vd_exp[n]].dp_treatcfg_path, VERBOSE=verbose

      ; load dp_calmrs if specified
      IF STRLEN(db_info.data[vd_exp[n]].dp_calmrs_path) NE 0 THEN BEGIN
        mrs_strct=dp_read_calmrs(DEF_FILE=db_info.data[vd_exp[n]].dp_calmrs_path, VERBOSE=verbose)
        dp_apply_calmrs, 0, mrs_strct, PATH=path_wd, VERBOSE=verbose
      ENDIF

      ; load dp_tgtmrs if specified
      IF STRLEN(db_info.data[vd_exp[n]].dp_tgtmrs_path) NE 0 THEN BEGIN
        tgt_strct=dp_read_tgtmrs(DEF_FILE=db_info.data[vd_exp[n]].dp_tgtmrs_path, VERBOSE=verbose)
        dp_apply_tgtmrs, 0, tgt_strct, PATH=path_wd, VERBOSE=verbose
      ENDIF

      ; load dp_instr_prc if specified
      IF STRLEN(db_info.data[vd_exp[n]].dp_instr_prc_path) NE 0 THEN BEGIN
        prc_strct=dp_read_instrprc(0, chromlist[0], substlist, $
                                   DEF_FILE=db_info.data[vd_exp[n]].dp_instr_prc_path, VERBOSE=verbose)
        dp_apply_instrprc, 0, PRC_STRCT=prc_strct, /QUIET, VERBOSE=verbose
      ENDIF

      ; load carry-over correction file
      IF STRLEN(db_info.data[vd_exp[n]].corr_carryover) NE 0 THEN BEGIN
        dp_read_cocorrparms, 0, /SEL_ONLY, DEF_FILE=db_info.data[vd_exp[n]].corr_carryover, VERBOSE=verbose
      ENDIF
      
      ; calculate relative responses
      dp_call_relresp_calc, 0, 0, 0, /OVERWRITE, /AUTO_SAMTREAT, VERBOSE=verbose, $
                            CALTREATS=caltreats, SAMTREATS=samtreats, CALIPMTHDS=cal_ip_mthd

      IF NOT load_1st_only THEN BEGIN
        ; save the dp file
        dp_refr_status, message='db script: save results...'+current_exp

        caldat, ((dp_chrom)[0].jdate)[0], mm,dd,yy
        cdate1 = STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)')
        caldat, SYSTIME(/JULIAN), mm,dd,yy,hh,mn,ss
        cdate2 = STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)') $
              +STRING(hh,format='(I02)')+STRING(mn,format='(I02)')
        cdate = cdate1+'_'+cdate2

        spec_filename=0
        folder=FILE_DIRNAME(FILE_DIRNAME(db_info.data[vd_exp[n]].chromdata_path))
        IF KEYWORD_SET(db_info.data[vd_exp[n]].dp_savefile_path) THEN BEGIN
          savepath = db_info.data[vd_exp[n]].dp_savefile_path
          IF STRMATCH(savepath, '*dp_data.dat') EQ 1 THEN BEGIN
            fname_dp_chrom=savepath
            spec_filename=1
          ENDIF ELSE BEGIN
            fname_dp_chrom = savepath+'\'+cdate+'_dp_data.dat'
          ENDELSE
        ENDIF ELSE BEGIN
          savepath = folder+'\__IAU_DATAPROC_save'
          db_info.data[vd_exp[n]].dp_savefile_path = savepath
          fname_dp_chrom = savepath+'\'+cdate+'_dp_data.dat'
        ENDELSE

        FILE_MKDIR, FILE_DIRNAME(fname_dp_chrom)
        save, dp_chrom, FILENAME=fname_dp_chrom
        fname_dp_expcfg = fname_dp_chrom.replace('_dp_data.dat', '_dp_expcfg.dat')
        save, dp_expcfg, FILENAME=fname_dp_expcfg

        ; save txt reports if desired
        IF spec_filename THEN savepath=FILE_DIRNAME(savepath)
        IF KEYWORD_SET(db_info.data[vd_exp[n]].save_txtreport) THEN BEGIN
          txt_path = savepath+'\dp_txt_report\'
          FILE_MKDIR, txt_path
          dp_res2txt, 0, 0, DEF_PATH=txt_path, /ALL, /BRIEF, VERBOSE=verbose
        ENDIF

        ; set current experiment inactive = processed
        db_info.data[vd_exp[n]].active = 0
        db_info.data[vd_exp[n]].proc_timestamp = jultime2timestring(SYSTIME(/JULIAN))

; +++ MOD
;         dump plots
;        FOR subst=0, N_ELEMENTS((dp_chrom[0].subst)[*,0])-1 DO $
;          dp_plot_rres_diag, 0, subst, /RRES, savepath='E:\temp'
; +++

        ; analyse NL?
        IF db_info.data[vd_exp[n]].analyse_NL GT 0 THEN BEGIN
          FOR subst=0, N_ELEMENTS((dp_chrom[0].subst)[*,0])-1 DO BEGIN
            fct_dgr = db_info.data[vd_exp[n]].analyse_NL
            saveplot = 'E:\temp\'+db_info.data[vd_exp[n]].exp_id
            saveplot = 0
            show_plots = 0
            nl_strct = dp_nlexp_analyse(fct_dgr, 0, subst, $
                                        SAVEPLOT=saveplot, SHOW_PLOTS=show_plots)
            IF nl_strct NE !NULL THEN dp_nlexp_res2txt, nl_strct, 0, subst, DIR=savepath+'\dp_nl_report\'
          ENDFOR
        ENDIF

      ENDIF ; end if: load first only

    ENDFOR ; end loop: N EXPERIMENTS

    ; save updated database reference table
    IF NOT load_1st_only THEN $
      write = dp_dbscript_write(db_info, PATH=FILE_DIRNAME(db_file))

    ; update main widget and open processing dialog
    config_dp_mainwid, event, DEF_UNAME='restore_dp'
    dp_wid_dataproc_ini
    dp_refr_status, message='db script: processed.'

  ENDIF ELSE BEGIN
    msg = DIALOG_MESSAGE('Script error: aborted.', /ERROR)
    dp_refr_status, /CLEAR
  ENDELSE

END
