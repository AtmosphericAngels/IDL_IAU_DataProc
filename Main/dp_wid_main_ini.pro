;+
; PRO: dp_wid_main_handle/dp_wid_main_ini
;
; AUTHOR: F. Obersteiner, Sep-2016 (reboot version), S.Sala (original version), modified by T.Schuck, Nov-2015.
;
; PURPOSE: main window, to load data and select a basic experiment configuration
;-
;------------------------------------------------------------------------------------------------------------------------
@dp_wid_main_tools
@dp_strcts2current_version
@dp_manage_calmrs
@dp_manage_tgtmrs
@dp_manage_instrprc
@dp_manage_treatcfg
@dp_manage_carryover
@dp_dbscript_call
;------------------------------------------------------------------------------------------------------------------------
PRO dp_wid_main_handle, event

  COMMON DP_DATA
  COMMON DP_WIDID
  
  IF error_handler_IO EQ 1 THEN BEGIN
    CATCH, Error_status
    IF Error_status NE 0 THEN BEGIN
      e_ix = '(!) Error '+STRING(Error_status)
      e_msg = ', '+STRING(!ERROR_STATE.MSG)
      msg = STRCOMPRESS(STRTRIM(e_ix+e_msg+' Returning.'))
      dlg = DIALOG_MESSAGE(msg, /ERROR)
      CATCH, /CANCEL
      dp_refr_status, MESSAGE='Returned from error.'
      RETURN
    ENDIF
  END
  
  ID_instr=WIDGET_INFO(event.top, find_by_uname='instr_dl')
  WIDGET_CONTROL, ID_instr, GET_VALUE=inst_val
  instr=inst_val[WIDGET_INFO(ID_instr, /DROPLIST_SELECT)]

  uname = WIDGET_INFO(event.id, /uname)
 
  verbose = 0

  CASE uname OF 
    'restore_chrom' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN ; dp_chrom is already a LIST, i.e. data was loaded before
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, $
                               DIALOG_PARENT=dp_widid.dp_mainwid)
          IF quest EQ 'No' THEN RETURN
        ENDIF        

        dp_chrom=dp_restore_chrom(dp_chrom, dp_vers, PATH=path_wd, VERBOSE=verbose)                
        dp_chrom=dp_correct_time(dp_chrom, VERBOSE=verbose) ; correct for "jumps" in Chemstation cdf timestamps        
        dp_expcfg = !NULL
        
        IF dp_widid.dp_dataproc NE -1 THEN BEGIN
          dp_destroy_wids, ID=dp_widid.dp_dataproc
          dp_widid.dp_dataproc = -1
        ENDIF
          
        config_dp_mainwid, event               
      END
    ;******************************************************************************************
    'load_expinfo' : $
      BEGIN
        del_results=0
        IF SIZE(dp_expcfg, /TYPE) EQ 11 THEN BEGIN ; dp_expcfg is already a LIST, i.e. data was loaded before
          quest=DIALOG_MESSAGE('Loaded ExpInfo found. Replace? Previous Results will be deleted...', $
                                /QUESTION, /DEFAULT_NO, DIALOG_PARENT=dp_widid.dp_mainwid)
          IF quest EQ 'Yes' THEN BEGIN
            del_results=1
            dp_destroy_wids, ID=dp_widid.dp_dataproc
          ENDIF ELSE RETURN
        ENDIF         
        dp_call_expinfo, OVERWRITE=del_results, VERBOSE=verbose    
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'restore_dp' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN ; dp_chrom is already a LIST, i.e. data was loaded before
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO, $
                               DIALOG_PARENT=dp_widid.dp_mainwid)
          IF quest EQ 'No' THEN RETURN
        ENDIF
          
        dp_file = DIALOG_PICKFILE(PATH=path_wd, TITLE='Please select a *_dp_data.dat file to restore.', $
                                  FILTER='*_dp_data.dat', /FIX_FILTER)
        
        IF STRLEN(dp_file)EQ 0 THEN RETURN ; abort if no file selected 
        config_file = dp_file.replace('_dp_data.dat', '_dp_expcfg.dat')
        IF FILE_TEST(config_file) EQ 0 THEN BEGIN
          msg=DIALOG_MESSAGE('No config file found. Aborted.', /ERROR)
          RETURN
        ENDIF
        
        dp_refr_status, MESSAGE='Restoring IAU_Dataproc data...'
        IF dp_chrom NE !NULL THEN HEAP_FREE, dp_chrom
        IF dp_expcfg NE !NULL THEN HEAP_FREE, dp_expcfg
        IF chromlist NE !NULL THEN HEAP_FREE, chromlist
        IF expinflist NE !NULL THEN HEAP_FREE, expinflist
        IF substlist NE !NULL THEN HEAP_FREE, substlist
        HEAP_GC
        
        RESTORE, dp_file
        RESTORE, config_file
        
        vcheck = dp_version_check(dp_chrom, VCHECK_VERSION=1.26, VERS_TAG='IAUDP_VERS')                                      
        IF vcheck LE 0 THEN BEGIN ; version of restored file = old? -> redefine structures.
          dp_refr_status, MESSAGE='Old version detected...'
          dp_chrom = dp_strct2current_chrom(dp_chrom, vcheck, dp_vers)
          dp_expcfg = dp_strct2current_expcfg(dp_expcfg)
        ENDIF
        
        IF dp_widid.dp_dataproc NE -1 THEN BEGIN
          dp_destroy_wids, ID=dp_widid.dp_dataproc
          dp_widid.dp_dataproc = -1
        ENDIF          
        config_dp_mainwid, event
        dp_refr_status, MESSAGE='Data restored.'
      END
    ;******************************************************************************************
    'save_dp' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) NE 11 OR SIZE(dp_expcfg, /TYPE) NE 11 THEN BEGIN
          msg=DIALOG_MESSAGE('Please load data first.', /ERROR)
          RETURN
        ENDIF ELSE BEGIN
          dp_refr_status, MESSAGE='Saving IAU_Dataproc data...'
          caldat, SYSTIME(/JULIAN), mm,dd,yy,hh,mn,ss
          datestr = STRING(yy,format='(I4)')+STRING(mm,format='(I02)')+STRING(dd,format='(I02)') $
                    +STRING(hh,format='(I02)')+STRING(mn,format='(I02)')
          fname=DIALOG_PICKFILE(TITLE='Please chose directory and filename...', $
                                PATH=path_wd, DEFAULT_EXTENSION='.dat', /OVERWRITE_PROMPT, $
                                FILE=datestr)
          fname=strreplace_iter(fname, '..dat', '')
          save, dp_chrom, FILENAME=fname+'_dp_data.dat'
          save, dp_expcfg, FILENAME=fname+'_dp_expcfg.dat'
          dp_refr_status, MESSAGE='Data saved.'                                              
        ENDELSE
      END
    ;******************************************************************************************
    'chroms_dl' : $
      BEGIN
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'expinfo_dl' : $
      BEGIN
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'instr_dl' : $
      BEGIN
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'exptype_dl' : $
      BEGIN
        ID_etype=WIDGET_INFO(event.top, find_by_uname='exptype_dl')
        exptype_ix=WIDGET_INFO(ID_etype, /DROPLIST_SELECT)        
        CASE exptype_ix OF
          0: BEGIN
               spec_val=['Samples'];, 'Intercalibration', 'Non-Linearity Exp.', 'Precision Exp.']
             END
          1: BEGIN
               spec_val='Samples'
             END
          ELSE:      
        ENDCASE       
        ID_espec=WIDGET_INFO(event.top, find_by_uname='expspec_dl')
        WIDGET_CONTROL, ID_espec, SET_VALUE=spec_val
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'expspec_dl' : $
      BEGIN
        IF N_ELEMENTS(dp_chrom) GT 1 THEN BEGIN
          msg=DIALOG_MESSAGE('Selection prohibited; please load only one experiment.')
          ID_espec=WIDGET_INFO(event.top, find_by_uname='expspec_dl')
          WIDGET_CONTROL, ID_espec, SET_DROPLIST_SELECT=0
        ENDIF
        config_dp_mainwid, event
      END
    ;******************************************************************************************
    'run_dp' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) NE 11 OR SIZE(dp_expcfg, /TYPE) NE 11 THEN BEGIN ; check if everything is loaded...
          msg=DIALOG_MESSAGE('Please load data/config first.', /ERROR)
          RETURN
        ENDIF
        dp_refr_status, MESSAGE='Called processing dialog.'
        w=[] ; check if data was analysed before; call sequence analyser if not
        FOR i=0, N_ELEMENTS(dp_expcfg)-1 DO w=[w, WHERE((dp_expcfg[i]).sequence.id NE -1)] ; id is -1 before sequence analysis  
        IF (WHERE(w EQ -1))[0] NE -1 THEN dp_analyse_seq, VERBOSE=verbose     
        dp_wid_dataproc_ini
      END
    ;******************************************************************************************
    'db_script_run' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO)
          IF quest EQ 'No' THEN RETURN $
            ELSE BEGIN
              dp_chrom    = !NULL
              dp_expcfg   = !NULL
              chromlist   = !NULL
              expinflist  = !NULL
              substlist   = !NULL
              instr       = ''
            ENDELSE
        ENDIF
        dp_dbscript_call, event
      END      
    ;******************************************************************************************
    'db_script_load1st' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN
          quest=DIALOG_MESSAGE('Loaded data found. Replace?', /QUESTION, /DEFAULT_NO)
          IF quest EQ 'No' THEN RETURN $
            ELSE BEGIN
              dp_chrom    = !NULL
              dp_expcfg   = !NULL
              chromlist   = !NULL
              expinflist  = !NULL
              substlist   = !NULL
              instr       = ''
            ENDELSE
        ENDIF
        dp_dbscript_call, event, /load_1st_only
      END
    ;******************************************************************************************  
    'set_path': $
      BEGIN
       path_new=DIALOG_PICKFILE(PATH=path_wd,TITLE='Please select a file path...', /DIRECTORY)
       IF path_new NE '' THEN path_wd = path_new
       dp_refr_status, MESSAGE='Path set.'
      END
    ;******************************************************************************************
    'exit': $
      BEGIN
        dp_destroy_wids, /ALL
        w = GETWINDOWS()
        IF N_ELEMENTS(w) GT 0 THEN FOREACH i, w DO i.close
      END
    ;******************************************************************************************
    ELSE:
  ENDCASE

END 

;#####################################################################################################################

PRO dp_wid_main_ini

COMMON DP_DATA
COMMON DP_WIDID

;++++++++++++ WIDGET BASE
  mainbase=WIDGET_BASE(title='IAU_DP_v'+dp_vers, mbar=dp_mainmen, column=1,$
                       XOFFSET= + 5, YOFFSET= + 5) ;rects[0, sel_mon] rects[1, sel_mon]                 

;++++++++++++ MENUS                       
  fileID=WIDGET_BUTTON(dp_mainmen, VALUE='File', /MENU)
    load_chrom_ID=WIDGET_BUTTON(fileID, VALUE='Load Experiment(s)' , uname='restore_chrom')
    load_einfo_ID=WIDGET_BUTTON(fileID, VALUE='Load Experiment-Info(s)', uname='load_expinfo')
    load_dp_ID=WIDGET_BUTTON(fileID, VALUE='Restore DP File', uname='restore_dp', /SEPARATOR)
    load_dp_ID=WIDGET_BUTTON(fileID, VALUE='Save DP File', uname='save_dp')
    ID=WIDGET_BUTTON(fileID, VALUE='Set Filepath...', UNAME='set_path', /SEPARATOR)
    ID=WIDGET_BUTTON(fileID, VALUE='Exit', UNAME='exit', /SEPARATOR)
    
  advID=WIDGET_BUTTON(dp_mainmen, VALUE='Advanced', /MENU)
    run=WIDGET_BUTTON(advID, VALUE='Run Database Script', UNAME='db_script_run')
    load=WIDGET_BUTTON(advID, VALUE='Script: Load 1st active', UNAME='db_script_load1st', /SEPARATOR)
     
;++++++++++++ DROPLISTS     
  dp_subbase1 = WIDGET_BASE(mainbase, column=1)

    dp_subbase3 = WIDGET_BASE(dp_subbase1, uname='dp_subbase3', column=3)
    
      TXT = WIDGET_LABEL(dp_subbase3, VALUE='Instrument:', /ALIGN_LEFT)
        inst_val=['Lab_QP/SFMS', 'GhOST_MS', 'Lab_BenchTOF', 'FASTOF', 'GhOST_ECD', 'AED', 'GHGGC_ECD/FID']
      CFGINST = WIDGET_DROPLIST(dp_subbase3, VALUE=inst_val, uname='instr_dl',/ALIGN_LEFT)   

      TXT = WIDGET_LABEL(dp_subbase3, VALUE='Experiment Type:', /ALIGN_LEFT)
        type_val=['Canister/Flask Series', 'In-Situ/Continuous', '(undefined)']
      CFGTYPE = WIDGET_DROPLIST(dp_subbase3, VALUE=type_val, uname='exptype_dl',/ALIGN_LEFT)
      
      TXT = WIDGET_LABEL(dp_subbase3, VALUE='Experiment Specification:', /ALIGN_LEFT)
        spec_val=['Samples', 'Precision Exp.', 'Intercalibration', 'Non-Linearity Exp.', '(undefined)']
      CFGSPEC = WIDGET_DROPLIST(dp_subbase3, VALUE=spec_val, uname='expspec_dl', /ALIGN_LEFT)

      dp_subbase2 = WIDGET_BASE(dp_subbase1, uname='dp_subbase2', column=1)
      SEP = WIDGET_LABEL(dp_subbase2, VALUE='')
      TXT = WIDGET_LABEL(dp_subbase2, VALUE='Loaded Experiment(s):', /ALIGN_LEFT)
      CDL = WIDGET_DROPLIST(dp_subbase2, VALUE='', uname='chroms_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)
      SEP = WIDGET_LABEL(dp_subbase2, VALUE='')
      TXT = WIDGET_LABEL(dp_subbase2, VALUE='Loaded Experiment Info File(s):', /ALIGN_LEFT)
      EDL = WIDGET_DROPLIST(dp_subbase2, VALUE='', uname='expinfo_dl', /DYNAMIC_RESIZE, /ALIGN_LEFT)

;++++++++++++ STATUS
    dp_proc=WIDGET_BASE(dp_subbase1, uname='dp_proc_base', COLUMN=1)
      TXT=WIDGET_LABEL(dp_proc, VALUE='   ', /ALIGN_LEFT)
      RUN=WIDGET_BUTTON(dp_proc, VALUE='Process Data!', uname='run_dp')  
        
;++++++++++++ STATUS
    dp_status=WIDGET_BASE(dp_subbase1, uname='dp_stat_base', column=1)
      TXT=WIDGET_LABEL(dp_status, VALUE='   ', /ALIGN_LEFT)
      TXT=WIDGET_LABEL(dp_status, VALUE='Status', /ALIGN_LEFT)
      TXT=WIDGET_TEXT(dp_status, VALUE='idle', XSIZE=55, /NO_NEWLINE, UNAME='status')     
      
;++++++++++++ (c)
    dp_credits = WIDGET_BASE(dp_subbase1, uname='dp_credits', column=1)
      SEP = WIDGET_LABEL(dp_credits, VALUE='--', /ALIGN_LEFT)
      TXT = WIDGET_LABEL(dp_credits, VALUE='(c) 2017 Univ. Frankfurt / IAU / Group A. Engel', /ALIGN_LEFT)

;++++++++++++ REALIZE!
  dp_widid.dp_mainwid = mainbase
  WIDGET_CONTROL, mainbase, /REALIZE 
  XMANAGER, 'dp_wid_main_handle', mainbase, /NO_BLOCK, event_handler='dp_wid_main_handle'
    
END