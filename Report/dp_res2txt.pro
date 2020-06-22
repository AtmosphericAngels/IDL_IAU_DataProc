;+
; PRO: dp_res2txt
;
; AUTHOR: F. Obersteiner, Sep-2016
; MODIFIED: T.J. Schuck, Feb-2017, export filename
;
; PURPOSE: results export to txt file(s). includes calculation of
;          PRELIMINARY MRs (no carry-over correction or whatever...)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_res2txt, sel_exp, sel_subst, PATH=path, DEF_PATH=def_path, ALL=all, BRIEF=brief, VERBOSE=verbose

  IF NOT KEYWORD_SET(verbose) THEN verbose = 0
  IF NOT KEYWORD_SET(brief) THEN brief = 0 ; default: detailed report

  IF verbose THEN BEGIN
    print, '+++'
    print, 'beginning results export to txt'
  ENDIF

  COMMON DP_DATA

  fname=''
  fpath=''
  prefix='nd_'
  suffix='_iaudp'
  sep=STRING(9B)
  sel_eval_mode = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]

  block_mode = ''
;  block_mode = '_'+((dp_chrom[sel_exp]).subst[sel_subst].rres.sam_treat)[0]


  IF KEYWORD_SET(all) THEN $
    IF KEYWORD_SET(def_path) THEN fpath=def_path ELSE $
      fpath = DIALOG_PICKFILE(PATH=path, /WRITE, /OVERWRITE_PROMPT, /DIRECTORY) $
  ELSE $
    IF NOT brief THEN $
      fname = DIALOG_PICKFILE(PATH=path, /WRITE, $
                 file=STRTRIM(STRCOMPRESS(((dp_chrom[sel_exp]).subst[sel_subst].name)[0], /REMOVE_ALL))+ $
                      (['_a', '_h'])[sel_eval_mode]+block_mode+suffix+'_det'+'.txt') $
         ELSE fname = DIALOG_PICKFILE(PATH=path, /WRITE, $
                 file=STRTRIM(STRCOMPRESS(((dp_chrom[sel_exp]).subst[sel_subst].name)[0], /REMOVE_ALL))+ $
                      (['_a', '_h'])[sel_eval_mode]+block_mode+suffix+'.txt')


  IF STRLEN(fname) EQ 0 AND STRLEN(fpath) EQ 0 THEN RETURN


  n_chrom = N_ELEMENTS((dp_chrom[sel_exp]))
  IF n_chrom GE 6 THEN ix = n_chrom*(0.5) ELSE ix = 0


  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  cal=((dp_expcfg[sel_exp]).expinfo.s_name)[(WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_cal))[0]]

  IF ((dp_expcfg[sel_exp]).cal_mrs.canister) EQ '' THEN call_mr_calc=0 ELSE call_mr_calc=1

  nvd=1
  IF brief THEN sel_chroms = $
    WHERE(((dp_expcfg[sel_exp]).expinfo.s_id) EQ id_sam OR ((dp_expcfg[sel_exp]).expinfo.s_id) EQ id_tgt, nvd) $
      ELSE sel_chroms = LINDGEN(n_chrom)

  IF nvd LE 0 THEN RETURN

  colheader=['Chromatogram',sep,'Date',sep,'Time',sep,'Sample_Name',sep,'rR',sep, $
             'Block_rR',sep,'Block_RSD',sep,'Precision_Flag',sep,'Prelim_MR']


;--------------------------------------------------*** all substances to individual txt files
  IF KEYWORD_SET(all) THEN BEGIN

      FOR sel_name=0, N_ELEMENTS((dp_chrom[sel_exp])[0].subst.name)-1 DO BEGIN

        eval_mode = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_select)[0]
        mode_string = (['signal_area', 'signal_height'])[eval_mode]
        w_corr = WHERE(((dp_chrom[sel_exp])[ix].subst[sel_name].rres.active_corr[0])[0] EQ !TRUE)
        IF w_corr[0] NE -1 THEN correction = (['carry-over'])[w_corr] $
          ELSE correction = 'none'

        IF FINITE((dp_chrom[sel_exp])[ix].subst[sel_name].ires.rt) THEN $
          prefix=STRCOMPRESS(STRING((dp_chrom[sel_exp])[ix].subst[sel_name].ires.rt, FORMAT='(F12.2)'), /REMOVE_ALL)+'_'
        IF prefix EQ 'nd_' AND FINITE((dp_chrom[sel_exp])[ix].subst[sel_name].ires.rt) THEN $
          prefix=STRCOMPRESS(STRING((dp_chrom[sel_exp])[ix].subst[sel_name].rt, FORMAT='(F12.2)'), /REMOVE_ALL)+'_'

        name=STRCOMPRESS((dp_chrom[sel_exp])[ix].subst[sel_name].name, /REMOVE_ALL)

        IF verbose THEN print, 'exporting results for: ', name

        fname=fpath+STRTRIM(prefix+name+(['_a', '_h'])[eval_mode]+block_mode+suffix+'.txt')
        exp_ts=jultime2timestring(((dp_chrom[sel_exp]).subst[sel_name].rres.dp_timestamp)[0])

        IF instr NE 'GhOST_ECD' THEN $
          mass=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].mass[(dp_chrom[sel_exp]).subst[sel_name].quant])[0], $
                           FORMAT='(D25.3)'), /REMOVE_ALL) ELSE mass='NaN'

        IF *(dp_expcfg[sel_exp]).cal_mrs.mr_ppt NE !NULL THEN BEGIN
          cal_mr=!VALUES.D_NAN
          cal_scale= 'NA'
          w=WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).cal_mrs.substance) EQ STRUPCASE(name))
          IF w[0] NE -1 THEN BEGIN
            cal_mr = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
            IF (*((dp_expcfg[sel_exp]).cal_mrs.unit)) NE !NULL THEN $
              unit = (*(dp_expcfg[sel_exp]).cal_mrs.unit)[w[0]] ELSE unit = 'NA'
            cal_scale = (*(dp_expcfg[sel_exp]).cal_mrs.scale)[w[0]]
            IF STRCOMPRESS(STRING(cal_scale), /REMOVE_ALL) EQ '-1' THEN cal_scale= 'NA'
          ENDIF
        ENDIF ELSE BEGIN
          cal_mr = 'NaN'
          unit = 'NA'
          cal_scale = 'NA'
        ENDELSE
        
        cal_prc = 'NaN'
        IF *(dp_expcfg[sel_exp]).instr_prc.mp_rel NE !NULL THEN BEGIN
          w = WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).instr_prc.substance) EQ STRUPCASE(name))
          IF w[0] NE -1 THEN $
            cal_prc = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
        ENDIF


        CASE eval_mode OF
          0: $
            BEGIN
              cal_mintomax = STRCOMPRESS(STRING($
                                 ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_mintomax)[0], $
                                 FORMAT='(D25.6)'), /REMOVE_ALL)
              cal_devtofit = STRCOMPRESS(STRING($
                                 ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_devtofit)[0], $
                                 FORMAT='(D25.6)'), /REMOVE_ALL)
              cal_block_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_block_rsd)
              w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
              IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
                ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
                                     ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_block_rsd), $
                                     /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

              sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd)
              w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
              IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
                ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
                                     ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd), $
                                     /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
            END
          1: $
            BEGIN
              cal_mintomax = STRCOMPRESS(STRING($
                                 ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_mintomax)[0], $
                                 FORMAT='(D25.6)'), /REMOVE_ALL)
              cal_devtofit = STRCOMPRESS(STRING($
                                 ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_devtofit)[0], $
                                 FORMAT='(D25.6)'), /REMOVE_ALL)
              cal_block_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_block_rsd)
              w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
              IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
                ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
                                     ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_block_rsd), $
                                     /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

              sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd)
              w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
              IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
                ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
                                     ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd), $
                                     /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
            END
        ENDCASE



        subst_name=(substlist[sel_exp])[sel_name]
        IF call_mr_calc THEN prelim_MRs = dp_calc_mrs(subst_name, sel_name, sel_exp, dp_chrom, dp_expcfg, EVAL_MODE=eval_mode) $
          ELSE prelim_MRs = MAKE_ARRAY(n_chrom, /DOUBLE, VALUE=!Values.D_NAN)

        OPENW, lun, fname, /GET_LUN

          PRINTF, lun, '*** IAU_DP_v'+dp_vers+' REPORT ***', FORMAT='(A)'
          
          ; FO 2020-03-03 add chrom date:
          PRINTF, lun, 'Experiment/Date:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), sep, $
                       jultime2timestring(mean(dp_chrom[sel_exp].jdate)), FORMAT='(A,A,A,A,A)'
          PRINTF, lun, 'Processing_Timestamp:', sep, exp_ts, FORMAT='(A,A,A)'
          PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
          PRINTF, lun, 'Cal_Gas:', sep, cal, FORMAT='(A,A,A)'
          PRINTF, lun, 'Substance:', sep, name, sep, 'm/Q:', sep, mass, sep, 'Eval_Mode:', sep, mode_string, $
                       FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
          PRINTF, lun, 'Cal_Interpol:', sep, $
                       ((dp_chrom[sel_exp]).subst[sel_name].rres.cal_ip_mthd)[0], sep, $
                       'Cal_Treat:', sep, ((dp_chrom[sel_exp]).subst[sel_name].rres.cal_treat)[0], sep, $
                       'Sample_Treat:', sep, ((dp_chrom[sel_exp]).subst[sel_name].rres.sam_treat)[0], $
                       FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
          PRINTF, lun, 'Precision Flagging: 0 = undefined, 1 = good, -1 = bad (more than ', prc_limits[0] , $
                       ' sigma), -2 = poor (more than ', prc_limits[1] ,' sigma)' , FORMAT='(A,F7.2,A,F7.2,A)'
          PRINTF, lun, 'Cal_MR:', sep, cal_mr, sep, 'MR_unit', sep, unit, sep, 'Cal_Scale:', sep, cal_scale, sep, $
                       'Instr_rel_MP:', sep, cal_prc, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
          PRINTF, lun, 'Cal_mintomax:', sep, cal_mintomax, sep, 'Cal_devtofit:', sep, cal_devtofit, $
                       sep, 'Cal_block_rsd:', sep, cal_block_rsd, FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
          PRINTF, lun, 'Sam_mean_rsd:', sep, sam_blocks_rsd, FORMAT='(A,A,A)'
          PRINTF, lun, 'Corrections applied:', sep, correction, FORMAT='(A,A,A)'
          PRINTF, lun, 'Separator:', sep, 'TAB', FORMAT='(A,A,A)'

          PRINTF, lun, colheader, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'


          FOR i=0, N_ELEMENTS(sel_chroms)-1 DO BEGIN

              Chromatogram=FILE_BASENAME(((dp_chrom[sel_exp]).fname)[sel_chroms[i]])
              Date=jultime2timestring(((dp_chrom[sel_exp]).jdate)[sel_chroms[i]], /ONLYDATE)
              Time=jultime2timestring(((dp_chrom[sel_exp]).jdate)[sel_chroms[i]], /HMSONLY)
              Sample_Name=((dp_expcfg[sel_exp]).expinfo.s_name)[sel_chroms[i]]

              CASE eval_mode OF
                0: $
                  BEGIN
                    rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.sam_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF rR EQ '-NaN' THEN rR='NaN'
                    Block_rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF Block_rR EQ '-NaN' THEN Block_rR='NaN'
                    Block_RSD=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF Block_RSD EQ '-NaN' THEN Block_RSD='NaN'
                    prc_flag=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.prc_flag)[sel_chroms[i]]), /REMOVE_ALL)
                  END
                1: $
                  BEGIN
                    rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.sam_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF rR EQ '-NaN' THEN rR='NaN'
                    Block_rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF Block_rR EQ '-NaN' THEN Block_rR='NaN'
                    Block_RSD=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                    IF Block_RSD EQ '-NaN' THEN Block_RSD='NaN'
                    prc_flag=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.prc_flag)[sel_chroms[i]]), /REMOVE_ALL)
                  END
              ENDCASE

              prelim_MR=STRCOMPRESS(STRING(prelim_MRs[sel_chroms[i]]), /REMOVE_ALL)

              PRINTF, lun, Chromatogram, sep, Date, sep, Time, sep, Sample_Name, sep, rR, sep, $
                           Block_rR, sep, Block_RSD, sep, prc_flag, sep, prelim_MR, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

          ENDFOR

        CLOSE, lun
      FREE_LUN, lun

    ENDFOR


;--------------------------------------------------*** single substance to individual txt file
  ENDIF ELSE BEGIN
    sel_name=sel_subst

      eval_mode = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_select)[0]
      mode_string = (['signal_area', 'signal_height'])[eval_mode]
      w_corr = WHERE(((dp_chrom[sel_exp])[ix].subst[sel_name].rres.active_corr[0])[0] EQ !TRUE)
      IF w_corr[0] NE -1 THEN correction = (['carry-over'])[w_corr] $
        ELSE correction = 'none'

      name=STRCOMPRESS((dp_chrom[sel_exp])[ix].subst[sel_name].name, /REMOVE_ALL)
      IF verbose THEN print, 'exporting results for: ', name

      exp_ts=jultime2timestring(((dp_chrom[sel_exp]).subst[sel_name].rres.dp_timestamp)[0])

      IF instr NE 'GhOST_ECD' THEN $
        mass=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].mass[(dp_chrom[sel_exp]).subst[sel_name].quant])[0], $
                                FORMAT='(D25.3)'), /REMOVE_ALL) ELSE mass='NaN'

      IF *(dp_expcfg[sel_exp]).cal_mrs.mr_ppt NE !NULL THEN BEGIN
        cal_mr=!VALUES.D_NAN
        unit = 'NA'
        cal_scale= 'NA'
        w=WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).cal_mrs.substance) EQ STRUPCASE(name))
        IF w[0] NE -1 THEN BEGIN
          cal_mr = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
          IF (*((dp_expcfg[sel_exp]).cal_mrs.unit)) NE !NULL THEN $
            unit = (*(dp_expcfg[sel_exp]).cal_mrs.unit)[w[0]] ELSE unit = 'NA'
          cal_scale = (*(dp_expcfg[sel_exp]).cal_mrs.scale)[w[0]]
          IF STRCOMPRESS(STRING(cal_scale), /REMOVE_ALL) EQ '-1' THEN cal_scale= 'NA'
        ENDIF
      ENDIF ELSE BEGIN
        cal_mr = 'NaN'
        unit = 'NA'
        cal_scale = 'NA'
      ENDELSE
      
      cal_prc = 'NaN'
      IF *(dp_expcfg[sel_exp]).instr_prc.mp_rel NE !NULL THEN BEGIN
        w = WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).instr_prc.substance) EQ STRUPCASE(name))
        IF w[0] NE -1 THEN $
          cal_prc = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
      ENDIF


      CASE eval_mode OF
        0: $
          BEGIN
            cal_mintomax = STRCOMPRESS(STRING($
                           ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_mintomax)[0], $
                           FORMAT='(D25.6)'), /REMOVE_ALL)
            cal_devtofit = STRCOMPRESS(STRING($
                           ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_devtofit)[0], $
                           FORMAT='(D25.6)'), /REMOVE_ALL)
            cal_block_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_block_rsd)
            w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
            IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
              ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
                                   ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.cal_block_rsd), $
                                   /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

            sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd)
            w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
            IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
              ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
                                   ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd), $
                                   /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
          END

        1: $
          BEGIN
            cal_mintomax = STRCOMPRESS(STRING($
                           ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_mintomax)[0], $
                           FORMAT='(D25.6)'), /REMOVE_ALL)
            cal_devtofit = STRCOMPRESS(STRING($
                           ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_devtofit)[0], $
                           FORMAT='(D25.6)'), /REMOVE_ALL)
            cal_block_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_block_rsd)
            w_finite = WHERE(FINITE(cal_block_rsd) EQ 1, n_finite)
            IF n_finite EQ 0 THEN cal_block_rsd='NaN' $
              ELSE cal_block_rsd = STRCOMPRESS(STRING(mean($
                                   ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.cal_block_rsd), $
                                   /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)

            sam_blocks_rsd = ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd)
            w_finite = WHERE(FINITE(sam_blocks_rsd) EQ 1, n_finite)
            IF n_finite EQ 0 THEN sam_blocks_rsd='NaN' $
              ELSE sam_blocks_rsd = STRCOMPRESS(STRING(mean($
                                   ((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd), $
                                   /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
          END
      ENDCASE


      subst_name=(substlist[sel_exp])[sel_name]
      IF call_mr_calc THEN prelim_MRs=dp_calc_mrs(subst_name, sel_subst, sel_exp, dp_chrom, dp_expcfg, EVAL_MODE=eval_mode) $
        ELSE prelim_MRs=MAKE_ARRAY(n_chrom, /DOUBLE, VALUE=!Values.D_NAN)


      OPENW, lun, fname, /GET_LUN

        PRINTF, lun, '*** IAU_DP_v'+dp_vers+' REPORT ***', FORMAT='(A)'
        PRINTF, lun, 'Experiment:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), FORMAT='(A,A,A)'
        PRINTF, lun, 'Processing_Timestamp:', sep, exp_ts, FORMAT='(A,A,A)'
        PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
        PRINTF, lun, 'Cal_Gas:', sep, cal, FORMAT='(A,A,A)'
        PRINTF, lun, 'Substance:', sep, name, sep, 'm/Q:', sep, mass, sep, 'Eval_Mode:', sep, mode_string, $
                     FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
        PRINTF, lun, 'Cal_Interpol:', sep, $
                     ((dp_chrom[sel_exp]).subst[sel_name].rres.cal_ip_mthd)[0], sep, $
                     'Cal_Treat:', sep, ((dp_chrom[sel_exp]).subst[sel_name].rres.cal_treat)[0], sep, $
                     'Sample_Treat:', sep, ((dp_chrom[sel_exp]).subst[sel_name].rres.sam_treat)[0], $
                     FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
        PRINTF, lun, 'Precision Flagging: 0 = undefined, 1 = good, -1 = bad (more than ', prc_limits[0] , $
                       ' sigma), -2 = poor (more than ', prc_limits[1] ,' sigma)' , FORMAT='(A,F7.2,A,F7.2,A)'
        PRINTF, lun, 'Cal_MR:', sep, cal_mr, sep, 'MR_unit:', sep, unit, sep, 'Cal_Scale:', sep, cal_scale, sep, $
                     'Instr_rel_MP:', sep, cal_prc, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'
        PRINTF, lun, 'Cal_mintomax:', sep, cal_mintomax, sep, 'Cal_devtofit:', sep, cal_devtofit, $
                     sep, 'Cal_block_rsd:', sep, cal_block_rsd, FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
        PRINTF, lun, 'Sam_mean_rsd:', sep, sam_blocks_rsd, FORMAT='(A,A,A)'
        PRINTF, lun, 'Corrections applied:', sep, correction, FORMAT='(A,A,A)'
        PRINTF, lun, 'Separator:', sep, 'TAB', FORMAT='(A,A,A)'

        PRINTF, lun, colheader, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'



        FOR i=0, N_ELEMENTS(sel_chroms)-1 DO BEGIN

            Chromatogram=FILE_BASENAME(((dp_chrom[sel_exp]).fname)[sel_chroms[i]])
            Date=jultime2timestring(((dp_chrom[sel_exp]).jdate)[sel_chroms[i]], /ONLYDATE)
            Time=jultime2timestring(((dp_chrom[sel_exp]).jdate)[sel_chroms[i]], /HMSONLY)
            Sample_Name=((dp_expcfg[sel_exp]).expinfo.s_name)[sel_chroms[i]]

            CASE eval_mode OF
              0: $
                BEGIN
                  rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.sam_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF rR EQ '-NaN' THEN rR='NaN'
                  Block_rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF Block_rR EQ '-NaN' THEN Block_rR='NaN'
                  Block_RSD=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.block_rsd)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF Block_RSD EQ '-NaN' THEN Block_RSD='NaN'
                  prc_flag=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_area.prc_flag)[sel_chroms[i]]), /REMOVE_ALL)
                END
              1: $
                BEGIN
                  rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.sam_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF rR EQ '-NaN' THEN rR='NaN'
                  Block_rR=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rrsp)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF Block_rR EQ '-NaN' THEN Block_rR='NaN'
                  Block_RSD=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.block_rsd)[sel_chroms[i]], FORMAT='(D25.6)'), /REMOVE_ALL)
                  IF Block_RSD EQ '-NaN' THEN Block_RSD='NaN'
                  prc_flag=STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[sel_name].rres.rsp_height.prc_flag)[sel_chroms[i]]), /REMOVE_ALL)
                END
            ENDCASE

            prelim_MR=STRCOMPRESS(STRING(prelim_MRs[sel_chroms[i]]), /REMOVE_ALL)

            PRINTF, lun, Chromatogram, sep, Date, sep, Time, sep, Sample_Name, sep, rR, sep, $
                         Block_rR, sep, Block_RSD, sep, prc_flag, sep, prelim_MR, FORMAT='(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)'

        ENDFOR

        CLOSE, lun
      FREE_LUN, lun

  ENDELSE

  IF verbose THEN BEGIN
    print, 'results export completed.'
    print, '+++'
  ENDIF

  dp_refr_status, MESSAGE='Results exported.'

END