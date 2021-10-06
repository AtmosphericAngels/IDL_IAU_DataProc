;+
; PRO: dp_show_restable
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: show results of data processing for selected experiment and substance (brief and detailed version)
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_show_restable, sel_exp, sel_subst, BRIEF=brief

  COMMON DP_DATA

  IF NOT KEYWORD_SET(brief) THEN brief = 0

  n_chrom = N_ELEMENTS((dp_chrom[sel_exp]))
  subst_name = (substlist[sel_exp])[sel_subst]
  eval_mode = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]
  mode_string = (['AREA', 'HEIGHT'])[eval_mode]

  IF ((dp_expcfg[sel_exp]).cal_mrs.canister) NE '' THEN $
    prelim_MRs=dp_calc_mrs(subst_name, sel_subst, sel_exp, dp_chrom, dp_expcfg, eval_mode) $
      ELSE prelim_MRs=MAKE_ARRAY(n_chrom, /DOUBLE, VALUE=!Values.D_NAN)

  IF brief THEN BEGIN ;++++++++++++++++++++++++++++++++++++++++BRIEF+++

    id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
    id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
    id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

    ids = (dp_expcfg[sel_exp]).expinfo.s_id
    w = WHERE(ids EQ id_sam OR ids EQ id_tgt, n_sam)
    IF n_sam EQ 0 THEN RETURN

    ref_restable = {  NUMBER       : 0L, $
                      FILENAME     : '', $
;                      S_ID         : 0L, $
                      SAMPLE_NAME  : '', $
;                      INT_FLAG     : 0L, $
  ;                    INT_COM      : '', $ ;
  ;                    QUANT        : 0L, $ ;
  ;                    MASS         : '', $ ;
;                      RV_DP        : '', $
  ;                    MFC_VOL      : '', $ ;
;                      DT_PRECON    : '', $
  ;                    T_RET        : '', $ ;
  ;                    n_av         : '', $ ;
  ;                    n_hv         : '', $ ;
  ;                    S_N          : '', $ ;
;                      USE_FLAG    : 0L, $ ;
                      MODE         : mode_string, $
                      RRSP         : '', $
                      MV_RRSP      : '', $
                      RSD_RRSP     : '', $
;                      RSD_CALBLOCK : '', $
                      SYS_PRC      : '', $
                      PRC_FLAG     : 0L, $
                      PRELIM_MR    : ''       }

    tblval = !NULL
    tblval=REPLICATE(ref_restable, n_sam)

    tblval.NUMBER       = (1+INDGEN(N_ELEMENTS(dp_chrom[sel_exp])))[w]
    tblval.FILENAME     = (FILE_BASENAME(dp_chrom[sel_exp].fname))[w]
;    tblval.S_ID         = (dp_expcfg[sel_exp]).expinfo.s_id
    tblval.SAMPLE_NAME  = ((dp_expcfg[sel_exp]).expinfo.s_name)[w]
;    tblval.INT_FLAG     = (dp_chrom[sel_exp]).subst[sel_subst].ires.flag
  ;  tblval.INT_COM      = (dp_chrom[sel_exp]).subst[sel_subst].ires.comment ;
  ;  tblval.QUANT        = (dp_chrom[sel_exp]).subst[sel_subst].quant ;
  ;  tblval.MASS         = STRING(((dp_chrom[sel_exp]).subst[sel_subst].mass)[tblval.QUANT], FORMAT='(F12.4)') ;
;    tblval.RV_DP        = STRCOMPRESS(STRING((dp_expcfg[sel_exp]).expinfo.rv_dp, FORMAT='(D12.3)'), /REMOVE_ALL)
  ;  tblval.MFC_VOL      = STRING((dp_expcfg[sel_exp]).expinfo.mfc_vol, FORMAT='(F12.3)') ;
;    tblval.DT_PRECON    = (dp_expcfg[sel_exp]).expinfo.dt
  ;  tblval.T_RET        = STRING((dp_chrom[sel_exp]).subst[sel_subst].ires.rt, FORMAT='(F12.3)') ;
  ;  tblval.n_av         = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.normalised, FORMAT='(F12.4)') ;
  ;  tblval.n_hv         = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.normalised, FORMAT='(F12.4)') ;
  ;  tblval.S_N          = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.s_n, FORMAT='(I12)') ;
;    tblval.USE_FLAG    = (dp_chrom[sel_exp]).subst[sel_subst].rres.use_flag ;

    CASE eval_mode OF
      0: $
        BEGIN
          tblval.RRSP         = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.sam_rrsp, $
                                 FORMAT='(D12.4)'), /REMOVE_ALL))[w]
          tblval.MV_RRSP      = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.block_rrsp, $
                                 FORMAT='(D12.4)'), /REMOVE_ALL))[w]
          tblval.RSD_RRSP     = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.block_rsd*100., $
                                 FORMAT='(D12.3)'), /REMOVE_ALL))[w]
          ;    tblval.RSD_CALBLOCK = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.cal_block_rsd*100., $
          ;                                              FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.SYS_PRC      = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.sys_prc*100., $
                                 FORMAT='(D12.3)'), /REMOVE_ALL))[w]
          tblval.PRC_FLAG     = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.prc_flag)[w]
        END
      1: $
        BEGIN
          tblval.RRSP         = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.sam_rrsp, $
                                 FORMAT='(D12.4)'), /REMOVE_ALL))[w]
          tblval.MV_RRSP      = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.block_rrsp, $
                                 FORMAT='(D12.4)'), /REMOVE_ALL))[w]
          tblval.RSD_RRSP     = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.block_rsd*100., $
                                 FORMAT='(D12.3)'), /REMOVE_ALL))[w]
          ;    tblval.RSD_CALBLOCK = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.cal_block_rsd*100., $
          ;                                              FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.SYS_PRC      = (STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.sys_prc*100., $
                                 FORMAT='(D12.3)'), /REMOVE_ALL))[w]
          tblval.PRC_FLAG     = ((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.prc_flag)[w]
        END
    ENDCASE

    tblval.PRELIM_MR    = (STRCOMPRESS(STRING(prelim_MRs), /REMOVE_ALL))[w]



    column_width=[1.4, $;    NUMBER
                  3.5, $;    FILENAME
;                  1.0, $;    S_ID
                  2.5, $;    SAMPLE_NAME
;                  2.0, $;    INT_FLAG
              ;    2.0, $;    INT_COM
              ;    1.5, $;    QUANT
              ;    2.0, $;    MASS
;                  2.0, $;    RV_DP
              ;    2.0, $;    MFC_VOL
;                  2.0, $;    DT_PRECON
              ;    2.0, $;    T_RET
              ;    2.0, $;    n_av
              ;    2.0, $;    n_hv
              ;    2.0, $;    S_N
;                  2.0, $;    USE_FLAG
                  2.0, $;    MODE
                  2.0, $;    RRSP
                  2.0, $;    MV_RRSP
                  2.4, $;    RSD_RRSP
;                  2.4, $;    RSD_CALBLOCK
                  2.0, $;    SYS_PRC
                  2.0, $;   PRC_FLAG
                  2.4     ]; PRELIM_MR


    scr_xsize = 24
    scr_ysize = 20

    col_labels = TAG_NAMES(ref_restable)
    col_labels[6] = col_labels[6]+'[%]'
    col_labels[7] = col_labels[7]+'[%]'

  ENDIF ELSE BEGIN ;++++++++++++++++++++++++++++++++++++++++DETAILED+++

    ref_restable = {  NUMBER       : 0L, $
                      FILENAME     : '', $
                      S_ID         : 0L, $
                      S_TYPE       : '', $
                      SAMPLE_NAME  : '', $
                      INT_FLAG     : 0L, $
  ;                    INT_COM      : '', $ ;
  ;                    QUANT        : 0L, $ ;
  ;                    MASS         : '', $ ;
                      RV_DP        : '', $
  ;                    MFC_VOL      : '', $ ;
                      DT_PRECON    : '', $
  ;                    T_RET        : '', $ ;
  ;                    n_av         : '', $ ;
  ;                    n_hv         : '', $ ;
  ;                    S_N          : '', $ ;
                      USE_FLAG     : 0L, $ ;
                      MODE         : mode_string, $
                      RRSP         : '', $
                      MV_RRSP      : '', $
                      RSD_RRSP     : '', $
                      RSD_CALBLOCK : '', $
                      SYS_PRC      : '', $
                      PRC_FLAG     : 0L, $
                      PRELIM_MR    : ''       }

    tblval = !NULL
    tblval=REPLICATE(ref_restable, N_ELEMENTS(dp_chrom[sel_exp]))

    tblval.NUMBER       = 1+INDGEN(N_ELEMENTS(dp_chrom[sel_exp]))
    tblval.FILENAME     = FILE_BASENAME(dp_chrom[sel_exp].fname)
    tblval.S_ID         = (dp_expcfg[sel_exp]).expinfo.s_id
    tblval.S_TYPE       = sid_name[(dp_expcfg[sel_exp]).expinfo.s_id-1]
    tblval.SAMPLE_NAME  = (dp_expcfg[sel_exp]).expinfo.s_name
    tblval.INT_FLAG     = (dp_chrom[sel_exp]).subst[sel_subst].ires.flag
  ;  tblval.INT_COM      = (dp_chrom[sel_exp]).subst[sel_subst].ires.comment ;
  ;  tblval.QUANT        = (dp_chrom[sel_exp]).subst[sel_subst].quant ;
  ;  tblval.MASS         = STRING(((dp_chrom[sel_exp]).subst[sel_subst].mass)[tblval.QUANT], FORMAT='(F12.4)') ;
    tblval.RV_DP        = STRCOMPRESS(STRING((dp_expcfg[sel_exp]).expinfo.rv_dp, FORMAT='(D12.3)'), /REMOVE_ALL)
  ;  tblval.MFC_VOL      = STRING((dp_expcfg[sel_exp]).expinfo.mfc_vol, FORMAT='(F12.3)') ;
    tblval.DT_PRECON    = (dp_expcfg[sel_exp]).expinfo.dt
  ;  tblval.T_RET        = STRING((dp_chrom[sel_exp]).subst[sel_subst].ires.rt, FORMAT='(F12.3)') ;
  ;  tblval.n_av         = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.normalised, FORMAT='(F12.4)') ;
  ;  tblval.n_hv         = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.normalised, FORMAT='(F12.4)') ;
  ;  tblval.S_N          = STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.s_n, FORMAT='(I12)') ;
    tblval.USE_FLAG    = (dp_chrom[sel_exp]).subst[sel_subst].rres.use_flag ;

    CASE eval_mode OF
      0: $
        BEGIN
          tblval.RRSP         = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.sam_rrsp, $
                                  FORMAT='(D12.4)'), /REMOVE_ALL)
          tblval.MV_RRSP      = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.block_rrsp, $
                                  FORMAT='(D12.4)'), /REMOVE_ALL)
          tblval.RSD_RRSP     = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.block_rsd*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.RSD_CALBLOCK = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.cal_block_rsd*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.SYS_PRC      = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.sys_prc*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.PRC_FLAG     = (dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_area.prc_flag
        END
      1: $
        BEGIN
          tblval.RRSP         = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.sam_rrsp, $
                                  FORMAT='(D12.4)'), /REMOVE_ALL)
          tblval.MV_RRSP      = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.block_rrsp, $
                                  FORMAT='(D12.4)'), /REMOVE_ALL)
          tblval.RSD_RRSP     = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.block_rsd*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.RSD_CALBLOCK = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.cal_block_rsd*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.SYS_PRC      = STRCOMPRESS(STRING((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.sys_prc*100., $
                                  FORMAT='(D12.3)'), /REMOVE_ALL)
          tblval.PRC_FLAG     = (dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_height.prc_flag
        END
    ENDCASE

    tblval.PRELIM_MR    = STRCOMPRESS(STRING(prelim_MRs), /REMOVE_ALL)



    t_precon = STRARR(n_chrom)
    FOR i=0, n_chrom-1 DO BEGIN
      caldat, SYSTIME(/JULIAN), mm, dd, yyyy, hh0, mn0, ss0
      caldat, SYSTIME(/JULIAN)+(tblval.DT_PRECON)[i], mm, dd, yyyy, hh1, mn1, ss1
      IF FIX(hh1, TYPE=4) GT FIX(hh0, TYPE=4) THEN $
        d_mn = (60.-FIX(mn0, TYPE=4))+FIX(mn1, TYPE=4) ELSE $
        d_mn = FIX(mn1, TYPE=4)-FIX(mn0, TYPE=4)
      d_ss = (60.-FIX(ss0, TYPE=4))+FIX(ss1, TYPE=4)+(d_mn-1.)*60.
      t_precon[i] = STRCOMPRESS(STRING(d_ss, FORMAT='(I)'), /REMOVE_ALL)
    ENDFOR
    tblval.DT_PRECON = t_precon



    column_width=[1.4, $;    NUMBER
                  4.5, $;    FILENAME
                  1.0, $;    S_ID
                  2.5, $;    S_TYPE
                  3.0, $;    SAMPLE_NAME
                  2.0, $;    INT_FLAG
              ;    2.0, $;    INT_COM
              ;    1.5, $;    QUANT
              ;    2.0, $;    MASS
                  2.0, $;    RV_DP
              ;    2.0, $;    MFC_VOL
                  2.0, $;    DT_PRECON
              ;    2.0, $;    T_RET
              ;    2.0, $;    n_av
              ;    2.0, $;    n_hv
              ;    2.0, $;    S_N
                  2.0, $;    USE_FLAG
                  2.0, $;    MODE
                  2.0, $;    RRSP
                  2.0, $;    MV_RRSP
                  2.4, $;    RSD_RRSP
                  2.4, $;    RSD_CALBLOCK
                  2.0, $;    SYS_PRC
                  2.0, $;   PRC_FLAG
                  2.4     ]; PRELIM_MR


    scr_xsize = 39
    scr_ysize = 20

    col_labels = TAG_NAMES(ref_restable)
    col_labels[12] = col_labels[12]+'[%]'
    col_labels[13] = col_labels[13]+'[%]'
    col_labels[14] = col_labels[14]+'[%]'

  ENDELSE


  mainbase = WIDGET_BASE(title='Results for: '+((dp_chrom[sel_exp]).subst[sel_subst].name)[0])

  ID=WIDGET_TABLE(mainbase, VALUE=tblval, COLUMN_LABELS=col_labels, SCR_XSIZE=scr_xsize, SCR_YSIZE=scr_ysize, $
                  XSIZE=N_ELEMENTS(column_width), COLUMN_WIDTH=column_width, units=2, /ROW_MAJOR, /SCROLL, $
                  UNAME='restable', UVALUE=((dp_chrom[sel_exp]).subst[sel_subst].name)[0])

  WIDGET_CONTROL, ID, /REALIZE

  dp_refr_status, MESSAGE='Created results table.'

END