;+
; PRO: dp_sel_meas_table
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: allows manual selection of specific chromatograms that will then be used or not used for rR etc. calculation
;          of a specific substance.
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_sel_meas_table, event

  COMMON DP_DATA
  COMMON DP_WIDID

  dp_widid.dp_selmeas = event.top

  uname = WIDGET_INFO(event.id, /UNAME)

  ID=WIDGET_INFO(event.top, FIND_BY_UNAME='restable')
  WIDGET_CONTROL, ID, GET_VALUE=tblval

  ID_experiment = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='exp_dl')
  sel_exp       = WIDGET_INFO(ID_experiment, /DROPLIST_SELECT)
  ID_substance  = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='subst_dl')
  sel_subst     = WIDGET_INFO(ID_substance, /DROPLIST_SELECT)

  tmp = (dp_chrom[sel_exp])
  tmp.subst[sel_subst].rres.use_flag = tblval.use_flag
  (dp_chrom[sel_exp]) = tmp

END

;#####################################################################################################################

PRO dp_sel_meas_table_ini, sel_exp, sel_subst

  COMMON DP_DATA

  ref_seltable = {  NUMBER       : 0L, $
                    FILENAME     : '', $
                    S_ID         : 0L, $
                    SAMPLE_NAME  : '', $
                    USE_FLAG     : 0L $
                  }


  tblval=!NULL
  tblval=REPLICATE(ref_seltable, N_ELEMENTS(dp_chrom[sel_exp]))

  tblval.NUMBER       = 1+INDGEN(N_ELEMENTS(dp_chrom[sel_exp]))
  tblval.FILENAME     = FILE_BASENAME(dp_chrom[sel_exp].fname)
  tblval.S_ID         = (dp_expcfg[sel_exp]).expinfo.s_id
  tblval.SAMPLE_NAME  = (dp_expcfg[sel_exp]).expinfo.s_name
  tblval.USE_FLAG     = (dp_chrom[sel_exp]).subst[sel_subst].rres.use_flag

  column_width=[1.4, $;    NUMBER
                3.5, $;    FILENAME
                1.0, $;    S_ID
                2.5, $;    SAMPLE_NAME
                2.0 $;     USE_FLAG
                ]

  tbl_base=WIDGET_BASE(TITLE=((dp_chrom[sel_exp]).subst[sel_subst].name)[0]+' : select measurements...')

  tbl_id=WIDGET_TABLE(tbl_base, VALUE=tblval, COLUMN_LABELS=TAG_NAMES(ref_seltable), SCR_XSIZE=18, SCR_YSIZE=26, $
                      XSIZE=N_ELEMENTS(column_width), COLUMN_WIDTH=column_width, units=2, /ROW_MAJOR, /SCROLL, $
                      UNAME='restable', UVALUE=((dp_chrom[sel_exp]).subst[sel_subst].name)[0], /EDITABLE)

  WIDGET_CONTROL, tbl_id, /REALIZE

  dp_refr_status, MESSAGE='Created selection table.'

  XMANAGER, 'dp_sel_meas_table', tbl_base, /NO_BLOCK, EVENT_HANDLER='dp_sel_meas_table'

END