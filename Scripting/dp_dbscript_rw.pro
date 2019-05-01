;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; 17-08, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; FUNCTION dp_dbscript_read reads iau_dataproc database reference table and returns collected data.
; FUNCTION dp_dbscript_write writes iau_dataproc database reference table with specific filename
; generated from date and time.
;
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_dbscript_read, FILE=file, PATH=path_wd

  sep = ';'
  dp_dbstrct = !NULL

  IF NOT KEYWORD_SET(file) THEN $
    file = DIALOG_PICKFILE(TITLE='Please chose a database reference table (.csv)', FILTER='*.csv', $
                           PATH=path_wd)

  IF file EQ !NULL OR STRLEN(file) EQ 0 THEN RETURN, dp_dbstrct

  nl = FILE_LINES(file)
  table = STRARR(nl)

  OPENR, lun, file, /GET_LUN
  READF, lun, table
  CLOSE, lun
  FREE_LUN, lun

  n_lines_hdr = LONG(table[0])

  IF n_lines_hdr EQ nl THEN RETURN, !NULL

  db_strct = { $
                active    :   -1,  $
                exp_id    :   '', $
                exp_descr    :   '', $
                chromdata_path    :   '', $
                expinfo_path    :   '', $
                expinfo_import_fct    :   '', $
                dp_savefile_path    :   '', $
                dp_treatcfg_path    :   '',  $
                dp_calmrs_path    :   '',  $
                dp_tgtmrs_path    :   '',  $
                dp_instr_prc_path    :   '',  $
                subst_namedef_path    :   '',  $
                save_txtreport    :   -1,  $
                analyse_NL    :   -1,  $
                proc_timestamp    :   '',  $
                corr_carryover    :   ''  $
              }

  dp_dbstrct = {header : table[0:n_lines_hdr-1], data : REPLICATE(db_strct, nl-n_lines_hdr)}

  FOR i=0, (nl-n_lines_hdr)-1 DO BEGIN ; loop over all defined experiments
    tmp=STRCOMPRESS(strsplit(STRTRIM(table[i+n_lines_hdr]), sep, /EXTRACT, /PRESERVE_NULL), /REMOVE_ALL)
    dp_dbstrct.data[i].active = LONG(tmp[0])
    dp_dbstrct.data[i].exp_id = tmp[1]
    dp_dbstrct.data[i].exp_descr = tmp[2]
    dp_dbstrct.data[i].chromdata_path = tmp[3]
    dp_dbstrct.data[i].expinfo_path = tmp[4]
    dp_dbstrct.data[i].expinfo_import_fct = tmp[5]
    dp_dbstrct.data[i].dp_savefile_path = tmp[6]
    dp_dbstrct.data[i].dp_treatcfg_path = tmp[7]
    dp_dbstrct.data[i].dp_calmrs_path = tmp[8]
    dp_dbstrct.data[i].dp_tgtmrs_path = tmp[9]
    dp_dbstrct.data[i].dp_instr_prc_path = tmp[10]
    dp_dbstrct.data[i].subst_namedef_path = tmp[11]
    dp_dbstrct.data[i].save_txtreport = tmp[12]
    dp_dbstrct.data[i].analyse_NL = tmp[13]
    dp_dbstrct.data[i].proc_timestamp = tmp[14]
    IF N_ELEMENTS(tmp) GT 15 THEN dp_dbstrct.data[i].corr_carryover = tmp[15]
  ENDFOR ; end loop through experiments

  RETURN, dp_dbstrct

END

;------------------------------------------------------------------------------------------------------------------------

FUNCTION dp_dbscript_write, dp_dbstrct, PATH=path, UTC=utc

  sep = ';'

  IF SIZE(dp_dbstrct, /TYPE) NE 8 THEN RETURN, !NULL

  IF NOT KEYWORD_SET(path) THEN $
    path = DIALOG_PICKFILE(TITLE='Please chose a folder where to store database reference table', /DIRECTORY)

  time = SYSTIME(/JULIAN, UTC=utc)

  CALDAT, time, MM, DD, YY, HH, MN, SS

  db_fname = path+'\'+$
    STRING(yy, FORMAT='(I04)')+STRING(mm, FORMAT='(I02)')+STRING(dd, FORMAT='(I02)')+$
    STRING(hh, FORMAT='(I02)')+STRING(mn, FORMAT='(I02)')+$
    '_iaudataproc_db.csv'

  n_lines_hdr = LONG(dp_dbstrct.header[0])
  n_exp = N_ELEMENTS(dp_dbstrct.data.active)
  cnf_str = STRARR(n_exp)

  dp_dbstrct.header[-3] = jultime2timestring(SYSTIME(/JULIAN))+sep

  FOR i=0, n_exp-1 DO BEGIN
    cnf_str[i] = $
    STRING(dp_dbstrct.data[i].active, FORMAT='(I)') + sep + $
    dp_dbstrct.data[i].exp_id + sep + $
    dp_dbstrct.data[i].exp_descr + sep + $
    dp_dbstrct.data[i].chromdata_path + sep + $
    dp_dbstrct.data[i].expinfo_path + sep + $
    dp_dbstrct.data[i].expinfo_import_fct + sep + $
    dp_dbstrct.data[i].dp_savefile_path + sep + $
    dp_dbstrct.data[i].dp_treatcfg_path + sep + $
    dp_dbstrct.data[i].dp_calmrs_path + sep + $
    dp_dbstrct.data[i].dp_tgtmrs_path + sep + $
    dp_dbstrct.data[i].dp_instr_prc_path + sep + $
    dp_dbstrct.data[i].subst_namedef_path + sep + $
    STRING(dp_dbstrct.data[i].save_txtreport, FORMAT='(I)') + sep + $
    STRING(dp_dbstrct.data[i].analyse_NL, FORMAT='(I)') + sep + $
    dp_dbstrct.data[i].proc_timestamp

    IF (WHERE(TAG_NAMES(dp_dbstrct.data) EQ 'CORR_CARRYOVER'))[0] NE -1 THEN $
      cnf_str[i] += sep + dp_dbstrct.data[i].corr_carryover $
    ELSE cnf_str[i] += sep + sep

  ENDFOR

  OPENW, lun, db_fname, /GET_LUN
    FOR j=0, n_lines_hdr-1 DO PRINTF, lun, dp_dbstrct.header[j]
    FOR i=0, n_exp-1 DO PRINTF, lun, STRCOMPRESS(cnf_str[i])
  CLOSE, lun
  FREE_LUN, lun

  RETURN, 1

END