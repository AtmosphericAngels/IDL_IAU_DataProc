;+
; FUNCTION dp_read_expinfo_file
;
; AUTHOR: F. Obersteiner, Nov. 2016
;
; PURPOSE:  common frame for reading experiment info files
;-
;--------------------------------------------------------------------------------------------------------------------
FUNCTION dp_read_expinfofile, file, exp_no, SEP=sep, DEF_OFFSET=def_offset, NAME=name, $
                         SEARCH_TAGS=search_tags, NL=nl, S_VOL_SELECT=s_vol_select, INSTRUMENT=instrument

  COMMON dp_data

    IF NOT KEYWORD_SET(sep) THEN sep = STRING(9b)
    IF NOT KEYWORD_SET(def_offset) THEN def_offset = 0L
    IF NOT KEYWORD_SET(s_vol_select) THEN s_vol_select = 0
    IF NOT KEYWORD_SET(instrument) THEN instrument = 'undef'
    IF NOT KEYWORD_SET(search_tags) THEN search_tags = ['opr','com','rv_vol','fname','nr','s_name','s_date','s_lat', $
                                                        's_lon','s_alt','s_id','s_vol','rv_ps','rv_pe','rv_dp', $
                                                        'mfc_flow','mfc_vol','Ts_cldhd','Te_cldhd','ts','te','dt','use']

    IF N_ELEMENTS(search_tags) NE 23 THEN RETURN, !NULL

    nl = FILE_LINES(file)

    OPENR, lun, file, /GET_LUN ; first run: check header size and separator
    line = ''
    FOR i=0, nl-1 DO BEGIN ; extract header information
      READF, lun, line
      IF line.contains('separator', /FOLD_CASE) EQ 1 THEN BEGIN
        IF line.contains('tab', /FOLD_CASE) THEN sep = STRING(9B)
        IF line.contains('semicolon', /FOLD_CASE) THEN sep = STRING(59B)
        ; add additional separators here
      ENDIF
      IF line.contains('header_size', /FOLD_CASE) EQ 1 THEN BEGIN
        tmp = strsplit(STRTRIM(line), sep, /EXTRACT)
        def_offset = FIX(tmp[-1], TYPE=2)
      ENDIF
      IF line.contains('s_vol_select', /FOLD_CASE) EQ 1 THEN BEGIN
        tmp = strsplit(STRTRIM(line), sep, /EXTRACT)
        IF STRUPCASE(tmp[-1]) EQ 'MFC' THEN s_vol_select = 1
        IF STRUPCASE(tmp[-1]) EQ 'DP' THEN s_vol_select = 0
      ENDIF
    ENDFOR
    CLOSE, lun
    FREE_LUN, lun


    IF nl-(def_offset+1) NE N_ELEMENTS((dp_chrom[exp_no]).fname) THEN BEGIN ; check number of measurements
      def_meas = STRCOMPRESS(STRING(nl-(def_offset+1)), /REMOVE_ALL)
      is_meas = STRCOMPRESS(STRING(N_ELEMENTS((dp_chrom[exp_no]).fname)), /REMOVE_ALL)
      !NULL = DIALOG_MESSAGE(FILE_BASENAME(file)+$
                         ': Number of measurements defined in expinfo file not equal to number of files in loaded experiment '+ $
                         '('+def_meas+' vs. '+is_meas+').', /ERROR)
      RETURN, !NULL
    ENDIF


    ref_expinfo = create_ref_expinfo() ; generate the empty structure for experiment info
    import=REPLICATE(ref_expinfo, nl-(def_offset+1))

    OPENR, lun, file, /GET_LUN ; reopen file for second run to import data
      line = ''
        FOR i=0, def_offset-1 DO READF, lun, line ; skip file header this time

    READF, lun, line ; read col header
    import_tags = STRUPCASE(strsplit(STRTRIM(line), sep, /EXTRACT))
    search_tags = STRUPCASE(search_tags)

    remove_strings=['[', ']', '.', '/'] ; clean col header and search tags of nasty stuff
    FOR i=0, N_ELEMENTS(remove_strings)-1 DO BEGIN
      search_tags=strreplace_iter(search_tags, remove_strings[i], '_', n_iter=MAX(STRLEN(search_tags)))
      import_tags=strreplace_iter(import_tags, remove_strings[i], '_', n_iter=MAX(STRLEN(import_tags)))
    ENDFOR

    col_ix = LONARR(N_ELEMENTS(search_tags))-1 ; get indices of respective columns...
    FOR i=0, N_ELEMENTS(search_tags)-1 DO BEGIN
      ix_vd=WHERE(STRMATCH(import_tags, search_tags[i], /FOLD_CASE) EQ 1)
      IF ix_vd[0] EQ -1 THEN col_ix[i] = -1 ELSE col_ix[i] = ix_vd[0]
    ENDFOR

    w_ess=[3, 5, 10, 12, 13, 16] ; fname, sample_name, sample_id, p0, p1, mfc_vol
    found_ess_ix = col_ix[w_ess]

    IF found_ess_ix[-1] EQ -1 AND (found_ess_ix[-3]*found_ess_ix[-2]) GT 0 THEN BEGIN
      found_ess_ix[-1] = 999
      s_vol_select = 0 ; coerce dp as s_vol because no MFC value found
    ENDIF

    IF (found_ess_ix[-1] NE -1) AND (found_ess_ix[-3] EQ -1) AND (found_ess_ix[-2] EQ -1) THEN BEGIN
      found_ess_ix[-2] = 999
      found_ess_ix[-3] = 999
      s_vol_select = 1 ; coerce mfc as s_vol because no dp value found
    ENDIF

    IF (WHERE(found_ess_ix EQ -1))[0] NE -1 THEN BEGIN
      errormsg = STRARR(N_ELEMENTS(w_ess)+1)
      errormsg[0] = 'Essential tag not found. Please make sure that the following parameters are specified:'
      errormsg[1:-1] = search_tags[w_ess]
      !NULL=DIALOG_MESSAGE(errormsg, /ERROR)
      CLOSE, lun
      FREE_LUN, lun
      RETURN, !NULL
    ENDIF

    n_chrom = nl[0]-def_offset-1

    FOR i=0, n_chrom-1 DO BEGIN; begin loop over all chromatograms listed in expinfo-file

      READF, lun, line

      tmp = strsplit(STRTRIM(line), sep, /EXTRACT, /PRESERVE_NULL)

      import[i].expinfo_fname = FILE_BASENAME(file)

      IF col_ix[0] NE -1 THEN import[i].opr = tmp[col_ix[0]]
      IF col_ix[1] NE -1 THEN import[i].com = [tmp[col_ix[1]]]
      IF col_ix[2] NE -1 THEN import[i].rv_vol = tmp[col_ix[2]]

      import[i].fname = tmp[col_ix[3]]

      IF col_ix[4] NE -1 THEN import[i].nr = tmp[col_ix[4]] $
        ELSE import[i].nr = i

      import[i].s_name = tmp[col_ix[5]]

      IF col_ix[6] NE -1 THEN import[i].s_date = tmp[col_ix[6]]

      IF col_ix[7] NE -1 THEN import[i].s_lat = tmp[col_ix[7]]
      IF col_ix[8] NE -1 THEN import[i].s_lon = tmp[col_ix[8]]
      IF col_ix[9] NE -1 THEN import[i].s_alt = tmp[col_ix[9]]

      import[i].s_id = tmp[col_ix[10]]

      IF col_ix[11] NE -1 THEN import[i].s_vol = tmp[col_ix[11]]

      import[i].s_vol_select = s_vol_select

      import[i].rv_ps = DOUBLE(tmp[col_ix[12]])
      import[i].rv_pe = DOUBLE(tmp[col_ix[13]])

      IF col_ix[14] NE -1 THEN import[i].rv_dp = DOUBLE(tmp[col_ix[14]]) $
        ELSE import[i].rv_dp = DOUBLE(tmp[col_ix[13]])-DOUBLE(tmp[col_ix[12]])

      IF col_ix[15] NE -1 THEN import[i].mfc_flow = DOUBLE(tmp[col_ix[15]])
      IF col_ix[16] NE -1 THEN import[i].mfc_vol = DOUBLE(tmp[col_ix[16]])

      IF col_ix[17] NE -1 THEN import[i].Ts_cldhd = tmp[col_ix[17]]
      IF col_ix[18] NE -1 THEN import[i].Te_cldhd = tmp[col_ix[18]]

      IF col_ix[19] NE -1 THEN import[i].ts = time2jultime(TIME = tmp[col_ix[19]])
      IF col_ix[20] NE -1 THEN import[i].te = time2jultime(TIME = tmp[col_ix[20]])
      IF col_ix[21] NE -1 THEN import[i].dt = time2jultime(TIME = tmp[col_ix[21]]) $
        ELSE IF import[i].ts NE !VALUES.D_NAN AND import[i].te NE !VALUES.D_NAN THEN $
                  import[i].dt = import[i].te - import[i].ts

      IF col_ix[22] NE -1 THEN import[i].use = FIX(tmp[col_ix[22]], TYPE=2)

    ENDFOR ; end file lines loop (chromatograms)

    CLOSE, lun
    FREE_LUN, lun


    CASE s_vol_select OF
      0: import.s_vol = import.rv_dp
      1: import.s_vol = import.mfc_vol
    ENDCASE


  RETURN, import

END