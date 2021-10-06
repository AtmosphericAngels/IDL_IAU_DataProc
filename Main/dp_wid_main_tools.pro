PRO config_dp_mainwid, event, DEF_UNAME=def_uname
; a droplist selection is changed on the main widget: check the new status and compare to the selections of
; other droplists. change or warn if necessary.

COMMON dp_data

; get uname...
  IF NOT KEYWORD_SET(def_uname) THEN $
    uname_sel = WIDGET_INFO(event.id, /UNAME) ELSE $; check caller
      uname_sel = def_uname

; gather abundantly used information...
  ID_chroms_dl = WIDGET_INFO(event.top, find_by_uname='chroms_dl')
  ID_expinfo_dl = WIDGET_INFO(event.top, find_by_uname='expinfo_dl')

  ID_instr = WIDGET_INFO(event.top, find_by_uname='instr_dl')
  ID_etype = WIDGET_INFO(event.top, find_by_uname='exptype_dl')
  ID_espec = WIDGET_INFO(event.top, find_by_uname='expspec_dl')

  sel_exp = WIDGET_INFO(ID_chroms_dl, /DROPLIST_SELECT)
  sel_expinfo = WIDGET_INFO(ID_expinfo_dl, /DROPLIST_SELECT)

; execute case-specific behavior
  CASE uname_sel OF

    'restore_chrom' : $
      BEGIN
        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN
          chromlist = []
          FOR i=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN
            chromlist=[chromlist, ((dp_chrom[i]).exp_fname)[0]]
            IF i EQ 0 THEN substlist=LIST(((dp_chrom[0]).subst.name)[*,0]) $
              ELSE substlist.add, ((dp_chrom[i]).subst.name)[*,0]
          ENDFOR
        ENDIF ELSE chromlist = ''

        WIDGET_CONTROL, ID_chroms_dl, set_VALUE=chromlist
        WIDGET_CONTROL, ID_expinfo_dl, set_VALUE=''
        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_val


        ; inst_val = ['Lab_QP/SFMS', 'GhOST_MS', 'Lab_BenchTOF', 'FASTOF', 'GhOST_ECD', 'AED', 'GHGGC_ECD/FID']
        ; ix              0             1             2             3           4         5         6
        ; instr type      1             1             2             3           4         5         6
        ;     0:not defined, 1:QPMS or SFMS, 2:ALMSCO_TOFMS, 3:TW_TOFMS, 4:GhostECD, 5:AED, 6:GHGGC_FID or _ECD

        IF SIZE(dp_chrom, /TYPE) EQ 11 THEN BEGIN
          instr_type = ((dp_chrom[0]).instr_type)[0]

          CASE 1 OF
            (instr_type LE 1): $
              BEGIN
                msg=DIALOG_MESSAGE('Instrument type ambiguous, please check selection.', /INFORMATION)
                WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=0
              END
            ELSE: WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=instr_type
          ENDCASE

            instr=instr_val[WIDGET_INFO(ID_instr, /DROPLIST_SELECT)]
        ENDIF ; end if 'dp_chrom is valid structure'
      END ; end case 'restore_chrom'


    'load_expinfo' : $
      BEGIN
        expinflist = []
        IF SIZE(dp_expcfg, /TYPE) EQ 11 THEN $
          FOR i=0, N_ELEMENTS(dp_chrom)-1 DO expinflist=[expinflist, ((dp_expcfg[i]).expinfo.expinfo_fname)[0]]

        WIDGET_CONTROL, ID_expinfo_dl, set_VALUE=expinflist

        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_vals
        WIDGET_CONTROL, ID_etype, GET_VALUE=etype_vals
        WIDGET_CONTROL, ID_espec, GET_VALUE=espec_vals
        instr=instr_vals[WIDGET_INFO(ID_instr, /DROPLIST_SELECT)]
        etype=etype_vals[WIDGET_INFO(ID_etype, /DROPLIST_SELECT)]
        espec=espec_vals[WIDGET_INFO(ID_espec, /DROPLIST_SELECT)]
        FOR n=0, N_ELEMENTS(dp_expcfg)-1 DO BEGIN
          ITS = {instrument: instr, type: etype, spec: espec}
          tmp_strct = (dp_expcfg)[n]  ; move structure out of list
          tmp_strct.setup = ITS
          (dp_expcfg)[n] = TEMPORARY(tmp_strct)  ; put strct with loaded values back into list
        ENDFOR
      END

    'restore_dp' : $
      BEGIN
        chromlist = []
        expinflist = []
        FOR i=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN
          chromlist=[chromlist, ((dp_chrom[i]).exp_fname)[0]]
          expinflist=[expinflist, ((dp_expcfg[i]).expinfo.expinfo_fname)[0]]
          IF i EQ 0 THEN substlist=LIST(((dp_chrom[0]).subst.name)[*,0]) ELSE substlist.add, ((dp_chrom[i]).subst.name)[*,0]
        ENDFOR

        WIDGET_CONTROL, ID_chroms_dl, set_VALUE=chromlist

        WIDGET_CONTROL, ID_expinfo_dl, set_VALUE=expinflist

        setup = ((dp_expcfg[0]).setup)

        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_vals
        WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=(WHERE(instr_vals EQ setup.instrument))[0]

        WIDGET_CONTROL, ID_etype, GET_VALUE=etype_vals
        WIDGET_CONTROL, ID_etype, SET_DROPLIST_SELECT=(WHERE(etype_vals EQ setup.type))[0]

        WIDGET_CONTROL, ID_espec, GET_VALUE=espec_vals
        WIDGET_CONTROL, ID_espec, SET_DROPLIST_SELECT=(WHERE(espec_vals EQ setup.spec))[0]

      END

    'chroms_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_expinfo_dl, SET_DROPLIST_SELECT=sel_exp ; adjust selection of expinfo droplist

        setup = ((dp_expcfg[sel_exp]).setup)

        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_vals
        WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=(WHERE(instr_vals EQ setup.instrument))[0]

        WIDGET_CONTROL, ID_etype, GET_VALUE=etype_vals
        WIDGET_CONTROL, ID_etype, SET_DROPLIST_SELECT=(WHERE(etype_vals EQ setup.type))[0]

        WIDGET_CONTROL, ID_espec, GET_VALUE=espec_vals
        WIDGET_CONTROL, ID_espec, SET_DROPLIST_SELECT=(WHERE(espec_vals EQ setup.spec))[0]
      END

    'expinfo_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_chroms_dl, SET_DROPLIST_SELECT=sel_expinfo ; adjust selection of chroms droplist

        setup = ((dp_expcfg[sel_expinfo]).setup)

        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_vals
        WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=(WHERE(instr_vals EQ setup.instrument))[0]

        WIDGET_CONTROL, ID_etype, GET_VALUE=etype_vals
        WIDGET_CONTROL, ID_etype, SET_DROPLIST_SELECT=(WHERE(etype_vals EQ setup.type))[0]

        WIDGET_CONTROL, ID_espec, GET_VALUE=espec_vals
        WIDGET_CONTROL, ID_espec, SET_DROPLIST_SELECT=(WHERE(espec_vals EQ setup.spec))[0]
      END

    'instr_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_instr, GET_VALUE=instr_val
        instr=instr_val[WIDGET_INFO(ID_instr, /DROPLIST_SELECT)]
        IF SIZE(dp_expcfg, /TYPE) EQ 11 THEN BEGIN ; dp_expcfg was created, write new selection to dp_expcfg
          tmp_strct = (dp_expcfg)[sel_exp]
          tmp_strct.setup.instrument = instr
          (dp_expcfg)[sel_exp] = tmp_strct
        ENDIF ELSE BEGIN
;          msg=DIALOG_MESSAGE('Please load Experiment Info File(s) first.', /ERROR)
;          WIDGET_CONTROL, ID_instr, SET_DROPLIST_SELECT=0
        ENDELSE
      END

    'exptype_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_etype, GET_VALUE=type_val
        type=type_val[WIDGET_INFO(ID_etype, /DROPLIST_SELECT)]
        IF SIZE(dp_expcfg, /TYPE) EQ 11 THEN BEGIN
          tmp_strct = (dp_expcfg)[sel_exp]
          tmp_strct.setup.type = type
          (dp_expcfg)[sel_exp] = tmp_strct
        ENDIF ELSE BEGIN
;          msg=DIALOG_MESSAGE('Please load Experiment Info File(s) first.', /ERROR)
;          WIDGET_CONTROL, ID_etype, SET_DROPLIST_SELECT=0
        ENDELSE
      END

    'expspec_dl' : $
      BEGIN
        WIDGET_CONTROL, ID_espec, GET_VALUE=spec_val
        spec=spec_val[WIDGET_INFO(ID_espec, /DROPLIST_SELECT)]
        IF SIZE(dp_expcfg, /TYPE) EQ 11 THEN BEGIN
          tmp_strct = (dp_expcfg)[sel_exp]
          tmp_strct.setup.spec = spec
          (dp_expcfg)[sel_exp] = tmp_strct
        ENDIF ELSE BEGIN
;          msg=DIALOG_MESSAGE('Please load Experiment Info File(s) first.', /ERROR)
;          WIDGET_CONTROL, ID_espec, SET_DROPLIST_SELECT=0
        ENDELSE
      END

    ELSE:

  ENDCASE

END
