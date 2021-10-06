FUNCTION  create_ref_expinfo

  ref_expinfo = $
    { $
    expinfo_fname:  "" ,$               ; Dateinamen des Expinfo-Files
    opr:            "" ,$               ; Operator-Name
    com:            STRARR(5),$         ; maximal 5 verschiedene Kommentare dürfen abgelegt werden
    rv_vol:         -1.,$               ; Volumen des Referenzbehälters (0.5, 1, 2, .... 10l)

    fname:          "",$                ; Filename des Chromatogramms
    nr:             -1 ,$               ; Fortlaufende (?) Nummer des chrominfo -Files

    s_name:         "" ,$               ; Name/Behälternummer der Probe, Name des Standards
    s_date:         "" ,$               ; Zeitstempel der Probennahme (UTC), STRING: 'dd.mm.yyyy hh:mn:ss'
    s_lat:          !VALUES.D_NAN,$     ; Probennahme Ort Latitude
    s_lon:          !VALUES.D_NAN,$     ; Probennahme Ort Longitude
    s_alt:          !VALUES.D_NAN,$     ; Probennahme Ort m asl

    s_id:           -1 ,$               ; 1=Probe (Air), 2=Target (Target), 3=Standard (Calibration), 4=Heliumblank (blank gas), 5=Vakuumblank (blank vacuum)
    s_vol:          !VALUES.D_NAN,$     ; Probe- bzw. Anreicherungsvolumen (see s_vol_select)
    s_vol_select:   -1, $               ; sample volume determination; 0: pressure difference, 1: mass flow controller

    rv_ps:          !VALUES.D_NAN,$     ; Anfangsdruck des Referenzvolumens in bar
    rv_pe:          !VALUES.D_NAN,$     ; Enddruck des Referenzvolumens in bar
    rv_dp:          !VALUES.D_NAN,$     ; Anreicherungsdruck = p_e-p_s

    mfc_flow:       !VALUES.D_NAN,$     ; Flowrate des MFC in [mL/min]
    mfc_vol:        !VALUES.D_NAN,$     ; Integral MFC-Signal [mL]

    Ts_cldhd:       !VALUES.F_NAN,$     ; Temperatur des Coldhead beim anreichern (Anfang)
    Te_cldhd:       !VALUES.F_NAN,$     ; Temperatur des Coldhead beim anreichern (Ende)

    ts:             !VALUES.D_NAN,$     ; Beginn der Anreicherung (Zeit)
    te:             !VALUES.D_NAN,$     ; Ende der Anreicherung (Zeit)
    dt:             !VALUES.D_NAN,$     ; Dauer der Anreicherung
    use:            1 $                 ; Flag, ob das Chromatogram verwendet werden soll (default=ja; 1)
    }

  RETURN, ref_expinfo

END