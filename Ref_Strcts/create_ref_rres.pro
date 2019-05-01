;#####################################################################################################################

FUNCTION create_ref_rsp

  rsp = {$

         normalised:       !VALUES.D_NAN,$   ; response; a/v or h/v normalised to values of calibration measurements
         norm_corrected:   !VALUES.D_NAN,$   ; holds corrected n_(a/v) or n_(h/v) values (e.g. carry-over corrected)

         cal_rsp:          !VALUES.D_NAN,$   ; response des cal; gemessen oder interpoliert
         cal_block_rsd:    !VALUES.D_NAN,$   ; RSD einens blocks aus cal-messungen
         cal_mintomax:     !VALUES.D_NAN,$   ; min-to-max deviation of cals in the series
         cal_devtofit:     !VALUES.D_NAN,$   ; deviation of cals to a 2nd dgr polyfit

         sam_rrsp:         !VALUES.D_NAN,$   ; relative response der samples
         block_rrsp:       !VALUES.D_NAN,$   ; mittelwert aus x proben eines probenblocks
         block_rsd:        !VALUES.D_NAN,$   ; RSD eines blocks von proben-messungen (intra-pair variability)
         sys_prc:          !VALUES.D_NAN,$   ; System - Präzision, welche aus "Repro"- bzw. Präzisions-Experiment
                                          ; bestimmt werden muss. -> externe Daten
         prc_flag:         0 $               ; Precision - Flag: 0: nicht gesetzt, 1: OK, -1: bad, -2: discard
         }

  RETURN, rsp

END

;#####################################################################################################################

FUNCTION create_ref_rres

  rsp = create_ref_rsp()

  ref_rres = { $

              rsp_area:     rsp, $             ; Response, etc. für area oder height berechnen
              rsp_height:   rsp, $             ; Response, etc. für area oder height berechnen

              rsp_select:   0, $               ; eval mode 0: based on signal area / 1: based on signal height

              active_corr:  BOOLARR(10), $     ; array that specifies applied corrections.
                                              ; active_corr[0]: carry-over correction

              s_n:          !VALUES.D_NAN, $   ; signal to noise

              ha_rat:       !VALUES.D_NAN, $   ; h/a ratio

              cal_ip_mthd:  '', $              ; verwendete Interpolationsmethode: 0=p2p, 1=calsmean, 2=linear_fit, 3=polyfit_dg2, 4=runnin_mean
              cal_treat:    '', $              ; Standardbehandlung ("block mean" etc.)
              sam_treat:    '', $              ; Probenbehandlung ("block mean", "nur letzten n Proben eines Blocks" etc.)

              use_flag:     1, $               ; substance-specific / 0: do not use this datapoint, 1: use this datapoint (default)
              data_select:  '', $              ; indicates if automatic or user-specific data point selection was performed

              dp_timestamp: !VALUES.D_NAN $    ; Datum, wann Datenprozessierung durchgeführt wurde

              }

  RETURN, ref_rres

END

;#####################################################################################################################