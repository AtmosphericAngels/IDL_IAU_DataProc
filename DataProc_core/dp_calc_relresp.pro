;+
; FUNCTION dp_calc_relresp
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: core function; calculates relative responsed for samples based on responses of the interpolated calibration
;          and the sample treatment method.
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_calc_relresp, xdata, ydata, vd_sam, cal_interpol, sel_samtreat, sequence

  COMMON dp_data

  sam_treat = sam_treat_mthd[sel_samtreat]

  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1

  rR_all = ydata/cal_interpol
  samples_rR = DBLARR(N_ELEMENTS(rR_all))*!VALUES.D_NAN

  blockmean_rR = DBLARR(N_ELEMENTS(rR_all))*!VALUES.D_NAN
  block_RSD = DBLARR(N_ELEMENTS(rR_all))*!VALUES.D_NAN

  ix_init = (sequence.ix_init_samblock)[WHERE(sequence.ix_init_samblock NE -1)]
  ix_end = (sequence.ix_end_samblock)[WHERE(sequence.ix_end_samblock NE -1)]

  ;cleanup for mess created by PRC analyser
  FOR i=0, N_ELEMENTS(ix_init)-2 DO BEGIN
    IF ix_init[i+1] LT ix_init[i] THEN BEGIN
      ix_init = ix_init[0:i]
      BREAK
    ENDIF
  ENDFOR
  FOR i=0, N_ELEMENTS(ix_end)-2 DO BEGIN
    IF ix_end[i+1] LT ix_end[i] THEN BEGIN
      ix_end = ix_end[0:i]
      BREAK
    ENDIF
  ENDFOR
  ; discard samples not bracketed by cals for PRC analyser
  ix_cal = sequence.ix_cal[WHERE(sequence.ix_cal NE -1)]
  ix_init = ix_init[WHERE(ix_init GT ix_cal[0] AND ix_init LT ix_cal[-1])]
  ix_end = ix_end[WHERE(ix_end GT ix_cal[0] AND ix_end LT ix_cal[-1])]
  ; more cleanup...
  IF N_ELEMENTS(ix_init) NE N_ELEMENTS(ix_end) THEN BEGIN
      IF ix_init[0] LE ix_end[0] THEN ix_init = ix_init[0:N_ELEMENTS(ix_end)-1]
  ENDIF
  ; PRC analyser stuff done

  vd_sam = vd_sam[WHERE(vd_sam GE ix_init[0] AND vd_sam LE ix_end[-1])]
  samples_rR[vd_sam] = ydata[vd_sam]/cal_interpol[vd_sam]

  n_perblock = ix_end-ix_init+1
  n_blocks = N_ELEMENTS(n_perblock)


  CASE sam_treat OF ; determine sample measurements to use based on selected sample treatment method

    'block_last_1':$
      BEGIN
        blockmean_rR[ix_end]=samples_rR[ix_end]
      END

    'block_last_2':$
      BEGIN
        FOR i=0, n_blocks-1 DO BEGIN
          s_ix=ix_end[i]-1 ; block of 2: start
          e_ix=ix_end[i]   ; block of 2: end
          blockmean_rR[e_ix] = $
            mean(samples_rR[s_ix:e_ix], /DOUBLE, /NAN)
          block_RSD[e_ix] = $
            stddev(samples_rR[s_ix:e_ix], /DOUBLE, /NAN) / blockmean_rR[e_ix]
        ENDFOR
      END

    'block_last_3':$
      BEGIN
        FOR i=0, n_blocks-1 DO BEGIN
          s_ix=ix_end[i]-2 ; block of 3: start
          e_ix=ix_end[i]   ; block of 3: end
          blockmean_rR[e_ix] = $
            mean(samples_rR[s_ix:e_ix], /DOUBLE, /NAN)
          block_RSD[e_ix] = $
            stddev(samples_rR[s_ix:e_ix], /DOUBLE, /NAN) / blockmean_rR[e_ix]
        ENDFOR
      END

    'block_last_4':$
      BEGIN
        FOR i=0, n_blocks-1 DO BEGIN
          s_ix=ix_end[i]-4 ; block of 4: start
          e_ix=ix_end[i]   ; block of 4: end
          blockmean_rR[e_ix] = $
            mean(samples_rR[s_ix:e_ix], /DOUBLE, /NAN)
          block_RSD[e_ix] = $
            stddev(samples_rR[s_ix:e_ix], /DOUBLE, /NAN) / blockmean_rR[e_ix]
        ENDFOR
      END

    ELSE: $ ; default: block mean (also applies if 'individual' is selected)
      BEGIN
        FOR i=0, n_blocks-1 DO BEGIN
          s_ix=ix_init[i] ; block start
          e_ix=ix_end[i]  ; block end
          blockmean_rR[e_ix] = $
            mean(samples_rR[s_ix:e_ix], /DOUBLE, /NAN)
          block_RSD[e_ix] = $
            stddev(samples_rR[s_ix:e_ix], /DOUBLE, /NAN) / blockmean_rR[e_ix]
        ENDFOR
      END
  ENDCASE


  strct={ $
          sam_rrsp: samples_rR, $
          block_rrsp: blockmean_rR ,$
          block_rsd: block_RSD $
        }

  RETURN, strct

END