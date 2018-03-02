;+
; PRO: dp_analyse_seq
;
; AUTHOR: F. Obersteiner, Sep-2016
;
; PURPOSE: analyse the measurement sequence of loaded experiments to determine cal and sample treatment options.
;          fills dp_expcfg.sequence structure with values that are useful in further data processing.
; 
; NOTE:
; cal_treat 'bracketing': interpolate cal between cals that bracket samples
; cal_treat 'block_mean': interpolate between block means; if first and last cal blocks are longer than
;                         the typical (median) blocklength of the sequence, only the typical number is used.
; cal_treat 'preceding':  only cals that precede a sample measurement are interpolated; in the last
;                         block, the last cal measurement is used
;
; sam_treat 'block_mean':   self-explanatory...
; sam_treat 'block_last_1': only the last measurement of each sample block is evaluated. requires n_samples/block GE 2.
; sam_treat 'block_last_2': only the last measurement of each sample block is evaluated. requires n_samples/block GE 3.
; sam_treat 'block_last_3': only the last measurement of each sample block is evaluated. requires n_samples/block GE 4.
; sam_treat 'block_last_4': only the last measurement of each sample block is evaluated. requires n_samples/block GE 5.
; sam_treat 'individual':   the only option for in situ / continuous measurement series
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_analyse_seq, ID_VECTOR=id_vector, QUIET=quiet, VERBOSE=verbose 

  COMMON DP_DATA
  
  IF NOT KEYWORD_SET(verbose) THEN verbose=0
  IF NOT KEYWORD_SET(id_vector) THEN internal_id_flag=1 ELSE internal_id_flag=0
   
  FOR n=0, N_ELEMENTS(dp_chrom)-1 DO BEGIN ; BEGIN loop over all loaded experiments
    tmp_strct = (dp_expcfg)[n]    
    
    IF internal_id_flag THEN id_vector = tmp_strct.expinfo.s_id $
      ELSE tmp_strct.expinfo.s_id = id_vector
       
    id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
    id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1
    id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1
    
    is_cal = LONARR(N_ELEMENTS(id_vector)) ; vector specifying calibration points; 1=cal, 0=other
    is_cal[WHERE(id_vector EQ id_cal)]=1
    is_sam = LONARR(N_ELEMENTS(id_vector)) ; vector specifying calibration points; 1=cal, 0=other
    is_sam[WHERE(id_vector EQ id_sam OR id_vector EQ id_tgt)]=1
    
    ix_init_cal=(WHERE(is_cal EQ 1))[0]
    ix_term_cal=(WHERE(is_cal EQ 1))[-1]
    ix_init_sam=(WHERE(is_sam EQ 1))[0]
    ix_term_sam=(WHERE(is_sam EQ 1))[-1]
    
    IF ix_init_cal GT ix_init_sam THEN miss_1st_cal=1 ELSE miss_1st_cal=0 ; missing first cal?
    IF ix_term_cal LT ix_term_sam THEN miss_last_cal=2 ELSE miss_last_cal=0 ; missing last cal?
    
    n_cbl = N_ELEMENTS(is_cal[WHERE((is_cal[1:-1]-is_cal[0:-2]) GT 0)])
    n_sbl = N_ELEMENTS(is_sam[WHERE((is_sam[1:-1]-is_sam[0:-2]) GT 0)])
    
    CASE (miss_1st_cal-miss_last_cal) OF ; correct for missing blocks
       0:  BEGIN ; samples bracketed 
             IF is_cal[0] EQ 1 THEN n_cbl=n_cbl+1 ; correct index shifting method error (cals)
           END
       1:  BEGIN ; fist cal missing
             IF is_sam[0] EQ 1 THEN n_sbl=n_sbl+1 ; correct index shifting method error (samples)
           END
      -2:  BEGIN ; last cal missing
             IF is_cal[0] EQ 1 THEN n_cbl=n_cbl+1 ; correct index shifting method error (cals)
             miss_last_cal=miss_last_cal-1
           END
      -1:  BEGIN ; initiating and terminating cal missing
             IF is_cal[0] EQ 1 THEN n_cbl=n_cbl+1 ; correct index shifting method error (cals)
             miss_last_cal=miss_last_cal-1
           END
    ENDCASE

    ix_vector=LINDGEN(N_ELEMENTS(id_vector))
    ix_cal=ix_vector[WHERE(is_cal EQ 1)]
    ix_sam=ix_vector[WHERE(is_sam EQ 1)]

    ix_cbl_start=[]
    ix_cbl_end=[]
    FOR i=0, N_ELEMENTS(is_cal)-2 DO BEGIN
      IF i EQ 0 AND is_cal[i] EQ 1 THEN ix_cbl_start=[ix_cbl_start,ix_vector[i]]
      IF is_cal[i] EQ 0 AND is_cal[i+1] EQ 1 THEN ix_cbl_start=[ix_cbl_start,ix_vector[i+1]]
      IF is_cal[i] EQ 1 AND is_cal[i+1] EQ 0 THEN ix_cbl_end=[ix_cbl_end,ix_vector[i]]
      IF i EQ N_ELEMENTS(is_cal)-2 AND is_cal[i+1] EQ 1 THEN ix_cbl_end=[ix_cbl_end,ix_vector[i+1]]
    ENDFOR
    
    ix_sbl_start=[]
    ix_sbl_end=[]
    FOR i=0, N_ELEMENTS(is_sam)-2 DO BEGIN
      IF i EQ 0 AND is_sam[i] EQ 1 THEN ix_sbl_start=[ix_sbl_start,ix_vector[i]]
      IF is_sam[i] EQ 0 AND is_sam[i+1] EQ 1 THEN ix_sbl_start=[ix_sbl_start,ix_vector[i+1]]
      IF is_sam[i] EQ 1 AND is_sam[i+1] EQ 0 THEN ix_sbl_end=[ix_sbl_end,ix_vector[i]]
      IF i EQ N_ELEMENTS(is_sam)-2 AND is_sam[i+1] EQ 1 THEN ix_sbl_end=[ix_sbl_end,ix_vector[i+1]]
    ENDFOR
    
                                     
    IF MAX(ix_sbl_end-ix_sbl_start+1)-5 GT -1 THEN ix_max=-1 ELSE ix_max=MAX(ix_sbl_end-ix_sbl_start+1)-6
    sam_treat = sam_treat_mthd[0:ix_max]; sam_treat_mthd is common variable, see def_common
       
    tmp_strct.sequence.ID = id_vector ; write information to dp_expcfg structure
    tmp_strct.sequence.n_cal_blocks      = n_cbl
    tmp_strct.sequence.n_sam_blocks      = n_sbl
    
    tmp_strct.sequence.ix_cal            = ix_cal
    tmp_strct.sequence.miss_1stcal       = miss_1st_cal
    tmp_strct.sequence.miss_lastcal      = miss_last_cal
    tmp_strct.sequence.ix_init_calblock  = ix_cbl_start
    tmp_strct.sequence.ix_end_calblock   = ix_cbl_end
    tmp_strct.sequence.cal_treat         = cal_treat_mthd ; commom variable, see def_common
    
    tmp_strct.sequence.ix_sam            = ix_sam
    tmp_strct.sequence.ix_init_samblock  = ix_sbl_start
    tmp_strct.sequence.ix_end_samblock   = ix_sbl_end
    tmp_strct.sequence.sam_treat         = sam_treat
    
    
    (dp_expcfg)[n] = tmp_strct ; put back into LIST
    
    IF NOT KEYWORD_SET(quiet) THEN BEGIN
      name_vector=tmp_strct.expinfo.s_name
      smpls=name_vector[WHERE(is_sam EQ 1)]
      count=1 ; there is at least one sample...
      FOR s=0, N_ELEMENTS(smpls)-2 DO IF smpls[s+1] NE smpls[s] THEN count=count+1
      IF count GT n_sbl THEN $
        msg=DIALOG_MESSAGE('Found multiple individual samples in one sample block. ' + $
                           'Select sample treatment "individual" to avoid faulty results.')
    ENDIF
    
    IF verbose THEN BEGIN
      print, '+++'
      print, 'sequence analyser output:'
      print, 'experiment: ', ((dp_chrom[n]).exp_fname)[0]
      IF miss_1st_cal EQ 1 THEN print, 'missing 1st cal!'
      IF miss_last_cal EQ 2 THEN print, 'missing last cal!'
      print, 'n cal blocks: ', n_cbl
      print, 'n cals/block: ' , ix_cbl_end-ix_cbl_start+1
      print, 'n sam blocks: ', n_sbl
      print, 'n smpls/block: ' , ix_sbl_end-ix_sbl_start+1
      print, 'index of calblocks start: ', ix_cbl_start
      print, 'index of calblocks end: ', ix_cbl_end
      print, 'index of samblocks start: ', ix_sbl_start
      print, 'index of samblocks end: ', ix_sbl_end
      print, '+++'
    ENDIF
    
  ENDFOR ; END loop over all loaded experiments
  
  dp_refr_status, MESSAGE='Sequence(s) analysed.'
  
END