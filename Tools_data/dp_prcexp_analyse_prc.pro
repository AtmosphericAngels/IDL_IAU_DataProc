;+
; CREATED
; 16-10, F.Obersteiner
; revised 17-06, F.Obersteiner, florian.obersteiner@kit.edu
;
; PURPOSE
; analyses instrument precision in a precision experiment
;
; OUTPUT: structure with results "prc_res" in the form of
; ;  prc_res:
;    ./name     (substance name)
;    ./caltreat     (treatment method: calibration)
;    ./samtreat     (treatment method: samples)
;    ./calip     (calibration interpolation method)
;    ./calblocksz     (size of calibration blocks, i.e. n measurements)
;    ./blszs      (different sample block sizes processed)
;    ./iter_per_block     (n iterations per block size processed)
;    ./samblock:
;        ./sz_n     (block size no. that is processed)
;        ./iter:
;            ./n      (iteration no.)
;            ./mean_block_rsd     (intra pair precision)
;            ./all_sam_sd     (inter pair precision)
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_prcexp_analyse_prc, sel_exp, min_blsz, max_blsz, cal_blsz, $
                                VERBOSE=verbose

  COMMON DP_DATA
  COMMON DP_WIDID

  IF NOT KEYWORD_SET(verbose) THEN verbose = 0

  strct = !NULL
  mpexp_cfg = dp_prcexp_analyse_seq(sel_exp, dp_expcfg, sid_name, VERBOSE=verbose)
  seq = mpexp_cfg.seq.id
  cal_ix = mpexp_cfg.cal_ix
  n_cals = mpexp_cfg.n_dp
  n_blszs = max_blsz-min_blsz+1 ; number of different blocksizes to process
  n_iter = LINDGEN(n_blszs)+min_blsz+cal_blsz ; number of iterations per processed blocksize
  substs = ((dp_chrom[sel_exp])[0].subst.name)
  n_substs = N_ELEMENTS(substs)

; generate results structure: build from inside...
; calling indices e.g. strct[subst_no].samblock[block_no].iter[iteration_no].all_sam_sd
  refstrct_iter = {n: 0L, $
                   mean_block_rrsp: !VALUES.D_NAN, $
                   mean_block_rsd: !VALUES.D_NAN, $
                   all_sam_rsd: !VALUES.D_NAN}

  refstrct_blocksz = {sz_n: 0L, $
                      iter: REPLICATE(refstrct_iter, MAX(n_iter))} ; !blocks have different n_iterations

  refstrct_subst = {name: '', $
                    caltreat: '', $
                    samtreat: '', $
                    calip: '', $
                    calblocksz: '', $
                    blszs: n_iter-cal_blsz, $
                    iter_per_block: n_iter, $
                    samblock: REPLICATE(refstrct_blocksz, n_blszs)}

  strct = REPLICATE(refstrct_subst, n_substs)


  IF verbose THEN BEGIN
    print, '+++'
    print, 'calling precision analyser...'
    print, 'n block sizes: ', n_blszs
    print, 'iterations per block size: ', n_iter
  ENDIF

  IF mpexp_cfg.abort EQ 1 THEN RETURN, strct

  ID_caltreat   = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='caltreat_dl')
  sel_caltreat  = WIDGET_INFO(ID_caltreat, /DROPLIST_SELECT)
  ID_samtreat   = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='samtreat_dl')
  sel_samtreat  = WIDGET_INFO(ID_samtreat, /DROPLIST_SELECT)
  ID_calip      = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='calip_dl')
  sel_calip     = WIDGET_INFO(ID_calip, /DROPLIST_SELECT)
  ID_eval_mode  = WIDGET_INFO(dp_widid.dp_dataproc, FIND_BY_UNAME='eval_mode')
  eval_mode     = WIDGET_INFO(ID_eval_mode, /DROPLIST_SELECT)

  org_data = (dp_chrom[sel_exp]) ; create variables: original values
  org_cfg = (dp_expcfg[sel_exp])
  tmp_data = (dp_chrom[sel_exp]) ; create variables: temporary values
  tmp_cfg = (dp_expcfg[sel_exp])

  IF verbose THEN print, '+++ called prc calculator +++'

  FOR i=0, n_blszs-1 DO BEGIN ; process different blocksizes...

    IF verbose THEN print, 'calculating prc, blocksize: ', min_blsz+i

    calsam_seq = [MAKE_ARRAY(cal_blsz, /LONG, VALUE=3), MAKE_ARRAY(min_blsz+i, /LONG, VALUE=1)]
    base_seq = []
    FOR j=0, n_cals DO base_seq=[base_seq, calsam_seq] ; create some surplus data...

    FOR k=0, n_substs-1 DO strct[k].samblock[i].sz_n = i

    FOR j=0, n_iter[i]-1 DO BEGIN ; for each blocksize, iterate block positions

      IF verbose THEN print, 'block iteration: ', j+1

      seq[cal_ix] = base_seq[j:N_ELEMENTS(cal_ix)+(j-1)] ; determine a hypothetical 'cal/sam' vector

      ; feed to seq analyser and dp_expcfg.expinfo.s_id
      dp_analyse_seq, ID_VECTOR=seq
      ; call rR calc... if a treatcfg was loaded, these values are used ('current' keyword not set)
      dp_call_relresp_calc, sel_caltreat, sel_samtreat, sel_calip, EVAL_MODE=eval_mode, /OVERWRITE

      FOR k=0, n_substs-1 DO BEGIN ; loop: results per substance
        strct[k].samblock[i].iter[j].n = j
        CASE eval_mode OF
          0: $
            BEGIN
              strct[k].samblock[i].iter[j].mean_block_rrsp = $
                mean((dp_chrom[sel_exp]).subst[k].rres.rsp_area.sam_rrsp, /NAN, /DOUBLE)
              strct[k].samblock[i].iter[j].mean_block_rsd = $
                mean((dp_chrom[sel_exp]).subst[k].rres.rsp_area.block_rsd, /NAN, /DOUBLE)
              strct[k].samblock[i].iter[j].all_sam_rsd = $
                stddev((dp_chrom[sel_exp]).subst[k].rres.rsp_area.sam_rrsp, /NAN, /DOUBLE) / $
                  strct[k].samblock[i].iter[j].mean_block_rrsp
            END
          1: $
            BEGIN
              strct[k].samblock[i].iter[j].mean_block_rrsp = $
                mean((dp_chrom[sel_exp]).subst[k].rres.rsp_height.sam_rrsp, /NAN, /DOUBLE)
              strct[k].samblock[i].iter[j].mean_block_rsd = $
                mean((dp_chrom[sel_exp]).subst[k].rres.rsp_height.block_rsd, /NAN, /DOUBLE)
              strct[k].samblock[i].iter[j].all_sam_rsd = $
                stddev((dp_chrom[sel_exp]).subst[k].rres.rsp_height.sam_rrsp, /NAN, /DOUBLE) / $
                  strct[k].samblock[i].iter[j].mean_block_rrsp
            END
        ENDCASE
      ENDFOR; end: results per substance
    ENDFOR ; end: iterations
  ENDFOR; end: blocksizes

  ; fill in substance information....
  FOR k=0, n_substs-1 DO BEGIN
    strct[k].name = ((dp_chrom[sel_exp]).subst[k].name)[0]
    strct[k].caltreat = ((dp_chrom[sel_exp]).subst[k].rres.cal_treat)[0]
    strct[k].samtreat = ((dp_chrom[sel_exp]).subst[k].rres.sam_treat)[0]
    strct[k].calip = ((dp_chrom[sel_exp]).subst[k].rres.cal_ip_mthd)[0]
    strct[k].calblocksz = cal_blsz
  ENDFOR

  (dp_chrom[sel_exp]) = org_data ; restore original values
  (dp_expcfg[sel_exp]) = org_cfg

  IF verbose THEN print, '+++ prc calculation finished +++'

  RETURN, strct

END