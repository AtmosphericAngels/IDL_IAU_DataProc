;------------------------------------------------------------------------------------------------------------------------
;+
; CREATED
; (16-10, idea but no code...)
; 17-06, F.Obersteiner, florian.obersteiner@kit.edu - actually wrote the code.
; 
; PURPOSE
; export precision experiment results to txt files (one per species).
; 
;-
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------
PRO dp_prcexp_res2txt, prc_res, sel_exp, DIR=dir

  COMMON DP_DATA
  
  ; one txt file per species: get n_subst
  n_subst = N_ELEMENTS(prc_res)
  
  ; get block and iteration loop counters
  blszs = prc_res[0].blszs
  n_iter = prc_res[0].iter_per_block
  
  ; prompt directory if not set
  IF NOT KEYWORD_SET(dir) THEN $
    dir = DIALOG_PICKFILE(TITLE='Please select a directory to store results.', /DIRECTORY)
    
  IF STRLEN(dir) EQ 0 THEN RETURN
  
  id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] + 1 ; configure IDs
  id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] + 1
  id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] + 1

  cal = ((dp_expcfg[sel_exp]).expinfo.s_name)[(WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_cal))[0]]
  
  ; define some file specs
  sep = STRING(9B)
  eval_mode = ((dp_chrom[0]).subst[0].rres.rsp_select)[0]
  mode_string = (['signal_area', 'signal_height'])[eval_mode]
  prefix = '\iaudp_prc_'
  suffix = '.txt'
  colheader1 = ['Iteration_01']
  colheader2 = ['Blcks_mean_rR', sep, 'Blcks_mean_rSD', sep, 'All_Sam_rSD', sep]
  tmp = colheader2
  FOR i=1, MAX(n_iter)-1 DO BEGIN
    colheader1 = [colheader1, sep,sep,sep, 'Iteration_'+STRING((i+1), FORMAT='(I02)')]
    colheader2 = [colheader2, tmp]
  ENDFOR 
  colheader2 = colheader2[0:-2]
  colheader1 = ['Sam_Blcksz', sep, colheader1, sep]
  colheader2 = [sep, colheader2] 
  colhdr_format = '(' 
  FOR i=0, N_ELEMENTS(colheader2) DO colhdr_format = colhdr_format+'A,'
  STRPUT, colhdr_format, ')', N_ELEMENTS(colheader2)*2+2
   
  FOR i=0, n_subst-1 DO BEGIN ; begin loop: substances
    
    fname = dir + prefix + prc_res[i].name + suffix
    mass = STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[i].mass[(dp_chrom[sel_exp]).subst[i].quant])[0], $
                                FORMAT='(D25.3)'), /REMOVE_ALL)
    
    
    OPENW, lun, fname, /GET_LUN
    
      ; header
      PRINTF, lun, '*** IAU_DP_v'+dp_vers+' PRC REPORT ***', FORMAT='(A)'
      PRINTF, lun, 'Experiment:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), FORMAT='(A,A,A)'
      PRINTF, lun, 'Processing_Timestamp:', sep, jultime2timestring(SYSTIME(/JULIAN)), FORMAT='(A,A,A)'
      PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
      PRINTF, lun, 'Cal_Gas:', sep, cal, sep, 'Cal_Blcksz:', sep, $
                   STRCOMPRESS(STRING(prc_res[i].calblocksz, FORMAT='(I)')), FORMAT='(A,A,A,A,A,A,A)'
      PRINTF, lun, 'Substance:', sep, prc_res[i].name, sep, 'm/Q:', sep, mass, sep, 'Eval_Mode:', sep, mode_string, $
                    FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
      PRINTF, lun, 'Cal_Interpol:', sep, prc_res[i].calip, sep, 'Cal_Treat:', sep, prc_res[i].caltreat, sep, $
                   'Sample_Treat:', sep, prc_res[i].samtreat, FORMAT='(A,A,A,A,A,A,A,A,A,A,A)'
      PRINTF, lun, 'Separator:', sep, 'TAB', FORMAT='(A,A,A)'
      PRINTF, lun, '*** End of Header ***', FORMAT='(A)'
      
      ; col header 1 & 2
      PRINTF, lun, colheader1, FORMAT=colhdr_format
      PRINTF, lun, colheader2, FORMAT=colhdr_format
      
      ; collect data
      mean_rrsp = prc_res[i].samblock.iter.mean_block_rrsp
      intra_prc = prc_res[i].samblock.iter.mean_block_rsd
      inter_prc = prc_res[i].samblock.iter.all_sam_rsd
      
      res_arr = DBLARR(MAX(n_iter)*3+1, N_ELEMENTS(blszs))
      res_arr[0,*] = blszs
      
      k=1
      FOR j=0, MAX(n_iter)-1 DO BEGIN
        res_arr[k,*] = mean_rrsp[j, *]
        res_arr[k+1,*] = intra_prc[j,*]
        res_arr[k+2,*] = inter_prc[j, *]
        k=k+3
      ENDFOR
       
      ; write data
      FOR l=0,N_ELEMENTS(blszs)-1 DO BEGIN
        str=[]
        FOR m=0, MAX(n_iter)*3 DO BEGIN
          v_str = (STRCOMPRESS(STRING(res_arr[m,l], FORMAT='(D25.7)'), /REMoVE_ALL)).replace('-NaN', 'NaN')
          str = [str, v_str, sep]
        ENDFOR
        str = str[0:-2]
        PRINTF, lun, str, FORMAT=colhdr_format
        
        means = FLTARR(3)
        means[0] = mean(mean_rrsp[*,l], /NAN)
        means[1] = mean(intra_prc[*,l], /NAN)
        means[2] = mean(inter_prc[*,l], /NAN)
        str = ['mean values over all iterations:',sep,'Blcks_mean_rR',sep,'Blcks_mean_rSD',sep,'All_Sam_rSD']
        PRINTF, lun, str, FORMAT='(A,A,A,A,A,A,A)'
        
        str = [sep]
        FOR m=0, 2 DO BEGIN  
          mv_str = (STRCOMPRESS(STRING(means[m], FORMAT='(D25.7)'), /REMoVE_ALL)).replace('-NaN', 'NaN')
;          mv_str = mv_str.replace('-NaN', 'NaN')
          str = [str, mv_str, sep]
        ENDFOR
        str = [str[0:-2], STRING(13B), STRING(10B)]
        PRINTF, lun, str, FORMAT='(A,A,A,A,A,A,A,A)'
      ENDFOR

    CLOSE, lun
    FREE_LUN, lun
    
  ENDFOR ; end loop: substances  

END
