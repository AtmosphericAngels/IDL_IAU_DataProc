;+
; AUTHOR: T. Schuck, June-2017
;
; PURPOSE: 
;   PRO dp_report_list export tab seperated text file with several substances
; 
; MODIFICATIONS:
;   FO, 17-06 - Integrated in dataproc v1.2. Unified with dp_read_exp_list.pro. Removed global variable "exp_list".
;-
;------------------------------------------------------------------------------------------------------------------------

PRO dp_report_list, sel_exp, PATH=path, VERBOSE=verbose

  COMMON dp_data

  IF NOT KEYWORD_SET(verbose) THEN verbose = 0
 
  file=DIALOG_PICKFILE(TITLE='Please select a substance list file.', PATH=path, FILTER='*.csv', /READ)
  IF file EQ '' THEN RETURN

  import_list=read_csv(file, N_TABLE_HEADER=1, TABLE_HEADER=TableHeader, COUNT=count)
  exp_list = STRARR(count)

  FOR i=0, count-1 DO BEGIN
    tmp=strsplit(STRTRIM(import_list.field1[i]), ';', /EXTRACT)
    exp_list[i] = tmp[0]
  ENDFOR


     
    IF verbose THEN print, 'writing:', exp_list

    fname = DIALOG_PICKFILE(PATH=path, /WRITE, FILE=instr+'_dp_list_mxr.txt')
    sep=STRING(9B)

    exp_header = STRARR(3*N_ELEMENTS(exp_list))

    FOR i=0, N_ELEMENTS(exp_list)-1 DO BEGIN
      exp_header[3*i] = exp_list[i]
      exp_header[3*i+1] = 'd_'+exp_list[i]
      exp_header[3*i+2] = 'flag_'+exp_list[i]
    ENDFOR

    colheader=make_array(n_elements(exp_header)+3, /STRING)

    colheader[0]='sample_date'
    colheader[1]='meas_date'
    colheader[2]='sample_name'

    FOR i=0, N_ELEMENTS(exp_header)-1 DO BEGIN
      colheader[i+3] = exp_header[i]
    ENDFOR


    exp_ts=jultime2timestring(((dp_chrom[sel_exp]).subst.rres.dp_timestamp)[0])
  
    id_cal = (WHERE(STRUPCASE(sid_name) EQ 'CALIBRATION'))[0] +1 ; configure IDs
    cal=((dp_expcfg[sel_exp]).expinfo.s_name)[(WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_cal))[0]]
    
    id_sam = (WHERE(STRUPCASE(sid_name) EQ 'AIR'))[0] +1 ; configure IDs
    id_tgt = (WHERE(STRUPCASE(sid_name) EQ 'TARGET'))[0] +1 ; configure IDs
    sam_tgt=((dp_expcfg[sel_exp]).expinfo.s_name)[WHERE((dp_expcfg[sel_exp]).expinfo.s_id EQ id_sam  OR (dp_expcfg[sel_exp]).expinfo.s_id EQ id_tgt)]
    
    samples = uniq(sam_tgt)
    nofsamples = n_elements(samples)
    
      
    m_date=jultime2timestring(((dp_chrom[sel_exp]).jdate)[0], /ONLYDATE)
   
    IF verbose THEN  print, (dp_expcfg[sel_exp]).cal_mrs.canister

 
    OPENW, lun, fname, /GET_LUN
  
    PRINTF, lun, '*** IAU_DP_v'+dp_vers+' REPORT ***', FORMAT='(A)'
    PRINTF, lun, 'Experiment:', sep, FILE_BASENAME((dp_chrom[sel_exp])[0].exp_fname[0]), FORMAT='(A,A,A)'
    PRINTF, lun, 'Processing_Timestamp:', sep, exp_ts, FORMAT='(A,A,A)'
    PRINTF, lun, 'Instrument:', sep, instr, FORMAT='(A,A,A)'
    PRINTF, lun, 'Cal_Gas:', sep, cal, FORMAT='(A,A,A)'
    PRINTF, lun, 'Precision Flagging: 0 = undefined, 1 = good, -1 = bad (more than ', prc_limits[0] , $
                 ' sigma), -2 = poor (more than ', prc_limits[1] ,' sigma)' , FORMAT='(A,F7.2,A,F7.2,A)'
    PRINTF, lun, 'Separator:', sep, 'TAB', FORMAT='(A,A,A)'
    PRINTF, lun, '*** End of first block ***', FORMAT='(A)'
  
    PRINTF, lun, 'substance',sep,'Instr_rel_MP',sep,'Sam_mean_rsd', FORMAT='(A,A,A,A,A)'

   
    FOR i=0, N_ELEMENTS(exp_list)-1 DO BEGIN
         name=exp_list[i] 
         
 ; !
 ; missing check if .instr_prc structure pointer is defined and valid ("filled").
 ; !
         w=WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).instr_prc.substance) EQ STRUPCASE(name))
         w2=WHERE(STRUPCASE(dp_chrom[sel_exp].subst.name) EQ STRUPCASE(name))
  
        IF w[0] NE -1 THEN BEGIN
          cal_prc = STRCOMPRESS(STRING((*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w[0]], FORMAT='(D25.6)'), /REMOVE_ALL)
        ENDIF ELSE  BEGIN
         cal_prc= 'NaN' 
        ENDELSE 
   
  ;;; does not check wether height or area was used !!!
        IF w2[0] NE -1 THEN BEGIN
          sam_blocks_rsd= STRCOMPRESS(STRING(mean(((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.block_rsd), $
          /NAN, /DOUBLE), FORMAT='(D25.6)'), /REMOVE_ALL)
        ENDIF ELSE  BEGIN
         sam_blocks_rsd= 'NaN' 
        ENDELSE 
    
       PRINTF, lun,  exp_list[i], sep, cal_prc, sep, sam_blocks_rsd, FORMAT='(A,A,A,A,A)'
  
  
    ENDFOR
  
    PRINTF, lun, '*** End of second block ***', FORMAT='(A)'
  
    PRINTF, lun, 'block mean mixing ratio', FORMAT='(A)'
    PRINTF, lun, 'block mean precision or instrumental precision whichever is worse', FORMAT='(A)'
    PRINTF, lun, 'block precision flag', FORMAT='(A)'
  
    PRINTF, lun, '*** End of Header ***', FORMAT='(A)'

    PRINTF, lun, colheader[0], FORMAT='(A,$)'
    FOR i=1, N_ELEMENTS(colheader)-1 DO BEGIN
      PRINTF, lun, sep, colheader[i], FORMAT='(A,A,$)'  ; no linebreak
    ENDFOR
    PRINTF, lun


    ;; i has to run over the number of different samples (incl. target)
    FOR i=0, nofsamples-1 DO BEGIN   
    
      sample_name = sam_tgt[samples[i]]
      printf, lun, 'YYYYDDMM', sep, m_date, FORMAT='(A,A,A,$)'
      printf, lun, sep, sample_name , FORMAT='(A,A,$)'
      
      w=WHERE((dp_expcfg[sel_exp]).expinfo.s_name EQ sample_name)
      w=w[(n_elements(w)-1)]

      FOR j=0, N_ELEMENTS(exp_list)-1 DO BEGIN;write  mixing ratio, rsd and prc flag 
        name=exp_list[j] 
        w2=WHERE(STRUPCASE(dp_chrom[sel_exp].subst.name) EQ STRUPCASE(name))
       
        w3=WHERE(*(dp_expcfg[sel_exp]).cal_mrs.substance EQ name)
        w4=WHERE(STRUPCASE(*(dp_expcfg[sel_exp]).instr_prc.substance) EQ STRUPCASE(name))
       
        IF w2[0] NE -1 THEN BEGIN
          rsd_num= ((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.block_rsd)[w]
          cal_prc_num = ((*(dp_expcfg[sel_exp]).instr_prc.mp_rel)[w4[0]])
            
          flag_num = ((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.prc_flag)[w]
                      
          IF w3[0] NE -1 THEN BEGIN
         
            mxr_num =   ((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.block_rrsp)[w] * (*(dp_expcfg[sel_exp]).cal_mrs.mr_ppt)[w3[0]]
                    
            IF(mxr_num LT 0) THEN BEGIN
              delta='NaN'
              mxr='NaN'
            ENDIF ELSE BEGIN
                    
              IF flag_num EQ 0 THEN  delta_num = mxr_num * cal_prc_num
              IF (flag_num NE 0) AND (rsd_num GE cal_prc_num) THEN  delta_num = mxr_num * rsd_num
              IF (flag_num NE 0) AND (rsd_num LT cal_prc_num)  THEN  delta_num = mxr_num * cal_prc_num
            
              mxr=  STRCOMPRESS(STRING(mxr_num, FORMAT='(D12.3)'), /REMOVE_ALL)
              delta = STRCOMPRESS(STRING(delta_num, FORMAT='(D12.3)'), /REMOVE_ALL)
                   
            ENDELSE
                             
          ENDIF ELSE $
            mxr = 'NaN'
             
          flag = STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.prc_flag)[w], $
                                      FORMAT='(I12)'), /REMOVE_ALL)
        
        
          rrsp = STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.block_rrsp)[w], $
                                      FORMAT='(D12.3)'), /REMOVE_ALL)
        
        
          rsd = STRCOMPRESS(STRING(((dp_chrom[sel_exp]).subst[w2[0]].rres.rsp_area.block_rsd)[w]*100., $
                                      FORMAT='(D12.3)'), /REMOVE_ALL)
        
        
        ENDIF ELSE  BEGIN
          rsd = 'NaN'
          mxr = 'NaN'
          rrsp = 'NaN'
          flag = 'NaN'
        ENDELSE
      
        PRINTF, lun, sep, mxr, sep, delta, sep , flag, FORMAT='(A,A,A,A,A,A,$)'  ; no linebreak
      ENDFOR
      
      PRINTF, lun
    
    ENDFOR

    CLOSE, lun
    FREE_LUN, lun  
 
    dp_refr_status, MESSAGE='Results exported.'
  
END