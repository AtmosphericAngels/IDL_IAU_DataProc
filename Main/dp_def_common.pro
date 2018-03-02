;------------------------------------------------------------------------------------------------------------------------
;+
; PURPOSE
; Definition of DataProc global variables.
;
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_def_common, version


; +++ data
; +++
  COMMON DP_DATA, path_wd, error_handler_IO, dp_chrom, dp_expcfg, chromlist, expinflist, substlist, $
                  dp_vers, cal_ip_mthd, cal_treat_mthd, sam_treat_mthd, sid_name, instr, prc_limits
                  

    path_wd     = ''
    error_handler_IO = 1
    
    dp_chrom    = !NULL ; main data, from restored chrom files
    dp_expcfg   = !NULL ; experiment configuration (expinfo-file)
    
    chromlist   = !NULL ; array of loaded experiments (chrom*.dat)
    expinflist  = !NULL ; array of loaded experiment-info files
    substlist   = !NULL ; list of substance name arrays found in chrom files (LIST type variable)

    dp_vers     = STRCOMPRESS(STRING(version, FORMAT='(F5.2)'), /REMOVE_ALL)
    
    cal_ip_mthd = ['p2p','calsmean','linear_fit','polyfit_dg2']
    
    cal_treat_mthd = ['bracketing','block_mean','preceding'] 
      
    sam_treat_mthd = ['block_mean','individual','block_last_1','block_last_2', $
                     'block_last_3','block_last_4']
  
    sid_name    = ['Air','Target','Calibration','Blank_Gas','Blank_Vacuum','undef','undef','undef'] ; IDs (e.g. cal = 3) are the indices in this
                                                                            ; vector +1
  
    instr       = ''
;    inst_val=['Lab_QP/SFMS', 'GhOST_MS', 'Lab_BenchTOF', 'FASTOF', 'GhOST_ECD', 'AED', 'GHGGC_ECD/FID']

    prc_limits = [2.,3.] ; multiples of instr precision that trigger spec. flags
                         ; below first limit: ok // between first and second limit: bad // above second limit: discard



; +++ widget identifiers
; +++
  COMMON DP_WIDID, dp_widid

    dp_widid =  {  $
                  dp_mainwid      : -1, $   ; main window (load data & basic experiment configuration)
                  dp_dataproc     : -1, $   ; "calculations" window
                  dp_restablewid  : -1, $   ; resultstable window
                  dp_selmeas      : -1, $   ; select measurements table
                  dp_prcexp       : -1, $   ; precision experiment analyser
                  dp_nlexp        : -1  $   ; non-linearity experiment analyser
                     } 
                                        
END