FUNCTION create_ref_nl, fct_dgr, sz_mrs

    nl_ref = { $ ; one parameter-set for each substance
              species:  '', $ ; substance name
              fct_dgr:  fct_dgr, $ ; degree of non-linearity fitfunction
              fct_parms:  DBLARR(fct_dgr+1)*!VALUES.D_NAN, $ ; parameters of fitfunction
              fct_sigma:  DBLARR(fct_dgr+1)*!VALUES.D_NAN, $ ; parameters' sigma
              max_dev_to_fit: !VALUES.D_NAN, $ ; deviation between target MRs and fit (in case of cal MR estimation / not defined)
              corr_max_dev_to_zero: !VALUES.D_NAN, $ ; maximum deviation of corrected values to zero (ideally: no deviation)
              cal_in_tgt_range: !VALUES.D_NAN, $ ; 0 or 1, if cal mixing ratio falls within tgt MRs range (cal MR defined)
              cal_MR_spec:  !VALUES.D_NAN, $ ; cal MR from config table
              cal_MR_err: !VALUES.D_NAN, $ ; cal MR abs. error from config table
              cal_est_MR: !VALUES.D_NAN, $ ; cal MR estimate based on tgt MRs if no cal MR specified
              cal_est_sig:  !VALUES.D_NAN, $ ; error estimate for cal MR calculated from tgt MRs
              lin_mrs:  DBLARR(sz_mrs)*!VALUES.D_NAN, $ ; MRs calculated linearily
              lin_mrs_rsd:  DBLARR(sz_mrs)*!VALUES.D_NAN, $ ; RSDs inferred from blocks
              tgt_names:  STRARR(sz_mrs), $ ; names of target gases
              tgt_mrs:  DBLARR(sz_mrs)*!VALUES.D_NAN, $ ; target MRs from table
              delta_mrs:  DBLARR(sz_mrs)*!VALUES.D_NAN $ ; lin_mrs-tgt_mrs
              }

  RETURN, nl_ref
  
END