FUNCTION create_ref_treatcfg

  ref_treatcfg={ $
                substance:  PTR_NEW(/ALLOCATE_HEAP), $
                cal_treat:     PTR_NEW(/ALLOCATE_HEAP), $
                sam_treat:    PTR_NEW(/ALLOCATE_HEAP), $
                cal_ip:    PTR_NEW(/ALLOCATE_HEAP), $
                eval_mode:   PTR_NEW(/ALLOCATE_HEAP) $ ; 'area' or 'height'
                }

  RETURN, ref_treatcfg

END