FUNCTION create_ref_cocorr

  ref_cocorr={ $
              substance:  PTR_NEW(/ALLOCATE_HEAP), $
              comment:    STRARR(6), $
              cal_to_sam: PTR_NEW(/ALLOCATE_HEAP), $ ; [up, down]
              sam_to_sam: PTR_NEW(/ALLOCATE_HEAP), $
              sam_to_cal: PTR_NEW(/ALLOCATE_HEAP) $
              } ; cal-to-cal is assumed to be drift-only!

  RETURN, ref_cocorr

END