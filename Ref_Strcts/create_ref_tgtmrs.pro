FUNCTION create_ref_tgtmrs

  ref_tgtmrs={ $
              tgt_name:   PTR_NEW(/ALLOCATE_HEAP), $
              substance:  PTR_NEW(/ALLOCATE_HEAP), $
              mr_ppt:     PTR_NEW(/ALLOCATE_HEAP), $  ; mixing ratio, absolut
              unc_ppt:    PTR_NEW(/ALLOCATE_HEAP), $  ; uncertainty, absolut
              unc_rel:    PTR_NEW(/ALLOCATE_HEAP), $  ; uncertainty, relative (0-1)
              scale:      PTR_NEW(/ALLOCATE_HEAP), $  ; primary calibration scale
              comment:    PTR_NEW(/ALLOCATE_HEAP) $
            }

  RETURN, ref_tgtmrs

END