FUNCTION create_ref_calmrs

; Heap memory pointers have to be used here as it is not known in advance,
; how many elements the structure will contain. Also, array sizes might be
; different for different experiments loaded in the dp_data list.

  ref_calmrs = { $
              canister:  '', $
              substance:  PTR_NEW(/ALLOCATE_HEAP), $
              mr_ppt:     PTR_NEW(/ALLOCATE_HEAP), $  ; mixing ratio, absolut
              unc_ppt:    PTR_NEW(/ALLOCATE_HEAP), $  ; uncertainty, absolut
              unc_rel:    PTR_NEW(/ALLOCATE_HEAP), $  ; uncertainty, relative (0-1)
              unit:       PTR_NEW(/ALLOCATE_HEAP), $
              scale:      PTR_NEW(/ALLOCATE_HEAP), $  ; primary calibration scale
              comment:    PTR_NEW(/ALLOCATE_HEAP) $
              }

  RETURN, ref_calmrs

END