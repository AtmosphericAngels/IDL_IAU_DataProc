FUNCTION create_ref_instrprc

  ref_instrprc={ $
                instrument: '', $      
                substance:  PTR_NEW(/ALLOCATE_HEAP), $
                mp_ppt:     PTR_NEW(/ALLOCATE_HEAP), $ ; instrument 'repro' from precision experiments, absolute
                mp_rel:     PTR_NEW(/ALLOCATE_HEAP), $ ; instrument 'repro' from precision experiments, relative (0-1)
                lod:        PTR_NEW(/ALLOCATE_HEAP), $ ; limit of detection,, absolut
                comment:    PTR_NEW(/ALLOCATE_HEAP) $
                }

  RETURN, ref_instrprc

END