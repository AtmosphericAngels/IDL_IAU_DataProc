;------------------------------------------------------------------------------------------------------------------------
;+
; PURPOSE
; structure that contains index arrays specifying cal and sample types
; requires n_measurements to specify the default length of the index vectors (see below)
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION create_ref_sequence, n_measurements

  def_vector = LONARR(n_measurements)+(-1L)

  ref_sequence = { $
                  ID:               def_vector, $ ; id vector from expinfo-file
                  n_cal_blocks:     -1L, $        ; number of...
                  n_sam_blocks:     -1L, $        ;

                  ix_cal:           def_vector, $ ; vector with value 1 at position n if true, 0 if false
                  miss_1stcal:      -1L, $        ; 1/0; implies new calculation requirement for relative response...
                  miss_lastcal:     -1L, $
                  ix_init_calblock: def_vector, $ ; position of first cal in each block. unused vector position can appear!
                  ix_end_calblock:  def_vector, $ ; position of last cal in each block. unused vector position can appear!
                  cal_treat:        STRARR(10), $ ; cal treatment options

                  ix_sam:           def_vector, $ ; same for samples...
                  ix_init_samblock: def_vector, $
                  ix_end_samblock:  def_vector, $
                  sam_treat:        STRARR(10)  $ ; sample treatment options
                  }

  RETURN, ref_sequence

END