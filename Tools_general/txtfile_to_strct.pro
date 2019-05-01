;+
; FUNCTION txtfile_to_strct
;
; AUTHOR: F. Obersteiner, June 2018
;
; VERSION: ALPHA
;
; PURPOSE: load data from a textfile and store it to an IDL structure.
;   Each data column is required to have a column name (e.g. "time" or "temperature_X").
;
; USES: valid_num function.
;   Tests if string can be converted to numeric based on regular expression.
;
; INPUT Args:
;   file: full file path
;   txtsep: column separator in txt file, e.g. ";"
;   nl_tbl_hdr: number of lines to skip befor data import
;
; RETURN VALUE: Structure with tagnames being the column names from the text file.
;   All columns are converted to floating point numbers; in case of a type conversion error,
;   type string is used
;
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION txtfile_to_strct, file, txtsep, nl_tbl_hdr

  IF file EQ '' THEN RETURN, !NULL
  IF FILE_TEST(file) EQ 0 THEN RETURN, !NULL

  nl = FILE_LINES(file)
  nl_data = nl-nl_tbl_hdr
  input = STRARR(nl)

  OPENR, LUN, file, /GET_LUN
  READF, LUN, input
  CLOSE, LUN
  FREE_LUN, LUN

  input = input[nl_tbl_hdr-1:nl-1] ; cut header

  tags = strsplit(STRCOMPRESS(input[0], /REMOVE_ALL), txtsep, /EXTRACT, /PRESERVE_NULL)
  n_parm = N_ELEMENTS(tags)

  data = STRARR(n_parm, nl-nl_tbl_hdr)
  FOR i=0L, nl-nl_tbl_hdr-1 DO $
    data[*,i]=strsplit(input[i+1], txtsep, /EXTRACT, /PRESERVE_NULL)

  strct={}
  FOR i=0, n_parm-1 DO BEGIN
    valid = valid_num(data[i,0])
    IF valid THEN col_type = 5 ELSE col_type = 7
    strct=CREATE_STRUCT(strct, tags[i], FIX(data[i,*], TYPE=col_type))
  ENDFOR

  RETURN, strct

END
