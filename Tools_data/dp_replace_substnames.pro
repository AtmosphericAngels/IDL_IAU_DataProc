;+
; PRO: dp_replace_substnames
;
; AUTHOR: F. Obersteiner, Nov-2016
;
; PURPOSE: load a definition file of search- & replace strings in case substance names from the restored *chrom.dat
;          file are outdated, i.e. need to be adjusted to MR table names, PRC table names etc..
;-
;------------------------------------------------------------------------------------------------------------------------
PRO dp_replace_substnames, PATH=path, DEF_FILE=def_file

  COMMON dp_data

  IF NOT KEYWORD_SET(def_file) THEN $
	file=DIALOG_PICKFILE(TITLE='Please select a name definition file.', PATH=path, FILTER='*.csv', /READ) $
		ELSE file=def_file

  IF file EQ '' THEN RETURN

  import=read_csv(file, N_TABLE_HEADER=5, TABLE_HEADER=TableHeader, COUNT=count)

  n_exp = N_ELEMENTS(dp_chrom)

  FOR i=0, n_exp-1 DO BEGIN ; begin loop: search & replace strings
    tmp_chrom = (dp_chrom)[i]
    tmp_subst = (substlist)[i]
    tmp_subst=strreplace_iter(tmp_subst, ',', '', n_iter=5) ; remove nasty stuff from msinfo...
    n_subst = N_ELEMENTS(tmp_subst)
    n_chrom = N_ELEMENTS(tmp_chrom)

    FOR j=0, count-1 DO BEGIN ; begin loop: experiments
      tmp=strsplit(STRTRIM(import.field1[j]), ';', /EXTRACT)
      search_str = tmp[0]
      replac_str = tmp[1]
      w_match=WHERE(STRUPCASE(tmp_subst) EQ STRUPCASE(search_str), n_match)
      IF n_match NE 0 THEN $
        tmp_subst[w_match]=replac_str
    ENDFOR ; end loop: search & replace strings

    new_subst=STRARR(n_subst, n_chrom) ; generate names matrix; n_subst x n_chrom
    FOR k=0, n_chrom-1 DO new_subst[*,k]=tmp_subst

    (tmp_chrom.subst.name) = new_subst
    (substlist)[i] = tmp_subst
    (dp_chrom)[i] = tmp_chrom
  ENDFOR ; end loop: experiments

  dp_refr_status, MESSAGE='Updated substance names.

END