;-----------------------------------------------------------------------------------------------------------------------
;+
; NAME:
; IAU_DataProc_Main
;
; Idea and Conception:
; H.Boenisch , A.Engel, F.Obersteiner, S.Sala, T.Schuck
;
; Programming:
; S.Sala & T.Schuck (alpha), F.Obersteiner (1.X)
;-
;------------------------------------------------------------------------------------------------------------------------
@dp_def_common
;------------------------------------------------------------------------------------------------------------------------
PRO IAU_DataProc

  COMMON dp_data

  dp_def_common, '1.30' ; string == version

  path_wd = 'D:\'

  error_handler_IO = 0

  dp_wid_main_ini

;  Outdir = 'D:\PROGRAMMING\debugging\iau_dataproc\rt'
;  sfile = 'D:\PROGRAMMING\IDL_WD\IAU_DataProc_1X\IAU_DataProc_v128.sav'
;  MAKE_RT, 'IAU_DataProc_v128', Outdir, SAVEFILE=sfile, /OVERWRITE

END
;------------------------------------------------------------------------------------------------------------------------
; Changelog (newest first)
;
; 2018-03-17
; CHANGELOG FROM NOW ON IN THE FORM OF GIT COMMITS!
;
; 2018-03-12 (v1.28, FO)
; - added unit tag to structures that contain absolute values / mixing ratios.
; - implemented unit tag functionality in cal mr handling procedures.
; - implemented unit tag functionality in tgt mr handling procedures.
;
; 2018-02-26 (v1.27, FO)
; - removal of carry-over config from dp_expcfg struct: finished. Boolean "active_corr" in rres struct is also
;   set to false.
; - added option to show loaded carry-over parameters.
; - added calculation of carry-over for height response and "calculate all".
; - carry-over, cal mrs, tgt mrs, prc: added option to apply config to all loaded experiments.
; - some tables and report files: removed "ppt" from columns etc. specifying mixing ratios.
; - treatment config and carry over config: moved to dp_manage ...  pro files.
;
; 2018-02-16 (v1.27, FO)
; - FUNCTION create_ref_rsp: added key 'norm_corrected'.
; - FUNCTION create_ref_rres: added key 'active_corr'.
; - processing widget, tools menu: added load and reset carry over correction.
; - created FUNCTION create_ref_cocorr to hold parameters of co correction.
; - (incomplete) created PRO dp_read_cocorrparms to add carry over correction parameters to dp expcfg.
; - (incomplete) created PRO dp_remv_cocorr to remove parameters and correction flag in rres.active_corr[0].
; - moved to dp_manage_calmrs.pro: FUNCTION dp_read_calmrs, PRO dp_apply_calmrs, PRO dp_remv_calmrs.
; - moved to dp_manage_tgtmrs.pro: FUNCTION dp_read_tgtmrs, PRO dp_apply_tgtmrs, PRO dp_remv_tgtmrs.
; - moved to dp_manage_instrprc.pro: FUNCTION dp_read_instrprc, PRO dp_apply_instrprc, PRO dp_remv_instrprc.
; - dp_expcfg: added key 'carryover' (created in PRO dp_call_expinfo).
; - main widget restore dp files functionality: changed check version to 1.26.
; - added /no_zero keyword to struct_assign in FUNCTION dp_strct2current_expcfg. Prevents generation of null pointers
;   in structures that were not present in the restored dp file.
; - created FUNCTION dp_calc_carryover in Tools_data folder.
;
; 2017-12-27 (v1.26, FO)
; - bugfix nl analyser, check for tgt mrs
;
; 2017-12-21 (v1.26, FO)
; - bugfix for restoring old version dp data: now also works with files that contain multiple experiments.
; - bugfix for "show tgt mr table": check for tgt mrs with ptr_valid() and check for null-pointer.
;
; 2017-11-30 (v1.26, FO)
; - IMPORTANT bugfix on calc_relresp, calculation of block mean relative response (had wrong indices specified).
;
; 2017-11-27 (v1.26, FO)
; - bugfix on main widget: experiment type droplist; 'undef' case now covered by case statement.
;
; 2017-11-24 (v1.26, FO)
; - nl analyser plot tweak; if cal is estimated and out of targed range, the plot of the fit is extended to the
;   estimated mr of the cal.
; - dataproc wid: added "reset" option for cal mixing ratios (PRO dp_remv_calmrs).
; - bugfix on PRO dp_apply_instrprc: handling of not matching instrument specification (default value of "quest"
;   variable).
; - dataproc wid: added "reset" option for instrument precision values.
; - dataproc wid: added "reset" option for treatment config.
; - apply/reset treatment config: overwrite set to zero -> loading or deleting a treat cfg should not change the
;   use_flag setting.
; - dataproc wid: added "reset" option for tgt mixing ratios.
;
; 2017-11-07 (v1.26, FO)
; - extended NL analyser report file.
;
; 2017-10-26 (v1.26, FO)
; - bugfix, nl analyser: target range determination now only includes targets that are found in the measurement series.
; - nl analyser: fit curve now smooth.
; - nl analyser: added error message in case no target MRs are found for selected species.
;
; 2017-09-29 (v1.26, FO)
; - nl strct: added tag "mean_dev_to_fit" to hold value of mean absolut deviation of fitted values - measured values
;   (targets).
; - nl analyser: changed calculation; now uses mean relative response for each target. measure errors for the fit are
;   calculated as error of the mean for each target. Thereby, different number of measurements per target are considered.
; - nl plot now even nicer!
;
; 2017-09-21 (v1.25, FO)
; - relresp calc, auto-select of sample treat method: included use-flag.
; - dp_calc_relresp: only for "block last 1", no block mean rR is calculated.
; - dp_calc_mrs: added check for sample treatment method; if "individual" is selected, individual rR is used. All
;   other cases block mean rR.
; - results plot and nl plot: improved x- and yrange determination.
; - application of instrument precision: implemented eval_mode (area- or height-specific)
;
; 2017-09-12 (v1.25, FO)
; - nl analyser: error estimates for fit now include target uncertainty from the loaded config file
;
; 2017-09-01 (v1.25, FO)
; - dp_apply_calmrs: check for multiple cal names is now limited to measurements with use flag set to 1.
; - bugfix on nl analyser: implementation of use_flag - values where use_flag is 0 are not considered anymore.
; - bugfix on nl analyser: in case a target is measured only once, the respective value in the measure_error variable is
;   set to the mean error (samples rR rsd) of all targets.
; - nl analyser tweaked error estimates for fit
; - nl analyser tweaked error estimate of derived mixing ratio
;
; 2017-08-23 (v1.25, FO)
; - nl_analyser: introduced cal MR estimation based on tgts rel. response.
; - nl analyser output: added estimated cal MR to textfile
; - added 'analyse_nl' tag to dataproc script
;
; 2017-08-19 (v1.25, FO)
; - results table: column header tweak: now says [%] if a value is displayed as a percentage
; - call relrespcalc routine: added keyword AUTO_SAMTREAT. If set, sample blocks are checked if multiple samples are
;   found within one block. if yes, treatment 'individual' is selected automatically.
; - dp_analyse_seq: bugfix on check for multiple individual samples in one sample block.
;
; 2017-08-15 (v1.24, FO)
; - dp_read_instrprc, dp_read_tgtmrs, dp_read_calmrs: replaced read_csv function with simple file openr and
;   added file_test.
; - dp_read_treatcfg: moved readf out of loop, added file_test.
; - bugfix on tgt mr table: added check if values are loaded.
; - dp_dbscript_call: added clearing of variables and heap memory (dangling pointers).
; - dp_analyse_seq: added keyword 'quiet' to suppress dialog popup.
; - processing widget: fixed update behavior after a treatment config has been applied.
; - integration of cal mrs, instr prc etc.: handling of temporary list elements -> introduced TEMPORARY function.
; - dp_dbscript_chk: check for existing files; modified so that only active (=1) entries of the table are checked.
; - main widget: added experiment spec / type 'undef'
; - script call routine: added setting of setup info structure
;
; 2017-08-11 (v1.24, FO)
; - begin implementing script feature =>
; - first version of FUNCTION dp_dbscript_read completed.
; - treatcfg loading: structural change on wid dataproc - relresp calc is now called AFTER treatcfg has been loaded
;   AND widget has been refreshed. Removed parameters sel_caltreat, sel_samtreat, sel_calip from PRO dp_read_treatcfg.
;   Added keyword FILE to override pickfile dialog.
; - PRO call_relresp_calc: code cleanup + added keywords caltreats, samtreats, calipmthds (definition of valid
;   entries from sequence analyser).
; - PRO dp_res2txt: added keyword DEF_PATH to get no dialog popup.
; - first version of FUNCTION dp_dbscript_write completed.
; - first version of FUNCTION dp_dbscript_chk completed.
; - PRO config_dp_mainwid: added keyword DEF_UNAME to trigger a specific operation without supplying an actual uname
;   in the event structure.
; - dataproc wid: added option to show target mixing ratio table -> PRO dp_show_tgtmrtable.
; - PRO dp_replace_substnames: added keyword DEF_FILE.
; - added subst name definition to scipt; if filepath is set, substance names will be updated after chrom data and
;   experiment info are loaded.
;
; 2017-08-09 (v1.24, FO)
; - modified search_tags for GHGGC expinfo.
; - FUNCTION time2jultime: changed keyword date_is_mdy to input_is_mdy to avoid ambiguous keyword error and
;   added check if input string length is LE 3, if yes then return a !VALUES.D_NAN.
; - FUNCTION jultime2timestring: changed keyword name from "dateonly" to "onlydate" to avoid ambiguous keywrd error.
;
; 2017-08-01 (v1.23, FO)
; - bugfix, widget destroyer: now works in all situations and uses widget_control bad_id keyword.
; - load experiment: added check if file contains chrom structure.
;
; 2017-07-04 (v1.23, FO)
; - finished txt file export for results of nl analyser.
; - bugfix on cal preceding method: missing first or last cal now treated correctly (no crash...).
; - bugfix on calculation of cal min2max variable: now also correct if first or last cal is missing.
;
; 2017-06-29 (v1.22, FO)
; - added eval mode to treat config table (additional column at end).
;   -> involves code modifications in:
;      FUNCTION create_ref_treatcfg, PRO dp_read_treatcfg, PRO dp_call_relresp_calc, PRO dp_export_treatconfig.
; - dp_call_relresp_calc: removed check for 'current' keyword if calculations are called for selected experiment &
;   substance as this call demands the keyword to be set.
; - dp_wid_dataproc_ini: added update of droplists prior to calling the event handler. Ensures that if a saved experiment
;   is loaded, the correct settings are displayed.
; - dp_nlexp_analyse: changed operation to process a singel species upon call, i.e. selected species on dataproc wid.
;
; 2017-06-21 (v1.21, FO)
; - added FUNCTION create_ref_nl to get a reference structure that holds non-linearity experiment results.
; - dp_calc_mrs: added input for keyword eval_mode
; - report to txt: added dp_calc_mrs function keyword eval_mode
; - completed a first running version of dp_nlexp_analyse.
;
; 2017-06-18 (v1.21, FO)
; - modified project folder structure to resemble composition of the program; moved some .pro files into different
;   folders.
; - dp_call_expinfo: added instrument 'GHGGC_ECD/FID' + configuration of import function.
; - modified time2jultime function to be able to process time strings in the form of "dd.mm.yyyy hh:mn:ss", transferred
;   to the function either as keyword TIME or DATE. time2jultime now also returns the NaN system variable if the string
;   'NaN' is transferred. Added keyword DATE_IS_MDY to let the function know that the supplied date string is
;   formatted 'mm.dd.yyyy'
;
; 2017-06-17 (v1.21, FO)
; - added dp_strcts2current_version.pro. Similar to IAU_Chrom, save files from older versions of dataproc can be
;   converted to the current version (structures...) with this routines.
; - the version checker also adds a structure tag that contains the current version of dataproc.
; - added FUNCTION create_ref_tgtmrs to hold information on target mixing ratios
; - integrated tgt mrs structure in: dp_call_expinfo, dp_gen_empty_expcfg
; - processing dialog: restructured config menu
; - added nl analyser widget (unfinished)
;
; 2017-06-15 (v1.21, FO)
; - PRC analyser report files: added cal block size info in file header.
; - PRO dp_apply_calmrs: added check for multiple cal names (not case-sensitive). Application of values will be
;   aborted if more than one cal name is specified to avoid ambiguous results.
;
; 2017-06-12 (v1.20, FO)
; - PRO dp_def_common: removed obsolete variables (cal_cyl, tgt_cyl, prc).
; - PRO dp_def_common: added variable 'prc_limits' to hold limits for prc flags.
; - Added Tanja's modification of report file header to display info on prc_limits.
; - Added Tanja's PRO dp_exp_list, renamed it to "dp_report_list" and unified it with PRO dp_read_exp_list.
; - PRO dp_call_relresp_calc: added application of instrument prc (if defined) also if calculation is only called for
;   specific experiment and substance.
;
; 2017-06-11 (v1.20, FO)
; - renamed some files that contain functions/procedures for precision experiment analysis,
;   made them all start with "dp_prcexp" and moved them to folder "Add_Features".
; - finished beta version of precision experiment analyser (to be tested).
; - brought back the good old process! button on the main widget.
; - in Add_Features folder: prepared procedures and functions for database script:
;   -> PRO dp_dbscript_call: main routine
;   -> FUNCTION dp_dbscript_chk: script checker
;   -> FUNCTION dp_dbscript_read / FUNCTION dp_dbscript_write (dp_dbscript_rw.pro): script import/export.
;
; 2017-05-22 (v1.19, FO)
; - MR plot: now shows individual samples if no blocks are available
; - processing dialog is now closed automatically if expinfo is reloaded
;
; 2017-05-19 (v1.19, FO)
; - renamed procedure: 'dp_load_expinfo' is now 'dp_call_expinfo'
; - droplists Experiment Type and Experiment Specification: removed fields that do not have implications yet.
; - dp_read_expinfofile: added condition 'only mfc vol available' if no dp, p0 or p1 given.
;
; 2017-05-12 (v1.19, FO)
; - added instrument types 'AED' and 'GHGGC-ECD/FID'
;
; 2017-05-09 (v1.19, FO)
; - fix of logic error on dp_interpol_cal: lines 70 and 75 added condition that block length at beginning / end must
;   be greater than typical block length so that it can be coerced to the typical block length. Avoids illegal subscript
;   range in sequence_ix array if the start/end cal block contains LESS or EQUAL the number of typical cals per block.
; - export of experiment mean values in case of multiple target measurements(blocks) should now work
;
; 2017-04-20 (v1.19, FO)
; - renamed: Enrich diagn. plot is now PreCon. ..., old diagn. plot is now Chrom. ...
;
; 2017-02-21 (v1.19, TJS)
; - changed naming of report.txt including block treatment in all report file names and using '_det' for
;   detailed report file names
; - added additional report (careful with column target mxr, average mixing ratio is printed to file,
;           doesn't work if there's more than one target bottle in the Experiment)
; - additional Diagnostic plot with enrichment data from Expinfo (mfc_vol, dt, s_vol)
; - dp_calc_mrs: replaced negative mixing ratios (due to -1 in Standard mixing ratio table) by NaN
;
; 2016-12-19 (v1.18, FO)
; - dp_interpol_cal: added calculation of deviation of cal values to a polyfit 2nd degr through these values.
; - cal ip mthds, linfit and polyfit: added check if enough data (gt 2)
; - ref struct rres: added tag rsp_select to indicate whether signal area or height is selected for data evaluation
; - dp_calc_mrs: removed double and n_digits keyword, double prec floats are now returned by default
; - rres rsp struct: renamed "cal_series_rsd" to "cal_devtofit"
; - rres rsp struct: renamed "cal_series_drift" to "cal_mintomax"
; - selection for area or height eval included in
;   -> dp_calc_mrs
;   -> dp_show_restable
;   -> dp_call_relresp_calc
;   -> dp_dp_res2txt (changed report file header)
;   -> config_dp_calcwid
;   -> dp_plot_mrs_prc
;
; 2016-11-24 (v1.17)
; - function dp_calc_mrs: added keyword 'double' to return a double precision float array instead of string array
; - added some status messages (plots and tables)
; - added dp_replace_substnames procedure. loads a .csv to get a definition of search- and replace strings for
;   substance names. called from processing dialog -> tools menu.
; - jultime2timestring function: added keyword "date_mdy" to format the output date string month-day-year. Default
;   changed to day-month-year.
; - no_case keyword added to strreplace_iter to make the search case-insensitive.
;
; 2016-11-15 (v1.16)
; - def_common: dp version as parameter to be set on main .pro
; - processing dialog: added diagnostics menu
; - moved 'detailed results table' to diagnostics menu
; - moved 'diagnostics plot' to diagnostics menu
; - dp_show_restable (detailed): added calculation of dt_precon in seconds
; - added PRO dp_plot_mrs_prc to plot precision and mixing ratios
; - restructured code: dp_wid_dataproc_handle
;
; 2016-11-11 (v1.15)
; - use flag: is now applied to rres-struct; at the moment the expinfo is imported.
; - save dp file: added option to enter a filename
; - revised GhOST_MS_generate_expinfofile.pro and adjusted search tags in dp_load_expinfo
;
; 2016-11-10 (v1.145)
; - Ghost expinfo now also imported by dp_read_expinfofile.
; - use GhOST_MS_generate_expinfofile.pro to generate the expinfo-file for a measurement series.
;
; 2016-11-09 (v1.14)
; - strreplace: modified to replace multiple occuraces of search string, renamed strreplace_iter.
; - cal mrs table import: separated data read function and apply routine.
; - instr prc import: added check if instrument specification in file and selection on main wid matches.
; - some rearrangements of .pros and folders
; - dp_read_expinfofile: added exception; if p0 and p1 values are found, mfc value is considered non-essential.
;
; 2016-11-08 (v1.13)
; - renamed dp_read_expinfo -> dp_load_expinfo
; - created dp_read_expinfo_file -> one function to read them all
; - dp_call_relresp_calc: calculation of n_A/V based on s_vol.
;
; 2016-11-07 (v1.12)
; - expinfo import routines: added header search. allows to search for tags like 'header_size' to specify a different
;   header size than the default.
; - expinfo import routines: added search for s_vol_select. variable is also available as keyword. if not set as keyword
;   or found in file header, defaul 0 (dp) is used.
; - added tag s_vol_select to ref_expinfo; will be used to indicate if dp (0) or mfc (1) was used to generate s_vol.
; - added dataproc wid: show -> sample volume info (dp or mfc)
;
; 2016-10-27 (v1.11)
; - code clean-up, added some comments.
; - added dynamic determination of IDs (e.g. Cal=3) based on indices in common variable sid_name.
; - added suffix 'iaudp' if a reportfile is generated for selected experiment and substance.
; - added mean sample block rsd to report file header.
; - rearranged main widget.
; - removed selection of monitor (display widget on desktop monitor instead)
; - added function "find_fnumber".
; - implemented function "find_fnumber" in dp_read_expinfo; allows a more sophisticated comparison of filenames specified
;   in expinfo and filenames found in loaded chrom (iau_chrom experiment). file numeration e.g. '_01' is now considered
;   to be equal to '_1' or '_0001' etc.
;
; 2016-10-20 (v1.10)
; - Processing Dialog: renamed 'load' to 'config', added export treat. config option (PRO dp_export_treatconfig).
; - dp_call_relresp_calc: added auto-select of default treat option if a non-valid option is stored in the config-file.
; - dp_correct_time: bugfix; if error is in first chromatogram, timestamp vector is corrected using the median of the
;   time differences between the measurements.
; - bugfix on integration flag: check GE 1, not EQ 1 (2="manually" integrated).
;
; 2016-10-18 (v1.09 beta)
; - overwrite behaviour: expinfo is now only overwritten if the re-import was successful.
; - error bars in results plot: added check if substances are available.
; - bugfix on call relresp calc: added check if treatconfig was actually loaded (pointer points to data, not !null).
; - bugfix on processing dialog: added 'else:' case for all undefined unames.
; - commented out: creation of samcon structure if a chrom.dat is restored.
; - added 'overwrite' keyword to dp_read_expinfo. if set, all results (rres) in dp_chrom are overwritten with NaN.
;
; 2016-10-14 (v1.08 beta)
; - introduced status message on main widget. moved 'hot button' to menu/process.
; - configured status messages on main widget.
; - bugfix on dp_destroy_wids: added check for valid id before attempting to destroy respective widget.
; - bugfix on dp_res2txt: correct selection of cal mr and instr prc if available in export txt header. also: replaced
;   '-NaN' with 'NaN'.
; - cleaned code, widget initialisation (selection of monitor to display and widget size).
; - included iau_dp version in report file header (added to first line).
; - added some status messages for processing dialog.
; - restructured processing dialog.
;
; 2016-10-13 (v1.07 beta)
; - bugfix on dp_interpol_cal, fit methods: removed NaNs from input vectors.
; - restructured processing dialog.
; - read_expinfo: changed case structure to type 'CASE 1 OF'.
; - bugfix on call_relresp_calc: if treat. config is loaded (current=0 / false), treat methods from config are used
;   correctly in cal_ip / calculation of rR. Also, substance comparison is now executed with strupcase.
;
; 2016-10-12 (v1.06 beta)
; - added warning message if multiple experiments are loaded and exp spec is not 'samples'.
; - added warning message if the sequence analyser findes less sample blocks than individual sample names read from
;   the exp-info file.
; - added table widgets to show loaded precision and mixing ratio data.
; - moved prc analyser to 'add_features' folder.
; - restructured processing dialog menus.
; - introduced dp_read_treatcfg.pro to load substance-specific treat methods.
; - introduced FUNCTION create_ref_treatcfg to create a structure within dp_expcfg that holds treatment method defaults
;   for specific substances. the treatcfg tag is introduced to dp_expcfg in dp_read_expinfo.pro.
; - dp_call_relresp_calc: introduced keyword CURRENT. if set, the current selection of treatment methods is used.
;   otherwise, relresp_calc will try to obtain settings from the treatcfg struct in dp_expcfg list.
; - added PRO dp_show_treatcfgtable (processing dialog -> 'show') to display loaded treatment configuration.
; - cleaned read_expinfo procedure and respective functions
;
; 2016-10-11 (v1.05 beta)
; - modified mini wid prc calc: added cal block size.
; - added additional checkbox 'overwrite' on proc dialog to specify overwrite keyword if calculation is called for a
;   specific selection. default is 0 / no / checkbox unchecked.
; - bugfix on dp_call_relresp_calc: correct usage of variables sel_subst and subst_name.
; - dp_dp_res2txt: added keyword "brief" to report results for samples/targets only. set to 1 in dp_wid_dataproc_handle.
; - changed folder structure to separate core features from 'on-top' features. created folder 'Add_Features'. contains
;   functions/procedures that deal with prc flagging and MR import.
;
; 2016-09-22 (v1.04 beta)
; - bugfix on export results to txt: mr calculation if /ALL
; - included preliminary MRs in results tables
; - tested save & restore option, seems to work
; - precision flag is now recalculated if rR calculation is re-executed after precision values have been loaded
; - plot: changed to errorplot, integrated errorbars if instrument precision values available
; - revised integration of cal mixing ratios (report returned by keyword verbose)
; - added folder Prc_Exp for functions/procedures needed to analyse precision experiments. contains dp_wid_prcexp_ini,
;   a mini-widget to set the min/max blocksize to analyse. dp_analyse_prcexp_seq: mini-version of the sequence analyser,
;   to test wether the loaded data is suitable for a typical precision experiment analysis. dp_analyse_prcexp: the
;   actual precision experiment analyser function.
; - reports of the precision experiment analysis will be created by dp_prcexp_res2txt in the report folder.
;
; 2016-09-21 (v1.03 beta)
; - pointer problem seems to be fixed now... (see 2016-09-20)
; - cleaned folder structure of project
; - programmed 'brief' results table wid for sel_exp/sel_name
; - instr prc: separate csv import and integration routine to allow separate recalculation of prc flags if rR values are
;   recalculated after a prc table has been loaded
;
;
; 2016-09-20 (v1.03 beta)
; - extended header of report files (cal scale etc. if available)
; - created function dp_calc_mrs to calculate MRs for the report files
; - removed FreeVar from project as it seems to cause pointers to become invalid
;
; 2016-09-19 (v1.03 beta)
; - programmed integration of instrument precision data.
; - modified report procedure to print additional cal information in header and precision flags
; - programmed integration of calibration gas mixing ratios
;
; 2016-09-17 (v1.02 beta)
; - added tool: flt_round2string, converts float to string and rounds to specified number of digits.
; - prepared introduction of precision experiment analyser tool
; - minor tweaks, i.e. destruction of widgets and widget size
;
; 2016-09-16 (v1.02 beta)
; - introduced dp_sel_meas_table allows the substance- and experiment-specific selection of measurements that should be
;   used for data processing. edits use_flag of rres structure in dp_chrom.
; - implemented rres struct use_flag in call_relresp_calc for the determinatino of vd_cal and vd_sam (vd_sam incl. tgts)
; - if use_flag is changed manually, this information is written back to rres struct 'data_select' (introduced).
; - introduced vd_cal in interpol_cal, all cal treat methods.
; - added handling of included/excluded datapoints in relresp calc
; - minor tweaks, i.e. selection of droplists on dp dialog if another experiment is selected
;
; 2016-09-15 (v1.01 beta, first beta-testing...)
; - created functions create_ref_instrprc and create_ref_calmrs to prepare the inclusion of instrument precision values
;   and calibration gas mixing rations. structures are added to dp_expcfg in dp_read_expinfo procedure
; - cal interpol: preceding method, from the last cal block a point is selected that would correspont to a cal point
;   that would precede the next sample assuming a typical block length (which is dynamically determined from the sequence
;   as the median of all cal blocks)
; - removed 'manual' from sample and cal treat methods
; - dp_interpol_cal: programmed 'block_mean', method
; - rres structure: cal_block_rsd is now calculated and written to struct if cal treat method is 'block_mean'
; - dp_interpol_cal: programmed 'calmeans','linear_fit' and 'polyfit_dg2' interpolation methods
; - dp_calc_relresp: programmed 'block_last...' methods
;
; 2016-09-13 (v0.9 alpha)
; - created PRO dp_dpres2txt to allow txt export of results. Can export results for all files of the selected experiment
;   to individual txt files or the selected substance of the selected experiment to a txt file.
; - added function jultime2timestring to tools folder. does what the name says.
; - introduced ECD data compatibility
; - rres structure: added cal_block_rsd
; - dp_interpol_cal: programmed 'preceding' method
;
; 2016-09-12 (v0.8 alpha)
; - dp_read_expinfo: outsourced actual expinfo import to functions dp_get_[...]_expinfo.
; - made the expinfo import work for lab, fastof and ghost
; - bugfix on dp_correct_time, now actually works
; - bugfix on dp_calc_relresp, now doesnt corrupt sam_treat_mthd variable
; - added update of cal treat / sam treat / cal ip droplists if another substance is selected.
; - indroduced 'apply manual changes' functionality; individual substances can now be evaluated with different
;   cal treat / sam treat / cal ip methods.
; - dp_modify_plot: changed determination of x- and yrange
; - modified plot rres: if there is no valid data, nothing is plotted
;
; 2016-09-09 (v0.7 alpha)
; - minor tweaks, overwriting of loaded data and cdf-file timestamp correction
;
; 2016-09-08 (v0.7 alpha)
; - restored plot functionality: plot diagnostic
; - renamed dp_wid_table_ini to dp_wid_restable_ini. made it non-editable as it should only show results.
; - restored functionality: show results table
; - introduced rres.eval_flag to handle the state of the data evaluation (0 = not evaluated, >0 = evaluated)
; - sequence analyser is now only called if sequence was not analysed yet
; - added error handler
; - if dp save file is restored: update settings of instr/exp type/spec droplists
; - if expinfo file(s) is/are overwritten, eval_flag is reset to 0
;
; 2016-09-07 (v0.6 alpha)
; - main wid: activated path change option
; - dp wid: treat droplist checks & minor tweaks
; - introduced general cal treat and sam treat options as common variables. options for individual experiments depend
;   on the sequence of the experiment.
; - introduced core calculation PRO dp_call_relresp_calc, FUNCTION dp_interpol_cal and FUNCTION dp_calc_relresp.
;   made it basically functional with bracketing cal option and sample block means.
; - summarised Stephan's plot functions in dp_plot_tools.pro
; - restored plot functionality: plot normalised responses
;
; 2016-09-06 (v0.5 alpha)
; - introduced PRO dp_analyse_seq to analyse the measurement sequence. This experiment-specific property is written to
;   dp_expcfg. Knowledge of the sequence (position of cals, blocksize of samples etc.) is needed to apply different
;   cal- and sample-treatment methods (to be selected on dataproc wid).
;
; 2016-09-05 (v0.4 alpha)
; - changed data structure: removed expinfo substructure from dp_chrom as this is not chromatogram-specific information
;   but experiment specific. created structure dp_expcfg for that purpose.
;   - added: dp_expcfg.sequence <- sequence information needed for choosing cal and sample treatment
;   - added: dp_expcfg.setup <- instrument, experiment type, experiment specification; needed for restoring dp files etc.
;   - implemented main widget refresh if data is loaded or a selection is made
;
; 2016-09-02 (v0.3 alpha)
; - changed widget structure; introduced second widget where the actual "processing" can take place. The idea was to
;   keep the code of the main widget readable.
;
; 2016-09-01 (v0.2 alpha)
; - import exp-info:
;   - simplified tag comparison of imported file and default (removed col strct)
;   - added string operation; allows the removal of certain phrases before the comparison of filenames given in dp_chrom
;     and expinfo ("remove_strings", l.218)
;   - added save and restore capability for the data that is being processed (incomplete)
;
; 2016-08-30 (v0.1 alpha)
; - "reboot" version; restructured MAIN wid, removed hidden fields.
;   - restructured experiment handling: multiple chrom files can now be imported into a LIST
;   - new structural tags are created upon import in dp_restore_chrom FUNCTION
;   - dp_correct_time: adjusted for dp_chrom in LIST structure
;   - main_wid: added basic configuration capability (instrument, experiment type)