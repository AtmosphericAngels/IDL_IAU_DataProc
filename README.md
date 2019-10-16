# IAU\_DataProc 1.26+ Readme

*Content*
=========

[1 Workflow 1]

[2 Getting started 3]

[2.1 Technical Notes 3]

[2.2 Main widget: "File" tab 4]

[2.3 Main widget: "Advanced" tab 5]

[2.4 Main widget: Config droplists and 'Process Data!' button 6]

[3 Data Processing 7]

[3.1 Treatment configuration 8]

[Treatment of calibration measurements 8]

[Interpolation of calibration points 8]

[Treatment of sample measurements 9]

[Eval Mode 9]

[Use Flag 9]

[3.2 Run calculations 10]

[3.3 Load specific configurations 11]

[3.4 Tools: Precision and Non-Linearity experiment 13]

[3.5 Diagnostics and report 17]

Workflow
========

IAU\_DataProc can analyse measurement series that have been analysed in IAU\_Chrom. Prerequisite: A relative calibration scheme, i.e. a sequence of calibration and sample measurements. The following list describes the workflow in IAU\_DataProc; Fig. 1 on the next page shows the interplay of IAU\_Chrom and IAU\_DataProc.

1.  ***Load IAU\_Chrom data.*** Input are results generated with IAU\_Chrom, i.e. integration of chromatographic signals (signal area, height etc.).  Sect. 2.

2.  ***Add the measurement sequence.*** IAU\_Chrom results do not contain information on what was measured in what sequence. Add this info for further analysis.  Sect. 2.

3.  ***Define sample and calibration treatment.*** Calibration, i.e. interpolation of calibration points, can be done in various ways, i.e. point-2-point, curve fitting etc.   Sect. 3.

4.  ***Run calculations and have a look.*** Use results plot and results table features to display results.  Sect. 3.

5.  ***Save results*** in IDL binary format. Allows you to continue working on the experiment later on. Results can also be saved as text-files.  Sect. 3.

6.  ***Specific tools for specific experiment types***: Analyse Precision and Non-Linearity experiments with specific tools.  Sect. 3.

The core features of IAU\_DataProc can be scripted: Settings for the evaluation of multiple experiment can be put into a csv-table and called from the IAU\_DataProc main widget. Based on this scripting feature, an experiment database can be created.

![1](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img01_IAU_Chrom_Dataproc_Workflow.PNG)

Fig. 1 -- Interaction of IAU\_Chrom and IAU\_DataProc. Right side of the schematic: processing step, left side: software implementation

Getting started
===============

Technical Notes 
----------------

-   Compatibility. IAU\_DataProc is written in the IDL programming language on and for Windows machines (Win 7). A version for Unix-based systems does not exist. Windows 10 can cause problems with the plot windows (lagging etc. but no crash).

-   Error handling is implemented which avoids total crashes in most cases. To avoid data loss in any case, save your progress regularly.

-   To use IAU\_ DataProc on the system partition on your Windows machine (C:\\), it might be necessary to run IDL or the IDL virtual machine in administrator mode.

-   To use the code version (not pre-compiled), an IDL installation (v8.4 or greater) is required.


**Start IAU\_DataProc:**

-   Runtime version, virtual machine only: run IAU\_DataProc\_v1...exe;

-   Code version, IDL installed: open and run the IAU\_DataProc \_v1...pro from the root directory of the IAU\_DataProc project.

Starting IAU\_DataProc opens the main widget, shown in Fig. 2.

![2](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img02_wid_main.PNG)

Fig. 2 - Main widget.

All further widgets (plots, tools) appear as soon as called/required.

Main widget: "File" tab
-----------------------

![3](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img03_wid_main_file.PNG)

Fig. 3 - File tab on main widget.

'File' '**Load Experiment(s)**': Load an IAU\_Chrom experiment, i.e. the IDL binary file that can be created in IAU\_Chrom with 'File' '**Save Experiment**' on the IAU\_Chrom main widget (\*chrom.sav). The loaded experiment(s) should of course contain signal integration results. Multiple \*chrom.sav files can be loaded.

'File' '**Load Experiment-Info(s)**': Load an experiment info file, i.e. a text/csv file that contains information about the measurement sequence of the loaded IAU\_Chrom experiment. If multiple IAU\_Chrom save files were loaded, multiple experiment info files have to be loaded accordingly.

'File' '**Restore DP File**': Restore a processed /saved experiment. Expected file name extension is \*dp\_data.dat. A configuration file with extension \*dp\_expcfg.dat is also expected to be in the same directory (automatically created if the 'save dp file' routine is called).

'File' '**Save DP File**': Save the current experiment including configuration and results to and IDL binary file. Two files are created; results are stored in \*dp\_data.dat, configuration is stored in \*dp\_expcfg.dat. If multiple IAU\_Chrom experiments were loaded / processed, IAU\_DataProc results will be combined to a single save file.

'File' '**Set Filepath...**': Set the default directory where to look for save files etc.

'File' '**Exit'**: Exit the program and close all widgets. Plot and table widgets may stay open and have to be closed manually.

Main widget: "Advanced" tab
---------------------------

![4](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img04_wid_main_advanced.PNG)

Fig. 4 -- Advanced tab on main widget.

'Advanced' '**Run Database Script**': Run a database script to process multiple experiments automatically.

'Advanced' '**Script: Load 1^st^ active**': Load the first experiment (and configuration) that is set to active (active flag = 1) from a database script.

Find a detailed explanation how to configure the database script in the header of the example script table header.

Main widget: Config droplists and 'Process Data!' button
--------------------------------------------------------

![5](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img05_wid_main_features.PNG)

Fig. 5 -- main widget options.

After experiment(s) and experiment info file(s) have been loaded (or a save file was restored), the droplists marked in Fig. 5 (upper part) should be set to appropriate selections.

-   **Instrument**: Should automatically be set to the correct value based on the information stored in the IAU\_Chrom save file. Correct manually if necessary.

-   **Experiment Type**: Select "Canister/Flask Series" if each sample was analysed in a block of repeated measurements of the same sample. Select "In situ/Continuous" if multiple samples were analysed in one block (between calibration measurements).

-   **Experiment Specification**: Unused feature. Leave at default selection ("Samples").

Press the "**Process Data!**" button (Fig. 5, lower part) to call the data processing widget (Fig. 6, next section).

Data Processing
===============

![6](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img06_wid_dataproc.PNG)

Fig. 6 -- data processing widget. Droplist selectsion in the "edit..." section resemble the current evaluation settings for the selected species in the selected experiment.

Fig. 6 shows the processing widget (dialog). There are different ways how to proceed with analysing the data. In principle, each species found in the IAU\_Chrom experiment(s) can be evaluated with a specific treatment configuration, i.e. treatment of calibration measurements, interpolation of the calibration points and treatment of the sample measurements (Fig. 6, "Edit..." section in the centre).

Treatment configuration
-----------------------

[]{#_Toc502136963 .anchor}***Treatment of calibration measurements*.** Determines which calibration measurements or blocks of measurements are considered as "calibration points".

-   **CAL Treat Mthd: bracketing**. Default. Only calibration measurements before and after sample measurements (blocks) are used. If three or more calibration measurements are found between sample measurements, those that do not directly bracket the sample measurements are discarded.

-   **CAL Treat Mthd: block\_mean.** The mean of each block of calibration measurements is used.

-   **CAL Treat Mthd: preceding.** Only calibration measurements directly before each sample measurement (block) are used.

[]{#_Toc502136964 .anchor}***Interpolation of calibration points*.** Sets the method for the temporal interpolation of the calibration points to where the samples were measured.

-   **CAL interpol Mthd: p2p.** Default. Interpolation is done from "point to point", i.e from one calibration point to the next.

-   **CAL interpol Mthd: calsmean.** The mean detector response of all calibration points is used (no interpolation).

-   **CAL interpol Mthd: lin\_fit.** A linear fit is calculated for the calibration points.

-   **CAL interpol Mthd: polyfit\_dg2.** A polynomial fit (2^nd^ degree) is calculated for the calibration points.

***\
***

[]{#_Toc502136965 .anchor}***Treatment of sample measurements*.** Determines which sample measurements are used, which are discarded and how blocks of measurements are treated.

-   **SAM treat Mthd: block\_mean.** Default. For each block of sample measurements, the mean detector relative to the interpolated calibration point (relative response, rR) is calculated. Do not use if multiple different samples are measured within one block (use "individual" in that case).

-   **SAM treat Mthd: individual.** Use if multiple samples are measured within one block, e.g. in case of continuous sampling. An individual relative response is calculated for each sample measurement.

-   **SAM treat Mthd: block\_last... .** Only the last ... measurements of a sample block are used. Mean relative response is calculated from the remaining measurements. Can be useful to discard measurement affected by carry-over.

[]{#_Toc502136966 .anchor}***Eval Mode***. IAU\_Chrom integration results contain signal area and signal height. Signal area is the default selection; change to signal height if a species should be evaluated with height data.

[]{#_Toc502136967 .anchor}***Use Flag***. Opens an editable table, see Fig. 7 red box. By setting the use\_flag of a measurement to zero (enter a "0" in the use\_flag column and press \[enter\]), the respective measurement can be excluded from further evaluation. To apply a changed use\_flag, re-run calculations by pressing the (Re)Calculate !" button (see also sect. 3.2). If you want to undo this operation (re-include the measurement), enter a "1" in the use\_flag column, press \[enter\] and recalculate.

![7](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img07_useflag_table.PNG)

Fig. 7 -- "edit use\_flag" table. Changes to other columns than the use\_flag column have no effect.

Run calculations
----------------

![8](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img08_wid_dataproc_runcalc.PNG)

Fig. 8 -- data processing widget, "run calculations".

**"(Re)Calculate !"** button. Use this option to calculate results for the selected species in the selected experiment or after the treatment configuration has been changed. If calculations were done before for other species, their results will not be recalculated.

**"overwrite use\_flag"** checkbox. Activate to reset the use\_flag of all measurements for the selected species and experiment to the default value specified in the experiment info file. This change is only applied if the "(Re)Calculate !" button is pressed.

All loaded data: **"Run Calculations !"** button. Calculate results for all species in all loaded experiments. The current selection of treatment configuration (see sect. 3.1) is used.

All loaded data: "**overwrite ALL use\_flags**" checkbox. If activated, the use\_flag is reset to the default value (specified in experiment info file) for all measurements. This change is only applied if the "Run Calculations !" button is pressed.

Load specific configurations
----------------------------

![9](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img09_wid_dataproc_config.PNG)

Fig. 9 - data processing widget, 'config' tab.

The config menu on the processing dialog allows you to load additional information for the experiment. This information will also be stored in the save file that can be created from the main widget.

-   **Precision Table**. A table containing reference measurement precision values for each species measured with an instrument. Precision values are used to plot error bars on the results plot (Fig. 16) and calculate the precision flag shown in the results table (Fig. 17).

-   **CAL Mixing Ratio Table**. A table containing mixing ratios attributed to species found in the calibration gas used. If loaded, mixing ratios will be calculated and displayed in the results table (Fig. 17). Note that this calculation assumes linear proportionality of calibration and sample detector response.

-   **TGT Mixing Ratio Table**. A table containing mixing ratios attributed to species found in the target gas(es) measured with in the experiment. Necessary if the NL analyser tool should be used to calculate a detector non-linearity correction function (see sect. 3.4).

-   **Treat. Config**. A table containing species-specific treatment configurations (see sect. 3.1). Avoids the manual selection of the treatment config for each species and is especially useful if different species should be evaluated with different treatment configs.

All configuration data/settings can be removed from the experiment by using the "Config → Reset → ..." function. The treatment configuration can also be exported by using the "Config → Export Treat. Config" function.

To see if and which configuration has been loaded, use the functions from the show-tab on the processing dialog (Fig. 10).

![10](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img10_wid_dataproc_show.PNG)

Fig. 10 - data processing widget, 'show' tab.

Tools: Precision and Non-Linearity experiment
---------------------------------------------

![11](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img11_wid_dataproc_tools.PNG)

Fig. 11 - data processing widget, 'tools' tab.

**Update Subst. Names.** Load a species name definition list. Useful e.g. if different names (spelling etc.) were used for the same species. Unambiguous species names are crucial since configuration data (Cal mixing ratio etc.) is selected based on the species' name.

**Analyse PRC Exp.** Calls a widget (Fig. 12) to analyse a precision experiment, i.e. a measurement series that only contains measurements of the same reference gas.

![12](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img12_tool_PRC.PNG)

Fig. 12 -- Precision experiment analyser widget.

Minimum and maximum block size ("blocksz") refer to the number of sample measurement in a row in between calibration measurements. At least two measurements are required to calculate a relative standard deviation, which is why the minimum value is a block size of two. The maximum block size is dependent on the number of measurements of the reference gas (precision experiment). Calibration block size can be set to 1, 2 or 3.

Trigger the calculation by pressing the "Analyse" button. After calculations are done, a file dialog pops up. It requires you to select a folder where to save results. For each species defined in the experiment, a text file with results will be created in the selected folder. Each file contains results (relative detector response and block relative standard deviation) for the selected block size range.

**Analyse NL Exp.** Calls a widget (Fig. 13) to analyse a non-linearity experiment, i.e. a series of measurements of target gases with known mixing ratios.

![13](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img13_tool_NL.PNG)

Fig. 13 -- Non-linearity experiment analyser widget.

**Requires**: Target gases measured in the measurement sequence and target mixing ratios loaded (see sect. 3.3)

**Mode 0**: No calibration gas mixing ratios loaded estimation of calibration gas mixing ratios.

**Mode 1**: Calibration gas mixing ratios loaded fitting a non-linearity correction curve.

**\
**

**Options:**

-   **Force Zero-Crossing.** Check this box to force the non-linearity correction function through zero, i.e. x/response = 0, y/mixing ratio = 0. Only use with NL analyser mode 1.

-   **Dump Plot.** Check this box to cause a results plot to appear after calculations are finished.

-   **FCT degree.** Degree of the polynomial that is fitted. Select an appropriate value (1-4) depending on how many different target mixing ratios were measured.

-   **Dump Report.** Check this box to save results to a text file after calculations are finished.

Press the "Analyse" button to call the calculation. If the show plot checkbox is checked, one of the following plots appear:

![14](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img14_plot_nlanalyser_calmr_est.PNG)

Fig. 14 -- NL analyser tool in mode 0, calibration gas mixing ratio estimation. X-axis: relative detector response (sample/cal), y-axis target mixing ratios (from definition file, see sect. 3.3).

![15](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img15_plot_nlanalyser_nlcorr.PNG)

Fig. 15 -- NL analyser tool mode 1, non-linearity correction function. X-axis: mixing ratio calculated by linear proportionality, y-axis: difference of mixing ratio calculated by linear proportionality minus the specified target mixing ratio (from definition file, see sect. 3.3). Black triangles: values before correction, blue asterisks: values after correction.

Note that you can only execute calculations for the species currently selected on the processing dialog (Fig. 6).

Diagnostics and report
----------------------

To have a look at your results, press the "Results Table" or "Results Plot" button. This opens a table widget / plot with results for the selected species in the selected experiment (Fig. 16 and Fig. 17).

![16](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img16_resplot.PNG)

Fig. 16 -- Results plot. Error bars for sample measurements only display if an instrument precision table has been loaded.

![17](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img17_restable.PNG)

Fig. 17 -- Results table. Call a more detailed version from the diagnostics menu (see Fig. 18).

![18](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img18_wid_dataproc_diagn.PNG)

Fig. 18 - Data processing widget, 'diagnostics' tab.

From the diagnostics menu, you can also call diagnostic plots with chromatographic ("**Chrom. Diagnostics Plot**") and sample preconcentration data ("**PreCon. Diagnostics Plot**") if available.

The "**Block PRC Plot**" function calls a plot that shows the relative standard deviations of all sample blocks.

If calibration gas mixing ratios have been loaded (see sect. 3.3), a plot that shows sample mixing ratios can be called with the "**MRs Plot**" function.

![19](https://raw.githubusercontent.com/MrFuppes/IDL_IAU_DataProc/master/doc/img/img19_wid_dataproc_report.PNG)

Fig. 19 - Data processing widget, 'report' tab.

From the report tab of the processing dialog, you can create different report files:

-   **All (sel. Exp.).** Creates report files for all species of the selected experiment. The name of the report file indicates the species.

-   **Selected.** Create a report file for the selected species of the selected experiment. The name of the report file indicates the species.

-   **Selected (detailed).** Same as "selected" but contains information that is more detailed.

-   **Means (sel. Exp.).** Creates a report file with average results for all species in the selected experiment.

-   **Exp. Subst. List (sel. Exp.).** Creates a report file containing results for specific species. Requires a species list to be loaded.

  [1 Workflow 1]: #workflow
  [2 Getting started 3]: #getting-started
  [2.1 Technical Notes 3]: #technical-notes
  [2.2 Main widget: "File" tab 4]: #main-widget-file-tab
  [2.3 Main widget: "Advanced" tab 5]: #main-widget-advanced-tab
  [2.4 Main widget: Config droplists and 'Process Data!' button 6]: #main-widget-config-droplists-and-process-data-button
  [3 Data Processing 7]: #data-processing
  [3.1 Treatment configuration 8]: #treatment-configuration
  [Treatment of calibration measurements 8]: #_Toc502136963
  [Interpolation of calibration points 8]: #_Toc502136964
  [Treatment of sample measurements 9]: #_Toc502136965
  [Eval Mode 9]: #_Toc502136966
  [Use Flag 9]: #_Toc502136967
  [3.2 Run calculations 10]: #run-calculations
  [3.3 Load specific configurations 11]: #load-specific-configurations
  [3.4 Tools: Precision and Non-Linearity experiment 13]: #tools-precision-and-non-linearity-experiment
  [3.5 Diagnostics and report 17]: #diagnostics-and-report
  [1]: media/image1.png {width="7.124577865266842in" height="8.224792213473316in"}
  [2]: media/image2.png {width="3.2610859580052494in" height="2.7559055118110236in"}
  [3]: media/image3.png {width="3.1496062992125986in" height="2.627309711286089in"}
  [4]: media/image4.png {width="3.1496062992125986in" height="2.644555993000875in"}
  [5]: media/image5.png {width="3.1496062992125986in" height="2.4575579615048118in"}
  [6]: media/image6.png {width="3.9370067804024496in" height="3.768844050743657in"}
  [7]: media/image7.png {width="4.797544838145232in" height="3.052509842519685in"}
  [8]: media/image8.png {width="3.937007874015748in" height="3.7688451443569555in"}
  [9]: media/image9.png {width="3.937007874015748in" height="3.7196576990376204in"}
  [10]: media/image10.png {width="3.937007874015748in" height="3.7773326771653544in"}
  [11]: media/image11.png {width="3.937007874015748in" height="3.7575995188101485in"}
  [12]: media/image12.png {width="3.4635673665791775in" height="1.7031375765529309in"}
  [13]: media/image13.png {width="2.8437707786526683in" height="2.35418416447944in"}
  [14]: media/image14.png {width="5.887112860892389in" height="4.05in"}
  [15]: media/image15.png {width="6.3in" height="4.334027777777778in"}
  [16]: media/image16.png {width="6.3in" height="3.3472222222222223in"}
  [17]: media/image17.png {width="6.3in" height="3.422222222222222in"}
  [18]: media/image18.png {width="3.937007874015748in" height="3.797113954505687in"}
  [19]: media/image19.png {width="3.937007874015748in" height="3.7679943132108487in"}