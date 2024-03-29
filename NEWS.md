# zooimage News

## Changes in zooimage 5.6.1

-   parseini() -\> type.convert(..., as.is = TRUE) = argument added to avoid spurious warnings.

-   zidDatMake(): make sure Label column is character, not factor.

-   getTrain(): make sure Class is factor.

-   Define auxiliary programs for Linux.

-   Bug correction in selectFile(): now filters are correct.

-   Bug correction in saveObjects(): multiple= is not an argument for the Unix version of the dlgOpen() dialog box + filters corrected.

## Changes in zooimage 5.6.0

-   Function skimageVars() added to calculate additional attributes using Python and Scikit-Image. The numpy and skimage Python packages are required and Python 3 is used. The reticulate R package is also needed now.

## Changes in zooimage 5.5.2

-   Bugs corrected in zidToZidb(), zidbMake() and sampleInfo() that prevented to convert old (zooimage v.1) .zid files to .zidb files.

-   Change import() to importFrom() in NAMESPACE for many dependencies for a more robust management of these dependencies.

-   importImg(), zieCompile() and zieCompileFlowCAM() are adapted to detect if a "regular" set of images, or FlowCAM data are compiled.

## Changes in zooimage 5.5.1

-   In makeZIVignettes(), scale bars are now recalculated and regenerated according to PixelSize/PixelUnit metadata.

-   Two new functions to help visualize the content of .zidb files: zibdSummary() displays all numerical data and all the metadata, and zidbPlotPage() plots the vignettes page by page (by 25 items on each page).

-   In zidDatMake(), only the two last image metadata was kept. This bug is corrected now.

-   zidbPlotPage() has now two more arguments: method & class to use validated data and define the class to filter before displaying vignettes.

## Changes in zooimage 5.5.0

-   Reconcile the new process introduced in ZooImage 5.4-10 (ZI5 files with colored vignettes) with the previous ones that produced ZI1 & ZI3 files.

## Changes in zooimage 5.4-12

-   Shiny part translated to English (thanks to Teresa Sofia Giesta da Silva).

## Changes in zooimage 5.4-11

-   Vignettes can now be created from within R, using the new `makeZIVignettes()` function.

-   `zidbMake()` and `makeZidb()` are updated to handle the new 'ZI5' format with tinted vignettes created directly from within R.

## Changes in zooimage 5.4-10

-   `sampleInfo()` now can process `_dat3.zim` and `_dat5.zim` files as well as the original `_dat1.zim` files.

-   `checkFirstLine()` can now deal with ZI1 - ZI5 files.

-   `zimDatList()` now recognizes `_dat1.zim`, `_dat3.zim` and `_dat5.zim` files.

-   `zidDatMake()` now creates ZI5 files.

-   `zisRead()` now tries to convert date, but if it fails, it continues leaving the unmodified string and issues a warning.

-   `zidbMake()` now creates ZI5 files and can replace `<<<SMP>>>` in label, `<<<DATE>>>` in date, and `<<<TIME>>>` in time metadata fields with appropriate values. Note that time must be provided in the new `smptime =` argument. In case `CellPart` is missing, the default value of `0.73` is assumed. Similarly, if `Replicates` is missing, a value of `1` is assumed by default. The `check.vignettes =` is now `FALSE` by default, since we may generate them on the fly.

## Changes in zooimage 5.4-9

-   A bug in `errorCorrection()` led to fail with message telling it is impossible to replace `GpFPDiff` because replacement has 0 rows and data has xxx.

-   Plankton sorter pages used <file://><path>/planktonSorter.html?v=xxxx, but the argument v= is falsely interpretted as part of the file path in Linux. So, the version information of the page is now dropped.

-   Function imported from grDevices, graphics and stats are now declared in NAMESPACE.

## Changes in zooimage 5.4-8

-   A bug in `processSamplesWithCells()` failed to select another "cells.rds" file when a default one is recorded.

-   `recode.ZITrain()` and `recode.ZITest()` mangled completely the classes when `depth =` argument was provided. Corrected.

-   Warnings were issued when `zieCompile()` did not found calibration images. Corrected.

-   The `processSamples()` function was not able to deal with `.zid` files any more. Now, it can use both `.zid` and `.zidb` files, but not a mix of them simultaneously.

-   A regression prevented `zieMake()` to finialize the `.zim` files from a `.zie` template. Corrected.

-   In `errorCorrection()`, a table with items not sorted alphabetically led to a mix in the correction/validation of the vignettes. Corrected.

## Changes in zooimage 5.4-7

-   Correction of a bug that prevented the calculation of cells per colonies to work (changes in `cellCompute()` not taken into account here).

-   The `calibration()` function to calibrate grayscale for flat-bed scanner is rewritten to use only R code.

-   A wrong default argument in `makeZidb()` is replaced with a more sensible value.

-   `processSample()` and `processSampleAll()`: new defaults for `keep =` and `detail =` arguments (takes classes with uppercase first character), and a couple of minor bug corrections.

-   Several bugs corrected in `viewResults()` and `exportResults()`.

-   External executables (Vuescan, Fiji or ImageJ, XnView and metadata editor) are not distributed any more with zooimage because this is not allowed by CRAN. The routine to detect those programs is more robust for Mac (should be in the Applications folder), and under Windows where it looks for a `/Zooimage/bin` folder into the Program Files (64bit first, then 32bit).

-   Correction of a bug in `zidbMake()` in the computation of the `.zidb` file name.

## Changes in zooimage 5.4-6

-   The default `dropVars()` function now also drops "MinFeret", "AR" and "Round" that make problems between old and new samples.

## Changes in zooimage 5.4-5

-   A bug in `activeLearing()` function prevented to use data and/or classifiers build with versions of zooimage older than 5.4-x. Corrected.

## Changes in zooimage 5.4-4

-   Various bugs corrected in the new functions for cells counting, calculation of samples using cells counting and the plankton sorter GUI.

## Changes in zooimage 5.4-3

-   Cells counting per colonies (per particles) is now provided.

-   `processSamples()` now look for `.zidb` files instead of `.zid` files.

## Changes in zooimage 5.4-2

-   Correction of one bug in the function `addVigsToTrain()`.

-   The functions `activeLearning()` and `viewFrenchManual()` were not exported in the `NAMESPACE`, but are required for the menus. Fixed.

## Changes in zooimage 5.4-1

-   Correction of various bugs by Guillaume Wacquet.

## Changes in zooimage 5.4-0

-   Functions to count cells in a particle (colony): `countCells()` and the corresponding `countCellsGUI()` function for an access through the menu.

-   Function to build predictive models for cells in particles (colonies) after counting: `cellModel()`.

-   Function to compute the number of cells in particles in a new sample: `cellCompute()`.

-   Function to make `.zidb` file for FlowCAM data through the menu: `makeZidbMakeFlowCAM()`.

-   Correct `makeClass()` function in the menu (missing formula).

-   Functions to process active learning: `contextSelection()`, the associated `addItemsToTrain()` and `dropItemsToTrain()` functions to complete the training set with validated items, and the corresponding `activeLearningGUI()` function for an access through the menu. Integration in the classification process.

-   Function `compTrain()` to compare two training sets and highlight differences.

-   Translation of the user manual in French and update to include all changes.

-   Adaptation of menus to include also the new features.

## Changes in zooimage 5.3-0

-   `calcVarsVIS()` now included in `calcVars()` and not accessible any more as top function (to avoid duplicate code).

-   `ecd()` and `ecdCell()` are now merged in `ecd()` using arguments `cells = 1`.

-   `errorCorrection()` and `planktonSorterPage()` functions now use the new interface to get R httpd server port, for R svn rev \>= 67550.

-   In the shiny app, dataTableOutput is now called from the DT package. Dependency to DT is now added.

-   The planktonSorter html page was sometimes too small, with the last group being wrapped consequently. Solved by increasing the page by 1 pixel \* groups.

-   A bug in `processSample()` prevented for calculation with both `keep =` and a data frame for `biomass =` provided simultaneously.

## Changes in zooimage 5.2-0

-   `importFlowCAM()` now can deal with color FlowCAM images (but they are first converted into grayscale because background calibration images are recorded as grayscale by Visual Spreadsheet -at least, the tested version 3.2.3-, hence we cannot subtract the background of the vignettes in color mode)!

-   `importFlowCAM()` now iterates a message to indicate progression of vignettes importation.

-   For `importFlowCAM()`, the default value of the argument `rgb.vigs =` is changed from `TRUE` to `FALSE`.

-   Functions to count cells in a particle (colony): `cellCount()` and the corresponding `cellCountGUI()` function for an access through the menu.

-   New utility function `ecdCell()` to calculate the ECD for one cell in a colony.

-   Functions to build predictive models for cells in particles (colonies) after counting: `cellModel()`.

-   Function to compute the number of cells in particles in a new sample: `cellCompute()`.

-   Function to make zidb file for FlowCAM data through the menu: `zidbMakeFlowCAMGUI()`.

-   Correct `makeClass()` function in the menu (missing formula).

## Changes in zooimage 5.1-0

-   `calcVars()`/`calcVarsVIS()` and `dropVars()` are reworked to use only `FIT_xxx` variables in case of FlowCAM data (and to calculated many derived vars from there). This way, there is no need any more of a second image analysis in ImageJ.

-   A new UI for error correction using shiny.

## Changes in zooimage 5.0-0

-   `importFlowCAM()` and `readFlowCAMlst()` are reworked to create complete `.zidb` files using all metadata from various version of Fluid Imaging's Visual Spreadsheet software.

## Changes in zooimage 4.0-2

-   `correctError()` has now a mode argument allowing to run the analysis in 'demo' and 'stat' mode, in addition to the default 'validation' mode

-   The internal `errorCorrection()` function did not intialized ntrusted and nsuspect in 'demo' mode

-   New version of the user manual (explanations of the new functions).

## Changes in zooimage 4.0-1

-   A bug (non initialisation of the confusion matrix) prevented to use `errorCorrection()` in demo or stat mode. Corrected.

## Changes in zooimage 4.0-0

-   Error correction functions added: `correctError()`.

-   Plankton sorter interface to manual validation added.

-   New dependency on the digest package.

-   A new menu entry, `Validate classification...`, is added for validation and error correction.

## Changes in zooimage 3.0-7

-   `readFlowCAMlst()` can now read Visual Spreadsheet `.lst` file format 017 where the name of the columns is explicitly provided in the header of the file.

-   `zimDatMakeFlowCAM()` now uses `readFlowCAMlst()` instead of the hidden function `.lstRead()` to avoid duplicated code.

-   `zidVerify()` now builds automatically the `_dat1.zim` file from FlowCAM data if it is not there, but corresponding `.lst` and `.zim` files are found in the parent directory.

## Changes in zooimage 3.0-6

-   `zidUncompress()` unzipped files in current directory, ignoring its `path =` argument. Solved now.

-   `zidUncompressAll()` now computes full names for `.zid` files argument by default.

-   `calcVarsVIS()` added. Specific function to calculate derived variables from FlowCAM's Visual Spreadsheet software.

-   Added the extraction of RGB vignettes when importing FlowCAM data.

## Changes in zooimage 3.0-5

-   Functions to import FlowCAM data directly.

## Changes in zooimage 3.0-4

-   Default `dropVars()`: adding a few items, including 'Skew', 'Kurt' and 'Solidity' that produce NAs in ImageJ for multiple ROIs objects.

## Changes in zooimage 3.0-3

-   Minor adaptations to DESCRIPTION file.

## Changes in zooimage 3.0-2

-   Assignment to `.GlobalEnv` is strictly restricted to GUI operations, and only following direct request by the end-user through an explicit dialog box. The internal function `.assignGlobal()` is used for that purpose.

## Changes in zooimage 3.0-1

-   `getTrain()` was not able to collect data when attributes measured on samples mixed in training set differ slightly. Now, data are more carefully merged.

-   Meaningless `FIT_xxx` attributes from the FlowCAM are now excluded by default in `dropVars()`.

-   The GUI function `vignettesClass()`, and thus, the corresponding menu entry were not implemented (the function prepareTest() that is called internally was working properly, but was not reacheable through the menu). Done now.

-   The GUI function `analyzeClass()` and the corresponding menu entry listed old (version 2) items. The list is now updated with new plots available in zooimage 3 and mlearning 1.

-   All the java code (ImageJ + zooimage plugins) are now moved to zooimageJ.

## Changes in zooimage 3.0-0

This is a major refactoring of code from zooimage 1.x and 2.X. NEWS file now.
