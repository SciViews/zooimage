package org.sciviews.zooimage.fitvis; 

//  Purpose: A generic model for FIT VIS data handling in ImageJ is required to 
//   work across multiple versions of FIT's VIS. 
//   
//  Inputs: 
//   A directory with ...
//     - paired collages and calibration images
//     - a data export file (csv format) which must contain for each subimage ...
//         - collage image name
//         - calibration image name
//         - subimage width and height
//         - start location in collage
//         - start location in calibration image
//         - Timestamp
//  
//   Caller also provides (via GUI, or macro arguments, 
//    or command line arguments) ...
//     - "Interval of recalibration? 
//          (minutes, default = 0 i.e. only one cal file)
//     - Pixel size (um per pixel, assume square pixels, default  pixelSize=1)
//     - Min area in um^2 (equivalent to MinABD only, default   minSize=0)
//     - Max area in um^2 (equivalent to MaxABD only, default   maxSize=Inf)
//     - One of "Light pixels", "Dark Pixels", 
//         or "Both Light and Dark Pixels" (default = "Dark" but could be 
//         "Light" or "Both")
//     - Dark pixel threshold (a relative grayscle value, default 
//        thresholdDark=25)
//     - Light pixel threshold (a relative grayscale value, 
//          default thresholdLight=0)
//     - "Fill Holes?" or "Don't Fill Holes?" (default = "Fill")
//     - "Largest object only?" or "Treat all objects as one" 
//          (default = "Largest")
//     - "Vingettes saved?" (default = "Vignettes")
//     - "Scalebar on vignette?" (default = "Scalebar")
//     - "Enhance vignette?" (default = "Enhance")
//     - "Outline vignettes?" (default = "Outline")
//     - "Masks saved?" (default = "Masks")
//     - "Verbose operation?" (default = "Verbose")
//  
//  Example from macro with everything explicitly specified: 
//  	run("FITVIS Basic", "myDirectory interval=0 pixelSize=1.234 minSize=12 maxSize=200 Dark thresholdDark=25 thresholdLight=0 Fill Largest Vignettes Scalebar Enhance Masks Verbose")
//  
//  
//  Output...
//    - The ImageJ results table as "results.csv"
//    - optional vignettes (enhanced? scalebar?, outlined?) in /_name
//    - optional masks in _mask/
//   
//  Subsequent work in other software (R)...  
//     - merge FIT's data_export.csv with "results.csv"
//     - zipping, etc.
//  
//  Specialized applications, configured to operate with individual
//   FIT VIS versions can then be created as wrappers to this plugin.
import ij.IJ;
import ij.ImagePlus;
import ij.WindowManager;
import ij.gui.GenericDialog;
import ij.gui.PolygonRoi;
import ij.gui.Roi;
import ij.io.OpenDialog;
import ij.measure.ResultsTable;
import ij.plugin.PlugIn;
import ij.plugin.filter.Analyzer;
import ij.plugin.frame.RoiManager;
import ij.process.ImageProcessor;
import ij.util.Tools;

import java.awt.Rectangle;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

public class FITVIS_ implements PlugIn {

	/** fully qualified directory to process  */
	String dir ; 
	
	/** equivalent to R's dirname(dir) */
	String name ;
	
	/** tools, config file contents, the output data */ 
	String exportFilename ;
	ZPItools zpiTools ;
	ZPIconfig vCfg ; 
	
	/** from data_export.csv file */
	ZPIvData vData;
	
	/** results aggregated from ImageJ */
	ZPIvData vResults ;
	
	/** names of the collages */
	String[] colFiles; 
	
	/** one cal file per collage (even if repeated) */
	String[] calFiles; 
	
	/** indices for collages and cal images */
	int[] colSlices, colStarts; 
	int[] calSlices, calStarts;
	
	/** collage stack  */
	ZPIVirtualStackPlus colImp; 
	
	/** calibration stack */
	ZPIVirtualStackPlus calImp; 
	
	int collageID ;
	int calibrationID;
	
	boolean bShowGUI ;
	boolean bVerbose ;
	
	String[] origList;

	public FITVIS_(){
		zpiTools = new ZPItools() ;
		vCfg = new ZPIconfig( zpiTools.getThisPluginPath() + "fitvis_.cfg" ) ; 
		vResults = new ZPIvData() ; 
		bVerbose = vCfg.getKeyValueBoolean("verbose") ;
		bShowGUI = false ; 
		collageID = 0 ; 
		calibrationID = 0; 
		dir  = "" ; 
		name = "" ; 
		exportFilename = ""; 
	}
	
	/**
	 * Implements the required run() method for Plugins
	 * 
	 * The "Macro.getOptions()" method is invoked to scan for 
	 * arguments passed from the macro (if called that way).
	 *
	 * Use the macro recorder to determine the appearance of the various
	 * arguments when called from a macro.
	 *
	 **/ 
	public void run(String opt){

		// for development only
		vCfg.setKeyValue("verbose", true);

		// String testOpt = "select=/Users/Shared/data/336-101800/data_export.csv interval=30 pixelsize=2.15 minsize=12 maxsize=1000 distance=45 use=dark thresholddark=20 thresholdlight=25 fill vignettes scalebar enhance outline masks batch verbose archive";
		// opt = testOpt;
		// IJ.log("opt=" + opt);

		if ((opt != null) && (opt.length() != 0)){
			System.out.println(opt);
			processOptionsFromMacros( opt ) ;
		} else {
			if (! showDialog() ) {return;}
			//showDialog();
			bVerbose = vCfg.getKeyValueBoolean("verbose");
		}
		
		if (vCfg.getKeyValue("usepixels").equalsIgnoreCase("light")){
			IJ.log("Light pixel segmentation not implemented yet");
			IJ.showMessage("Light pixel segmentation not implemented yet");
			return;
		}

		if (vCfg.getKeyValue("usepixels").equalsIgnoreCase("both")){
			IJ.log("Light and Dark pixel segmentation not implemented yet");
			IJ.showMessage("Light and Dark pixel segmentation not implemented yet");
			return;
		}

		process();

	} // end of run()

	// end of setFileAndDirectoryNames

	public void processOptionsFromMacros(String opt){
		String[] opts = opt.split(" ") ;
		String[] s; 
		for (int i = 0; i < opts.length; i++){
			s = opts[i].split("=");
			if (s[0].equalsIgnoreCase("interval")){
				vCfg.setKeyValue("interval", s[1]);
			} else if (s[0].equalsIgnoreCase("pixelSize")){
				vCfg.setKeyValue("pixelSize", s[1]);
			} else if (s[0].equalsIgnoreCase("minSize")){
				vCfg.setKeyValue("minSize", s[1]);
			} else if (s[0].equalsIgnoreCase("maxSize")) {
				vCfg.setKeyValue("maxSize",s[1]);
			} else if (s[0].equalsIgnoreCase("distance")){
				vCfg.setKeyValue("distance", s[1]);
			} else if (s[0].equalsIgnoreCase("use")) {
				vCfg.setKeyValue("usePixels", s[1]);
			} else if (s[0].equalsIgnoreCase("thresholdLight")) {
				vCfg.setKeyValue("thresholdLight", s[1]);
			} else if (s[0].equalsIgnoreCase("thresholdDark")) {
				vCfg.setKeyValue("thresholdDark", s[1]);
			} else if (s[0].equalsIgnoreCase("Fill")) {
				vCfg.setKeyValue("fill", "true");
			} else if (s[0].equalsIgnoreCase("largest")) {
				vCfg.setKeyValue("largest", "true");
			} else if (s[0].equalsIgnoreCase("vignettes")) {
				vCfg.setKeyValue("vignettes", "true");
			} else if (s[0].equalsIgnoreCase("scalebar")) {
				vCfg.setKeyValue("scalebar", "true");
			} else if (s[0].equalsIgnoreCase("enhance")) {
				vCfg.setKeyValue("enhance", "true");
			} else if (s[0].equalsIgnoreCase("outline")){
				vCfg.setKeyValue("outline", "true");
			} else if (s[0].equalsIgnoreCase("masks")) {
				vCfg.setKeyValue("masks", "true");
			} else if (s[0].equalsIgnoreCase("verbose")) {
				vCfg.setKeyValue("verbose", "true");
			} else if (s[0].equalsIgnoreCase("batch")){
				vCfg.setKeyValue("batch", "true");
			} else if (s[0].equalsIgnoreCase("archive")){
				vCfg.setKeyValue("archive", "true");
			} else if (s[0].equalsIgnoreCase("select")) {
				setFileAndDirectoryNames(s[1]);
			} else {
				IJ.log("oops! unrecognized argument: " + opts[i]);
			}
		} //for loop through arguments
		bVerbose = vCfg.getKeyValueBoolean("verbose");
	}


	/**
	 * This is the primary method for stepping through the processing collages
	 */
	public void process(){

		//needed for measuring performance
		long startTime = System.currentTimeMillis();

		//prepare to archive if requested
		if (vCfg.getKeyValueBoolean("archive") == true){
			File ff = new File(dir);
			name = ff.getName();
			origList  = zpiTools.listFiles(dir);
		}

		if (bVerbose == true){
			IJ.log("Configuration...");
			IJ.log("dir=" + dir);
			IJ.log("exportFilename="+exportFilename);
			vCfg.print();
		}

		//load the data exported by FlowCAM (a user-specified .txt file)
		if (loadExportData() == false){
			IJ.log("Error loading the export data file: " + exportFilename);
			return;
		}
		vData.split();
		vData.delim = ",";

		//turns off inversion and conversion scaling
		IJ.run("Appearance...", "  menu=0");

		//open the collages
		try{
			openCollages() ; 
		} catch( Exception e){
			IJ.log("Error opening collages");
		}
		
		//open the calibration images
		try{
			openCalImages() ;
		} catch( Exception e){
			IJ.log("Error opening collages");
		}

		//create the output container    
		this.vResults = new ZPIvData();
		vResults.delim = ",";

		//bring the collages forward (more entertaining) and set the scale
		IJ.selectWindow("Collages");
		IJ.run("Set Scale...", "distance=1 known="+ vCfg.getKeyValue("pixelSize") +" pixel=1 unit=um global");

		//now process each collage slice - calling the companion macro to do the
		//per subimage processing
		try{
			processCollages() ;
		} catch( Exception e){
			IJ.log("Error processing collages");
			return;
		}

		//save the results - what name do we want?
		vResults.writeFile(dir + "results.csv");

		//archive if requested
		if (vCfg.getKeyValueBoolean("archive") == true){
			String rawDir = zpiTools.constructDirName(dir, zpiTools.rawDirName);
			if (zpiTools.mkDir(rawDir)==false){
				IJ.log("Unable to create _raw subdirectory");
				return;
			}
			String[] list2 = new String[origList.length];
			for (int i = 0; i<origList.length;i++){list2[i] = dir + origList[i];}
			String zipName = rawDir + name +".zip";
			if( zpiTools.zipFiles(list2, zipName, true)==false){
				IJ.log("Unable to zip raw data");
				return;
			} 
		}// archive the originals


		// a few final niceties
		long endTime = System.currentTimeMillis();
		if (bVerbose){
			IJ.log("FITVIS_ Done!");
			double et = (endTime-startTime)/1000.;
			IJ.log("Results written to " + dir + "results.csv");
			IJ.log("Elapsed time = "+ et + " seconds");
			IJ.log("Average time per subimage = " + 
					et/vResults.size() + " seconds");
		}


	}//end of process



	/**
	 * Processes each collage, in turn, by calling the FITVIS_.txt macro.
	 */
	private void processCollages(){
		String tab = "\t";
		String eol = "\n";
		String macroFile = zpiTools.getPluginPath() + "FITVIS.macro";
		String s2 = "";
		String s3 = "";
		String stub = "verbose=" + bVerbose + tab + 
		"batchmode="+ vCfg.getKeyValue("batch") + tab + 
		"directory="+dir + tab + 
		"pixelsize=" + vCfg.getKeyValue("pixelSize") + tab + 
		"minsize=" + vCfg.getKeyValue("minSize") + tab + 
		"maxsize=" + vCfg.getKeyValue("maxSize") + tab + 
		"distance="+vCfg.getKeyValue("distance") + tab + 
		"usepixels=" + vCfg.getKeyValue("usePixels") + tab + 
		"thresholddark=" + vCfg.getKeyValue("thresholdDark") + tab + 
		"thresholdlight=" + vCfg.getKeyValue("thresholdlight") + tab + 
		"fill=" + vCfg.getKeyValue("fill") + tab + 
		"largest=" + vCfg.getKeyValue("largest") + tab + 
		"vignettes=" + vCfg.getKeyValue("vignettes") + tab + 
		"scalebar=" + vCfg.getKeyValue("scalebar") + tab + 
		"enhance=" + vCfg.getKeyValue("enhance") + tab + 
		"outline=" + vCfg.getKeyValue("outline") + tab + 
		"masks=" + vCfg.getKeyValue("masks") + tab + 
		"log=" + vCfg.getKeyValue("log") + tab + 
		"collageID="+ collageID + tab  + 
		"calibrationID="+ calibrationID + tab + "slice=";

		int iID = vData.getFieldIndex("Id");
		int cX = vData.getFieldIndex("CaptureX");
		int cY = vData.getFieldIndex("CaptureY");
		int sX = vData.getFieldIndex("SaveX");
		int sY = vData.getFieldIndex("SaveY");
		int pW = vData.getFieldIndex("PixelW");
		int pH = vData.getFieldIndex("PixelH");

		int[] x;
		int[] y;
		int nSlices = colStarts.length-1;
		//      ID   collageSlice, sX, sY   pW  pH  calSlice  cX  cY
		//       0    1            2    3   4   5       6     7    8
		for (int iSlice = 0; iSlice < nSlices ; iSlice++){
			s2 = stub + (iSlice+1) + "\n";
			x = seq(colStarts[iSlice], colStarts[iSlice +1]-colStarts[iSlice]);
			// y = seq(calStarts[iSlice], calStarts[iSlice+1]-calStarts[iSlice]);

			for (int k = 0; k < x.length ; k++){
				s3 = 
					vData.getDataElement(iID, x[k]) + tab  +
					colSlices[x[k]] + tab + 
					vData.getDataElement(sX, x[k]) + tab  +
					vData.getDataElement(sY, x[k]) + tab  + 
					vData.getDataElement(pW, x[k]) + tab  +
					vData.getDataElement(pH, x[k]) + tab  +
					calSlices[x[k]] + tab  +
					vData.getDataElement(cX,x[k]) + tab  +
					vData.getDataElement(cY,x[k]) + eol;
				//IJ.log(s3);
				s2  = s2 + s3;
			}//k-loop
			//tah-dah - this is where the macro is called
			s3 = IJ.runMacroFile(macroFile, s2);
			//IJ.log("result="+s3);
			//the first line has a copy of the current fieldnames of measures
			
			String[] rs = s3.split("\n");
			if(iSlice==0){
				vResults.vData.add(rs[0]);
			}             
			for ( int k = 1; k<rs.length;k++){
				vResults.vData.add(zpiTools.fixDecimals(rs[k],3,","));
			}

		}//iSlice-loop
		
	}


	/**
	 * Opens the collages
	 **/
	private void openCollages(){
		int colIndex = vData.getFieldIndex("Filename");//index of collage column
		this.colStarts = getCollageStarts(true); //collages
		int nCol = colStarts.length-1;//reduce by one because of the getImageSTarts
		this.colSlices = startsToSlices(colStarts);
		String[] colNames = new String[nCol];//get the unique names
		for (int i=0; i<nCol; i++){
			colNames[i] = vData.getDataElement(colIndex, colStarts[i]);
		}
		ZPIVirtualStackPlus colImp = new ZPIVirtualStackPlus(dir, colNames);
		colImp.setTitle("Collages");
		colImp.show(); 
		int ids[] = WindowManager.getIDList();
		collageID = ids[ids.length-1];   
	}

	/**
	 * Opens the cal images
	 **/
	private void openCalImages(){
		this.calStarts = getCalStarts(true); //cal images
		int nCal = calStarts.length-1;//padding trick 
		this.calSlices = startsToSlices(calStarts);
		String[] allNames = zpiTools.listFiles(dir);
		String[] calNames = new String[nCal];//
		IJ.log("nCal="+nCal);
		int count = 0;
		for (int i = 0; i< allNames.length; i++){
			if (allNames[i].matches("^cal.*\\.tif$")){
				calNames[count] = allNames[i];
				count++;
			}
		} 

		//populate the remaining cal files with the first if the 
		//number of cal*.tif files is less than the number of iterations
		// of the recalibration cycle
		if (count < nCal){
			for (int i = count; i<nCal; i++){
				calNames[i] = calNames[count-1];
			}
		} 
		ZPIVirtualStackPlus calImp = new ZPIVirtualStackPlus(dir, calNames);
		calImp.setTitle("Calibrations");
		calImp.show();
		int ids[] = WindowManager.getIDList();
		calibrationID = ids[ids.length-1];  
	}

	/**
	 * given the data_export.csv fully qualified filename
	 * extract the directory and filename
	 */
	private void setFileAndDirectoryNames(String arg){  
		File file = new File(arg);
		exportFilename = file.getName();
		dir = file.getParent() + File.separator;    
	}

	/**
	 * Sequences a vector from start to start + length
	 **/ 
	private int[] seq(int start, int length){
		int[] x = new int[length];
		for (int i = 0; i<length; i++){ x[i] = i + start;}
		return x;
	}

	/**
	 * Populates the slice indices
	 */
	private int[] startsToSlices(int[] starts){
		int n = starts[starts.length-1];
		int[] slices = new int[n];
		for (int i = 0; i < (starts.length-1); i++){
			for (int j = starts[i]; j < starts[i+1]; j++){
				slices[j] = i+1;
			}
		}
		return slices;
	}
	/**
	 * Finds the start index for each cal image or collage in the vData
	 **/  
	private int[] getCollageStarts(boolean appendLastIndex){
		String theName = "Filename";    
		vData.split();
		int index = vData.getFieldIndex(theName);
		String[] names = vData.getDataColumn(index);
		int[] v = new int[names.length];
		v[0] = 0;
		int count = 1;
		for (int i=1;i<names.length;i++){
			//   IJ.log("i="+i+ "  " + names[i] + "   " + names[i].equalsIgnoreCase(names[i-1]));
			if (names[i].equalsIgnoreCase(names[i-1]) == false){
				v[count]=i;
				count++;
			}
		}  

		// IJ.log("Count="+count);  
		// arraycopy(Object src, int srcPos, Object dest, int destPos, int length) 
		int[] r;
		if (appendLastIndex == true){
			r  = new int[count+1];
			v[count] = names.length;
			System.arraycopy(v, 0, r, 0, count+1);
		} else {
			r = new int[count];
			System.arraycopy(v,0,r,0,count);
		}

		return r;
	}

	/**
	 * Similar to getImageStarts, but uses timestamps and interval (from
	 * vCfg.getKeyValueDouble("interval")) to determine the sequence 
	 * of cal files.  All this effort assumes that the OS returns the 
	 * cal files in the ascending order.
	 */
	private int[] getCalStarts(boolean appendLastIndex){
		vData.split();
		String[] timestamp = vData.getDataColumn("Timestamp");
		String fmt = guessDateFormat(timestamp[0]);
		long[] ts = stringToDate(timestamp, fmt);
		long dt = (long) (60 * 1000 * vCfg.getKeyValueLong("interval"));
		//long nIntervals = 1;
		//if (dt > 0) {nIntervals = (ts[ts.length-1]-ts[0])/dt;}
		int[] v = new int[ts.length];
		v[0] = 0;
		int count = 1;
		//IJ.log("dt="+dt);
		long start = ts[0];
		if (dt > 0 ){
			for (int i=1;i<ts.length;i++){
				//   IJ.log("ts["+i+"]="+ts[i]+ " >>> "+ dt*count);
				if ((ts[i] > (start + dt * count)) == true){
					v[count]=i;
					count++;
				}
			}    
		}    
		//IJ.log("CalCount=" +count); 
		// arraycopy(Object src, int srcPos, Object dest, int destPos, int length) 
		int[] r;
		if (appendLastIndex == true){
			r  = new int[count+1];
			v[count] = ts.length;
			System.arraycopy(v, 0, r, 0, count+1);
		} else {
			r = new int[count];
			System.arraycopy(v,0,r,0,count);
		}

		return r;    
	}//getCalStarts

	/**
	 * Guesses the correct date-time format string
	 */
	private String guessDateFormat(String t){

		String fmt = "yyyy-MM-dd HH:mm:ss.SSS";
		if (t.contains("T")) {
			fmt = "yyyy-MM-dd'T'HH:mm:ss.SSS";
		} else if (t.length() < 8){
			fmt = "mm:ss.s";
		} else if (t.length() < 12){
			fmt = "HH:mm:ss.s";
		}

		return fmt;
	} 

	/** Given a date-time string, convert to long integer of time. 
	 * @param t a date-time string of the format 2007-01-16 09:58:27.123 
	 *   (or without milliseconds)
	 * @param fmt the format string for the parser - see examples...
	 *   example 1: "2007-05-23 14:45:42.974" "yyyy-MM-dd HH:mm:ss.SSS"
	 *   example 2: "20:32.3"   "HH:mm:s"
	 *   example 3: "2009-01-29T13:22:17.046" "yyyy-MM-dd'T'HH:mm:ss.SSS"
	 * @return the number of milliseconds since January 1, 1970, 00:00:00 GMT 
	 */
	private long stringToDate(String t , String fmt){
		//2007-01-16 09:58:27.123   <<<< this is an example date-time string
		//String p = "yyyy-MM-dd HH:mm:ss";//pattern
		//if (t.length() != 19) { p = p + ".SSS";}        
		try {
			Date d = new SimpleDateFormat(fmt).parse(t);
			return d.getTime();
		} catch(Exception e){
			IJ.log("Error parsing ..." + t);
			return -1;
		}                       
	}//stringToDate 

	/**
	 * Same as for scalar input
	 */
	/** Given a date-time string, convert to long integer of time. 
	 * @param t a date-time string of the format 2007-01-16 09:58:27.123 
	 *   (or without milliseconds)
	 * @param fmt the format string for the parser - see examples...
	 *   example 1: "2007-05-23 14:45:42.974" "yyyy-MM-dd HH:mm:ss.SSS"
	 *   example 2: "20:32.3"   "HH:mm:s"
	 *   example 3: "2009-01-29T13:22:17.046" "yyyy-MM-dd'T'HH:mm:ss.SSS"
	 * @return the number of milliseconds since January 1, 1970, 00:00:00 GMT 
	 */
	private long[] stringToDate(String[] t , String fmt){
		//2007-01-16 09:58:27.123   <<<< this is an example date-time string
		//String p = "yyyy-MM-dd HH:mm:ss";//pattern
		//if (t.length() != 19) { p = p + ".SSS";}     
		long[] time = new long[t.length];
		Date d;
		for (int i = 0; i<t.length; i++){
			try {
				d = new SimpleDateFormat(fmt).parse(t[i]);
				time[i] = d.getTime();
			} catch(Exception e){
				time[i] = (long) -1;
			} 
		}
		return time;                     
	}//stringToDate 

	/**
	 * Loads the data from the user selected data file and test
	 * to make sure the minimum fields are available.  These include:
	 * "CaptureX","CaptureY","SaveX","SaveY","PixelW","PixelH","Filename"
	 **/
	private boolean loadExportData(){
		//load the data into a ZPIvData
		this.vData = new ZPIvData(dir + exportFilename, ",");
		//TextWindow tw = vData.showInTextWindow("Data");
		//these are required    
		String[] mustHave = {"CaptureX","CaptureY","SaveX","SaveY","PixelW","PixelH",
				"Filename", "Timestamp"};
		//get the fieldnames (and join them to one string)
		String fnames = vData.join(vData.getFieldnames());
		//use string tests 
		for (int i = 0; i< mustHave.length; i++){
			if (fnames.contains(mustHave[i]) == false){
				IJ.showMessage("Export data file is missing: "  + mustHave[i]);
				return false;
			}
		}

		return true;
	}// end of loadExportData

	private boolean showDialog(){

		if (exportFilename.length() == 0) {
			OpenDialog od = new OpenDialog("Select FlowCAM export file", null);
			dir = od.getDirectory();
			exportFilename = od.getFileName();
			if (exportFilename == null) {return false;}
			if (exportFilename.length() == 0){return false;}
		}

		GenericDialog gd = new GenericDialog("FITVIS_");
		gd.addNumericField("Interval for calibration",vCfg.getKeyValueDouble("interval"),0); 
		gd.addNumericField("PixelSize", vCfg.getKeyValueDouble("pixelSize"), 4);
		gd.addNumericField("MinSize um", vCfg.getKeyValueDouble("minSize"),2);
		gd.addNumericField("MaxSize um", vCfg.getKeyValueDouble("maxSize"),2);
		gd.addNumericField("Distance to neighbor um", vCfg.getKeyValueDouble("distance"),2);
		String[] use = {"Dark", "Light", "Both"};
		gd.addChoice("Use pixels", use, vCfg.getKeyValue("usePixels"));
		gd.addNumericField("ThresholdDark", 
				vCfg.getKeyValueDouble("thresholdDark"),0);
		gd.addNumericField("ThresholdLight", 
				vCfg.getKeyValueDouble("thresholdLight"),0);

		String[] labels = {"Fill", "Largest object only", "Vignettes saved",
				"Scalebar on vignettes", "Enhance vignettes", "Outline vignettes",
				"Masks saved", "Log filter", "Verbose mode", "Batch mode", "Archive raw"};
		boolean[] values = { 
				vCfg.getKeyValueBoolean("fill"),
				vCfg.getKeyValueBoolean("largest"),
				vCfg.getKeyValueBoolean("vignettes"),
				vCfg.getKeyValueBoolean("scalebar"),
				vCfg.getKeyValueBoolean("enhance"),
				vCfg.getKeyValueBoolean("outline"),
				vCfg.getKeyValueBoolean("masks"),
				vCfg.getKeyValueBoolean("log"),
				vCfg.getKeyValueBoolean("verbose"),
				vCfg.getKeyValueBoolean("batch"),
				vCfg.getKeyValueBoolean("archive")};
		int nrow = labels.length/2 + (labels.length % 2); 
		gd.addCheckboxGroup(nrow, 2, labels, values);

		gd.showDialog();

		boolean bOK = gd.wasCanceled();
		if ( bOK == true) {return false;}

		//dir = gd.getNextString();  
		bOK = vCfg.setKeyValue("interval", gd.getNextNumber());  
		bOK = vCfg.setKeyValue("pixelSize", gd.getNextNumber());
		bOK = vCfg.setKeyValue("minSize", gd.getNextNumber());
		bOK = vCfg.setKeyValue("maxSize", gd.getNextNumber());
		bOK = vCfg.setKeyValue("distance", gd.getNextNumber());
		bOK = vCfg.setKeyValue("usePixels", gd.getNextChoice());
		bOK = vCfg.setKeyValue("thresholdDark", gd.getNextNumber());
		bOK = vCfg.setKeyValue("thresholdLight", gd.getNextNumber());
		bOK = vCfg.setKeyValue("fill", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("largest", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("vignettes", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("scalebar", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("enhance", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("outline", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("masks", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("log", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("verbose", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("batch", gd.getNextBoolean());
		bOK = vCfg.setKeyValue("archive", gd.getNextBoolean());        
		return true; 
	}



	/**
	 * Performs the logscale filtering
	 * step1 = LOG(inverse(subImage)) - LOG(inverse(calImage))
	 * step2 = ANTILOG(step1)
	 * @arg arg A string of the "colID calID" from the macro
	 **/
	public static String doLogscaleFilter(String arg){

		String[] s = arg.split(" ");
		int sID = (new Integer(s[0])).intValue();//subimageID
		int cID = (new Integer(s[0])).intValue();//calibrationID

		ImagePlus sImp = WindowManager.getImage(sID);
		String sName = sImp.getTitle();
		IJ.selectWindow(sID);
		//IJ.run("32-bit");
		ImageProcessor sIp = sImp.getProcessor();  
		sIp.invert();
		//sIp = sIp.convertToFloat();

		ImagePlus cImp = WindowManager.getImage(cID);
		String cName = cImp.getTitle(); 
		ImageProcessor cIp = cImp.getProcessor(); 
		cIp.invert(); 
		//cIp = sIp.convertToFloat();

		sIp.log();
		cIp.log();
		for (int x = 0; x<cImp.getWidth(); x++){
			for (int y = 0; y<cImp.getHeight(); y++){
				sIp.putPixelValue(x,y, sIp.getPixelValue(x,y) - cIp.getPixelValue(x,y));
			}
		}

		sIp.exp();
		//sImp.setProcessor(sName,sIp.convertToByte(false));
		sImp.updateAndDraw();

		return("true"); 
	}

	/**
	 * Returns the results table column headings as a tab delimited
	 * string.  This may be called from a macro with 
	 * result = call("FITVIS_.getResultsHeadings()"); 
	 * Note that the first is the label which may be a single space - this
	 * is clipped before returning.
	 * @return A string of the results table headings.
	 */
	public static String getResultsHeadings(){
		ResultsTable rt = Analyzer.getResultsTable();
		String s = rt.getColumnHeadings();
		int clip = s.indexOf("\t");
		return s.substring(clip+1);
	}


	/**
	 * accepts the maximum separation distance (in pixels) for particles to be considered
	 * as one.  The RoiManager must be populated with the candidate particles
	 * 
	 * @ return the number of combined objects
	 **/ 
	public static String analyzeParticleSeparation(String args){
		// maxDistance, originX, originY
		String[] s = args.split("\t");
		double maxD = Tools.parseDouble(s[0]);
		int ox = (int) Tools.parseDouble(s[1]); //origin of image
		int oy = (int) Tools.parseDouble(s[2]);
		RoiManager rm = RoiManager.getInstance();
		int nRoi = rm.getCount();
		if (nRoi == 1) {
			return "1";
		}
		Roi[] rois = rm.getRoisAsArray();
		PolygonRoi p0 = new PolygonRoi(rois[0].getPolygon(), rois[0].POLYGON);
		PolygonRoi p1 = new PolygonRoi(rois[0].getPolygon(), rois[0].POLYGON);;
		//an array to keep track of connections
		double[][] dist = new double[nRoi][nRoi];
		int[][] flag = new int[nRoi][nRoi];
		for (int i = 1; i< nRoi; i++){
			for (int j = 0; j<nRoi; i++){
				dist[i][j] = -1.0;
				flag[i][j] = -1;
			}
		} 
		int ngroup = 0;
		int bx0; //bounding box coords relative to image
		int by0;
		int bx1;
		int by1;
		int[] x0; //relative to bounding box
		int[] y0;
		int[] x1; // the coordinates of the second polygon
		int[] y1; // relative to ROIs bounding box
		Rectangle r; //used to get the bounds 
		for (int i = 0; i<nRoi; i++){
			r = rois[i].getBounds();
			bx0 =  (int) r.getX() + ox;
			by0 =  (int) r.getY() + oy;
			p0 = new PolygonRoi(rois[i].getPolygon(), rois[i].POLYGON); 
			x0 =  p0.getXCoordinates();
			y0 =  p0.getYCoordinates();
			for (int j = i; j < nRoi; j++){
				if (j != i) {
					r = rois[j].getBounds();
					bx1 =  (int) r.getX() + ox;
					by1 =  (int) r.getY() + oy;
					p1 = new PolygonRoi(rois[j].getPolygon(), rois[j].POLYGON); 
					x1 =  p1.getXCoordinates();
					y1 =  p1.getYCoordinates();          
					//          x1 = (double[]) rois[j].getXCoordinates();
					//          y1 = (double[]) rois[j].getYCoordinates();
					dist[i][j] = computeSmallestDistance(x0,y0,x1,y1, bx0,by0, bx1, by1);
				}
			}

		} // i-loop through Rois

		return "done";
	}// analyzeParticleSeparation

	/**
	 * Calculates the smallest distance between two polygons by testing the 
	 * vertices of the first agaunst each of the second indices.
	 * @param x0 the x position of polygon0 vertices relative to bx0
	 * @param y0 the y position of polygon0 vertices relative to by0
	 * @param x1 the x position of polygon1 vertices relative to bx1
	 * @param y1 the y position of polygon1 vertices relative to by1
	 * @param bx0 the x origin of the polygon0
	 * @param by0 the y origin of the polygon0
	 * @param bx1 the x origin of the polygon1
	 * @param by1 the y origin of the polygon1
	 * @return the smallest distance between the two polygons
	 */     
	private static double computeSmallestDistance(int[] x0, int[] y0, int[] x1, int[] y1, int bx0, int by0, int bx1, int by1){
		int d = 99999999; //some very high starting point
		int dx; //the x displacement
		int dy; //the y displacement
		int d2; //a temporary variable
		for (int i = 0; i < x0.length ; i++){
			for (int j = 0; j < x1.length; j++){
				dx = ( x0[i] + bx0) - (x1[j] + bx1);
				dy = (y0[i] + by0) - (y1[j] + by1);
				d2 = dx*dx + dy*dy;
				if (d2 < d) { d = d2;}
			}
		}
		return Math.sqrt((double) d);
	}
} //FITVIS_ class