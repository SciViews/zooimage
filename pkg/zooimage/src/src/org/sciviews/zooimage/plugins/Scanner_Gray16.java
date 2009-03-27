package org.sciviews.zooimage.plugins ;

import ij.Prefs;
import ij.gui.GenericDialog;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileUtilities;


// TO DO:
// - Save the ROI file
//private ROIStore rois = new ROIStore();
//ResultsTable rt = Analyzer.getResultsTable();
//rois.addParticles(WindowManager.getCurrentImage(),rt,"ROI",0,0); // assuming the current window is the mask
//String Name = source_file + "_roi.zip";
//rois.saveRoiZip(Directory+Name);
// - When stop on the combined picture, zoom at 100% + activate the hand tool
// - Is it possible to position the log window at the right of the screen and to save its position?

/**
  * ZooImage1 process images and analyze particle for flatbed scanner with 16bit graylevels.
  * @author Ph. Grosjean, 2005 (phgrosjean@sciviews.org)
  * @version 1.1-0.
  */
public class Scanner_Gray16 extends ZooImagePlugin {

	
	public Scanner_Gray16( ){
		super();
		calibration.setPixsize(0.01058) ;
		calibration.setMinsize(0.25) ;
		calibration.setMaxsize(10) ;
		
		options.set("maskFromVIS", true) ;
		options.set("useOD", true );
		options.set("ziptiff", Prefs.get("ZI1.Scan16g.boolean", false) ) ;
		
	}
	
	@Deprecated
	@Override
	public boolean showGUI_processZimFile(){

		// Show the dialog box
		GenericDialog gd = new GenericDialog("ZooImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir);
		gd.addNumericField("Min. equiv. diam. (" + calibration.getPixelunit() + "):", calibration.getMinsize(), 3);
		gd.addNumericField("Max. equiv. diam. (" + calibration.getPixelunit() + "):", calibration.getMaxsize(), 3);
		gd.addCheckbox("Zip TIFF images", options.get("ziptiff" ) );

		gd.addCheckbox("Analyze particles", options.get("analyzepart"));
		gd.addCheckbox("Make vignettes", options.get("makevigs"));
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs"));
		gd.addCheckbox("Show outlined objects", options.get("showoutline") );

		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		options.set("allfiles", gd.getNextBoolean() );
		remotedir = FileUtilities.fsepDir(gd.getNextString());
		calibration.setMinsize( (double)gd.getNextNumber() );
		calibration.setMaxsize( (double)gd.getNextNumber() );
		
		options.set("ziptiff", gd.getNextBoolean() );
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );
	
		// Make sure that the directory provided exists
		if (!FileUtilities.dirExists(remotedir) ) { 
			Log.error("The directory " + remotedir + " is invalid!");
			return(false);
		}
		
		// Save the configuration
		Prefs.set("ZI1.Scan16g.boolean", options.get("allfiles" ));
		Prefs.set("ZI1.Scan16g.boolean", options.get("ziptiff" ) );
		Prefs.set("ZI1.Scan16g.boolean", options.get("analyzepart"));
		Prefs.set("ZI1.Scan16g.boolean", options.get("makevigs"));
		Prefs.set("ZI1.Scan16g.boolean", options.get("sharpenvigs"));
		Prefs.set("ZI1.Scan16g.boolean", options.get("showoutline"));
		return(true);
 	}
	
}
