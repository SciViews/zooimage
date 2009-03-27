package org.sciviews.zooimage.plugins ;

import ij.Prefs;
import ij.gui.GenericDialog;

import java.util.Vector;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.FileUtilities;
import org.sciviews.zooimage.tools.Threshold;


// TO DO:
// - processRAW: include metadata as comment in the zip file!!!
// - Dialog box and arguments to account for spatial calibration:
//     the length of one pixel in mm
//   These arguments as saved as global preferences
// - A simplified procedure to calibrate lengths
// - Save and restore parameters of the analysis in Prefs, or private config files
// - Compare both _dat1.txt to decide if one has to make vignettes or not
// - Stop the whole process if too many errors
// - When stop on the combined picture, zoom at 100% + activate the hand tool
// - Is it possible to position the log window at the right of the screen and to save its position?


/**
 * PhytoImage1 process images and analyze particle for microscope images taken with a digital camera.
 * @author Ph. Grosjean (phgrosjean@sciviews.org)
 * @version 1.0-0.
 */
public class Microscope_Color extends ZooImagePlugin {

  // The following list should be constructed dynamically... only add more than one item if you need several different treatments
	
	protected static String[] methods = {"default", "alternate"};
	
	/**
	 * Threshold data
	 * Contains edgethreshold,darkthreshold, lightthreshold, contrastsat 
	 */
	private Threshold thresholds ;
	
	/**
	 * Constructor
	 */
	public Microscope_Color( ){
		super( ) ;
		calibration.setPixsize( 0.1 ) ;
		calibration.setMinsize( 20 ) ;
		calibration.setMaxsize( 10000 ) ;
		calibration.setWhitepoint(0) ;
		calibration.setBlackpoint(0) ;
		calibration.setPixelunit("�m") ;
		
		options.set("maskfromVIS", true) ;
		options.set("useVIS", false ) ;
		options.set("useOD", false ) ;
		options.set("ziptiff", false ) ;
		
		thresholds = new Threshold( ) ;
		
	}
	
	@Override
	public String getRawImageExtension() {
		return FileExtensions.extJPG ;
	}
	
	@Override
	protected Vector<String> getParams(){
		Vector<String> parameters = super.getParams() ;
		thresholds.fill( parameters ) ;
		return parameters ;
	}

	@Deprecated
	@Override
	public boolean showGUI_processZimFile(){
	
		// Show the dialog box
		GenericDialog gd = new GenericDialog("PhytoImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir);

		gd.addCheckbox("Analyze particles", options.get("analyzepart") );
		gd.addCheckbox("Make vignettes", options.get("makevigs") );
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs") );
		gd.addCheckbox("Show outlined objects", options.get("showoutline") );
		
		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		options.set("allfiles", gd.getNextBoolean() );
		remotedir = FileUtilities.fsepDir(gd.getNextString());

		options.set("ziptiff", false );
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );
		
		// Make sure that the directory provided exists
		if( ! FileUtilities.dirExists(remotedir) ){
			Log.error("The directory " + remotedir + " in invalid!");
			return(false);
		}
		
		Prefs.set("ZI1.boolean", options.get("allfiles") );
		Prefs.set("ZI1.string", calibration.getMethod() );
		Prefs.set("ZI1.boolean", options.get("analyzepart") );
		Prefs.set("ZI1.boolean", options.get("makevigs") );
		Prefs.set("ZI1.boolean", options.get("sharpenvigs") );
		Prefs.set("ZI1.boolean", options.get("showoutline") );
		return(true);
 	}

	/**
	 * @param thresholds the thresholds to set
	 */
	public void setThresholds(Threshold thresholds) {
		this.thresholds = thresholds;
	}

	/**
	 * @return the thresholds
	 */
	public Threshold getThresholds() {
		return thresholds;
	}

	
}
