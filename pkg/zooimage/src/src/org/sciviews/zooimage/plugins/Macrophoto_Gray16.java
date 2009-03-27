package org.sciviews.zooimage.plugins ;

import ij.Prefs;
import ij.gui.GenericDialog;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileUtilities;

// TO DO:
// - Stop the whole process if too many errors
// - When stop on the combined picture, zoom at 100% + activate the hand tool
// - Is it possible to position the log window at the right of the screen and to save its position?


/**
  * ZooImage1 process images and analyze particle for 16bit grayscale images coming
  * from digital cameras and converted from RAW files to zipped tif pictures.
  * @author Ph. Grosjean, 2006-2007 (phgrosjean@sciviews.org)
  * @version 1.1-0.
  */
public class Macrophoto_Gray16 extends ZooImagePlugin {

	// The following list should be constructed dynamically... only add more than one item if you need several different treatments
	protected static String[] methods = {"default [0.25 - 10]", "wide spectrum [0.15 - 20]"};
	
	/**
	 * Constructor for the Macrophoto_Gray16 plugin
	 */
	public Macrophoto_Gray16(){
		
		super() ;
		
		// set various calibration options
		calibration.setCalibs( new String[] { 
			"Default (from zim file)", "Custom values"
		} ) ;
		calibration.setCalib( Prefs.get("ZI1.string", "Canon 20D - 100mm") ) ;
		calibration.setMinsize(0.25) ;
		calibration.setMaxsize(10) ;
		calibration.setMethod( Prefs.get("ZI1.string", "default [0.25 - 10]") ) ;
		
		// set various process options
		options.set( "useOD", true ) ;
		
	}
	
	
	
	@Override
	@Deprecated
	public boolean showGUI_processZimFile(){
		
		// Show the dialog box
		GenericDialog gd = new GenericDialog("ZooImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir);
		gd.addChoice("Parameters set:", methods, calibration.getMethod() );
		gd.addCheckbox("Analyze particles", options.get("analyzepart" ) );
		gd.addCheckbox("Make vignettes", options.get("makevigs" ) );
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs") );
		gd.addCheckbox("Show outlined objects", options.get("showoutline" ) );
		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		
		options.set("allfiles", gd.getNextBoolean() );
		remotedir = FileUtilities.fsepDir(gd.getNextString());
		int i1Index = gd.getNextChoiceIndex();
		int i2Index = 0; // gd.getNextChoiceIndex();
		options.set( "ziptiff", false ) ; 
		options.set( "analyzepart", gd.getNextBoolean() );
		options.set( "makevigs", gd.getNextBoolean() );
		options.set( "sharpenvigs", gd.getNextBoolean() );
		options.set( "showoutline", gd.getNextBoolean() );
		
		// Make sure that the directory provided exists
		if (!FileUtilities.dirExists(remotedir)) {
			Log.error("The directory " + remotedir + " is invalid!");
			return(false);
		}
		
		// Recalculate method (should be done dynamically)!
		calibration.setMethod( methods[i1Index] );
		if (i1Index == 1) {
			// Give a different set of values... just to demonstrate!
			calibration.setMinsize(0.15) ;
			calibration.setMaxsize(20) ;
			options.set( "useOD", true );
		}

		// Recalculate calibration (should be done dynamically)!
		// Here it is just to demonstrate... keep same parameters
		calibration.setCalib( calibration.getCalib(i2Index) );
		
		// Should get calibration values from the given file!
		// Save the configuration
		Prefs.set("ZI1.string", calibration.getMethod() );
		Prefs.set("ZI1.string", calibration.getCalib());
		
		Prefs.set("ZI1.boolean", options.get("allfiles") );
		Prefs.set("ZI1.boolean", options.get("analyzepart") );
		Prefs.set("ZI1.boolean", options.get("makevigs"));
		Prefs.set("ZI1.boolean", options.get("sharpenvigs") );
		Prefs.set("ZI1.boolean", options.get("showoutline") );
		
		return(true);
 	}

}
