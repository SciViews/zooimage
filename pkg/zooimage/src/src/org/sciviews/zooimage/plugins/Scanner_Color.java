package org.sciviews.zooimage.plugins ;

import ij.Prefs;
import ij.gui.GenericDialog;

import java.util.Vector;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.FileUtilities;
import org.sciviews.zooimage.tools.RGB;


// TO DO:
// - A better way to merge RGB
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
 * ZooImage1 process images and analyze particle for flatbed scanner pictures (PVA mode).
 * @author Ph. Grosjean (phgrosjean@sciviews.org), G. Boyra & X. Irigoien
 * @version 1.0-0 
 */
public class Scanner_Color extends ZooImagePlugin {


	/**
	 * The following list should be constructed dynamically... 
	 * only add more than one item if you need several different treatments
	 */
	protected static String[] methods = {"default [0.75 - 10]", "wide spectrum [0.25 - 20]"};
	
	/**
	 * The staining used
	 */
	private String staining = "haematoxilyn";
	
	/**
	 * Red, green, blue and threshold parameters
	 */
	private RGB rgb ;
	
	/**
	 * Constructor
	 */
	public Scanner_Color(){
		super();
		
		calibration.setPixsize(0.04233) ;
		calibration.setCalibs(new String[]{"Haematoxilyn", "Eosin"}) ;
		calibration.setMinsize(0.75) ;
		calibration.setMaxsize(10) ;
		calibration.setMethod( Prefs.get("ZI1.string", "default [0.75 - 10]") ) ;
		calibration.setCalib( Prefs.get("ZI1.string", "Haematoxilyn") ) ;
		
		options.set("maskfromVIS", true) ;
		options.set("useOD", false ) ;
		options.set("ziptiff", false ) ;
		
		rgb = new RGB( 0.1, 0.8, 0.9, 125 ) ;
		
	}
	
	public RGB getRgb() {
		return rgb;
	}
	
	@Override
	public String getRawImageExtension(){
		return FileExtensions.extJPG ;
	}
	
	@Override
	protected Vector<String> getParams(){
		Vector<String> parameters = super.getParams() ;
		parameters.add(	"Staining=" + staining ) ;
		rgb.fill( parameters ) ;
		return(parameters);
	}

	@Deprecated
	@Override
	public boolean showGUI_processZimFile(){

		// Show the dialog box
		GenericDialog gd = new GenericDialog("ZooImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir);
		gd.addChoice("Parameters set:", methods, calibration.getMethod() );
		gd.addChoice("Calibration set:", calibration.getCalibs(), calibration.getCalib() );
		
		gd.addCheckbox("Analyze particles", options.get("analyzepart") );
		gd.addCheckbox("Make vignettes", options.get("makevigs" ));
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs" ));
		gd.addCheckbox("Show outlined objects", options.get("showoutline") );
		
		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		options.set("allfiles", gd.getNextBoolean() );
		
		remotedir = FileUtilities.fsepDir(gd.getNextString());
		
		int i1Index = gd.getNextChoiceIndex();
		int i2Index = gd.getNextChoiceIndex();
		//iterations = (int) gd.getNextNumber();
		//options.set("ziptiff", gd.getNextBoolean() );
		
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );
		
		// Make sure that the directory provided exists
		if (!FileUtilities.dirExists(remotedir)){
			Log.error("The directory " + remotedir + " in invalid!");
			return(false);
		}
		
		// Recalculate method (should be done dynamically)!
		calibration.setMethod( methods[i1Index] );
		if (i1Index == 1) {
			// Give a different set of values... just to demonstrate!
			calibration.setMinsize(0.25) ;
			calibration.setMaxsize(20) ; 
			options.set("useOD", false );
		}
		
		// Recalculate calibration (should be done dynamically)!
		// Here it is just to demonstrate... keep same parameters
		calibration.setCalib( calibration.getCalib(i2Index) ) ;
		
		// Should get calibration values from the given file!
		// ...
		// Save the configuration
		Prefs.set("ZI1.boolean", options.get("allfiles" ));
		Prefs.set("ZI1.string", calibration.getMethod() );
		Prefs.set("ZI1.string", calibration.getCalib() );
		Prefs.set("ZI1.boolean", options.get("ziptiff"));
		Prefs.set("ZI1.boolean", options.get("analyzepart") );
		Prefs.set("ZI1.boolean", options.get("makevigs"));
		Prefs.set("ZI1.boolean", options.get("sharpenvigs"));
		Prefs.set("ZI1.boolean", options.get("showoutline"));
		return(true);
 	}
	
}
