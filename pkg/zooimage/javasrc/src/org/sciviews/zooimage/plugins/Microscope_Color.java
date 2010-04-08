package org.sciviews.zooimage.plugins ;

import java.util.Vector;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.plugins.gui.Microscope_Color_Gui;
import org.sciviews.zooimage.tools.FileExtensions;
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
		calibration.setMethod("default") ;
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

	@Override
	public ZooImagePluginGui getGui() {
		return new Microscope_Color_Gui(this);
	}
	
// The following list should be constructed dynamically... only add more than one item if you need several different treatments
	
	protected static String[] methods = {"default", "alternate"};
	
	public String[] getMethods(){
		return methods ;
	}
	
}
