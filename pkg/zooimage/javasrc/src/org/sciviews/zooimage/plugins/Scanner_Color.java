package org.sciviews.zooimage.plugins ;

import ij.Prefs;

import java.util.Vector;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.plugins.gui.Scanner_Color_Gui;
import org.sciviews.zooimage.tools.FileExtensions;
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
	
	@Override
	public ZooImagePluginGui getGui() {
		return new Scanner_Color_Gui( this );
	}

	@Override
	public String[] getMethods() {
		return methods ;
	}
	
}
