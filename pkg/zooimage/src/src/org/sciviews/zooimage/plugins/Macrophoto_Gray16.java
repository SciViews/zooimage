package org.sciviews.zooimage.plugins ;

import ij.Prefs;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.plugins.gui.Macrophoto_Gray16_Gui;

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
	public ZooImagePluginGui getGui() {
		return new Macrophoto_Gray16_Gui(this);
	}
	
}
