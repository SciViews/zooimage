package org.sciviews.zooimage.plugins ;

import ij.Prefs;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.plugins.gui.Scanner_Gray16_Gui;


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

	@Override
	public ZooImagePluginGui getGui() {
		return new Scanner_Gray16_Gui( this );
	}
	
}
