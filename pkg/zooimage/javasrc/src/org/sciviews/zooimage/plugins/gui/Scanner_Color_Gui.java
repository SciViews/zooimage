package org.sciviews.zooimage.plugins.gui;

import ij.Prefs;
import ij.gui.GenericDialog;

import java.io.File;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.log.Log;

public class Scanner_Color_Gui extends ZooImagePluginGui {

	public Scanner_Color_Gui(ZooImagePlugin plugin) {
		super(plugin);
	}

	@Override
	public boolean zimOptionsDialog() {
		// Show the dialog box
		GenericDialog gd = new GenericDialog("ZooImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir.getAbsolutePath());
		gd.addChoice("Parameters set:", plugin.getMethods(), calibration.getMethod() );
		gd.addChoice("Calibration set:", calibration.getCalibs(), calibration.getCalib() );

		gd.addCheckbox("Analyze particles", options.get("analyzepart") );
		gd.addCheckbox("Make vignettes", options.get("makevigs" ));
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs" ));
		gd.addCheckbox("Show outlined objects", options.get("showoutline") );

		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		options.set("allfiles", gd.getNextBoolean() );

		remotedir = new File(gd.getNextString());
		// Make sure that the directory provided exists
		if (!remotedir.exists() || !remotedir.isDirectory()){
			Log.error("The directory " + remotedir + " in invalid!");
			return(false);
		}


		int i1Index = gd.getNextChoiceIndex();
		int i2Index = gd.getNextChoiceIndex();
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );

		
		// Recalculate method (should be done dynamically)!
		calibration.setMethod( plugin.getMethods()[i1Index] );
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
