package org.sciviews.zooimage.plugins.gui;

import ij.Prefs;
import ij.gui.GenericDialog;

import java.io.File;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.log.Log;

public class Scanner_Gray16_Gui extends ZooImagePluginGui {

	public Scanner_Gray16_Gui(ZooImagePlugin plugin) {
		super(plugin);
	}

	@Override
	public boolean zimOptionsDialog() {

		// Show the dialog box
		GenericDialog gd = new GenericDialog("ZooImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir.getAbsolutePath());
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
		remotedir = new File( gd.getNextString() );
		calibration.setMinsize( (double)gd.getNextNumber() );
		calibration.setMaxsize( (double)gd.getNextNumber() );

		options.set("ziptiff", gd.getNextBoolean() );
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );

		// Make sure that the directory provided exists
		if (!remotedir.exists() || !remotedir.isDirectory() ) { 
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
