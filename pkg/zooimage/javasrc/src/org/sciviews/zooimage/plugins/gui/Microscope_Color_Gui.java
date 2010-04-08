package org.sciviews.zooimage.plugins.gui;

import ij.Prefs;
import ij.gui.GenericDialog;

import java.io.File;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.ZooImagePluginGui;
import org.sciviews.zooimage.log.Log;

public class Microscope_Color_Gui extends ZooImagePluginGui {

	public Microscope_Color_Gui(ZooImagePlugin plugin) {
		super(plugin) ;
	}

	@Override
	public boolean zimOptionsDialog() {
		// Show the dialog box
		GenericDialog gd = new GenericDialog("PhytoImage1 Image Processor");
		gd.addMessage("Selected item: " + zimfile);
		gd.addCheckbox("Process all items in this directory?", options.get("allfiles") );
		gd.addStringField("Read from directory: ", remotedir.getAbsolutePath() );
		gd.addCheckbox("Analyze particles", options.get("analyzepart") );
		gd.addCheckbox("Make vignettes", options.get("makevigs") );
		gd.addCheckbox("Sharpen vignettes", options.get("sharpenvigs") );
		gd.addCheckbox("Show outlined objects", options.get("showoutline") );
		
		gd.showDialog();
		if (gd.wasCanceled()) return(false);
		options.set("allfiles", gd.getNextBoolean() );
		remotedir = new File( gd.getNextString());
		
		options.set("ziptiff", false );
		options.set("analyzepart", gd.getNextBoolean() );
		options.set("makevigs", gd.getNextBoolean() );
		options.set("sharpenvigs", gd.getNextBoolean() );
		options.set("showoutline", gd.getNextBoolean() );
		
		// Make sure that the directory provided exists
		if( ! remotedir.exists() || !remotedir.isDirectory() ){
			Log.error("The directory " + remotedir + " in invalid!");
			return(false);
		}
		
		Prefs.set("ZI1.boolean", options.get("allfiles") );
		
		Prefs.set("ZI1.string", calibration.getMethod() ); // problem
		Log.log("10"); 
		
		Prefs.set("ZI1.boolean", options.get("analyzepart") );
		Log.log("11"); 
		Prefs.set("ZI1.boolean", options.get("makevigs") );
		Log.log("12"); 
		Prefs.set("ZI1.boolean", options.get("sharpenvigs") );
		Log.log("13"); 
		Prefs.set("ZI1.boolean", options.get("showoutline") );
		Log.log("14"); 
		
		return(true);

	}

}
