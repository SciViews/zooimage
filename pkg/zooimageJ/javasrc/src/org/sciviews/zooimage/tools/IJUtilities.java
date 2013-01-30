package org.sciviews.zooimage.tools;

import ij.IJ;
import ij.ImagePlus;
import ij.WindowManager;
import ij.gui.ImageWindow;
import ij.io.OpenDialog;

import java.awt.Frame;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.log.Log;

public class IJUtilities {

	/**
	 * A slightly modified version of IJ.selectWindow() which indicates if the window exists or not
	 * @see IJ#selectWindow(String)
	 */
	public static boolean selectWinAndCheck(String title) {
		
		long start = System.currentTimeMillis();
		
		// 4 sec timeout
		while (System.currentTimeMillis() - start < 4000) { 
			Frame frame = WindowManager.getFrame(title);
			if (frame != null && !(frame instanceof ImageWindow)) {
				//selectWindow(frame);
				return(false); // Because this is not an image
			}
			int[] wList = WindowManager.getIDList();
			int len = wList != null?wList.length:0;
			for (int i = 0; i < len; i++) {
				ImagePlus imp = WindowManager.getImage(wList[i]);
				if (imp != null) {
					if (imp.getTitle().equals(title)) {
						IJ.selectWindow(imp.getID());
						return(true);
					}
				}
			}
			IJ.wait(10);
		}
		return(false);
	}

	/**
	 * Opens an image
	 * The convention is the following one:
	 * If ImgName != "" and it exists, then activate it. Otherwise:
	 * "myimage" => tries to open it (check if it exists first)
	 * "" => shows a dialog box to choose an image
	 * "-" => do nothing and return always false
	 * ImgName changes the name of the image if != "", and check if it is the current image
	 * @param ImgName image name
	 * @param ImgFile image file
	 * @return
	 */
	public static boolean openImg(String ImgName, String ImgFile) {
		
		if (ImgName != "") {
			if (selectWinAndCheck(ImgName)) return(true);
		}
		
		if (ImgFile == "-") return(false);
		
		if (ImgFile == "") {
			OpenDialog od = new OpenDialog("Open '" + ImgName + "' image", "");
			if (od.getFileName() == null) return(false);
			ImgFile = od.getFileName();
		}
		
		// Look if this image exists
		if (!FileUtilities.fileExists(ImgFile)) return(false);
		
		// Open it and change its name to ImgName
		IJ.run("Open...", "path='" + ImgFile + "'");
		if (ImgName != "") {
			IJ.getImage().setTitle(ImgName);
			return(selectWinAndCheck(ImgName));
		} else {
			return(true);
		}
	}
	
	/**
	 * Make sure to close all images that possibly remain opened
	 * We don't use closeAllWindows() here, because we want to *force* closing of all images!
	 */
	public static void closeAllImages(){ 
		Log.debug( "Closing all images" ) ; 
		while (WindowManager.getImageCount() > 0) {
			IJ.getImage();
			IJ.run("Close");
		}
		Log.debug( "   ... ok" ) ; 
		
	}

	/**
	 * Check that ImageJ is in a correct state for the process to run
	 * 
	 * @return true if ImageJ is in a good state to perform the analysis
	 */
	public static boolean verifyState( ZooImagePlugin plugin) {

		// Test ImageJ version
		if (IJ.versionLessThan( ZooImagePlugin.minversion )) {
			Log.showStatus("--- ERROR! ---");
			return(false);
		}
		
		// Test memory configuration
		if (IJ.maxMemory() < (int)( ZooImagePlugin.needmemory * 1000000)) {
			Log.error( plugin.getDescriptor().getName() + " needs " + ZooImagePlugin.needmemory + " Mb of RAM memory!\n"+
				     "Please, use a computer with more than that amount of RAM memory and\n"+
				     "change the memory option in ImageJ (Edit -> Options -> Memory)\n"+
				     "to " + ZooImagePlugin.needmemory + " Mb. You have to restart ImageJ for these to take effect!");
			Log.showStatus("--- ERROR! ---");
			return(false);
		}
		
		// Close all images first (and possibly ask to save them)
		WindowManager.closeAllWindows();
		
		// If there are images opened taking too much memory, ask to close them
		IJ.freeMemory();
		if ((IJ.maxMemory() - IJ.currentMemory()) < (int)(ZooImagePlugin.needmemory * 0.95 * 1000000) ||
			 WindowManager.getImageCount() > 0) {
			Log.error( plugin.getDescriptor().getName() + " needs all available RAM memory!\n"+
				     "Please, close all opened images. If this message\n"+
				     "shows again, restart ImageJ to make sure you really\n"+
				     "have all memory available for ZooImage process!\n");
			Log.showStatus("--- INTERRUPTED! ---");
			return(false);
		}
		return(true);
	}

	
}
