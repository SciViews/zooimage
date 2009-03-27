package org.sciviews.zooimage.tools;

import java.io.File;

/**
 * Utility methods to deal with vignettes
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class VignetteUtilities {

	/**
	 * Delete the 'sample' subdir of basedir and all files (vignettes) in it
	 * @param basedir base directory
	 * @param sample sample directory
	 * @return
	 */
	public static boolean clearVignettes(String basedir, String sample) {
		// Note: files deletion is not recursive => it fails to delete the directory
		// if there are subdirectories in it!
		String dir = basedir + sample;
		// Check if this directory exists
		File dirFile = new File(dir);
		boolean success = true;
		if (dirFile.exists() && dirFile.isDirectory()) {
			// Delete all files in this dir
			String[] flist = dirFile.list();
			if (flist != null) {
				for (int i = 0; i < flist.length; i++) {
					success = ((new File(dir + File.separator + flist[i])).delete());
				}
			}
			success = dirFile.delete();
		} else {
			if (dirFile.exists()) {
				success = false;
			}
		}
		return(success);
	}
	
	/**
	 * makes a vignette
	 * @param ImgName
	 * @param rt
	 * @param i
	 * @param vigFile
	 * @return true if the vignette was successfully created
	 */

	
	
}
