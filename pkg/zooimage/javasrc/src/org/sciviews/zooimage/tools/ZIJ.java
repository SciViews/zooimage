package org.sciviews.zooimage.tools;

import ij.IJ;
import ij.ImagePlus;
import ij.measure.Calibration;
import ij.process.ImageProcessor;

import org.sciviews.zooimage.log.Log;

/**
 * Convenience wrapper to work around ImageJ reliance on
 * the window manager 
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class ZIJ {

	private static ImagePlus image ;
	
	/**
	 * Simple wrapper around the run method that prints debugging information
	 * 
	 * @param imp the ImagePlus on which to run the command
	 * @param command the command
	 * @param options the options
	 * @see IJ#run(ImagePlus, String, String)
	 */
	public static void run(ImagePlus imp, String command, String options){
		Log.debug( "image    : " + imp) ; 
		Log.debug( "file info: " + imp.getOriginalFileInfo().fileName ) ;
		Log.debug( "command  : " + command ) ;
		if( options != null){
			Log.debug( "options  : " + options ) ;
		}
		IJ.run( imp, command, options ) ;
		Log.debug( "-------- ok " ) ;
	}
	
	public static void run(ImagePlus imp, String command){
		run( imp, command, null ) ;
	}
	
	
	/**
	 * Runs the command with the options on the current image 
	 * (as seen by this class and not as seen by IJ.getImage)
	 * @param command
	 * @param options
	 */
	public static void run( String command, String options){
		run( image, command, options ) ;
	} 
	
	/**
	 * Workaround so that we can use setThreshold without needing to have a
	 * window open at the time
	 * 
	 * @param image The ImagePlus on which to set the threshold
	 * @param lowerThreshold lower threshold
	 * @param upperThreshold upper threshold
	 * 
	 * @see IJ#setThreshold(double, double) that does the same but the ImagePlus
	 * that is retrieved  by {@link IJ#getImage()}
	 */
	public static void setThreshold(ImagePlus img, double lowerThreshold, double upperThreshold) {
		Log.debug( "Set threshold ("+lowerThreshold+","+upperThreshold+") : " + img ) ;
		int mode = ImageProcessor.RED_LUT;
		Calibration cal = img.getCalibration();
		lowerThreshold = cal.getRawValue(lowerThreshold); 
		upperThreshold = cal.getRawValue(upperThreshold); 
		img.getProcessor().setThreshold(lowerThreshold, upperThreshold, mode);
		if (mode != ImageProcessor.NO_LUT_UPDATE) {
			img.getProcessor().setLutAnimation(true);
			img.updateAndDraw();
		}
	}
	
	public static void setCurrentImage(ImagePlus image){
		ZIJ.image = image ;
	}
	
}

