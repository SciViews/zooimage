package org.sciviews.zooimage.utils;

import ij.ImagePlus;
import ij.process.ImageConverter;
import ij.process.ImageProcessor;

import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.ZIJ;

public class Threshold {

	// TODO: replace calls to ZIJ
	public static void applyThreshold( ImagePlus image, int lower, int upper, boolean smooth){
		ImageProcessor processor = image.getProcessor() ;
		
		Log.debug( "min = " + processor.getMin() + ", max = " +processor.getMax() ) ;
		
		if( image.getType() != ImagePlus.GRAY8 ){
			new ImageConverter(image).convertToGray8() ;
		}
		processor.setThreshold(lower, upper, ImageProcessor.BLACK_AND_WHITE_LUT) ;
		ZIJ.run( image, "Threshold", "thresholded remaining black" ) ;
		if( smooth){
			ZIJ.setThreshold(image, 0, 128) ;
			ZIJ.run( image, "Smooth" ) ;
			ZIJ.run( image, "Threshold", "thresholded remaining black" ) ;
		}
		
	}
	
	
}
