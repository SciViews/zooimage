package org.sciviews.zooimage.macros.scanner_gray16;

import ij.IJ;
import ij.ImagePlus;
import ij.io.FileOpener;
import ij.plugin.ImageCalculator;

import java.io.File;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.Images;
import org.sciviews.zooimage.tools.ZIJ;

public class Scanner_Gray16_VIS extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {

		ImageFile image = processor.getImage() ;
		
		Log.debug( "Retrieving calibration data : " + image ) ;
		CalibrationData calibration = image.getCalibration() ;
		
		// ------------- (1) we start with the raw image, that we treat until we have the blur 
		ImagePlus blur = image.open( ) ; 
		blur.setTitle( "ZI1_Blur" ) ;
	
		// calibrate the gray levels
		calibration.calibrateGrays( blur ) ;
		
		ZIJ.run(blur , "Invert", null);
		ZIJ.run(blur , "Square Root", null);
		ZIJ.run(blur , "Invert", null);
		ZIJ.run(blur , "8-bit", null);
		IJ.freeMemory();
		
		ZIJ.run(blur , "Max...", "value=230");
		ZIJ.run(blur , "Add...", "value=25");

		File tempFile = null ; 
		try{
			tempFile = File.createTempFile("zooimage_", "_tmp.gif") ;
		} catch( Exception e){}
		IJ.save(blur, tempFile.getAbsolutePath() ) ;
		
		// Blur the image
		ZIJ.run(blur , "Gaussian Blur...", "sigma=10");
		IJ.freeMemory();
		
		FileOpener.setShowConflictMessage(false) ;
		ImagePlus shadow = IJ.openImage(tempFile.getAbsolutePath()) ;
		shadow.setTitle("ZI1_Temp") ;
		
		// here we call ImageCalculator manually, otherwise
		// it opens a new image which is not silent enough
		// for command line mode
		// after the calculate step, the first image it is
		// passed is the result
		ImageCalculator calc = new ImageCalculator( ) ;
		calc.calculate("operation=Subtract", shadow, blur) ;
		shadow.setTitle("ZI1_shadow") ;
		IJ.freeMemory();
		
		ImagePlus vis = IJ.openImage(tempFile.getAbsolutePath()) ;
		vis.setTitle("ZI1_Temp") ;
		IJ.freeMemory();
		calc.calculate("operation=Add", vis, shadow ) ;
		
		vis.setTitle( Images.imgVIS ) ;
		ZIJ.run(vis, "Subtract...", "value=15");
		
		// clean up
		tempFile.delete();
		shadow.close() ;
		blur.close() ;
		
		return vis ;
	}

}
