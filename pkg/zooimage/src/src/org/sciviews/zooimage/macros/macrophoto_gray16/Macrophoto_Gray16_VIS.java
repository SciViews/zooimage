package org.sciviews.zooimage.macros.macrophoto_gray16;

import ij.IJ;
import ij.ImagePlus;
import ij.io.FileOpener;
import ij.plugin.ImageCalculator;

import java.io.File;
import java.io.IOException;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.tools.Images;
import org.sciviews.zooimage.tools.ZIJ;

/**
 * VIS macro for the Macrophoto_Gray16 process
 */
public class Macrophoto_Gray16_VIS extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
		
		ImageFile image = processor.getImage() ;
		CalibrationData calibration = image.getZim().getPlugin().getCalibration() ;
		File tempFile = null ; 
		try{
			tempFile = File.createTempFile("zooimagetemp", ".gif"  ) ;
		} catch( IOException e){}
		
		ImagePlus blur = image.open() ;
		
		// Calibrate the grays
		calibration.calibrateGrays(blur) ;
		
		// Digicam: smooth background
		ZIJ.run(blur, "Smooth");
		ZIJ.run(blur, "Invert");
		ZIJ.run(blur, "Square Root");
		ZIJ.run(blur, "Invert");
		ZIJ.run(blur, "8-bit");
		IJ.freeMemory();
		ZIJ.run(blur, "Max...", "value=230");
		ZIJ.run(blur, "Add...", "value=25");
		
		// Digicam: sharpen again
		ZIJ.run(blur, "Sharpen");
		
		// Due to memory limitations, save a temporary version of that image
		IJ.save(blur, tempFile.getAbsolutePath() ) ;
		IJ.freeMemory();
		
		// Blur the image
		blur.setTitle("ZI1_Blur");
		ZIJ.run(blur, "Gaussian Blur...", "sigma=10");
		IJ.freeMemory();
		
		// Reopen the temporary version and delete the temporary file
		FileOpener.setShowConflictMessage(false) ;
		ImagePlus shadow = IJ.openImage(tempFile.getAbsolutePath()) ;
		shadow.setTitle("ZI1_Temp") ;
		
		ImageCalculator calc = new ImageCalculator( ) ;
		calc.calculate("operation=Subtract", shadow, blur) ;
		shadow.setTitle("ZI1_Shadow") ;
		
		// Reopen temp 
		ImagePlus vis = IJ.openImage(tempFile.getAbsolutePath()) ;
		vis.setTitle("ZI1_Temp") ;
		calc.calculate("operation=Add", vis, shadow) ;
		vis.setTitle( Images.imgVIS ) ;
		ZIJ.run(vis, "Subtract...", "value=15");
		
		// clean
		tempFile.delete() ;
		shadow.close() ;
		blur.close() ;
		
		return vis ;
	}

}
