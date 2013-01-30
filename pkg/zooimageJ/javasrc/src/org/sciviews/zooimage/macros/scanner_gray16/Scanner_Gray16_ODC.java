package org.sciviews.zooimage.macros.scanner_gray16;

import ij.ImagePlus;
import ij.process.ImageStatistics;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.ZIJ;

public class Scanner_Gray16_ODC extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException{
		
		ImageFile image = processor.getImage() ;
		CalibrationData calibration = image.getCalibration() ;
		
		// we need the raw image to start with
		ImagePlus imp = image.open() ;
		if( Log.getMode() == Log.IMAGEJ){
			imp.setTitle( "ZI1_ODC") ;
			imp.show() ;
		}
		
		ZIJ.run(imp, "Add...", "value=" + calibration.getWhitepoint() ) ; 
		imp.setRoi(0, 0, 1, 1);
		ZIJ.run(imp, "Add...", "value=65535");
		ZIJ.run(imp, "Select None");
		ZIJ.run(imp, "Log");
		ZIJ.run(imp, "Subtract Background...", "rolling=" + CalibrationData.rollballsize + " white");
		ZIJ.run(imp, "Calibrate...", "function=None unit=OD text1= text2=[0 1] global");
		ImageStatistics stats = imp.getStatistics();
		int maxval = (int)(stats.max);
		ZIJ.run(imp, "Min...", "value=" + (maxval - (65535 - calibration.getBlackpoint() )));
		
		imp.setRoi(0, 1, 1, 1);
		ZIJ.run(imp, "Max...", "value=" + (maxval - (65535 - calibration.getBlackpoint() )));
		ZIJ.run(imp, "Select None");
		ZIJ.run(imp, "8-bit");
		ZIJ.run(imp, "Max...", "value=254");
		ZIJ.run(imp, "Invert");
		ZIJ.run(imp, "Set Scale...", "distance=1 known=" + calibration.getPixsize() + 
				" pixel=1 unit=" + calibration.getPixelunit() + " global");
		ZIJ.run(imp, "Calibrate...", "function=[Straight Line] unit=OD text1=[0 250] text2=[0 1] global");
		
		return imp ;
		
	}

}
