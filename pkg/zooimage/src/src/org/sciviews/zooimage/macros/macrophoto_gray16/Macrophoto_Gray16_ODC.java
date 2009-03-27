package org.sciviews.zooimage.macros.macrophoto_gray16;

import ij.ImagePlus;
import ij.process.ImageStatistics;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.tools.ZIJ;

public class Macrophoto_Gray16_ODC extends ZooImageMacro {
		
	@Override
	public ImagePlus run(ImageFileProcessor processor ) throws ZooImageException {
		
		ImageFile image = processor.getImage() ;
		
		CalibrationData calibration = 
			image.getZim().getPlugin().getCalibration() ;
		
		ImagePlus imp = image.open() ;
		
		ZIJ.run(imp, "Add...", "value=" + calibration.getWhitepoint() );
		imp.setRoi(0, 0, 1, 1);
		ZIJ.run(imp, "Add...", "value=65535");
		ZIJ.run(imp, "Select None");
		ZIJ.run(imp, "Log");
		ZIJ.run(imp, "Subtract Background...", "rolling=250 white");
		ZIJ.run(imp, "Calibrate...", "function=None unit=OD text1= text2=[0 1] global");
		ImageStatistics stats = imp.getStatistics();
		int maxval = (int)(stats.max);
		
		// ### TODO: homogenize the definition of the blackpoint between the two systems!!!
		ZIJ.run(imp, "Min...", "value=" + calibration.convertBlackPoint( maxval ) ) ;
		imp.setRoi(0, 1, 1, 1);
		
		// Idem
		ZIJ.run(imp, "Max...", "value=" + calibration.convertBlackPoint( maxval ) ) ;
		ZIJ.run(imp, "Select None");
		ZIJ.run(imp, "8-bit");
		ZIJ.run(imp, "Max...", "value=254");
		ZIJ.run(imp, "Invert");
		ZIJ.run(imp, "Set Scale...", "distance=1 known=" + calibration.getPixsize() + " pixel=1 unit=mm global");
		ZIJ.run(imp, "Calibrate...", "function=[Straight Line] unit=OD text1=[0 250] text2=[0 1] global");
		
		return imp ;
	}

}
