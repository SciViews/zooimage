package org.sciviews.zooimage.macros;

import ij.IJ;
import ij.ImagePlus;
import ij.plugin.ImageCalculator;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.tools.Images;

public class ZooImageMacro_CMB extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
		
		ImagePlus vis_imp = processor.getVis().open( Images.imgVIS ) ;
		ImagePlus out_imp = processor.getOut().open( Images.imgOUT) ;
		
		ImageCalculator calc = new ImageCalculator() ;
		calc.calculate("operation=AND", vis_imp, out_imp) ;
		IJ.freeMemory();
		
		out_imp.close() ;
		
		return vis_imp; 
	}

}
