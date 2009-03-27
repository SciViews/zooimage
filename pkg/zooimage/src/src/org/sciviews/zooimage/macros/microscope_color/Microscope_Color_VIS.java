package org.sciviews.zooimage.macros.microscope_color;

import ij.ImagePlus;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.tools.RGB;

/**
 * Split RGB channels and keep only green one (the cleanest one!)        
 *
 */
public class Microscope_Color_VIS extends ZooImageMacro  {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
	
		ImagePlus imp = processor.getImage().open() ;
		
		RGB.keepGreenChannel(imp) ;
		
        return imp ;
	}

}
