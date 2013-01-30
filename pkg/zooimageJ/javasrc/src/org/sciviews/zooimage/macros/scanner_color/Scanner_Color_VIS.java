package org.sciviews.zooimage.macros.scanner_color;

import ij.ImagePlus;
import ij.process.ImageConverter;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.plugins.Scanner_Color;
import org.sciviews.zooimage.tools.Images;
import org.sciviews.zooimage.tools.RGB;
import org.sciviews.zooimage.tools.ZIJ;

public class Scanner_Color_VIS extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {

		ImageFile image = processor.getImage(); 
		RGB rgb = ( (Scanner_Color) image.getZim().getPlugin() ).getRgb() ;
		ImagePlus base = image.open();
		if( Log.getMode() == Log.IMAGEJ){
			base.setTitle( Images.imgVIS) ;
			base.show();
		}
		
		ZIJ.run(base, "Smooth");

		// Split RGB channel, apply weight and combine again
		rgb.weight( base ) ;

		// Convert into gray levels
		new ImageConverter( base ).convertToGray8() ;
		
		// Get a lighter background
		ZIJ.run(base, "Add...", "value=50");

		return base ;
	}

}
