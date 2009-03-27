package org.sciviews.zooimage.macros.scanner_gray16;

import ij.ImagePlus;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.utils.Threshold;

public class Scanner_Gray16_MSK extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
		
		ProcessOptions options = processor.getPlugin().getOptions( ) ;
		
		// depending on the maskFromVIS option, we open the 
		// VIS image or the raw image
		ImagePlus base = options.get("maskFromVIS") ? 
			processor.getVis().open() : processor.getImage().open() ;
				
		Threshold.applyThreshold(base, 0, 230, true); 	
		
		return base ;
	}

}
