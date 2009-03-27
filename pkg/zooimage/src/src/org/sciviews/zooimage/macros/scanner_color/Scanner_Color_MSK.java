package org.sciviews.zooimage.macros.scanner_color;

import ij.ImagePlus;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.plugins.Scanner_Color;
import org.sciviews.zooimage.tools.RGB;
import org.sciviews.zooimage.utils.Threshold;

public class Scanner_Color_MSK extends ZooImageMacro {
	
	@Override
	public ImagePlus run( ImageFileProcessor processor) throws ZooImageException {
		
		ProcessOptions options = processor.getPlugin().getOptions( ) ;
		ImagePlus base = options.get("maskFromVIS") ? 
			processor.getVis().open() : processor.getImage().open() ;
		Scanner_Color plugin = (Scanner_Color) processor.getImage().getZim().getPlugin() ; 
		RGB rgb = plugin.getRgb() ;
		
		Threshold.applyThreshold(base, 0, rgb.getThreshold(), true) ;
		
		return base ;
	}

}
