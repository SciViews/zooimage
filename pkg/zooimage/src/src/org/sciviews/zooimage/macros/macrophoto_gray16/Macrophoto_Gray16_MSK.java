package org.sciviews.zooimage.macros.macrophoto_gray16;

import ij.ImagePlus;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.Images;
import org.sciviews.zooimage.utils.Threshold;


/**
 * MSK macro for process Macrophoto_Gray16
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class Macrophoto_Gray16_MSK extends ZooImageMacro {

	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
		
		ProcessOptions options = processor.getPlugin().getOptions( ) ;
		ImagePlus base = options.get("maskFromVIS") ? 
			processor.getVis().open() : processor.getImage().open() ;

		if( Log.getMode() == Log.IMAGEJ){
			base.setTitle(Images.imgMSK) ;
			base.show() ;
		}
		// apply 230 threshold, smooth, and apply 128 threshold
		Threshold.applyThreshold(base, 0, 230, true) ;
		
		return base ;
	}

}
