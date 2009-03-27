package org.sciviews.zooimage.utils;

import ij.CompositeImage;
import ij.ImagePlus;
import ij.ImageStack;
import ij.gui.Roi;
import ij.process.ImageProcessor;
import ij.process.LUT;

import org.sciviews.zooimage.log.Log;

/**
 * Contains a static method that duplicates an ImagePlus
 * silently. Based on ij.plugin.filter.Duplicater, but silent
 * 
 * @author Romain Francois <francoisromain@free.fr>
 * @see ij.plugin.filter.Duplicater#duplicate(ImagePlus)
 */
public class Duplicater {

	public static ImagePlus clone( ImagePlus imp ){

		Log.debug( "duplicating" + imp ) ;
		
		int stackSize = imp.getStackSize();
		ImagePlus imp2;
		Roi roi = imp.getRoi();
		ImageProcessor ip2 = imp.getProcessor().crop();
		imp2 = imp.createImagePlus();
		imp2.setProcessor(null, ip2);
		String info = (String)imp.getProperty("Info");
		if (info!=null)
			imp2.setProperty("Info", info);
		if (stackSize>1) {
			ImageStack stack = imp.getStack();
			String label = stack.getSliceLabel(imp.getCurrentSlice());
			if (label!=null && label.indexOf('\n')>0)
				imp2.setProperty("Info", label);
			if (imp.isComposite()) {
				LUT lut = ((CompositeImage)imp).getChannelLut();
				imp2.getProcessor().setColorModel(lut);
			}
		}
		
		if (roi!=null && roi.isArea() && roi.getType()!=Roi.RECTANGLE)
			imp2.restoreRoi();
		
		return imp2 ;
	}

}
