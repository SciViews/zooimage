package org.sciviews.zooimage.macros.microscope_color;

import ij.ImagePlus;
import ij.plugin.ImageCalculator;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.plugins.Microscope_Color;
import org.sciviews.zooimage.tools.RGB;
import org.sciviews.zooimage.tools.Threshold;
import org.sciviews.zooimage.tools.ZIJ;
import org.sciviews.zooimage.utils.Duplicater;

public class Microscope_Color_MSK extends ZooImageMacro {

	// TODO
	@Override
	public ImagePlus run(ImageFileProcessor processor) throws ZooImageException {
		
		ProcessOptions options = processor.getPlugin().getOptions( ) ;
		ImageFile image = processor.getImage() ;
		ImagePlus base = options.get("maskFromVIS") ? 
			processor.getVis().open() : image.open() ;
		if( Log.getMode() == Log.IMAGEJ){
			base.setTitle("ZI1_Temp") ;
			base.show() ;
		}
		Microscope_Color plugin = (Microscope_Color) image.getZim().getPlugin() ;
		Threshold thresholds = plugin.getThresholds() ;

		// First, create a mask on edges
		ImagePlus edge_imp = Duplicater.clone( base ) ; 
		if( Log.getMode() == Log.IMAGEJ){
			edge_imp.setTitle( "ZI1_MskEdge" ) ;
			edge_imp.show();
		}
		ZIJ.run(edge_imp, "Enhance Contrast", "saturated=" + thresholds.getContrastsat() );
		ZIJ.run(edge_imp, "Find Edges");
		org.sciviews.zooimage.utils.Threshold.applyThreshold(
				edge_imp, thresholds.getEdge(), 255, false); 
	
		// Split particle channels and keep only green one (the cleanest one)
		RGB.keepGreenChannel(base) ;
		ImagePlus green_imp = Duplicater.clone(base) ;
		if( Log.getMode() == Log.IMAGEJ){
			green_imp.setTitle("ZI1_Msk");
			green_imp.show() ;
		}
		// Create a second mask, based on this green channel
		ImagePlus mask2_imp = Duplicater.clone(green_imp) ;
		org.sciviews.zooimage.utils.Threshold.applyThreshold(
				mask2_imp, 0, thresholds.getDark(), false); 
		if( Log.getMode() == Log.IMAGEJ){
			mask2_imp.setTitle("ZI1_Msk2") ;
			mask2_imp.show() ;
		}
		
		//Threshold at a lower level for second version, but after substracting back
		ZIJ.run(green_imp, "Subtract Background...", "rolling=50 white");
		org.sciviews.zooimage.utils.Threshold.applyThreshold(
				green_imp, 0, thresholds.getLight(), false); 
		
		// Add the three
		ImageCalculator calc = new ImageCalculator( ); 
		calc.calculate( "operation=AND" , green_imp, mask2_imp) ;
		calc.calculate( "operation=AND" , green_imp, edge_imp) ;
		mask2_imp.close() ;
		edge_imp.close( ) ;
		
		return green_imp ;
	}

}
