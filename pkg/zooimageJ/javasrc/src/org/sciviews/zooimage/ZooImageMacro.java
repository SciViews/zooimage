package org.sciviews.zooimage;

import ij.IJ;
import ij.ImagePlus;

import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.SecondaryImage;
import org.sciviews.zooimage.log.Log;

/**
 * A Zoo Image macro. Contains a run method that returns
 * true if the process was successful
 * 
 * @author Romain Francois <francoisromain@free.fr>
 */
public abstract class ZooImageMacro {
	
	/**
	 * Run method. To be written by implementations of this class
	 * 
	 * @return the processed image (as an ImagePlus object)
	 */
	public abstract ImagePlus run( ImageFileProcessor processor ) throws ZooImageException ;

	/**
	 * Wrap the running of the macro between two calls to IJ.freeMemory
	 * so that memory is flushed before and after the macro runs 
	 * 
	 * @param processor
	 * @return the processed image (as an ImagePlus object)
	 */
	public ImagePlus process( ImageFileProcessor processor ) throws ZooImageException {
		IJ.freeMemory() ;
		Log.debug( "<" + getClass().getName() + ">" ) ;
		ImagePlus result = run( processor ) ;
		Log.debug( "</" + getClass().getName() + ">" ) ;
		IJ.freeMemory() ;
		return result ;
	}
	
	public void processAndSave( ImageFileProcessor processor, SecondaryImage sim) throws ZooImageException {
		ImagePlus imp = process( processor ) ;
		sim.save( imp ) ;
		imp.close( ) ;
	}
	
}
