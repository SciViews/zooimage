package org.sciviews.zooimage.files;

import org.sciviews.zooimage.ImageFileProcessor;



/**
 * Representation of a Mask Image
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class MaskImage extends SecondaryImage {

	private static final String TYPE = "MSK" ;
	
	/**
	 * Constructor
	 * 
	 * @param image the raw image associated with this mask image
	 */
	public MaskImage( ImageFileProcessor processor ){
		super( processor ) ;
	}
	
	/**
	 * type of mask images
	 */
	@Override
	public String getType( ){
		return TYPE ;
	}

}
