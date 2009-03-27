package org.sciviews.zooimage.files;

import org.sciviews.zooimage.ImageFileProcessor;



/**
 * Representation of a Out Image
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class OutImage extends SecondaryImage {

	private static final String TYPE = "OUT" ;
		
	/**
	 * Constructor
	 * 
	 * @param image the raw image associated with this mask image
	 */
	public OutImage( ImageFileProcessor processor ){
		super( processor ) ;
	}
	
	/**
	 * type of out images
	 */
	@Override
	public String getType( ){
		return TYPE ;
	}

	
}
