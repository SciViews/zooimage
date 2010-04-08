package org.sciviews.zooimage.files;

import org.sciviews.zooimage.ImageFileProcessor;



/**
 * Representation of a VIS image
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class VisImage extends SecondaryImage {

	private static final String TYPE = "VIS" ;
	
	/**
	 * Constructor for a VIS image
	 * @param image 
	 */
	public VisImage( ImageFileProcessor processor ){
		super( processor) ;
	}
	
	/**
	 * The type of the VIS image
	 */
	@Override
	public String getType( ){
		return TYPE ;
	}
	
}
