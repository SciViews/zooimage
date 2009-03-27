package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

/**
 * Exception triggered when the RAW image can not be open
 * 
 * @author Romain Francois <francoisromain@free.fr>
 * @see ImageFile#open()
 */
public class CouldNotOpenRawImageException extends ZooImageException {

	private static final long serialVersionUID = 7880278949321229620L;

	public CouldNotOpenRawImageException( ImageFile image){
		super( "Could not open RAW image : " + image.getFile().getAbsolutePath() ) ;
	}
}
