package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

/**
 * Exception indicating that a macro to deal with a secondary image is not available
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class MissingMacroException extends ZooImageException {

	private static final long serialVersionUID = -6945534937693326084L;

	public MissingMacroException( SecondaryImage image ){
		super( "Missing macro to create image ("+image.getType()+") " + 
				image.getFile().getAbsolutePath() ) ;
	}

}
