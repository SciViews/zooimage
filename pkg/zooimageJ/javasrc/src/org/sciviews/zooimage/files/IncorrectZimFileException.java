package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

/**
 * Exception that signals an incorrect zim file
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class IncorrectZimFileException extends ZooImageException {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Constructor
	 */
	public IncorrectZimFileException( String problem ){
		super(problem) ;
	}

	
}
