package org.sciviews.zooimage;

import org.sciviews.zooimage.exceptions.ZooImageException;

/**
 * Thrown when the result table is empty
 * 
 * @author Romain Francois <francoisromain@free.fr>
 * @see ImageFileProcessor#processVIG()
 */
public class EmptyTableException extends ZooImageException {

	private static final long serialVersionUID = 622542671503596615L;

	public EmptyTableException(){
		super( "ERROR: not able to make vignettes; table empty or closed." ) ;
	}
}
