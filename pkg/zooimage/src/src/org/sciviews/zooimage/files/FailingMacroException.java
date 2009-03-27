package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

/**
 * Exception thrown when a macro did not generate the
 * secondary image properly 
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class FailingMacroException extends ZooImageException {

	private static final long serialVersionUID = -1237566397618072491L;

	public FailingMacroException( SecondaryImage image) {
		super( "Failed to apply macro to create the " + image.getType() + " : " + 
				image.getFile().getAbsolutePath() ) ;
	}
}
