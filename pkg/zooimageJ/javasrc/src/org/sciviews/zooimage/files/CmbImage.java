/**
 * 
 */
package org.sciviews.zooimage.files;

import org.sciviews.zooimage.ImageFileProcessor;

/**
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class CmbImage extends SecondaryImage {

	private static final String TYPE = "CMB" ;
	
	/**
	 * @param image
	 */
	public CmbImage(ImageFileProcessor processor) {
		super(processor);
	}

	@Override
	public String getType() {
		return TYPE ;
	}

}
