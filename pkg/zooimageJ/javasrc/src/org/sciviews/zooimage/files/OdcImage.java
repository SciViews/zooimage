package org.sciviews.zooimage.files;

import org.sciviews.zooimage.ImageFileProcessor;



public class OdcImage extends SecondaryImage {

	private static final String TYPE = "ODC" ;
	
	/**
	 * Constructor
	 * @param image raw image this is associated with
	 */
	public OdcImage(ImageFileProcessor processor) {
		super(processor);
	}

	/**
	 * ODC type
	 */
	@Override
	public String getType() {
		return TYPE ;
	}

}
