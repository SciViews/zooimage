package org.sciviews.zooimage;

import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ZimFile;

public class NoVignetteCreatedException extends ZooImageException {

	private static final long serialVersionUID = 6382053089793254173L;

	public NoVignetteCreatedException( ZimFile zim ){
		super( "ERROR: impossible to create vignettes for " + zim.getSample() ) ;
	}
	
}
