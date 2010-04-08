package org.sciviews.zooimage;

import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ZimFile;

public class NotAllVignettesCreatedException extends ZooImageException {

	private static final long serialVersionUID = 2214157257170069436L;

	public NotAllVignettesCreatedException( ZimFile zim, int n, int nok){
		super( "ERROR: " + (n - nok) + " on " + n + " vignettes were not created for " + zim.getSample() ) ;
	}
	
}
