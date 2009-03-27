package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

public class NoImageException extends ZooImageException {

	private static final long serialVersionUID = -4096512625768783531L;

	public NoImageException( SecondaryImage sim){
		super( "No image to save into (" + sim.getType() + 
				") " + sim.getFile().getAbsolutePath() ) ;
	}
}
