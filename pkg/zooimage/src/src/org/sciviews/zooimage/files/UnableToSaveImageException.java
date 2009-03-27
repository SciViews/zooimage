package org.sciviews.zooimage.files;

import ij.ImagePlus;

import org.sciviews.zooimage.exceptions.ZooImageException;

public class UnableToSaveImageException extends ZooImageException {

	private static final long serialVersionUID = -4189432250615797603L;

	public UnableToSaveImageException( ImagePlus im, SecondaryImage sim){
		super( "unable to save image " + 
				im.toString() + 
				"into secondary image of type (" + sim.getType() + 
				") : " + 
				sim.getFile().getAbsolutePath() ) ;
	}
}
