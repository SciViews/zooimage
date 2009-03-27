package org.sciviews.zooimage.tools;

import java.io.IOException;

import org.sciviews.zooimage.exceptions.ZooImageException;

public class ZooImageIOException extends ZooImageException {

	private static final long serialVersionUID = 1056418246696662796L;

	public ZooImageIOException( IOException ioe ){
		super( ioe.getMessage() ) ;
	}
}
