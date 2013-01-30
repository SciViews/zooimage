package org.sciviews.zooimage.files;

import org.sciviews.zooimage.exceptions.ZooImageException;

public class WorkDirectoryIsNotDirectoryException extends ZooImageException {

	private static final long serialVersionUID = 1L;

	public WorkDirectoryIsNotDirectoryException( ){
		super( "_work directory is not a directory" ) ;
	}
	
}
