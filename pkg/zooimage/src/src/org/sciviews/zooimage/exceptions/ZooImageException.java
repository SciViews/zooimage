package org.sciviews.zooimage.exceptions;

import org.sciviews.zooimage.log.Log;

/**
 * Super class of all ZooImage Exceptions
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class ZooImageException extends Exception {

	private static final long serialVersionUID = 1L;

	private String problem ;
	
	/**
	 * Constructor 
	 * @param problem
	 */
	public ZooImageException(String problem) {
		super(problem) ;
		this.problem = problem ;
	}

	/**
	 * Logs the exception message
	 * @see Log#log(String)
	 */
	public void log(){
		Log.log( getMessage() ) ;
	}
	
	/** 
	 * Gets the problem
	 * @return the problem
	 */
	public String getProblem() {
		return problem;
	}
	
}
