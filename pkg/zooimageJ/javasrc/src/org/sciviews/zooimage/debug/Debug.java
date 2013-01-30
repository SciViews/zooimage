package org.sciviews.zooimage.debug;

/**
 * Contains flags useful for debugging
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class Debug {

	/**
	 * NORMAL mode
	 */
	public static final int NORMAL = 0 ;
	
	/**
	 * DEBUG mode - more messages are printed
	 */
	public static final int DEBUG = 1 ; 
	
	/**
	 * Current level of verbosity
	 */
	private static int level = Debug.DEBUG ;
	
	/**
	 * Sets the level of verbosity
	 * @param level new level
	 */
	public static void setLevel( int level){
		Debug.level = level ;
	}
	
	/**
	 * Gets the verbosity level
	 * @return the current level
	 */
	public static int getLevel( ){
		return level ;
	}
	
}
