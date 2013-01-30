package org.sciviews.zooimage.log;

import ij.IJ;

import org.sciviews.zooimage.debug.Debug;

/**
 * Logging for ZooImage. Facilities for logging messages
 * to either the standard output or the ImageJ 
 * logging facilities
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class Log {

	/**
	 * Indicates if we are in BATCH mode (log messages 
	 * need to be sent to the standard outpu stream)
	 * or not (messages sent to ImageJ log method)
	 */
	
	/**
	 * BATCH mode, when this mode is on, messages
	 * are sent to the standard output
	 * @see System.out#println
	 */
	public static final int BATCH = 0 ;
	
	/**
	 * IMAGEJ mode, when this mode is on, messages
	 * are sent to ImageJ logging window
	 * @see IJ#log(String)
	 */
	public static final int IMAGEJ = 1 ;
	
	/**
	 * Mode (BATCH or IMAGEJ)
	 */
	private static int mode = BATCH ;
	
	/**
	 * Logs a message to standard output (in batch mode)
	 * or to ImageJ standard logging facilities
	 * @see IJ#log(String)
	 */
	public static void log( String s){
		if( mode == BATCH ){
			System.out.println(s);
		} else{
			IJ.log( s ) ;
		}
	}
	
	/**
	 * Sets the mode
	 * @param mode the new mode
	 */
	public static void setMode(int mode) {
		Log.mode = mode;
	}	
	
	/**
	 * Gets the mode
	 * @return the current mode
	 */
	public static int getMode() {
		return mode;
	}

	/**
	 * Logs an error message to either standard error stream
	 * of to ImageJ error function
	 * @param s error message
	 */
	public static void error(String s) {
		if( mode == BATCH ){
			System.err.println(s);
		} else{
			IJ.error( s ) ;
		}
	}

	/**
	 * Shows the status (only makes sense in IMAGEJ mode)
	 * @param s status to show
	 */
	public static void showStatus(String s) {
		if( mode == IMAGEJ ){
			IJ.showStatus(s) ;
		}
	}

	/**
	 * Prints a log message only if debugging flag is on
	 * @param string
	 */
	public static void debug(String string) {
		if( Debug.getLevel() == Debug.DEBUG){
			Log.log( string ) ;
		}
	}
}
