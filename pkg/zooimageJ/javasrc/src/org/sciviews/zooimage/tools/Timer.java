package org.sciviews.zooimage.tools;

/**
 * Simple timer
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class Timer {

	/**
	 * Start time
	 */
	private long start ;
	
	/**
	 * Operations count
	 */
	private int count ;
	
	/**
	 * Constructor for Timer, records the current time
	 */
	public Timer(){
		start = System.currentTimeMillis() ;
		count=0;
	}
	
	/**
	 * Increment the number of operations that were performed 
	 * and return the new number
	 * 
	 * @return the new opera
	 */
	public int addOperation(){
		count++ ;
		return count ;
	}
	
	/**
	 * Returns the number of milli seconds since the start time
	 * @return the number of milli seconds since start
	 */
	public long getElapsedMilliseconds(){
		long now = System.currentTimeMillis() ;
		return ( now - start ) ; 
	}
	
	/**
	 * Returns the number of seconds since the start time
	 * @return the number of seconds since start
	 */
	public long getElapsedSeconds(){
		return getElapsedMilliseconds() / 1000 ;
	}
	
	/**
	 * Returns the number of minutes since the start time
	 * @return the number of minutes since start
	 */
	public long getElapsedMinutes() {
		return getElapsedSeconds() / 60;
	}
	
	/**
	 * Gets the number of operations that have been performed 
	 * 
	 * @return the number of operations
	 */
	public int getCount(){
		return count ;
	}
	
	/**
	 * Gets the average number of milliseconds per operation
	 * @return the average time an operation took (in milliseconds)
	 */
	public long getAverageMilliseconds(){
		if( count == 0 ) return getElapsedMilliseconds() ;
		return getElapsedMilliseconds() / count ;
	}

	/**
	 * Gets the average number of seconds per operation
	 * @return the average time an operation took (in seconds)
	 */
	public long getAverageSeconds(){
		return getAverageMilliseconds() / 1000 ;
	}

	/**
	 * Gets the average number of minutes per operation
	 * @return the average time an operation took (in minutes)
	 */
	public long getAverageMinutes(){
		return getAverageSeconds() / 60 ;
	}

}
