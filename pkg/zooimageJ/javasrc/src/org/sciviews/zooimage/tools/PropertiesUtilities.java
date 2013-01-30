package org.sciviews.zooimage.tools;

/**
 * Utilities to manipulate Properties
 */
public class PropertiesUtilities {

	/**
	 * Returns the property as an int
	 * @param prop property
	 * @return the property as an int
	 */
	public static int getIntProperty( String prop ){
		return (int)Math.round( getDoubleProperty( prop ) ) ;
	}
	
	/**
	 * Returns the property as a double
	 * @param prop property 
	 * @return the property as a double
	 */
	public static double getDoubleProperty( String prop ){
		return Double.parseDouble(prop) ;
	}
	
}
