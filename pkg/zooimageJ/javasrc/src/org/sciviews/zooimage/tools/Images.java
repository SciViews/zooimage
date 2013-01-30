package org.sciviews.zooimage.tools;

import java.util.HashMap;

/**
 * Various Image used by ZooImage
 */
public class Images {
	
	/**
	 * Raw image
	 */
	public static final String imgRAW = "ZI1_RAW";
	
	/**
	 * Visual image
	 */
	public static final String imgVIS = "ZI1_VIS";
	
	/**
	 * Mask image
	 */
	public static final String imgMSK = "ZI1_MSK";
	
	/**
	 * O.D. calibrated image
	 */
	public static final String imgODC = "ZI1_ODC";
	
	/**
	 * Outline image
	 */
	public static final String imgOUT = "ZI1_OUT";
	
	/**
	 * Vignette image
	 */
	public static final String imgVIG = "ZI1_VIG";
	
	/**
	 * Combo visu + outline
	 */
	public static final String imgCMB = "ZI1_CMB";
	
	
	private static HashMap<String,String> map ;
	static{
		map = new HashMap<String, String>() ;
		map.put( "RAW", imgRAW) ;
		map.put( "VIS", imgVIS) ;
		map.put( "MSK", imgMSK) ;
		map.put( "ODC", imgODC) ;
		map.put( "OUT", imgOUT) ;
		map.put( "VIG", imgVIG) ;
		map.put( "CMB", imgCMB) ;
	}
	
	/**
	 * Returns the image title suitable for the given type
	 * @param type the type of image
	 */
	public static String get( String type ){
		return map.get(type) ;
	}
	
	
}
