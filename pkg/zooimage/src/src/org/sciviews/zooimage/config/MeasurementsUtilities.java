package org.sciviews.zooimage.config;

import ij.measure.Measurements;

import java.util.HashMap;
import java.util.StringTokenizer;

public class MeasurementsUtilities {
	
	private static HashMap<String,Integer> map ;
	static{
		map = new HashMap<String,Integer>();
		map.put( "area", Measurements.AREA)  ; // 1
		map.put( "mean", Measurements.MEAN)  ; // 2
		map.put( "standard", Measurements.STD_DEV)  ; // 4
		map.put( "modal", Measurements.MODE)  ; // 8
		map.put( "min", Measurements.MIN_MAX)  ; // 16
		map.put( "centroid", Measurements.CENTROID)  ; // 32
		map.put( "center", Measurements.CENTER_OF_MASS)  ; // 64
		map.put( "perimeter", Measurements.PERIMETER)  ; // 128
		map.put( "limit", Measurements.LIMIT )  ; //  256
		map.put( "bounding", Measurements.RECT)  ; // 512
		map.put( "labels", Measurements.LABELS)  ; // 1024
		map.put( "fit", Measurements.ELLIPSE)  ; // 2048
		map.put( "invert", Measurements.INVERT_Y)  ; // 4096
		map.put( "circularity", Measurements.CIRCULARITY)  ; // 8192
		map.put( "shape", Measurements.SHAPE_DESCRIPTORS)  ; // 8192
		map.put( "feret's", Measurements.FERET)  ; // 16384
		map.put( "integrated", Measurements.INTEGRATED_DENSITY)  ; // 0x8000
		map.put( "median", Measurements.MEDIAN)  ; // 0x10000
		map.put( "skewness", Measurements.SKEWNESS)  ; // 0x20000
		map.put( "kurtosis", Measurements.KURTOSIS)  ; // 0x40000
		map.put( "fraction", Measurements.AREA_FRACTION)  ; // 0x80000
		map.put( "slice", Measurements.SLICE)  ; // 0x100000
		map.put( "scientific", Measurements.SCIENTIFIC_NOTATION)  ; // 0x200000;
	}
	
	public static int getMeasurements(String mes){
		StringTokenizer st = new StringTokenizer( mes ) ;
		int result = 0;
		while( st.hasMoreTokens( )){
			String tok = st.nextToken() ;
			if( map.containsKey(tok)){
				result += map.get(tok).intValue() ;
			}
		}
		return result; 
	}
	
	
}
