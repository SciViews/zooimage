package org.sciviews.zooimage.utils;

import ij.ImagePlus;
import ij.process.ByteProcessor;

public class Gray8 {

	
	public static void add( ImagePlus image, byte value){
		ByteProcessor bp = (ByteProcessor)image.getProcessor() ;
		int n = image.getWidth() * image.getHeight() ;
		for( int i=0; i<n; i++) {
			bp.set(i, bp.get(i) + value) ;
		}
	}
	
}
