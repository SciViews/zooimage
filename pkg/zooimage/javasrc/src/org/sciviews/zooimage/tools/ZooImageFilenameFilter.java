package org.sciviews.zooimage.tools;

import java.io.File;
import java.io.FilenameFilter;

import org.sciviews.zooimage.log.Log;

/**
 * Filename filter used in ZooImage, filters by a sample name and 
 * an extension
 */
public class ZooImageFilenameFilter implements FilenameFilter {

	/**
	 * File extension to check, upper cased at creation
	 */
	private String extension ;
	
	/**
	 * Sample to accept, if this is null, then the sample is not checked
	 */
	private String sample ;
	
	/**
	 * Constructor for the filename filter
	 * @param extension
	 * @param sample
	 */
	public ZooImageFilenameFilter( String extension, String sample){
		this.extension = extension.toUpperCase() ;
		this.sample = sample ;
	}
	
	public ZooImageFilenameFilter(String extension) {
		this.extension = extension.toUpperCase() ;
		this.sample = null ;
	}

	/**
	 * Accepts files whose name start with the sample name
	 * and of the extension
	 */
	@Override
	public boolean accept(File dir, String name) {
		File f = new File( dir + File.separator + name);
		
		boolean result = true ;
		if( f.isDirectory() ) result = false ;
		if( result && !f.getName().toUpperCase().endsWith(extension) ) result = false ;
		if( result && sample != null && !f.getName().startsWith(sample) ) result = false;
		Log.debug( "file=" + f.getName() + ", extension = " + extension + ", sample = " + sample + "  (" + (result ? "-" : "x") +")"  ) ;
		
		return result ;
	}

}
