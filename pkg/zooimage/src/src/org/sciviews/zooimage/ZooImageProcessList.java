package org.sciviews.zooimage;

import java.util.HashMap;

/**
 * Utility that displays the list of available processes
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class ZooImageProcessList {

	/**
	 * @param args command line arguments, ignored
	 */
	public static void main(String[] args) {
		HashMap<String,ZooImagePluginDescriptor> plugins = 
			ZooImagePluginDescriptor.getPlugins() ;
		for( String p : plugins.keySet()){
			System.out.println( p ) ;
		}
	}

}
