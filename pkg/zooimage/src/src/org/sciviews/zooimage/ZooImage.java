package org.sciviews.zooimage;

import ij.IJ;
import ij.io.FileOpener;
import ij.plugin.PlugIn;

import java.io.File;
import java.lang.reflect.Constructor;
import java.util.HashMap;

import org.sciviews.zooimage.log.Log;

/**
 * Provides a ZooImage plugin
 * @author Romain Francois <francoisromain@free.fr>
 */
public class ZooImage implements PlugIn {

	/**
	 * Map of the available zooimage plugins
	 */
	private static final HashMap<String,ZooImagePluginDescriptor> plugins = 
		ZooImagePluginDescriptor.getPlugins() ;
	
	/**
	 * Returns an instance of a ZooImagePlugin given its name
	 * @param name name of the plugin (simple class name)
	 * @return the plugin instance
	 */
	public static ZooImagePlugin getPlugin( String name){
		if( plugins.containsKey(name ) ){
			Class<?extends ZooImagePlugin> clazz = plugins.get(name).getClazz() ;
			try {
				Constructor<?extends ZooImagePlugin> c_ = 
					clazz.getConstructor( (Class<?>[]) null ) ;
				ZooImagePlugin p = c_.newInstance( (Object[]) null );
				return p ;
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return null ;
	}

	/**
	 * Main method to use one of ZooImage plugin from the command line
	 * 
	 * @param args command line arguments
	 * <ol>
	 * 	<li>name of the plugin (simple name of the class)</li>
	 *  <li>directory or zim file to process</li>
	 *  <li>output directory</li>
	 * </ol>
	 */
	public static void main(String args[]) {
		
		if(args.length != 3 && args.length != 2 ) {
			Log.log("usage: zooimage pluginname myfile.zim|mydir [resultdir]");
			showAvailablePlugins() ;
		} else {
			// Need to start ImageJ
			Log.setMode(Log.BATCH ) ;
			
			// the following is temporary until
			// we find a solution to just use ImageJ as 
			// a library without the GUI
			//						ImageJ.main( null ) ;
			//						ImageJ ij = IJ.getInstance();    	
			// 						ij.setVisible(false) ;
			
			// Process the arguments
			FileOpener.setShowConflictMessage(false) ;
			
			String name = args[0] ;
			String dir = args[1] ;
			File out = null;
			if( args.length == 3 ){
				out = new File( args[2] ) ;
			}
			ZooImagePlugin plugin = getPlugin( name ) ;
			if( plugin == null ){
				Log.log( "Could not find zoo image process : '" + name + "'") ;
				showAvailablePlugins() ;
			} else{
				
				// is dir a directory or a zim file
				File file = new File( dir ) ;
				if( file.isDirectory() ){
					plugin.processDirectory(dir, out) ;
				} else{
					plugin.processSingleZimFile(dir, out) ;
				}
				
			}
			System.exit(0);
		}
	}
	
	/**
	 * Entry point from the ImageJ menu
	 */
	public void run( String arg ){
		getPlugin(arg).run( ) ;
	}
	
	
	/**
	 * Show the available plugins, their version and their description
	 */
	public static void showAvailablePlugins( ){
		Log.log( "" ) ;
		Log.log( "Available ZooImage plugins: ") ;
		Log.log( "" ) ;
		for( ZooImagePluginDescriptor descriptor: plugins.values() ){
			Log.log( "'" + descriptor.getName() + "': (" + descriptor.getVersion() + ")" ) ;
			Log.log( "   " + descriptor.getDescription() ) ;
			Log.log( "" ) ;
		}
	}

	
	/**
	 * The about box for the supplied plugin
	 * @param name the name of the plugin for which we want information
	 */
	public static void showAbout(String name) {
		ZooImagePluginDescriptor descriptor = plugins.get(name) ;
		IJ.showMessage("About " + name + "... (version " + descriptor.getVersion()  + ")",
			descriptor.getDescription() + 
			"Copyright 2006, " + descriptor.getAuthor() + "\n" +
			"License: GPL 2 or above at your convenience.");
	}

	/**
	 * Returns the descriptor of a Zoo Image Plugin
	 * @param name name of the plugin
	 * @return descriptor
	 */
	public static ZooImagePluginDescriptor getDescriptor(String name) {
		return plugins.get(name) ;
	}

	
}
