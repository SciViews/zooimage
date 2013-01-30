package org.sciviews.zooimage.config;

import ij.Prefs;

import java.util.HashMap;

/**
 * Various options shared by all plugins
 */
public class ProcessOptions {

	/**
	 * Map of the options
	 */
	private HashMap<String,Boolean> options ;
	
	public ProcessOptions(){
		options = new HashMap<String,Boolean>() ;
		set( "allfiles"   , Prefs.get("ZI1.boolean", true ) ) ;
		set( "analyzepart", Prefs.get("ZI1.boolean", true ) ) ;
		set( "makevigs"   , Prefs.get("ZI1.boolean", true ) ) ;
		set( "sharpenvigs", Prefs.get("ZI1.boolean", true ) ) ;
		set( "showoutline", Prefs.get("ZI1.boolean", true ) ) ;
		set( "ziptiff"    , Prefs.get("ZI1.boolean", false) ) ;
		set( "useOD"      , Prefs.get("ZI1.boolean", false) ) ;
		set( "maskfromVIS", Prefs.get("ZI1.boolean", false) ) ;
	}
	
	/**
	 * Does this process have this option enabled
	 * @param option An option 
	 * @return true if the option is enabled
	 */
	public boolean get( String option ){
		if( ! options.containsKey( option ) ) return false ;
		return options.get( option ).booleanValue() ;
	}
	
	/**
	 * Sets an option
	 * @param option the name of the option
	 * @param value the new value of the option
	 */
	public void set( String option,  boolean value){
		options.put( option, new Boolean(value) ) ;
	}
	
}
