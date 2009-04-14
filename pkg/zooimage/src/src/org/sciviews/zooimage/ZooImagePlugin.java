package org.sciviews.zooimage;

import java.awt.Dimension;
import java.io.File;
import java.util.TreeSet;
import java.util.Vector;

import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.config.ScaleConfig;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ZimFile;
import org.sciviews.zooimage.gui.ZooImageGui;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.IJUtilities;
import org.sciviews.zooimage.tools.Timer;

/**
 * Superclass of all ZooImage plugins
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public abstract class ZooImagePlugin {
	
	// Conditions of use
	
	/**
	 * Minimum ImageJ version required
	 */
	public static final String minversion = "1.37v";
	
	/**
	 * Memory required, in Mb (have more actual RAM in your computer, of course!)
	 */
	public static final int needmemory = 400;
	
	/**
	 * Zim files to process, ordered by fraction
	 */
	private TreeSet<ZimFile> zimfiles ;
	
	/**
	 * contains calibration information:
	 * calibs, pixsize, pixelunit, whitepoint, blackpoint 
	 */
	public CalibrationData calibration ;
	
	/**
	 * Options about the process
	 * allfiles, analyzepart, makevigs, sharpenvigs, showoutline, ziptiff
	 */
	public ProcessOptions options ;
	
	/**
	 * Contains description of this plugin (author, version, description)
	 * read from the zooimage.config property file
	 */
	private ZooImagePluginDescriptor descriptor ;
	
	/**
	 * Constructor for a ZooImagePlugin process
	 * Sets the calibration and options with default
	 * values shared by all plugins. 
	 */
	public ZooImagePlugin(){
		descriptor = ZooImage.getDescriptor( 
				getClass().getSimpleName() ) ;
		
		// TODO: These should be in property files
		calibration = new CalibrationData( 
				null,    // calibs                                   
				0.01058, // pixsize
				"mm", 	 // pixelunit
				2000,    // whitepoint
				53000    // blackpoint
				) ;
		options = new ProcessOptions() ;
		
	}
	
	/**
	 * Returns the file extension considered to be raw image by this
	 * plugin. *.tif is the default for most plugins, but the Scanner_Color
	 * plugin used *.jpg files instead
	 * 
	 */
	public String getRawImageExtension(){
		return FileExtensions.extTIF ;
	}
	
	
	/**
	 * The about box for this plugin
	 */
	public void showAbout() {
		ZooImage.showAbout( descriptor.getName() ) ; 
	}

	/**
	 * Process a single ZIM file
	 * @param file zim file to process
	 * @param remote remote directory where to store the results
	 */
	public void processSingleZimFile( String file, File remote ){
		zimfiles = new TreeSet<ZimFile>() ;
		try {
			zimfiles.add( new ZimFile(file, remote, this) ) ;
		} catch( ZooImageException e){
			e.log() ;
		}
		process() ;
	}
	
	/**
	 * Process All Zim files in the directory
	 * @param dir directory to process
	 * @param remote remote directory where to store the results
	 */
	public void processDirectory(String dir, File remote ){
		zimfiles = ZimFile.getZimFiles( dir, remote, this ); 
		process() ;
	}
		
	/**
	 * Returns the scale configuration to use for creating vignettes
	 * @param width width
	 * @param pixsize size of pixel
	 */
	public ScaleConfig getScaleConfig( int width, double pixsize ){
		
		int which = ScaleConfig.MEDIUM ;
		if( width < (0.7/pixsize) ){
			which = ScaleConfig.SMALL ;
		} 
		if( width > (1.5/pixsize) ){
			which = ScaleConfig.LARGE ;
		}
		
		ScaleConfig config = null ;
		if( which == ScaleConfig.SMALL ){
			config =  new ScaleConfig(0.3, 2, 8) ;
		} else if( which == ScaleConfig.MEDIUM ) {
			config =  new ScaleConfig(0.5, 3, 10) ;
		} else if( which == ScaleConfig.LARGE){
			config =  new ScaleConfig(1.0, 4, 11) ;
		} 
		return config;
	}
	
	/**
	 * <p>This is the main function. It process all the Zim files.
	 * The actual work is done by the process method
	 * of the ZimFileProcessor class (or classes that extend it)</p>
	 * 
	 * <p>This method mainly start a timer, attempts to process each 
	 * file of the set of files, and log when it is done</p>
	 * 
	 * @see ZimFileProcessor#process()
	 */
	public void process(){
		
		// Start the actual process
		Timer timer = new Timer() ; 
		
		Log.log( "===== ZooImage1 " + descriptor.getName() + " v. " + descriptor.getVersion() + " =====");
		
		int i=0; 
		for( ZimFile zf: zimfiles){
			i++; 
			
			Log.log( "Processing zim file: ["+i+"/"+zimfiles.size()+"] " + zf.getName() ) ;
			
			// Process the current zim file
			try{
				new ZimFileProcessor( zf, this ).run() ;
				timer.addOperation() ;
			} catch( Exception e){
				Log.error( e.getMessage() ) ;
			}
		}
		
		// Logging 
		Log.log( timer.getCount() + "/" + zimfiles.size() + 
				" file(s) correctly processed in " + 
				timer.getElapsedMinutes() + " min");
		Log.log("Mean time per successfully processed file: " + 
				timer.getAverageSeconds() + " sec");

	}
	

	/**
	 * Returns the descriptor of the process
	 * @return the descriptor
	 */
	public ZooImagePluginDescriptor getDescriptor() {
		return descriptor;
	}
	
	
	/**
	 * Returns the list of parameters to send to the DAT file
	 * @return
	 */
	 protected Vector<String> getParams(){
		 Vector<String> parameters = new Vector<String>() ;
		 parameters.add( "Process=" + descriptor.getName() ) ;
		 parameters.add( "Version=" + descriptor.getVersion() ) ;
		 calibration.fill( parameters ) ;
		 return parameters ;
	 }
	
	 
	
	/**
	 * Called when the plugin is used from ImageJ menu
	 */
	public void run() {
		
		// Sets the Logging to ImageJ mode 
		Log.setMode( Log.IMAGEJ ) ;

		
		// checks that ImageJ is able to work
		if (!IJUtilities.verifyState(this)) return;
		
		ZooImagePluginGui gui = getGui() ;
		
		try{
			gui.run();
		} catch( Exception e){
			return ;
		}
		
		// Start processing
		process( ) ;
		
	}
		
	/**
	 * The gui object that parameterizes this plugin
	 * @return
	 */
	public abstract ZooImagePluginGui getGui() ;
	
	/**
	 * Returns the calibration information associated with this plugin
	 * @return the calibration information
	 */
	public CalibrationData getCalibration() {
		return calibration ;
	}

	/**
	 * Returns the options associated with this plugin
	 * @return the options
	 */
	public ProcessOptions getOptions() {
		return options ;
	}

	
	public abstract String[] getMethods() ;
	
}
