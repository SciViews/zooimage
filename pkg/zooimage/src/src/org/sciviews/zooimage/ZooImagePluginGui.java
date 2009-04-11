package org.sciviews.zooimage;

import ij.io.OpenDialog;

import java.io.File;

import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.ZimFile;

/**
 * Class that sets the parameters of the associated zoo image plugin
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public abstract class ZooImagePluginGui {

	protected ZooImagePlugin plugin; 
	protected ProcessOptions options ;
	protected CalibrationData calibration ;
	
	protected String zimfile; 
	protected String zimdir ;
	protected File remotedir; 
	
	public ZooImagePluginGui( ZooImagePlugin plugin ){
		this.plugin = plugin ;
		options = plugin.getOptions() ;
		calibration = plugin.getCalibration() ;
	}
	
	public void run() throws ZooImageException {
		
		openZimFileDialog() ;
		zimOptionsDialog();
		
		if( options.get("allfiles")){
			plugin.processDirectory(zimdir, remotedir ) ;
		} else{
			plugin.processSingleZimFile(zimfile, remotedir ) ;
		}
		
	}
	
	public void openZimFileDialog() throws ZooImageException {
		OpenDialog od = new OpenDialog("Open a .zim file...", "");
		zimdir = od.getDirectory();
		if (zimdir == null){
			throw new ZooImageException( "Empty Zim directory" ) ;
		}
		
		zimfile = od.getFileName();
		if (zimfile == ""){
			throw new ZooImageException( "Empty Zim file") ;
		}
		
		ZimFile.check ( new File( zimdir + zimfile) ) ; 
		
		remotedir = new File( zimdir ) ;
	}
	
	public abstract boolean zimOptionsDialog() ;
	
	
}
