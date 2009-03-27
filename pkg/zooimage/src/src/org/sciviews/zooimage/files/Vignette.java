package org.sciviews.zooimage.files;

import ij.IJ;
import ij.ImagePlus;
import ij.measure.ResultsTable;

import java.io.File;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.config.ScaleConfig;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.log.Log;

public class Vignette {

	protected int index ;
	protected ImageFile raw ;
	protected ZimFile zim ;
	protected File file ;
	protected ImagePlus base ;
	protected ResultsTable results ;
	
	public Vignette( ImageFile raw, int index, ImagePlus base, ResultsTable results ){
		this.raw = raw; 
		this.index = index ;
		this.zim = raw.getZim() ;
		this.base = base ; 
		this.results = results ;
		
		file = new File( zim.getDirectory() + File.separator + 
				zim.getSampleDir() + File.separator + 
				zim.getSample() + "_" + (index+1) + ".jpg" ) ;
	}
	
	
	public boolean run() throws ZooImageException  {
		
		IJ.freeMemory();
		
		ZooImagePlugin plugin = raw.getZim().getPlugin() ;
		double pixsize = plugin.getCalibration().getPixsize() ;
		
		// Calculate a bounding rectangle 1.5 times larger than the object
		int BX, BY, Width, Height;
		Width = (int)(results.getValue("Width", index) / pixsize);
		Height = (int)(results.getValue("Height", index) / pixsize);
		BX = (int)(results.getValue("BX", index) / pixsize - Width / 4);
		BY = (int)(results.getValue("BY", index) / pixsize - Height / 4);
		Width = (int)(Width * 1.5);
		Height = (int)(Height * 1.5);
		
		// Copy this ROI in a new image
		base.setRoi(BX, BY, Width, Height) ;
		base.copy(false) ;
		ImagePlus vig = IJ.createImage("Vignette" + (index+1) , "8-bit White", Width, Height, 1);
		vig.paste() ;
		
		// Add a scale bar
		ScaleConfig config = plugin.getScaleConfig(Width, pixsize ) ;
		IJ.run(vig, "Scale Bar...", "width=" + config.getBarwidth() + " height=" + config.getBarheight() +
			" font=" + config.getFontsize() + " color=Black location=[Upper Right]");
		
		IJ.save(vig, file.getAbsolutePath() ) ;
		vig.close();
		
		Log.debug( "Created vignette ("+(index+1)+") : " + file.getParent() + "/" + file.getName() ) ;
		return file.exists() ;
	}

}
