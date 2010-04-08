package org.sciviews.zooimage;

import ij.IJ;
import ij.ImagePlus;
import ij.measure.ResultsTable;
import ij.plugin.filter.ParticleAnalyzer;

import java.io.File;

import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.config.MeasurementsUtilities;
import org.sciviews.zooimage.config.ProcessOptions;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.files.CmbImage;
import org.sciviews.zooimage.files.ImageFile;
import org.sciviews.zooimage.files.MaskImage;
import org.sciviews.zooimage.files.OdcImage;
import org.sciviews.zooimage.files.OutImage;
import org.sciviews.zooimage.files.Vignette;
import org.sciviews.zooimage.files.VisImage;
import org.sciviews.zooimage.files.ZimFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.IJUtilities;
import org.sciviews.zooimage.tools.Images;
import org.sciviews.zooimage.tools.ZIJ;
import org.sciviews.zooimage.utils.Threshold;

/**
 * Class that processes one image
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class ImageFileProcessor {

	/**
	 * Image file to process
	 */
	protected ImageFile image ;
	
	/**
	 * Calibration information
	 */
	protected CalibrationData calibration ;
	
	/**
	 * Process options
	 */
	protected ProcessOptions options ;
	
	/**
	 * Zim file associated with the image
	 */
	protected ZimFile zim ;
	
	/**
	 * Processor for the Zim file
	 */
	protected ZimFileProcessor zimProcessor ;
	
	/**
	 * The plugin that called the zim file processor
	 * that called this image processor
	 */
	protected ZooImagePlugin plugin ;

	/**
	 * VIS image associated with the image being processed
	 */
	protected VisImage vis ;
	
	/**
	 * ODC image associated with the image being processed
	 */
	protected OdcImage odc ;
	
	/**
	 * MSK image associated with the image being processed
	 */
	protected MaskImage msk ;
	
	/**
	 * OUT image associated with the image being processed
	 */
	protected OutImage out ;
	
	/**
	 * CMB image associated with the image being processed
	 */
	protected CmbImage cmb ;
	
	/**
	 * Result table created by the analyze particles part
	 */
	protected ResultsTable results ;
	
	/**
	 * Constructor 
	 * @param image image file to process
	 * @param zimProcessor processor for the zim file associated with the image
	 */
	public ImageFileProcessor( ImageFile image, ZimFileProcessor zimProcessor ){
		this.image = image ;
		this.calibration = zimProcessor.getCalibration( ) ;
		this.options = zimProcessor.getOptions( );
		this.zim = zimProcessor.getZim() ;
		this.zimProcessor = zimProcessor ;
		this.plugin = zimProcessor.getPlugin( ) ;
		vis = new VisImage( this ) ;
		odc = new OdcImage( this ) ;
		msk = new MaskImage( this ) ;
		out = new OutImage( this ) ;
		cmb = new CmbImage( this ) ;
	}
	
	public ZimFileProcessor getZimProcessor() {
		return zimProcessor;
	}
	
	/**
	 * Runs this job (Process the image)
	 */
	public void run() throws ZooImageException {
		
		// Make sure all images are closed
		IJUtilities.closeAllImages() ;
	
		// First, we need to check that we have all
		// necessary gif images, and if we don't we
		// need to make them
		Log.debug( "Checking gifs" ) ;
		boolean checkGif = vis.exists() ;
		if (options.get("useOD") && checkGif) {
			checkGif = odc.exists() ; 
		}
		Log.debug( "   ... ok" ) ;
		
		
		// If at least one is missing, we need the RAW image
		if (!checkGif) {
			
			// Make sure all the required .gif files are made
			processVIS() ;
			
			// Make the MASK image if needed
			processMSK() ;
			
			// Process ODC
			processODC() ;
			
		}
		
		// Now we can analyze the particles
		analyzeParticles() ;
		
		// Now we can make the vignettes
		// Should we make the vignettes?
		if ( options.get("makevigs" ) ){
			processVIG() ;
		}
		
		// Should we compute the combined visual + outline file (only the last image)
		if ( options.get("showoutline") && zimProcessor.isProcessingLastImage() ){
			processCMB() ;
		}
				
	}
		
	/**
	 * Makes the VIS image from the RAW image
	 */
	public void processVIS() throws ZooImageException {
		
		if( vis.exists() ) return ;
		
		Log.debug( " <processVIS>" ) ;
		
		// Does the actual processing, specific to each implementation
		vis.getMacro().processAndSave(this, vis) ;
		
		Log.debug( " </processVIS>" ) ;
		
	}
	
	/**
	 * Makes the MSK image from either the VIS image
	 * or the RAW image (depending on the "maskFromVis" option)
	 */
	public void processMSK( ) throws ZooImageException {
		
		if( msk.exists() ) return ;
		
		Log.debug( " <processMSK>" ) ;
		
		msk.getMacro().processAndSave(this, msk)  ;
		
		Log.debug( " </processMSK>" ) ;
		
	}
	
	
	public void processODC() throws ZooImageException {
		
		// No need for this if we don't use it!
		if (!options.get("useOD") | odc.exists() ) return ;	
		
		Log.debug( " <processODC>" ) ;
		
		// call the actual treatment
		odc.getMacro().processAndSave(this, odc) ;
	
		Log.debug( " </processODC>" ) ;

	}
	
	public void analyzeParticles() throws ZooImageException {
			
			// Analyze particles, using a mask (ImgMask), and an O.D. calibrated image (ImgOD)
			ImagePlus mask_image = msk.open( zim.getSample() ) ;
			mask_image.setTitle( zim.getSample() ) ;
			if( Log.getMode() == Log.IMAGEJ){
				mask_image.show() ;
			}
			IJ.freeMemory();
			
			// Calibrate the picture in length (and possibly in grey levels)
			ZIJ.run(mask_image, "Set Scale...", "distance=1 known=" + calibration.getPixsize() + 
					" pixel=1 unit=" + calibration.getPixelunit() + " global");
			
			// Apply the 0-128 threshold
			Threshold.applyThreshold(mask_image, 0, 128, false) ;
			
			// We need minsize and maxsize in pixels, and it is currently in ECD (pixelunit) => convert
			int pixminsize = calibration.getMinsizeAsPixel() ; 
			int pixmaxsize = calibration.getMaxsizeAsPixel() ;
			
			results = new ResultsTable() ;
			
			ParticleAnalyzer pa = new ParticleAnalyzer( 
					ParticleAnalyzer.SHOW_OUTLINES + ParticleAnalyzer.INCLUDE_HOLES , 
					MeasurementsUtilities.getMeasurements( calibration.getPmes() ), 
					results, pixminsize, pixmaxsize ) ;
			pa.setHideOutputImage(true);
			pa.analyze( mask_image ) ;
			out.save( pa.getOutputImage() ) ;
			if( Log.getMode() == Log.IMAGEJ){
				results.show("results" ) ;
			}
			mask_image.close();
			
		}
	
	
	/**
	 * Combine a visual and an outline for easier diagnostic of 
	 * segmentation and particle analysis
	 *
	 */
	public void processCMB( ) throws ZooImageException {
	
		Log.debug( " <processCMB>" ) ;
		
		// call the actual treatment
		cmb.getMacro().processAndSave(this, cmb) ;
	
		Log.debug( " </processCMB>" ) ;
		
	}
	
	
	/**
	 * Make vignettes for all blobs identified by the particles analysis
	 */
	public void processVIG() throws ZooImageException {
		
		// A table of measurements must be opened!
		if( results == null){
			throw new EmptyTableException( ) ;
		}
		int n = results.getCounter();
		if (n == 0) {
			throw new EmptyTableException( ) ; 
		}
		
		ImagePlus vis_imp = vis.open( Images.imgVIS ) ;
		vis_imp.setTitle(Images.imgVIS) ;
		if( Log.getMode() == Log.IMAGEJ){
			vis_imp.show( );
		}
		IJ.freeMemory();
		
		// Copy the measurements table in this directory
		String pathZIM2 = zim.getDirectory() + File.separator + 
			zim.getSampleDir() + File.separator +
			zim.getSample() + FileExtensions.extDAT ;
		zim.makeDATFile( plugin.getParams(), getResults(), 
				getZimProcessor().getCount(), pathZIM2 ) ;
		// FileUtilities.copyFile( zim.getFile().getAbsolutePath() , pathZIM2 );
		
		// Possibly sharpen the vignettes
		if (options.get("sharpenvigs") ) {
			ZIJ.run(vis_imp, "Sharpen");
		}
		
		// Make sure the spatial calibration is correct and in pixelunit
		IJ.run(vis_imp, "Set Scale...", "distance=1 known=" + 
				calibration.getPixsize() + " pixel=1 unit=" + 
				calibration.getPixelunit() + " global");
		
		// Switch jpeg quality to 75
		IJ.run(vis_imp, "Input/Output...", "jpeg=75");
		
		// Make vignettes
		int nok = 0;
		for (int i = 0; i < n; i++) {
			Vignette vig  = new Vignette( image, i, vis_imp, results) ;
			if( vig.run() ){
				nok++ ;
			}
		}
		
		// Check result and report failure
		if (nok < n) {
			// Indicate how many vignettes are not created
			if (nok == 0) {
				throw new NoVignetteCreatedException( zim ) ;
			} else {
				throw new NotAllVignettesCreatedException( zim, n, nok ) ;
			}
		}
		
		vis_imp.close( ) ;
	}

	public ResultsTable getResults() {
		return results;
	}
	
	/**
	 * The raw image this processor is dealing with
	 * 
	 * @return the raw image
	 */
	public ImageFile getImage() {
		return image ;
	}
	
	/**
	 * VIS image associated with the image being processed
	 */
	public VisImage getVis(){
		return vis ;
	}
	
	/**
	 * ODC image associated with the image being processed
	 */
	public OdcImage getOdc() {
		return odc ;
	}
	
	/**
	 * MSK image associated with the image being processed
	 */
	public MaskImage getMsk() {
		return msk ;
	}
	
	/**
	 * OUT image associated with the image being processed
	 */
	public OutImage getOut(){
		return out;
	}
	
	/**
	 * CMB image associated with the image being processed
	 */
	public CmbImage getCmb(){
		return cmb ;
	}

	public ZooImagePlugin getPlugin() {
		return plugin ;
	}
	
}
