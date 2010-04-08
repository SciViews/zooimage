package org.sciviews.zooimage.files;

import ij.IJ;
import ij.ImagePlus;
import ij.io.FileOpener;

import java.io.File;
import java.lang.reflect.Constructor;
import java.util.HashMap;

import org.sciviews.zooimage.ImageFileProcessor;
import org.sciviews.zooimage.ZooImageMacro;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.Images;

/**
 * Secondary Images associated to a raw image.
 * 
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public abstract class SecondaryImage {

	private static HashMap<String,String> extensions ;
	static{
		extensions = new HashMap<String,String>() ;
		extensions.put( "VIS", FileExtensions.extVIS) ;
		extensions.put( "ODC", FileExtensions.extODC) ;
		extensions.put( "MSK", FileExtensions.extMSK) ;
		extensions.put( "OUT", FileExtensions.extOUT) ;
	}


	/**
	 * Zim file associated with the image
	 */
	protected ZimFile zim ;

	/**
	 * Raw image this is derived from
	 */
	protected ImageFile image ;

	/**
	 * The file associated with this secondary image
	 */
	protected File file ;

	/**
	 * The processor that deals with the raw image associated
	 * with this secondary image
	 */
	protected ImageFileProcessor processor; 

	/**
	 * Constructor 
	 * @param image the raw image this is derived from
	 */
	public SecondaryImage( ImageFileProcessor processor ){
		this.processor = processor ;
		this.image = processor.getImage() ;
		this.zim = image.getZim() ;
		this.file = new File( zim.getTypeImage( getExtension() , false, processor.getZimProcessor().getCount() ) ) ;
	}

	/**
	 * The processor that deals with the raw image
	 * associated with this secondary image
	 * 
	 * @return the raw image processor
	 */
	public ImageFileProcessor getProcessor() {
		return processor;
	}

	/**
	 * The file extension of this type of image 
	 * @return the file extension
	 */
	public String getExtension( ){
		String type = getType( ) ; 
		if( ! extensions.containsKey( getType() )){
			return null ;
		} 
		return extensions.get(type) ;
	}

	/**
	 * Type of this secondary image
	 * @return
	 */
	public abstract String getType( ) ;

	/**
	 * Gets the preferred title of this image
	 */
	public String getTitle(){
		return Images.get( getType() ) ;
	}

	public ImagePlus open( ) throws ZooImageException{
		return open( getTitle( ) ) ;
	}


	/**
	 * Opens the image and set its title
	 * 
	 * @param title a title to give to the window, if null, this is not used
	 * @return the ImagePlus created
	 * @throws ZooImageException if the macro is not available of it is fails
	 */
	public ImagePlus open( String title ) throws ZooImageException {
		ImagePlus imp = null ; 
		// this is where we need to make it 
		// this needs to call the appropriate macro
		if( !file.exists() ){

			ZooImageMacro macro = getMacro() ;
			if( macro == null ){
				throw new MissingMacroException(this) ;
			}

			imp = macro.process( processor ) ;
			if( imp == null ){
				throw new FailingMacroException( this ) ;
			}
			save( imp ) ;

		} else {
			FileOpener.setShowConflictMessage(false) ;
			imp = IJ.openImage( file.getAbsolutePath() ) ;
		}

		// set the title if needed
		if( title != null){
			imp.setTitle( title ) ;
		}

		return imp ;

	}

	/**
	 * Indicates if this image exists
	 * 
	 * @return true if the secondary image already exists 
	 */
	public boolean exists(){
		return file.exists() ;
	}


	/**
	 * Returns the Zoo image macro that makes this secondary image
	 * 
	 * @return this returns a dummy zoo image macro
	 */
	public ZooImageMacro getMacro(){

		Log.debug( "Loading macro for image [" + getType() + "] : " + file.getAbsolutePath() ) ;

		String pname = image.getZim().getPlugin().getDescriptor().getName() ;
		String classname = "org.sciviews.zooimage.macros." + pname.toLowerCase() + "." + pname + "_" + getType() ;
		ZooImageMacro macro ;

		try{
			macro = getMacro( classname ) ;
		} catch( Exception e ){

			// Try one level up
			try{
				macro = getMacro( "org.sciviews.zooimage.macros.ZooImageMacro_" + getType() ) ;
			} catch( Exception e2){

				// use a dummy macro
				macro = new ZooImageMacro( ){
					public ImagePlus run(ImageFileProcessor processor){
						return null; 
					}
				} ;
			}

		}
		Log.debug( "... " + macro ) ;

		return macro ; 
	}

	@SuppressWarnings("unchecked")
	private static ZooImageMacro getMacro( String classname) throws Exception {
		Log.debug( "Trying to load macro : " + classname ) ;
		Class<?extends ZooImageMacro> clazz = (Class<?extends ZooImageMacro>)Class.forName( classname ) ;
		Constructor<?extends ZooImageMacro> c_ = clazz.getConstructor((Class<?>[])null);
		ZooImageMacro macro =  c_.newInstance( (Object[])null ) ;
		return macro ;
	}


	/**
	 * This file
	 * @return this file 
	 */
	public File getFile() {
		return file ;
	}

	/**
	 * Saves the current image in ImageJ in the file of this secondary image
	 * 
	 */
	public void save( ImagePlus im) throws UnableToSaveImageException {
		try{
			IJ.save( im, file.getAbsolutePath() ) ;
		} catch( Exception e){
			throw new UnableToSaveImageException( im, this ) ;
		}
	}

	public void save( ) throws ZooImageException {
		ImagePlus im ; 
		try{
			im = IJ.getImage() ; 
		} catch( Exception e){
			throw new NoImageException( this ) ;
		}
		save( im ) ;
	}

}
