package org.sciviews.zooimage.files;

import ij.IJ;
import ij.ImagePlus;
import ij.io.FileOpener;

import java.io.File;
import java.util.TreeSet;

import org.sciviews.zooimage.config.CalibrationData;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.FileUtilities;
import org.sciviews.zooimage.tools.Images;

public class ImageFile  implements Comparable<ImageFile> {

	private static final long serialVersionUID = 1L;

	/**
	 * The file of the image
	 */
	private File file ;
	
	/**
	 * Extension of the file
	 */
	private String extension ; 
	
	/**
	 * Directory associated with the image
	 */
	private String dir ;
	
	/**
	 * Zim file associated with the image
	 */
	private ZimFile zim ;
	
	/**
	 * Constructor
	 * @param path file path of the image
	 * @param extension extension of the image
	 * @param dir directory where the image is
	 * @param zim Zim file associated with the image
	 */
	public ImageFile( String path, String extension, String dir, ZimFile zim ){
		this.file = new File( path ) ;
		this.extension = extension ;
		this.setDir(dir);
		this.zim = zim ;
		
		Log.debug( "image: " + file.getAbsolutePath()) ;

	}
	
	/**
	 * Moves this image to the raw directory. If this is a RAW image already, just 
	 * moves it to the RAW directory. If not, saves it as a RAW image 
	 * to the RAW directory
	 */
	public void moveToRawDirectory( ){
		
		File SaveFile = new File( 
			zim.getRawDirectory() + File.separator + 
			FileUtilities.replaceExtension( file.getName(), extension, FileExtensions.extRAW) ); 
		
		if( !SaveFile.exists() ){
			if( extension == FileExtensions.extRAW ){
				// If this is already a RAW image, then just move it across
				file.renameTo(SaveFile) ;
			} else {
				// If not a RAW image, then we need to open it and save it as a RAW image
				FileOpener.setShowConflictMessage(false) ;
				ImagePlus im = IJ.openImage(file.getAbsolutePath()) ;
				IJ.save(im, SaveFile.getAbsolutePath()) ;
				im.close();
				file.delete() ;
			}
		}
		
		this.file = SaveFile ; 
		this.extension = FileExtensions.extRAW ;
		this.setDir(zim.getRawDirectory());
		
	}
	
	/**
	 * Returns the set of images related to the zim file
	 * The following possibilities are checked on after the other. 
	 * The first one that has at least on image wins: 
	 * 
	 * <ul>
	 * <li>TIF images in the directory of the zim file</li>
	 * <li>RAW (zipped) images in the raw directory of the zim file</li>
	 * <li>RAW (zipped) images in the remote directory associated with the zim file</li>
	 * <li>RAW (zipped) images in the work directory of the zim file</li>
	 * </ul>
	 * 
	 * @param zimFile a zim file for which we want associated images
	 * @return the set of images associated with the zim file
	 */
	public static TreeSet<ImageFile> getImageFiles(ZimFile zim ) {
		TreeSet<ImageFile> images = new TreeSet<ImageFile>() ;
		
		append( images, zim.getPlugin().getRawImageExtension(), zim.getDirectory(), zim ) ;
		if( images.size() > 0) return images ;
		
		append( images, FileExtensions.extRAW, zim.getRawDirectory(), zim ) ; 
		if( images.size() > 0) return images ;
		
		append( images, FileExtensions.extRAW, zim.getRemoteDirectory() , zim ) ; 
		if( images.size() > 0) return images ;
		
		append( images, FileExtensions.extRAW, zim.getWorkDirectory(), zim ) ;
		if( images.size() > 0) return images ;
		
		/**
		 * append( images, zim.getPlugin().getRawImageExtension(), zim.getWorkDirectory(), zim ) ;
		if( images.size() > 0) return images ;
		
		append( images, zim.getPlugin().getRawImageExtension(), zim.getRemoteDirectory() , zim ) ; 
		 */
		
		return images ;
	}

	/**
	 * Appends image file with the given extension from the 
	 * given directory, and return the total number of images
	 * in the set so far
	 * 
	 */
	private static void append( TreeSet<ImageFile> images, 
			String extension, String dir, ZimFile zim){
		
		Log.log("Look for '"+ extension +"' images in '" + dir + "'" );
    	String[] ims = FileUtilities.listFiles( dir, extension, zim.getSample() ) ; 
    	Log.log("Found "+ ims.length  +" images " );
    	for( String image: ims){
			images.add( new ImageFile( dir + File.separator + image, extension, dir, zim ) ) ;
		}
	}

	/**
	 * @param file the file to set
	 */
	public void setFile(File file) {
		this.file = file;
	}

	/**
	 * @return the file
	 */
	public File getFile() {
		return file;
	}

	/**
	 * Implementation of Comparable
	 */
	@Override
	public int compareTo(ImageFile o) {
		return getFile().compareTo( o.getFile( ) ) ;
	}

	/**
	 * Opens the raw image
	 * 
	 * @return true if the raw image was loaded and activated
	 */
	public ImagePlus open() throws CouldNotOpenRawImageException {
		Log.debug( "Opening raw image " + file.getAbsolutePath() ) ; 
		FileOpener.setShowConflictMessage(false) ;
		ImagePlus imp = IJ.openImage( file.getAbsolutePath() ) ;
		Log.debug( imp.toString() ) ;
		Log.debug( "... ok" ) ; 
		imp.setTitle( Images.imgRAW ) ;
		
		return imp ;
	}

	/**
	 * @param dir the dir to set
	 */
	public void setDir(String dir) {
		this.dir = dir;
	}

	/**
	 * @return the dir
	 */
	public String getDir() {
		return dir;
	}

	/**
	 * The zim file associated with this image
	 * 
	 * @return the zim file
	 */
	public ZimFile getZim() {
		return zim ;
	}
	
	/**
	 * The calibration information of the plugin that called
	 * the zim file calling this image
	 * 
	 * @return calibration information
	 */
	public CalibrationData getCalibration(){
		return getZim().getPlugin().getCalibration() ;
	}
	
	
}
