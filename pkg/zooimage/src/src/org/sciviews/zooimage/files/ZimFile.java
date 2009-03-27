package org.sciviews.zooimage.files;

import ij.measure.ResultsTable;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.regex.Pattern;

import org.sciviews.zooimage.ZooImagePlugin;
import org.sciviews.zooimage.exceptions.ZooImageException;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.BufferUtilities;
import org.sciviews.zooimage.tools.Directories;
import org.sciviews.zooimage.tools.FileExtensions;
import org.sciviews.zooimage.tools.FileUtilities;

/**
 * Representation of ZIM files. 
 * <p>A zim file name is made of the following parts: 
 * <ul>
 * 	<li>the sample name</li>
 *  <li>A + character</li>
 *  <li>the fraction name</li>
 *  <li>the zim extension</li>
 * </ul>
 * 
 * @author Romain Francois <francoisromain@free.fr>
 */
public class ZimFile implements Comparable<ZimFile> {
	
	/** 
	 * Zim file
	 */
	private File file ;
	
	/**
	 * Set of images associated with this Zim file
	 */
	private TreeSet<ImageFile> imageFiles ;
	
	/**
	 * The sample name of the zim file, eg everything of the name of the file
	 * up to the last +
	 */
	private String sample ;
	
	/**
	 * The fraction name (between the last + and the dot)
	 */
	private String fraction ;
	
	/**
	 * Pattern to recognize the first line of a Zim file
	 */
	private static final Pattern FIRST_LINE = Pattern.compile( "ZI1" ) ;
	
	/**
	 * Remote directory 
	 */
	private File remoteDir ;
	
	/**
	 * Zoo Image plugin that has invoked this zim file
	 */
	protected ZooImagePlugin plugin ;
	
	
	/**
	 * Constructor
	 * @param f the path of the zim file 
	 * @throws IncorrectZimFileException when the Zim file is not correct
	 * @throws WorkDirectoryIsNotDirectoryException when the Work directory exists but is a file
	 */
	public ZimFile( String f, File remoteDir, ZooImagePlugin plugin ) throws IncorrectZimFileException, WorkDirectoryIsNotDirectoryException{
		file = new File( f ) ;
		this.plugin = plugin ;
		check(file) ;
		
		String name = getName() ;
		sample = name.substring(0, name.lastIndexOf('+')) ;
		fraction = name.substring(name.lastIndexOf('+')+1, name.lastIndexOf('.') ) ;
		
		createDirectory( Directories.wrkDir ) ;
		createDirectory( Directories.rawDir ) ;
		createDirectory( getSampleDir( ) ) ;
		
		// copyToDatFile( );
		
		setRemoteDir( remoteDir ) ;
		
		imageFiles = ImageFile.getImageFiles(this) ; 
		
		debug();
		
	}

	private void debug() {
		Log.debug( "zim file     : " + file.getAbsolutePath() ) ;
		Log.debug( "  sample     : " + sample  ) ;
		Log.debug( "  fraction   : " + fraction ) ;
		Log.debug( "  remote dir : " + remoteDir.getAbsolutePath() ) ;
		Log.debug( "  images     : " + imageFiles.size() ) ;
	}
	
	/**
	 * Returns the zoo image plugin that invoked this zim file
	 * @return the ZooImagePlugin
	 */
	public ZooImagePlugin getPlugin() {
		return plugin;
	}
	
	/**
	 * Returns the directory that contains the zim file
	 * @return the directory name
	 * @see File#getCanonicalPath()
	 */
	public String getDirectory(){
		String dir = "" ; 
		try{
			dir = file.getParentFile().getCanonicalPath() ; 
		} catch( Exception e){
			Log.error( e.getMessage() ) ;
		}
		return dir ;
	}
	
	/**
	 * The basename of the zim file
	 * @return the name of the file
	 * @see File#getName()
	 */
	public String getName(){
		return file.getName() ;
	}
	
	/**
	 * Returns the fraction (between the last + and the extension dot)
	 * @return the name of the fraction
	 */
	public String getFraction(){
		return fraction ;
	}
	
	/**
	 * Returns the sample (sample+fraction)
	 * @return the sample name 
	 */
	public String getSample(){
		return sample + "+" + fraction ;
	}
	
	/**
	 * Returns the sample name (without the fraction)
	 * @return
	 */
	public String getSampleDir(){
		return sample ;
	}
	
	/**
	 * Returns the file
	 * @return the actual file
	 */
	public File getFile() {
		return file;
	}
	
	/**
	 * Checks if a file is a ZIM file
	 * @throws IncorrectZimFileException when a problem occurs
	 */
	public static void check( File file) throws IncorrectZimFileException {
		
		Log.debug( "checking zim file: " + file.getAbsolutePath() ) ; 
		
		// the file must not be a directory
		if( file.isDirectory() ){
			throw new IncorrectZimFileException( "Zim file cannot be directories" ) ;
		}
		
		// needs the .zim extension
		if( !FileUtilities.hasExtension(file, FileExtensions.extZIM) ) {
			throw new IncorrectZimFileException("Zim file must have the '" + FileExtensions.extZIM + "' extension") ; 
		}

		// first line must be ZI1
		String first = BufferUtilities.getFirstLine(file) ;
		Log.debug( "First line of zim file: " + first ) ;
		if( ! FIRST_LINE.matcher(first).find() ){
			throw new IncorrectZimFileException( "Zim files must have 'ZI1' in their first line" ) ;
		} 
		
		// must contain a "+" in the name
		if( !file.getName().contains( "+" )){
			throw new IncorrectZimFileException("Zim file must have a + in their name") ; 
		}
		
	}

	/**
	 * Returns a Set of ZimFile in the directory
	 * Gets the files with the zim extension in the directory 
	 * and tries to add them. 
	 * The files are ordered by fractions
	 * 
	 * @param dir directory that is supposed to contain zim files
	 * @param remoteDir remote directory to associate with each of the files
	 * @return a set of ZimFile
	 */
	public static TreeSet<ZimFile> getZimFiles(String dir, File remoteDir, ZooImagePlugin plugin){
		TreeSet<ZimFile> set = new TreeSet<ZimFile>() ;
		
		String[] files = FileUtilities.listFiles(dir, FileExtensions.extZIM) ;
		for( String f: files){
			try{
				ZimFile zf = new ZimFile( dir + File.separator + f, remoteDir, plugin);
				set.add( zf ) ;
			} catch( ZooImageException e){
				e.log() ;
			}
		}
		return set ;
	}

	/**
	 * Compares the base name of the two zim files, used to store 
	 * zim files in the correct order when processing a directory
	 */
	@Override
	public int compareTo(ZimFile o) {
		return getName().compareTo( o.getName() ) ;
	}
	
	/**
	 * Extracts the properties from the Zim file
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public HashMap<String,String> extractProperties() throws IOException {
		HashMap<String,String> map = new HashMap<String,String>() ;
		BufferedReader buf = new BufferedReader(new FileReader( file ));
		
		String txtline = "";
		String key = "";
		String value = "";
		int sepIndex = 0;
		while (true) {
			txtline = buf.readLine();
			if (txtline == null) break;
			// Look if it contains something like 'key = value' and extract 'key' and 'value'
			sepIndex = txtline.indexOf("=");
			if (sepIndex >= 0) {
				key = txtline.substring(0, sepIndex).trim();
				value = txtline.substring(sepIndex + 1, txtline.length()).trim();
				map.put( key, value) ;
			}
		}
		return map ;
	}
	
	/**
	 * Copy this file, append process parameters, then measurements to cazfile
	 * if the file already exist, measurements will be appended to the file
	 * 
	 * @param parameters Parameters to write to the file
	 * @param count 
	 * @param txtfile Text file to append
	 * @return the file path of the created file
	 */
	public void makeDATFile(Vector<String> parameters, ResultsTable table, int count ) {
		
		String output = getDirectory() + File.separator + 
			Directories.wrkDir + File.separator + 
			getSample() + "." + count + FileExtensions.extDAT ;
		
		Log.debug( "saving zim file to " + output ) ;
		
		try{
			
			// The output file
			BufferedWriter out = new BufferedWriter(new FileWriter(output));

			// Write data from zim file
			BufferUtilities.copy( new BufferedReader(new FileReader( getFile() )) , out );

			// Write parameters data
			out.write("\n");
			out.write("[Process]\n");
			BufferUtilities.writeLines( out, parameters) ;
			out.write("\n");

			out.write("[Data]\n");
			out.write("!Item\tLabel");
			out.write( table.getColumnHeadings( ) + "\n" );

			// Write data from the txt file
			for( int i=0; i<table.getCounter(); i++){ 
				table.setLabel(getSample(), i);
				String line = table.getRowAsString(i) ;
				Log.debug( "::: " + line) ;
				out.write( line  + "\n" );
			}
			
			// close the buffer
			out.close();
			
		} catch( Exception e){
			Log.error( e.getMessage() ) ;
		}
		
		if (!FileUtilities.fileExists(output) ) {
			Log.log("ERROR: saving measurements " + output);
		}
		
	}
	
	/**
	 * Creates the subdirectory dir of the directory of this zim file if it does not 
	 * already exist
	 * @param dir name of the directory to create
	 */
	public void createDirectory(String dir) {
		File work = new File( getDirectory() + File.separator + dir ) ;
		if( ! work.exists() ){
			work.mkdir() ;
		} 
	}
	
	
	/**
	 * Copies the ZIM file to a dat file in the work directory
	 */
	public void copyToDatFile(){
		try{
			createDirectory( Directories.wrkDir ) ;
			String datfile = getWorkFile( getSample() + FileExtensions.extDAT ) ;
			if( new File( datfile).exists() ) return ;

			FileUtilities.copyFile( file.getCanonicalPath(), datfile ) ;
		} catch( Exception e){
			Log.error( e.getMessage() ) ;
		}
	}
	
	/**
	 * Returns the path name of a file in the work directory
	 * @param s basename of the file
	 * @return full name of the file, in the work directory
	 * @see Directories#wrkDir
	 */
	public String getWorkFile( String s ){
		String workfile = getDirectory() + File.separator + 
			Directories.wrkDir + s  ;
		return workfile ;
	}
	
	/**
	 * Returns the path name of a file in the raw directory
	 * @param s basename of the file
	 * @return full name of the file, in the raw directory
	 * @see Directories#rawDir
	 */
	public String getRawFile( String s ){
		String workfile = getDirectory() + File.separator + 
			Directories.rawDir + s ;
		return workfile ;
	}
	
	
	/**
	 * Returns the set of images related to this zim file
	 * @return the set of images related to this zim file
	 */
	public TreeSet<ImageFile> getImageFiles() {
		return imageFiles;
	}

	/**
	 * Returns the raw directory associated with this zim file
	 * Creates it if needed
	 * @return
	 */
	public String getRawDirectory() {
		String rawdir = getDirectory() + File.separator + Directories.rawDir ;
		(new File(rawdir)).mkdir() ;
		return rawdir ;
	}
	
	/**
	 * Returns the work directory associated with this zim file
	 * Creates it if needed
	 * @return
	 */
	public String getWorkDirectory() {
		String workdir = getDirectory() + File.separator + Directories.wrkDir ;
		(new File(workdir)).mkdir() ;
		return workdir ;
	}

	/**
	 * @param remoteDir the remoteDir to set
	 */
	public void setRemoteDir(File remoteDir) {
		
		if( remoteDir == null || ! remoteDir.isDirectory() ){
			this.remoteDir = new File(getWorkDirectory()) ;
		} else{
			this.remoteDir = remoteDir ;
		}
	}

	/**
	 * @return the remoteDir
	 */
	public File getRemoteDir() {
		return remoteDir;
	}

	/** 
	 * Returns the remote directory, as a String
	 * @return the remote directory (as a String)
	 */
	public String getRemoteDirectory() {
		return getRemoteDir().getAbsolutePath() ;
	}
	
	/**
	 * Checks if the zim file has associated images of the following type
	 *
	 * @return true if this zim file has an image of the given type
	 */
	public boolean hasImages( String type, int count ){
		return getImage(type, count) != null ;
	}
	
	/**
	 * Returns the file path of an image of the given type associated with this zim file
	 * @param type type of image
	 * @return the file path of the image
	 */
	public String getImage( String type, int count){
		
		if (type == FileExtensions.extRAW) { 	// We look for a RAW image
			return getRawImage( ) ;
		} else {	
			return getTypeImage( type, true, count ) ;
		}
		
	}

	
	public String getRawImage(){
		String path ;
		
		// Is it a tif image in the same directory as the .zim file?
		path = getDirectory() + File.separator + getSample() + plugin.getRawImageExtension() ; 
		if( FileUtilities.fileExists(path) ) return path ;

		// Look in the _raw subdir for a .zip file
		path = getDirectory() + File.separator + Directories.rawDir + 
			getSample() + FileExtensions.extRAW ;
		if( FileUtilities.fileExists(path) ) return path ;

		// Look in the remotedir
		path = getRemoteDirectory() + File.separator + getSample() + FileExtensions.extRAW ;
		if( FileUtilities.fileExists(path) ) return path ;
		
		// give up
		return null ;
	}
	
	public String getTypeImage(String type, boolean mustExist, int count ){
		String path ;
		String basename= getSample() + "." + (count+1) + type ;
		
		// Look in the remote directory
		path = getRemoteDirectory() + File.separator + basename ;
		if( FileUtilities.fileExists(path) ) return path ;

		// We look for a file in the _work directory
		path = getDirectory() + File.separator + Directories.wrkDir + basename ;
		if( !mustExist || FileUtilities.fileExists(path) ) return path ;
		
		// give up
		return null ;
	}

	
}
