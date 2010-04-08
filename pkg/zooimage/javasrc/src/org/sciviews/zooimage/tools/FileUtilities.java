package org.sciviews.zooimage.tools ;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.sciviews.zooimage.files.ZimFile;

/**
 * File Utility methods
 */
public class FileUtilities{
	
	/**
	 * The subdirectory where RAW images are located
	 */
	private static final String rawDir = "_raw" + File.separator;
	
	/**
	 * The working subdirectory for storing reworked images
	 */
	private static String wrkDir = "_work" + File.separator; 
	
	/**
	 * Get the name of a file, given its full path
	 * @param path full path 
	 * @return basename of the file
	 */
	public static String getFile(String path) {
		return((new File(path)).getName());
	}
	
	/**
	 * Calculate base dir: if dir is \_raw or \_work => choose parent dir
	 * @param dir 
	 * @return
	 */
	public static String getBaseDir(String dir) {
		String out = dir ;
		if (dir.toUpperCase().endsWith(rawDir.toUpperCase() + File.separator))
			out = dir.substring(0, dir.length() - 4 - File.separator.length());
		if (dir.toUpperCase().endsWith(wrkDir.toUpperCase() + File.separator))
			out = dir.substring(0, dir.length() - 8 - File.separator.length());
		return(out);
	}

	/**
	 * Make sure a directory ends with a file separator
	 * @param dir a directory name 
	 * @return the same directory name, with a file separator at the end
	 */
	@Deprecated
	public static String fsepDir(String dir) {
		String out ; 
		if ( dir.endsWith(File.separator)){
			out = dir ;
		} else{
			out = dir + File.separator ;
		}
		return(out);
	}

	/**
	 * Given a file, it returns its directory
	 * @param file file
	 * @return directory in which the file is
	 */
	public static String getDir(String file) {
		return (new File(file)).getAbsoluteFile().getParent() ;
	}

	/**
	 * This function makes sure that a given subdirectory exists in a basedirectory
	 * Otherwise it creates it (parent directory must exist)
	 * @param basedir
	 * @param subdir
	 * @return
	 */
	public static boolean makeDir(String basedir, String subdir) {
		
		File fileDir = new File(basedir + subdir);
		
		// Check if it exists and if it is a directory
		if (fileDir.exists()) return(fileDir.isDirectory()); // Return false if it is a file indeed
		
		// create it otherwise
		boolean success = fileDir.mkdir();
		return( success );
	}

	/**
	 * Checks if a directory exists (and is a directory, not a file)
	 * @param dir directory name
	 * @return true if dir is a directory
	 */
	public static boolean dirExists(String dir) {
		File fileDir = new File(dir);
		return( fileDir.exists() && fileDir.isDirectory() ) ;
	}

	/**
	 * Checks if a file exists
	 * @param file a file 
	 * @return true if the file exists
	 */
	public static boolean fileExists(String file) {
		return((new File(file)).exists());
	}

	/**
	 * Copy a file
	 * @param source file we want to copy
	 * @param dest destination
	 * @throws ZooImageIOException
	 */
	public static void copyFile(String source, String dest) throws ZooImageIOException {
		try{
			BufferedWriter out = new BufferedWriter(new FileWriter(dest));
			BufferedReader in  = new BufferedReader(new FileReader(source)) ;
			BufferUtilities.copy(in, out) ;
			out.close();
		} catch( IOException ioe){
			throw new ZooImageIOException( ioe ) ;
		}
		
	}
	
	
	/**
	 * Determines if a file is a .zim file. 
	 * Checks that the file is not a directory, that it has a zim extension
	 * and that the first line contains "ZI1"
	 * @param file file 
	 * @return true if this is a zim file
	 * @see ZimFile#check(File)
	 */
	@Deprecated
	public static boolean isZIMFile(String file) {
		try {
			ZimFile.check(new File(file)) ;
		} catch( Exception e){
			return false ;
		} 
		return true ;
	}
	
	
	/**
	 * Delete the file
	 * @param file file to delete
	 */
	public static void delete (String file){
		  new File(file).delete();
	}
	
	/**
	 * Rename source to target
	 * @param source
	 * @param target
	 * @throws IOException
	 */
	public static void renameFile( String source, String target) throws IOException {
		new File(source).renameTo( new File(target) ) ;
	}
	
	/**
	 * Checks if a file has the given extension
	 * @param file file to check
	 * @param extension extension to check
	 * @return true if the file has the given extension
	 */
	public static boolean hasExtension( File file, String extension){
		return file.getName().toUpperCase().endsWith(extension.toUpperCase()) ;
	}
	
	/**
	 * List files under the directory with the given extension and starting with the 
	 * sample name
	 * @param dir directory to filter
	 * @param extension file name extension
	 * @param sample sample
	 * @return array of file names 
	 */
	public static String[] listFiles( String dir, String extension, String sample){
		return new File(dir).list(new ZooImageFilenameFilter( extension, sample )) ;
	}
	
	/**
	 * List all files under the directory with the given extension
	 * @param dir directory to filter
	 * @param extension file name extension
	 * @return array of file names
	 */
	public static String[] listFiles( String dir, String extension){
		return new File(dir).list(new ZooImageFilenameFilter( extension )) ;
	}
	
	/**
	 * Replaces an extension by another one in a file name
	 * @param file file name
	 * @param extension present extension of the file
	 * @param newExtension new extension of the file
	 * @return the file with the modified extension
	 */
	public static String replaceExtension( String file, String extension, String newExtension){
		return file.substring(0, file.length() - extension.length()) + newExtension ;
	}

	/**
	 * Indicates if s1 and s2 are the same file path
	 * @param s1 a path
	 * @param s2 another path
	 * @return true if s1 and s2 are the same file path
	 */
	public static boolean samePath(String s1, String s2) {
		File f1 = new File( s1 ) ; 
		File f2 = new File( s2 ) ;
		try{
			return f1.getCanonicalPath().equals( f2.getCanonicalPath() ) ;
		} catch(Exception e){
			return f1.getAbsolutePath().equals( f2.getAbsolutePath() ) ;
		}
	}

	
}


