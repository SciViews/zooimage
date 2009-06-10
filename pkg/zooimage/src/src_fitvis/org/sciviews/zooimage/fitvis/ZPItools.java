package org.sciviews.zooimage.fitvis; 


import ij.*;
import ij.process.*;
import ij.io.*;
import ij.plugin.PlugIn;
import ij.text.*; //TextWindow and TextPanel
import ij.util.*;  //access to Tools

import java.io.*; //for file handling
import java.util.*; //for Vector stuff
import java.text.*;  //for DateFormat or SimpleDataFormat
import java.util.zip.*; //for zipping
import java.net.URL; //for plugin path


/** 
 * This class collects  utility methods that are used commonly 
 *  in Zoo-Phyto Image processing steps.
 * <p> 
 * (1) Utility is provided in the following areas ...
 *  <ul> 
 *   <li>plugin introspection (plugin path)
 *   <li>basic file/directory handling (create, move, test, copy, read/write,zip, ...)
 *   <li>basic user preference storage and access
 *   <li>basic string handling (split, join, match, ...)
 *   <li>basic number-to-string and string-to-number handling
 *   <li>basic configuration handling (combination of Vector and String handling as key=value pairs)
 *   <li> basic data-in-vector handling  - note the "getData" series of methods 
 *        assume that the input vector is comprised of String Arrays of uniform
 *        length.  No test is made to ensure compliance.
 *   <li>basic array handling (sorting, locate value,..) 
 * </ul>
 * <p>
 * (2) This HTML help created using javadoc ala...
 *   <li>>cd [ImageJ]/plugins/ZooPhytoImage
 *   <li>>javadoc -author -d ZPIhelp ZPItools.java ZPIvData.java FIT_VIS.java FIT_IMS.java ZPIVirtualStackPlus.java ZPIVirtualStack.java ZPIconfig.java ZPIStackPlus.java
 *<p> 
 * (3) For more info see <a href = "http://www.sciviews.org/zooimage/">ZooPhytoImage</a>.
 * @author ZooPhytoImage Team
 * @author btupper@bigelow.org
 * @author phgrosjean@sciviews.org
 * @version 0.0
 * 2007-09-26 BT fixed a bug in getUniqIndex()
 */

public class ZPItools {

	/**
	 * ZPIsep is "." which is used to demarcate the components of sample info in the name 
	 * of the sample (such as BHD.2007-08-14.F).
	 */
	public  String ZPIsep = ".";
	/** The standard "_raw" directory name */
	public  String rawDirName = "_raw";
	/** The standard "_work" directory name */
	public  String workDirName = "_work";
	/** The standard "_mask" directory name - subdirectory of "_work" */
	public  String maskDirName = "_mask";
	/** The standard "_outline" directory name - subdirectory of "_work" */
	public  String outlineDirName = "_outline";
	/** The standard "_roi" directory name - subdirectory of "_work" */
	public  String roiDirName = "_roi";
	/** The number decimals to use in screen and file output for the ImageJ results */
	public  int decimals = 3;
	/** The vector to container user preferences */
	public  Vector <Object> vPrefs = new Vector<Object>();


	/** Constructor that will automatically load a preference set given the fully qualified filename */  
	public ZPItools(String prefFile){
		loadPrefs(prefFile);
	}

	public ZPItools(){
	}

	//---------
	//  Preference
	//---------
	/** gets a preference value as a string */
	public  String getPref(String name){
		return getKeyValue(name, vPrefs);
	}//getPref
	/** gets a preference as a boolean  (works for true/false and 1/0 */
	public  boolean getPrefBool(String name){
		String pref = getPref(name);
		return (pref.equalsIgnoreCase("true") || pref.equalsIgnoreCase("1"));
	}
	/** gets a preference as int */
	public  int getPrefInt(String key){
		return parseInt(getPref(key));
	}
	/** gets a preference as long */
	public  long getPrefLong(String key){
		return parseLong(getPref(key));
	}
	/** gets a preference as a double */
	public  double getPrefDouble(String key){
		return Tools.parseDouble(getPref(key));
	} 
	/** sets a preference */
	public  boolean setPref(String key, String value){
		return setKeyValue(key, value, vPrefs);
	}
	//* sets a preference as a double*/
	public  boolean setPref(String key, double value){
		return setKeyValue(key, toString(value), vPrefs);
	}
	//* sets a preference with a boolean*/
	public  boolean setPref(String key, boolean value){
		String s = "";
		return setKeyValue(key, s.valueOf(value), vPrefs);
	}
	/** prints the contents of the vPrefs Vector */
	public void showPrefs(){
		for (int i=0; i< vPrefs.size();i++){IJ.log((String) vPrefs.get(i));}
	}
	/** given the preference filename (fully qualified with path!) read into vPrefs */
	public  boolean loadPrefs(String filename){
		if (vPrefs.size() != 0){vPrefs.clear();}
		readFileAsVector(filename, vPrefs);
		return !vPrefs.isEmpty();
	}
	/** saves the prefences to the specified file (be sure to fully qualify with path!) */
	public  boolean savePrefs(String filename){
		return writeVectorToFile(filename, vPrefs);
	}

	//-------
	// File Handling
	//-------

	/**
	 * Returns the fully qualified path to this plugin - as a private method
	 *
	 * @return a String of the fully qualified path to this class
	 */
	public String getThisPluginPath(){
		String full = ZPItools.class.getResource("ZPItools.class").toString() ;  
		String path = full.substring( full.indexOf(':')+1 ) ; // remove the "jar:"
		path = path.substring(path.indexOf(':') + 1) ; // remove the file:
		path = path.substring(0, path.indexOf('!') ) ;        // remove the path within the jar file
		String pPath = (new File( path )).getParent() + File.separator;
		return pPath;
	}
	
	/** Returns the fully qualified path to this plugin
	 *
	 * @return a String of the fully qualified path to this class
	 */
	public String getPluginPath(){
		return getThisPluginPath();
	}

	/** Returns the fully qualified path to specified folder in
	 * this class path.
	 *
	 * @param directory A String of the subdirectory path. The path need not exist.
	 * @return a String of the fully qualified path to this directory
	 */
	public String getPluginPath(String directory){
		return getPluginPath() + directory + File.separator;
	}


	/** 
	 * Given a list of files and an extension, return the files that have that extension
	 * or some substring.  This is a convenience wrapper around matchSubstring.
	 *
	 * @param list The String array of filenames.
	 * @param ext the extension of substring to match.
	 * @return a string array of files that match the extension (or some substring)
	 * @see matchSubstring
	 */
	public  String[] getFiles(String[] list, String ext){
		return matchSubstring( list,  ext);
	} // end of getFiles

	/**
	 * Ensures that the provided directory path includes the system 
	 * path separator at the end.
	 *
	 * @param path the path to test
	 * @return a String path descriptor with or without separator at end
	 */
	public  String markDirectory(String path){
		return (new File(path)).getAbsolutePath() + File.separator;
	}  

	/**
	 * Lists all non-directory, non-hidden files in a directory
	 *
	 * @param thisDir the directory who contents are to be listed 
	 * @return a string array of the non-directory and non-hidden objects in the directory.  These do not
	 * contain the fully qualified path.
	 */
	public  String[] listFiles(String thisDir){
		return listFiles( new File(thisDir));
	}

	/**
	 * Lists all non-directory, non-hidden files in a directory
	 *
	 * @param thisDir the directory who contents are to be listed 
	 * @return a string array of the non-directory and non-hidden objects in the directory.  These do not
	 * contain the fully qualified path.
	 */
	public  String[] listFiles(File thisDir){
		int count = 0;
		String[] fullList = thisDir.list();
		boolean[] flag = new boolean[fullList.length];
		for (int i=0; i<flag.length;i++){
			File file = new File(fullList[i]);
			flag[i] = !file.isHidden() && !file.isDirectory();
			if (flag[i] == true) {count++;}
		}
		if (count==0){return null;}
		String[] trimmedList = new String[count]; 
		int j = 0;
		for (int i = 0; i<flag.length;i++){
			if (flag[i]==true){
				trimmedList[j] = fullList[i];
				j++;
			}
		}
		return trimmedList;
	}
	/**
	 * Test whether a file/directory exists
	 *
	 * @param name a String fully qualified file or directory name
	 * @returns boolean true if it exists
	 **/
	public boolean testFileExists(String name){
		if (name == null) {
			return false;
		}
		//make sure the directory is read/write accessible
		File test = new File(name);
		return test.exists();
	}

	/** 
	 * Test a file or directory for read/write access. 
	 *
	 * @param name a String fully qualified file or directory name
	 * @returns boolean true if read/write access possible 
	 **/
	public  boolean testFileAccess(String name){
		if (name == null) {
			return false;
		}
		//make sure the directory is read/write accessible
		File test = new File(name);
		//test for read/write
		if (test.canRead() == false) {
			IJ.log("Unable to read from file/directory - are permissions set to read?");
			return false;
		}
		if (test.canWrite() == false) {
			IJ.log("Unable to write to file/directory - are permissions set to write?");
			return false;
		}
		return true;
	}//end of testFileAccess

	/**
	 * Returns the image width, height and type - not efficient but very handy.
	 * Wayne Rasband showed how to do this on the list serve. Image type is as defined
	 * the ImagePlus class.  See list serve note on 5 Oct 2007.
	 * @param file the fully qualifed filename
	 * @return a 3 element integer array of width, height and type
	 */
	public int[] getFileImageDims( String file){
		int[] arr = new int[3];
		ImagePlus imp = IJ.openImage(file);
		//this is taken from an example Wayne Rasband described on the mailing list 
		//Fri, 5 Oct 2007 
		if (imp!=null) {
			arr[0] = imp.getWidth();
			arr[1] = imp.getHeight();
			arr[2] = imp.getType();
			imp.close();
		} else { 
			arr = null;
		}
		return arr;  
	}//getFileImageDims

	/** 
	 * Given a generic TextWindow will save the contents as a text file. No checking 
	 * is performed on the existence of the file and overwrite is automatic.
	 *
	 * @param tw the ImageJ TextWindow reference
	 * @param path the fully qualified filename 
	 */
	public  void saveTextWindow(TextWindow tw, String file){
		TextPanel tp = tw.getTextPanel();
		tp.saveAs(file);
	}//end of saveText

	/** 
	 * Returns contents of file as a string array, lines separated by "\n". 
	 * 
	 * @param filename the fully name of file to read
	 * @return a string array with one file line per element
	 */
	public  String[] readFileAsStrings(String filename){
		Vector v = readFileAsVector( filename);
		String[] arr = new String[v.size()];
		for (int i = 0; i<v.size();i++){arr[i] = (String) v.get(i);}
		return arr;
	}

	/** 
	 * Returns contents of file as a single string, lines separated by "\n". 
	 * 
	 * @param filename the fully name of file to read
	 * @return a string with file lines separated with "\n"
	 */

	public  String readFileAsString( String filename){
		Vector v = readFileAsVector( filename);
		String s = "";
		for (int i = 0; i<v.size() ; i++){
			s = s.concat((String)v.get(i)).concat("\n");
		}
		return s;
	}//end of readFileAsString

	/** 
	 * Returns a Vector of Strings with each line of the text file stored as element. 
	 * 
	 * @param filename the fully name of file to read
	 * @return a Vector with file lines separated with stored as String in each element
	 */ 
	public  Vector readFileAsVector( String filename){
		Vector <Object> v = new Vector<Object>();
		File file = new File( filename );
		String thisName = file.getName();
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			while (in.ready() == true){
				v.add(in.readLine());           
			}
			in.close(); 
		} catch (FileNotFoundException e){
			IJ.showStatus("Error - file not found");
			return null;
		} catch (IOException e){
			IJ.showStatus("Error - input output error");
			return null;
		}     
		return v;
	} //readFileAsVector

	/** 
	 * Adds lines of text in a file to the provided Vector v.
	 *
	 * @param filename the fully name of file to read
	 * @param v the vector to which the liines are appended
	 * @return boolean flag indicating success
	 */   
	public  boolean readFileAsVector( String filename, Vector v){
		File file = new File(  filename );
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			while (in.ready() == true){
				v.add(in.readLine());          
			}
			in.close(); 
		} catch (FileNotFoundException e){
			IJ.showStatus("Error - file not found");
			return false;
		} catch (IOException e){
			IJ.showStatus("Error - input output error");
			return false;
		}     
		return true;
	} //readFileAsVector

	/**
	 * Writes the contents of the vector to a text file - one element per line.
	 * Content of each element should be String castable.
	 *
	 * @param filename the name of the destination file - will be overwritten.
	 * @param v the vector with String castable elements
	 * @return a boolean indicating success
	 */

	public  boolean writeVectorToFile(String filename, Vector v){
		boolean OK = true;
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(filename));
			for (int i = 0; i< v.size(); i++){
				out.write( (String) v.get(i) + "\n");
			}
			out.close();
		} catch (IOException e) {
			IJ.log("Error writing to " + filename);
			OK = false;
		}
		return OK;
	}//writeVectorToFile

	/**
	 * Constructs the directory for the given keyword directory name.
	 * Example:  maskDir = ZPItools.constructDirName(root, "dog")
	 * will return <root>/dog/ on Mac OSX while...
	 * Example:  maskDir = ZPItools.constructDirName(root, ZPITools.maskDirName)
	 * will return <root>/_mask/ on Mac OSX
	 *
	 * @param root the root directory - if it doesn't have a trailing file separator it will be added to it.
	 * @param dirName the name of the directory to attach to the root
	 * @return the <root> with an appended dirName
	 */
	public  String constructDirName(String root, String dirName){
		String out = markDirectory(markDirectory(root) + dirName);
		return out;
	}//constructDirName

	/** 
	 * Crafts a ZPI format directory name based upon the keyword.
	 * 
	 * @param root the root directory for the sample set.
	 * @param key the keyword that indicates the directory...
	 *   "raw", "work", "mask", "outline", case insensitive.  If the key is not
	 * recognized then the result is the same as calling constructDirName().
	 */
	public  String zpiDirName(String root, String key){

		if (key.equalsIgnoreCase("raw")){ 
			return constructDirName(root, rawDirName);
		} else if (key.equalsIgnoreCase("work")){
			return constructDirName(root, workDirName);
		} else if (key.equalsIgnoreCase("mask")){
			return constructDirName(constructDirName(root, workDirName), maskDirName);
		} else if (key.equalsIgnoreCase("outline")){
			return constructDirName(constructDirName(root, workDirName), outlineDirName);
		} else if (key.equalsIgnoreCase("roi")){
			return constructDirName(constructDirName(root, workDirName), roiDirName);
		} else {return constructDirName(root, key);}
	}

	/** 
	 * Creates a specified directory unless it already exists.
	 * Adapted from Philipe Grossjean's makeDir 
	 *
	 * @param dirname the name of the directory to create
	 * @return boolean flag indicating success
	 */
	public  boolean mkDir(String dirname){
		File dir = new File(dirname);
		// Check if it exists and if it is a directory
		// Return false if it is a file otherwise true
		if (dir.exists() && dir.isDirectory()) {return(true);}
		boolean bSuccess = dir.mkdir();
		return bSuccess;
	}//mkDir

	/** 
	 * Moves files from one directory to another - *on the same volume* only.  
	 * Mix-and-match between OSs or Volumes is not allowed. 
	 *
	 * @param files the fully qualifed names of files to move
	 * @param destDir the fully qualified destination directory name
	 * @return a boolean array indicating success or failure for each input file
	 */
	public  boolean[] moveFiles(String files[], String destDir){ 
		boolean[] bOK = new boolean[files.length];
		File dir = new File(destDir);
		for (int i=0; i<files.length;i++){
			bOK[i] = moveFile(files[i], dir);
		}
		return bOK;
	}//moveFiles

	/**  
	 * Moves a single file.
	 *
	 * @param inFile the fully qualified name of file to move
	 * @param destDir the fully qualified destination directory name
	 * @return a boolean indicating success or failure 
	 */
	public  boolean moveFile(String inFile, File destDir){
		//this is compromised
		File thisFile = new File(inFile);
		return thisFile.renameTo(new File(destDir, thisFile.getName()));
	}//moveFile

	/**
	 * Copy a single file. Taken from  http://www.rgagnon.com/javadetails/java-0064.html
	 *
	 * @param srcFile the fully qualified filename for the source file
	 * @param dstFile the fully qualified filename for the destination file
	 */
	public  void copyFile(String srcFile, String dstFile){
		try {
			FileInputStream fis  = new FileInputStream(new File(srcFile));
			FileOutputStream fos = new FileOutputStream(new File(dstFile));
			byte[] buf = new byte[1024];
			int i = 0;
			while((i=fis.read(buf))!=-1) {
				fos.write(buf, 0, i);
			}
			fis.close();
			fos.close();
		} catch (Exception e) {
			IJ.showStatus("Error copying files");
			return;
		}         
	}//end of copyFile

	/**
	 * Copy a single file. Taken from  http://www.rgagnon.com/javadetails/java-0064.html
	 *
	 * @param srcFile the fully qualified filename for the source file
	 * @param dstFile the fully qualified filename for the destination file
	 */
	public boolean deleteFile(String file){
		boolean ret = false;
		File ff = new File(file);
		if ((ff.exists()==false) || (ff.canWrite()==false)){ return ret;}
		try {
			ff.delete();
			ret = true;
		} catch (Exception e) {
			IJ.showStatus("Error deleting file: " + file);
		}         
		return ret;
	}//end of deleteFile


	/** 
	 * Renames a file or directory
	 *
	 * @param srcDir the original (source) file or directory
	 * @param dstDir the destination file or directory
	 * @return a boolean indicating success
	 */
	public  boolean renameFile(String src, String dst){
		return moveFile(src, new File(dst));
	}

	/** 
	 * Zips a list of files to a single zip file.
	 *
	 * @param files2zip the fully qualified names of files to zip
	 * @param zipFile the name of the destination zip file
	 * @param deleteOrig set to true to delete the orginal files
	 * @return a boolean flag indicating success of failure
	 */
	public  boolean zipFiles(String[] files2zip, String zipFile, boolean deleteOrig){
		boolean bOK = false;
		byte[] buffer = new byte[1024];
		try {
			FileOutputStream fos = new FileOutputStream(zipFile);
			ZipOutputStream zos = new ZipOutputStream(fos);

			for (int i = 0; i < files2zip.length; i++){
				File thisFile = new File(files2zip[i]);
				if (!thisFile.isDirectory() && !thisFile.isHidden()) {
					FileInputStream inS = new FileInputStream(files2zip[i]);
					zos.putNextEntry(new ZipEntry(thisFile.getName()));
					int len;
					while ((len = inS.read(buffer))>0){
						zos.write(buffer,0,len);
					}
					zos.closeEntry();
					inS.close();
					if (deleteOrig==true){
						thisFile.delete();
					}
				}
			}
			zos.finish();
			zos.close();
			//fos.finalize();
			fos.close();
			bOK = true;   
		} catch (IllegalArgumentException iae) {
			iae.printStackTrace();
			IJ.log("zipFiles: Illegal argument"); 
			bOK = false;
		} catch (FileNotFoundException fnfe) {
			fnfe.printStackTrace();
			IJ.log("zipFiles: File note found");
			bOK = false;
		} catch (IOException ioe){
			ioe.printStackTrace();
			IJ.log("zpiFiles: IO exception");
			bOK = false;
		}

		return bOK;
	}//zipFiles


	//-----------------------------------------------------------------------------
	//
	//  Array handling (unique values, sorting, value locating, ...)
	//
	//-----------------------------------------------------------------------------

	/** 
	 * Subsets an array given the indices of elements to return.
	 * 
	 * @param arr the array to subset.
	 * @param idx an integer array of indices to return
	 * @param a subset of the input array composed of elements at idx
	 */
	public  String[] arraySubset(String[] arr, int[] idx){
		String[] r = new String[idx.length];
		for (int i = 0; i<idx.length;i++){r[i] = arr[idx[i]];}
		return r;
	}


	/** 
	 * Returns the unique values of input array in a long array.
	 * 
	 * @param arr the input array - must be sorted
	 * @return an integer array of the unique values 
	 */
	public  long[] getUniqueValueLong(long [] arr){
		int[] index  = getUniqIndex(arr);
		long[] val = new long[index.length];
		for (int i=0;i<index.length;i++){
			val[i] = arr[index[i]];
		}
		return val;
	}// end of getUnique

	/** 
	 * Returns the unique values of input array in an integer array.
	 * 
	 * @param arr the input array - must be sorted
	 * @return an integer array of the unique values 
	 */
	public  int[] getUniqueValueInt(int [] arr){
		int[] index  = getUniqIndex(arr);
		int[] val = new int[index.length];
		for (int i=0;i<index.length;i++){
			val[i] = arr[index[i]];
		}
		return val;
	}// end of getUniqueValue

	/** 
	 * Returns the indices of the unique elements in an integer array.
	 * 
	 * @param arr the inut array - must be sorted
	 * @return the indices of the unique values
	 */
	public  int[] getUniqIndex( int[] arr){
		int len = arr.length;
		int[] vindex = new int[len];
		int n = 1;
		int[] retDefault = new int[1];
		retDefault[0] = 0;
		//no need to go further if more than one
		if (len == 1) { return retDefault;}
		//determine sort order
		boolean ascnd = (arr[n-1] >= arr[0]);
		if (isSorted(arr, ascnd) == false) {return retDefault;}
		//if the element is not equal to the one before 
		//then add it to the list - the first one always is uniq
		for(int i = 1; i< len; i++){
			if(arr[i] != arr[i-1]) {
				vindex[i] = i;
				n++;
			} else {
				vindex[i] = -1;
			}
		}
		//make a new index array that is just long enough
		int[] index = new int[n];
		int p = 0;
		for (int i = 0; i < vindex.length; i++){
			if (vindex[i] != -1){
				index[p] = vindex[i];
				p++;
			}
		}
		return index;
	} //getUniqIndex

	/** 
	 * Returns the indices of the unique elements in a long array.
	 * 
	 * @param arr the inut array - must be sorted
	 * @return the indices of the unique values
	 */
	public  int[] getUniqIndex( long[] arr){
		int len = arr.length;
		int[] vindex = new int[len];
		int n = 1;
		int[] retDefault = new int[1];
		retDefault[0] = 0;
		//no need to go further if more than one
		if (len == 1) { return retDefault;}
		//determine sort order
		boolean ascnd = (arr[n-1] >= arr[0]);
		if (isSorted(arr, ascnd) == false) {return retDefault;}
		//if the element is not equal to the one before 
		//then add it to the list - the first one always is uniq
		for(int i = 1; i< len; i++){
			if(arr[i] != arr[i-1]) {
				vindex[i] = i;
				n++;
			} else {
				vindex[i] = -1;
			}
		}
		//make a new index array that is just long enough
		int[] index = new int[n];
		int p = 0;
		for (int i = 0; i < n; i++){
			if (vindex[i] != -1){
				index[p] = vindex[i];
				p++;
			}
		}
		return index;
	} //getUniqIndex

	/** 
	 * Test for sorted integer array, caller must decide if it is ascending or desecending 
	 *
	 * @param arr the array to test
	 * @param ascend a boolean indicating ascending (true) or descending (false) order
	 */
	public   boolean isSorted( int arr[], boolean ascend) {

		boolean isSorted = true;

		if (ascend == true) {
			for (int i = 0; i < (arr.length-1) ; i++) {
				if (arr[i] > arr[i+1]){
					isSorted = false;
					break;
				}  //test
			} // i loop
		} else {
			for (int i = 0; i < (arr.length-1) ; i++) {
				if (arr[i] < arr[i+1]){
					isSorted = false;
					break;
				}  //test
			} // i loop
		}
		return isSorted;        
	}   // isSorted int 

	/** 
	 * Test for sorted long array, caller must decide if it is ascending or desecending 
	 *
	 * @param arr the array to test
	 * @param ascend a boolean indicating ascending (true) or descending (false) order
	 */
	public   boolean isSorted( long arr[], boolean ascend) {

		boolean isSorted = true;

		if (ascend == true) {
			for (int i = 0; i < (arr.length-1) ; i++) {
				if (arr[i] > arr[i+1]){
					isSorted = false;
					break;
				}  //test
			} // i loop
		} else {
			for (int i = 0; i < (arr.length-1) ; i++) {
				if (arr[i] < arr[i+1]){
					isSorted = false;
					break;
				}  //test
			} // i loop
		}
		return isSorted;        
	}   // isSorted int  


	/** 
	 * Searches for the val in a sorted double precision array.
	 * Returns the index of the array value less than or equal to this value
	 * (or greater than or equal is descending order) 
	 * adapted from locate() found in Numerical Recipes in C 
	 *
	 * @param arr the sorted array to search
	 * @param val the value to look for
	 *
	 * @return the index of the value's location in the array or -1 if not found
	 */ 
	public  int valueLocate( double arr[], double val) {
		int index = -1;
		int n = arr.length;
		boolean ascnd = (arr[n-1] >= arr[0]);

		int iL = 0;
		int iU = n-1;
		int iM = n-1;

		while ((iU-iL) > 1) {
			iM = (iU+iL) >> 1;
				if ( (val >= arr[iM]) == ascnd) {
					iL = iM; 
				} else {
					iU = iM;
				}	//ascending or descending order
		} //end of while loop	

		if (ascnd == true) {
			if ( val < arr[0]) {
				index = -1;
			} else if (val >= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		} else {
			if ( val > arr[0]) {
				index = -1;
			} else if (val <= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		}
		return index;
	}

	/** 
	 * Searches for the val in a sorted integer array.
	 * Returns the index of the array value less than or equal to this value
	 * (or greater than or equal is descending order) 
	 * adapted from locate() found in Numerical Recipes in C 
	 *
	 * @param arr the sorted array to search
	 * @param val the value to look for
	 *
	 * @return the index of the value's location in the array or -1 if not found
	 */
	public  int valueLocate( long arr[], long val) {
		int index = -1;
		int n = arr.length;
		boolean ascnd = (arr[n-1] >= arr[0]);

		int iL = 0;
		int iU = n-1;
		int iM = n-1;

		while ((iU-iL) > 1) {
			iM = (iU+iL) >> 1;
		if ( (val >= arr[iM]) == ascnd) {
			iL = iM; 
		} else {
			iU = iM;
		}	//ascending or descending order
		} //end of while loop	

		if (ascnd == true) {
			if ( val < arr[0]) {
				index = -1;
			} else if (val >= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		} else {
			if ( val > arr[0]) {
				index = -1;
			} else if (val <= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		}
		return index;
	}//

	/** 
	 * Searches for the val in a sorted integer array.
	 * Returns the index of the array value less than or equal to this value
	 * (or greater than or equal is descending order) 
	 * adapted from locate() found in Numerical Recipes in C 
	 *
	 * @param arr the sorted array to search
	 * @param val the value to look for
	 *
	 * @return the index of the value's location in the array or -1 if not found
	 */
	public  int valueLocate( int arr[], int val) {
		int index = -1;
		int n = arr.length;
		boolean ascnd = (arr[n-1] >= arr[0]);

		int iL = 0;
		int iU = n-1;
		int iM = n-1;

		while ((iU-iL) > 1) {
			iM = (iU+iL) >> 1;
		if ( (val >= arr[iM]) == ascnd) {
			iL = iM; 
		} else {
			iU = iM;
		}	//ascending or descending order
		} //end of while loop	

		if (ascnd == true) {
			if ( val < arr[0]) {
				index = -1;
			} else if (val >= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		} else {
			if ( val > arr[0]) {
				index = -1;
			} else if (val <= arr[n-1]) {
				index = n-1;
			} else {
				index = iL ;
			}
		}
		return index;
	}//vo

	//-----------------------------------------------------------------------------
	//
	//  Vector Handling - note the "getData" series of methods assume that the input
	//    vector is comprised of String Arrays of uniform length.  No test is made
	//    to insure compliance.
	//
	//-----------------------------------------------------------------------------

	/**
	 * Prints the contents of the vector to the ImageJ log window.
	 * 
	 * @param v The vector to print.
	 */
	public  void showVector(Vector v){
		for (int i = 0; i < v.size();  i++){ IJ.log(i+" = "+(String) v.get(i));}
	}


	/**
	 * Sets an element in the Vector of String Arrays
	 * 
	 * @param colIndex the index of the column to set.
	 * @param rowIndex the row index (essentially the vector index)
	 * @param v the Vector of String Arrays
	 * @param value the value to assign the element
	 * @return a boolean indicating success
	 */
	public  boolean setDataElement(int colIndex, int rowIndex, Vector v, String value){
		String[] d = (String[]) v.get(rowIndex);
		d[colIndex] = value;
		v.set(rowIndex, d);
		return true;
	}//setDataElement

	/**
	 * Sets the values in a column in the Vector of String Arrays.  Be advised
	 * that no chack is made to ensure that the input vector and values have the
	 * same number of elements.
	 * 
	 * @param colIndex the index of the column to set.
	 * @param v the Vector of String Arrays
	 * @param value the values to assign to the elements in the column
	 * @return a boolean indicating success
	 */
	public  boolean setDataColumn(int colIndex, Vector v, String[] value){
		for (int i = 0; i<value.length;i++){setDataElement(colIndex, i, v, value[i]);}
		return true;
	}// setDataColumn

	/**
	 * Returns a column of data from the Vector of String arrays.
	 *
	 * @param colIndex the index of the column to retrieve.
	 * @param v the vector of String arrays.
	 * @return a String array of values 
	 */
	public  String[] getDataColumn(int colIndex, Vector v){
		String[] r = new String[v.size()];
		for (int i = 0; i<v.size();i++){r[i] = getDataElement(colIndex, i, v);}
		return r;
	}//getDataColumn

	/**
	 * Returns a column of double data from the Vector of String arrays.
	 *
	 * @param colIndex the index of the column to retrieve.
	 * @param v the vector of String arrays.
	 * @return a double array of values 
	 */
	public  double[] getDataColumnDouble(int colIndex, Vector v){
		double[] r = new double[v.size()];
		for (int i = 0; i<v.size();i++){
			r[i] = getDataElementDouble(colIndex, i,v);
		}
		return r;
	}//getDataColumnDouble

	/**
	 * Returns element specified in the Vector of arrays.  The elements of the
	 * Vector should be String-castable arrays.
	 * 
	 * @param colIndex the column index of the element (essentially the index into 
	 *   the array located in the vector at row
	 * @param rowIndex the row index (essentially the vector index)
	 * @return the element at [colIndex, rowIndex] as a String
	 */
	public  String getDataElement(int colIndex, int rowIndex, Vector v){
		String r = null;
		r = ((String[])v.get(rowIndex))[colIndex];
		return r;
	}//getDataElement

	/**
	 * Returns element specified in the Vector of arrays as a double.  
	 * The elements of the Vector should be double-castable arrays.
	 * 
	 * @param colIndex the column index of the element (essentially the index into 
	 *   the array located in the vector at row
	 * @param rowIndex the row index (essentially the vector index)
	 */
	public  double getDataElementDouble(int colIndex, int rowIndex, Vector v){
		String r = null;
		r = ((String[])v.get(rowIndex))[colIndex];
		return Tools.parseDouble(r);
	}//getDataElement      

	/**
	 * Joins the individual elements of a vector of String arrays such
	 * the it becomes a Vector of Strings (each element is concatenated)
	 * Use this with care as no test is made of the contents of the input v
	 *
	 * @param delim The delimiter used to join the string arrays together
	 * @param v the vector of array strings
	 * @return a boolean indicating success
	 */
	public  boolean joinVector(String delim, Vector v){
		String[] a;  //temporary string array
		String s;   //temporary string
		for (int i = 0; i<v.size();i++){
			a = (String[])v.get(i);
			s = a[0];
			if (a.length >= 1){
				for (int j=1; j<a.length;j++){
					s = s +delim+ a[j];
				}
			}            
			v.set(i, s);
		}
		return true;
	}

	/** 
	 * Splits a vector of strings to a vector of string arrays 
	 * using the specified delimiter 
	 *
	 * @param delim The delimiter used to split the string elements of v
	 * @param v the vector of strings to split
	 * @return a boolean indicating success
	 */
	public  boolean splitVector(String delim, Vector v){
		for (int i = 0; i<v.size();i++){
			v.set(i, ((String)v.get(i)).split(delim));
		}
		return true;
	}

	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as double
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @param v the vector of Strings that contains the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, double value, Vector v){
		return setKeyValue(key, toString(value), v);
	}//setKeyValue

	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as String
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @param v the vector of Strings that contains the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, String value, Vector v){
		if (value == null) {return false;}
		int idx = getKeyValueIndex(key, v);
		if (idx == -1) {return false;}
		v.set(idx, key.concat("=").concat(value));
		return true;
	}//setKeyValue

	/**
	 * Tests a key value pair for the existsance of a value.
	 * 
	 * @param key the keyword to test - case insensitive
	 * @param v the vector of strings that contains the keyword
	 * @return true if the specified keyword has a non-zero length value 
	 * and false if the value is missing  
	 */
	public boolean keyHasValue(String key, Vector v){
		int idx = getKeyValueIndex(key, v);
		if (idx == -1) {return false;}
		String[] a = (((String)v.get(idx)).trim()).split("=");
		if (a.length == 1) {
			return false;
		} else {
			return true;     
		}
	} //keyHasvalue

	/** 
	 * Gets the value given by a keyword=value pairing - returns a String which
	 * might be empty.
	 * 
	 * @param key the keyword to get  - a case insensitive String
	 * @param v the vector of Strings that contains the keyword
	 * @return the String value of key
	 */    
	public String getKeyValue(String key, Vector v){
		int idx = getKeyValueIndex(key, v);
		if (idx == -1) {return "";}
		String[] a = ((String)v.get(idx)).split("=");
		if (a.length == 1) {
			return "";
		} else {
			return (String) a[a.length-1];     
		}
	}//getKeyValue

	/** 
	 * Gets the value given by a keyword=value pairing - returns a double
	 * 
	 * @param key the keyword to get - a case insensitive String
	 * @param v the vector of Strings that contains the keyword
	 * @return the double value of key
	 */
	public  double getKeyValueDouble(String key, Vector v){
		String value = getKeyValue(key, v);
		return Tools.parseDouble(value);
	}//getKeyValueDouble

	/** 
	 * Returns the index at which the case insesntive keyword is found in 
	 * the vector. Returns -1 if not found in the key=value pairings.  
	 *
	 * @param key the keyword to get - a case insensitive String
	 * @param v the vector of Strings that contains the keyword
	 * @return the integer index location of the key=value paring in the Vector
	 */
	public  int getKeyValueIndex(String flag, Vector v){
		String s;
		int index = -1;
		for (int i = 0; i<v.size();i++){
			s= ((String) v.get(i)).toUpperCase();
			if ( s.startsWith(flag.toUpperCase())) {
				index = i;
				break;
			}
		}
		return index;
	}//getKeyValueIndex    

	/** 
	 * Gets the index of flag ala [MyFlag].
	 * Flags mark the beginning of key=value blocks.
	 * 
	 * @param flag the keyword to get with or without brackets.  This is case sensitive.
	 * @param v the vector of Strings that contains the Flag
	 * @return index of the flag
	 */
	public  int getFlagIndex(String flag, Vector v){
		for (int i = 0; i<v.size();i++){
			if ( ((String)v.get(i)).indexOf(flag) != -1) {
				return i;
			}
		}
		return -1; 
	} //getFlagIndex

	/**
	 * Gets the contents of a vector (elements of which must be castable to String)
	 * as a String array.
	 *
	 * @param vector the vector to mine for strings
	 * @param startIndex the inclusive index of the element to start from
	 * @param endIndex the inclusive index of the element to end at
	 */
	public  String[] getVectorAsStrings(Vector v, int startIndex, int endIndex){
		String[] s = new String[endIndex-startIndex+1];
		for (int i = startIndex; i == endIndex; i++){
			s[i - startIndex] = (String) v.get(i);
		}
		return s;
	} //getVectorAsStrings

	//-----------------------------------------------------------------------------
	//
	//  Window Handling
	//
	//-----------------------------------------------------------------------------
	/** 
	 * Close all windows except the log window (in theory)
	 */
	public  boolean closeAllImages(){           
		while (WindowManager.getImageCount() > 0) {
			ImagePlus imp = IJ.getImage();
			if (imp.getTitle().equalsIgnoreCase("log") == false){IJ.run("Close");}
		}
		return true;
	}// closeAllWindows


	//--------
	//  String Handling
	//--------


	/**
	 * Returns a index array in the input array elements that contain the substring
	 * <p>
	 * This method is handy for picking out a subset of filenames from a list of filenames,
	 * for example ...
	 * <p>
	 *  theseFiles = ZPItools.matchSubstringIndex(listOfFiles, ".tif");
	 * 
	 * @param list the array of strings in which the substring is scanned
	 * @param substring the substring to scan each element of the input list - note that the 
	 * location within the string is not important and wildcards are not supported
	 * @return a integer array of the elements that contain substring or null in none
	 * @see matchSubstring()
	 */
	public  int[] matchSubstringIndex(String[] list, String substring){

		// if ((list == null) || (list.length==0)){return null;}
		int counter = 0;
		boolean[] flag = new boolean[list.length]; 

		for (int i = 0; i<list.length; i++){
			if (list[i].indexOf(substring) != -1){
				flag[i] = true;
				counter++;
			}
		}
		if (counter==0)  { return null;}
		int[] index = new int[counter];
		int jcount = 0;
		for (int j = 0; j<list.length; j++){
			if (flag[j] == true){
				index[jcount] = j;
				jcount++;
			}//if the flag is true, copy the filename
		}//j loop
		return index;
	} // end of matchStringsIndex

	/**
	 * Returns a String array of Strings in the input that contain the substring
	 * <p>
	 * This method is handy for picking out a subset of filenames from a list of filenames,
	 * for example ...
	 * <p>
	 *  myTiffFiles = ZPItools.matchSubstring(listOfFiles, ".tif");
	 * 
	 * @param list the array of strings in which the substring is scanned
	 * @param substring the substring to scan each element of the input list - note that the 
	 * location within the string is not important and wildcards are not supported
	 * @return a String array of the elements that contain substring or null in none
	 * @see matchSubstringIndex()
	 */
	public  String[] matchSubstring(String[] list, String substring){
		//IJ.log("searching strings for " + substring);
		int[] index = matchSubstringIndex(list, substring);
		if (index == null){return null;}
		String[] s = new String[index.length];    
		//IJ.log("index.length="+index.length);
		for (int i = 0; i< index.length; i++){ s[i] = list[index[i]];}
		return s;
	} // end of matchStrings


	/**
	 * Strips the specified String sequence from the input String.
	 *
	 * @param input the input string to be modified
	 * @param removeSequence the string sequence to remove
	 * @return a copy of the input with the sequence removed
	 */
	public  String stripFromString(String input, String removeSequence){
		return input.replaceAll(removeSequence, "");
	}

	/**
	 * Strips the specified String sequence from the input String array.
	 *
	 * @param input the input string array to be modified
	 * @param removeSequence the string sequence to remove
	 * @return a copy of the input array with the sequence removed from each
	 *   element
	 */
	public  String[] stripFromString(String input[], String removeSequence){
		String[] out = new String[input.length];
		for (int i = 0; i < input.length ; i++){
			out[i] =  input[i].replaceAll(removeSequence, "");
		}
		return out;
	}

	/**  
	 * Concatenates array of strings and returns a single specified delimiter string 
	 *
	 * @param arg the string array to concatenate
	 * @param delim the delimiter to use such as ",", "\t", "\n"
	 * @return a single string
	 */
	public  String concatStrings(String[] arg, String delim){
		String s = new String(arg[0]);
		if ( arg.length > 1 ){
			for (int i=1;i<arg.length;i++){
				s = s.concat(delim).concat(arg[i]);
			}
		}
		return s;
	}//concatStrings


	/** 
	 * Converts an integer to String 
	 *
	 * @param v the integer value to cast as a String
	 * @return a String representation of the integer
	 */
	public  String toString(int v){
		Integer V = new Integer(v);
		return V.toString();
	}
	/** 
	 * Converts an long to String 
	 *
	 * @param v the long value to cast as a String
	 * @return a String representation of the integer
	 */
	public  String toString(long v){
		Long V = new Long(v);
		return V.toString();
	}

	/**     
	 * Converts a float to String 
	 *
	 * @param v the float value to cast as a String
	 * @return a String representation of the float value
	 */
	public  String toString(float v){
		Float V = new Float(v);
		return V.toString();
	}

	/**     
	 * Converts a double to String - use IJ.d2s instead
	 *
	 * @param v the double value to cast as a String
	 * @return a String representation of the double value
	 */
	public  String toString(double v){
		Double V = new Double(v);
		return V.toString();
	}   

	//---------
	//  Number Parsing
	//---------
	/** 
	 * Converts a string to integer (ala Tools.parseDouble(arg)) 
	 *
	 * @param s the String to convert to integer
	 * @return an long integer
	 */
	public  long parseLong(String s){
		long defaultValue = 0;
		try {
			Long d = new Long(s);
			defaultValue = d.longValue();
		} catch (NumberFormatException e) {}
		return defaultValue;
	} //end parseLong
	/** 
	 * Converts a string to integer (ala Tools.parseDouble(arg)) 
	 *
	 * @param s the Striing to convert to integer
	 * @return an long integer
	 */     public  int parseInt(String s){
		 int defaultValue = 0;
		 try {
			 Integer d = new Integer(s);
			 defaultValue = (int) d.intValue();
		 } catch (NumberFormatException e) {}
		 return defaultValue;
	 } //end parseInt    

	 /** 
	  * Reformats a sequence of numbers represented in a delimited string to a fixed number of decimals.
	  * <p>
	  * The float values returned from an IJ macro don't have the decimal precision 
	  * control seen in the "Measurements" window.  To fix that, this method will 
	  * take one string line at time - split around delimiter,scan for floats and 
	  * replace with the specified decimal place precision if appropriate 
	  *
	  * @param input A String of concentanted numbers (joined by delimiter)
	  * @param decimalPlaces The number of decimal places to retain (see IJ.d2s())
	  * @param delimiter the delimiter separating the elements oof input string (",", "\t", etc.)
	  * @return A String with transformed numbers speparated by delimiter 
	  */
	 public  String fixDecimals(String input, int decimalPlaces, String delimiter){
		 String[] s = input.split(delimiter);
		 s = fixDecimals(s, decimalPlaces);
		 return concatStrings(s,delimiter);
	 } //fixDecimals as concatenated String

	 /** 
	  * Reformats a sequence of numbers represented in a string array to a fixed number of decimals.
	  * <p>
	  * The float values returned from an IJ macro don't have the decimal precision 
	  * control seen in the "Measurements" window.  To fix that, this method will 
	  * take one string line at time - split around delimiter,scan for floats and 
	  * replace with the specified decimal place precision if appropriate 
	  *
	  * @param input A String array of numbers to fix
	  * @param decimalPlaces The number of decimal places to retain (see IJ.d2s())
	  * @return A String array with transformed numbers speparated by delimiter 
	  */
	 public  String[] fixDecimals(String[] input, int decimalPlaces){
		 for (int i = 0; i<input.length ;i++){input[i] = fixDecimals(input[i], decimalPlaces);}
		 return input;
	 }//fixDecimals as String array    
	 /** 
	  * Reformats a number represented as a string to a fixed number of decimals.
	  * <p>
	  * The float values returned from an IJ macro don't have the decimal precision 
	  * control seen in the "Measurements" window.  To fix that, this method will 
	  * take one string line at time - split around delimiter,scan for floats and 
	  * replace with the specified decimal place precision if appropriate 
	  *
	  * @param input A String of the input number
	  * @param decimalPlaces The number of decimal places to retain (see IJ.d2s())
	  * @return A String with the transformed number
	  */
	 public  String fixDecimals(String input, int decimalPlaces){
		 if (input.indexOf(".") != -1){input =  IJ.d2s(Tools.parseDouble(input), decimalPlaces);}
		 return input;
	 } 


}//end of ZPI_Tools class




