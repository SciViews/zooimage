package org.sciviews.zooimage.fitvis; 

/** 
  * This class creates a ImagePlus with a special ZPIVirtualStack that 
  * will optionally transform all the images to 8-bit *before* being added 
  * to the stack.  
  *<p> 
  * (3) For more info see <a href = "http://www.sciviews.org/zooimage/">ZooPhytoImage</a>.
  * @author ZooPhytoImage Team
  * @author btupper@bigelow.org
  * @author phgrosjean@sciviews.org
  * @version 0.0
  * 2007-11-19 BT
*/

import ij.*;
import ij.io.*;

import java.io.*;
/** 
  * This convenience class will load and display a stack given a list of filenames.
  * The stack will be a VirtualStack (only the visible slice is loaded).
  *
*/
public class ZPIVirtualStackPlus extends ImagePlus {

/** 
  * Constructs a stack from a diretcory plus a String array of file names. 
  * @param directory the fully qualified path
  * @param files A String array with filenames 
  * @param toByte If true all images in stack are converted to 8 bit, otherwse they
  *   are left in native format.
*/    
  public ZPIVirtualStackPlus(String directory, String[] names, boolean toByte) {
    super();
    String dir = markDirectory(directory);
    int[] dims = getDimensions(dir + names[0]);
    //get the stack
    ZPIVirtualStack stack = new ZPIVirtualStack(dims[0], dims[1], dir, toByte);
    if (stack == null){return;}
    //add each item
    for (int i = 0; i< names.length; i++){
      IJ.showProgress(i, names.length);
      stack.addSlice(names[i]);
    }
    setStack(names[0],stack);       
  }
 
/** 
  * Constructs a stack from a diretcory plus a String array of file names. Files
  * will be converted to 8-bit if required.
  * @param directory the fully qualified path
  * @param files A String array with filenames 
  * 
*/    
  public ZPIVirtualStackPlus(String directory, String[] names) {
    super();
    String dir = markDirectory(directory);
    int[] dims = getDimensions(dir + names[0]);
    //get the stack
    ZPIVirtualStack stack = new ZPIVirtualStack(dims[0], dims[1], dir, true);
    if (stack == null){return;}
    //add each item
    for (int i = 0; i< names.length; i++){
      IJ.showProgress(i, names.length);
      stack.addSlice(names[i]);
    }
    setStack(names[0],stack);       
  }
   
/** 
  * Constructs a stack from a String of absolute file paths concentanated 
  * with the "\n" newline character 
  *
  * @param concat_files A single String with fully qualified filenames 
  *   delimited with the newline character. 
  * @param toByte If true all images in stack are converted to 8 bit, otherwse they
  *   are left in native format.
  */
  public ZPIVirtualStackPlus(String concat_files, boolean toByte) {
    super();
    String[] files = concat_files.split("\n");

    File file = new File(files[0]);
    String dir = markDirectory(file.getParent());
    int[] dims = getDimensions(files[0]);
    String name = file.getName();
    
    ZPIVirtualStack stack = new ZPIVirtualStack(dims[0], dims[1], dir, toByte);
    if (stack == null){return;}
    //add each item
    for (int i = 0; i< files.length; i++){
        IJ.showProgress(i, files.length);
            //check each file
        file = new File(files[i]);
        stack.addSlice(file.getName());
    }
    setStack(name,stack);  
  }//end of constructor with single String
	
    
/** 
  * Constructs a virtual stack from a String array of absolute file paths. 
  *
  * @param files A String array with fully qualified filenames 
  * @param toByte If true all images in stack are converted to 8 bit, otherwse they
  *   are left in native format.
  */    
  public ZPIVirtualStackPlus(String[] files, boolean toByte) {
    super();
    File file = new File(files[0]);
    String dir = markDirectory(file.getParent());
    int[] dims = getDimensions(files[0]);
    String name = file.getName();
    
    ZPIVirtualStack stack = new ZPIVirtualStack(dims[0], dims[1], dir, toByte);
    if (stack == null){return;}
    //add each item
    for (int i = 0; i< files.length; i++){
        IJ.showProgress(i, files.length);
            //check each file
        file = new File(files[i]);
        stack.addSlice(file.getName());
    }
    setStack(name,stack);    
  }//end of constructor with String array  
  
/**
  * Ensures that the provided directory path includes the system 
  * path separator at the end.
  *
  * @param path the path to test
  * @return a String path descriptor with or without separator at end
*/
  private String markDirectory(String path){
    return (new File(path)).getAbsolutePath() + File.separator;
  }  
  
/**
  * returns a two element array of [width, height]
  * @param file The fulley qualified filename to query
  * @return a two element integer array specifying width and height
  */
  private int[] getDimensions(String file){
    //this is taken from an example Wayne Rasband described on the mailing list 
    //Fri, 5 Oct 2007 
    int[] arr = new int[2];
    ImagePlus imp = IJ.openImage(file);
    if (imp == null) {
      IJ.log("error opening " + file);
      return null;
    }
    arr[0] = imp.getWidth();
    arr[1] = imp.getHeight();
    imp.close();
    return arr;
  }
} //ZPIVirtualStack