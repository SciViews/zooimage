package org.sciviews.zooimage.fitvis; 

//----------
//  ZPIStackPlus
//----------

import ij.*;
import ij.io.*;
import java.io.*;
/** 
  * This convenience class will load and display a stack given a list of filenames.
  *
*/
public class ZPIStackPlus extends ImagePlus {

/** 
  * Constructs a stack from a String of absolute file paths concentanated 
  * with the "\n" newline character 
  *
  * @param concat_files A single String with fully qualified filenames 
  * delimited with the newline character. 
*/
    public ZPIStackPlus(String concat_files) {
      //String[] files = concat_files.split("\n");
      this(concat_files.split("\n"));
    }//end of constructor with single String
	
/** 
  * Constructs a stack from a diretcory plus a String array of absolute fie names. 
  *
  * @param files A String array with fully qualified filenames 
*/    
  public ZPIStackPlus(String directory, String[] names) {
      super();
      String dir = directory.substring(directory.length()-1);
      if (dir.equalsIgnoreCase(File.separator)==false){
        dir = directory + File.separator;
      } else {
        dir = directory + "";
      }
      String[] files = new String[names.length];
      for (int i=0; i<names.length; i++){ files[i] = dir + names[i];}
       // open all files and place their processors in an ImageStack
       // ...
       Opener opener = new Opener();
       File file = new File(files[0]);
       String name = file.getName();
       if  (file.exists() == false) {
           IJ.log("File not found: " + files[0]);
           return;
       }
       
       IJ.showProgress(0, files.length);
       ImagePlus imp = opener.openImage(files[0]);
       int w = imp.getWidth();
       int h = imp.getHeight();
       ImageStack stack = new ImageStack(imp.getWidth(), imp.getHeight());
       stack.addSlice(name, imp.getProcessor());
       
       if (files.length > 1) {
           for (int i = 1; i< files.length; i++){
               IJ.showProgress(i, files.length);
                   //check each file
               file = new File(files[i]);
               if  (file.exists() == false) {
                   IJ.log("File not found: " + files[i]);
                   return ;
               }
               
               imp = opener.openImage(files[i]);
               
               //check the dimensions of each image
               if ((imp.getWidth() != w) || (imp.getHeight() != h)){
                   IJ.log("Images must all the same size");
                   return ;
               }
               
               stack.addSlice(file.getName(), imp.getProcessor());
           }
       }
      setStack(name,stack);
    }//end of constructor with directory + String array
    
    
/** 
  * Constructs a stack from a String array of absolute file paths. 
  *
  * @param files A String array with fully qualified filenames 
*/    
  public ZPIStackPlus(String[] files) {
       super();
       // open all files and place their processors in an ImageStack
       // ...
       Opener opener = new Opener();
       File file = new File(files[0]);
       String name = file.getName();
       if  (file.exists() == false) {
           IJ.log("File not found: " + files[0]);
           return;
       }
       
       IJ.showProgress(0, files.length);
       ImagePlus imp = opener.openImage(files[0]);
       int w = imp.getWidth();
       int h = imp.getHeight();
       ImageStack stack = new ImageStack(imp.getWidth(), imp.getHeight());
       stack.addSlice(name, imp.getProcessor());
       
       if (files.length > 1) {
           for (int i = 1; i< files.length; i++){
               IJ.showProgress(i, files.length);
                   //check each file
               file = new File(files[i]);
               if  (file.exists() == false) {
                   IJ.log("File not found: " + files[i]);
                   return ;
               }
               
               imp = opener.openImage(files[i]);
               
               //check the dimensions of each image
               if ((imp.getWidth() != w) || (imp.getHeight() != h)){
                   IJ.log("Images must all the same size");
                   return ;
               }
               
               stack.addSlice(file.getName(), imp.getProcessor());
           }
       }
      setStack(name,stack);
    }//end of constructor with String array
    


  }//end of ZPIStackPlus definition
