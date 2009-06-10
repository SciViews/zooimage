package org.sciviews.zooimage.fitvis; 

/** 
  * This class creates a VirtualStack that will optionally transform all the images
  * to 8-bit *before* added to the stack.  The key to this class is the 
  * getProcessor() method override.
  *<p> 
  * (3) For more info see <a href = "http://www.sciviews.org/zooimage/">ZooPhytoImage</a>.
  * @author ZooPhytoImage Team
  * @author btupper@bigelow.org
  * @author phgrosjean@sciviews.org
  * @version 0.0
  * 2007-11-19 BT
*/

import ij.*;
import ij.process.*;
import ij.io.*;

import java.io.*;
/** 
  * This convenience class will load and display a stack given a list of filenames.
  * The stack will be a VirtualStack (only the visible slice is loaded).
  *
  * Use the zpiTools.getFileImageDims() method or use the ZPIVirtualStackPlus class
  *   to get width and height first.
  *
  */
public class ZPIVirtualStack extends VirtualStack{

  public boolean bToByte = true;
  
/** 
  * Constructs a virtual stack where all images are converted to 8bit. 
  * @param width the width of the input image in pixels
  * @param height the height of the image in pixels
  * @param dir the fully qualified path
  * @param toByte set to true to force all images to 8 bit, 
  *   native depth otherwise they are left as 
  */    
  public ZPIVirtualStack(int width, int height, String dir, boolean toByte) {
    super(width, height, null, dir);  
    bToByte = toByte;
  }//constructor
  
/** Returns an ImageProcessor for the specified slice, 
  * if not 8-bit then it is converted first *IF* required to do so.
  * where 1<=n<=nslices. Returns null if the stack is empty.
  * @param n the nth slice numbered processor to return
  * @return and ImageProcessor
  */
    public ImageProcessor getProcessor(int n) {
      //IJ.log("getProcessor: "+n+"  "+getFileName(n));
      ImagePlus imp = new Opener().openImage(getDirectory(), getFileName(n));
      if (imp!=null) {
        ImageProcessor ip = imp.getProcessor();
        if (bToByte) {
          if(imp.getType() != 0){
            ip=ip.convertToByte(true);
          }
        }
        return ip;
      } else {
          return null;
      }
   }   

} //ZPIVirtualStack