package org.sciviews.zooimage.tools ;

import ij.ImagePlus;
import ij.process.ColorProcessor;
import ij.process.ImageConverter;

import java.util.Vector;


public class RGB {

	/**
	 * Coefficient for red channel
	 */
	private double red ;
	
	/**
	 * Coefficient for blue channel
	 */
	private double blue ;
	
	/**
	 * Coefficient for red channel
	 */
	private double green ;
	
	/**
	 * Threshold level for colors
	 */
	private int threshold;
	
	/**
	 * Constructor
	 * @param red red channel
	 * @param green green channel
	 * @param blue blue channel
	 * @param threshold threshold
	 */
	public RGB( double red, double green, double blue, int threshold ){
		this.setRed(red);
		this.setBlue(blue);
		this.setGreen(green);
		this.setThreshold(threshold);
	}

	/**
	 * @param red the red to set
	 */
	public void setRed(double red) {
		this.red = red;
	}

	/**
	 * @return the red
	 */
	public double getRed() {
		return red;
	}

	/**
	 * @param blue the blue to set
	 */
	public void setBlue(double blue) {
		this.blue = blue;
	}

	/**
	 * @return the blue
	 */
	public double getBlue() {
		return blue;
	}

	/**
	 * @param green the green to set
	 */
	public void setGreen(double green) {
		this.green = green;
	}

	/**
	 * @return the green
	 */
	public double getGreen() {
		return green;
	}

	/**
	 * @param threshold the threshold to set
	 */
	public void setThreshold(int threshold) {
		this.threshold = threshold;
	}

	/**
	 * @return the threshold
	 */
	public int getThreshold() {
		return threshold;
	}
	
	/**
	 * Fill the vector with red, green and blue and threshold
	 * @param parameters vector to fill
	 */
	public void fill( Vector<String> parameters){
		parameters.add(	"RedCoef=" + red ) ;
		parameters.add(	"BlueCoef=" + blue ) ;
		parameters.add(	"GreenCoef=" + green ) ;
		parameters.add(	"Threshold=" + threshold ) ;
	}
	
	
	/**
	 * multiply each channel of the image by the corresponding 
	 * item in the rgb object 
	 * 
	 * @param image
	 */
	public void weight( ImagePlus image ){
		
		ImageConverter converter = new ImageConverter( image ) ;
		converter.convertToRGB() ;
		ColorProcessor processor = (ColorProcessor)image.getProcessor() ;
		
		int length = processor.getWidth() * processor.getHeight() ;  
		byte[] r = new byte[length];
		byte[] g = new byte[length];
		byte[] b = new byte[length];
		processor.getRGB(r, g, b) ;
		for( int i=0; i<length; i++){
			r[i] *= red ;
			g[i] *= green ;
			b[i] *= blue ;
		}
		processor.setRGB( r, g, b) ;
		
	}
	
	public static void keepGreenChannel( ImagePlus image){
		ImageConverter converter = new ImageConverter( image ) ;
		converter.convertToRGB() ;
		ColorProcessor processor = (ColorProcessor)image.getProcessor() ;
		int length = processor.getWidth() * processor.getHeight() ;  
		byte[] r = new byte[length];
		byte[] g = new byte[length];
		byte[] b = new byte[length];
		processor.getRGB(r, g, b) ;
		for( int i=0; i<length; i++){
			r[i] = 0;
			b[i] = 0;
		}
		processor.setRGB( r, g, b) ;
	}
	
	
}
