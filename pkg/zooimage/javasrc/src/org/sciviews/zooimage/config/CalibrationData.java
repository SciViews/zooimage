package org.sciviews.zooimage.config;

import ij.ImagePlus;

import java.util.HashMap;
import java.util.Vector;

import org.sciviews.zooimage.files.ZimFile;
import org.sciviews.zooimage.log.Log;
import org.sciviews.zooimage.tools.PropertiesUtilities;
import org.sciviews.zooimage.tools.ZIJ;
import org.sciviews.zooimage.tools.ZimUtilities;

/**
 * Configuration of the calibration data
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class CalibrationData {

	
	/**
	 * An identifier for this calibration set
	 */
	private String calib ; 
	
	/**
	 * The name of this parameters set
	 */
	private String method ;
	
	/**
	 * Calibrations
	 */
	private String[] calibs ;
	
	/**
	 * Default pixel size in mm (for 600dpi)
	 */
	private double pixsize ;				
	
	/**
	 * Unit of the pixel
	 */
	private String pixelunit ;
	
	/**
	 * Location of the white point (see calibration procedure in the ZooImage flatbed manual)
	 */
	private int whitepoint ; 
	
	/**
	 * Location of the black point (see calibration procedure in the ZooImage flatbed manual)
	 */
	private int blackpoint ; 
	
	
	/**
	 * Minimum ECD in mm
	 */
	private double minsize ;
	
	/** 
	 * Maximum ECD in mm
	 */
	private double maxsize ;
	
	/**
	 * Number of decimals
	 */
	private int decimals = 4 ;
	
	/** 
	 * Features
	 */
	private String pmes = 
		"area mean standard modal min centroid center perimeter " +
		"bounding fit circularity feret's integrated median " +
		"skewness kurtosis limit display";
	
	// constants
	public static final int whitepointref = 2000 ;
	public static final int blackpointref = 53000 ;
	public static final double Xmax = 65535.0 ;
	
	/**
	 * Size of the rolling ball for background elimination
	 */
	public static final int rollballsize = 250;	
	
	/**
	 * Number of bins for the histogram
	 */
	public static final int histbins = 20 ;
	
	/**
	 * Constructor
	 * @param calibs Calibration operations
	 * @param pixsize pixel size (in pixel unit)
	 * @param pixelunit pixel unit
	 * @param whitepoint size of a black point
	 * @param blackpoint size of a white point
	 */
	public CalibrationData( String[] calibs, double pixsize, 
			String pixelunit, int whitepoint, int blackpoint){
		
		setCalibs(calibs) ;
		setPixsize(pixsize) ;
		setPixelunit(pixelunit) ;
		setWhitepoint(whitepoint) ;
		setBlackpoint(blackpoint) ;
	}

	/**
	 * Constructor using default values
	 */
	public CalibrationData(){
		this( null, 0.0, "mm", whitepointref, blackpointref) ;
	}
	
	/**
	 * Read properties from the zim file
	 * @param file zim file to extract properties from
	 */
	public void read( ZimFile file ){
		try{
			processProperties( file.extractProperties() ) ;
			Log.log("Pix=" + pixsize + ", WP=" + whitepoint + ", BP=" + blackpoint );
		} catch( Exception e){
			Log.error( "cannot read zim file: " + e.getMessage() ) ;
		}
	}
	
	/**
	 *  Here, we must add a process to calibrate grays a little bit
	 *  Otherwise, they are too different from image to image and from the scanner
	 *   We use WhitePoint and BlackPoint information to make Black value of gray at 6000 and White value at 63535
	 *	We calculate: Xw = 65535 - WP; Yw = 63535; Xb = exp(BP * ln(65535) / 65535) - WP; Yb = 6000
	 *	Conversion is done with a straight line transformation
	 *	We first multiply picture by slope = (Yw - Yb) / (Xw - Xb)
	 *	Then, we add the intercept = (Xw*Yb - Xb*Yw) / (Xw - Xb)
	 * @param raw 
	 */
	public void calibrateGrays(ImagePlus raw ){
	
		Log.debug( "<" +  getClass().getName() + ">" ) ;
		
		ZIJ.run(raw, "Calibrate...", "function=None unit=OD text1= text2=[0 1] global");
		
		double Xw = Xmax - (double)whitepoint;
		double Yw = Xmax - (double)whitepointref;
		double Xb = Math.exp(blackpoint * Math.log(Xmax) / Xmax) - whitepoint;
		double Yb = Math.exp(blackpointref * Math.log(Xmax) / Xmax) - whitepointref;
		double slope = (Yw - Yb) / (Xw - Xb);
		double intercept = (Xw*Yb - Xb*Yw) / (Xw - Xb);
		
		ZIJ.run(raw, "Add...", "value=" + (intercept/slope));
		ZIJ.run(raw, "Multiply...", "value=" + slope);
		ZIJ.run(raw, "Subtract Background...", "rolling=" + rollballsize + " white");
		
		Log.debug( "</" +  getClass().getName() + ">" ) ;
	}
	
	/**
	 * Fill the vector with parameters from this calibration data
	 * @param parameters a vector to fill
	 */
	public void fill( Vector<String> parameters ){
		parameters.add( "ProcessPixSize=" + pixsize )  ;
		parameters.add( "ProcessPixUnit=" + pixelunit) ;
		parameters.add( "ProcessWhitePoint=" + whitepoint ) ;
		parameters.add( "ProcessBlackPoint=" + blackpoint ) ;
		parameters.add( "ProcessMinSize=" + minsize ) ;
		parameters.add( "ProcessMaxSize=" + maxsize ) ;
		parameters.add( "ProcessDecimals=" + decimals ) ;
		parameters.add( "Method=" + method ) ;
		parameters.add( "Calibration=" + calib ) ;
	}
	
	/**
	 * Returns the number of bins for the histogram
	 * @return
	 */
	public int getHistbins(){
		return histbins ;
	}
	
	/**
	 * Returns the minimum size, expressed in pixel
	 * @return number of pixels
	 */
	public int getMinsizeAsPixel( ){
		return convertToPixel(minsize);
	}
	
	/**
	 * Returns the maximum size, expressed in pixels
	 * @return
	 */
	public int getMaxsizeAsPixel( ){
		return convertToPixel(maxsize);
	}
	
	/**
	 * Convert a number of mm to a number of pixels
	 * @param x length in mm
	 * @return number of pixels to represent that length
	 */
	public int convertToPixel( double x){
		return (int)( (x / 2.0) * (x / 2.0) * 3.1416 / (pixsize * pixsize));
	}
	
	/**
	 * Converts a value
	 * This was with the scanner's definition of blackpoint (65535 - x)
	 * @param value
	 * @return
	 */
	public int convertBlackPoint( int value){
		return (value - (65535 - blackpoint ) ) ;
	}
	
	/**
	 * Process properties read from the zim file. This process default properties 
	 * and other plugins might want to process others, see the Scanner_Gray16 
	 * plugin for example. 
	 * 
	 * @param map property map read from the zim file
	 * @see ZimUtilities#readProperties(String) used to read the properties from the file
	 */
	public void processProperties( HashMap<String, String> map) {
		
		if( map.containsKey( "PixelSize" ) ){
			pixsize = PropertiesUtilities.getDoubleProperty( map.get("PixelSize" )) ;
		}
		if( map.containsKey( "WhitePoint" )){
			whitepoint = PropertiesUtilities.getIntProperty(map.get("WhitePoint")) ;
		}
		if( map.containsKey( "BlackPoint" )){
			blackpoint = PropertiesUtilities.getIntProperty(map.get("BlackPoint")) ;
		}
		
		if( map.containsKey( "PixelUnit" ) ){
			pixelunit = map.get("PixelUnit" ) ;
		}
		
		if( map.containsKey( "MinSize" ) ){
			setMinsize(PropertiesUtilities.getDoubleProperty( map.get("MinSize" )));
		}
		
		if( map.containsKey( "MaxSize" )){
			setMaxsize(PropertiesUtilities.getDoubleProperty(map.get("MaxSize")));
		}
		
		if( map.containsKey( "Decimals" )){
			setDecimals(PropertiesUtilities.getIntProperty(map.get("Decimals")));
		}
		
		if( map.containsKey( "Features" )){
			setPmes(map.get( "Features" ));
		}
		
	}
	
	
	
	
	
	
	
	/**
	 * @param calibs the calibs to set
	 */
	public void setCalibs(String[] calibs) {
		this.calibs = calibs;
	}


	/**
	 * @return the calibs
	 */
	public String[] getCalibs() {
		return calibs;
	}
	
	public String getCalib(int i){
		return calibs[i] ;
	}
	
	public int getCalibsLength(){
		return calibs.length ;
	}

	public String getCalib(){
		return calib ;
	}
	
	public void setCalib( String calib){
		this.calib = calib ; 
	}

	/**
	 * @param pixsize the pixsize to set
	 */
	public void setPixsize(double pixsize) {
		this.pixsize = pixsize;
	}


	/**
	 * @return the pixsize
	 */
	public double getPixsize() {
		return pixsize;
	}


	/**
	 * @param whitepoint the whitepoint to set
	 */
	public void setWhitepoint(int whitepoint) {
		this.whitepoint = whitepoint;
	}


	/**
	 * @return the whitepoint
	 */
	public int getWhitepoint() {
		return whitepoint;
	}


	/**
	 * @param pixelunit the pixelunit to set
	 */
	public void setPixelunit(String pixelunit) {
		this.pixelunit = pixelunit;
	}


	/**
	 * @return the pixelunit
	 */
	public String getPixelunit() {
		return pixelunit;
	}


	/**
	 * @param blackpoint the blackpoint to set
	 */
	public void setBlackpoint(int blackpoint) {
		this.blackpoint = blackpoint;
	}


	/**
	 * @return the blackpoint
	 */
	public int getBlackpoint() {
		return blackpoint;
	}

	/**
	 * @param minsize the minsize to set
	 */
	public void setMinsize(double minsize) {
		this.minsize = minsize;
	}

	/**
	 * @return the minsize
	 */
	public double getMinsize() {
		return minsize;
	}

	/**
	 * @param maxsize the maxsize to set
	 */
	public void setMaxsize(double maxsize) {
		this.maxsize = maxsize;
	}

	/**
	 * @return the maxsize
	 */
	public double getMaxsize() {
		return maxsize;
	}

	/**
	 * @param decimals the decimals to set
	 */
	public void setDecimals(int decimals) {
		this.decimals = decimals;
	}

	/**
	 * @return the decimals
	 */
	public int getDecimals() {
		return decimals;
	}

	/**
	 * @param pmes the pmes to set
	 */
	public void setPmes(String pmes) {
		this.pmes = pmes;
	}

	/**
	 * @return the pmes
	 */
	public String getPmes() {
		return pmes;
	}

	/**
	 * @param method the method to set
	 */
	public void setMethod(String method) {
		this.method = method;
	}

	/**
	 * @return the method
	 */
	public String getMethod() {
		return method;
	}

	
}
