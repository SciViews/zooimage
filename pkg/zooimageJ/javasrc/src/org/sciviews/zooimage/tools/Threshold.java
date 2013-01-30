package org.sciviews.zooimage.tools;

import java.util.Vector;

public class Threshold {

	/**
	 * Saturation for contrast enhancement (%)
	 */
	private double contrastsat ; 
	
	/**
	 * Threshold level for edge detected mask
	 */
	private int edge ;
	
	/**
	 * Threshold level for dark area
	 */
	private int dark ; 
	
	/**
	 * Threshold level for light area after background elimination
	 */
	private int light ;
	
	/**
	 * Threshold data
	 * @param contrastsat Saturation for contrast enhancement (%)
	 * @param edge Threshold level for edge detected mask
	 * @param dark Threshold level for dark area
	 * @param light Threshold level for light area after background elimination
	 */
	public Threshold( double contrastsat, int edge, int dark, int light){
		this.setContrastsat(contrastsat);
		this.setEdge(edge);
		this.setLight(light);
		this.setDark(dark);
	}

	/**
	 * Constructor using default values
	 */
	public Threshold() {
		this( 0.1, 60, 55, 255 ) ;
	}

	/**
	 * @param contrastsat the contrastsat to set
	 */
	public void setContrastsat(double contrastsat) {
		this.contrastsat = contrastsat;
	}

	/**
	 * @return the contrastsat
	 */
	public double getContrastsat() {
		return contrastsat;
	}

	/**
	 * @param edge the edge to set
	 */
	public void setEdge(int edge) {
		this.edge = edge;
	}

	/**
	 * @return the edge
	 */
	public int getEdge() {
		return edge;
	}

	/**
	 * @param dark the dark to set
	 */
	public void setDark(int dark) {
		this.dark = dark;
	}

	/**
	 * @return the dark
	 */
	public int getDark() {
		return dark;
	}

	/**
	 * @param light the light to set
	 */
	public void setLight(int light) {
		this.light = light;
	}

	/**
	 * @return the light
	 */ 
	public int getLight() {
		return light;
	}

	/**
	 * Fill the parameters with the threshold data
	 * @param parameters vector to fill with this data
	 */
	public void fill(Vector<String> parameters) {
		parameters.add( "ContrastSat=" + contrastsat ) ;
		parameters.add( "EdgeThreshold=" + edge ) ;
		parameters.add(	"DarkThreshold=" + dark) ;
		parameters.add( "LightThreshold=" + light ) ;
	}

	
}
