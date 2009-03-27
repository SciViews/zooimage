package org.sciviews.zooimage.config;

/**
 * Configuration of the scale
 * @author Romain Francois <francoisromain@free.fr>
 *
 */
public class ScaleConfig {
	private double barwidth ;
	private int barheight ;
	private int fontsize ;
	
	public static final int SMALL  = 0; 
	public static final int MEDIUM = 1; 
	public static final int LARGE  = 2; 
	
	public ScaleConfig( double barwidth, int barheight, int fontsize){
		this.setBarwidth(barwidth);
		this.setBarheight(barheight);
		this.setFontsize(fontsize);
	}

	/**
	 * @param barheight the barheight to set
	 */
	public void setBarheight(int barheight) {
		this.barheight = barheight;
	}

	/**
	 * @return the barheight
	 */
	public int getBarheight() {
		return barheight;
	}

	/**
	 * @param barwidth the barwidth to set
	 */
	public void setBarwidth(double barwidth) {
		this.barwidth = barwidth;
	}

	/**
	 * @return the barwidth
	 */
	public double getBarwidth() {
		return barwidth;
	}

	/**
	 * @param fontsize the fontsize to set
	 */
	public void setFontsize(int fontsize) {
		this.fontsize = fontsize;
	}

	/**
	 * @return the fontsize
	 */
	public int getFontsize() {
		return fontsize;
	}
	
	
	
}
