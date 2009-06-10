package org.sciviews.zooimage.fitvis; 

import ij.*;
import ij.text.*; //TextWindow and TextPanel
import ij.util.*;  //access to Tools

import java.io.*; //for file handling
import java.util.*; //for Vector stuff


/**
 * This class collects utility methods that are used commonly 
 * in Zoo-Phyto Image configuration files.  Configuration files
 * are simple ASCII text files that arranged into blocks of
 * keyword=value pairings.  As of the first version, suitable 
 * configuration files include the FIT_VIS_PREFS.txt, FIT_IMS_PREFS.txt,
 * and FlowCAM's ctx files.
 * <p>
 * Utility is provided in the following areas ...
 *  <ul> 
 *   <li>Config file reading and writing.
 *   <li>Display in an ImageJ TextWindow
 *   <li>Data access by keyword (get/set)
 *   <li>Keyword=Value additions and deletions
 * </ul>
 * <p>
 * For more info see <a href = "http://www.sciviews.org/zooimage/">ZooPhytoImage</a>.
 * @author ZooPhytoImage Team
 * @author btupper@bigelow.org
 * @author phgrosjean@sciviews.org
 * @version 0.0
 */

public class ZPIconfig{
	public String file = "";
	public String delim = "=";
	public Vector <String> vData = new Vector<String>();

	/** 
	 * a generic constructor
	 */
	public ZPIconfig(){}

	/** Creates and instance of this class and reads in the given file.  
	 * The file should be a simple ASCII text
	 *
	 * @param file the fully qualified filename to read
	 */
	public ZPIconfig(String file){
		boolean bOK = readFile(file);
	}


	/**
	 * Returns the number of elements in the data vector
	 *
	 * @return the number of data elements
	 */
	public int size(){
		return vData.size();
	}

	/**
	 * Prints the contents to ImageJ's log
	 */
	public void print(){

		for (int i = 0; i < size(); i++){
			IJ.log("  " + get(i));
		}

	}
	/**
	 * A shortcut to the Vector get() method.  
	 *
	 * @param index the index of the item to retrieve
	 * @return a string of the requested line
	 */
	public String get(int index){
		return (String) vData.get(index);
	}
	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as double
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, double value){
		return setKeyValue(key, toString(value) );
	}//setKeyValue

	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as int
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, int value){
		return setKeyValue(key, toString(value) );
	}//setKeyValue

	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as boolean
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, boolean value){
		return setKeyValue(key, toString(value));
	}//setKeyValue

	/** 
	 * Sets the value given a keyword=value pairing  - accepts value as String
	 * 
	 * @param key the keyword to set
	 * @param value the value of the keyword
	 * @return boolean flag indicating success
	 */
	public  boolean setKeyValue(String key, String value){
		if (value == null) {return false;}
		int idx = getKeyValueIndex(key);
		if (idx == -1) {return false;}
		vData.set(idx, key.concat("=").concat(value));
		return true;
	}//setKeyValue

	/**
	 * Tests a key value pair for the existsance of a value - the key must exist.
	 * 
	 * @param key the keyword to test - case insensitive
	 * @return true if the specified keyword has a non-zero length value 
	 * and false if the value is missing  
	 */
	public boolean keyHasValue(String key){
		int idx = getKeyValueIndex(key);
		if (idx == -1) {return false;}
		String[] a = (((String)vData.get(idx)).trim()).split("=");
		if (a.length == 1) {
			return false;
		} else {
			return true;     
		}
	} //keyHasvalue


	/** 
	 * Gets the value given by a keyword=value pairing index - returns a String which
	 * might be empty.
	 * 
	 * @param idx the index to the keyword to get
	 * @return the String value of key
	 */    
	public String getKeyValue(int idx){
		if (idx == -1) {return "";}
		String[] a = ((String)vData.get(idx)).split("=");
		if (a.length == 1) {
			return "";
		} else {
			return (String) a[a.length-1];     
		}
	}//getKeyValue


	/** 
	 * Gets the value given by a keyword=value pairing - returns a String which
	 * might be empty.
	 * 
	 * @param key the keyword to get  - a case insensitive String
	 * @return the String value of key
	 */    
	public String getKeyValue(String key){
		int idx = getKeyValueIndex(key);
		if (idx == -1) {return "";}
		String[] a = ((String)vData.get(idx)).split("=");
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
	 * @return the double value of key
	 */
	public  double getKeyValueDouble(String key){
		String value = getKeyValue(key);
		return Tools.parseDouble(value);
	}//getKeyValueDouble

	/** 
	 * Gets the value given by a keyword=value pairing - returns an int
	 * 
	 * @param key the keyword to get - a case insensitive String
	 * @return the int value of key
	 */
	public int getKeyValueInt(String key){
		return parseInt(getKeyValue(key));
	}//getKeyValueInt

	/** 
	 * Gets the value given by a keyword=value pairing - returns a long
	 * 
	 * @param key the keyword to get - a case insensitive String
	 * @return the long value of key
	 */
	public long getKeyValueLong(String key){
		return parseLong(getKeyValue(key));
	}//getKeyValueLong


	/** 
	 * Gets the value given by a keyword - returns a boolean true if the 
	 * value is "true"
	 * 
	 * @param key the keyword to get - a case insensitive String
	 * @return the booleab value of key
	 */
	public  boolean getKeyValueBoolean(String key){
		String value = getKeyValue(key);
		return value.equalsIgnoreCase("true");
	}//getKeyValueBoolean

	/** 
	 * Returns the index at which the case insesntive keyword is found in 
	 * the vector. Returns -1 if not found in the key=value pairings.  The correct
	 * index is found using simple case-insesnitve string matching via <String>.indexOf() 
	 * Thus the user is advised to make sure that the input search string will be
	 * unique.  For example, suppose there are two keys named FooBarBoo and FooBarHoo.
	 * calling this with int index = <ZPIconfig>.getKeyValue("foobar") will return the
	 * first "FooBar..." encountered.
	 *
	 * @param key the keyword to get - a case insensitive String
	 * @return the integer index location of the key=value paring in the Vector
	 */
	public  int getKeyValueIndex(String flag){
		String s;
		int index = -1;
		for (int i = 0; i<vData.size();i++){
			s= ((String) vData.get(i)).toUpperCase();
			if ( (s.indexOf(flag.toUpperCase())) != -1) {
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
	 * @return index of the flag
	 */
	public  int getFlagIndex(String flag){
		for (int i = 0; i<vData.size();i++){
			if ( ((String)vData.get(i)).indexOf(flag) != -1) {
				return i;
			}
		}
		return -1; 
	} //getFlagIndex

	/**
	 * Gets athe contents of the Vextor as a String array.
	 * 
	 * @return a String array of the vector contents
	 */
	public  String[] getVectorAsStrings(){
		return getVectorAsStrings(0, vData.size()-1);
	} //getVectorAsStrings

	/**
	 * Gets the contents of a vector
	 * as a String array.
	 *
	 * @param startIndex the inclusive index of the element to start from
	 * @param endIndex the inclusive index of the element to end at
	 */
	public  String[] getVectorAsStrings(int startIndex, int endIndex){
		String[] s = new String[endIndex-startIndex+1];
		for (int i = startIndex; i == endIndex; i++){
			s[i - startIndex] = (String) vData.get(i);
		}
		return s;
	} //getVectorAsStrings

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

	/**     
	 * Converts a boolean to String
	 *
	 * @param v the boolean value to cast as a String
	 * @return a String representation of the double value
	 */
	public  String toString(boolean v){
		String S = "";
		return S.valueOf(v);
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
	 */     
	public  int parseInt(String s){
		int defaultValue = 0;
		try {
			Integer d = new Integer(s);
			defaultValue = (int) d.intValue();
		} catch (NumberFormatException e) {}
		return defaultValue;
	} //end parseInt    


	/** 
	 * Reads data from a file and loads into the fieldnames and data vectors
	 * Using this assumes the fieldnames are the first row
	 * 
	 * @param filename the fully qualified name of file to read
	 * @return a boolean indicating success
	 */ 
	public boolean readFile(String filename){ 
		return readFile(filename, 0);
	}

	/** 
	 * Reads data from a file and loads into the fieldnames and data vectors,
	 * using this skips the first "n" rows which are ready into the header vector.
	 * 
	 * @param filename the fully qualified name of file to read
	 * @param nSkip the number of lines to skip before reding in the fieldnames row
	 * @return a boolean indicating success
	 */ 
	public boolean readFile(String filename, int nSkip){
		File file = new File( filename );
		boolean out = true;
		String line; 
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			//empty out existing data
			vData.clear();
			//skip over these lines
			if (nSkip !=0 ){ 
				for (int i=0; i<nSkip; i++){
					if (in.ready() == true){
						line = in.readLine();
					}
				}
			}   
			//the remaining lines are data
			while (in.ready() == true){
				line = in.readLine() ;
				vData.add( line ) ;
			}
			in.close(); 

		} catch (FileNotFoundException e){
			IJ.showStatus("Error - file not found: " + filename);
			out = false;
		} catch (IOException e){
			IJ.showStatus("Error - input output error: " + filename);
			out = false;
		}    
		if (out == true){
			this.file = filename;
		}
		return out;
	} //readFile

	/**
	 * Writes the contents of the vector to a text file - one element per line.
	 *
	 * @return a boolean indicating success
	 */

	public  boolean writeFile(){
		boolean OK = true;
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			for (int i = 0; i< vData.size(); i++){
				out.write( (String) vData.get(i) + "\n");
			}
			out.close();
		} catch (IOException e) {
			IJ.log("Error writing to " + file);
			OK = false;
		}
		return OK;
	}//writeVectorToFile

	/**
	 * Shows the contents in a new text window.
	 *
	 * @return An ImageJ TextWindow  or null.
	 */
	public TextWindow showInTextWindow(String name){
		TextWindow tw = new TextWindow( name , null, 600, 400); 
		for (int i = 0; i<vData.size();i++){tw.append((String) vData.get(i));}
		return tw;
	}
}//ZPIclass