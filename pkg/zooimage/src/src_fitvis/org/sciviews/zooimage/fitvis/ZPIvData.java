package org.sciviews.zooimage.fitvis; 

import ij.*;
import ij.text.*; //TextWindow and TextPanel
import ij.util.*;  //access to Tools

import java.io.*; //for file handling
import java.util.*; //for Vector stuff

/** 
  * This class collects utility methods that are used commonly 
  * in Zoo-Phyto Image processing of record based data.  Record based data is loosely defined
  * as a simple spreadsheet where each column is composed of a series of fields.  Each row represents
  * a set of features and or labels for a particle.  The records are stored as either an array of
  * Strings OR a scalar string with each field delimited by comma, tab, etc.  This class is aware 
  * of the type of delimiter used and the state of each record (split or concatenated.) 
  * <p> 
  * Internally, the lines of data are held as elements in a Java Vector which is simply a resizeable list.
  * Much of the functionality associated with Vectors are exposed since the Vectors are public variables.
  * Three Vectors are used: a data vector, a column name vector and a header vector.
  * <p>  
  * The class provides a mechanism for maintaining fieldnames.  There should be one fieldname for 
  * each field in each record.  These are essentially column names. 
  * <p>
  * If there are header lines in the file (other than fieldnames row) then these are saved in yet another
  * vector.
  * <p>
  * This class borrows much functionality found in many the ZPItools class. Using this class
  * maybe more memory efficient as the ZPItools will make copies of the vectors passed to it while
  * this does not.  However, both classes are maintained - ZPItools for its generic accessibility and 
  * ZPIvData for its limited but efficient functionality.
  * <p>
  * (1) Utility is provided in the following areas ...
  *  <ul> 
  *   <li>File reading and writing.
  *   <li>Display in an ImageJ TextWindow
  *   <li>Data access as columns, rows, or individual elements.
  * </ul>
  * <p>
  * (3) For more info see <a href = "http://www.sciviews.org/zooimage/">ZooPhytoImage</a>.
  * @author ZooPhytoImage Team
  * @author btupper@bigelow.org
  * @author phgrosjean@sciviews.org
  * @version 0.0
  */



public class ZPIvData {
  /** The delimiter that separates fields of data in each record - by default tab ("\t") */
  public String delim = "\t";
  /** A flag to indicate the state of the data (as arrays or as concatenated strings) */
  public boolean dataAsArray = false;
  /** The vector that contains arbitrary lines of header info (if any)*/
  public Vector <Object>vHeader = new Vector<Object>();
  /** The vector of column names */
  public Vector <Object>vFieldnames = new Vector<Object>();
  /** The vector of data */
  //Vector<String> tmp = new Vector<String>();
  public Vector <Object> vData = new Vector<Object>();
  
/** 
  * a generic constructor
  */
  public ZPIvData(){}
  
/** Creates and instance of this class and reads in the given file.  The files should be a simple 
  * ASCII delimited file of columns of data where the first row contains the column names (fieldnames)
  *
  * @param file the fully qualified filename to read
  */
  public ZPIvData(String file, String sep){
    this.delim = sep;
    boolean bOK = readFile(file);
  }

  public ZPIvData(String file){
    boolean bOK = readFile(file);
  }

/**
  * Sets an element in the data.
  * 
  * @param colIndex the index of the column to set.
  * @param rowIndex the row index (essentially the vector index)
  * @param value the value to assign the element
  * @return a boolean indicating success
  */
  public boolean setDataElement(int colIndex, int rowIndex, String value){
    split();
    boolean ret = true;
    //IJ.log("dataAsArray=" + dataAsArray);
    //IJ.log("line= " + ((Object) vData.get(rowIndex)).toString());
    String[] d = (String[]) vData.get(rowIndex);
    d[colIndex] = value;
    try{
      vData.set(rowIndex, d);
    } catch (Exception e){
      ret = false;;
    }
    return ret;
  }//setDataElement
  
/** 
  * Set the data element in the row, column specified by number and name
  *.
  *
  * @param fieldname the field name (will be converted to column index)
  * @param rowIndex the row index (essentially the vector index)
  * @param value the value to assign the element
  * @return a boolean indicating success
  */
  public boolean setDataElement(String fieldname, int rowIndex, String value){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return setDataElement(colIndex, rowIndex, value);
    } else {
      return false;
    }
  }
  
/**
  * Sets the values in a column of vData.  Be advised
  * that no check is made to ensure that the vData and values have the
  * same number of elements.
  * 
  * @param colIndex the index of the column to set.
  * @param value the values to assign to the elements in the column
  * @return a boolean indicating success
  */
  public boolean setDataColumn(int colIndex, String[] value){
    split();
    for (int i = 0; i<value.length;i++){setDataElement(colIndex, i,value[i]);}
    return true;
  }// setDataColumn
      
/**
  * Sets the values in a column by column name.  Be advised
  * that no check is made to ensure that the vData and values have the
  * same number of elements.
  * 
  * @param fieldname the name of the column to set.
  * @param value the values to assign to the elements in the column
  * @return a boolean indicating success
  */
  public boolean setDataColumn(String fieldname, String[] value){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return setDataColumn(colIndex, value);
    } else {
    return false;
    }
  }// setDataColumn
  
/** Returns a group of data elements specified by column index and row indices.
  * 
  * @param colIndex The index of the column that contains the requested data.
  * @param rowIndices The indicies of the data to return
  * @return a String array of elements.
  */
  public String[] getDataColumn(int colIndex, int[] rowIndices){
    String[] r = new String[rowIndices.length];
    for (int i=0; i<rowIndices.length;i++){
      r[i] = getDataElement(colIndex, rowIndices[i]);
    }
    return r;
  }
  
/** Returns a group of data elements specified by column name and row indices.
  * 
  * @param fieldname The name of the column that contains the requested data.
  * @param rowIndices The indicies of the data to return
  * @return a String array of elements.
  */
  public String[] getDataColumn(String fieldname, int[] rowIndices){
    int colIndex = getFieldIndex(fieldname);
    return getDataColumn(colIndex, rowIndices);
  }
  
/**
  * Returns a column of data.
  *
  * @param colIndex the index of the column to retrieve.
  * @return a String array of values 
  */
  public String[] getDataColumn(int colIndex){
    split();
    String[] r = new String[vData.size()];
    for (int i = 0; i<vData.size();i++){r[i] = getDataElement(colIndex, i);}
    return r;
  }//getDataColumn
  
/**
  * Returns a column of data as specified by column name
  *
  * @param fieldname the name of the column to retrieve.
  * @return a String array of values 
  */
  public String[] getDataColumn(String fieldname){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return getDataColumn(colIndex);
    } else {
      return null;
    }
  }//getDataColumn

/**
  * Returns a column of int data from the Vector of String arrays.
  *
  * @param colIndex the index of the column to retrieve.
  * @return a double array of values 
  */
  public int[] getDataColumnInt(int colIndex){
    split();
    int[] r = new int[vData.size()];
    for (int i = 0; i<vData.size();i++){
      r[i] = getDataElementInt(colIndex, i);
    }
    return r;
  }//getDataColumnint
     
/**
  * Returns a column of int data from the Vector of String arrays.
  *
  * @param fieldname the name of the column to retrieve.
  * @return a double array of values 
  */
  public int[] getDataColumnInt(String fieldname){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return getDataColumnInt(colIndex);
    } else {
      return null;
    }
  }//getDataColumnInt  
  
/**
  * Returns a column of long data from the Vector of String arrays.
  *
  * @param colIndex the index of the column to retrieve.
  * @return a double array of values 
  */
  public long[] getDataColumnLong(int colIndex){
    split();
    long[] r = new long[vData.size()];
    for (int i = 0; i<vData.size();i++){
      r[i] = getDataElementLong(colIndex, i);
    }
    return r;
  }//getDataColumnLong
     
/**
  * Returns a column of long data from the Vector of String arrays.
  *
  * @param fieldname the name of the column to retrieve.
  * @return a double array of values 
  */
  public long[] getDataColumnLong(String fieldname){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return getDataColumnLong(colIndex);
    } else {
      return null;
    }
  }//getDataColumnLong  
  
/**
  * Returns a column of double data from the Vector of String arrays.
  *
  * @param colIndex the index of the column to retrieve.
  * @return a double array of values 
  */
  public double[] getDataColumnDouble(int colIndex){
    split();
    double[] r = new double[vData.size()];
    for (int i = 0; i<vData.size();i++){
      r[i] = getDataElementDouble(colIndex, i);
    }
    return r;
  }//getDataColumnDouble
     
/**
  * Returns a column of double data from the Vector of String arrays.
  *
  * @param fieldname the name of the column to retrieve.
  * @return a double array of values 
  */
  public double[] getDataColumnDouble(String fieldname){
    int colIndex = getFieldIndex(fieldname);
    if (colIndex != -1){
      return getDataColumnDouble(colIndex);
    } else {
      return null;
    }
  }//getDataColumnDouble
     


/**
  * Returns element specified
  * 
  * @param colIndex the column index of the element (essentially the index into 
  *   the array located in the vector at rowIndex
  * @param rowIndex the row index (essentially the vector index)
  * @return the element at [colIndex, rowIndex] as a String
  */
    public String getDataElement(int colIndex, int rowIndex){
      split();
      return ((String[])vData.get(rowIndex))[colIndex];
    }//getDataElement

/**
  * Returns element specified by column name and row number
  * 
  * @param colIndex the column index of the element (essentially the index into 
  *   the array located in the vector at row
  * @param rowIndex the row index (essentially the vector index)
  * @return the element at [colIndex, rowIndex] as a String
  */
    public String getDataElement(String fieldname, int rowIndex){
      int colIndex = getFieldIndex(fieldname);
      if (colIndex != -1){
        return getDataElement(colIndex, rowIndex);
      } else {
        return null;
      }
    }//getDataElement
  
 
/**
  * Returns element specified as a int.
  * 
  * @param colIndex the column index of the element (essentially the index into 
  *   the array located in the vector at row)
  * @param rowIndex the row index (essentially the vector index)
  */
    public int getDataElementInt(int colIndex, int rowIndex){
      return parseInt(getDataElement(colIndex, rowIndex));
    }//getDataElementInt    

/**
  * Returns element specified by column and row number as a int 
  * - it is up to the user to SPLIT the data first!
  * 
  * @param fieldname the name of the field (column name)
  * @param rowIndex the row index (essentially the vector index)
  */
    public int getDataElementInt(String fieldname, int rowIndex){
      int colIndex = getFieldIndex(fieldname);
      if (colIndex != -1){
        return parseInt(getDataElement(colIndex, rowIndex));
      } else {
        return parseInt("");
      }
    }//getDataElementInt   
       
/**
  * Returns element specified as a long - 
  * it is up to the user to SPLIT the data first!
  * 
  * @param colIndex the column index of the element (essentially the index into 
  *   the array located in the vector at row)
  * @param rowIndex the row index (essentially the vector index)
  */
    public long getDataElementLong(int colIndex, int rowIndex){
      return parseLong(getDataElement(colIndex, rowIndex));
    }//getDataElementLong    

/**
  * Returns element specified by column and row number as a long 
  * - it is up to the user to SPLIT the data first!
  * 
  * @param fieldname the name of the field (column name)
  * @param rowIndex the row index (essentially the vector index)
  */
    public long getDataElementLong(String fieldname, int rowIndex){
      int colIndex = getFieldIndex(fieldname);
      if (colIndex != -1){
        return parseLong(getDataElement(colIndex, rowIndex));
      } else {
        return parseLong("");
      }
    }//getDataElementLong    

  
    
/**
  * Returns element specified as a double - it is up to the user to SPLIT the data first!
  * 
  * @param colIndex the column index of the element (essentially the index into 
  *   the array located in the vector at row)
  * @param rowIndex the row index (essentially the vector index)
  */
    public double getDataElementDouble(int colIndex, int rowIndex){
      return Tools.parseDouble(getDataElement(colIndex, rowIndex));
    }//getDataElementDouble    

/**
  * Returns element specified by column and row number as a double 
  * - it is up to the user to SPLIT the data first!
  * 
  * @param fieldname the name of the field (column name)
  * @param rowIndex the row index (essentially the vector index)
  */
    public double getDataElementDouble(String fieldname, int rowIndex){
      int colIndex = getFieldIndex(fieldname);
      if (colIndex != -1){
        return Tools.parseDouble(getDataElement(colIndex, rowIndex));
      } else {
        return Tools.parseDouble("");
      }
    }//getDataElementDouble    

/**
  * Returns the fieldnames as a string array
  *
  *@return a string array of the fieldnames
  */
  public String[] getFieldnames(){
    String[] names;
    if (dataAsArray){
      names = (String[])vFieldnames.get(0);
    } else {
      names = ((String) vFieldnames.get(0)).split(delim);
    }
    return names;
  }
  
  
/**
  * Returns the index of the field that matches the provided string (case independent)
  *
  * @param fieldname the name of the field index to fetch
  * @return returns the zero based index of the field that matches that proved string OR -1 upon failure
  */
  public int getFieldIndex(String fieldname){
    if (vFieldnames.size() == 0){ return -1;}
    split();
    String[] a = (String[]) vFieldnames.get(0);;
    int index = -1;
    
    for (int i = 0; i< a.length;i++){
      //IJ.log(fieldname + " -> " + i + "=" + a[i].toString());
      if (a[i].equalsIgnoreCase(fieldname)==true){
        index = i;
        break;
      }
    }
    return index;
  }
  
/**
  * Counts the number of columns
  * 
  * @return the number of columns as indicated by the number of fieldnames
  */
  public int countColumns(){
    if (vFieldnames.size() == 0){ return -1;}
    split();
    String[] a = (String[]) vFieldnames.get(0);
    return a.length;
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
  * If the rows of data and the fieldnames are held as String Arrays
  * then this method willjoin them with the delimiter
  *
  * @return a boolean indicating success
  * @see split()
  */
  public boolean join(){
    //IJ.log("* join *");
    if (dataAsArray == false){return !dataAsArray;}
    String[] a;  //temporary string array
    String s;   //temporary string
    
    if (vFieldnames.size() !=0){
      a = (String[]) vFieldnames.get(0);
      s = a[0];
      for (int j = 1;j<a.length;j++){ s = s + delim + a[j];}
      vFieldnames.set(0, s);
    }
      
    if (vData.size() !=0){
      for (int i = 0; i<vData.size();i++){
          a = (String[])vData.get(i);
          s = a[0];
          if (a.length >= 1){
              for (int j=1; j<a.length;j++){
                  s = s +delim+ a[j];
              }
          }            
          vData.set(i, s);
      }
    }
    dataAsArray = false;
    //IJ.log("* isJoined *");
    return !dataAsArray;
  }//join
 
 
/**
  * Joins any given string array using the current delimiter
  *
  * @param arg A String array
  * @return a String of the input concatenated using the delimiter
  */
  public String join(String[] arg){
    String s = arg[0];
    for (int j = 1;j<arg.length;j++){ s = s + delim + arg[j];}    
    return s;
  } 
  
/** 
  * Splits the strings data and fieldnames into string arrays 
  * using the specified delimiter.  If the data are already split then it returns immediately,
  * with little overhead to call this method - call it often.
  *
  * @return a boolean indicating success
  * @see join()
  */
    public boolean split(){
      //IJ.log("* split *");
      if (dataAsArray == true) {return dataAsArray;}
      if (vFieldnames.size() != 0){ vFieldnames.set(0, Tools.split((String)vFieldnames.get(0), delim));}
      if (vData.size() != 0){
        for (int i = 0; i<vData.size();i++){ 
          vData.set(i, Tools.split((String)vData.get(i), delim));
        }
      }
      dataAsArray = true;
      //IJ.log("* isSplit *");
      return dataAsArray;
    }//split


/**
  * Shows the contents in a new text window.
  *
  * @return An ImageJ TextWindow  or null.
  */
  public TextWindow showInTextWindow(String name){
    TextWindow tw = new TextWindow( name , name, 600, 400); 
    join();
    
    tw.append("DELIM=" + (delim.equalsIgnoreCase("\t") ? "tab" : delim));
    tw.append((String)vFieldnames.get(0));
    for (int i = 0; i<vData.size();i++){tw.append((String) vData.get(i));}
    
    return tw;
  }
    
/** Write the data to an ASCII text file, includes header and fieldnames.
  *
  * @param filename the fully qualified filename destination.
  * @return a boolean indicating success (true) or failure (false)
  */
  public boolean writeFile(String filename){
    return writeFile(filename, false, false);
  }
  
/** 
  * Writes the data to and ASCII text file, header and fieldnames are optional.
  * 
  * @param filename the fully qualified filename destination.
  * @param excludeHeader Set to true to exclude the header info (if any) from the output.
  * @param exlcudeFieldnames Set to true to exclude the fieldnames from the output.
  * @return a boolean indicating success (true) or failure (false)
  */
  public boolean writeFile(String filename, boolean excludeHeader, boolean excludeFieldnames){
    boolean OK = true;
    boolean isSplit = dataAsArray;
    if (dataAsArray==true){split();}
      try {
        BufferedWriter out = new BufferedWriter(new FileWriter(filename));
        if ((excludeHeader == false) && (vHeader.size() != 0)){
          for (int i = 0; i<vHeader.size();i++){out.write( (String) vHeader.get(i) + "\n");}
        }
        if ((excludeFieldnames == false) && (vFieldnames.size() != 0)){
          out.write( (String) vFieldnames.get(0) + "\n");
        }
        if (vData.size() != 0) {
          for (int i = 0; i< vData.size(); i++){
            out.write( (String) vData.get(i) + "\n");
          }
        }
        out.close();
      } catch (IOException e) {
        IJ.log("Error writing to " + filename);
        OK = false;
      }
    if (isSplit==true){join();}
    return OK;
  }//writeFile

/** 
 * Reads data from a file and loads into the fieldnames and data vectors
 * Using this assumes the fieldnames are the first row
 * 
 * @param filename the fully qualified name of file to read
 * @return a boolean indicating success
*/ 
  public boolean readFile(String filename){ return readFile(filename, 0);}
  
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
    String dummy = "";
    try {
      BufferedReader in = new BufferedReader(new FileReader(file));
      //empty out existing data
      vData.clear();
      vFieldnames.clear();
      vHeader.clear();
      
      //skip over these lines
      if (nSkip !=0 ){ 
        for (int i=0; i<nSkip; i++){
          if (in.ready() == true){vHeader.add(in.readLine());}
        }
      }
      //the next line should be the fieldnames
      if (in.ready()==true){ vFieldnames.add(in.readLine());}
        
      //the remaining lines are data
      while (in.ready() == true){vData.add(in.readLine());}
      
      in.close(); 
      
      } catch (FileNotFoundException e){
              IJ.showStatus("Error - file not found: " + filename);
              return false;
      } catch (IOException e){
              IJ.showStatus("Error - input output error: " + filename);
              return false;
      }    
      //return split();  2009-02-28 swithed to simple true
      return true;
    } //readFile
  
/**
  * Prints the contents of the vector to the ImageJ log window.
  * 
  */
  public void showInLog(){
    if (vHeader.size() != 0){
      for (int i = 0; i<vHeader.size();i++){IJ.log((String) vHeader.get(i));}
    }
    boolean isSplit = dataAsArray;
    if (dataAsArray==true){split();}
    if (vFieldnames.size() != 0){
      for (int i = 0; i < vFieldnames.size();  i++){ IJ.log((String) vFieldnames.get(i));}
    }
    if (vData.size() != 0){
      for (int i = 0; i < vData.size();  i++){ IJ.log((String) vData.get(i));}
    }
    if (isSplit ==  true) {join();} 
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
   public long parseLong(String s){
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
  public int parseInt(String s){
       int defaultValue = 0;
       try {
             Integer d = new Integer(s);
             defaultValue = (int) d.intValue();
       } catch (NumberFormatException e) {}
       return defaultValue;
  } //end parseInt   
}//end of class definition 
