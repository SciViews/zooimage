package org.sciviews.zooimage.tools;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Writer;
import java.util.Vector;

public class BufferUtilities {
	
	/**
	 * Copy one buffer to another
	 * @param in input buffer
	 * @param out output buffer
	 */
	public static void copy( BufferedReader in, Writer out) throws IOException {
		String txtline = "";
		while (true) {
			txtline = in.readLine();
			if (txtline == null) break;
			out.write(txtline + "\n");
		}
		in.close();
	}
	
	/**
	 * Writes each element of lines as a line in the writer
	 * @param out stream to write to
	 * @param lines lines to write
	 * @throws IOException 
	 */
	public static void writeLines( Writer out, Vector<String> lines) throws IOException{
		for( String line: lines){
			out.write( line + "\n") ;
		}
	}
	
	/**
	 * Returns the first line of a file
	 * @param file a file path
	 * @return the first line of the file
	 */
	public static String getFirstLine( File file){
		String first = "" ;
		try{
			BufferedReader in = new BufferedReader( new FileReader( file ) ) ;
			first = in.readLine() ;
			in.close() ;
		} catch( Exception e ){}
		return first ;
	}
	
}
