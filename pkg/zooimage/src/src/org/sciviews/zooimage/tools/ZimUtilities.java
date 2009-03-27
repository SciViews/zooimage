package org.sciviews.zooimage.tools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;

/**
 * Utilities to manipulate Zim files
 */
public class ZimUtilities {

	/**
	 * Import properties from a zim file (key=value)
	 * @param file zim file
	 * @return property map from the file
	 * @throws IOException when cannot read from the file
	 */
	@Deprecated
	public static HashMap<String,String> readProperties(String file) throws IOException {
		HashMap<String,String> map = new HashMap<String,String>() ;
		BufferedReader buf = new BufferedReader(new FileReader( file ));
		
		String txtline = "";
		String key = "";
		String value = "";
		int sepIndex = 0;
		while (true) {
			txtline = buf.readLine();
			if (txtline == null) break;
			// Look if it contains something like 'key = value' and extract 'key' and 'value'
			sepIndex = txtline.indexOf("=");
			if (sepIndex >= 0) {
				key = txtline.substring(0, sepIndex).trim();
				value = txtline.substring(sepIndex + 1, txtline.length()).trim();
				map.put( key, value) ;
			}
		}
		return map ;
	}
	
	/**
	 * Copy zimfile, append process parameters, then measurements to cazfile
	 * @param zimfile Zim file to read and write back to cazfile
	 * @param parameters Parameters to write to the file
	 * @param txtfile Text file to append
	 * @param cazfile Output file
	 * @throws IOException
	 */
	public static void makeDATFile(String zimfile, Vector<String> parameters, String txtfile, String cazfile) throws IOException {
		
		// The output file
		BufferedWriter out = new BufferedWriter(new FileWriter(cazfile));

		// Write data from zim file
		BufferUtilities.copy(  new BufferedReader(new FileReader(zimfile))  , out );

		// Write parameters data
		out.write("\n");
		out.write("[Process]\n");
		BufferUtilities.writeLines( out, parameters) ;
		out.write("\n");

		// Write data from the txt file
		out.write("[Data]\n");
		out.write("!Item");
		BufferUtilities.copy( new BufferedReader(new FileReader(txtfile)) , out ); 

		// close the buffer
		out.close();

	}
}
