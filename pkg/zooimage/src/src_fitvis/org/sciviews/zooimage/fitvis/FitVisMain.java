package org.sciviews.zooimage.fitvis;

import ij.IJ;
import ij.WindowManager;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

public class FitVisMain {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		String arg ; 
		String batch = null; 
		for( int i=0; i<args.length;i++){
			arg = args[i];
			if( arg.startsWith("--batch=")){
				batch = arg.substring( 8 ) ; 
			}
		}
		
		if( batch == null ){
			FITVIS_ plugin;
			plugin = new FITVIS_();
			plugin.run( null ); 
		} else{
			processBatch( batch );  
		}
	}


	public static void processBatch( String file ){
		try{
			BufferedReader in = new BufferedReader(new FileReader(file)) ;
			/* the column names */
			String[] names = in.readLine().split(",") ;
			for( int i=0; i<names.length; i++){
				names[i] = unquote( names[i] ); 
			}

			String dir = (new File(file)).getParent() + "/" ;

			/* process each line */
			while( true ){
				String[] line = in.readLine().split(",") ; 
				if( line == null ) break; 
				FITVIS_ fv = new FITVIS_();
				String argline = assembleParameters(dir, names, line ) ; 
				fv.run( argline ); 
				closeAllImages() ;
			}

		} catch( Exception e){
			e.printStackTrace(); 
		}
	}

	public static String assembleParameters(String dir, String[] names, String[] line){

		StringBuilder sb = new StringBuilder("") ;
		
		for (int i=0;i<names.length;i++){
			if (names[i].equals("select") ){
				if (line[i].length() != 0){
					sb.append( "select=" + dir + unquote( line[i] ) );
				}
			} else if (names[i].equals("interval") ){
				sb.append( " interval=" + line[i] );
			} else if (names[i].equals("pixelsize")){
				sb.append( " pixelsize="+line[i] );
			} else if (names[i].equals("minsize") ){
				sb.append( " minsize="+line[i] );
			} else if (names[i].equals("maxsize")){
				sb.append( " maxsize="+line[i] );
			} else if (names[i].equals("distance")){
				sb.append( " distance="+line[i] );
			} else if (names[i].equals("use")){
				sb.append( " use="+line[i] );
			} else if (names[i].equals("thresholddark")){
				sb.append( " thresholddark="+line[i] );
			} else if (names[i].equals("thresholdlight")){
				sb.append( " thresholdlight=" + line[i] );
			} else if (names[i].equals("fill")){
				if (line[i] == "TRUE") { 
					sb.append( " fill" );
				}
			} else if (names[i].equals("largest")){
				if (line[i]=="TRUE") {
					sb.append( " largest" );
				}
			} else if (names[i].equals("vignettes")){
				if (line[i]=="TRUE") {
					sb.append( " vignettes" );
				}
			} else if (names[i].equals("scalebar")){
				if (line[i]=="TRUE") {
					sb.append( " scalebar" );
				}
			} else if (names[i].equals("enhance")){
				if (line[i]=="TRUE") {
					sb.append(  " enhance" );
				}
			} else if (names[i].equals("outline")){
				if (line[i]=="TRUE") {
					sb.append( " outline" );
				}
			} else if (names[i].equals("masks")){
				if (line[i]=="TRUE") {
					sb.append( " masks" );
				}
			} else if (names[i].equals("verbose")){
				if (line[i]=="TRUE") {
					sb.append( " verbose" );
				}
			} else if (names[i].equals("log") ){
				if (line[i]=="TRUE") {
					sb.append( " log" );
				}
			} else if (names[i].equals("batch")){
				if (line[i]=="TRUE") {
					sb.append( " batch" );
				}  
			} else {
				System.out.println("Parameter not recognized: " + names[i]);
			}
		}

		return sb.toString();
	}

	private static String unquote(String s){
		if( s.charAt(0) == '"' || s.charAt(0) == '"' ){
			s = s.substring(1);
		}
		if( s.charAt(s.length()-1) == '"' || s.charAt(s.length()-1) == '"' ){
			s = s.substring(0, s.length() - 1) ;
		}
		return s; 
	}
	
	private static void closeAllImages(){
		while (WindowManager.getImageCount() != 0 ) {
			WindowManager.getImage(WindowManager.getImageCount()).close() ;
		}
	}


	
}
