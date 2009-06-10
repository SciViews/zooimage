// this macro can run this from the command line
//
// MacOSX  assumes current directory is /Applications/ImageJ
//  java -jar -mx256m ImageJ.app/Contents/Resources/Java/ij.jar -macro plugins/FITVIS/FITVIS_batch.ijm /Users/Shared/data/FIT_VIS_kevin/test-3/test-largest/batchExampleParameters.csv
//
// WIndows XP
// ??

print("FITVIS_Batch.ijm");

bfile = getArgument();
//print("argument="+bfile);

if (lengthOf(bfile) == 0){
  bfile = File.openDialog("Select the batch parameters file");
}
data = File.openAsString(bfile);
data = replace(data, "\"", "");

dir = File.getParent(bfile) + File.separator();

arg = split(data, "\n");
n = arg.length-1;
names = split(arg[0], ",");

for (i = 0; i<n; i++){
	closeImages();
	a = split(arg[i+1], ",");
	s = assembleParameters(dir, names, a);
	//s = assembleParameters(arg[0], arg[i+1]);
	run("FITVIS ", s);
}


function assembleParameters(dir, names, s){
	//select,interval,pixelsize,minsize,maxsize,use,thresholddark,thresholdlight,
	//fill,largest,vignettes,scalebar,enhance,outline,masks,verbose
	r = "";
	//names = split(inNames,",");
	//s = split(inS, ",");
	for (i=0;i<names.length;i++){
		if (names[i]=="select"){
			if (lengthOf(s[i]) != 0){r= r + "select="+dir+s[i];}
		} else if (names[i]=="interval"){
      r = r + " interval=" + s[i];
		} else if (names[i]=="pixelsize"){
		  r = r + " pixelsize="+s[i];
		} else if (names[i] == "minsize"){
		  r = r + " minsize="+s[i];
		} else if (names[i]=="maxsize"){
		  r = r + " maxsize="+s[i];
	  } else if (names[i]=="distance"){
		  r = r + " distance="+s[i];
		} else if (names[i]=="use"){
		  r = r + " use="+s[i];
		} else if (names[i]=="thresholddark"){
		  r = r + " thresholddark="+s[i];
		} else if (names[i] == "thresholdlight"){
		  r = r + " thresholdlight=" + s[i];
		} else if (names[i]=="fill"){
		  if (s[i] == "TRUE") { r = r + " fill";}
		} else if (names[i]=="largest"){
		  if (s[i]=="TRUE") {r = r + " largest";}
		} else if (names[i] == "vignettes"){
		  if (s[i]=="TRUE") {r = r+ " vignettes";}
		} else if (names[i] == "scalebar"){
		  if (s[i]=="TRUE") {r = r + " scalebar";}
		} else if (names[i] == "enhance"){
		  if (s[i]=="TRUE") {r = r + " enhance";}
		} else if (names[i] == "outline"){
		  if (s[i]=="TRUE") {r = r + " outline";}
		} else if (names[i] == "masks"){
		  if (s[i]=="TRUE") {r = r + " masks";}
		} else if (names[i] == "verbose"){
		  if (s[i]=="TRUE") {r = r + " verbose";}
		} else if (names[i] == "log") {
		  if (s[i]=="TRUE") {r = r + "log";}
		} else if (names[i] == "batch"){
		  if (s[i]=="TRUE") {r = r + " batch";}  
		} else {
		  print("Parameter not recognized: " + names[i]);
		}
	}//i-loop

  //if (indexOf(r, " ") == 0) {r = substring(r, 1);}
    
	return r;
}

//closes all of the image windows
function closeImages(){
  while (nImages()>0) {
    selectImage(nImages());  
    run("Close");
  }
  return;
}

