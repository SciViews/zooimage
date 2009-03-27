ZooImage1 analyze particle from the command line.
It requires ImageJ 1.35b or later.
by Ph. Grosjean, 2005 (phgrosjean@sciviews.org)
license: GPL 2 or above at your convenience.

(Not that this currently does not work with many plugins!!!)
To run the program, use:

    java -cp ij.jar:. Gray16bits_2400dpi blobs.tif > results.txt (Unix)
    java -cp ij.jar;. Gray16bits_EPSON_2400dpi blobs.tif > results.txt (Windows)

The files ij.jar, Gray16bits_2400dpi.class and blobs.tif are assumed to be in the current directory. The plugins directory must also be in the current directory if any plugins are called. Alternatively, define the "plugins.dir" property so it points to the directory that contains the plugins directory, as in these examples:

    java -Dplugins.dir=/usr/local/ImageJ -cp /usr/local/ij.jar:.Gray16bits_2400dpi blobs.tif > results.txt (Unix)
    java -Dplugins.dir=C:\ImageJ -cp C:\ImageJ\ij.jar;. Gray16bits_2400dpi blobs.tif > results.txt (Windows) 
