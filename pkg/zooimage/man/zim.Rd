\name{zim}
\alias{create.zim}
\alias{edit.zim}
\alias{extract.zims}
\alias{is.zim}
\alias{list.dat1.zim}
\alias{list.zim}
\alias{make.zim}
\alias{refresh.zims}
\alias{verify.zim}

\title{ Manipulate .zim files (ZooImage Metadata/Measurements) }
\description{
  Various fonctions to manipulate ZooImage Metadata in .zim format (either '*.zim', or
  '*_dat1.zim' files).
}
\usage{
create.zim(zimfile = NULL, template = NULL, editor = getOption("ZIEditor"),
    edit = TRUE, wait = FALSE)
edit.zim(name, editor = getOption("ZIEditor"), wait = FALSE)
make.zim(dir = ".", pattern = "\\.[tT][iI][fF]$", images =
    list.files(dir, pattern), show.log = TRUE, bell = FALSE)
extract.zims(zipdir = ".", zipfiles = list.files(zipdir, pattern =
    "\\.[zZ][iI][pP]$"), path = NULL, replace = FALSE, check.unzip = TRUE,
    show.log = TRUE, bell = FALSE)
refresh.zims(zipdir = ".", zipfiles = list.files(zipdir, pattern =
    "\\.[zZ][iI][pP]$"), zimdir = NULL, check.zip = TRUE, check.zim = TRUE,
    show.log = TRUE, bell = FALSE)
list.zim(zidir)
list.dat1.zim(zidir)
is.zim(zimfile, check.ext = FALSE)
verify.zim(zimfile, check.ext = FALSE, is.dat1 = FALSE, check.table = FALSE)
}

\arguments{
  \item{zimfile}{ A .zim file }
  \item{name}{ idem }
  \item{template}{ A .zim template to start with, if the .zim file does not exist yet }
  \item{editor}{ A program to use for editing the .zim file }
  \item{edit}{ Do we edit the .zim file that we just created? }
  \item{wait}{ Do we wait that the file is edited? In this case, R is frozen until the editor is closed }
  \item{dir}{ A directory where .zim files will be created }
  \item{pattern}{ The pattern matching for automatically listed images that require a .zim file }
  \item{images}{ The list of images requiring a .zim file (either all image matching 'pattern' in 'dir',
      or provide your own listing here) }
  \item{show.log}{ Do we display the log at the end of the process? }
  \item{bell}{ Do we ring the bell at the end of a long process? }
  \item{zipdir}{ A directory where to find .zip files }
  \item{zipfiles}{ A list of .zip files (by default, all .zip files in 'zipdir') }
  \item{path}{ The path where to extract zims. If \code{NULL}, it is computed from zipdir: it is either
     the same path, or the parent directory if last directory is named '_raw' }
  \item{replace}{ Do we replace existing .zim files? }
  \item{check.unzip}{ Do we test first if Info-Zip 'unzip' program is available? }
  \item{zimdir}{ The directory where the .zim files are located }
  \item{check.zip}{ Do we test first if Info-Zip 'zip' program is available? }
  \item{check.zim}{ Do we verify .zim files before refreshing metadata in .zip files? }
  \item{zidir}{ A directory containing one or more .zim files }
  \item{check.ext}{ Verify that the extension is '.zim' }
  \item{is.dat1}{ Is it a '\_dat1.zim' file, that is a file collecting metadata AND objects measurements? }
  \item{check.table}{ Try to read the table of measurements in the '[Data]' section. Ignored if
    \code{is.dat1 = FALSE} }
}

\value{
  \code{create.zim()}, \code{edit.zim()}, \code{make.zim()} are invoked for their side-effect
  of creating and/or editing .zim files on disk. \code{create.zim()} returns \code{TRUE} invisibly,
  in case of successful creation of all required .zim files, \code{FALSE} otherwise (details
  of problems are logged in the log file using \code{logProcess()}. The same mechanism
  (returning \code{TRUE} or \code{FALSE} invisibly, with detailled description of the problem
  in the log file) is used by \code{extract.zims()} and \code{refresh.zims()} also called for
  their side-effects of manipulating .zim/.zip files.

  \code{list.zim()} and \code{list.dat1.zim()} return either a vector of character strings with
  the name of .zim files, or \code{NA} if there is no .zim files in 'zidir'.

  \code{is.zim()} simply returns \code{TRUE} or \code{FALSE}.

  \code{verify.zim()} returns an integer in case of successful verification of the .zim file. This
  integer represents the number of objects in the measurement table (zero, if there is no
  '[Data]' section in the file, see hereunder). In the case of an error during verification,
  the function returns a character string with explicit description of the problem.
}

\details{
  ZooImage Metadata/Measurements ('.zim' and '_dat1.zim' files, respectively) are
  text files containing metadata (that is, additional information required to process
  the images, like sample identification, information about collection and process of
  the sample, digitizing hardware and software, etc...). These metadata are represented
  as a pair 'key' = 'value' in ANSI encoding and are organized into sections written
  in square brackets on a separate line. For instance, '[Subsample]' defines a
  'Subsample' section. The first line of .zim files must always be 'ZI1' in the case
  of ZooImage version 1. This identifiant allows for making incompatible changes in
  future versions (first line will then become 'ZI2', 'ZI3', etc...) without taking the
  risk to accidentally try processing these newer versions with an old, incompatible
  version of ZooImage in the future. Here are the first few lines of an example .zim file:
  for instance).

    \preformatted{
ZI1
[Image]
Hardware=EPSON 4870
Software=VueScan 8.0.10  # See ZooImage.ini file for VueScan config
...
    }

  After 'ZI1' in the first line, there is a definition of an 'Image' section, with two keys:
  'Hardware' with value 'EPSON 4870' and 'Software' with value 'VueScan 8.0.10', followed
  by a comment (everything after the '#' sign). Take care: since '#' defines a comment, do
  not use it, neither in keys, nor in values!

  Take care to define \bold{unique keys accross all sections}! The section are just there
  to organize your metadata into logical subunits... but they are not considered in the
  process. If you define a key named 'mykey' both in '[Section1]' and in '[Section2]',
  only the first occurence of 'mykey' will be used by ZooImage!

  The ZooImage Measurements ('_dat1.zim' files) are structured the same way, but there is a
  special '[Data]' section at the end that contains a tab-delimited table with all measurements
  done on identified objects, during the image analysis (process of the images). This table
  starts with a header naming the colums, with two first columns being necessary '!Item' and
  'Label'. 'Label' is the name of the image where the object is found and 'Item' is a unique
  identifiant (usually a number) given to that object in the image (i.e., Label+Item is the
  unique identifier of each object in the whole series). The other columns define the
  measurements done on the objects (area, perimeter, length, distribution of gray
  levels, etc.). The amount and name of measurements are not fixed. It is the particular
  ImageJ plugin that you use to process your image that defines them (it means that adding
  new measurements is very easy to do and they are automatically considered by ZooImage).
  Note that these measurements are converted using calibration information, if available.
  That is, lengths are in microns, surfaces are in squre microns and gray levels are in OD (Optical
  Densities), so that, measurements are comparable from picture to picture, even if
  spatial resolution or distribution of gray levels (contrast, luminosity, ...) are not
  exactly the same in all images of the series! The table must also contains four
  additional columns with obligatory names being 'BX', 'BY', 'Width', 'Height'. There
  are the coordinates of the top-left corner of the bounding box around the object (BX, BY)
  and the Width and Height of this box. These fields are required to locate the object in
  the original image. Here is a short abstract of a [Data] section:

    \preformatted{
... (metadata definitions as above)

[Data]
!Item  Label    Area    Perim.  ...(other mes.)	 BX	    BY	  Width	 Height
1	   Smp1+A1	0.4634  0.0582  ...              28.89	0.20  1.42	 0.83
2	   Smp1+A1 	0.0705  0.0248	...              72.40	0.35  2.37	32.16
3	   Smp1+A2 	0.0498  0.0566	...              75.43	0.69  75.44	 0.70
...
    }

  The reasons to choose such a simple text format for representing metadata is simplicity
  and flexibility. Plain text files are readable by any computer program and no sophisticated
  database engine, or database structure, is required to represent those data. Also, besides
  obligatory fields in the metadata sections, you can add as many key=value entries as you need
  to collect together the metadata required in your particular application. ZooImage will
  automatically read them and store them at the right place, available to you at any time
  during your analyses in ZooImage! That way, ZooImage is very flexible and capable to
  process many different kinds of data, even most exotic ones.

  \code{create.zim()} and \code{edit.zim()} call the associated metadata editor
  (by default, the one defined as \code{options(ZIEditor = ....)}. By default, it is a nice
  little text editor with code folding (for sections), syntax coloring and autocompletion
  features to limit keying errors. Of course, you can use any other editor. In particular,
  you can use a spreadsheet, like Excel, Gnumeric, or OpenOffice Calc to process these files.
  This is particularly useful for the tabular '[Data]' section, more comfortably edited in
  as a spreadsheed. Just save your file as dQuote{tab-delimited text file} when you have done
  and close the spreadsheet program (Excel won't allow ZooImage to access the .zim file when
  the file is opened as a spreadsheet). Just redefine \code{options(ZIEditor = ...)} to use,
  e.g, Excel automatically with ZooImage (full path of the 'Excel.exe' file).

  \code{make.zim()} creates one or more .zim files corresponding to the selected list
  of images provided in 'images', and allows for editing them one-by-one. It is the
  basic function for creating all .zim files manually for a set of image to be analyzed
  in ZooImage. See also \code{make.zie()} for an alternative, and automatic way
  to create all those .zim files.

  \code{extract.zims()} and \code{refresh.zims()} work in conjonction with zipped
  TIFF image, as obtained by \code{zip.img()} and \code{zip.img.all()}
  (also done using \code{compress.zid}). In these .zip files, metadata
  is located in the zip archive comment. This comment is extracted into corresponding
  .zim file by \code{extract.zims()} for one or several zip archives. On the opposite,
  these comments are updated in the zip archive with latest information present in
  .zim files using \code{refresh.zims()}.

  The four last function are auxiliary utilities to deal with .zim files. \code{list.zim()}
  and \code{list.dat1.zim()} are convenient functions to list all '*.zim', or '*_dat1.zim'
  files respectively, in a given 'zidir' directory. \code{is.zim()} simply checks if the file
  is a correct .zim file, checking first line of the file that must be 'ZI1' for ZooImage
  version 1 (current one). This routine returns \code{TRUE} or \code{FALSE} according to the
  result (the file extension is also checked if \code{check.ext = TRUE}).

  Finally, \code{verify.zim()} is a very important function. It checks the validity and
  syntax of any .zim file. All required fields are checked. In case of an error, the function
  returns an explicit error message as a character string. On the other hand, if the
  verification process succeeds, the function returns a number corresponding to the number
  of objects whose measurements are recorded in the data table (for a '_dat1.zim'
  file), or '0' (zero, no measurements) for a '.zim' file containing only metadata.

  \code{verify.zim()} checks for the presence of required fields. For .zim files:
  Section '[Image]' with 'Author', 'Hardware', 'Software' and 'ImageType' (for instance,
  "trans_16bits_gray" for a 16bit graylevels picture obtained by transparency, that is, using
  transmitted light) fields, section '[Fraction]' with 'Code' (A, B, C, ...), 'Min' and 'Max'
  (the minimum and maximum mesh sizes used to fraction the sample, or -1 if Min and or Max
  sieves are not used) and section '[Subsample]' with fields 'Subpart' (a number indicating
  how much of the fraction is actually digitized, for instance, 0.15 for 15\% of the fraction),
  'SubMethod' (volumetry, Motoda, etc.), 'CellPart' (the fraction of the digitizing cell actually
  covered by all images made), 'Replicates', 'VolIni' (the volume of seawater, in cubic meters or any
  of your favored unit, that was collected in the sea for this sample) and 'VolPrec', the precision
  at which 'VolIni' is measured, expressed in the same unit.

  For '_dat1.zim' files, the function checks for the presence of all the previous fields, plus:
  '[Process]' section with fields 'Version' (version of the processing function), 'Method'
  (method used to process the images), 'MinSize', 'MaxSize' (the minimum and maximum ecd
  -equivalent circular diameter- of the particule to be considered and measured), 'Calibration'
  (data for gray levels calibration) and 'ProcessPixSize' (data for spatial calibration: size of
  one pixel in microns, or any of your favorite length unit). Column headers 'Item', 'Label', 'BX',
  'BY', 'Height', 'Width', plus at least one additional measrurement are checked too.
  If \code{check.table = TRUE}, the function also tries to read the table of measurements
  and checks for its integrity (it takes longer for checking many large '_dat1.zim' files!).
}

\note{
  Developers have the opportunity to add custom fields (both sections and keys in these sections)
  to be checked by \code{verify.zim()}, in order to match specific uses of ZooImage. Since the
  verification of metadata is a crucial step in the analysis, they are strongly encouraged to
  add such custom rules: the default checking procedure is very basic!

  There are two possible ways to extend verification: adding fields in the list of required ones,
  or using a custom function. The second solution is more complex, but you have the possibility
  to check also the values associated with keys, where the first solution just check the presence
  of those keys, no mather the values associated with them. You can combine both approaches.

  To add required keys to be checked, just create an option(ZI.zim = list(....)) with
  a list of four components: "zim.required", "dat1.zim.required", "dat1.data.required" and
  "active". Put the list of sections (between square brackets) and keys that must be present
  in the '.zim' files in "zim.required" and those that must be present in the '_dat1.zim'
  files in "dat1.zim.required" components. Place a list of column headers that you need in
  the [Data] table in the "dat1.data.required" component of the list. The "active" component
  must be \code{TRUE} to activate the verification of these extra fields and column headers
  (otherwise, they are ignored). See the example hereunder.

  To add your own verification rules, add a R function in the "verify" component of the list.
  That function should be defined as: \code{function(zimfile, ...) # Your code here} and it
  should return either "" (if no error found), or an explicit message in case of error(s).

  Alternatively, you can completelly redefine \code{verify.zim()} in the "verify.all" component
  of the list. In this case, the other rules are completelly ignored, and you must perform
  the whole treatment in your function (start from the code of \code{verify.zim()} to make sure
  your own function has a similar behaviour!

  Look at examples, and you will better understand how this works!
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{logProcess}}, \code{\link{make.zie}}, \code{\link{zip.img}}, \code{\link{zip.img.all}}, \code{\link{compress.zid}}, }
\examples{
    # Create a minimalist .zim file from current template
    (zimfile <- paste(tempfile(), "zim", sep = "."))
    create.zim(zimfile, edit = FALSE)

    # Display its content
    if (interactive()) file.show(zimfile)

    # list zim files in the temporary directory
    list.zim(tempdir())

    # Is this a correct .zim file?
    is.zim(zimfile)
    verify.zim(zimfile) # Returns 0 => verification OK, with 0 records in [Data]


    ##### The rest of this example is for programmers #####
    # Add more required sections and keys for metadata verifications
    # Add more required columns in the table of measurements
    option(ZI.zim = list(active = TRUE,
        zim.required = c("[NewSection]", "requiredkey1", "requiredkey2"),
        dat1.zim.required = c(("[PostProcess]", "requiredkey3"),
        dat1.data.required = c("Area", "Perim.", "Circ.", "Feret"))
    verify.zim(zimfile) # Of course, these new keys are missing!

    # Now, inactivate these extra verifications without deleting them
    oZI.zim <- getOption("ZI.zim")
    oZI.zim$active <- FALSE
    option(ZI.zim = oZI.zim)
    rm(oZI.zim) # not needed any more
    verify.zim(zimfile) # This time, extra verifications are not used any more => OK!

    # Add some verification code to the existing verification procedure
    option(ZI.zim = list(active = TRUE,
        verify = function(zimfile, ...) {
            # Your verification code here, for instance:
            Lines <- scan(zimfile, character(), sep = "\t", skip = 1, flush = TRUE,
		        quiet = TRUE, comment.char = "#")
            # Check if 'Code=B' or 'Code=C', using regular expression
            # Note: look at regular expression syntax (for instance, at:
            #       http://zez.org/article/articleprint/11/)
            # Extra spaces are allowed before and after '=', and after the value
            if (length(grep("^Code\\s*=\\s*[B|C]\\s*$", Lines)) == 0) {
                # The condition is not matched!
                return("[Fraction] Code must be either 'A', or 'B'!")
            } else {
                # Everything is fine: return an empty string
                return("")
            }
        })
    verify.zim(zimfile) # Since Code=A, verification fails!

    # Reset original verification rules
    option(ZI.zim = NULL)

    # Erase the example .zim file
    unlink(zimfile)
}
\keyword{ utilities }
