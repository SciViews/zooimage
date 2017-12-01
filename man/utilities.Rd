\name{utilities}
\alias{sampleInfo}
\alias{underscoreToSpace}
\alias{trimString}
\alias{listSamples}
\alias{makeId}
\alias{addClass}
\alias{calcVars}
\alias{dropVars}
\alias{ecd}
\alias{parseIni}
\alias{calibrate}
\alias{getDec}
\alias{zipNoteAdd}
\alias{zipNoteGet}
\alias{makeZIVignettes}

\title{ Various utility functions used by ZooImage }
\description{
  These functions are usually not called directly by the user, but they are
  interesting for developers.
}

\usage{
sampleInfo(filename, type = c("sample", "fraction", "image", "scs",
    "date", "id", "frac", "imgnbr"), ext = "_dat[135][.]zim$")

underscoreToSpace(string)
trimString(string)

listSamples(ZIobj)
makeId(ZIDat)
addClass(ZIDat, ZIobj)
calcVars(x, drop.vars = NULL, drop.vars.def = dropVars())
dropVars()

ecd(area, cells = 1)

parseIni(data, label = "1")

calibrate(ODfile)

getDec()

zipNoteAdd(zipfile, zimfile)
zipNoteGet(zipfile, zimfile = NULL)

makeZIVignettes(orig.dir = getwd(), target.dir = dirname(orig.dir), clean.work = FALSE)
}

\arguments{
  \item{filename}{ name of a file from which to extract information. It is
    supposed to be spelled as: SCS.xxxx-xx-xx.SS+Fnn.ext where 'SCS' is the
    series-cruise-station code, 'xxxx-xx-xx' is the date of collection
    (year-month-day), 'SS' is the unique sample identifier, 'F' is the fraction,
    'nn' is the image number (when there are several images per fraction) or
    'nn.mmm' when there are 'mmm' pictures taken to cover a bigger area of cell
    'nn', and 'ext' is the file extension. }
  \item{type}{ the type of data to extract (see examples) for
    \code{sampleInfo()}, or the type of file to select in \code{selectFile()}. }
  \item{ext}{ the pattern to use (regular expression) to eliminate file
    extension from the 'filename'. }
  \item{string}{ a character string to rework, or a vector of character strings. }
  \item{ZIobj}{ a ZooImage object (here, a 'ZIDat', 'ZIDesc', 'ZITrain' or
    'ZITest' object; most probably one of the last two for \code{addClass()}). }
  \item{ZIDat}{ a 'ZIDat' object, or a data frame with correct column labels. }
  \item{x}{ a data frame, but most probably, a 'ZIDat' object. }
  \item{drop.vars}{ a character vector with names of variables to drop, or
    \code{NULL} (by default) to keep them all. }
  \item{drop.vars.def}{ a second list of variables to drop contained in a
    character vector. That list is supposed to match the name of variables that
    are obviously non informative and that are dropped by default. It can be
    gathered automatically using \code{dropVars()}. That list includes Id,
    Label, Dil, ... (see details, variables with an asterisk). }
  \item{area}{ a numerical vector with areas from which ECDs are calculated
    (Equivalent Circular Diameter, a more suitable term for 2D images than ESD,
	Equivalent Spherical Diameter). }
  \item{cells}{ the number of cells in the particle (colony). If different from 1,
    the area is first divided by the number of cells bevore calculating the cell
	individual ECD. }
  \item{data}{ a vector containing the data to parse. }
  \item{label}{ a label to include for the parsed data. }
  \item{ODfile}{ an image file of O.D. calibrated items that can be used to
    calibrate grayscales. }
  \item{zipfile}{ a zip archive. }
  \item{zimfile}{ a .zim file to use, or to create. If \code{NULL} (default)
    in \code{zipNoteGet()}, the data are not written in a file, but returned. }
  \item{orig.dir}{ the directory containing the data (current directory by
    default) }
  \item{target.dir}{ where to place the results, by defaut, the parent directory
    of \code{orig.dir}}
  \item{clean.work}{ should we clean intermediary items (\code{FALSE}, by default)}
}

\value{
  A string or vector of strings for \code{sampleInfo()}, \code{listSamples()}
  and \code{makeId()}. For those functions, \code{character(0)} is returned to
  indicate a problem (usually with a warning issued to explain it), while
  an empty string (\code{""}) is returned in case there is no corresponding
  element found.

  The data.frame with additional columns for calculated variables with
  \code{calcVars()}. Variables to drop are gathered using \code{dropVars()},
  altogether with a list provided explicitly in the \code{drop.vars =} argument.
  The list of variable names to drop automatically and silently can be stored in
  a variable named \code{ZI.dropVarsDef} or in \code{options(ZI.dropVarsDef = ....)}.

  A vector of numerical values for \code{ecd()}.

  Transformed strings for \code{trimstring()} and \code{underscoreToSpace()}

  \code{parseIni()} reads the data and creates a list of data frames. Each
  entry in the list maps one section in the ini file (with the same name). For
  'key=value' pairs, a one line data frame containing values and with keys as
  column names. The first column of these data frames is named label and get the
  corresponding value passed by the 'label' argument. That way, one can easily
  keep track of entries when data frames originated from various different ini
  files are merged together.

  \code{calibrate()} returns a vector of two numbers with white and black point
  calibration (gray levels corresponding, respectively to O.D. = 0 and
  O.D. = 1.024), plus a "msg" attribute with some explanation in case of problem.

  \code{zipNoteAdd()} returns \code{TRUE} or \code{FALSE} depending if the data
  from the zimfile was successfully added to the zip archive or not. Problem is
  returned in a warning.
  \code{zipNoteGet()} returns the comment included in the zip archive
  (invisibly if 'zimfile' is not \code{NULL}), \code{character(0)} if no comment
  if found, or \code{NULL} in case of a problem. The problem is detailled in a
  warning.
}

\details{
  As these functions are not made to be directly used by end-users, We don't
  give more details here. Developers interested to use these functions are
  encouraged to look at their code in the zooimage package source!

  Here is the list of all variables you got after running the standard version
  of \code{calcVars()} on ZIDat objects made by one of the ZooImage ImageJ
  plugins (you can provide your own version for, e.g., calculating
  more features):

  \tabular{lll}{
    Variable    \tab Description                                                       \tab Origin \cr
        Area \tab Area of the region of interest (ROI)                                 \tab ImageJ \cr
        Mean \tab Average gray value of the ROI                                        \tab ImageJ \cr
      StdDev \tab Standard deviation of the gray values                                \tab ImageJ \cr
        Mode \tab Most frequent gray value within the ROI                              \tab ImageJ \cr
         Min \tab Minimum gray value within the ROI                                    \tab ImageJ \cr
         Max \tab Maximum gray value within the ROI                                    \tab ImageJ \cr
          X* \tab X coordinate of the centroid of the ROI in the image                 \tab ImageJ \cr
          Y* \tab Y coordinate of the centroid of the ROI in the image                 \tab ImageJ \cr
         XM* \tab X coordinate of the center of mass of the ROI in the image           \tab ImageJ \cr
         YM* \tab Y coordinate of the center of mass of the ROI in the image           \tab ImageJ \cr
      Perim. \tab Perimeter of the ROI                                                 \tab ImageJ \cr
         BX* \tab X coordinate of the upper left corner of the bounding rectangle (BR) \tab ImageJ \cr
         BY* \tab Y coordinate of the upper left corner of the BR                      \tab ImageJ \cr
      Width* \tab Width of the rectangle enclosing the ROI                             \tab ImageJ \cr
     Height* \tab Height of the rectangle enclosing the ROI                            \tab ImageJ \cr
       Major \tab Length of the longest axis of the ellipse fitted to the ROI          \tab ImageJ \cr
       Minor \tab Length of the smallest axis of ellipse fitted to the ROI             \tab ImageJ \cr
      Angle* \tab Angle between longest axis and an horizontal line                    \tab ImageJ \cr
       Circ. \tab Circularity of the ROI                                               \tab ImageJ \cr
       Feret \tab Longest Feret diameter                                               \tab ImageJ \cr
      IntDen \tab Sum of the gray values within the ROI                                \tab ImageJ \cr
      Median \tab Median value of the gray values within the ROI                       \tab ImageJ \cr
        Skew \tab Third order moment for the gray value                                \tab ImageJ \cr
        Kurt \tab Fourth order moment for the gray value                               \tab ImageJ \cr
     XStart* \tab X coordinate of initial point for the outline of the ROI             \tab ImageJ \cr
     YStart* \tab Y coordinate of initial point for the outline of the ROI             \tab ImageJ \cr
         Id* \tab Unique identifier of the ROI (Label_Item)                            \tab zooimage \cr
      Label* \tab Unique name of the image                                             \tab zooimage \cr
       Item* \tab Name of the ROI                                                      \tab zooimage \cr
         ECD \tab Equivalent circular diameter of the ROI                              \tab zooimage \cr
        Dil* \tab Dilution coefficient to use for that ROI                             \tab zooimage \cr
 AspectRatio \tab Aspect ratio of the ROI                                              \tab zooimage \cr
    CentBoxD \tab Distance between the centroid and the center of the BR               \tab zooimage \cr
GrayCentBoxD \tab Distance between the center of mass and the center of the BR         \tab zooimage \cr
  CentroidsD \tab Distance between the centroid and the center mass                    \tab zooimage \cr
       Range \tab Range of the gray values in the ROI                                  \tab zooimage \cr
     MeanPos \tab Position of mean gray value in the range of gray values              \tab zooimage \cr
      SDNorm \tab Normalized standard deviation of the gray values                     \tab zooimage \cr
          CV \tab Coefficient of variation of gray values                              \tab zooimage \cr
     MeanDia \tab Mean diameter calculated on Major and Minor                          \tab zooimage \cr
    MeanFDia \tab Mean diameter calculated on Feret and Minor                          \tab zooimage \cr
     Transp1 \tab Transparency calculated using ECD and MeanDia                        \tab zooimage \cr
     Transp2 \tab Transparency calculated using ECD and MeanFDia                       \tab zooimage \cr
  Elongation \tab Elongation of the ROI                                                \tab zooimage \cr
 Compactness \tab Compactness of the ROI                                               \tab zooimage \cr
   Roundness \tab Roundness of the ROI                                                 \tab zooimage \cr
      Class* \tab Manual identification of the vignette for that ROI                   \tab zooimage \cr
  Predicted* \tab Automatic identification of the vignette for that ROI                \tab zooimage \cr
 Predicted2* \tab Second automatic identification of the vignette for that ROI         \tab zooimage
  }

  For the origin, ImageJ = measured during image ananlysis plugin in ImageJ,
  zooimage = calculated either during importation of data, or by
  \code{calcVars()}. Variables whose name ends with an asterisk are dropped by
  default.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{noExtension}}, \code{\link{selectObject}}}

\examples{
# Given a correct ZooImage name for a sample, return parts of it
smp__ <- "MTLG.2010-03-15.H1+A1.03_dat1.zim"
sampleInfo(smp__, "sample")
sampleInfo(smp__, "fraction")
sampleInfo(smp__, "image")
sampleInfo(smp__, "scs")
sampleInfo(smp__, "date")
sampleInfo(smp__, "id")
sampleInfo(smp__, "frac")
sampleInfo(smp__, "imgnbr")
rm(smp__)
sampleInfo(c("ScanG16.2004-10-20+A1.tif", "ScanG16.2004-10-20+B1.tif"),
    type = "sample", ext = extensionPattern("tif"))

# Character strings manipulation functions
underscoreToSpace("Some_string_to_convert")
trimString("    \tString with\textra spaces  \t")

# Variables calculation utilities
df__ <- data.frame(Label = c("Alabel", "AnotherLabel"), Item = c("01", "02"))
makeId(df__)
rm(df__)
ecd(1:10)
ecd(1:10, cells = 2)
ecd(1:10, cells = 1:10)

### TODO: addClass(), calibrate(), calcVars(), parseIni(), zipNoteAdd() and zipNoteGet() examples
}

\keyword{ utilities }
