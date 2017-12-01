\name{zidb}
\alias{zidbMake}
\alias{zidbMakeAll}
\alias{zidToZidb}
\alias{zidToZidbAll}
\alias{zidbToZid}
\alias{zidbToZidAll}
\alias{zidbLink}
\alias{zidbDatRead}
\alias{zidbSampleRead}
\alias{zidbPlotNew}
\alias{zidbDrawVignette}

\title{ Manage .zidb files (ZooImage Databases) }
\description{
  Compress, uncompress .zidb files that contain data for a sample. Starting from
  ZooImage 3, the new format uses filehash tables for better performances.
  Conversion from and to the old .zid format (a zip archive indeed) is supported
  for compatibility with old datasets.
}

\usage{
zidbMake(zidir, zidbfile = paste0(sub("[/\\\\]+$", "", zidir), ".zidb"), zisfile =
    file.path(dirname(zidir), "Description.zis"), type = "ZI5", smptime = "",
    check = TRUE, check.vignettes = FALSE, replace = FALSE, delete.source = replace)
zidbMakeAll(path = ".", samples, zisfiles = file.path(dirname(samples),
    "Description.zis"), type = "ZI5", check = TRUE,
    check.vignettes = FALSE, replace = FALSE, delete.source = replace)
    
zidToZidb(zidfile, zisfile = file.path(dirname(zidfile),
    "Description.zis"), replace = FALSE, delete.source = replace)
zidToZidbAll(path = ".", zidfiles, zisfiles = file.path(dirname(zidfiles),
    "Description.zis"), replace = FALSE, delete.source = replace)
zidbToZid(zidbfile, zisfile = file.path(dirname(zidbfile),
    "Description.zis"), replace = FALSE, delete.source = replace)
zidbToZidAll(path = ".", zidbfiles, zisfiles = file.path(dirname(zidbfiles),
    "Description.zis"), replace = FALSE, delete.source = replace)
    
zidbLink(zidbfile)
zidbDatRead(zidbfile)
zidbSampleRead(zidbfile)

zidbPlotNew(main = "ZooImage collage", ...)
zidbDrawVignette(rawimg, type = "jpg", item, nx = 5, ny = 5, vmar = 0.01)
}

\arguments{
  \item{zidir}{ a directory containing data to put in a .zidb files. }
  \item{zidbfile}{ the path of the .zidb file. }
  \item{zidbfiles}{ the path of a series of .zidb files. }
  \item{zidfile}{ the path of a .zid file. }
  \item{zidfiles}{ the path of a series of .zid files. }
  \item{zisfile}{ the path of the .zis file that contains description of this
    sample. }
  \item{zisfiles}{ the path of a series of .zis files that contain description
    of the processed samples. }
  \item{type}{ the ZI file format, currently only 'ZI5' type is supported. For
    \code{zidbDrawVignette(), it is the type of vignette image. Currently, it
    can be either "jpg", or "png". }}
  \item{smptime}{ the time the sample was collected. This value will replace a
    \code{<<<TIME>>>} placeholder in the metadata. You can also use
    \code{<<<SMP>>>} and \code{<<<DATE>>>} placeholders in corresponding metadata
    and they will be replaced by values computed from the sample name. }
  \item{check}{ do we check the files in this directory before/after compression? }
  \item{check.vignettes}{ do we check if the future .zidb file contains all
    vignettes? This is \code{FALSE} by default because they may not exist yet.
    This is the case when they are generated on the fly, like in the new version
    5.5 processes. }
  \item{replace}{ do we replace existing files? }
  \item{delete.source}{ do we delete source files after compression? }
  \item{path}{ look for files in this path. }
  \item{samples}{ a list of 'samples', i.e., subdirectories to process. }
  \item{main}{ the title of the new plot. }
  \item{\dots}{ further arguments passed to the \code{plot()} function. }
  \item{rawimg}{ the raw content of a vignette, as stored in a .zidb file. }
  \item{item}{ the item number to draw (enumeration from left to right and from
    top to bottom). }
  \item{nx}{ the number of vignettes in a column. }
  \item{ny}{ the number of vignettes in a row. }
  \item{vmar}{ the relative size of vignette margins. }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{zidCompress}} }

\examples{
##TODO...
}

\keyword{ utilities }
