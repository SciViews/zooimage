\name{import}
\alias{readFlowCAMctx}
\alias{readFlowCAMlst}
\alias{importFlowCAM}

\title{ Function to import data from the FlowCAM }
\description{
  These functions read data and metadata from FlowCAM runs and import them into
  ZooImage objects.
}

\usage{
readFlowCAMctx(ctx, stop.it = TRUE)
readFlowCAMlst(lst, skip = 2, read.ctx = TRUE)
importFlowCAM(lst, rgb.vigs = FALSE,  type = "ZI3", replace = FALSE)
}

\arguments{
  \item{ctx}{ the path to a .ctx FlowCAM file. }
  \item{stop.it}{ should the process stop in case of an error? Set this to
    \code{FALSE} when you run in batch mode and want the error to be repported
	back to the calling code. }
  \item{lst}{ the path to a .lst FlowCAM file. }
  \item{skip}{ the number of lines to skip in the .lst table before reading the
    data. This is usally two lines. }
  \item{read.ctx}{ should we also read the .ctx file with \code{readFlowCAMctx()}? }
  \item{rgb.vigs}{ do we build color vignettes that mix OD, visual and mask in the
    three RGB channels? By default, not (FALSE) }
  \item{type}{ the type of \code{.zidb} file to create. Currently, only supports
    \code{type = "ZI3"} (defaulf value). }
  \item{replace}{ a boolean indicating if an existing \code{.zidb} file should
    be replaced by a new one. }
}

\value{
  A \code{zidb} object with the converted data, metadata and images.
}

\details{
  The Visual Spreadsheet software provided with the FlowCAM is constantly
  changing specifications from version to version. This is mainly tested with
  VS 1.5.14 and 3.2.3.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{selectObject}}}

\examples{
## TODO...
}

\keyword{ utilities }
