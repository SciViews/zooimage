\name{as.raster.jobjRef}
\alias{as.raster.jobjRef}

\title{ Support for converting ImageJ images to raster images }

\description{
  Support for converting ImageJ images to raster images.
}

\usage{
\S3method{as.raster}{jobjRef}(x, \dots)
}

\arguments{
  \item{x}{ A java reference of class \samp{ij.ImagePlus}. }
  \item{\dots}{ Not used. }
}

\value{
  A raster object. See \link[grDevices]{as.raster}.
}

\author{
  Contributed by Paul Murrell \url{http://www.stat.auckland.ac.nz/~paul/}
}

\seealso{ \code{\link[grDevices]{as.raster}}, \code{\link[graphics]{raster}} }

\examples{
\dontrun{
## Load an image in ImageJ
logo <- system.file("images", "R.jpg", package = "RImageJ")
image <- IJ$openImage(logo)
## Use this image in a R plot
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
rasterImage(image, 100, 300, 150, 350, interpolate = FALSE)
rasterImage(image, 100, 400, 150, 450)
rasterImage(image, 200, 300, 200 + xinch(.5), 300 + yinch(.3), interpolate = FALSE)
rasterImage(image, 200, 400, 250, 450, angle = 15, interpolate = FALSE)
}
}

\keyword{ graphics}
