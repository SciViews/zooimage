\name{IJ}
\alias{IJ}
\docType{data}

\title{ IJ object }

\description{
  This represents an object of class \code{ij.IJ} created when the RImageJ
  package is loaded.
}

\usage{
IJ
}

\format{
  This is a Java object, instance of the class \code{ij.IJ}, that is created
  when the package is loaded.
}

\details{
 The ImageJ macro language is basically made of calls to methods of the IJ
 class. This object allows to mimic the macro language by using the convenient
 dollar notation. See examples below.
}

\references{
  The Java documentation of the class IJ:
  \url{http://rsbweb.nih.gov/ij/developer/api/ij/IJ.html}
  
  ImageJ macro language:
  \url{http://rsbweb.nih.gov/ij/developer/macro/macros.html}
}

\seealso{ \code{\link{ImageJ}}, \code{\link{IJWindowManager}} }

\examples{
\dontrun{
## Load a JPEG image in ImageJ and show it
logo <- system.file("images", "R.jpg", package = "RImageJ")
image <- IJ$openImage(logo)
image$show()
## Manipulate this image in ImageJ
IJ$run("8-bit")
IJ$run("Invert")
## ... and close it
image$close()
}
}

\keyword{utilities}
