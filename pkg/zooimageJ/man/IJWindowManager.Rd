\name{IJWindowManager}
\alias{IJWindowManager}
\docType{data}

\title{ IJ WindowManager object }

\description{
  This represents an object of class \code{ij.WindowManager} created when the
  RImageJ package is loaded. This Java class manages all ImageJ windows.
}

\usage{
IJWindowManager
}

\format{
  This is a Java object, instance of the class \code{ij.WindowManager}, that is
  created when the package is loaded.
}

\references{
  The Java documentation of the class WindowManager:
  \url{http://rsbweb.nih.gov/ij/developer/api/ij/WindowManager.html}
}

\seealso{ \code{\link{ImageJ}}, \code{\link{IJ}} }

\examples{
\dontrun{
## Get the number of currently opened ImageJ windows
IJWindowManager$getImageCount()
## Close all currently opened ImageJ windows 
IJWindowManager$closeAllWindows()
}
}

\keyword{utilities}
