\name{ImageJ}
\alias{ImageJ}
\docType{data}

\title{ ImageJ object }

\description{
  This represents an object of class \code{ij.ImageJ} created when the RImageJ
  package is loaded.
}

\usage{
ImageJ
}

\format{
  This is a Java object, instance of the class \code{ij.ImageJ}, that is created
  when the package is loaded. In case the ImageJ instance cannot be created,
  it contains \code{NULL} (for example, when AWT cannot be instanciated on this
  machine). In this case, you still can access a few \code{IJ} methods that do
  not require AWT, but most of ImageJ features will not be available. You should
  rather make sure ImageJ can be fully instanciated on your machine, and you can
  test this by looking if the ImageJ object contains something else than
  \code{NULL}.
}

\details{
  The ImageJ routines are loaded within R, and all theirs features are
  accessible through the menus and toolbar. Many other aspects of ImageJ can be
  manipulated from within R, by using the associated methods (see
  \code{.jmethods(ImageJ)}).
}

\references{
  The Java documentation of the class IJ:
  \url{http://rsbweb.nih.gov/ij/developer/api/ij/ImageJ.html}
}

\seealso{ \code{\link{IJ}}, \code{\link{IJWindowManager}} }

\examples{
\dontrun{
## Normally the ImageJ toolbar and menus are visible, but you can hide them
ImageJ$hide()
ImageJ$isVisible()
## Redisplay them again
ImageJ$show()
ImageJ$isVisible()
}
}

\keyword{utilities}
