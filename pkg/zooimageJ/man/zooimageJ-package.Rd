\name{zooimageJ-package}
\alias{zooimageJ-package}
\docType{package}

\title{ Zooimage bindings for ImageJ }

\description{
  Bindings between R and the Java based image processing and analysis platform
  ImageJ for Zoo/PhytoImage.
}

\details{
  \tabular{ll}{
    Package: \tab zooimageJ\cr
    Type: \tab Package\cr
    Version: \tab 3.0-0\cr
    Date: \tab 2013-01-28\cr
    License: \tab GPL-3\cr
    LazyLoad: \tab yes\cr
  }
  

  On certain Mac OS X versions (like Snow Leopard), it is impossible to start AWT
  from rJava started from R.app or from the terminal. Since ImageJ depends on
  AWT, it will not run. The solution is to use R and RImageJ from within JGR
  \url{http://www.rforge.net/JGR/}.
}

\author{
  Romain Francois & Philippe Grosjean for the R package
  Wayne Rasband for ImageJ

  Maintainer: Philippe Grosjean <phgrosjean@sciviews.org>
}

\references{
  ImageJ: \url{http://rsbweb.nih.gov/ij/index.html}
}

\keyword{ package }
