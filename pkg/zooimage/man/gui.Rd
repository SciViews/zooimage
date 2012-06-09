\name{gui}
\alias{ZIDlg}
\alias{aboutZI}
\alias{exitZI}
\alias{closeAssistant}
\alias{closeZooImage}
\alias{viewManual}
\alias{calib}
\alias{optInOutDecimalSep}

\title{ The ZooImage GUI (Graphical User Interface) }
\description{
  These function display menus and dialog boxes to access ZooImages function without programming.
  Most of them are not intended to be called directly.
}
\usage{
ZIDlg()
aboutZI(graphical = FALSE)
exitZI()
closeAssistant()
closeZooImage()
viewManual()
calib()
optInOutDecimalSep()
}

\arguments{
  \item{graphical}{ If \code{TRUE}, a graphical dialog box displays the information.
    Otherwise, information is printed at the R console. }
}

\value{
  \code{ZIDlg()} is called for its side effect of dispalying the main ZooImage dialog box.
  \code{aboutZI()} gives sone information about the current ZooImage version.
  \code{exitZI()} unloads the zooimage package (for instance, to allow updating it).
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{zidCompress}} }
\examples{
# The dialog box can be started just by issuing
# > ZIDlg()
}

\keyword{ utilities }
