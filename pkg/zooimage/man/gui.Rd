\name{gui}
\alias{ZIDlg}
\alias{AboutZI}
\alias{ExitZI}
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
AboutZI(graphical = FALSE)
ExitZI()
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
  \code{AboutZI()} gives sone information about the current ZooImage version.
  \code{ExitZI()} unloads the zooimage package (for instance, to allow updating it).
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{compress.zid}} }
\examples{
# The dialog box can be started just by issuing
# > ZIDlg()
}
\keyword{ utilities }

