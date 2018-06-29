\name{zic}
\alias{zicCheck}

\title{ Check .zic files (ZooImage Categories) }
\description{
  Categories (i.e., plankton taxa), with possibly several sub-levels are defined
  in .zic files. This function check the files are correct.
}

\usage{
zicCheck(zicfile)
}

\arguments{
  \item{zicfile}{ the name of the .zic file to test. }
}

\value{
  This function return \code{TRUE} or \code{FALSE}, depending on the content
  of the tested file.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{prepareTrain}} }

\examples{
## Check that Detailed.zic file in the /etc subdir is correct
zicCheck(file.path(svMisc::getTemp("ZIetc"), "Detailed.zic"))
}

\keyword{ utilities }
