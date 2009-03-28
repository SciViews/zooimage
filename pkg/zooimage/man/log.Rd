\name{log}
\alias{logProcess}
\alias{logView}
\alias{logClear}

\title{ Log long processes into a file }
\description{
  These functions manage the ZooImage logging feature for long-running processes.
}
\usage{
logProcess(message, topic = NULL, file = file.path(tempdir(), "ZooImage.log"),
    logit = TRUE, stop = FALSE, show.log = stop)
logView(file = file.path(tempdir(), "ZooImage.log"), title = "ZooImage log",
    clear = TRUE, warn = FALSE)
logClear(file = file.path(tempdir(), "ZooImage.log"))
}

\arguments{
  \item{message}{ The message to log. }
  \item{topic}{ An optional topic for this message }
  \item{file}{ The log file }
  \item{logit}{ Should the message be logged, or should it behave normally in R
    (displayed immediatelly for \code{stop()}, for instance). }
  \item{stop}{ Is it a \code{stop()} message? }
  \item{show.log}{ Do we show the log file now? }
  \item{title}{ A title for a new log file }
  \item{clear}{ Do we clear a current log first? }
  \item{warn}{ Do we issue a warning if the file is not found? }
}

\value{
  These functions return nothing. They are used for their side-effect of adding messages to a log file.
}

\details{
  The usual behaviour of R, which is to stop code execution at the first error encountered (or \code{stop()}
  call is not suitable for the very long treatments we want to do with ZooImage. Indeed, we want
  to start processes that take many hours and leave the computer unattended. As much as possible,
  we want to recover from an error and, let's say, continue processing the following sample instead
  of halting the code execution. With this approach, we need to log all encountered error. It is the
  role of the functions here to manage the centralized ZooImage log file.
  
  As much as possible, long processes in Zooimage first perform a battery of quick checks to make sure
  the process will run flawlessly. So, it is possible to initiate the long process, watch at those
  checkings, and leave the computer working unattended with peace in mind once the checking phase
  succeeds.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{Progress}}}

\examples{
  logClear()
  logProcess("Something to put in the log file")
  logView()
  logClear()
}
\keyword{ utilities }

