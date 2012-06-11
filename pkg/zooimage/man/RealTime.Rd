\name{RealTime}
\alias{lstRead}
\alias{realtimeOptions}
\alias{realtimeProcess}
\alias{realtimeLoop}

\title{ The ZooImage Real-time recognition functions for the FlowCAM }
\description{
  These functions allow the user to recognize in near real-time particles
  digitized with the FlowCAM.
}

\usage{
lstRead(lstfile, skip = 2)
realtimeOptions(lstdir = ".", ZIClass, type = "b", size.threshold = NULL,
    breaks = seq(0.05, 3, by = 0.1), conv.dir = ".", images.per.sec = 7,
	flow.cell = 600, concentration = "p/mL", collage = NULL, size = 5, lag = 2,
	abd = NULL, bio = NULL, spectra = NULL, compare.smp = NULL, group = NULL,
    identify = FALSE, log = FALSE, slope = FALSE)
realtimeProcess(List, ZIClass, conv = c(1, 0, 1), collage = NULL,
    flow.cell = 600, images.per.sec = 5, size = 5, lag = 2)
realtimeLoop(delay = 15000)
}

\arguments{
  \item{lstfile}{ a connection or a character string giving the name of the FlowCAM
    'list' file to read }
  \item{skip}{ number of row to skip in the 'list' file }
  \item{lstdir}{ The directory where the FlowCAM .lst file is located }
  \item{ZIClass}{ A classifier object }
  \item{type}{ The type of graph: \code{"b"} for barplot, or \code{"l"} for lines }
  \item{size.threshold}{ The threshold to use for size ofg the particles }
  \item{breaks}{ The breaks for size classes }
  \item{conv.dir}{ The directory where the conversion file is located }
  \item{images.per.sec}{ The number of images per second in autoimage mode }
  \item{flow.cell}{ The width in microns of the FlowCAM cell }
  \item{concentration}{ The unit used for concentration }
  \item{collage}{ How collages are used }
  \item{size}{ The size of the mobile window }
  \item{lag}{ The lag between two mobile windows }
  \item{abd}{ Do we calculate abundances? }
  \item{bio}{ Do we calculate biovolumes/biomasses? }
  \item{spectra}{ Do we calculate size spectra? }
  \item{compare.smp}{ Do we compare to previous samples? }
  \item{group}{ Which group to plot? }
  \item{identify}{ Do we identify points with the mouse? }
  \item{log}{ Plot in log-scale? }
  \item{slope}{ Do we calculate the slope of the size spectrum? }
  \item{List}{ The list to process }
  \item{conv}{ The conversion table for biovolumes/biomasses }
  \item{delay}{ The delay in ms before looking for further FlowCAM data }
}

\value{
  \code{lstRead()} is called to read a 'list' file at regular interval.
  \code{realtimeOptions()} is used to provide argument to the loop applied at
  regular interval.
  \code{realtimeProcess()} is used to recognize the current sample in near-real
  time.
  \code{realtimeLoop()} is the loop which applies functions for the real time
  recogntion.
}

\author{ Kevin Denis <Kevin.Denis@umons.ac.be> and
  Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\examples{
# TO DO
}

\keyword{utilities}
