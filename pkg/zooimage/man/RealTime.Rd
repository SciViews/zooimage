\name{RealTime}
\alias{read.lst}
\alias{realtimeOptions}
\alias{realtimeProcess}
\alias{realtimeLoop}

\title{ The ZooImage Real-time recognition functions for the FlowCAM }
\description{
  These functions allow the user to recognize in near real-time particles
  digitized with the FlowCAM.
}

\usage{
read.lst(x, skip = 2)
realtimeOptions(lstdir = ".", ZIClass, type = "b", SizeThreshold = NULL,
    breaks = seq(0.05, 3, by = 0.1), convdir = ".", ImgPerSec = 7,
	FlowCell = 600, Concentration = "p/mL", Collage = NULL, Size = 5, Lag = 2,
	Abd = NULL, Bio = NULL, Spectra = NULL, Compa = NULL, Group = NULL,
    identify = FALSE, log = FALSE, Slope = FALSE)
realtimeProcess(List, ZIClass, conv = c(1, 0, 1), Collage = NULL,
    FlowCell = 600, ImgPerSec = 5, Size = 5, Lag = 2)
realtimeLoop(delay = 15000)
}

\arguments{
  \item{x}{ a connection or a character string giving the name of the FlowCAM
    'list' file to read }
  \item{skip}{ number of row to skip in the 'list' file }
  \item{lstdir}{ The directory where the FlowCAM .lst file is located }
  \item{ZIClass}{ A classifier object }
  \item{type}{ The type of graph: \code{"b"} for barplot, or \code{"l"} for lines }
  \item{SizeThreshold}{ The threshold to use for size ofg the particles }
  \item{breaks}{ The breaks for size classes }
  \item{convdir}{ The directory where the conversion file is located }
  \item{ImgPerSec}{ The number of images per second in azutoimage mode }
  \item{FlowCell}{ The width in microns of the FlowCAM cell }
  \item{Concentration}{ The unit used for concentration }
  \item{Collage}{ How collages are used }
  \item{Size}{ The size of the mobile window }
  \item{Lag}{ The lag between two mobile windows }
  \item{Abd}{ Do we calculate abundances? }
  \item{Bio}{ Do we calculate biovolumes/biomasses? }
  \item{Spectra}{ Do we calculate size spectra? }
  \item{Compa}{ Do we compare to previous samples? }
  \item{Group}{ Which group to plot? }
  \item{identify}{ Do we identify points with the mouse? }
  \item{log}{ Plot in log-scale? }
  \item{Slope}{ Do we calculate the slope of the size spectrum? }
  \item{List}{ The list to process }
  \item{conv}{ The conversion table for biovolumes/biomasses }
  \item{delay}{ The delay in ms before looking for further FlowCAM data }
}

\value{
  \code{read.lst()} is called to read a 'list' file at regular interval.
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
