\name{RealTime}
\alias{read.lst}
\alias{predict.ZIClass.Real.Time}
\alias{Biomass}
\alias{hist.spectrum}
\alias{loop.opts}
\alias{SampleCurrent}
\alias{CompaSamplePrev}
\alias{CompaSampleList}
\alias{process}
\alias{plotLines}
\alias{Export}
\alias{SelectGroups}
\alias{SmpToComp}
\alias{save.loop.res}
\alias{loopAsynch}

\title{ The ZooImage Real time recognition functions for FlowCAM parameters }
\description{
  These functions allow the user to recognize in near real time particles
  digitized with the FlowCAM. Some functions are currently developed for the
  AMORE III project.
}

\usage{
read.lst(x, skip = 2)
predict.ZIClass.Real.Time(object, ZIDat, calc.vars = TRUE, class.only = FALSE,
    type = "class", na.rm = NULL, \dots)
Biomass(tab)
hist.spectrum(spect, breaks = seq(0.05, 0.6, by = 0.05), width = 0.1,
    xlab = "classes (mm)", ylab = "log(abundance + 1)", main = "",
	ylim = c(0, 10))
loop.opts(lst = ".", classif = ZIC, type = NULL, SizeThreshold = NULL,
    Export_Collages = NULL, ZIprevSmp = NULL, ZIlist = NULL, Abd.all = TRUE,
    Abd.gp = NULL, Spec.all = NULL, Spec.gp = NULL, Bio.all = NULL,
	Bio.gp = NULL, breaks = seq(0.05, 3, by = 0.1), conv = ".",
    ZICompAbd = NULL, ZICompSpectra = NULL, ZICompBiomass = NULL,
	ZICompSlope = NULL, ZICompAbd.gp = NULL, ZICompBio.gp = NULL)
SampleCurrent()
CompaSamplePrev()
CompaSampleList()
process()
plotLines()
Export()
SelectGroups(ZIC)
SmpToComp(Prev = NULL, Samples = NULL, Table = NULL)
save.loop.res(lst, Classif, breaks = seq(0.25, 2, by = 0.1), conv = c(1, 0, 1),
    save.dir = NULL)
loopAsynch()
}

\arguments{
  \item{x}{ a connection or a character string giving the name of the FlowCAM
    'list' file to read }
  \item{skip}{ number of row to skip in the 'list' file }
  \item{object}{ the classifier to use for automatic recognition (ZIClass
    object) }
  \item{ZIDat}{ A data frame of the 'list' file which contains measurements made
    by the FlowCAM }
  \item{calc.vars}{ If \code{TRUE}, A function to use to calculate variables
    from the original data frame }
  \item{class.only}{ If \code{TRUE}, return a vector with classification,
    otherwise, return a 'ZIDat' object with 'Ident' column appended to it }
  \item{type}{ For \code{predict()}, if \code{"prob"}, return probabilities
    only if random Forest is the used algorithm, else only classes are returned.
	For \code{loop.opts()}, if \code{"NULL"} barplot are provided, else
	\code{"l"} lines are provided -- not implemented yet --. }
  \item{na.rm}{ If \code{TRUE}, rows with NA values are not classified by the
    algorithm }
  \item{\dots}{ Further arguments passed to the function }
  \item{tab}{ A data.frame resulting of the automatic recognition with an
    'Ident' column }
  \item{spect}{ A vector of log transformed abundance of particles per size
    classes }
  \item{breaks}{ Limits for the classes where to break the spectrum }
  \item{width}{ The width of bars on the barplot }
  \item{xlab}{ Legend of x axis }
  \item{ylab}{ Legend of y axis }
  \item{main}{ General legend of the barplot }
  \item{ylim}{ Numeric vector of y axis limits (min, max) }
  \item{lst}{ A connection or a character string giving the name of the
    FlowCAM 'list' file to read }
  \item{classif}{ The classifier to use for automatic recognition (ZIClass
    object) }
  \item{SizeThreshold}{ Size threshold in microns to filter particles, if
    \code{"NULL"} no size threshold is applied -- not implemented yet --. }
  \item{Export_Collages}{ number of collages to copy in artificial subsamples,
    if \code{"NULL"} no artificial subsample created -- not implemented yet --. }
  \item{ZIprevSmp}{ a connection or a character string giving the name of the
    'list' file to compare, else \code{"NULL"} no comparison. }
  \item{ZIlist}{ a list of character strings giving the name of the 'list' files
    to compare, else \code{"NULL"} no comparison. }
  \item{Abd.all}{ If \code{"TRUE"} only total relative abundance is measured on
    the current sample, else \code{"NULL"}. }
  \item{Abd.gp}{ a character vector with names of groups to compare in relative
    abundance, else \code{"NULL"}. }
  \item{Spec.all}{ If \code{"TRUE"} only total size spectra is provided for the
    current sample, else \code{"NULL"}. }
  \item{Spec.gp}{ a character vector with names of groups to compare in size
    spectra, else \code{"NULL"}. }
  \item{Bio.all}{ If \code{"TRUE"} only total biomass is provided for the
    current sample, else \code{"NULL"}. }
  \item{Bio.gp}{ a character vector with names of groups to compare in relative
    abundance, else \code{"NULL"}. }
  \item{conv}{ Conversion parameters :a table with 4 column: group and the 3
    parameters per group. }
  \item{ZICompAbd}{ If \code{"TRUE"} only total relative abundance is compared
    between the current sample and other(s), else \code{"NULL"}. }
  \item{ZICompSpectra}{ If \code{"TRUE"} only total size spectra is compared
    between the current sample and other(s), else \code{"NULL"}. }
  \item{ZICompBiomass}{ If \code{"TRUE"} only total biomass is compared between
    the current sample and other(s), else \code{"NULL"}. }
  \item{ZICompSlope}{ If \code{"TRUE"} slope of log transformed size spectra are
    compared between the current sample and other(s), else \code{"NULL"}. }
  \item{ZICompAbd.gp}{ a character vector with names of groups to compare in
    relative abundance between the current sample and other(s), else
	\code{"NULL"}. }
  \item{ZICompBio.gp}{ a character vector with names of groups to compare in
    relative biomass between the current sample and other(s), else
	\code{"NULL"}. }
  \item{ZIC}{ a classifier (ZIClass object). }
  \item{Prev}{ a connection or a character string giving the name of the 'list'
    file to compare. }
  \item{Samples}{ a connection or a character string giving the name of the
    directory containing samples to compare. }
  \item{Table}{ a connection or a character string giving the name table to
    compare -- not implemented yet --. }
  \item{Classif}{ the classifier to use for automatic recognition
    (ZIClass object). }
  \item{save.dir}{ a connection or a character string giving the name of the
    directory to save the results. }
}

\value{
  \code{read.lst()} is called to read a 'list' file at regular interval.
  \code{predict.ZIClass.Real.Time()} is used to recognize the read 'list' file
  at regular interval.
  \code{Biomass()} is used to calculate biomass of a sample.
  \code{hist.spectrum()} is used to calculate barplot of log transformed size
  spectra.
  \code{loop.opts()} is used to provide argument to the loop applied at regular
  interval.
  \code{SampleCurrent()} is used to analyze the current sample in near-real time.
  \code{CompaSamplePrev()} is used to analyze and compare the current sample
  with one sample already digitized in near-real time.
  \code{CompaSampleList()} is used to analyze and compare the current sample with
  several samples already digitized in near-real time.
  \code{process()} is used to recognize the current sample in near-real time.
  \code{plotLines()} is used to plot abundance of the current sample in near-real
  time on line graph. -- currently developed --
  \code{Export()} is used to create artificial sub sample for transect.
  -- currently developed --
  \code{SelectGroups()} is used to select groups to plot.
  \code{SmpToComp()} is used to select the sample to compare with the current
  sample.
  \code{save.loop.res()} is used to save results of the automatic recognition of
  the current sample.
  \code{tclFun_()} is used to control the loop.
  \code{loopAsynch()} is the loop which applies functions for the real time
  recogntion.
}

\author{ Kevin Denis <Kevin.Denis@umons.ac.be> and Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\examples{
# TO DO
# 
}

\keyword{ utilities }
