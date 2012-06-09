\name{ZIMan}
\alias{classVignettes}
\alias{classVignettesAll}
\alias{addIdent}
\alias{ZIManRead}
\alias{reclass}
\alias{confusionCompa}
\alias{ZIManCompa}
\alias{SelectParam}
\alias{createThreshold}
\alias{subpartThreshold}

\title{ Manipulate 'ZIMan' objects (containing manual validation of predictions) }
\description{
  Methods to classify vignettes in different folders and manually correct predictions.
  'ZIMan' contain a hierarchy of groups (taxonomic or not) and a link to a
  series of items belonging to these groups. It is obtained after manual validation of
  automatic classification of various objects from .zid files.
}
\usage{
classVignettes(zidfile, ZIDat = NULL, Dir = "_manuValidation", ZIClass, log = TRUE,
    ProbaThreshold = NULL, ProbaBio = NULL, DiffMax = 0.2, Filter = NULL)
classVignettesAll(zidfiles, ZIClass, Dir = "_manuValidation", log = TRUE)
addIdent(RdataFile, Auto)
ZIManRead(dir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)
reclass(ZIMan)
confusionCompa(ZIMan)
ZIManCompa(ZIMan)
createThreshold(ZIDat)
subpartThreshold(ZIDat, Filter = NULL)
}

\arguments{
  \item{zidfile}{ where to read the zidfile to classify? }
  \item{ZIDat}{ a ZIDat object with or without a column of automatic prediction}
  \item{Dir}{ the name to give to the folder containing the directories of taxonomic groups}
  \item{ZIClass}{ a ZIClass object used to predict particles before classifying them in folders}
  \item{log}{ if TRUE information are provided to the user}
  
  \item{ProbaThreshold}{ .... }
  \item{ProbaBio}{ .... }
  \item{DiffMax}{ .... }
  
  \item{Filter}{ filter to apply to the particles before classifying them}
  \item{zidfiles}{ where to read the zidfiles to classify? }
  \item{RdataFile}{ where to read the Rdata file to modify? }
  \item{Auto}{ vector of identification to add in the 'Ident' column}
  \item{dir}{ Where to read data for making a ZIMan object? }
  \item{creator}{ The name of the author of this classification (or the method, if done automatically) }
  \item{desc}{ A short description of this classification }
  \item{keep_}{ Do we keep items in the '\_' subdirectory (corresponding to unclassified ones)? }
  \item{na.rm}{ Do we remove item with missing values? By default, not. }
  \item{ZIMan}{ A 'ZIMan' object }
}

\author{ Kevin Denis <Kevin.Denis@umons.ac.be> }

\seealso{ \code{\link{ZIClass}} }
\examples{
##TODO...
}
\keyword{ utilities }

