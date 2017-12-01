\name{ZIRes}
\alias{processSample}
\alias{processSampleAll}
\alias{print.ZIRes}
\alias{rbind.ZIRes}

\title{ Various function to calculate results ('ZIRes' object) }
\description{
  Having classified items in a 'ZIDat' object, these function calculate various
  statistic descriptors of whole samples (abundances, biomasses, size spectra)
  and they collect them together in a 'ZIRes' object.
}
\usage{
processSample(x, sample, keep = NULL,
    detail = NULL, classes = "both", header = c("Abd", "Bio"),
    cells = NULL, biomass = NULL, breaks = NULL)
processSampleAll(path = ".", zidbfiles, ZIClass = NULL, keep = NULL,
    detail = NULL, classes = "both", header = c("Abd", "Bio"),
    cells = NULL, biomass = NULL, breaks = NULL)

\method{print}{ZIRes}(x, \dots)
\method{rbind}{ZIRes}(\dots, deparse.level = 1)
}

\arguments{
  \item{x}{ a 'ZIDat' object or similar data frame for \code{processSample()}, or
    a 'ZIRes' object for the other functions. }
  \item{sample}{ the sample 'Id' to use for selecting items of one sample only,
    in case the object contains data for several samples. It should not be the
    case for 'ZIDat' objects, and you do not have to provide this argument then. }
  \item{keep}{ a character vector with names of the levels to keep in the
    analysis for the classes, or \code{NULL} to keep them all. }
  \item{detail}{ a character vector with names of classes for which to calculate
    separate statistics. The special levels \code{[other]} and \code{[total]}
    are also added. If \code{NULL}, only the total for the sample is returned. }
  \item{classes}{ a character string with \code{"Class"} to use the manual
    classification for splitting particles in the sample, or \code{"Predicted"}
    to use automatic classification, or \code{"both"} (default) to use Class in
    priority, but falling back to Predicted for particles whose Class is not
    defined. One can also specify the name of another factor variable in \code{x}. }
  \item{header}{ a character vector with one or two strings to use as headers for,
    respectively, abundances and biomasses. }
  \item{cells}{ the path to an .rds file containing cells counting models, as used
    by \code{cellCompute()}. }
  \item{biomass}{ a specification for biomass conversion. Can be \code{NULL} (by
    default) for turning off biomass calculation, or a numeric vector of three
    values P1, P2 and P3 (same biomass for all classes with Bio = P1 * ECD^P3 + P2),
    or a data frame with the name of classes in first column, P1, P2 and P3 in
    columns 2 to 4. You must also provide one line with class \code{"[other]"} that
    is used for biomass claculation for classes not otherwise specified. }
  \item{breaks}{ either \code{NULL} (default) to turn off size spectra calculations
    or a numeric vector for ECD classes breaks. }
  \item{path}{ the path containing your ZIDB or ZID files to use for samples processing. }
  \item{zidbfiles}{ a list of ZIDB or ZID files to process in batch. }
  \item{ZIClass}{ a 'ZIClass' object to use to classify particles during the
    process of your samples. }
  \item{\dots}{ further arguments passed to the methods }
  \item{deparse.level}{ integer controlling the way labels are constructed for
    non-matrix-like arguments (defined in the generic, but not used here). }
}

\author{ Philippe Grosjean & Kevin Denis }

\seealso{ \code{\link{ZIClass}} }

\examples{
##TODO...
}

\keyword{ utilities }

