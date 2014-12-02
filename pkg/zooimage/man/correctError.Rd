\name{correctError}
\alias{correctError}

\title{ The ZooImage error correction (manual validation) tools }
\description{
  Open a web page for manual validation and error correction of predicted abundances in samples.
}
\usage{
correctError(zidb, classifier, data = zidbDatRead(zidb), mode = "validation",
fraction = 0.05, sample.min = 100, grp.min = 2, random.sample = 0.1,
algorithm = "rf", diff.max = 0.2, prop.bio = NULL, reset = TRUE,
result = NULL)
}

\arguments{
  \item{zidb}{ Path to a Zidb file. }
  \item{classifier}{ A ZIClass object appropriate for this sample and the desired classification. }
  \item{data}{ A ZIDat or a ZITest object matching that sample (by default, it is the ZIDat object contained in the zidb file). }
  \item{mode}{ The mode to use for error correction. By default, \code{mode = "validation"},
    where particles are manually validated. \code{mode = "demo"} is the same one, but it sorts particles
    according to the Class variable in data, ignoring changes made in the user interface (so that one
    can explain the logic of the process without care about how particles are manually resorted).
    Finally, \code{mode = "stat"} do not display the user interface at all and calculates all steps
    directly to show gain from the process from 0 to 100\% of the particles validated. }
  \item{fraction}{ The fraction of items to validate at each step (1/20th by default). }
  \item{sample.min}{ Minimal number of items to take at each step. }
  \item{grp.min}{ Minimal number of items to take for each group, on average. }
  \item{random.sample}{ Fraction of random sample considered, when validating suspect items. }
  \item{algorithm}{ Machine learning algorithm used to detect suspect items. }
  \item{diff.max}{ Maximum difference allowed between probabilities in first and second class before considering the item is suspect. }
  \item{prop.bio}{ Weight to apply to the groups for considering them as suspects (use biological or external considerations to build this). }
  \item{reset}{ Do we reset analysis in the case a temporary file already exists for that sample (recommended). }
  \item{result}{ Name of the object in the calling environment where the results will be stored (ZITest object).
    If not provided or \code{NULL}, it is the basename of the zidb file without extension plus
    \code{_valid}. }
}

\value{
  Returns nothing. It is called for its side-effect to install a web interface
  for manual validation of samples.
  
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{zidbDatRead}} }
\examples{
# TODO...
}

\keyword{ utilities }
