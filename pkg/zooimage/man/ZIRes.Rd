\name{ZIRes}
\alias{Abd.sample}
\alias{histSpectrum}
\alias{merge.ZITable}
\alias{plot.ZITable}
\alias{plotAbdBio}

\title{ Various function to calculate results ('ZIRes' object) }
\description{
  Having classified items in a 'ZIDat' object, these function calculate various
  statistic descriptors of whole samples (abundances, biomasses, size spectra)
  and they collect them together in a 'ZIRes' table.
}
\usage{
Abd.sample(ZIDat, sample, taxa = NULL, groups = NULL, type = c("absolute",
    "log", "relative"), header = "Abd")
histSpectrum(spect, class = 1:18 * 0.3/3 + 0.2, lag = 0.25, log.scale = TRUE,
    width = 0.1, xlab = "classes (mm)", ylab = if (log.scale)
    "log(abundance + 1)/m^3" else "Abundance (ind./m^3", main = "", ylim = c(0, 2),
    plot.exp = FALSE)
merge.ZITable(x, y, ...)
plot.ZITable(x, y, ...)
plotAbdBio(t, y1, y2, y3, ylim = c(0, 3), xlab = "Date", ylab = "log(abundance + 1)",
    main = "", cols = c("green", "blue", "red"), pchs = 1:3, hgrid = 1:3, vgrid = t,
    vline = NULL, xleg = min(vgrid), yleg = ylim[2], legend = c("series 1",
    "series 2", "series 3"), type = "o")
}

\arguments{
  \item{ZIDat}{ A 'ZIDat' object }
  \item{sample}{ A sample }
  \item{taxa}{ Taxa to consider }
  \item{groups}{ Groups to consider }
  \item{type}{ Type of plot }
  \item{header}{ Header for the variable }
  \item{spect}{ A spectrum to plot }
  \item{class}{ The classes to use for breaking the spectrum }
  \item{lag}{ The lag for where the first class starts }
  \item{log.scale}{ Do we use log scale for the histogram? }
  \item{width}{ Width of bars in the histogram }
  \item{xlab}{ Label for the x axis }
  \item{ylab}{ Label for the y axis }
  \item{main}{ Main title of the graph }
  \item{ylim}{ Limits for the y axis }
  \item{plot.exp}{ Do we plot also the exponential model fitting this spectrum? }
  \item{x}{ A first table }
  \item{y}{ A second table }
  \item{\dots}{ Additionnal tables }
  \item{t}{ The time vector }
  \item{y1}{ The first series to plot against time }
  \item{y2}{ The second series to plot against time }
  \item{y3}{ The third series to plot against time }
  \item{cols}{ Colors to use for the series }
  \item{pchs}{ The points to use for the series }
  \item{hgrid}{ Where to put horizontal grid? }
  \item{vgrid}{ Where to put vertical grid? }
  \item{vline}{ One or more vertical lines to draw }
  \item{xleg}{ X position of the legend }
  \item{yleg}{ Y position of the legend }
  \item{legend}{ The legend for the three series }
}

\author{ Philippe Grosjean & Kevin Denis }

\seealso{ \code{\link{ZIClass}} }
\examples{
##TODO...
}
\keyword{ utilities }

