\name{gui-utilities}
\alias{selectObject}
\alias{selectList}
\alias{selectFile}
\alias{selectGroups}
\alias{createThreshold}
\alias{imageViewer}
\alias{startPgm}
\alias{modalAssistant}

\title{ Various GUI utility functions used by ZooImage }
\description{
  These functions are usually not called directly by the user, but they are
  interesting for developers. They allow to select elements through dialog boxes.
}

\usage{
selectObject(class = "data.frame", default = "", multiple = FALSE,
    title = paste0("Choose a ", class, ":"))
selectList(class = "data.frame", default = "", multiple = FALSE,
    title = paste0("Choose a list (of ", class, "s):"))
selectFile(type = c("ZipZid", "ZimZis", "LstZid", "ZidZidb", "Zip", "Zid", "Zidb",
    "Zim", "Zis", "Zie", "Zic", "Img", "TifPgm", "RData"), multiple = FALSE,
    quote = TRUE, title = NULL)
selectGroups(groups, multiple = TRUE, title = "Select taxa you want to plot")

createThreshold(ZIDat)

imageViewer(dir = getwd(), pgm = getOption("ZI.ImageViewer"))
startPgm(program, cmdline = "", switchdir = FALSE, iconize = FALSE, wait = FALSE)
modalAssistant(title, text, init, options = NULL, check = NULL,
    select.file = NULL, returnValOnCancel = "ID_CANCEL", help.topic = NULL)
}

\arguments{
  \item{class}{ the class of objects to retrieve (or class of list components
    for \code{selectList()}). }
  \item{default}{ the default item selected in the list. }
  \item{multiple}{ are multiple selections allowed? }
  \item{title}{ the title of the dialog box. }
  \item{type}{ the type of file to list in selection dialog box. }
  \item{quote}{ do we add quotes (\") around file names? }
  \item{groups}{ a list of groups to select from. }
  \item{ZIDat}{ a ZIDat object. }
  \item{dir}{ directory to open in the image viewer. }
  \item{pgm}{ program to use as image viewer. If not provided and not defined
    in the \code{option(ZI.ImageViewer = ...)}, then, a reasonable default
    program is used (the file explorer is no better program found). }
  \item{program}{ name of the program to start. It must match an entry in R
    options giving the actual executable that correspond to that program. }
  \item{cmdline}{ the command line to run to start this program. }
  \item{switchdir}{ do we switch R current directory to the directory where
    the program is located? }
  \item{iconize}{ in case the ZooImage assistant is open, do we iconize it? }
  \item{wait}{ do we wait that the external program is closed? }
  \item{text}{ textual explanations to show in the modal assistant. }
  \item{init}{ initial values for the modal assistant. }
  \item{options}{ options to select in the modal assistant. }
  \item{check}{ checkbox to add in the modal assistant. }
  \item{select.file}{ prompt for a file to select in the modal assistant. }
  \item{returnValOnCancel}{ what to return if the user clicks the Cancel
    button in the modal assistant dialog box? }
  \item{help.topic}{ help topic to associate with the Help button of the modal
    assistant dialog box.}
}

\value{
  A string or vector of strings of selected items. \code{character(0)} is
  returned to indicate the user clicked 'Cancel', while an empty string
  (\code{""}) is returned in case there is no corresponding element found.
  
  \code{createThreshold()} proposes a dialog box to create a threshold on one
  variable in a ZIDat object (indicate minimum and maximum value allowed for
  that variable).
  
  For \code{imageViewer()}, \code{TRUE} or \code{FALSE} is returned invisibly,
  depending if the program could be lauched or not. The problem is reported in
  a warning.
  
  \code{startPgm()} is mostly invoked for its side effect of starting an
  external program. Status code returned by the program is returned if
  \code{wait = TRUE}.
  
  \code{modalAssistant()} is currently disabled, and it will thus display no
  dialog box and return \code{returnValOnCancel} directly.
}

\details{
  As these functions are not made to be directly used by end-users, We don't
  give more details here. Developers interested to use these functions are
  encouraged to look at their code in the zooimage package source!
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{listSamples}}, \code{\link{noExtension}}}

\examples{
\dontrun{
## Create two datasets in R and ask for selecting one:
df1__ <- data.frame(x = 1:3, y = 4:6)
df2__ <- data.frame(z = 1:10)
selectObject() # Try also to click 'Cancel'
## Can select both too
selectObject(multiple = TRUE, title = "Choose one or more data.frames")
selectObject("nonexistingclass") # Returns an empty string!

## Create lists containing only data frames as components
lst1__ <- list(A = df1__, B = df2__)
lst2__ <- list(C = df1__)
selectList() # Try also to click 'Cancel'
## Can select both too
selectList(multiple = TRUE, title = "Select one or more lists")
selectList("nonexistingclass")
rm(df1__, df2__, lst1__, lst2__)

## Select one or more ZooImage files
selectFile() # One Zip or Zid file

## Select groups to process
selectGroups(c("Copepods", "Appendicularians", "Medusae"))

## Start default image viewer on the current working directory
imageViewer()

## TODO: examples for createThreshold(), startPgm() and modalAssistant()
}
}

\keyword{ utilities }
