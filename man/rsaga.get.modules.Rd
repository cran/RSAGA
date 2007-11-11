\name{rsaga.get.modules}
\alias{rsaga.get.libraries}
\alias{rsaga.get.lib.modules}
\alias{rsaga.get.modules}
\alias{rsaga.search.modules}
\title{Find SAGA libraries and modules}
\description{These functions list the SAGA libraries
(\code{rsaga.get.libraries}) and modules (\code{rsaga.get.lib.modules},
\code{rsaga.get.modules}) available in a SAGA installation, 
and allow to perform a full-text search among these
functions.}
\usage{
rsaga.get.libraries(path = rsaga.env()$modules,
    dll = .Platform$dynlib.ext)
rsaga.get.lib.modules(lib, env = rsaga.env(), interactive = FALSE)
rsaga.get.modules(libs, env = rsaga.env(), ...)
rsaga.search.modules(text, modules, search.libs = TRUE,
    search.modules = TRUE, env = rsaga.env(), 
    ignore.case = TRUE, ...)
}
\arguments{
  \item{text}{character string to be searched for in the names
        of available libraries and/or modules}
  \item{search.libs, search.modules}{logical (default
        \code{TRUE}: should \code{text} be searched for in
        library and/or module names?}
  \item{ignore.case}{logical (default \code{FALSE}): should
        the text search in library/module names be
        case sensitive?}
  \item{lib, libs}{character vector (\code{libs}) or character
        string (\code{lib}) with the name(s) of library/ies in
        which to look for modules; if \code{libs} is missing,
        all libraries will be processed}
  \item{modules}{optional list:
        result of \code{rsaga.get.modules}; if missing,
        a list of available modules will be retrieved using
        that function}
  \item{env}{list, setting up a SAGA geoprocessing environment
        as created by \code{\link{rsaga.env}}}
  \item{path}{path of SAGA library files (\code{modules} subfolder
        in the SAGA installation folder); defaults to the
        path determined by \code{\link{rsaga.env}}.}
  \item{dll}{file extension of dynamic link libraries}
  \item{interactive}{logical (default \code{FALSE}): should
        modules be returned that can only be executed in
        interactive mode (i.e. using SAGA GUI)?}
  \item{\dots}{currently only \code{interactive} to be passed on
        to \code{rsaga.get.lib.modules}}
}
%\details{ }
\value{\code{rsaga.get.libraries} returns a character vector with the
names of all SAGA libraries available in the folder
\code{env$modules}.

\code{rsaga.get.lib.modules} returns a \code{data.frame} with:
\item{name}{the names of all modules in library \code{lib},}
\item{code}{their numeric identifiers,}
\item{interactive}{and a logical variable indicating whether a
    module can only be executed in interactive (SAGA GUI) mode.}

\code{rsaga.get.modules} returns a list with, for each SAGA
library in \code{libs}, a \code{data.frame} with module information
as given by \code{rsaga.get.lib.modules}. If \code{libs} is missing, 
all modules in all libraries will be retrieved.
}
%\references{ }
\author{Alexander Brenning}
\note{For information on the usage of SAGA command line modules,
see \code{\link{rsaga.get.usage}} (or \code{\link{rsaga.html.help}}),
or the RSAGA interface function, if available.}
\seealso{\code{\link{rsaga.get.usage}}, \code{\link{rsaga.html.help}}, 
    \code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}} }
\examples{
\dontrun{
# make sure that 'rsaga.env' can find 'saga_cmd.exe'
# before running this:
rsaga.get.libraries()
# list all modules in my favorite libraries:
rsaga.get.modules(c("io_grid", "grid_tools", "ta_preprocessor",
    "ta_morphometry", "ta_lighting", "ta_hydrology"))
# list *all* modules (quite a few!):
# rsaga.get.modules(interactive=TRUE)

# find modules that remove sink from DEMs:
rsaga.search.modules("sink")
# find modules that close gaps (no-data areas) in grids:
rsaga.search.modules("gap")
}
}
\keyword{spatial}
\keyword{interface}
