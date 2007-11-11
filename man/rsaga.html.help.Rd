\name{rsaga.html.help}
\alias{rsaga.html.help}
\title{HTML help on a SAGA module or library}
\description{This function tries to obtain SAGA's HTML help for
    the specified library or module. NOTE: HTML help files are
    not provided with all SAGA distributions.}
\usage{
rsaga.html.help(lib, module, env = rsaga.env(), ...)
}
\arguments{
  \item{lib}{name of the SAGA library, or one of the \code{rsaga.}
        module functions such as \code{\link{rsaga.hillshade}} }
  \item{module}{name or numeric identifier of 
        SAGA module in library \code{lib}; \code{module=NULL} links
        to the main help page of the SAGA library \code{lib}}
  \item{env}{list, setting up a SAGA geoprocessing environment
        as created by \code{\link{rsaga.env}}}
  \item{\dots}{additional arguments to be passed to \code{\link{browseURL}}}
}
\details{This help is not always available,
    there are some mismatches between libraries and their HTML files,
    and the HTML files are designed for use with SAGA GUI, not with
    the command line version. Some HTML help pages are also linked to
    the wrong module; in this case the SAGA library's help page may
    provide a link to the module's help page.
    
    In many cases \code{\link{rsaga.get.usage}} will provide more reliable
    information.}
% \value{ }
% \references{ }
\author{Alexander Brenning}
% \note{}
\seealso{\code{\link{rsaga.get.usage}}, \code{\link{rsaga.geoprocessor}},
    \code{\link{rsaga.env}}, \code{\link{browseURL}} }
\examples{
\dontrun{rsaga.html.help(rsaga.parallel.processing)}}
\keyword{utilities}
\keyword{interface}
