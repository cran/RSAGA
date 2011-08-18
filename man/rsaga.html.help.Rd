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
  \item{\dots}{additional arguments}
}
\details{Doesn't seem to work with SAGA GIS 2.0.2+, needs to be updated, sorry.
Please use \code{\link{rsaga.get.usage}} or \code{\link{rsaga.search.modules}}
or \code{\link{rsaga.get.modules}} instead. This function is currently only
kept as a place holder for future referencing of SAGA GIS Wiki pages
that are currently under development.}
% \value{ }
% \references{ }
\author{Alexander Brenning}
% \note{}
\seealso{\code{\link{rsaga.get.usage}}, \code{\link{rsaga.geoprocessor}},
    \code{\link{rsaga.env}} }
% \examples{}
\keyword{utilities}
\keyword{interface}
