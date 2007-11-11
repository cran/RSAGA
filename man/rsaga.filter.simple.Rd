\name{rsaga.filter.simple}
\alias{rsaga.filter.simple}
\title{Simple Filters}
\description{Apply a smoothing, sharpening or edge filter to
a SAGA grid.}
\usage{
rsaga.filter.simple(in.grid, out.grid, mode = "circle", 
    method = c("smooth", "sharpen", "edge"), radius, ...)
}
\arguments{
  \item{in.grid}{input: SAGA grid file
        (default file extension: \code{.sgrd})}
  \item{out.grid}{output: SAGA grid file}
  \item{mode}{character or numeric: shape of moving window,
        either \code{"square"} (=0) or \code{"circle"} (=1, default)}
  \item{method}{character or numeric: \code{"smooth"} (=0),
        \code{"sharpen"} (=1), or \code{"edge"} (=2)}
  \item{radius}{positive integer: radius of moving window}
  \item{\dots}{optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment}
}
% \details{ }
\value{The type of object returned depends on the \code{intern} argument
passed to the \code{\link{rsaga.geoprocessor}}. For \code{intern=FALSE}
it is a numerical error code (0: success), or otherwise (default)
a character vector with the module's console output.

The \code{mode} argument is passed to SAGA as a \code{-MODE} command line
option. This option used to be called \code{-SEARCH_MODE} under
SAGA 2.0.1, so this function will cause an error under SAGA 2.0.1.}
% \references{  }
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA module)}
\note{This function uses module 0 in the SAGA library \code{grid_filter}.}
\seealso{\code{\link{rsaga.filter.gauss}}}
\examples{\dontrun{rsaga.filter.simple("dem","dem-smooth",radius=4)}}
\keyword{spatial}
\keyword{interface}
