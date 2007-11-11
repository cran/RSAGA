\name{rsaga.filter.gauss}
\alias{rsaga.filter.gauss}
\title{Gauss Filter}
\description{Smooth a grid using a Gauss filter.}
\usage{
rsaga.filter.gauss(in.grid, out.grid, sigma, 
    radius = ceiling(2 * sigma), ...)
}
\arguments{
  \item{in.grid}{input: SAGA grid file
        (default file extension: \code{.sgrd})}
  \item{out.grid}{output: SAGA grid file}
  \item{sigma}{numeric, >0.0001: standard deviation parameter of
        Gauss filter}
  \item{radius}{positive integer: radius of moving window}
  \item{\dots}{optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment}
}
% \details{ }
\value{The type of object returned depends on the \code{intern} argument
passed to the \code{\link{rsaga.geoprocessor}}. For \code{intern=FALSE}
it is a numerical error code (0: success), or otherwise (default)
a character vector with the module's console output.}
% \references{ }
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA module)}
\note{This function uses module 1 in the SAGA library \code{grid_filter}.

This SAGA module had a bug under 2.0.1 which has been corrected in
    version 2.0.2. (SAGA used to crash when this module was called.)}
\seealso{\code{\link{rsaga.filter.simple}} }
% \examples{ }
\keyword{spatial}
\keyword{interface}
