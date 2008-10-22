\name{rsaga.inverse.distance}
\alias{rsaga.inverse.distance}
\alias{rsaga.nearest.neighbour}
\alias{rsaga.triangulation}
\alias{rsaga.modified.quadratic.shephard}
\title{Spatial Interpolation Methods}
\description{Spatial interpolation of point data
using inverse distance to a power (inverse distance weighting,
IDW), nearest neighbors, or modified quadratic shephard.}
\usage{
rsaga.inverse.distance(in.shapefile, out.grid, field, 
        power = 1, maxdist = 100, nmax = 10,
        target = rsaga.target(), ...)
rsaga.nearest.neighbour(in.shapefile, out.grid, field,
    target = rsaga.target(), ...)
rsaga.modified.quadratic.shephard(in.shapefile, out.grid, field,
    quadratic.neighbors = 13, weighting.neighbors = 19,
    target = rsaga.target(), ...)
}
\arguments{
  \item{in.shapefile}{Input: point shapefile
        (default extension: \code{.shp}).}
  \item{out.grid}{Output: filename for interpolated grid (SAGA grid file).
        Existing files will be overwritten!}
  \item{field}{numeric(!): number (not name!) of attribute in the
        shapefile's attribute table to be interpolated;
        the first attribute is represented by a zero.}
  \item{power}{numeric (>0): exponent used in inverse distance 
        weighting (usually 1 or 2)}
  \item{maxdist}{numeric: maximum distance of points to be used for
        inverse distance interpolation (search radius)}
  \item{nmax}{Maximum number of nearest points to be used for interpolation}
  \item{quadratic.neighbors}{integer >=5; ??}
  \item{weighting.neighbors}{integer >=3; ??}
  \item{target}{list: parameters identifying the target area,
        e.g. the lower left corner and size of grid, or name of a
        reference grid; see \code{\link{rsaga.target}}.}
  \item{\dots}{Optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment.}
}
\details{Inverse distance weighting (IDW) uses module 0 in the 
SAGA library \code{grid_gridding}.
Nearest neighbour interpolation uses module 1, and triangulation 
is performed by module 4.}
\references{QSHEP2D: Fortran routines implementing the Quadratic Shepard 
method for bivariate interpolation of scattered data 
(see R. J. Renka, ACM TOMS 14 (1988) pp.149-150).
Classes: E2b. Interpolation of scattered, non-gridded 
multivariate data.}
\author{Alexander Brenning (R interface), Andre Ringeler and Olaf Conrad (SAGA modules)}
\note{
See the example section in the help file for \code{write.shapefile}
in package \code{shapefiles} to learn how to apply these interpolation
functions to a shapefile exported from a data.frame.

Modified Quadratic Shephard method: based on module 660 in TOMS (see references).}
\seealso{\code{\link{rsaga.ordinary.kriging}}, and \code{idw} in package
\code{gstat}.}
% \examples{}
\keyword{spatial}
\keyword{interface}
