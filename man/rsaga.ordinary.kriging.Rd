\name{rsaga.ordinary.kriging}
\alias{rsaga.ordinary.kriging}
\title{Local Ordinary Kriging}
\description{Perform ordinary kriging using a local
search neighborhood (local ordinary kriging).
Also supports block kriging.}
\usage{
rsaga.ordinary.kriging(in.shapefile, out.grid, 
    out.variance.grid, field, 
    model = c("spherical", "exponential", "gaussian"),
    nugget = 0, sill = 10, range = 100,
    log.transform = FALSE, maxdist = 1000, blocksize, 
    nmin = 4, nmax = 20, target = rsaga.target(), ...)
}
\arguments{
  \item{in.shapefile}{Input: point shapefile
        (default extension: \code{.shp}).}
  \item{out.grid}{Output: filename for interpolated grid (SAGA grid file).
        Existing files will be overwritten!}
  \item{out.variance.grid}{Output (optional): SAGA grid for 
        kriging variances}
  \item{field}{numeric(!): number (not name!) of attribute in the
        shapefile's attribute table to be interpolated;
        the first attribute is represented by a zero.}
  \item{model}{character: variogram model to be used;
        defaults to \code{"spherical"}.}
  \item{nugget}{numeric (>=0): Nugget effect}
  \item{sill}{numeric (>=0): Sill of the variogram}
  \item{range}{numeric (>=0): Variogram range}
  \item{log.transform}{logical: apply a log transformation to the observations?
        (default: \code{FALSE}).}
  \item{maxdist}{numeric: maximum distance of nearest points to be used for
        kriging (search radius)}
  \item{nmin}{numeric: Minimum number of points (within the local search
        neighborhood) required for interpolation.}
  \item{nmax}{numeric: Maximum number of nearest points to be used for interpolation}
  \item{blocksize}{numeric: block size for block kriging;
        block kriging is applied if this parameter is specified.
        If \code{blocksize} is missing (default), ordinary (point)
        kriging is used.}
  \item{target}{list: parameters identifying the target area,
        e.g. the lower left corner and size of grid, or name of a
        reference grid; see \code{\link{rsaga.target}}.}
  \item{\dots}{Optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment.}
}
%\details{}
%\references{  }
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA module)}
\note{This function uses module 4 ("Ordinary Kriging") in SAGA
library \code{grid_gridding} (users of the GUI of SAGA GIS should 
not be confused by the fact that the "Ordinary Kriging" module appears
first in the GUI's module listing - it is in fact module 4).

The SAGA module support some other variogram models(?), but I am not
quite sure what they are doing, so they (and the associated 
additional parameters) are currently
not supported by this wrapper function. The module's usage page
also mentions a \code{FORMULA} argument, but this seems to be a 
mistake.}
\seealso{\code{\link{rsaga.inverse.distance}}, 
\code{\link{rsaga.target}}; see also
\code{krige} in package \code{gstat}.}
\examples{\dontrun{
# Krige attribute 0 from the points shapefile to
# a grid with the same extent and resolutionn as the
# (pre-existing) geology grid:
rsaga.ordinary.kriging("points", "dem", field = 0, maxdist = 1000,
    target = rsaga.target(target="target.grid",
    target.grid = "geology"))
# Specify a target grid manually (see rsaga.target):
rsaga.ordinary.kriging("points", "dem", field = 0, radius = 1000,
    target = rsaga.target("grid.system", 
        system.nx = 200, system.ny = 300,
        system.xy = c(604853,7465013), system.d = 50))
}}
\keyword{interface}
\keyword{spatial}
