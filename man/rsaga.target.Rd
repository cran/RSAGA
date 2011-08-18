\name{rsaga.target}
\alias{rsaga.target}
\title{Define target grid for interpolation}
\description{Define the resolution and extent of
a target grid for interpolation by SAGA modules
based on (1) fitting the extent to the input data,
(2) an existing SAGA grid file,
(3) user-defined parameters, or (4) the header data of
an ASCII grid. Intended to be used with RSAGA's interpolation
functions. WARNING: THIS FUNCTION LIKELY DOESN'T WORK WITH
SAGA GIS 2.0.5 BECAUSE OF A CHANGE IN THE PARAMETERIZATION
OF TARGET GRIDS...
}
\usage{
rsaga.target(target = c("user.defined", "grid.system", 
    "target.grid", "header"),
    user.cellsize = 100, user.fit.extent = TRUE,
    user.x.extent, user.y.extent, user.bbox,
    system.nx, system.ny, system.xy, system.d,
    target.grid, header)
}
\arguments{
  \item{target}{character: method used for defining the
        target grid}
  \item{user.fit.extent}{Only for \code{target="user.defined"}:
        logical; if \code{TRUE}, use the dimensions of an
        input grid supplied to the SAGA module, e.g.
        to \code{rsaga.ordinary.kriging}.
        The other \code{user.*} variables should not be provided
        if \code{user.fit.extent=TRUE}.}
  \item{user.cellsize}{Only for \code{target="user.defined"}:
        raster resolution}
  \item{user.x.extent, user.y.extent}{
        Only for \code{target="user.defined"}:
        numeric vectors of length 2: minimum and 
        maximum coordinates of grid cell center points}
  \item{user.bbox}{Only for \code{target="user.defined"}:
        alternative way of specifying extent
        (either use \code{bbox} OR \code{user.*.extent}):
        2x2 matrix of the form \code{rbind(user.x.extent,user.y.extent)}.}
  \item{system.nx, system.ny}{Only for \code{target="grid.system"}:
        number of columns and rows of the grid}
  \item{system.xy}{Only for \code{target="grid.system"}:
        numeric vector of length 2 giving the x and y coordinates
        at the center of the grid's lower left cell}
  \item{system.d}{Only for \code{target="grid.system"}:
        cellsize}
  \item{target.grid}{Only for \code{target="target.grid"}:
        character string giving the name of a SAGA grid file
        that specifies the extent and resolution of the
        target grid}
  \item{header}{Only for \code{target="header"}:
        list: ASCII grid header (as returned e.g. by
        \code{\link{read.ascii.grid.header}}) or
        defined manually; must at least have components
        \code{ncols}, \code{nrows}, \code{cellsize},
        and either \code{x/yllcorner} or \code{x/yllcenter}.}
}
%\details{}
%\references{  }
\author{Alexander Brenning}
\note{This function is to be used with RSAGA functions
\code{\link{rsaga.ordinary.kriging}}, \code{\link{rsaga.inverse.distance}},
\code{\link{rsaga.nearest.neighbour}} and \code{\link{rsaga.modified.quadratic.shephard}}
Note that these are currently only compatible with SAGA GIS 2.0.4.
}
\seealso{\code{\link{read.ascii.grid.header}}}
\examples{\dontrun{
# Krige attribute 0 from the points shapefile to
# a grid with the same extent and resolutionn as the
# (pre-existing) geology grid:
rsaga.ordinary.kriging("points", "dem", field = 0, maxdist = 1000,
    target = rsaga.target(target="target.grid",
    target.grid = "geology"))
# Specify a target grid manually (see above):
rsaga.ordinary.kriging("points", "dem", field = 0, radius = 1000,
    target = rsaga.target("grid.system", 
        system.nx = 200, system.ny = 300,
        system.xy = c(604853,7465013), system.d = 50))
}}
\keyword{interface}
\keyword{spatial}
