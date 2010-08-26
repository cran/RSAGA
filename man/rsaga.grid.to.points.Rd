\name{rsaga.grid.to.points}
\alias{rsaga.grid.to.points}
\alias{rsaga.grid.to.points.randomly}
\title{Convert SAGA grid file to point shapefile}
\description{Convert SAGA grid file to point shapefile -
either completely or only a random sample of grid cells.}
\usage{
rsaga.grid.to.points(in.grids, out.shapefile, 
    in.clip.polygons, exclude.nodata = TRUE, ...)
rsaga.grid.to.points.randomly(in.grid, out.shapefile, freq, ...)
}
\arguments{
  \item{in.grids}{Input: names of (possibly several) SAGA 
        grid files to be converted into a point shapefile.}
  \item{in.grid}{Input: SAGA grid file from which to sample.}
  \item{out.shapefile}{Output: point shapefile
        (default extension: \code{.shp}).
        Existing files will be overwritten!}
  \item{in.clip.polygons}{optional polygon shapefile
        to be used for clipping/masking an area}
  \item{exclude.nodata}{logical (default: \code{TRUE}):
        skip 'nodata' grid cells?}
  \item{freq}{integer >=1: sampling frequency: on average 1 out of 'freq' 
        grid cells are selected}
  \item{\dots}{Optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment.}
}
%\details{}
%\references{}
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA modules)}
\note{These functions use modules 3 and 4 in SAGA library \code{shapes_grid}.}
\seealso{\code{\link{rsaga.add.grid.values.to.points}}}
\examples{\dontrun{
rsaga.grid.to.points.randomly("dem", "dempoints", freq = 20)
}}
\keyword{spatial}
\keyword{interface}
