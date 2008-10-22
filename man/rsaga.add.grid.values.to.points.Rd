\name{rsaga.add.grid.values.to.points}
\alias{rsaga.add.grid.values.to.points}
\title{Add Grid Values to Point Shapefile}
\description{Pick values from SAGA grids and attach
them as a new variables to a point shapefile.
THIS SAGA MODULE CURRENTLY SEEMS TO CRASH SAGA (but not R).}
\usage{
rsaga.add.grid.values.to.points(in.shapefile, in.grids, out.shapefile, 
    method = c("nearest.neighbour", "bilinear",
      "idw", "bicubic.spline", "b.spline"), ...)
}
\arguments{
  \item{in.grids}{Input: character vector with names of 
        (one or more) SAGA
        grid files to be converted into a point shapefile.}
  \item{in.shapefile, out.shapefile}{In/Output: point shapefiles
        (default extension: \code{.shp}).}
  \item{method}{interpolation method to be used;
        choices: nearest neighbour interpolation (default),
        bilinear interpolation, inverse distance weighting,
        bicubic spline interpolation, B-splines.}
  \item{\dots}{Optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment.}
}
\details{Retrieves information from the selected grids at the 
positions of the points of the selected points layer and adds 
it to the resulting layer.}
%\references{}
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA modules)}
\note{This function uses module 0 in SAGA library \code{shapes_grid}.}
\seealso{\code{\link{pick.from.points}}, \code{\link{pick.from.ascii.grid}},
\code{\link{pick.from.saga.grid}}, \code{\link{rsaga.grid.to.points}}}
%\examples{\dontrun{}}
\keyword{spatial}
\keyword{interface}