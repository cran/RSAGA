\name{pick.from.points}
\alias{pick.from.points}
\alias{pick.from.shapefile}
\alias{pick.from.ascii.grid}
\alias{pick.from.saga.grid}
\title{Pick Variable from Spatial Dataset}
\description{These functions pick (i.e. interpolate without worrying too
much about theory) values of a spatial variables from a data stored in a
data.frame, a point shapefile, or an ASCII or SAGA grid, using nearest neighbor
or kriging interpolation. \code{pick.from.points} is the core function that
is called by the different wrappers.}
\usage{
pick.from.points(data, src, pick, 
    method = c("nearest.neighbour", "krige"), 
    set.na = FALSE, radius = 200, nmin = 0, nmax = 100, 
    sill = 1, range = radius, nugget = 0, 
    model = vgm(sill - nugget, "Sph", range = range, nugget = nugget), 
    log = rep(FALSE, length(pick)), X.name = "x", Y.name = "y", cbind = TRUE)
pick.from.shapefile(data, shapefile, X.name = "x", Y.name = "y", ...)
pick.from.ascii.grid(data, file, path, varname, prefix, 
    method = c("nearest.neighbour", "krige"), nodata.values = c(-9999, -99999), 
    at.once, quiet = TRUE, X.name = "x", Y.name = "y", 
    nlines = Inf, cbind = TRUE, range, radius, ...)
pick.from.saga.grid(data, filename, path, varname, prec = 7,
    env = rsaga.env(), ...)
}
\arguments{
  \item{data}{data.frame giving the coordinates (in columns specified by
        \code{X.name, Y.name}) of point locations at which to interpolate
        the specified variables or grid values}
  \item{src, shapefile}{data.frame or point shapefile }
  \item{pick}{variables to be picked (interpolated) from \code{src};
        if missing, use all available variables, except those specified
        by \code{X.name} and \code{Y.name}}
  \item{method}{interpolation method to be used; uses a partial match to
        the alternatives \code{"nearest.neighbor"} (currently the default)
        and \code{"krige"}}
  \item{set.na}{logical: if a column with a name specified in \code{pick}
        already exists in \code{data}, how should it be dealt with?
        \code{set.na=FALSE} (default) only overwrites existing data if the 
        interpolator yields a non-\code{NA} result;
        \code{set.na=TRUE} passes \code{NA} values returned by the interpolator
        on to the results data.frame}
  \item{radius}{numeric value specifying the radius of the local neighborhood
        to be used for interpolation; defaults to 200 map units (presumably meters),
        or, in the functions for grid files, \code{2.5*cellsize}.}
  \item{nmin, nmax}{numeric, for \code{method="krige"} only:
        see \code{krige} function in package \pkg{gstat} }
  \item{sill}{numeric, for \code{method="krige"} only: the overall sill 
        parameter to be used for the variogram}
  \item{range}{numeric, for \code{method="krige"} only: the variogram range}
  \item{nugget}{numeric, for \code{method="krige"} only: the nugget effect}
  \item{model}{for \code{method="krige"} only: 
        the variogram model to be used for interpolation;
        defaults to a spherical variogram with parameters specified by
        the \code{range}, \code{sill}, and \code{nugget} arguments;
        see \code{vgm} in package \pkg{gstat} for details}
  \item{log}{logical vector, specifying for each variable in \code{pick}
        if interpolation should take place on the logarithmic scale
        (default: \code{FALSE})}
  \item{X.name, Y.name}{names of the variables containing the x and y coordinates}
  \item{cbind}{logical: shoud the new variables be added to the input data.frame
        (\code{cbind=TRUE}, the default), or should they be returned as a separate
        vector or data.frame? \code{cbind=FALSE}}
  \item{file}{file name (relative to \code{path}, default file extension \code{.asc})
        of an ASCII grid from which to pick a variable, or an open connection
        to such a file}
  \item{path}{optional path to \code{file}}
  \item{varname}{character string: a variable name for the variable interpolated from
        grid file \code{file} in \code{pick.from.*.grid}; if missing, variable name
        will be determined from \code{file}name by a call to
        \code{\link{create.variable.name}} }
  \item{prefix}{an optional prefix to be added to the \code{varname} }
  \item{nodata.values}{numeric vector specifying grid values that should
        be converted to \code{NA}; in addition to the values specified here,
        the nodata value given in the input grid's header will be used}
  \item{at.once}{logical: should the grid be read as a whole or line by line?
        \code{at.once=FALSE} is useful for processing large grids that do not
        fit into memory; the argument is currently by default \code{FALSE}
        for \code{method="nearest.neighbour"}, and it currently MUST be
        \code{TRUE} for all other methods (in these cases, \code{TRUE} is
        the default value); piecewise processing with \code{at.once=FALSE}
        is always faster than processing the whole grid \code{at.once}}
  \item{quiet}{logical: provide information on the progress of grid processing
        on screen? (only relevant if \code{at.once=FALSE} and
        \code{method="nearest.neighbour"})}
  \item{nlines}{numeric: stop after processing \code{nlines} lines of the
        input grid; useful for testing purposes}
  \item{filename}{character: name of a SAGA grid file, default extension \code{.sgrd}}
  \item{prec}{numeric, specifying the number of digits to be used in converting
        a SAGA grid to an ASCII grid in \code{pick.from.saga.grid}}
  \item{env}{list: RSAGA geoprocessing environment created by 
        \code{\link{rsaga.env}}}
  \item{...}{arguments to be passed to \code{pick.from.points}}
}
% \details{ }
\value{If \code{cbind=TRUE}, columns with the new, interpolated variables
are added to the input data.frame \code{data}.

If \code{cbind=FALSE}, a data.frame only containing the new variables
is returned (possibly coerced to a vector if only one variable is processed).}
\references{ ~put references to the literature/web site here ~ }
\author{Alexander Brenning}
\note{
\code{method="krige"} requires the \pkg{gstat} package.

\code{pick.from.shapefile} requires the \pkg{shapefiles} package.

The nearest neighbour interpolation currently randomly breaks ties if
\code{pick.from.points} is used, and in a deterministic fashion (rounding
towards greater grid indices, i.e. toward south and east) in the grid
functions.
}
\seealso{ \code{\link{grid.to.xyz}}, %\code{\link{vgm}}, \code{\link{krige}}, 
    \code{\link{read.ascii.grid}}, \code{\link{write.ascii.grid}} }
\examples{
# use the meuse data for some tests:
require(gstat)
data(meuse)
data(meuse.grid)
meuse.nn = pick.from.points(data=meuse.grid, src=meuse, 
    pick=c("cadmium","copper","elev"), method="nearest.neighbour")
meuse.kr = pick.from.points(data=meuse.grid, src=meuse,
    pick=c("cadmium","copper","elev"), method="krige", radius=100)
# it does make a difference:
plot(meuse.kr$cadmium,meuse.nn$cadmium)
plot(meuse.kr$copper,meuse.nn$copper)
plot(meuse.kr$elev,meuse.nn$elev)
}
\keyword{spatial}
