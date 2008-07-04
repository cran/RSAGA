\name{rsaga.wetness.index}
\alias{rsaga.wetness.index}
\title{SAGA Modules SAGA Wetness Index}
\description{Calculate the SAGA Wetness Index (SWI), a modified
    topographic wetness index (TWI)}
\usage{
rsaga.wetness.index( in.dem, out.wetness.index, 
    out.carea, out.cslope, out.mod.carea, t.param, ...)
}
\arguments{
  \item{in.dem}{input: digital elevation model (DEM) as SAGA grid file
        (default file extension: \code{.sgrd})}
  \item{out.wetness.index}{output (optional): wetness index grid.
        Existing files of the same name will be overwritten!}
  \item{out.carea}{output (optional): catchment area}
  \item{out.cslope}{output (optional): catchment slope}
  \item{out.mod.carea}{output (optional): modified catchment area}
  \item{t.param}{positive numeric value (optional): undocumented}
  \item{\dots}{optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment}
}
\details{The SAGA Wetness Index is similar to the 
Topographic Wetness Index (TWI), but it is based on a modified 
catchment area calculation (\code{out.mod.carea}), which does 
not treat the flow as a thin film as done in the calculation of
catchment areas in conventional algorithms. As a result, the
SWI tends to assign a more realistic, higher potential soil wetness
than the TWI to grid cells situated in valley floors with a small
vertical distance to a channel.}
\value{The type of object returned depends on the \code{intern} argument
passed to the \code{\link{rsaga.geoprocessor}}. For \code{intern=FALSE}
it is a numerical error code (0: success), or otherwise (default)
a character vector with the module's console output.}
\references{Boehner, J., Koethe, R. Conrad, O., Gross, J., 
Ringeler, A., Selige, T. (2002): Soil Regionalisation by 
Means of Terrain Analysis and Process Parameterisation. 
In: Micheli, E., Nachtergaele, F., Montanarella, L. (ed.): 
Soil Classification 2001. European Soil Bureau, Research 
Report No. 7, EUR 20398 EN, Luxembourg. pp.213-222.}
\author{Alexander Brenning (R interface), Juergen Boehner and
Olaf Conrad (SAGA module)}
\note{This function uses module 15 from the SAGA library 
\code{ta_hydrology}.}
\seealso{\code{\link{rsaga.parallel.processing}}, 
\code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}}}
\examples{
\dontrun{
# using SAGA grids:
rsaga.wetness.index("dem.sgrd","swi.sgrd")
}}
\keyword{spatial}
\keyword{interface}
