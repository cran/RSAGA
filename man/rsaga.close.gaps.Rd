\name{rsaga.close.gaps}
\alias{rsaga.close.gaps}
\title{SAGA Module Close Gaps}
\description{Close (Interpolate) Gaps}
\usage{
rsaga.close.gaps(in.dem, out.dem, threshold = 0.1, ...)
}
\arguments{
  \item{in.dem}{input: digital elevation model (DEM) as SAGA grid file
        (default file extension: \code{.sgrd})}
  \item{out.dem}{output: DEM grid file without no-data values (gaps).
        Existing files will be overwritten!}
  \item{threshold}{tension threshold for adjusting the interpolator (default: 0.1)}
  \item{\dots}{optional arguments to be passed to \code{\link{rsaga.geoprocessor}},
        including the \code{env} RSAGA geoprocessing environment}
}
\details{Larger tension thresholds can be used to reduce overshoots and
undershoots in the surfaces used to fill (interpolate) the gaps.}
\value{The type of object returned depends on the \code{intern} argument
passed to the \code{\link{rsaga.geoprocessor}}. For \code{intern=FALSE}
it is a numerical error code (0: success), or otherwise (default)
a character vector with the module's console output.}
%\references{  }
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA module)}
\note{This function uses module 7 from the SAGA library \code{grid_tools}.}
\seealso{\code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}}}
\examples{
\dontrun{
# using SAGA grids:
rsaga.close.gaps("rawdem.sgrd","dem.sgrd")
# using ASCII grids:
rsaga.esri.wrapper(rsaga.close.gaps,in.dem="rawdem",out.dem="dem")
}}
\keyword{spatial}
\keyword{interface}
