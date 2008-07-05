\name{rsaga.parallel.processing}
\alias{rsaga.parallel.processing}
\title{Parallel Processing}
\description{Calculate the size of the local catchment area
(contributing area), the catchment height, catchment slope
and aspect, and flow path length, using parallel processing
algorithms including the recommended multiple flow direction
algorithm. This set of algorithms processes a digital elevation
model (DEM) downwards from the highest to the lowest cell.}
\usage{
rsaga.parallel.processing(in.dem, in.sinkroute, in.weight, 
    out.carea, out.cheight, out.cslope, out.caspect, out.flowpath, 
    step, method = "mfd", linear.threshold = Inf, 
    convergence = 1.1, ...)
}
\arguments{
  \item{in.dem}{input: digital elevation model (DEM) as SAGA grid file
        (default file extension: \code{.sgrd})}
  \item{in.sinkroute}{optional input: SAGA grid with sink routes}
  \item{in.weight}{optional intput: SAGA grid with weights}
  \item{out.carea}{output: catchment area grid}
  \item{out.cheight}{optional output: catchment height grid}
  \item{out.cslope}{optional output: catchment slope grid}
  \item{out.caspect}{optional output: catchment aspect grid}
  \item{out.flowpath}{optional output: flow path length grid}
  \item{step}{integer >=1: step parameter}
  \item{method}{character or numeric: choice of processing algorithm:
        Deterministic 8 (\code{"d8"} or 0),
        Rho 8 (\code{"rho8"} or 1),
        Braunschweiger Reliefmodell (\code{"braunschweig"} or 2),
        Deterministic Infinity (\code{"dinf"} or 3),
        Multiple Flow Direction (\code{"mfd"} or 4, default).}
  \item{linear.threshold}{numeric (number of grid cells):
        threshold above which linear flow (i.e. the Deterministic 8 
        algorithm) will be used;  linear flow is disabled 
        for \code{linear.threshold=Inf} (default)}
  \item{convergence}{numeric >=0: a parameter for tuning convergent/
        divergent flow; default value of \code{1.1} gives realistic
        results and should not be changed}
  \item{\dots}{further arguments to \code{\link{rsaga.geoprocessor}}}
}
\details{Refer to the references for details on the available algorithms.}
\value{The type of object returned depends on the \code{intern} argument
passed to the \code{\link{rsaga.geoprocessor}}. For \code{intern=FALSE}
it is a numerical error code (0: success), or otherwise (default)
a character vector with the module's console output.}
\references{Deterministic 8:

O'Callaghan, J.F., Mark, D.M. (1984): The extraction of drainage networks 
from digital elevation data. Computer Vision, Graphics and Image Processing,
28: 323-344.

Rho 8:

Fairfield, J., Leymarie, P. (1991): Drainage networks from grid digital 
elevation models. Water Resources Research, 27: 709-717.

Braunschweiger Reliefmodell:

Bauer, J., Rohdenburg, H., Bork, H.-R. (1985): Ein Digitales Reliefmodell 
als Vorraussetzung fuer ein deterministisches Modell der Wasser- und 
Stoff-Fluesse. Landschaftsgenese und Landschaftsoekologie, H. 10, 
Parameteraufbereitung fuer deterministische Gebiets-Wassermodelle,
Grundlagenarbeiten zu Analyse von Agrar-Oekosystemen, eds.:
Bork, H.-R., Rohdenburg, H., p. 1-15.

Deterministic Infinity:

Tarboton, D.G. (1997): A new method for the determination of flow directions 
and upslope areas in grid digital elevation models. Water Ressources Research, 
33(2): 309-319.

Multiple Flow Direction:

Freeman, G.T. (1991): Calculating catchment area with divergent flow based 
on a regular grid. Computers and Geosciences, 17: 413-22.

Quinn, P.F., Beven, K.J., Chevallier, P., Planchon, O. (1991): The prediction 
of hillslope flow paths for distributed hydrological modelling using digital 
terrain models. Hydrological Processes, 5: 59-79.
}
\author{Alexander Brenning (R interface), Olaf Conrad (SAGA module)}
\note{This function uses module 0 from SAGA library \code{ta_hydrology}.}
\seealso{\code{\link{rsaga.wetness.index}},
\code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}} }
% \examples{}
\keyword{spatial}
\keyword{interface}
