\name{RSAGA-package}
\alias{RSAGA-package}
\alias{RSAGA}
\docType{package}
\title{RSAGA: SAGA Geoprocessing and Terrain Analysis in R}
\description{RSAGA provides access to geocomputing and terrain analysis functions of SAGA from within R by running the command line version of SAGA. In addition, several R functions for handling and manipulating ASCII grids are provided, including a flexible framework for applying local functions (including predict methods of fitted models) or focal functions to multiple grids.}
\details{
\tabular{ll}{
Package: \tab RSAGA\cr
Type: \tab Package\cr
Version: \tab 0.92-1\cr
Date: \tab 2011-08-18\cr
License: \tab GPL-2\cr
}
RSAGA provides direct access to SAGA functions including a comprehensive set
of terrain analysis algorithms for calculating local morphometric properties
(slope, aspect, curvature), hydrographic characteristics (size, height, and
aspect of catchment areas), and other process-related terrain attributes
(potential incoming solar radiation, topographic wetness index, and more).
In addition, (R)SAGA provides functions for importing and exporting different
grid file formats, and tools for preprocessing grids, e.g. closing gaps or
filling sinks.

RSAGA adds a framework for creating custom-defined focal functions, e.g.
specialized filter and terrain attributes such as the topographic
wind shelter index, within R.  This framework can be used to apply predict
methods of fitted statistical models to stacks of grids representing predictor
variables.  Furthermore, functions are provided for conveniently picking 
values at point locations from a grid using kriging or nearest neighbour 
interpolation.

RSAGA requires the free SAGA GIS (versions 2.0.4 - 2.0.7 are currently 
supported) and its user-contributed modules to be available on your computer.
These can be downloaded under GPL from
\url{http://sourceforge.net/projects/saga-gis/}.  Please check 
\code{\link{rsaga.env}} to make sure that RSAGA can find your 
local installation of SAGA. RSAGA has been tested with SAGA GIS
versions 2.0.4 to 2.0.7; however, interface functions for interpolation
methods currently do not work with SAGA GIS 2.0.5+; either use SAGA GIS 2.0.4
instead, or call interpolation modules directly using the (lower level)
rsaga.geoprocessor function.

Thanks to Olaf Conrad, Andre Ringeler and all the other SAGA
developers and contributors providing this excellent geocomputing tool!
Thanks to Rainer Hurling, Johan van de Wauw and others for helping to adapt SAGA to 
and test on unix platforms.
}
\author{Alexander Brenning <brenning@uwaterloo.ca>}
% \references{ }
\keyword{ package }
\keyword{ spatial }
\keyword{ interface }
% \seealso{ e.g. \code{\link[<pkg>:<pkg>-package]{<pkg>}} }
% \examples{ }
