\name{rsaga.get.usage}
\alias{rsaga.get.usage}
\title{Usage of SAGA command line modules}
\description{\code{rsaga.get.usage} provides information on the 
usage of and arguments required by SAGA command line modules.}
\usage{
rsaga.get.usage(lib, module, env = rsaga.env(), show = TRUE)
}
\arguments{
  \item{lib}{name of the SAGA library}
  \item{module}{name or numeric identifier of 
        SAGA module in library \code{lib}}
  \item{env}{list, setting up a SAGA geoprocessing environment
        as created by \code{\link{rsaga.env}}}
  \item{show}{logical (default: \code{TRUE}; display usage on
        R console?}
}
\details{This funciton is intended to provide information required
to use the \code{\link{rsaga.geoprocessor}} and for writing your 
own high-level interface function for SAGA modules.
R--SAGA interfaces already exist for some SAGA modules, e.g.
\code{\link{rsaga.hillshade}}, \code{\link{rsaga.local.morphometry}}.
For information on the usage and arguments }
\value{The character vector with usage information is invisibly returned.}
%\references{  }
\author{Alexander Brenning}
%\note{}
\seealso{\code{\link{rsaga.geoprocessor}}, \code{\link{rsaga.env}},
    \code{\link{rsaga.html.help}}}
\examples{
\dontrun{
rsaga.get.usage("io_grid",1)
rsaga.get.usage("ta_preprocessor",2)
rsaga.get.usage("ta_morphometry",0)
}
}
\keyword{spatial}
\keyword{interface}
