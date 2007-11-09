\name{rsaga.env}
\alias{rsaga.env}
\title{Set up the RSAGA Geoprocessing Environment}
\description{\code{rsaga.env} creates a list with system-dependent
    information on SAGA path, module path and
    data (working) directory. Such a list is required by all
    RSAGA geoprocessing functions.}
\usage{
rsaga.env( workspace=".", cmd="saga_cmd.exe", path, modules,
    check.libpath=TRUE, check.SAGA=TRUE, check.PATH=TRUE,
    check.windowsdefault=.Platform$OS.type=="windows"  )
}
\arguments{
  \item{workspace}{path of the working directory for SAGA;
        defaults to the current directory (\code{"."}).}
  \item{cmd}{name of the SAGA command line program; defaults
        to \code{saga_cmd.exe}, its name under Windows}
  \item{path}{path in which to find \code{cmd}; \code{rsaga.env}
        is usually able to find SAGA on your system if it is
        installed; see Details.}
  \item{modules}{path in which to find SAGA libraries;
        see Details}
  \item{check.libpath}{if \code{TRUE} (default), first look
        for SAGA in the folder where the RSAGA package is
        installed}
  \item{check.SAGA}{if \code{TRUE} (default), next check the
        path given by the environment variable \code{SAGA},
        if it exists}
  \item{check.PATH}{if \code{TRUE} (default), next look for
        SAGA in all the paths in the \code{PATH} environment
        variable}
  \item{check.windowsdefault}{if \code{TRUE}, look for SAGA
        in the folder \code{C:/Progra~1/saga_vc}. While SAGA
        does not have an installer programm, unpacking the
        downloaded SAGA zip file would currently create a folder
        \code{saga_vc}. Therefore this should be a good guess. }
}
\details{
\code{rsaga.env} tries to compile infromation on the SAGA
environment; this is not easy because there is no standard
installation folder and procedure. If \code{path} is
missing, \code{rsaga.env} first looks for an environment
variable \code{SAGA}; if this is undefined, it checks the
current working directory, then the paths given in the 
PATH environment variable, and finally the function's
guess is \code{"C:/Progra~1/saga_vc"}.

The default \code{modules} folder is the value of the
\code{SAGA_MLB} environment variable. If this is undefined,
the \code{"modules"} subfolder of the \code{path} folder
is \code{rsaga.env}'s final guess.
}
\value{A list with components \code{workspace}, \code{cmd},
\code{path}, and \code{modules}, with values as passed to
\code{rsaga.env} or default values as described in the Details
section.}
%\references{}
\author{Alexander Brenning}
\note{Note that the default \code{workspace} is \code{"."},
not \code{getwd()}; i.e. the default SAGA workspace folder
is not fixed, it changes each time you change the R working
directory using \code{setwd}.

There is no guarantee that this package will work with a 
version of SAGA other than 2.0.2. It is therefore recommended
to keep a copy of this SAGA version in the RSAGA library
folder, where \code{RSAGA} will by default look first SAGA.
This version will then be used even if a newer version of
SAGA is available somewhere else on your computer.
}
%\seealso{  }
\examples{
\dontrun{
# Check the default RSAGA environment on your computer:
rsaga.env()
# SAGA data in C:/sagadata, binaries in C:/saga_vc:
myenv <- rsaga.env(workspace="C:/sagadata", path="C:/saga_vc")
# Use the 'myenv' environment for SAGA geoprocessing:
rsaga.hillshade("dem","hillshade",env=myenv)
# ...creates (or overwrites) grid "C:/sagadata/hillshade.sgrd"
# derived from digital elevation model "C:/sagadata/dem.sgrd"
}
}
\keyword{spatial}
\keyword{interface}
