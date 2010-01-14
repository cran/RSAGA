\name{rsaga.env}
\alias{rsaga.env}
\title{Set up the RSAGA Geoprocessing Environment}
\description{\code{rsaga.env} creates a list with system-dependent
    information on SAGA path, module path and
    data (working) directory. Such a list is required by all
    RSAGA geoprocessing functions.}
\usage{
rsaga.env( workspace=".", 
    cmd = ifelse(.Platform$OS.type=="windows", "saga_cmd.exe", "saga_cmd"), 
    path, modules, check.libpath = TRUE, check.SAGA = TRUE, 
    check.PATH = .Platform$OS.type == "windows", 
    check.os.default = TRUE,
    os.default.path = ifelse(.Platform$OS.type=="windows", 
        "C:/Progra~1/SAGA-GIS", "/usr/local/bin") )
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
  \item{check.PATH}{if \code{TRUE} (default on Windows), next look for
        SAGA in all the paths in the \code{PATH} environment
        variable; defaults to \code{FALSE} on non-Windows OS}
  \item{check.os.default}{if \code{TRUE}, look for SAGA
        in the folder specified by \code{os.default.path}.}
  \item{os.default.path}{\code{C:/Progra~1/SAGA-GIS}
        (under Windows) or \code{/usr/local/bin}
        (under unix)}
}
\details{
\code{rsaga.env} tries to compile infromation on the SAGA
environment; this is not easy because there is no standard
installation folder and procedure. If \code{path} is
missing, \code{rsaga.env} first looks for an environment
variable \code{SAGA}; if this is undefined, it checks the
current working directory, then the paths given in the 
PATH environment variable, and finally the function's
guess is \code{"C:/Progra~1/SAGA-GIS"} (or \code{"/usr/local/bin"}
on non-Windows systems).

The default \code{modules} folder under Windows is the \code{modules}
subfolder of the SAGA binaries' folder. The \code{SAGA_MLB} environment variable
is not being checked. Under Unix, the default \code{modules}
folder is \code{/usr/local/lib/saga}.
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

The default SAGA folder used to be \code{C:/Progra~1/saga_vc},
now it is \code{C:/Progra~1/SAGA-GIS} because the most recent SAGA version
installs by default in this folder.
}
%\seealso{  }
\examples{
\dontrun{
# Check the default RSAGA environment on your computer:
rsaga.env()
# SAGA data in C:/sagadata, binaries in C:/SAGA-GIS:
myenv <- rsaga.env(workspace="C:/sagadata", path="C:/SAGA-GIS")
# Use the 'myenv' environment for SAGA geoprocessing:
rsaga.hillshade("dem","hillshade",env=myenv)
# ...creates (or overwrites) grid "C:/sagadata/hillshade.sgrd"
# derived from digital elevation model "C:/sagadata/dem.sgrd"
}
}
\keyword{spatial}
\keyword{interface}
