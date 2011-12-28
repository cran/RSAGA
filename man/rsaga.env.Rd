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
    path, modules, version, check.libpath = TRUE, check.SAGA = TRUE, 
    check.PATH = .Platform$OS.type == "windows", 
    check.os.default = TRUE,
    os.default.path = ifelse(.Platform$OS.type=="windows",
        "C:/Progra~1/SAGA-GIS",
        sub('/saga_cmd','',system2('which',args='saga_cmd',stdout=TRUE))) )
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
  \item{version}{optional character string: SAGA GIS (API) version, 
        e.g. \code{"2.0.8"}; if missing, a call to \code{\link{rsaga.get.version}}
        is used to determine version number of SAGA API }
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
  \item{os.default.path}{under Windows, \code{C:/Progra~1/SAGA-GIS};
        under unix, an attempt is made to locate \code{saga_cmd}}
}
\details{IMPORTANT: Unlike R functions such as \code{\link{options}}, which
changes and saves settings somewhere in the background, \code{\link{rsaga.env}}
does not actually 'save' any settings, it simply creates a list that can
(and has to) be passed to other \code{rsaga.*} functions. See example below.

I strongly recommend to install SAGA GIS in \code{"C:/Program Files/SAGA-GIS"} in the
case of English-language Windows platforms (the equivalent non-English installation folder
in the case of non-English Windows versions seems to work as well). If this is the
only SAGA GIS copy on the computer and you do \emph{not} define a Windows environment variable
\code{SAGA}, then RSAGA should normally be able to find your SAGA GIS installation.

\code{rsaga.env} tries to compile infromation on the (R)SAGA
environment. If \code{path} is missing, \code{rsaga.env} first looks for an environment
variable \code{SAGA}; if this is undefined, it checks the
current working directory, then the paths given in the 
PATH environment variable, and finally the function's
guess is \code{"C:/Progra~1/SAGA-GIS"} (or \code{"/usr/local/bin"}
on non-Windows systems).

The default \code{modules} folder under Windows is the \code{modules}
subfolder of the SAGA binaries' folder. The \code{SAGA_MLB} environment variable
is \emph{not} being checked by \code{rsaga.env}.

Under Unix, the default \code{modules} folder is as specified in the
\code{SAGA_MLB} environment variable. If this is empty / not set, then
the following backup path is used. If \code{path} ends with "/bin", 
then "/bin" is changed to "/lib/saga"
and taken as the \code{modules} path; otherwise, \code{/usr/local/lib/saga}
is used.
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
directory using \code{setwd}.}
\seealso{\code{\link{rsaga.get.version}}}
\examples{
\dontrun{
# Check the default RSAGA environment on your computer:
myenv <- rsaga.env()
myenv
# SAGA data in C:/sagadata, binaries in C:/SAGA-GIS, modules in C:/SAGA-GIS/modules:
myenv <- rsaga.env(workspace="C:/sagadata", path="C:/SAGA-GIS")
# Unix: SAGA in /usr/bin (instead of the default /usr/local/bin),
# and modules in /use/lib/saga:
# myenv <- rsaga.env(path="/usr/bin")
# Use the 'myenv' environment for SAGA geoprocessing:
rsaga.hillshade("dem","hillshade",env=myenv)
# ...creates (or overwrites) grid "C:/sagadata/hillshade.sgrd"
# derived from digital elevation model "C:/sagadata/dem.sgrd"

# Same calculation with different SAGA version:
# (I keep several versions in SAGA-GIS_2.0.x folders:)
myenv05 = rsaga.env(path = "C:/Progra~1/SAGA-GIS_2.0.5")
rsaga.hillshade("dem","hillshade205",env=myenv05)
}
}
\keyword{spatial}
\keyword{interface}
