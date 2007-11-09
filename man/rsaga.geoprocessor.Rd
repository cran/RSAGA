\name{rsaga.geoprocessor}
\alias{rsaga.geoprocessor}
\title{Generic R interface for SAGA modules}
\description{This function is the workhorse of the R--SAGA interface:
    It calls the SAGA command line tool to run SAGA modules and pass
    arguments.}
\usage{
rsaga.geoprocessor(lib, module = NULL, param = list(), silent = TRUE, 
        beep.off = TRUE, show.output.on.console = TRUE, invisible = TRUE, 
        intern = TRUE, env = rsaga.env(), display.command = FALSE, 
        reduce.intern=TRUE, ...)
}
\arguments{
  \item{lib}{Name of the SAGA library to be called (see Details).}
  \item{module}{Number (\code{>=0}) or name of the module to called within
        the library \code{lib} (see Details).}
  \item{param}{A list of named arguments to be passed to the SAGA module
        (see Examples).}
  \item{silent}{SAGA command line option \code{-silent}: avoid asking
        for interactive user input; does NOT turn off SAGA's beep.}
  \item{beep.off}{if \code{TRUE}, try to turn off SAGA's beep using the
        command \code{net stop beep} under Windows; doesn't always work...}
  \item{show.output.on.console}{a logical (default: \code{TRUE}),
        indicates whether to capture 
        the output of the command and show it on the R console
        (see \code{\link{system}}).}
  \item{invisible}{a logical, indicates whether the command window 
        should be visible on the screen.}
  \item{intern}{a logical, indicates whether to make the output 
        of the command an R object}
  \item{env}{A SAGA geoprocessing environment, i.e. currently a list with information
        on the SAGA and SAGA modules paths and the name of the working directory
        in which to look for input and output files.
        (Defaults: see \code{\link{rsaga.env}}.)}
  \item{display.command}{Display the DOS command line for executing the
        SAGA module (including all the arguments to be passed).
        Default: \code{FALSE}.}
  \item{reduce.intern}{If \code{intern=TRUE}, reduce the text output
        of SAGA returned to R by eliminating redundant lines showing
        the progress of module execution etc. (default: \code{TRUE}).}
  \item{\dots}{Additional arguments to be passed to \code{\link{system}}.}
}
\details{This workhorse function establishes the interface between the
SAGA command line program and R by submitting a system call.
This is a low-level function that may be used for directly accessing SAGA;
specific functions such as \code{rsaga.hillshade} are intended to be
more user-friendly interfaces to the most frequently used SAGA modules.
These higher-level interfaces support default values for the arguments 
and perform some error checking; they should therefore be preferred
if available.
}
\value{The type of object returned depends on the \code{intern} argument
passed to \code{\link{system}}.

If \code{intern=FALSE}, a numerical error/success code is returned,
where a value of \code{0} corresponds to success and a non-zero
value indicates an error. Note however that the function always
returns a success value of \code{0} if \code{wait=FALSE}, i.e. if
it does not wait for SAGA to finish.

If \code{intern=TRUE} (default), the console output of SAGA is returned as a
character vector. This character vector lists the input file names and
modules arguments, and gives a more or less detailed report of the
function's progress. Redundant information can be cancelled out by
setting \code{reduce.intern=TRUE}.
}
% \references{ }
\author{Alexander Brenning (R interface); Olaf Conrad and the
SAGA development team (SAGA development)}
\note{Existing output files will be overwritten by SAGA without prompting!

If a terrain analysis function is not directly interfaced by
one of the RSAGA functions, you might still find it in the
growing set of SAGA libraries and modules.
The names of all libraries available in your SAGA installation
can be obtained using
\code{\link{rsaga.get.libraries}} (or by checking the directory
listing of the \code{modules} folder in the SAGA directory).
The names and numeric codes of
all available modules (globally or within a specific library) are
retreived by \code{\link{rsaga.get.modules}}. Full-text search
in library and module names is performed by 
\code{\link{rsaga.search.modules}}.
For information on the usage of SAGA command line modules,
see \code{\link{rsaga.get.usage}}, or the RSAGA interface function
if available.

\code{display.command=TRUE} is mainly intended for debugging purposes
to check if all arguments are passed correctly to SAGA CMD.}
\seealso{\code{\link{rsaga.env}}, \code{\link{rsaga.get.libraries}},
    \code{\link{rsaga.get.modules}}, \code{\link{rsaga.search.modules}},
    \code{\link{rsaga.get.usage}};
    \code{\link{rsaga.esri.wrapper}} for a wrapper for ESRI ASCII/binary grids;
    \code{\link{rsaga.hillshade}} and other higher-level functions.}
\examples{
\dontrun{
rsaga.hillshade("dem","hillshade",exaggeration=2)
# using the RSAGA geoprocessor:
rsaga.geoprocessor("ta_lighting",0,list(ELEVATION="dem.sgrd",SHADE="hillshade",EXAGGERATION=2))
# equivalent DOS command line call:
# saga_cmd.exe ta_lighting 0 -silent -ELEVATION dem.sgrd -SHADE hillshade -EXAGGERATION 2 
}
}
\keyword{spatial}
\keyword{interface}
