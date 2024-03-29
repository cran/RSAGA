% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSAGA-core.R
\name{rsaga.esri.wrapper}
\alias{rsaga.esri.wrapper}
\title{Use RSAGA functions for ESRI grids}
\usage{
rsaga.esri.wrapper(
  fun,
  in.esri = TRUE,
  out.esri = TRUE,
  env = rsaga.env(),
  esri.workspace = env$workspace,
  format = "ascii",
  georef = "corner",
  prec = 5,
  esri.extension,
  condensed.res = TRUE,
  clean.up = TRUE,
  intern = TRUE,
  ...
)
}
\arguments{
\item{fun}{function: one of the RSAGA geoprocessing functions, such as \code{\link[=rsaga.close.gaps]{rsaga.close.gaps()}} or \code{\link[=rsaga.hillshade]{rsaga.hillshade()}} etc.}

\item{in.esri}{logical: are input grids provided as ESRI grids (\code{in.esri=TRUE}) or as SAGA grids?}

\item{out.esri}{logical: should output grids be converted to ESRI grids?}

\item{env}{RSAGA environment as returned by \code{\link[=rsaga.env]{rsaga.env()}}}

\item{esri.workspace}{directory for the input and output ESRI ASCII/binary grids}

\item{format}{output file format, either \code{"ascii"} (default; equivalent: \code{format=1}) for ASCII grids or \code{"binary"} (equivalent: \code{0}) for binary ESRI grids (\code{.flt}).}

\item{georef}{character: \code{"corner"} (equivalent numeric code: \code{0}) or \code{"center"} (default; equivalent: \code{1}). Determines whether the georeference will be related to the center or corner of its extreme lower left grid cell.}

\item{prec}{number of digits when writing floating point values to ASCII grid files (only relevant if \code{out.esri=TRUE}).}

\item{esri.extension}{extension for input/output ESRI grids: defaults to \code{.asc} for \code{format="ascii"}, and to \code{.flt} for \code{format="binary"}}

\item{condensed.res}{logical: return only results of the RSAGA geoprocessing function \code{fun} (\code{condensed.res=TRUE}), or include the results of the import and export operations, i.e. the calls to \code{\link[=rsaga.esri.to.sgrd]{rsaga.esri.to.sgrd()}} and \code{\link[=rsaga.sgrd.to.esri]{rsaga.sgrd.to.esri()}}? (see Value)}

\item{clean.up}{logical: delete intermediate SAGA grid files?}

\item{intern}{\code{intern} argument to be passed to \code{\link[=rsaga.geoprocessor]{rsaga.geoprocessor()}}; see Value}

\item{...}{additional arguments for \code{fun}; NOTE: ESRI ASCII/float raster file names should NOT include the file extension (.asc, .flt); the file extension is defined by the \code{esri.extension} and \code{format} arguments!}
}
\value{
The object returned depends on the \code{condensed.res} arguments and the \code{intern} argument passed to the \code{\link[=rsaga.geoprocessor]{rsaga.geoprocessor()}}.

If \code{condensed.res=TRUE} and \code{intern=FALSE}, a single numerical error code (0: success) is returned. If \code{condensed.res=TRUE} and \code{intern=TRUE} (default), a character vector with the module's console  output is returned (invisibly).

If \code{condensed.res=FALSE} the result is a list with components \code{in.res}, \code{geoproc.res} and \code{out.res}. Each of these components is either an error code (for \code{intern=FALSE}) or  (for \code{intern=TRUE}) a character vector with the console output of the input (\code{\link[=rsaga.esri.to.sgrd]{rsaga.esri.to.sgrd()}}), the geoprocessing (\code{fun}), and the output conversion (\code{\link[=rsaga.sgrd.to.esri]{rsaga.sgrd.to.esri()}}) step, respectively. For \code{in.esri=FALSE} or \code{out.esri=FALSE}, the corresponding component is \code{NULL}.
}
\description{
This wrapper converts input grid files provided in ESRI binary (.flt) or ASCII (.asc) formats to SAGA's (version 2) grid format, calls the RSAGA geoprocessing function, and converts the output grids back to the ESRI grid format. Conversion can also be limited to either input or output grids.
}
\details{
ESRI ASCII/float raster file names should NOT include the file extension (.asc, .flt); the file extension is defined by the \code{esri.extension} and \code{format} arguments!
}
\note{
Note that the intermediate grids as well as the output grids may overwrite existing files with the same file names without prompting the user. See example below.
}
\examples{
\dontrun{
rsaga.esri.wrapper(rsaga.hillshade,in.dem="dem",out.grid="hshd",condensed.res=FALSE,intern=FALSE)
# if successful, returns list(in.res=0,geoproc.res=0,out.res=0),
# and writes hshd.asc; intermediate files dem.sgrd, dem.hgrd, dem.sdat,
# hshd.sgrd, hshd.hgrd, and hshd.sdat are deleted.
# hshd.asc is overwritten if it already existed.
}
}
\seealso{
\code{\link[=rsaga.esri.to.sgrd]{rsaga.esri.to.sgrd()}}, \code{\link[=rsaga.sgrd.to.esri]{rsaga.sgrd.to.esri()}}, \code{\link[=rsaga.geoprocessor]{rsaga.geoprocessor()}}, \code{\link[=rsaga.env]{rsaga.env()}}
}
\keyword{interface}
\keyword{spatial}
