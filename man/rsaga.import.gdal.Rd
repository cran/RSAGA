\name{rsaga.import.gdal}
\alias{rsaga.import.gdal}
\title{Import Grid Files to SAGA grid format using GDAL}
\description{These functions provide simple interfaces for reading and
writing grids from/to ASCII grids and Rd files. Grids are stored in
matrices, their headers in lists.}
\usage{
rsaga.import.gdal(in.grid, out.grid, saga.version = "2.0.4", ...)
}
\arguments{
  \item{in.grid}{file name of a grid in a format supported by GDAL}
  \item{out.grid}{output SAGA grid file name;
    defaults to \code{in.grid} with the file extension being removed;
    file extension should not be specified, it defaults to .sgrd}
  \item{saga.version}{character string, indicating the SAGA GIS version number;
    currently only 2.0.4 and 2.0.5 are supported. The reason for providing
    SAGA GIS version support is that some of the SAGA command line argument
    names have changed}
  \item{...}{additional arguments to be
        passed to \code{rsaga.geoprocessor} }
}
\details{The GDAL Raster Import module of SAGA imports grid data from various 
file formats using the Geospatial Data Abstraction Library (GDAL) by 
Frank Warmerdam. More information is available at \url{http://www.gdal.org/}

If \code{in.grid} has more than one band (e.g. RGB GEOTIFF), then output grids
with file names of the form \eqn{in.grid{\_}01.sgrd}{in.grid_01.sgrd},
\eqn{in.grid{\_}02.sgrd}{in.grid_02.sgrd} etc. are written,
one for each band.

The following raster formats are currently supported:
\itemize{
\item{VRT}{Virtual Raster}
\item{GTiff}{GeoTIFF}
\item{NITF}{National Imagery Transmission Format}
\item{HFA}{Erdas Imagine Images (.img)}
\item{SAR-CEOS}{CEOS SAR Image}
\item{CEOS}{CEOS Image}
\item{ELAS}{ELAS}
\item{AIG}{Arc/Info Binary Grid}
\item{AAIGrid}{Arc/Info ASCII Grid}
\item{SDTS}{SDTS Raster}
\item{DTED}{DTED Elevation Raster}
\item{PNG}{Portable Network Graphics}
\item{JPEG}{JPEG JFIF}
\item{MEM}{In Memory Raster}
\item{JDEM}{Japanese DEM (.mem)}
\item{GIF}{Graphics Interchange Format (.gif)}
\item{ESAT}{Envisat Image Format}
\item{BSB}{Maptech BSB Nautical Charts}
\item{XPM}{X11 PixMap Format}
\item{BMP}{MS Windows Device Independent Bitmap}
\item{PCIDSK}{PCIDSK Database File}
\item{PNM}{Portable Pixmap Format (netpbm)}
\item{DOQ1}{USGS DOQ (Old Style)}
\item{DOQ2}{USGS DOQ (New Style)}
\item{ENVI}{ENVI .hdr Labelled}
\item{EHdr}{ESRI .hdr Labelled}
\item{PAux}{PCI .aux Labelled}
\item{MFF}{Atlantis MFF Raster}
\item{MFF2}{Atlantis MFF2 (HKV) Raster}
\item{FujiBAS}{Fuji BAS Scanner Image}
\item{GSC}{GSC Geogrid}
\item{FAST}{EOSAT FAST Format}
\item{BT}{VTP .bt (Binary Terrain) 1.3 Format}
\item{L1B}{NOAA Polar Orbiter Level 1b Data Set}
\item{FIT}{FIT Image}
\item{USGSDEM}{USGS Optional ASCII DEM}
\item{GXF}{GeoSoft Grid Exchange Format}
}
}
% \value{ }
\references{GDAL website: \url{http://www.gdal.org/}}
\author{Alexander Brenning (R interface), Olaf Conrad / Andre Ringeler (SAGA module),
    Frank Warmerdam (GDAL)}
% \note{ }
\seealso{\code{read.ascii.grid}, \code{rsaga.esri.to.sgrd}, \code{read.sgrd},
    \code{read.Rd.grid} }
% \examples{ }
\keyword{spatial}
\keyword{interface}
\keyword{file}
