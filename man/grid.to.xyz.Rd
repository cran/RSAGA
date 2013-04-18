\name{grid.to.xyz}
\alias{grid.to.xyz}
\title{Convert Grid Matrix to (x,y,z) data.frame}
\description{Convert a grid matrix to a (x,y,z) data.frame.}
\usage{
grid.to.xyz(data, header, varname = "z", colnames = c("x", "y", varname))
}
\arguments{
  \item{data}{grid data: either a grid data matrix, or a list with
        components \code{data} (a matrix with the grid data) and
        \code{header} (the grid header information);
        see \code{\link{read.ascii.grid}} for details }
  \item{header}{optional list giving grid header information;
        see \code{\link{read.ascii.grid}} for details}
  \item{varname}{character: name to }
  \item{colnames}{names to be given to the columns corresponding to
        the x and y coordinates and the grid variable in the output
        data.frame}
}
% \details{ }
\value{a data.frame with three columns (names are specified in the
\code{colnames} argument) giving the x and y coordinates and the attribute
values at the locations given by the grid \code{data} }
% \references{ }
\author{Alexander Brenning}
% \note{ }
\seealso{\code{\link{read.ascii.grid}}, \code{\link{pick.from.ascii.grid}} }
\examples{
\dontrun{
d = read.ascii.grid("dem")
xyz = grid.to.xyz(d,varname="elevation")
str(xyz)
}
}
\keyword{spatial}
