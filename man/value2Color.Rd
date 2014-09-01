\name{value2Color}
\alias{value2Color}
\title{
  Translate a numerical value into a color
}
\description{
Simple function to map numerical input values into colors.
}
\usage{
value2Color(x, colramp = colorRampPalette(brewer.pal(11, "RdBu"))(255), min.x, max.x, color.centered = TRUE, force.range = FALSE)
}
\arguments{
  \item{x}{
    The value(s) that should be translated into corresponding colors.
}
\item{colramp}{
  The color ramp that represents the color space corresponding to the
  numerical input space.
}
\item{min.x}{
  Smallest value to be expected as an input.
}
\item{max.x}{
  Largest value to be expected as an input.
}
\item{color.centered}{
  Whether the color bar/palette should be centered, i.e. symmetric
  around the central value.
}
\item{force.range}{
  If true, any input value that is larger than \code{max.x} or smaller
  than \code{min.x} will be set to \code{max.x} or \code{min.x} respectively.
}
}
\value{
  A character (vector) with corresponding color(s).
}
\author{
Johannes Rainer
}
\examples{
value2Color( c( 2, 3, 1, 3 ), min.x=1, max.x=3 )
}
\keyword{ color }
