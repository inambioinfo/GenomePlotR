\name{transformTo}
\alias{transformTo}
\title{
Transform coordinate system.
}
\description{
Transforms (linear) coordinates from one coordinate system (defined by its range) into an other coordinate system (defined by its range).
}
\usage{
transformTo(x, old.range = range(x), new.range)
}
\arguments{
  \item{x}{
Coordinates in the old coordinate system that should be transformed.
}
  \item{old.range}{
The range (minimal and maximal value) of the old coordinate system.
}
  \item{new.range}{
The range of the coordinate system into which the values in \code{x} should be transformed.
}
}
\value{
The values in \code{x} transformed into the new coordinate system.
}
\author{
Johannes Rainer
}

\seealso{
\code{\link{plotFeatures}}
}
\examples{
# transform x into a coordinate system ranging from 10 to 100.
# in this simple exampl the 1 is mapped to 10 and 8 to 100
x <- c( 1, 4, 2, 8 )
transformTo( x, new.range=c( 10, 100 ) )

y <- c( 1, 5, 10, 4 )

x-y
transformTo( x, old.range=range( c( x, y ) ), new.range=c( 10, 100 ) )

transformTo( x, old.range=range( c( x, y ) ), new.range=c( 10, 100 ) ) - transformTo( y, old.range=range( c( x, y ) ), new.range=c( 10, 100 ) )

}
\keyword{ data }
