\name{strand2Num}
\alias{strand2Num}
\title{
Encode strand information as a number.
}
\description{
Transforms character description of the genomic strand into a number (+1 for forward strand, -1 for reverse strand).
}
\usage{
strand2Num(x)
}
\arguments{
  \item{x}{
Either a number +1 or -1, or a character string +, - +1, -1.
}
}
\value{
+1 for forward strand, -1 for reverse strand.
}
\author{
Johannes Rainer
}
\examples{
strand2Num( "+" )
strand2Num( "-1" )
strand2Num( 1 )
}
\keyword{ data }
