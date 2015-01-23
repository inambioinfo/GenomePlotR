\name{plotRle}
\alias{plotRle}
\alias{plot}
\alias{plot,Rle-method}
\title{
  Plot a Rle object
}
\description{
  This function allows to plot a \code{Rle} (run length encoded) object
  more efficiently than the default \code{plot} function.
}
\usage{
  plotRle( x, add=TRUE, offset=0, ylab="values", xlab="position", ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A \code{Rle} object.
  }
  \item{add}{
    Whether the data should be added to an existing plot.
  }
  \item{offset}{
    amount by which the x-position values should be shiftet.
  }
  \item{ylab}{
    the label for the y-axis.
  }
  \item{xlab}{
    the label for the x-axis.
  }
  \item{\dots}{
    Additional arguments to be passed to the \code{plot} or \code{points} function (like \code{pch} for point type, \code{col} for the color...).
  }
}
\details{
  This function allows to efficiently plot a run-length encoded object.
}
\value{
  A plot.
}
\author{
Johannes Rainer
}
\seealso{
  \code{\link{transformTo}}, \code{\link{plotFeatures}}
}
\keyword{ data }

