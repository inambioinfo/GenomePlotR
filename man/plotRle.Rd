\name{plotRle}
\Rdversion{1.1}
\alias{boxplot}
\alias{boxplot,Rle-method}
\alias{plot}
\alias{plotRle}
\alias{plot,Rle-method}
\alias{rleboxplot}
\alias{rleboxplot.stats}
\title{
  Plotting functions for Rle objects
}
\description{
  These functions provide some functionality to visualize (numeric) \code{Rle} (run length encoded) objects.
}
\usage{

  \S4method{boxplot}{Rle}( x, range=1.5, ... )

  \S4method{plot}{Rle}( x, y, ... )

  plotRle( x, add=FALSE, offset=0, ylab="values", xlab="position", ... )

  rleboxplot( x, range=1.5, ... )

  rleboxplot.stats( x, coef=1.5, do.conf=TRUE, do.out=FALSE )

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{add}{
    Whether the data should be added to an existing plot.
  }

  \item{coef}{
    see help for function \code{boxplot.stats} for details.
  }

  \item{do.conf}{
    whether confidence intervals should be calculated. See help for
    function \code{boxplot.stats} for more details.
  }

  \item{do.out}{
    whether outliers should be returned; see help for function
    \code{boxplot.stats} for more details. Setting this to \code{TRUE}
    for a large \code{Rle} object is discouraged.
  }

  \item{offset}{
    amount by which the x-position values should be shifted.
  }

  \item{range}{
    see help for function \code{boxplot} for details.
  }

  \item{x}{
    A \code{Rle} object.
  }

  \item{xlab}{
    the label for the x-axis.
  }

  \item{y}{
    Not used.
  }

  \item{ylab}{
    the label for the y-axis.
  }

  \item{\dots}{
    Additional arguments to be passed to the \code{plot} or
    \code{points} function (like \code{pch} for point type, \code{col}
    for the color).
  }

}
\section{Functions}{
  \describe{

    \item{boxplot}{
      creates a boxplot for a \code{Rle} object.
    }

    \item{plot}{
      plots a \code{Rle} object; this function calls \code{plotRle}.
    }

    \item{plotRle}{
      plots a (numeric!) \code{Rle} object. This function is much more
      efficient than the standard \code{plot} function on \code{Rle}
      objects as, instead of plotting each individual value, it plots
      just the changes (i.e. the runs).
    }

    \item{rleboxplot}{
      creates a boxplot for a \code{Rle} object; uses the
      \code{rleboxplot.stats} function to calculate the required values
      and the \code{bxp} for the actual plotting.
    }

    \item{rleboxplot.stats}{
      calculates the required statistics to generate a boxplot; works on
      both, numeric \code{Rle} objects or numeric vectors.
    }

  }
}
\details{
  These functions allows to efficiently plot a run-length encoded object.
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

