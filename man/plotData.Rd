\name{plotData}
\alias{plotData}
\title{
Plotting data
}
\description{
This function is in principal identical to the \code{plot} and \code{points}, i.e. it either plots data points or adds them to an existing plot. Besides that the function also allows to transform/rescale the values and the coordinate system of the data in order to be able to plot e.g. data points with a range of values larger than available in an existing plot. X and y axis, if plotted, correspond always to the original data range of the submitted values.
}
\usage{
plotData(x, y, add = FALSE, x.axis = !add, y.axis = !add, x.axis.side = 1, y.axis.side = 2, transform.range.x = NULL, original.range.x = range(x), original.range.y = range(y), transform.range.y = NULL, ylim, xlim, xlab = "x", ylab = "y", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numerical vector with the values to plot. If not submitted a sequence from 1 to the length of \code{y} is used instead.
}
  \item{y}{
Numerical vector with the y-coordinates of the points.
}
  \item{add}{
If the data should be added to an existing plot.
}
  \item{x.axis}{
If a x-axis should be drawn (displaying the original, and not the transformed, data range).
}
  \item{y.axis}{
If a y-axis should be drawn (displaying the original, and not the transformed, data range).
}
  \item{x.axis.side}{
The side where the x axis should be added to the plot (1 bottom or 3 top).
}
  \item{y.axis.side}{
The side where the y axis should be added to the plot (2 left or 4 right side).
}
  \item{transform.range.x}{
Transform the x values that they fit into this new data range. Note: to add more then one set of data points the \code{original.range.x} argument should also be used (see the notes section as well as the examples below).
}
  \item{original.range.x}{
The original data range of the values from argument \code{x}.
}
  \item{original.range.y}{
The original data range of the values from argument \code{y}.
}
  \item{transform.range.y}{
Transform the y values that they fit into this new data range. Note: to add more then one set of data points to an existing plot, the \code{original.range.x} argument should also be used (see the notes and warnings section as well as the examples below).
}
  \item{ylim}{
Optionally define the limits on the y-axis.
}
  \item{xlim}{
Optionally define the limits on the x-axis.
}
  \item{xlab}{
Label for the x-axis. Defaults to x.
}
  \item{ylab}{
Label for the y-axis. Defaults to y.
}
  \item{\dots}{
Additional arguments to be passed to the \code{plot} or \code{points} function (like \code{pch} for point type, \code{col} for the color...).
}
}
\details{
This function allows to add data points to an existing plot, also if the xlim and ylim of the plot would not allow to display the data.
}
\value{
A plot.
}
\author{
Johannes Rainer
}
\note{
If more then one set of data points is added to a plot and the \code{transform.range.x} and \code{transform.range.y} arguments are used it is important to use also the \code{original.range.x} and \code{original.range.y} arguments to define the range of all data points that will be plotted, otherwise the individual sets of points are each scaled to the new range. See the examples section below for the correct and wrong usage of this function.
}

\seealso{
\code{\link{transformTo}}, \code{\link{plotFeatures}}
}
\examples{

## generate some random data to be plotted.
set.seed( 18011977 )
Y <- rnorm( 5 )
Y2 <- rnorm( 5 )

Y
Y2
## assume we want to plot this data into a plot with a range from 5-20 on the
##x and 2-10 on the y axis:
plotData( x=1:5, y=Y, transform.range.x=c( 5, 20 ), transform.range.y=c( 2, 10 ), original.range.y=range( c( Y, Y2 ) ) )
plotData( x=1:5, y=Y2, transform.range.x=c( 5, 20 ), transform.range.y=c( 2, 10 ), original.range.y=range( c( Y, Y2 ) ), add=TRUE, col="green" )
points( 20, 10, pch=15 )

## The code below should never be used to add data from more then one data vector
##into an existing plot using the transform.range.x and transform.range.y
## the function will always assign the smalles value in y to 2 and the largest
##to 10, unless we specifiy the range of all values we want to add to the plot
##by the argument original.range
plotData( x=1:5, y=Y, transform.range.x=c( 5, 20 ), transform.range.y=c( 2, 10 ) )
plotData( x=1:5, y=Y2, transform.range.x=c( 5, 20 ), transform.range.y=c( 2, 10 ), add=TRUE, col="red" )

## add data to an existing plot...
plot( 3, 3, pch=15, xlim=c( 2, 4 ), ylim=c( 2, 4 ) )
plotData( x=1:5, y=Y, transform.range.x=c( 3, 4 ), transform.range.y=c( 2.5, 4 ), add=TRUE, original.range.y=range( c( Y, Y2 ) ), y.axis=TRUE, y.axis.side=4 )
plotData( x=1:5, y=Y2, transform.range.x=c( 3, 4 ), transform.range.y=c( 2.5, 4 ), add=TRUE, original.range.y=range( c( Y, Y2 ) ), col="green" )


}
\keyword{ data }
