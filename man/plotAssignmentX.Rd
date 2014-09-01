\name{plotAssignmentX}
\alias{plotAssignmentX}
\title{
Plot assignment lines
}
\description{
Plot lines or polygons mapping between regions on the x axis.
}
\usage{
plotAssignmentX(x.left.bottom, x.right.bottom, x.left.top, x.right.top, ybottom, ytop, col = rgb(0.9, 0.9, 0.9), border = "grey", assign.style = "polygon", vert.length = abs(diff(c(ybottom, ytop)))/10)
}
\arguments{
  \item{x.left.bottom}{
The bottom left coordinates of the regions.
}
  \item{x.right.bottom}{
The bottom right coordinates of the regions.
}
  \item{x.left.top}{
The top left coordinates of the regions.
}
  \item{x.right.top}{
The top right coordinates of the regions.
}
  \item{ybottom}{
The position of the bottom regions on the y-axis.
}
  \item{ytop}{
The position of the upper/top regions on the y-axis.
}
  \item{col}{
The color (background color for \code{assign.style=polygon})
}
  \item{border}{
The border color.
}
  \item{assign.style}{
The style of assignments. One of \code{line} (drawing lines) or \code{polygon} (plot assignment using polygons).
}
  \item{vert.length}{
A factor specifying how long the vertical part of the polygons should be.
}
}
\author{
Johannes Rainer
}
\seealso{
\code{\link{plotFeatures}}
}
\examples{

b.l <- c( 1, 2, 3, 4 )
b.r <- c( 2, 3, 4, 5 )
t.l <- c( 2, 5, 6, 8 )
t.r <- c( 4, 5.5, 7, 10 )
plot( 3, 3, pch=NA, xlim=c( 0, 12 ), ylim=c( 0, 5 ) )
rect( xleft=b.l, xright=b.r, ybottom=0.5, ytop=1 )
rect( xleft=t.l, xright=t.r, ybottom=4, ytop=4.5 )
plotAssignmentX( x.left.b=b.l, x.right.b=b.r, x.left.t=t.l, x.right.t=t.r, ybottom=1, ytop=4, assign.style="line" )
plotAssignmentX( x.left.b=b.l, x.right.b=b.r, x.left.t=t.l, x.right.t=t.r, ybottom=1, ytop=4 )

plotAssignmentX( x.left.b=b.l, x.right.b=b.r, x.left.t=t.l, x.right.t=t.r, ybottom=1, ytop=4, col=c( "lightgrey", "green", "red", "lightgrey" ) )


}
\keyword{ data }
