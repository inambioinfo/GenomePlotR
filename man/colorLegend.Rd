\name{colorLegend}
\alias{colorLegend}
\title{
  Simple color legend
}
\description{
Draws a simple color legend/bar.
}
\usage{
colorLegend(x = 0, y = 0, min.x, max.x, colramp = colorRampPalette(brewer.pal(11, "RdBu"))(255), nr.cols = 13, height = 5, width = 0.8)
}
\arguments{
  \item{x}{
    x position on the plot where to place the bar.
}
\item{y}{
  y position on the plot.
}
\item{min.x}{
  smallest value to be drawn on the bar.
}
\item{max.x}{
  largest value.
}
\item{colramp}{
  the color ramp.
}
\item{nr.cols}{
  number of colors to be drawn.
}
\item{height}{
  the height of the color bar.
}
\item{width}{
  the width of the color bar
}
}
\value{
  Draws a color legend/bar.
}
\author{
  Johannes Rainer
}
\keyword{ color }
