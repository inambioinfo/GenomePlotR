
colorLegend <- function( x=0, y=0, min.x, max.x, colramp=colorRampPalette( brewer.pal( 11, "RdBu" ) )( 255), nr.cols=13, height=5, width=0.8 ){
  if( nr.cols > 13 ){
    nr.cols <- 13
  }
  Color <- values2Colors( seq( from=min.x, to=max.x, length=nr.cols ) , colramp=colramp, min.x=min.x, max.x=max.x )
  rectheight <- height/nr.cols
  rect( xleft=rep( x, nr.cols ), xright=rep( x+width, nr.cols ), ybottom=seq( y, y+height-rectheight, by=rectheight ), ytop=seq( y+rectheight, y+height, by=rectheight ), col=Color, border=NA )
  text(  x=rep( x+width, 3 ), y=c( y+rectheight/2, y+height/2, y+height-rectheight/2 ), labels= format( c( min.x, mean( c( min.x, max.x ) ), max.x ), digits=2 ), pos=4 )
}


## the following function was taken from the Rppi package...
###
# translate a numerical value to a color.
# x: the value for which the corresponding color in the colorspace should be returned.
# colramp: the colorramp/colors that should be used for this. if x values are scattered around 0 a color ramp like red to blue is recommended,
# min.x: the minimal value of all x values
# max.x: the maximal x value. the function will create a colorspace based on these minimal and maximal values. if min.x is negativ the function assumes that the values are scatt
# this behaviour can be surpressed by setting color.centered=FALSE
# force.range: if values outside min.x and max.x should be set to min.x and max.x
value2Color <- function(x , colramp=colorRampPalette( brewer.pal( 11, "RdBu" ) )( 255), min.x, max.x, color.centered=TRUE, force.range=FALSE ){
  require(RColorBrewer)
  require( lattice )
  if( missing(x) | missing(min.x) | missing(max.x) ){
    stop("Arguments x, min.x and mas.x are required!\n")
  }
  if( color.centered ){
    max.x <- max( c( abs( min.x ), max.x ), na.rm=TRUE )
    min.x <- -max.x
  }
  if( force.range ){
    x[ x < min.x ] <- min.x
    x[ x > max.x ] <- max.x
  }
  return( level.colors( x, at=do.breaks( c( min.x, max.x), length( colramp ) ), col.regions=colramp ) )
}
