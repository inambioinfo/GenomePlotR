## !!! if plotData with transform.range.x and transform.range.y is called several times to add data points to the same plot, the range of all values in the original data set has to be specified by oringinal.range.x and original.range.y!!! 
plotData <- function( x, y, add=FALSE, x.axis=!add, y.axis=!add, x.axis.side=1, y.axis.side=2, transform.range.x=NULL, original.range.x=range( x ), original.range.y=range( y ), transform.range.y=NULL, ylim, xlim, xlab="x", ylab="y", ... ){
  # input parameter checking
  if( class( x )=="Rle" ){
    ## calling a different function...
  }
  if( missing( x ) & missing( y ) ){
    stop( "x and/or y have to be submitted!" )
  }
  if( missing( x ) ){
    x <- seq( 1, length( y ) )
  }
  ## finished
#  x.range.original <- range( x )
#  y.range.original <- range( y )
  if( !is.null( transform.range.x ) ){
    x.new <- transformTo( x, old.range=original.range.x, new.range=transform.range.x )
    if( missing( xlim ) )
      xlim <- transform.range.x
  }
  else{
    x.new <- x
    if( missing( xlim ) )
      xlim <- range( x.new )
  }
  if( !is.null( transform.range.y ) ){
    #cat( "original: ", y, "\n" )
    y.new <- transformTo( y, old.range=original.range.y, new.range=transform.range.y )
    #cat( "transformed: ", y.new, "\n" )
    if( missing( ylim ) )
      ylim <- transform.range.y
  }
  else{
    y.new <- y
    if( missing( ylim ) )
      ylim <- range( y.new )
  }
  if( !add ){
    #cat( "xlim", xlim, "ylim", ylim, "\n" )
    ## using plot...
    plot( x.new, y.new, xaxt="n", yaxt="n", ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, ... )
  }
  else{
    ## just points
    points( x.new, y.new, ... )
  }
  if( x.axis ){
#    At <- pretty( range( x.new ) )  # are already transformed (if they were transformed...)...
    if( !is.null( transform.range.x ) ){  # have to calculate the corresponding labels in the original range
      At <- pretty( range( transform.range.x ) )
      Labels <- format( transformTo( At, new.range=original.range.x, old.range=transform.range.x ), digits=2 )
    }
    else{
      At <- pretty( range( original.range.x ) )
      Labels <- At
    }
    axis( side=x.axis.side, at=At, label=Labels )
  }
  if( y.axis ){
    if( !is.null( transform.range.y ) ){  # have to calculate the corresponding labels in the original range
      Labels <- pretty( original.range.y )
      Labels <- Labels[ Labels >= min( original.range.y ) & Labels <= max( original.range.y ) ]
      At <- transformTo( Labels, new.range=transform.range.y, old.range=original.range.y )
      labpos <- mean( transform.range.y )
#      At <- pretty( range( transform.range.y ) )
#      Labels <- format( transformTo( At, new.range=original.range.y, old.range=transform.range.y ), digits=2 )
    }
    else{
      At <- pretty( range( original.range.y ) )
      Labels <- At
      labpos <- mean( original.range.y )
    }
    axis( side=y.axis.side, at=At, label=Labels )
    mtext( side=2, at=labpos, text=ylab, line=2, adj=0.5, cex=par( "cex.lab" ) )
    #mtext( side=y.axis.side, line=2, cex=par( "cex.lab" ), text=ylab )
  }
}


