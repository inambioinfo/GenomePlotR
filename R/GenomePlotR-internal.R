.plotStrandArrows <-
function( start, end, y, height, nr.arrows=10, forward=TRUE,... ){
  Xheight <- height * diff( par( "usr" )[ 1:2 ] ) / diff( par( "usr" )[ 3:4 ] )
  ## consider the aspect ratio of the plot...
  Xheight <- Xheight * par( "fin" )[ 2 ] / par( "fin" )[ 1 ]
  #Xs <- seq( start+Xheight, end, length.out=nr.arrows )
  if( forward ){
    if( diff( c( start, end ) ) > Xheight ){
      Xs <- seq( start + Xheight, end, Xheight )
    }
    else{
      Xs <- seq( start, end, Xheight )
    }
    X0s <- rep( Xs-Xheight/2, 2 )
    X1s <- rep( Xs, 2 )
    Y0s <- c( rep( y+height/2, length( Xs ) ), rep( y-height/2, length( Xs ) ) )
    Y1s <- rep( y, length( Xs ) )
  }
  else{
    if( diff( c( start, end ) ) > Xheight ){
      Xs <- seq( end-Xheight, start, -Xheight )
    }
    else{
      Xs <- seq( start, end, Xheight )
    }
    X1s <- rep( Xs+Xheight/2, 2 )
    X0s <- rep( Xs, 2 )
    Y1s <- c( rep( y+height/2, length( Xs ) ), rep( y-height/2, length( Xs ) ) )
    Y0s <- rep( y, length( Xs ) )
  }
  segments( x0=X0s, x1=X1s, y0=Y0s, y1=Y1s, ... )
  #Inch <- (height/2) * par( "pin" )[2L]/diff( par( "usr" )[ 3:4 ] )
  #arrows( x0=Xs, x1=Xs+Xheight, y0=rep( y, length( Xs ) ), y1=rep( y, length( Xs ) ), length=Inch, ... )
}


## simple wrapper function for data frames. can call start also on data frames
#start <- function( x, col, ... ){
#  if( class( x )=="data.frame" ){
#    if( missing( col ) ){
#      warning( "column name for start coordinates not defined, assuming start" )
#      col <- "start"
#    }
#    return( x[ , col ] )
#  }
#  else{
#    return( start( x ) )
#  }
#}
#end <- function( x, col, ... ){
#  if( class( x )=="data.frame" ){
#    if( missing( col ) ){
#      warning( "column name for start coordinates not defined, assuming start" )
#      col <- "end"
#    }
#    return( x[ , col ] )
#  }
#  else{
#    return( end( x ) )
#  }
#}
#seqnames <- function( x, col, ... ){
#  if( class( x )=="data.frame" ){
#    if( missing( col ) ){
#      warning( "column name for start coordinates not defined, assuming start" )
#      col <- "chromosome_name"
#    }
#    return( x[ , col ] )
#  }
#  else{
#    return( seqnames( x ) )
#  }
#}
#strand <- function( x, col, ... ){
#  if( class( x )=="data.frame" ){
#    if( missing( col ) ){
#      warning( "column name for start coordinates not defined, assuming start" )
#      col <- "chromosome_strand"
#    }
#    return( x[ , col ] )
#  }
#  else{
#    return( strand( x ) )
#  }
#}




