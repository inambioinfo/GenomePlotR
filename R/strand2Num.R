strand2Num <-
function( x ){
  x <- paste( x, "1", sep="" )
  x <- as.numeric( x )
  if( x < 0 ){
    return( -1 )
  }
  else{
    return( 1 )
  }
}

