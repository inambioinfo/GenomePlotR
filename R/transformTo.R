transformTo <-
function( x, old.range=range( x ), new.range ){
  if( missing( new.range ) ) return( x )
  if( length( new.range )!=2 ) stop( "new.range has to be a numeric vector of length 2!" )
  #mult.fact <- diff( new.range ) / diff( old.range )
  return( ( ( x-old.range[ 1 ] ) * (new.range[ 2 ]-new.range[ 1 ]) / (old.range[ 2 ] - old.range[ 1 ]) ) + new.range[ 1 ])
}

