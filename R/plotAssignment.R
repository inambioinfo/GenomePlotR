# col: one color or vector of colors.
plotAssignmentX <- function( x.left.bottom, x.right.bottom, x.left.top, x.right.top, ybottom, ytop, col=rgb( 0.9, 0.9, 0.9 ), border="grey", assign.style="polygon", vert.length=abs( diff( c( ybottom, ytop ) ) )/10 ){
  assign.style <- match.arg( assign.style, c( "line", "polygon" ) )
  ## argument checking...
  if( any( c( missing( x.left.bottom ), missing( x.right.bottom ), missing( x.left.top ), missing( x.right.top ), missing( ybottom ), missing( ytop ) ) ) ) stop( "x.left.bottom, x.right.bottom, x.left.top, x.right.top, ybottom, ytop are required" )
  if( diff( range( c( length( x.left.bottom ), length( x.right.bottom ), length( x.left.top ), length( x.right.top ) ) ) )!=0 ) stop( "vectors with different lengths submitted!" )
  ##
  if( length( col )!=length( x.left.bottom ) ){
    col <- rep( col[ 1 ], length( x.left.bottom ) )
  }
  if( length( border ) != length( x.left.bottom ) ){
    border <- rep( border[ 1 ], length( x.left.bottom ) )
  }
  if( assign.style=="line" ){
    for( i in 1:length( x.left.bottom ) ){
      lines( x=c( x.left.top[ i ] + ( abs( diff( c( x.right.top[ i ], x.left.top[ i ] ) ) ) ) / 2, x.left.bottom[ i ] + ( abs( diff( c( x.right.bottom[ i ], x.left.bottom[ i ] ) ) ) ) / 2 ), y=c( ytop, ybottom ), col=col[ i ] )
    }
  }
  if( assign.style=="polygon" ){
    for( i in 1:length( x.left.bottom ) )
    polygon( x=rep( c( x.left.bottom[ i ], x.left.top[ i ], x.right.top[ i ], x.right.bottom[ i ] ), each=2 ),
            y=c( ybottom, ybottom+vert.length, ytop-vert.length, ytop, ytop, ytop-vert.length, ybottom+vert.length, ybottom ),
            col=col[ i ],
            border=border[ i ]
            )
  }
}




