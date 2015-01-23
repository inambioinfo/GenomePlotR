## this might be more efficient function than the default plot function on an Rle object...
## x: the Rle
## add: add to an existing plot?
## offset: shift the start position by the specified number.
plotRle <- function( x, add=FALSE, offset=0, ylab="values", xlab="position", ... ){
    ## instead of calling plot on the Rle we want to make use of the way how Rle's are encoded:
    ## hint from Herve Pages.
    position <- integer( 2 * length( start( x ) ) )
    idx <- 2L * seq_along( start( x ) )
    position[ idx ] <- end( x ) + offset
    position[ idx - 1L ] <- start( x ) + offset
    ## position <- as.numeric( unlist( strsplit( paste( start( x ), end( x ), sep=":" ), split=":" ) ) ) + offset
    if( add ){
        points( x=position, y=rep( x@values, each=2 ), ... )
    }else{
        plot( x=position, y=rep( x@values, each=2 ), xlab=xlab, ylab=ylab, ... )
    }
}

if( !isGeneric( "plot", ) )
    setGeneric( "plot", function( x, y, ... )
               standardGeneric( "plot" ))

setMethod( "plot", "Rle", function( x, y, ... ){
    plotRle( x, ... )
} )
