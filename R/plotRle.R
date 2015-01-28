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

## that's like the boxplot.stats function... just for Rle...
rleboxplot.stats <- function( x, coef=1.5, do.conf=TRUE, do.out=FALSE ){
    quants <- quantile( x, probs=c( 0.25, 0.5, 0.75 ) )
    iqr <- quants[ 3 ] - quants[ 1 ]
    if( coef > 0 ){
        Ranges <- range( x[ x <= ( quants[ 3 ] + coef * iqr ) & x >= ( quants[ 1 ] - coef * iqr ) ] )
    }else{
        Ranges <- range( x )
    }
    n <- length( x )
    conf <- if( do.conf )
                quants[ 2 ] + c( -1.58, 1.58 ) * iqr/sqrt( n )
    out <- numeric()
    if( do.out & coef > 0) {
        ## returning all these outliers... as numeric vector!!! might be pretty large...
        out <- as.numeric( x[ x > ( quants[ 3 ] + coef * iqr ) | x < ( quants[ 1 ] - coef * iqr ) ] )
    }
    Vals <- list( stats=c( Ranges[ 1 ], quants, Ranges[ 2 ] ),
                 n=n,
                 conf=conf,
                 out=out )
    return( Vals )
}

## primitive boxplot function; works with Rle's and also numeric vectors.
rleboxplot <- function( x, range=1.5, ... ){
    ## have to make a matrix of stats...
    Vals <- rleboxplot.stats( x, coef=range )
    stats <- matrix( 0, nrow=5L, ncol=1 )
    conf <- matrix( 0, nrow=2L, ncol=1 )
    stats[ , 1 ] <- Vals$stats
    conf[ , 1 ] <- Vals$conf
    z <- list( stats=stats,
              n=Vals$n,
              conf=conf,
              out=Vals$out,
              group=numeric(0L),
              names=NA )
    bxp( z=z, ... )
}

## define a method boxplot for the Rle
setMethod( "boxplot", "Rle", function( x, range=1.5, ... ){
    rleboxplot( x, range=range, ... )
} )




