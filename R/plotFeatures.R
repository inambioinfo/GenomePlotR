if( !isGeneric( "plotFeatures" ) )
  setGeneric( "plotFeatures", function( x, ... )
  standardGeneric( "plotFeatures" ))

setMethod( "plotFeatures", "IRanges", function( x, ... ){
  plotFeatures( x=data.frame( chromosome_start=start( x ), chromosome_end=end( x ), stringsAsFactors=FALSE ), ... )
})

setMethod( "plotFeatures", "GRanges", function( x, ... ){
  ## converting the GRanges object into a data.frame, thus all additional annotation columns are also submitted
  ## to the plotFeatures function.
  plotFeatures( x=as.data.frame( x ) , ... )
})


#### The "main" plotFeatures function...
setMethod( "plotFeatures", "data.frame", function( x , strand.arrows=FALSE, rectheight=1, add=FALSE, group.by, group.line=TRUE, start.col="start", end.col="end", strand.col="strand", transform.range.x=NULL, transform.range.y=NULL, spacing=1.25, color="lightgrey", border, x.axis=TRUE, y.axis=TRUE, x.axis.side=1, y.axis.side=2, coding.color="darkgrey", coding.border, coding.color.col, coding.border.col, coding.start.col=NULL, coding.end.col=NULL, color.col=NULL, border.col=NULL, label.col=NULL, label.color="black", label.color.col, label.cex=1, original.range.x, original.range.y, highlight.color.col, highlight.border.col ){
  x0=0
  y0=0
  ## check input paramters...
  ## check for start/end coordinates...
#  if( class( x )!="data.frame" ) stop( "plotFeatures is only implemented for data.frames" )
  if( class( x )=="data.frame" ){
    if( !any( c( start.col, end.col ) %in% colnames( x ) ) ) stop( paste( "Columns", start.col, "and", end.col, "missing!" ) )
  }
  if( missing( group.by ) ){
    group.by=NULL
    coding.start.col=NULL
  }
  else{
    if( !any( colnames( x ) == group.by ) ){
      group.by=NULL
      coding.start.col=NULL
      warning( paste( "Can not group features by column ", group.by, ". Column does not exist.", sep="" ) )
    }
  }
  if( strand.arrows ){
    if( !any( colnames( x ) == strand.col ) ){
      strand.arrows=FALSE
      warning( paste( "Can not plot strand arrows, column ", strand.col, " does not exist.", sep="" ) )
    }
  }
  if( !is.null( coding.start.col ) ){
    if( !any( colnames( x )==coding.start.col ) ){
      warning( "Column ", coding.start.col, " not found in x" )
      coding.start.col=NULL
      coding.end.col=NULL
    }
  }
  if( !is.null( coding.end.col ) ){
    if( !any( colnames( x )==coding.end.col ) ){
      warning( "Column ", coding.end.col, " not found in x" )
      coding.start.col=NULL
      coding.end.col=NULL
    }
  }
  else{
    coding.start.col=NULL
  }
  #### the colors:
  ## global variables: color and border
  ## group.by specific colors defined in x: color.col and border.col
  if( missing( border ) ){
    border <- color
  }
  if( !is.null( color.col ) ){
    if( !any( colnames( x )==color.col ) ){
      warning( "Column ", color.col, " not found in x" )
      color.col=NULL
    }else{
      if( !is.null( coding.start.col ) ){
        ## unsetting this... let's see what happens...
        #warning( "color.col and plotting coding regions are mutually exclusive!" )
        #coding.start.col=NULL
      }
      ## set border.col equal to color.col, if not set.
      if( is.null( border.col ) ){
        border.col <- color.col
      }
    }
  }
  if( !is.null( border.col ) ){
    if( !any( colnames( x )==border.col ) ){
      warning( "Column ", border.col, " not found in x" )
      border.col=NULL
    }
    else{
      ## overwriting the coding border. so we will ALWAYS draw the border.
      #coding.border=NA
      #coding.border.col <- NULL
    }
  }
  ## new:
  if( !missing( coding.color.col ) ){
    if( !any( colnames( x )==coding.color.col ) ){
      warning( "Column ", coding.color.col, " not found in x" )
      coding.color.col=NULL
    }
  }else{
    coding.color.col <- NULL
  }
  if( !missing( coding.border.col ) ){
    if( !any( colnames( x )==coding.border.col ) ){
      warning( "Column ", coding.border.col, " not found in x" )
      coding.border.col=NULL
    }
  }else{
    coding.border.col <- coding.color.col
  }
  ## :wen
  if( missing( coding.border ) ){
    coding.border=coding.color
  }
  if( !missing( label.col ) ){
    if( !any( colnames( x )==label.col ) ){
      warning( "label.col ", label.col, " not found in x, omitting." )
      label.col=NULL
    }
  }
  if( !missing( label.color.col ) ){
    if( !any( colnames( x )==label.color.col ) ){
      warning( "label.color.col ", label.color.col, " not found in x, omitting." )
      label.color.col=NULL
    }
  }else{
    label.color.col <- NULL
  }
  if( !missing( highlight.color.col ) ){
    if( length( highlight.color.col )==1 ){
      ## expect to have the column name
      if( !any( colnames( x )==highlight.color.col ) ){
        warning( "highlight.color.col ", highlight.color.col, " not found in colnames( x ); omitting." )
        highlight.color.col <- NULL
      }
    }else{
      ## probably a color vector...
      if( length( highlight.color.col )==nrow( x ) ){
        ## that's ok.
        x <- cbind( x, GenomePlotR.highlight.color=highlight.color.col )
        highlight.color.col <- "GenomePlotR.highlight.color"
      }else{
        warning( "length of highlight.color.col does not match number of rows of x; omitting." )
        highlight.color.col <- NULL
      }
    }
  }else{
    highlight.color.col <- NULL
  }
  if( !missing( highlight.border.col ) ){
    if( length( highlight.border.col )==1 ){
      ## expect to have the column name
      if( !any( colnames( x )==highlight.border.col ) ){
        warning( "highlight.border.col ", highlight.border.col, " not found in colnames( x ); omitting." )
        highlight.border.col <- NULL
      }
    }else{
      ## probably a color vector...
      if( length( highlight.border.col )==nrow( x ) ){
        ## that's ok.
        x <- cbind( x, GenomePlotR.highlight.color.border=highlight.border.col )
        highlight.border.col <- "GenomePlotR.highlight.color.border"
      }else{
        warning( "length of highlight.border.col does not match number of rows of x; omitting." )
        highlight.border.col <- NULL
      }
    }
  }else{
    highlight.border.col <- NULL
  }
  ## finished input argument checking
  #################
  ## process x coordinates.
  if( !missing( original.range.x ) ){
    x.range.original <- original.range.x
  }else{
    x.range.original <- range( c( x[ , start.col ], x[ , end.col ] ) )  ## need this for the axis...
  }
  if( !is.null( transform.range.x ) ){
    x[ , start.col ] <- transformTo( x[ , start.col ], old.range=x.range.original, new.range=transform.range.x )
    x[ , end.col ] <- transformTo( x[ , end.col ], old.range=x.range.original, new.range=transform.range.x )
  }
  ## the y range...
  if( is.null( group.by ) ){
    ybottom <- rep( 0, nrow( x ) )
    ytop <- ybottom+rectheight
    ycenter <- ybottom+rectheight/2
  }
  else{
    Groups <- unique( x[ , group.by ] )
    ybottom <- seq( from=0, length.out=length( Groups ), by=rectheight*spacing )
    ytop <-ybottom+rectheight
    ycenter <- ybottom+rectheight/2
  }
  if( !is.null( transform.range.y ) ){
    if( !missing( original.range.y ) ){
      old.range <- original.range.y
    }else{
      old.range <- range( c( ybottom, ytop ) )
    }
    ybottom <- transformTo( ybottom, new.range=transform.range.y, old.range=old.range )
    ytop <- transformTo( ytop, new.range=transform.range.y, old.range=old.range )
    ycenter <- transformTo( ycenter, new.range=transform.range.y, old.range=old.range )
  }

  #cat( "xcoords range:", range( x[ , start.col ] ), " y coords: ", range( ybottom ), "\n" )
  #cat( "ycoords ", ybottom, "\n" )
  ## plot
  if( !add ){
    ## have to make a new plot...
    plot( 3, 3, pch=NA, xlab="", ylab="", xaxt="n", yaxt="n", ylim=range( c( ybottom, ytop )+y0 ) , xlim=range( c( x[ , start.col ], x[ , end.col ] )+x0 ), bty="n" )
  }
  if( is.null( group.by ) ){
    rectheight <- diff( range( ybottom ) ) / spacing
    ## plot all in one line!
    Color <- color    
    if( !is.null( color.col ) ){ Color <- x[ , color.col ] }
    Border <- border
    if( !is.null( border.col ) ){ Border <- x[ , border.col ] }
    rect( ybottom=ybottom+y0, ytop=ytop+y0, xleft=x[ , start.col ]+x0, xright=x[ , end.col ]+x0, col=Color, border=Border )
    if( !is.null( label.col ) ){
      text( x=x[ , start.col ] + ( x[ , end.col ]-x[ , end.col ] ) / 2, y=ycenter, labels=x[ , label.col ], col=label.color, cex=label.cex, adj=c( 0.5, 0.5 ) )     
    }
  }
  else{
    #rectheight <- diff( range( ybottom ) ) / length( Groups ) / spacing
    ## loop through Groups
    for( i in 1:length( Groups ) ){
      rectheight <- ytop[ i ] - ybottom[ i ]
      ## subset and order by start position...
      x.sub <- x[ x[ , group.by ]==Groups[ i ], ]
      x.sub <- x.sub[ order( x.sub[ , start.col ] ), ]
      x.left <- x.sub[ , start.col ]+x0
      x.right <- x.sub[ , end.col ]+x0
      Color <- color
      if( !is.null( color.col ) ){ Color <- x.sub[ , color.col ] }
      Border <- border
      if( !is.null( border.col ) ){ Border <- x.sub[ , border.col ] }
      if( group.line ) lines( x=range( c( x.left, x.right ) ), y=rep( ycenter[ i ], 2 )+y0, col=Color )
      ## the arrows specifying the strand
      if( strand.arrows ){
        .plotStrandArrows( min( x.left ), max( x.right ), y=ycenter[ i ] + y0, height=rectheight, col=Color, forward=strand2Num( x.sub[ 1, strand.col ] ) > 0 )
      }
      rect( ybottom=ybottom[ i ]+y0, ytop=ytop[ i ] + y0, xleft=x.left, xright=x.right, col=Color, border=Border )
      ## coding region:
      if( !is.null( coding.start.col ) ){
        coding.start <- x.sub[ , coding.start.col ][ 1 ]
        coding.end <- x.sub[ , coding.end.col ][ 1 ]
        if( !is.na( coding.start ) & !is.na( coding.end ) ){
          ## transform it...
          if( !is.null( transform.range.x ) ){
            coding.start <- transformTo( coding.start, old.range=x.range.original, new.range=transform.range.x )
            coding.end <- transformTo( coding.end, old.range=x.range.original, new.range=transform.range.x )
            ## make shure that coding.start and coding.end are not outside of the range of start end...
            coding.start <- max( c( coding.start, min( x.sub[ , start.col ] ) ) )
            coding.end <- min( c( coding.end, max( x.sub[ , end.col ] ) ) )
          }
          x.left.coding <- unique( sort( c( coding.start, x.left[ x.left >= coding.start & x.left <= coding.end ] ) ) )
          #cat( x.left, "\n" )
          x.right.coding <- unique( sort( c( x.right[ x.right >= coding.start & x.right <= coding.end ], coding.end ) ) )
          #cat( x.right, "\n" )
          if( !is.null( coding.color.col ) ){
            the.coding.color <- x.sub[ 1, coding.color.col ]
          }else{
            the.coding.color <- coding.color
          }
          if( !is.null( coding.border.col ) ){
            the.border.color <- x.sub[ 1, coding.border.col ]
          }else{
            the.border.color <- coding.border
          }          
          rect( xleft=x.left.coding, xright=x.right.coding, ybottom=ybottom[ i ], ytop=ytop[ i ], col=the.coding.color, border=the.border.color )
        }
      }
      ## any exon highlighting???
      if( !is.null( highlight.color.col ) ){
        ## check if there are any rectangles to draw at all...
        x.sub[ is.null( x.sub[ , highlight.color.col ] ), highlight.color.col ] <- NA
        do.highlight <- !is.na( x.sub[ , highlight.color.col ] )
        if( any( do.highlight ) ){
          rect( xleft=x.left[ do.highlight ], xright=x.right[ do.highlight ], ybottom=ybottom[ i ], ytop=ytop[ i ], col=x.sub[ do.highlight, highlight.color.col ] )
        }
      }
      ## any exon highlighting// border???
      if( !is.null( highlight.border.col ) ){
        ## check if there are any rectangles to draw at all...
        x.sub[ is.null( x.sub[ , highlight.border.col ] ), highlight.border.col ] <- NA
        do.highlight <- !is.na( x.sub[ , highlight.border.col ] )
        if( any( do.highlight ) ){
          rect( xleft=x.left[ do.highlight ], xright=x.right[ do.highlight ], ybottom=ybottom[ i ], ytop=ytop[ i ], border=x.sub[ do.highlight, highlight.border.col ] )
        }
      }      
      ## adding a label... if needed.
      if( !is.null( label.col ) ){
        the.label.color <- label.color
        if( !is.null( label.color.col ) ){
          the.label.color <- x.sub[ 1, label.color.col ]
        }
        text( x=min( x.left )+( max( x.right )-min( x.left ) )/2, y=ycenter[ i ], labels=x.sub[ 1, label.col ], col=the.label.color, cex=label.cex, adj=c( 0.5, 0.5 ) )
      }
    }
    if( y.axis ){
      axis( side=y.axis.side, at=ycenter, label=Groups, las=2, col=NA )
    }
  }
  ## x axis...
  if( x.axis ){
    At <- pretty( range( c( x[ , start.col ], x[ , end.col ] ) ) )
    if( !is.null( transform.range.x ) ){
      Labels <- round ( transformTo( At, old.range=range( c( x[ , start.col ], x[ , end.col ] ) ), new.range=x.range.original ) )
    }
    else{
      Labels <- At
    }
    axis( side=x.axis.side, at=At, label=Labels )
  }
}
)

