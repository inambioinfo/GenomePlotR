\name{GenomePlotR-package}
\alias{GenomePlotR-package}
\alias{GenomePlotR}
\docType{package}
\title{
Plotting data in genomic context
}
\description{
Function to plot transcripts, genes or other features together with data (e.g. microarray probe data or sequencing data) on genomic coordinates.
}
\details{
\tabular{ll}{
Package: \tab GenomePlotR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-03-20\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
}
}
\author{
Johannes Rainer

Maintainer: Johannes Rainer <johannes.rainer@i-med.ac.at>
}
\references{
http://bioinfo.i-med.ac.at
}
\examples{
## plotting exons of a transcript and random values for each exon that could represent
## the expression of the individual exons.
data( Transc )
colnames( Transc )

## subsetting to one transcript
Transc.sub <- Transc[ Transc$transcript_id=="ENST00000399265", ]
## order according to chromosomal start position
Transc.sub <- Transc.sub[ order( Transc.sub$exon_chrom_start ), ]

## create an empty plot with the desired dimensions
plot( 3, 3, pch=NA, xlim=c( 1, 10 ), ylim=c( 1, 10 ) , xaxt="n", yaxt="n", bty="n", xlab="", ylab="" )

## plot the features inside the plot in the upper area, from 9 to 10
plotFeatures( Transc.sub, start.col="exon_chrom_start", end.col="exon_chrom_end", add=TRUE, strand.arrows=TRUE, group.by="transcript_id", strand.col="transcript_chrom_strand", transform.range.x=c( 1, 10 ), transform.range.y=c( 9, 10 ), x.axis=TRUE, x.axis.side=3 )

## now plot the data into the range on the y-axis from 1 to 6 and in equally spaced
## locations on the x-axis.
Data <- rnorm( length( unique( Transc.sub[ , "exon_id" ] ) ) )
Data2 <- rnorm( length( unique( Transc.sub[ , "exon_id" ] ) ) )
## plotting the data
plotData( x=1:length( Data )+0.5, y=Data, transform.range.x=c( 1, 10 ), transform.range.y=c( 1, 6 ), original.range.y=range( c( Data, Data2 ) ), original.range.x=c( 1, length( Data )+1 ), pch=16, col="green", add=TRUE, y.axis=TRUE, y.axis.side=2 )

## adding the second set of values
plotData( x=(1:length( Data ))+0.5, y=Data2, transform.range.x=c( 1, 10 ), transform.range.y=c( 1, 6 ), original.range.y=range( c( Data, Data2 ) ), original.range.x=c( 1, length( Data )+1 ), pch=16, col="blue", add=TRUE )

## now use polygons to map exons to the equidistant data points.
plotAssignmentX( x.left.bottom=transformTo( x=1:length( Data ), old.range=c( 1, length( Data )+1 ), new.range=c( 1, 10 ) ), x.right.bottom=transformTo( x=(1:length( Data ))+1, old.range=c( 1, length( Data )+1 ), new.range=c( 1, 10 ) ), x.left.top=transformTo( Transc.sub[ , "exon_chrom_start" ], old.range=range( Transc.sub[ , c( "exon_chrom_start", "exon_chrom_end" ) ] ), new.range=c( 1, 10 ) ), x.right.top=transformTo( Transc.sub[ , "exon_chrom_end" ], old.range=range( Transc.sub[ , c( "exon_chrom_start", "exon_chrom_end" ) ] ), new.range=c( 1, 10 ) ), ybottom=6.2, ytop=8.8 )

########################################
#### an alternative, easyer way would be:
plot( Data, xlim=c( 0.5, length( Data )+0.5 ), ylim=c( min( c( Data, Data2 ) ), 6 ), bty="n", xaxt="n", yaxt="n", xlab="", ylab="", pch=16, col="green" )
points( Data2, pch=16, col="blue" )
axis( at=pretty( range( c( Data, Data2 ) ) ), side=2 )

## plot the transcript into the plot this time using the xlim of the exising plot.
plotFeatures( Transc.sub, start.col="exon_chrom_start", end.col="exon_chrom_end", add=TRUE, strand.arrows=TRUE, group.by="transcript_id", strand.col="transcript_chrom_strand", transform.range.x=c( 1, length( Data ) ), transform.range.y=c( 5, 6 ), x.axis=TRUE, x.axis.side=3 )

## now use polygons to map exons to the equidistant data points.
plotAssignmentX( x.left.bottom=seq( 0.5, length.out=length( Data ) ), x.right.bottom=seq( 1.5, length.out=length( Data ) ), x.left.top=transformTo( Transc.sub[ , "exon_chrom_start" ], old.range=range( Transc.sub[ , c( "exon_chrom_start", "exon_chrom_end" ) ] ), new.range=c( 1, length( Data ) ) ), x.right.top=transformTo( Transc.sub[ , "exon_chrom_end" ], old.range=range( Transc.sub[ , c( "exon_chrom_start", "exon_chrom_end" ) ] ), new.range=c( 1, length( Data ) ) ), ybottom=max( c( Data, Data2 )+0.1 ), ytop=4.9 )


}
\keyword{ package }
