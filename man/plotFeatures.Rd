\name{plotFeatures}
\alias{plotFeatures}
\alias{plotFeatures,data.frame-method}
\alias{plotFeatures,IRanges-method}
\alias{plotFeatures,GRanges-method}
\title{
Plot genomic features
}
\description{
Plots transcripts genes or other features with start, end coordinates.
}
\usage{
plotFeatures( x, ... )
\S4method{plotFeatures}{data.frame}(x, strand.arrows = FALSE, rectheight = 1, add = FALSE, group.by, group.line = TRUE, start.col = "start", end.col = "end", strand.col = "strand", transform.range.x = NULL, transform.range.y = NULL, spacing = 1.25, color = "lightgrey", border, x.axis = TRUE, y.axis = TRUE, x.axis.side = 1, y.axis.side = 2, coding.color = "darkgrey", coding.border, coding.color.col, coding.border.col, coding.start.col = NULL, coding.end.col = NULL, color.col=NULL, border.col=NULL, label.col=NULL, label.color="black", label.color.col, label.cex=1, original.range.x, original.range.y, highlight.color.col, highlight.border.col )
}
\arguments{
  \item{x}{
data.frame with the features to be plotted. Requires columns specifying the start and end positions of the individual features. For each row a rectangle representing the features is plotted. Alternatively also a IRanges or GRanges object with start and end coordinates can be submitted. \code{GRanges} objects are internally converted into a \code{data.frame} using the \code{as.data.frame} function from the \code{IRanges} package, thus, all additional data columns in the submitted \code{GRanges} object are passed on to the plotting function. This enables to use all parameters for the plotFeatures function defined for \code{data.frames}.
}
  \item{...}{additional arguments to be passed on to the plotFeatures function for data.frames.}
  \item{strand.arrows}{
If the strand should be indicated by arrows pointing form 5' to 3' end. Requires that \code{x} contains a column specifying the strand of the feature (+, -, +1 or -1).
}
  \item{rectheight}{
The height of the feature rectangles in the units of the y axis.
}
  \item{add}{
If the features should be added to an existing plot.
}
  \item{group.by}{
Column name in \code{x} allowing to group rows/features, e.g. if \code{group.by} is the name of a column in \code{x} with transcript ids and the individuals rows correspond to exons, then each transcript is drawn in one line.
}
  \item{group.line}{
If grouped features should be connected by a line.
}
  \item{start.col}{
Column name in \code{x} containing the start coordinates.
}
  \item{end.col}{
Column name in \code{x} containing the end coordinates.
}
  \item{strand.col}{
Column with strand information. Only used if \code{strand.arrows=TRUE}.
}
  \item{transform.range.x}{
Transform x-coordinates (start end positions) in \code{x} into the new coordinate system defined by this new range.
}
  \item{transform.range.y}{
Transform y-coordinates (position on the y axis where the features are drawn) into the new coordinate system defined by this new range.
}
  \item{spacing}{
Spacing between features on different lines on the y-axis.
}
  \item{color}{
Color to fill the feature rectangles.
}
  \item{border}{
Color for the border of the rectangles (or NA to omit drawing rectangle borders).
}
  \item{x.axis}{
If a x-axis should be drawn.
}
  \item{y.axis}{
If a y-axis should be drawn.
}
  \item{x.axis.side}{
Side where the x-side should be drawn (1 (bottom) or 3 (top)).
}
  \item{y.axis.side}{
Side where the x-side should be drawn (2 (left) or 4 (right)).
}
  \item{coding.color}{
Color to be used for coding region (only in combination with group.by and coding.start.col, coding.end.col).
}
\item{coding.border}{
How the border of the rectangles for the coding regions should be drawn (either a color or NA to not draw them, defaults to coding.color).
}
\item{coding.color.col}{
Column in \code{x} specifying the color(s) to fill the coding region for each transcript (or entity grouped by group.by); overwrites \code{coding.color} parameter).
}
\item{coding.border.col}{
Column in \code{x} specifying the border color(s) for the coding region of each transcript (or entity grouped by group.by); overwrites \code{coding.border} parameter).
}
  \item{coding.start.col}{
Column in \code{x} containing the start coordinate of the coding region.
}
  \item{coding.end.col}{
Column in \code{x} containing the end coordinate of the coding region.
}
\item{color.col}{
Column in \code{x} specifying the color for the individual feature rectangles; overwrites \code{color} parameter.
}
\item{border.col}{
Column in \code{x} specifying the color for the border of the individual feature rectangles; overwrites \code{border} parameter.
}
\item{label.col}{
Name of a column in x that contains text labels that should be drawn into the plot (into the feature).
}
\item{label.color}{
The color in which the label should be displayed.
}
\item{label.color.col}{
Column in \code{x} specifying the color for the text labels; overwrites \code{label.color} parameter.
}

\item{label.cex}{
Cex for the size of the label text.
}
  \item{original.range.x}{
The original range in x-direction of the submitted data (e.g. range of the chromosomal coordinates). See details for more information.
}
  \item{original.range.y}{
The original range of values in y-direction.
}
  \item{highlight.color.col}{
The name of the column in \code{x} specifying for some rows/features a specific highlight color, or a vector of colors (same length as rows in \code{x}). A color should only be defined for the feature to be highlighted, \code{NA} should be defined for all other features. See details section and examples below.
}
  \item{highlight.border.col}{
Same as \code{highlight.color.col}, but defining the color to be used for the rectangle border.
}
}
\details{
The function can draw simple features on a chromosomal coordinate system defined by their start and end position, or more complex features like intron-exon structures of transcripts including different coloring for coding region or UTR. The arguments \code{transform.range.x} and \code{transform.range.y} are especially useful if features should be plotted into specific regions of and existing plot (see examples). If not explicitely defined, \code{original.range.x} and \code{original.range.y} are estimated on the submitted data/coordinates. Sometimes, e.g. if \code{plotFeatures} is used to add several features to an existing plot, is adviseable to either don't transform coordinates (e.g. if on the same chromosomal scale keep the x-coordinates untransformed), or use the same original range. Also setting \code{xaxs="i"} and/or \code{yaxs="i"} is sometimes useful, as it forces the plotting region to extend to the extreme values in the data, or the specified \code{xlim} and/or \code{ylim}. See help of the \code{par} function for more information.
If no specific border color is specified, the color used to fill the rectangles is used, i.e. the color defined by \code{color} is used if \code{border} is not set, or the colors specified in column \code{color.col} if \code{border.col} is not set.

Note: if this function is used to plot the genomic representation of a transcript including the coloring of the coding region and individual exons should be highlighted, the \code{highlight.color.col} and/or \code{highlight.border.col} parameters should be used. This seems a little cumbersome, but the highlighted exon has to be added to the plot after the coding region has been drawn.
}
\value{
Creates a plot or adds features to an existing plot.
}
\author{
Johannes Rainer
}

\seealso{
\code{\link{transformTo}}, \code{\link{plotAssignmentX}}
}
\examples{
# load a data frame with some data.
data( Transc )

colnames( Transc )

plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end" )

plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end" )

## plot the transcript ids into the plot
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", label.col="transcript_id", y.axis=FALSE )

plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE )

## draw transcripts starting with ENST in green, with TRAN in red.
Transc <- cbind( Transc, color.fill=brewer.pal( 3, "Paired" )[ 3 ], stringsAsFactors=FALSE )
Transc[ grep( Transc$transcript_id, pattern="^TRAN" ), "color.fill" ] <- brewer.pal( 5, "Paired" )[ 5 ]
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE, color.col="color.fill" )

## we might however not like to have the coding region drawn in grey. we could either change the coding.color parameter, or specify the color of the coding region for each transcript:
Transc <- cbind( Transc, coding.color=brewer.pal( 4, "Paired" )[ 4 ], stringsAsFactors=FALSE )
Transc[ grep( Transc$transcript_id, pattern="^TRAN" ), "coding.color" ] <- brewer.pal( 6, "Paired" )[ 6 ]
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE, color.col="color.fill", coding.color.col="coding.color" )

## we could draw transcript with the ID ENST00000399272 in blue (light blue for non-coding, blue for coding region)
Transc[ Transc$transcript_id=="ENST00000399272", "color.fill" ] <- brewer.pal( 3, "Paired" )[ 1 ]
Transc[ Transc$transcript_id=="ENST00000399272", "coding.color" ] <- brewer.pal( 3, "Paired" )[ 2 ]
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE, color.col="color.fill", coding.color.col="coding.color" )

## highlighting a specific exon is a little more complicated if we draw a coding region (otherwise we could just change the color for the exon in the original data.frame Transc).
Transc <- cbind( Transc, highlight=NA )
Transc[ Transc$exon_id=="ENSE00001043968", "highlight" ] <- "magenta"
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE, color.col="color.fill", coding.color.col="coding.color", highlight.border.col="highlight" )

## placing the labels again into the plot, but using matching colors.
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", strand.col="transcript_chrom_strand", strand.arrows=TRUE, color.col="color.fill", coding.color.col="coding.color", highlight.border.col="highlight", label.col="transcript_id", label.color.col="coding.color" )



# draw the features into a specific region of an existing plot
plot( x=1:10, y=rnorm( 10 ), ylim=c( -1, 10 ), yaxt="n", ylab="" )
plotFeatures( Transc, start.col="exon_chrom_start", end.col="exon_chrom_end", group.by="transcript_id", coding.start.col="transcript_coding_chrom_start", x.axis.side=3, transform.range.x=c( 1, 10 ), transform.range.y=c( 3, 10 ), add=TRUE )

# fancy one with some fake data.
Transc.sub <- Transc[ Transc$transcript_id=="ENST00000399265", ]
Data <- rnorm( length( unique( Transc.sub[ , "exon_id" ] ) ) )
plot( x=1:length( Data ), y=Data, ylim=c( -2, 12 ), yaxt="n", ylab="", xlab="exons", xlim=c( 0.5, length( Data )+0.5 ) )

plotFeatures( Transc.sub, start.col="exon_chrom_start", end.col="exon_chrom_end", coding.start.col="transcript_coding_chrom_start", coding.end.col="transcript_coding_chrom_end", group.by="transcript_id", strand.arrow=TRUE, strand.col="transcript_chrom_strand", add=TRUE, transform.range.x=c( 1, length( Data ) ), transform.range.y=c( 8, 12 ), x.axis.side=3 )

old.range <- range( c( Transc.sub[ , "exon_chrom_start" ], Transc.sub[ , "exon_chrom_end" ] ) )
plotAssignmentX( ytop=7.5, ybottom=2, x.left.t=transformTo( sort( unique( Transc.sub[ , "exon_chrom_start" ] ) ), old.range=old.range, new.range=c( 1, length( Data ) ) ), x.right.t=transformTo( sort( unique( Transc.sub[ , "exon_chrom_end" ] ) ), old.range=old.range, new.range=c( 1, length( Data ) ) ), x.left.b=( 1:length( Data ) )-0.5, x.right.b=( 1:length( Data ) )+0.5 )



}
\keyword{ data }
