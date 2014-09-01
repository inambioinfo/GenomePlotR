\name{Transc}
\alias{Transc}
\docType{data}
\title{
Data frame with transcripts of a gene.
}
\description{
Data frame with exon start/end coordinates for transcripts of a gene.
}
\usage{data(Transc)}
\format{
  A data frame with 84 observations on the following 8 variables.
  \describe{
    \item{\code{transcript_id}}{The transcript id of the transcript.}
    \item{\code{gene_id}}{The id of the gene.}
    \item{\code{exon_id}}{The id of the exons.}
    \item{\code{exon_chrom_start}}{Start coordinates of the exons.}
    \item{\code{exon_chrom_end}}{End coordinates of the exons.}
    \item{\code{transcript_chrom_strand}}{The strand where the gene is located.}
    \item{\code{transcript_coding_chrom_start}}{The start coordinates of the coding region for the transcripts.}
    \item{\code{transcript_coding_chrom_end}}{The end coordinate of the coding region.}
  }
}
\examples{
data(Transc)
Transc[ 1:4,  ]
}
\keyword{datasets}
