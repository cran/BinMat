% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peakRemove.R
\name{peak.remove}
\alias{peak.remove}
\title{Removes samples with peaks equal to or less than a specified threshold value.}
\usage{
peak.remove(x, thresh)
}
\arguments{
\item{x}{Binary matrix (consolidated or original).}

\item{thresh}{Peak threshold value for removal.}
}
\value{
Filtered dataset, and either the row name/s or row number/s of samples that were removed.
}
\description{
Removes samples with a peak number less than a specified value.
}
\examples{
mat = BinMatInput_ordination
new = peak.remove(mat, 4)

}
