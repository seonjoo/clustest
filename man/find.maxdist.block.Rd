% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.maxdist.block.R
\name{find.maxdist.block}
\alias{find.maxdist.block}
\title{Find the maximum distance between data block}
\usage{
find.maxdist.block(dat, indx, seg.length = 1000)
}
\arguments{
\item{dat}{:n x p matrix with rows are observations, columns are variables}

\item{indx}{1 < length(indx) < n index vector}

\item{seg.length}{segmentation length (default=1000).
If seg.length > length(indx), floor(nrow(dat)/4) will be used.}
}
\value{
aa : maximum distance between specific indiced data block
}
\description{
Find the maximum distance between data block
}
