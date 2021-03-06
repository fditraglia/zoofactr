% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{vech}
\alias{vech}
\title{Half-vectorization of a symmetric matrix}
\usage{
vech(M)
}
\arguments{
\item{M}{An n by n symmetric matrix.}
}
\value{
A column vector containing the vech of M, i.e.
the n * (n + 1) / 2 unique elements of M.
}
\description{
Half-vectorization of a symmetric matrix
}
\details{
Throws an error if M is not square, but does not test if M is
symmetric. Elements above the main diagonal are simply ignored.
}
\examples{
M <- matrix(c(11, 12, 13, 14,
              12, 22, 23, 24,
              13, 23, 33, 34,
              14, 24, 34, 44), 4, 4, byrow = TRUE)
vech(M)
}

