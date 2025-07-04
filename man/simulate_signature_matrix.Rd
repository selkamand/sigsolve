% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigsolve.R
\name{simulate_signature_matrix}
\alias{simulate_signature_matrix}
\title{Simulate a Signature Matrix}
\usage{
simulate_signature_matrix(signatures, channels, digits = 2, seed = NULL)
}
\arguments{
\item{signatures}{A vector of signature names to be used as column names in the resulting matrix.}

\item{channels}{A vector of channel names to be used as row names in the resulting matrix.}

\item{digits}{Integer value indicating the number of decimal places to which the proportions should be rounded. Default is 2.}

\item{seed}{An optional seed value for reproducibility. Default is NULL, meaning no seed is set.}
}
\value{
A matrix where rows represent channels and columns represent signatures. Each entry is a randomly generated proportion, and the values in each column sum to 1.
}
\description{
This function simulates a matrix where each column corresponds to a signature, and each entry in the matrix is a random proportion of a set of channels. The proportions in each column sum to 1.
}
\examples{
signatures <- c("Signature1", "Signature2", "Signature3")
channels <- c("Channel1", "Channel2", "Channel3")
simulate_signature_matrix(signatures, channels, digits = 2, seed = 123)
}
