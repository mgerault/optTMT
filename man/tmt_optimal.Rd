% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmt_optimal.R
\name{tmt_optimal}
\alias{tmt_optimal}
\title{tmt_optimal}
\usage{
tmt_optimal(
  TMTset = c("10", "11", "16", "18", "32", "35"),
  ncond = 3,
  rep = 3,
  nmix = 0,
  nbatch = 1,
  tmt_correction = NULL,
  exact = FALSE,
  maxiter = 10,
  your_design = NULL
)
}
\arguments{
\item{TMTset}{Character specifying the size of the TMT set. Either 10, 11, 16, 18, 32 or 35}

\item{ncond}{The number of unique condition}

\item{rep}{The number of replicate.
If of length one, it assume the same number of replicates for each condition.
If of the same length as the number of unique condition, it will assume a specific number of
replicate for each condition.}

\item{nmix}{The number of carrier channel. Only produce interference to other channels.}

\item{nbatch}{Number of TMT batch}

\item{tmt_correction}{A data.frame of three columns stating the amount of impurities in %
of the corresponding channels. If NULL, takes the default data from optTMT.}

\item{exact}{Logical to compute the exact optimum. To do so, all possible permutations will be computed.
Hence, it cannot be performed for a TMT set of greater size than 11.}

\item{maxiter}{If exact is FALSE, the maximum number of iteration of the optimization procedure.}

\item{your_design}{A named vector corresponding to the starting experimental design to optimize.
If NULL, will start with all the unique conditions grouped together.}
}
\value{
A data.frame with all the computed design ordered according their global interference
}
\description{
Function to compute the optimal experimental design
}
