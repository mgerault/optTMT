% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_design.R
\name{draw_design}
\alias{draw_design}
\title{draw_design}
\usage{
draw_design(design, tmt_correction = NULL)
}
\arguments{
\item{design}{A named vector stating the which sample is assigned to which TMT channel.}

\item{tmt_correction}{A data.frame of three columns stating the amount of impurities in %
of the corresponding channels. If NULL, takes the default data from optTMT.}
}
\value{
A ggplot object showing which channel produce interference in your experimental design.
}
\description{
Function to plot your experimental design
}
