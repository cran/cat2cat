% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat2cat.R
\name{get_freqs}
\alias{get_freqs}
\title{Getting frequencies for a vector with an optional multiplier argument}
\usage{
get_freqs(x, multiplier = NULL)
}
\arguments{
\item{x}{vector}

\item{multiplier}{vector how many times to repeat certain value}
}
\value{
data.frame with two columns `input` `Freq`
}
\description{
getting frequencies for a vector with an optional multiplier argument
}
\examples{
data(occup)

head(get_freqs(occup$code[occup$year == "2008"]))

head(get_freqs(occup$code[occup$year == "2010"]))

head(get_freqs(occup$code[occup$year == "2008"], occup$multiplier[occup$year == "2008"]))

head(get_freqs(occup$code[occup$year == "2010"], occup$multiplier[occup$year == "2010"]))
}
