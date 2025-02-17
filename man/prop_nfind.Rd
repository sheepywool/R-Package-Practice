% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/week4.R
\name{prop_nfind}
\alias{prop_nfind}
\title{Two Sample Proportions Sample Size Calculator}
\usage{
prop_nfind(p1, p2, alpha, power, ratio)
}
\arguments{
\item{p1}{Success rate in sample 1}

\item{p2}{Success rate in sample 2}

\item{alpha}{Significance level}

\item{power}{Power, 1-beta}

\item{ratio}{Ratio between sample size 1 and 2}
}
\value{
Sample size needed for significance in two samples proportions test
}
\description{
Two Sample Proportions Sample Size Calculator
}
\examples{
prop_nfind(0.5, 0.8, 0.05, 0.8, 1)
}
