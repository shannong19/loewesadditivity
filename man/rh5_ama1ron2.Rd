% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{rh5_ama1ron2}
\alias{rh5_ama1ron2}
\title{RH5 and AMA1RON2}
\format{a 38 x 15 data set where the columns are of the following format
\describe{
\item{well}{ one of iRBC (the max), uRBC (the min), RPMI (??), or comb (which is short for combination)}
\item{AMA1RON2}{dose of AMA1RON2 in mg/mL}
\item{RH5}{dose of RH5 in mg/mL}
\item{exp{x}{y}rep{z}}{the results from experiment x, sub item y, repetition z}
}}
\usage{
rh5_ama1ron2
}
\description{
The data is the raw data for a combination dose of RH5 and AMA1RON2.  The data was collected by PEOPLE and on DATE on this GRANT.
}
\examples{
data("rh5_ama1ron2")
head(rh5_ama1ron2)
}
\keyword{datasets}
