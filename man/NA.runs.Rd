% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NA.runs.r
\name{NA.runs}
\alias{NA.runs}
\title{Missing data runs for daily time series.}
\usage{
NA.runs(TS, quiet = FALSE)
}
\arguments{
\item{TS}{data.frame of streamflow time series loaded with \code{\link{read.flows}}.}

\item{quiet}{A boolean to indicate message printing.}
}
\value{
Returns a data.frame with the following columns:
  \itemize{
    \item Start - Date of the start of the missing data period
    \item End - Date of the end of the missing data period
    \item Duration - number of days in the missing data period
  }
}
\description{
This function takes a data.frame from create.ts and returns a data.frame of 
missing data runs.
}
\examples{
data(caniapiscau)
cania.sub <- caniapiscau[300:1800,]
cania.ts <- create.ts(cania.sub)
res <- NA.runs(cania.ts)
print(res)
}
\seealso{
\code{\link{create.ts}} to create input, \code{\link{NA.count.runs}} to sum the
  the missing data occurrences by year or month.
}
\author{
Jennifer Dierauer
}
