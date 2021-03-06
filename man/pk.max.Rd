% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk.max.R
\name{pk.max}
\alias{pk.max}
\title{Annual maximum series}
\usage{
pk.max(TS)
}
\arguments{
\item{TS}{output from \code{\link{create.ts}} containing a data.frame of 
the daily streamflow time series}
}
\value{
Returns a numeric vector containing the annual maximum flow (m3/s) 
  series, by hydrologic year. The "times" attribute contains the hydrologic year 
  for each element in the vector.
}
\description{
This function returns the annual maximum series from a daily streamflow time
series.
}
\examples{
data(cania.sub.ts)
res <- pk.max(cania.sub.ts)
res2 <- screen.metric(res, "Q (m3/s)")
}
\author{
Jennifer Dierauer
}
\seealso{
See \code{\link{create.ts}} to format the input flow series.

  See \code{\link{pk.max.doy}} to find the day of year for each annual
  maximum flow event.
}

