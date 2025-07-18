% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set.plot.titles.R
\name{set.plot.titles}
\alias{set.plot.titles}
\title{Set plot titles}
\usage{
set.plot.titles(
  TS,
  title.elements = c("StationID", "StnName", "Country"),
  delimeter = " - ",
  custom.title = NULL,
  title.size = 1
)
}
\arguments{
\item{TS}{data.frame of streamflow time series loaded with \code{\link{read.flows}}.}

\item{title.elements}{A character vector with the title elements you want to 
include in the plot title, in the desired order. Possible values are: Agency, 
StationID, StnName, StateProv, Country, Lat, Lon, CatchmentArea_km2, 
MetadataSource. Default is \code{c("StationID", "StnName", "Country")}. 
Additional examples: \code{c("StnName", "StateProv")}, 
\code{c("StnName", "StationID")}, etc.}

\item{delimeter}{separator for title elements, default is " - "}

\item{custom.title}{String of a custom plot title. Default is NULL. Will
supersede title.format if not NULL.}

\item{title.size}{parameter cex for the base::plot function. Number 
indicating the amount by which plotting text and symbols should be scaled 
relative to the default. 1=default, 1.5 is 50 percent larger, 0.5 is 50 percent smaller, etc.}
}
\value{
Returns the input TS data.frame with a 'plot title' attribute added. 
  This attribute will be the default option used for all plot titles unless an 
  alternative title is passed to the plotting function, e.g. with \code{\link{regime}}.
}
\description{
Sets the title to be used for all plots.
}
\examples{

# Load example ROBIN streamflow data
robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
TS <- read.flows(robin_path)
TS <- set.plot.titles(TS, title.elements = c("StationID", "StnName"))
regime(TS, title = TRUE)

TS <- set.plot.titles(TS, custom.title = "My Custom Plot Title")
regime(TS, title = TRUE)
}
\author{
Jennifer Dierauer
}
