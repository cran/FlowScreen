\name{FlowScreen}
\alias{FlowScreen}
\alias{FlowScreen}
\docType{package}
\title{
Screen Daily Discharge Time Series for Temporal Trends and Change Points
}
\description{
This package can be used to calculate more than 30 different streamflow metrics
and identify temporal trends and changepoints. It is intended for use
as a data quality screening tool aimed at identifying streamflow records
that may have anthropogenic impacts or data inhomogeneity.
}
\details{
\tabular{ll}{
Package: \tab FlowScreen\cr
Type: \tab Package\cr
Version: \tab 1.2.5\cr
Date: \tab 2018-05-07\cr
License: \tab GPL (>= 2)\cr
}
Daily streamflow time series downloaded with the Environment Canada Data Explorer can be loaded with \code{\link{read.flows}}. The \code{\link{read.flows}} function can also be used to load daily streamflow time series from the USGS. The streamflow regime can be visualized with \code{\link{regime}}.  A list of 30 streamflow metrics that describe high flows, low flows, and baseflows can be calculated using \code{\link{metrics.all}}. The temporal occurrence of changepoints for all metrics or for only the high flow, baseflow, or low flow metrics can be analyzed using \code{\link{screen.cpts}}. If the streamflow time series has multiple metrics exhibiting changepoints within the same year (or few years), the time series can be further analyzed using \code{\link{screen.summary}} which creates a summary plot showing the significant temporal trends and changepoints for the high flow, low flow, or baseflow metrics. The \code{\link{screen.metric}} can be used to create a time series plot for one metric at a time. The \code{screen.metric} function works with individual metrics output from the following functions: \code{\link{pk.max}}, \code{\link{pk.max.doy}}, \code{\link{Qn}}, \code{\link{pk.bf.stats}}, \code{\link{dr.seas}}, \code{\link{MAMn}}, \code{\link{bf.stats}}, \code{\link{pk.cov}}, and \code{\link{bf.seas}}.The \code{\link{screen.frames}} function creates individual plots from the \code{\link{screen.summary}} function. The \code{\link{screen.frames}} function can also be used to create custom summary plots, see the example code in the function documentation. 
}
\author{
Jennifer Dierauer, Paul H. Whitfield

Maintainer: Jennifer Dierauer <jen.r.brand@gmail.com>

}
\references{
Bard, A., Renard, B., Lang, M. 2011. The AdaptAlp Dataset: Description, guidance, and analyses. In AdaptAlp WP 4 Report, 15. Lyon, France: Cemagraf. \cr

Bard, A., Renard, B., Lang, M., Giuntoli, I., Korck, J., Koboltschnig, G., Janza, M., d'Amico, M., Volken, D. 2015. Trends in the hydrologic regime of Alpine rivers. Journal of Hydrology online. \cr

Svensson, C., Kundzewicz, Z.W., Maurer, T. 2005. Trend detection in river flow series: 2. Flood and low-flow index series. Hydrological Sciences Journal 50:811-824. \cr

Whitfield, P.H. 2012. Why the provenance of data matters: Assessing "Fitness for Purpose" for environmental data. Canadian Water Resources Journal 37:23-36. \cr

Whitfield, P.H. 2013. Is 'Center of Volume' a robust indicator of changes in snowmelt timing? Hydrological Processes 27:2691-2698. \cr
}

\keyword{ package }
\seealso{
\code{\link[evir]{pot}}, \code{\link[evir]{decluster}}, \code{\link[changepoint]{cpt.meanvar}}, \code{\link[zyp]{zyp.trend.vector}}, \code{\link[Kendall]{Kendall}}
}
\examples{
\dontrun{
# load daily streamflow time series for the Caniapiscau River
data(caniapiscau)

# summary plot of the annual flow regime
caniapiscau.ts <- create.ts(caniapiscau)
regime(caniapiscau.ts)

# calculate high flow, low flow, and baseflow metrics
res <- metrics.all(caniapiscau.ts)

# plot histogram of changepoints for high flow, low flow, and baseflow metrics
screen.cpts(res, type="h")
screen.cpts(res, type="l")
screen.cpts(res, type="b")

# or plot all changepoints together
cpts <- screen.cpts(res)

# create screening plots for high, low, and baseflow metrics
screen.summary(res, type="h")
screen.summary(res, type="l")
screen.summary(res, type="b")
}

}
