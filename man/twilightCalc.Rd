\name{twilightCalc}
\alias{twilightCalc}
\title{Calculate twilight events (sunrise/sunset) from light intensity measurements over time}
\description{Defines twilight events (sunrise/sunset) at times when the light intensity measurements (\emph{light}) pass the defined light intensity threshold. An interactive plot can be drawn to assess the calculations and improve e.g. select only the realistic events.}
\usage{twilightCalc(datetime,light,LightThreshold=TRUE,maxLight=NULL,ask=TRUE,nsee=500) }
\arguments{
  \item{datetime}{date and time of light intensity measurements e.g. 2008-12-01 08:30 "UTC" (see: \code{\link{as.POSIXct}},\link[=Sys.timezone]{time zones}).}
  \item{light}{\code{numerical} value of the light intensity (usually arbitrary units).}
  \item{LightThreshold}{the light intensity threshold for the twilight event calibration. If \code{Default}, it will be set slightly above (3 units) the baseline level (measurement during the night).}
  \item{maxLight}{if the geolocator record the maximum light value of a certain time span, give the interval of maximum recordings in minutes (e.g. 5).}  
  \item{ask}{\code{logical}, if TRUE the interactive plot will start after the calculation.}
  \item{nsee}{number of points to plot per screen.}
}
\value{A \code{data frame}. Each row contains two subsequent twilight events (\emph{tFirst, tSecond}) and \emph{type} defining wether \emph{tFirst} refers to sunrise (1) or sunset (2).}
\note{
Depending on shading during light intensity measurements (e.g. due to vegetation, weather, etc., see Lisovski et \emph{al.} 2012) the light intensities may pass the light intensity threshold several times during the day, resulting false sunrises and sunsets. It is highly recommended to check the derived events visually (\code{ask=TRUE}).Twilight events can be deleted and undeleted by clicking the (first) mouse button at the particular position in the graph. The second mouse buttom (or esc) moves the time series forward. Note, that a backward option is not included.
}
\author{Simeon Lisovski}