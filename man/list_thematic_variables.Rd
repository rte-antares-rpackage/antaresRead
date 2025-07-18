% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thematic_trimming.R
\name{list_thematic_variables}
\alias{list_thematic_variables}
\title{List of thematic trimming variables available according to study version}
\usage{
list_thematic_variables(opts = simOptions())
}
\arguments{
\item{opts}{list of simulation parameters returned by the function
\code{\link{setSimulationPath}}}
}
\value{
\code{data.frame} of available columns
}
\description{
Minimal version required is v8.8
}
\examples{
\dontrun{
# Display list (use first `setSimulationPath()` to have an active study loaded)
list_thematic_variables()
}
}
