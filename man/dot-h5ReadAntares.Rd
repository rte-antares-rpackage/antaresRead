% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5_antaresReadH5.R
\name{.h5ReadAntares}
\alias{.h5ReadAntares}
\title{Read data}
\usage{
.h5ReadAntares(
  path,
  areas = NULL,
  links = NULL,
  clusters = NULL,
  districts = NULL,
  mcYears = NULL,
  misc = FALSE,
  thermalAvailabilities = FALSE,
  hydroStorage = FALSE,
  hydroStorageMaxPower = FALSE,
  reserve = FALSE,
  linkCapacity = FALSE,
  mustRun = FALSE,
  thermalModulation = FALSE,
  timeStep = "hourly",
  select = NULL,
  showProgress = TRUE,
  simplify = TRUE,
  perf = FALSE
)
}
\arguments{
\item{path}{{character} path of h5file to load}

\item{areas}{see \link[antaresRead]{readAntares}}

\item{links}{see \link[antaresRead]{readAntares}}

\item{clusters}{see \link[antaresRead]{readAntares}}

\item{districts}{see \link[antaresRead]{readAntares}}

\item{mcYears}{see \link[antaresRead]{readAntares}}

\item{misc}{see \link[antaresRead]{readAntares}}

\item{thermalAvailabilities}{see \link[antaresRead]{readAntares}}

\item{hydroStorage}{see \link[antaresRead]{readAntares}}

\item{hydroStorageMaxPower}{see \link[antaresRead]{readAntares}}

\item{reserve}{see \link[antaresRead]{readAntares}}

\item{linkCapacity}{see \link[antaresRead]{readAntares}}

\item{mustRun}{see \link[antaresRead]{readAntares}}

\item{thermalModulation}{see \link[antaresRead]{readAntares}}

\item{timeStep}{see \link[antaresRead]{readAntares}}

\item{select}{see \link[antaresRead]{readAntares}}

\item{showProgress}{see \link[antaresRead]{readAntares}}

\item{simplify}{see \link[antaresRead]{readAntares}}

\item{perf}{\code{boolean}, eval performance during developpement time, to remove}
}
\description{
Read data
}
\keyword{internal}
