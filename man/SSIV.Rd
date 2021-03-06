% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSIV.R
\name{SSIV}
\alias{SSIV}
\title{Shift Share IV estimator}
\usage{
SSIV(
  location_outcomes,
  industry_shocks,
  industry_location_shares,
  methods = c("Uncorrected", "AKM", "BHJ"),
  equally_weighted = FALSE,
  lagIV = FALSE,
  lagsIV = 1,
  industryFE = FALSE,
  timeFE = FALSE
)
}
\arguments{
\item{location_outcomes}{Location outcomes}

\item{industry_shocks}{Industry shocks}

\item{industry_location_shares}{Industry location shares}

\item{methods}{(optional) default is c("Uncorrected", "AKM", "BHJ")}

\item{equally_weighted}{(optional) default is FALSE}

\item{lagIV}{(optional) default is FALSE}

\item{industryFE}{(optional) default is FALSE}

\item{timeFE}{(optional) default is FALSE}
}
\description{
Shift Share IV estimator
}
