% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gaps.R
\name{gap.cover}
\alias{gap.cover}
\title{Calculate the number, length, and percent of gaps}
\usage{
gap.cover(dima.tables, by.line = FALSE, tall = FALSE, by.year = FALSE,
  breaks = c(20, 25, 50, 100, 200), type = "canopy")
}
\arguments{
\item{dima.tables}{Raw tables as imported from a DIMA. Use data from \code{read.dima( all=T)}.}

\item{by.line}{Logical. If \code{TRUR} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.}

\item{tall}{Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.}

\item{by.year}{Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.}
}
\description{
Calculate the number, length, and percent of gaps by plot or line.
}
