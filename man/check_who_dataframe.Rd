% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_who_dataframe.R
\name{check_who_dataframe}
\alias{check_who_dataframe}
\title{Check a dataframes dimensions and values to ensure it matches the underlying report}
\usage{
check_who_dataframe(
  df,
  rows,
  cols,
  unique_values,
  na_values = list(),
  known_values = list()
)
}
\arguments{
\item{df}{a dataframe}

\item{rows}{the number of rows expected in the dataframe}

\item{cols}{the number of columns expected in the dataframe}

\item{unique_values}{a named vector with names corresponding to columns in df
and values corresponding to the number of unique values
expected in each column. Used for the \verb{WHO region} and
\code{Country/area} columns.}

\item{na_values}{a list with tuples corresponding to cells in df}

\item{known_values}{a list with tuples corresponding to cells in df and their known values}
}
\description{
Check a dataframes dimensions and values to ensure it matches the underlying report
}
