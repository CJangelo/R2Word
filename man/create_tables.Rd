% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_tables.R
\name{create_tables}
\alias{create_tables}
\title{Create Frequency Tables}
\usage{
create_tables(
  freq.table,
  percent.table = NULL,
  decimal1 = 0,
  decimal2 = 1,
  row.names.provided = NULL,
  col.names.provided = NULL,
  prop.table.margin = NULL,
  prop.table.N = NULL,
  add.margins = F,
  add.N.to.colnames = F,
  add.col.of.rownames = F,
  name.of.rownames = NULL
)
}
\arguments{
\item{freq.table}{pass the frequency table to the function. Must be a matrix
or a dataframe.}

\item{percent.table}{pass the table of percentages; default is to compute this in the function}

\item{decimal1}{number of decimals in sum}

\item{decimal2}{number of decimals in percentage}

\item{row.names.provided}{pass a character vector of the row names for the output table}

\item{col.names.provided}{pass a character vecotr of the column nanmes for the output table}
}
\value{
Returns a dataframe of character values
}
\description{
Create tables of row/column sums, with percentages suitable for output
addresses rounding and formatting, prepares tables for output to MS Word file
options to include total values as well
}
