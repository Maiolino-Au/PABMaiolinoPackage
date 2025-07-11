% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_summarize_data.R
\name{s3.summarize.data}
\alias{s3.summarize.data}
\title{s3.summarize.data}
\usage{
s3.summarize.data(data)
}
\arguments{
\item{data}{A \code{data.table} with an 'id' column and one or more numeric columns to be summed.}
}
\value{
A named numeric vector containing the row sums, with names taken from the 'id' column.
}
\description{
Summarize rows of a data.table by summing across columns
}
\author{
Maiolino_Aurelio
}
