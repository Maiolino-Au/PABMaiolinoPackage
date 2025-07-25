% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s7_summary_plot_peaks.R
\name{s7.summary.plot.peaks}
\alias{s7.summary.plot.peaks}
\title{s7.summary.plot.peaks}
\usage{
s7.summary.plot.peaks(
  data_merged = merged_data,
  data_unmerged = unmerged_peaks
)
}
\arguments{
\item{data_merged}{A \code{data.table} or data frame containing merged gene-peak data, including columns \code{chr} and \code{peak_cpm}. Defaults to \code{merged_data}.}

\item{data_unmerged}{A \code{data.table} or data frame containing unmerged peaks, including columns \code{chr} and \code{peak_cpm}. Defaults to \code{unmerged_peaks}.}
}
\value{
A list with two ggplot objects: the first for merged peaks, the second for unmerged peaks.
}
\description{
Plot Peak Intensity Distribution by Chromosome

Generates and saves boxplots of ATAC-seq peak intensity (CPM) distributions per chromosome for both merged (gene-associated) and unmerged peaks.
The resulting plots are saved as PNG files in a results directory.
}
\author{
Maiolino_Aurelio
}
