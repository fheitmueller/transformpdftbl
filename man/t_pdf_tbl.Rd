% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformpdftbl.R
\name{t_pdf_tbl}
\alias{t_pdf_tbl}
\title{transform table output}
\usage{
t_pdf_tbl(df, pl, column = NULL, separator = " ")
}
\arguments{
\item{df}{is the dataframe to be transformed}

\item{pl}{is a vector containing the numbers of the "principle lines". Can for example be obtained by a formula like the following: which(is.na(df[,1]), arr.ind = TRUE)}

\item{column}{is a vector containing the names of the columns to be copied. Defaults to all columns.}

\item{separator}{is the separator between the copied columns. Defaults to 1 whitespace.}
}
\value{
returns the transformed dataframe
}
\description{
When, for example, exporting a table from a pdf, then content belonging to one line is often scattered across several lines.
This function copies lines that belong together in one and deletes the rest.
}
