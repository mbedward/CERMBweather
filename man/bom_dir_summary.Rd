% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir_functions.R
\name{bom_dir_summary}
\alias{bom_dir_summary}
\title{Get summary information for BOM weather station files in a directory}
\usage{
bom_dir_summary(
  path,
  pattern = "_Data_\\\\d{6}_.*txt$",
  include = c("data", "all", "empty")
)
}
\arguments{
\item{path}{Path to the input directory.}

\item{pattern}{A regular expression pattern used to identify weather station
data files. The default is \code{"_Data_\\d{6}_.*txt$"} which means the
string '_Data_' followed by six digits (station identifier), an underscore,
more characters (which will usually be digits), and ending with the file
extension 'txt'. For more information about specifying regular expression
patterns see \code{\link[base]{regex}}.}

\item{include}{One of the following (may be abbreviated): 'data' (default) to
only list stations with non-empty data sets; 'all' to list all stations;
'empty' to only list stations with empty data sets.}
}
\value{
A data frame with columns station (identifier); filename; filesize.
}
\description{
This function summarizes the contents of a directory containing delimited
text data files (usually with a '.txt' extension) for one or more weather
stations. By default, data files are identified by having the string '_Data_'
followed by the weather station number in the file name. See notes on the
\code{pattern} argument below.
}
