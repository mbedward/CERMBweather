% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_functions.R
\name{bom_db_get_station_dates}
\alias{bom_db_get_station_dates}
\title{Get first and last record dates for stations}
\usage{
bom_db_get_station_dates(db, the.table, sql.only = FALSE)
}
\arguments{
\item{db}{A database connection object.}

\item{the.table}{The table to query: either 'aws' or 'synoptic'
(may be abbreviated).}

\item{sql.only}{If \code{TRUE}, the function returns the SQL code for the
query but does not run it. This can be useful if you want to further
modify the query. If \code{FALSE} (default), the function runs the query
and returns the resulting data.}
}
\value{
A data frame with columns: station, earliest_date, latest_date.
}
\description{
This function performs a fast query to find the dates of the earliest and
the most recent record for each station.
}
\details{
The speed of this query with large tables comes from the use of a LATERAL
join. This is a type of join provided by PostgreSQL that allows a SQL query
to function like a for-each loop. It also relies on the presence of an index
on the date_local field. The approach is adapted from an example (number 5)
in \href{https://stackoverflow.com/a/34715134/40246}{this StackOverflow answer.}
}
