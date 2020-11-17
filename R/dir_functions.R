#' Get summary information for BOM weather station files in a directory
#'
#' This function summarizes the contents of a directory containing delimited
#' text data files (usually with a '.txt' extension) for one or more weather
#' stations. By default, data files are identified by having the string '_Data_'
#' followed by the weather station number in the file name. See notes on the
#' \code{pattern} argument below.
#'
#' @param path Path to the input directory.
#'
#' @param pattern A regular expression pattern used to identify weather station
#'   data files. The default is \code{"_Data_\\d{6}_.*txt$"} which means the
#'   string '_Data_' followed by six digits (station identifier), an underscore,
#'   more characters (which will usually be digits), and ending with the file
#'   extension 'txt'. For more information about specifying regular expression
#'   patterns see \code{\link[base]{regex}}.
#'
#' @param include One of the following (may be abbreviated): 'data' (default) to
#'   only list stations with non-empty data sets; 'all' to list all stations;
#'   'empty' to only list stations with empty data sets.
#'
#' @return A data frame with columns station (identifier); filename; filesize.
#'
#' @export
#'
bom_db_dir_summary <- function(path,
                               pattern = "_Data_\\d{6}_.*txt$",
                               include = c("data", "all", "empty")) {

  include <- match.arg(include)

  # Identify station data files
  files <- dir(path, full.names = FALSE)
  files <- stringr::str_subset(files, pattern = pattern)

  filepaths <- sapply(files, function(f) .safe_file_path(path, f))

  sizes <- file.size(filepaths)

  station <- CERMBweather::bom_db_extract_station_numbers(files)

  dat <- data.frame(station, filename = files, filesize = sizes,
                    stringsAsFactors = FALSE)

  ii <- switch(include,
               data = sizes > 0,
               all = rep(TRUE, length(files)),
               empty = sizes == 0)

  dat[ii, ]
}
