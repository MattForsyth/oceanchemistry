#' A data scrubber
#'
#' This function helps you remove error values from your data. It is common for oceanographic instrumentation to report error values
#' during a project. Typically these are \code{-999} or \code{0}. Whatever your error value is, this function helps replace these values
#' with \code{NA}. This effectively removes error values from the dataset. The clean data is saved under \code{dataset} in the global environment.
#'
#' @param dataframe The dataset to be cleaned.
#' @param replacee The error values to be replace.
#' @param replacer The value used to replace your error values. This is automativally set to \code{NA}, but can be changed.
#' @keywords Scrubber, ocean, dataset, data, error
#' @export
#' @examples
#' library(oceanchemistry)
#'
#' data("dirty_greenland")
#' head(dirty_greenland)
#'
#' scrubber(dirty_greenland, c(-999,0))
#'
#'

scrubber <- function(dataframe, replacee, replacer = NA){

  df <- lapply(dataframe, function(x) replace(x,x %in% replacee, replacer) )
  dv <- as.data.frame(df)

  assign('dataset', dv, envir=.GlobalEnv)
}




