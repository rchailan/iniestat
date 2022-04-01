#' Read an opt file at \code{file_path} and convert it into a `tidy-tibble`.
#' 
#' @param file_path the OPT file to parse. This file must contains tracking
#' infos.
#' @return TODO
#' 
#' @examples  
#' \dontrun{
#' # use as follows
#' my_tracking_df <- read_opt('my_f24_file.xml')
#' }
read_opt <- function(file_path=NULL) {
  require(dplyr)
  
  # verify arguments --------------------------------------------------------
  if (is.null(file_path)) {
    stop("The `file_path` argument has to be set.")
  }
  
  # read file ---------------------------------------------------------------
}