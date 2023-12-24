
#' Check if an instance is a member of a list or vector
#'
#' @param instance an R6 instance
#' @param vector_or_list a vector or list (possibly empty)
#'
#' @return TRUE of FALSE
#' @author fnaufel
#' @export
#'
is_in <- function(
  instance, vector_or_list
) {

  for (i in vector_or_list) {
    if (identical(i, instance)) {
      return(TRUE)
    }
  }

  return(FALSE)

}
