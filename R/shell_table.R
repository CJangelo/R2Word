#' Create a Function to make shell tables
#' @param numeric.tables pass the data frame
#' @param cols specify which columns of the dataframe you want the numbers
#' converted to x
#' @param decimals how many xx to add
#' @param NA.string  determines how to print out missing (i.e., NA) strings; default is blank
#' @return outputs dataframe with character vectors
#'
#' @export

shell_table <- function(out = NULL, cols = NULL, decimals = 2, NA.string = NA){

  out.shell <- out
  out.shell <- as.data.frame(out.shell)

  if (!is.null(cols)) {
    out.shell <- out.shell[, cols, drop = F]
  }


  if (any(sapply(out.shell, is.numeric))) {
      tmp <- out.shell[, sapply(out.shell, is.numeric), drop = F]
      tmp <- do.call(data.frame, lapply(tmp, abs))
      out.shell[, sapply(out.shell, is.numeric)] <- tmp
  }

  out.shell <- R2Word::round_numbers(out.shell,
                                     cols = NULL,
                                     decimals = decimals,
                                     NA.string = NA.string)

  # Replace integers with x
    for (int.val in 0:9) {
      for (cc in colnames(out.shell)) {

          out.shell[, cc] <- gsub(pattern = paste0(int.val),
                                replace = 'x',
                                x = out.shell[, cc])
        }
      }


  if (!is.null(cols)) {
    out[, cols] <- out.shell
  } else {
    out <- out.shell
  }


  return(out)


} #end function




