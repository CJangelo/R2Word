
#' Round Numbers
#'
#'
#' Round numbers to a specified number of decimal places
#' converts numeric values to a character value
#' used to prepare the numeric values for a table output
#' @param decimals vector specifies the number of decimals to round each numeric column to; default is 2
#' @return will return a dataframe of character values
#' @export



round_numbers <- function(out, cols = NULL, decimals = 2, NA.string = NA){

  output <- out  #

  if(is.null(cols)){

    if(is.numeric(out)) {

        cols <- 1:ncol(out)

    } else {

        cols <- which (unlist(lapply(out, is.numeric)))

    }
  }# end assign "cols" unless passed



  # Round the variables:

  for(i in 1:nrow(out)){

      it <- 1

    for(j in cols){

        if(length(decimals) > 1){
          #print.decimals <- decimals[j]
          print.decimals <- decimals[it]
        } else {
          print.decimals <- decimals
        }

      output[i , j] <-
        sprintf(paste0("%0.", print.decimals, "f"), out[i, j])

      it <- it + 1

    }
  }

  output[output == 'NA'] <- NA.string # you want to let the other print function decide what to do with the NA
  output[output == 'NaN'] <- NA.string # you want to let the other print function decide what to do with the NA

  return(output)

}
