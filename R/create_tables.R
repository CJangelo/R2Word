
#' Create Frequency Tables
#'
#' Create tables of row/column sums, with percentages suitable for output
#' addresses rounding and formatting, prepares tables for output to MS Word file
#' options to include total values as well
#'
#' @param freq.table pass the frequency table to the function
#' @param percent.table pass the table of percentages; default is to compute this in the function
#' @param decimal1 number of decimals in sum
#' @param decimal2 number of decimals in percentage
#' @param row.names.provided pass a character vector of the row names for the output table
#' @param col.names.provided pass a character vecotr of the column nanmes for the output table
#' @return Returns a dataframe of character values


#' @export


create_tables <- function(freq.table, percent.table = NULL, decimal1 = 0, decimal2 = 1,
                          row.names.provided = NULL, col.names.provided = NULL,
                          prop.table.margin = NULL, prop.table.N = NULL,
                          add.margins = F,
                          add.N.to.colnames = F,
                          add.col.of.rownames = F,
                          name.of.rownames = NULL){



  #first, check to see if they're matrices: this may not always function the way you want, better to put them in yourself:
  # Do this yourself, too many possible errors
  #if(is.vector(freq.table)){freq.table <- matrix(freq.table, nrow = 1)}
  #if(is.vector(percent.table)){freq.table <- matrix(freq.table, nrow = 1)}


    if(is.null(percent.table) & !is.null(prop.table.N)){ percent.table <- (100/prop.table.N)*freq.table }


    if(is.null(percent.table)){percent.table <- 100*prop.table(freq.table, margin = prop.table.margin)}


if(!all(dim(freq.table) == dim(percent.table))){stop} # they had better have same dimensions, otherwise, stop function


  # need to add the margins up here, I think:

output <- matrix(NA, nrow = nrow(freq.table), ncol = ncol(freq.table))

for(r in 1:nrow(output)){
  for(c in 1:ncol(output)){


    output[r, c] <- paste0(
                            sprintf(paste0("%.", decimal1, "f"), freq.table[r, c]),
                            '(', sprintf(paste0("%.", decimal2, "f"), percent.table[r, c]), ')')


  }
}


### 9.25.2018
# TO DO: Fix the marginals - you need to paste 'N' into the corner to fill out the cbind function

# rowsums:
if(add.margins == 1){

  if(prop.table.margin == 1){ output <- cbind(output, paste0(rowSums(freq.table), '(100.0)')) }

    else{output <- cbind(output, paste0(rowSums(freq.table))) }

}


# colsums:
if(add.margins == 2){

  if(prop.table.margin == 2){ output <- rbind(output, paste0(colSums(freq.table), '(100.0)')) }

    else{output <- rbind(output, paste0(colSums(freq.table))) }

}

# both!
if(add.margins == 'both'){

 if(!is.null(prop.table.margin)){

  if(prop.table.margin == 1){  output <- cbind(output, paste0(rowSums(freq.table), '(100.0)'))
                                    output <- rbind(output, c(paste0(colSums(freq.table)), paste0(sum(freq.table), '(100.0)')))   }

  if(prop.table.margin == 2){  output <- cbind(output, paste0(rowSums(freq.table)))
                                    output <- rbind(output, c(paste0(colSums(freq.table), '(100.0)'), paste0(sum(freq.table), '(100.0)')))   }
 }

  if(is.null(prop.table.margin)){  output <- cbind(output, paste0(rowSums(freq.table)))
                                   output <- rbind(output, c(paste0(colSums(freq.table)), sum(freq.table)))   }

}

####

# provided? if yes, use them
if(!is.null(row.names.provided)){rownames(output) <- row.names.provided}
if(!is.null(col.names.provided)){colnames(output) <- col.names.provided}

#otherwise just pull them from the input
#if(is.null(row.names.provided)){rownames(output) <- rownames(freq.table)}
#if(is.null(col.names.provided)){colnames(output) <- colnames(freq.table)}

if(is.null(row.names.provided)){
  if(add.margins == 2 | add.margins == 'both'){rownames(output) <- c(rownames(freq.table), 'Total')}else{rownames(output) <- rownames(freq.table) }
}

if(is.null(col.names.provided)){
  if(add.margins == 1 | add.margins == 'both'){colnames(output) <- c(colnames(freq.table), 'Total')}else{colnames(output) <- colnames(freq.table) }
}


# add the "Column Name, n=colSums(freq.table, na.rm = T)"  #9.9.2019
if(add.N.to.colnames == T){ #9.27.19, added the option, lol

    if(add.margins %in% c(1, 'both')) {  # 10.29.19 - fixed naming if you have a column for the totals

     colnames(output) <-paste0(colnames(output), ', n=', c(colSums(freq.table, na.rm = T), sum(freq.table, na.rm = T)))

  } else {

      colnames(output) <- paste0(colnames(output), ', n=', colSums(freq.table, na.rm = T))
  }
}


# 9.27.19 - make a dataframe with the rownames the first column
if(add.col.of.rownames == T){

    df.output <-
      data.frame(
        ' ' = rownames(output),
        output,
        stringsAsFactors=F
      )

    if(is.null(name.of.rownames)){
      colnames(df.output) <- c(' ', colnames(output)) # might not be necessary, but the naming function for dataframes is weird
    } else {
      colnames(df.output) <- c(name.of.rownames, colnames(output))
    }

    rownames(df.output) <- 1:nrow(df.output)

    output <- df.output
} #end make data frame w/ column of rownames


    return(output)


}




