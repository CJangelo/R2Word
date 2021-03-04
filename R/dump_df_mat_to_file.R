#' Dump a dataframe/matrix to MS Word file
#'
#' This function takes a dataframe or matrix and dumps it out as a table in a MS Word file. The
#' table will be printed in the R Viewer as well. The defaults
#' are such that minimal fussing with the R object is required in order to create
#' the MS Word table. This was designed to streamline the process of moving from completed analysis
#' to output.


#' @param decimals a vector of the number of decimals to round each numeric column to, can also be a scalar
#' if that number applies to all the numbers; default is 2
#' @param NA.string determines how to print out missing (i.e., NA) strings; default is blank
#' @param fontname specify the font; default is 'Arial'
#' @param table.title pass a character vector specifying the table title
#' @param table.footnote pass a character vector specifying a footnote
#' @param print.dir pass a file path for the file to be printed out to
#' @param file.name pass a name for the file
#'
#' @return outputs table in a MS Word table and prints table to R Viewer
#'
#' @export



dump_df_mat_to_file <- function(out,
                                cols = NULL, decimals = 2, NA.string = NA,  # pass to round_numbers_2
                                fontname = 'Arial',  # pass to flextable
                                table.title = NULL,
                                table.footnote = NULL,
                                print.dir = NULL,
                                file.name = NULL){   # print out


  # pass the variables to R1_round_numbers() function
  #source("C:/Users/ciaconangelo/Documents/R1 Code Library/R1_Function_round_numbers.R")

  out <- round_numbers(out, cols = cols, decimals = decimals, NA.string = NA.string)


  # library(flextable)
  # library(officer)

  myft <- flextable::flextable(out)
    # Make header bold:
  myft <- flextable::bold(myft, part = 'header')
  # Align it
  myft <- flextable::autofit(myft)
  # Center:
  myft <- flextable::align(myft, align = "center", part = 'all' )


    # Optionals:
  if(!is.null(table.title)){

      myft <- flextable::add_header_lines(myft, values = table.title )
      myft <- flextable::bold(myft, part = 'header')


  }

  if(!is.null(table.footnote)){

      myft <- flextable::add_footer_row(x = myft, top = F,  colwidths=ncol(out), values = table.footnote)

  }


  # Set the font to Arial:
  myft <- flextable::font(myft, fontname = fontname,  part = 'all')
  # Font size:
  myft <- flextable::fontsize(myft,  size = 11, part = "all")
 # Align it - 9.17.20: Modified the alignment
  # https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
  #myft <- autofit(myft)
  myft <- flextable::set_table_properties(myft, layout = 'autofit')



####################################################################
  # Print out the table using officeR
  out.doc <- officer::read_docx()
  out.doc <- flextable::body_add_flextable(x = out.doc, value= myft)

  if(is.null(print.dir)) {

      print.dir <- dirname(getwd())
      print.dir <- paste0(print.dir, "/TABLES")

  }

  if(is.null(file.name)) {

     file.name <- paste0(colnames(out), collapse = '')
     file.name <- paste0('out_', file.name)

  }

  print(out.doc, target = paste0(print.dir,'/', file.name , '.docx') )

  # Print out to the console what you're printing to the file:
  return(myft)

 }
