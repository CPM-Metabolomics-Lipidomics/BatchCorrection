#' data
#'
#' @title Read the data file
#'
#' @description
#' Read the data file. It can be in different formats
#'
#' @param file_path character(1) containing the file path to the imported data
#'     file.
#' @param sep character(1) for several file formats a separator can be specified.
#' @param first_column_as_index logical(1) should the first column be used as
#'     index.
#'
#' @return data.frame
#'
#' @importFrom stringr str_sub
#' @importFrom readxl read_xlsx
#' @importFrom utils read.table
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
read_data <- function(file_path = NULL,
                      sep = NULL,
                      first_column_as_index = FALSE) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  # if (is.na(sep)) {
  #   if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
  #     sep <- '\t'
  #   }
  # }

  if (first_column_as_index) {
    index <- 1
  } else {
    index <- NULL
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table <- as.data.frame(readxl::read_xlsx(path = file_path))
  } else {
    if (is.null(sep)) {
      sep <- find_delim(file_path = file_path)
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    } else {
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    }
  }

  # if (!is.null(index)) {
  #   duplicates <- duplicated(data_table[, index])
  #   if (sum(duplicates) > 0) {
  #     print(paste0('Removed ', sum(duplicates), ' duplicated features'))
  #     data_table <- data_table[!duplicates, ]
  #     rownames(data_table) <- data_table[, 1]
  #     data_table[, 1] <- NULL
  #   }
  # }

  original_count <- ncol(data_table)
  if (original_count > 1) {
    data_table <- data_table[, !duplicated(colnames(data_table))]
    final_count <- ncol(data_table)
    if(original_count != final_count) {
      print(paste0("Removed ", original_count - final_count, " duplicated columns"))
    }
  }

  return(data_table)
}


#' @title Find the delimiter of a file
#'
#' @description
#' Find the delimiter of a file by reading the first 10 lines.
#'
#' @param file_path character(1) path of the file.
#'
#' @return character(1), the delimiter found.
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
find_delim = function(file_path = NULL) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  probe <- paste(readLines(con = file_path,
                           n = 10),
                 collapse = "")
  sep <- c(
    "\t" = lengths(regmatches(x = probe,
                              m = gregexpr(pattern = "\t",
                                           text = probe))),
    "," = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ",",
                                          text = probe))),
    ";" = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ";",
                                          text = probe)))
  )

  return(names(which.max(sep)))
}


#' @title Distribution plot
#'
#' @description
#' Distribution plot of the sample types
#'
#' @param data data.frame with all the data.
#' @param title character(1), title of the plot.
#'
#' @return ggplot2 object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes .data geom_bar geom_text labs theme_minimal
#'
#' @noRd
distribution_plot <- function(data = NULL,
                              title = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .data$count)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "lightblue") +
    ggplot2::geom_text(ggplot2::aes(label = .data$count),
                       vjust = -0.5,
                       hjust = 0.5,
                       size = 4) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = title) +
    ggplot2::theme_minimal()

  return(p)
}
