#Copyright © 2016 RTE Réseau de transport d’électricité

#' Copy data to the clipboard
#' 
#' \code{copyToClipboard} is a utility function that copies data to the
#' clipboard. The data can then be copied in another program like excel.
#' 
#' @note
#' The function is useful only for small data objects: for a table,
#' only the 50000 rows are copied to clipboard. If the table to copy
#' is longer, either use filters to reduce the number of rows or write the
#' table in text file with \code{\link{write.table}}
#' 
#' @param x
#' an object used to select a method.
#' 
#' @param what
#' character or numeric indicating which element to copy to clipboard (areas,
#' links, clusters or districts)
#' 
#' @param ... 
#' arguments passed to \code{\link{write.table}}
#' 
#' @return 
#' The function does not return anything. It is only used to interact with the
#' clipboard
#' 
#' @examples
#'  # This only works on Windows systems
#' \dontrun{
#' x <- data.frame(a = sample(10), b = sample(10))
#' 
#' copyToClipboard(x)
#' 
#' # Try to open excel and use CTRL + V to copy the data in a spreadsheet.
#' }
#' 
#' @export
copyToClipboard <- function(x, ...) {
  if (!exists("writeClipboard", getNamespace("utils"))) {
    stop("This function works only on windows systems")
  }
  UseMethod("copyToClipboard", x)
  invisible()
}

#' @rdname copyToClipboard
#' @export 
copyToClipboard.antaresDataList <- function(x, what, ...) {
  if (length(x) == 1) copyToClipboard(x[[1]])
  else {
    if (missing(what)) {
      cat("Which element do you want to copy to clipboard ?\n")
      for (i in 1:length(x)) cat(i, ":", names(x)[i], "\n")
      what <- scan(what = numeric(), n = 1)
    }
    copyToClipboard(x[[what]])
  }
}

#' @export
copyToClipboard.data.frame <- function(x, ...) {
  if (nrow(x) > 50000) {
    x <- x[1:50000, ]
    warning("Table is too large. Only 50000 rows are copied to clipboard")
  }
  write.table(x, file = textConnection(".txt", "w", local=TRUE), 
              sep="\t", row.names = FALSE, ...)
  utils::writeClipboard(.txt)
}

#' @export
copyToClipboard.matrix <- function(x, ...) {
  if (nrow(x) > 50000) {
    x <- x[1:50000, ]
    warning("Matrix is too large. Only 50000 rows are copied to clipboard")
  }
  write.table(x, file = textConnection(".txt", "w", local=TRUE), 
              sep="\t", row.names = FALSE, col.names = FALSE, ...)
  utils::writeClipboard(.txt)
}

#' @export
copyToClipboard.default <- function(x, ...) {
  copyToClipboard(as.matrix(x), ...)
}
