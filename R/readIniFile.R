#Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function that reads the content of a .ini file and convert it to a
#' list
#'
#' @param file
#' file path
#'
#' @return
#' A list with an element for each section of the .ini file.
#'
#' @noRd
#'
readIniFile <- function(file, stringsAsFactors=FALSE) {
  X <- readLines(file)
  sections <- grep("^\\[.*\\]$", X)
  starts <- sections + 1
  ends <- c(sections[-1] - 1, length(X))
  L <- vector(mode="list", length=length(sections))
  names(L) <- gsub("\\[|\\]", "", X[sections])
  for(i in seq(along = sections)){
    if (starts[i] >= ends[i]) next
    pairs <- X[seq(starts[i], ends[i])]
    pairs <- pairs[pairs != ""]
    pairs <- strsplit(pairs, "=")

    key <- sapply(pairs, function(p) gsub("^ +| +$","", p[1]))
    value <- lapply(pairs, function(p) {
      v <- gsub("^ +| +$","", p[2])
      if (v == "true") return(TRUE)
      if (v == "false") return(FALSE)
      type.convert(v, as.is = !stringsAsFactors)
    })

    L[[i]] <- value
    names(L[[i]]) <- key
  }
  L
}

# Keep for reference but never used
# writeIniFile <- function(x, file, overwrite = FALSE) {
#   if (!overwrite & file.exists(file)) stop("File already exists.")
#   
#   file <- file(file, open = "w")
#   
#   for (n in names(x)) {
#     cat(paste0("[", n,"]\n"), file = file)
#     for (k in names(x[[n]])) {
#       v <- x[[n]][[k]]
#       if (is.na(v)) v <- ""
#       if (is.logical(v)) v <- ifelse(v, "true", "false")
#       cat(k, " = ", v, "\n", file = file, sep = "")
#     }
#     cat("\n", file = file)
#   }
#   
#   close(file)
# }
