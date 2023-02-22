#' @title Read digest file
#'
#' @param opts simulation options
#' @param endpoint
#'   Suffix of path for digest file
#'   Default is : "mc-all/grid/digest.txt" added to opts$simDataPath
#' 
#' @importFrom purrr quietly
#'
#' @return list of 5 tables (begin, areas, middle, links lin., links quad.)
#' 
#' @export
readDigestFile <- function(opts, endpoint = "mc-all/grid/digest.txt"){
  
  digestPath <- file.path(opts$simDataPath, endpoint)
  digest <- list()
  readDigestTable <- quietly(fread)
  nextIdx <- 0
  
  for (elmt in c("begin", "areas", "middle", "lin")){
    currTable <- readDigestTable(digestPath, skip = nextIdx, header = T)
    nextIdx <- as.numeric(gsub("Stopped early on line ", "", strsplit(currTable$warnings, "\\.")[[1]][1]))
    digest[[elmt]] <- currTable$result[,V1 := NULL]
  }
  
  #table link quad (last table)
  digest$quad <- readDigestTable(digestPath, skip = nextIdx, header = T)$result[,V1 := NULL]
  digest
}





#' @title Merge two digests
#'
#' @param digest_new new digest with missing lines
#' @param digest_ori original digest with all lines
#' 
#' @return 
#'   updated digest
#'   list of 5 tables (begin, areas, middle, links lin., links quad.)
#' 
#' @seealso
#' \code{\link{readDigestFile}}
#' 
#' @export
mergeDigests <- function(digest_new, digest_ori){
  res <- list()
  
  #begin
  currTable <- copy(digest_new$begin)
  output <- lapply(colnames(currTable), function(x){
    currTable[, eval(x) := max(currTable[, x, with = F], digest_ori$begin[, x, with = F])]})
  res$begin <- currTable
  
  #areas
  res$areas <- rbind(digest_new$areas, digest_ori$areas[!V2 %in% digest_new$areas$area], use.names = F)
  setorder(res$areas, area)
  res$areas[`H. LEV` == 0, c("H. LEV", "H. OVFL", "H. VAL") := "N/A"]
  
  #middle
  currTable <- copy(digest_new$middle)
  output <- lapply(colnames(currTable), function(x){
    currTable[, eval(x) := max(currTable[, x, with = F], digest_ori$middle[, x, with = F])]})
  res$middle <- currTable
  
  #lin
  linNew <- copy(digest_new$lin)
  linOri <- copy(digest_ori$lin)
  missingLinks <- setdiff(colnames(linOri), colnames(linNew))
  linNew[, (missingLinks) := "ORI"]
  linNew <- rbind(linNew, linOri[!`...To` %in% linNew$`...To`])
  setcolorder(linNew, colnames(linOri))
  setorder(linNew, `...To`)
  for (nm in missingLinks){
    idx = which(c(linNew[,nm,with = F])[[1]] %in% "ORI")
    linNew[idx, eval(nm) := linOri[idx, nm, with = F]]
  }
  res$lin <- linNew
  
  #quad
  quadNew <- copy(digest_new$quad)
  quadOri <- copy(digest_ori$quad)
  missingLinks <- setdiff(colnames(quadOri), colnames(quadNew))
  quadNew[, (missingLinks) := "ORI"]
  quadNew <- rbind(quadNew, quadOri[!`...To` %in% quadNew$`...To`])
  setcolorder(quadNew, colnames(quadOri))
  setorder(quadNew, `...To`)
  for (nm in missingLinks){
    idx = which(c(quadNew[,nm, with = F])[[1]] %in% "ORI")
    quadNew[idx, eval(nm) := quadOri[idx, nm, with = F]]
  }
  res$quad <- quadNew
  
  res
}




#' @title Write digest file
#'
#' @param digest list of 5 elements similar to what is returned by \code{\link{readDigestFile}}
#' @param opts simulation options
#' 
#' @return 
#'   updated digest
#'   list of 5 tables (begin, areas, middle, links lin., links quad.)
#'   
#' @seealso
#' \code{\link{readDigestFile}}
#' 
#' @export
writeDigest <- function(digest, opts = simOptions()){
  digestFile <- file.path(opts$simDataPath, "mc-all", "grid", "digest.txt")
  
  # First table
  write("digest", digestFile)
  write.table(digest$begin, digestFile, row.names = F, quote = F, sep = "\t", append = T)
  write("", digestFile, append = T)
  
  ## Digest areas
  write.table(digest$areas, digestFile, row.names = F, quote = F, sep = "\t", append = T)
  write("\n", digestFile, append = T)
  
  ## deuxieme table et retours ligne
  write("digest", digestFile, append = T)
  write.table(digest$middle, digestFile, row.names = F, quote = F, sep = "\t", append = T)
  write(rep("\n",5), digestFile, append = T)
  
  ## Digest links LIN
  write("Links (FLOW LIN.)", digestFile, append = T)
  write( "\tFrom...", digestFile, append = T)
  write.table(digest$lin, digestFile, row.names = F, quote = F, sep = "\t", append = T)
  write(rep("\n",2), digestFile, append = T)
  
  ## Digest links QUAD
  write("Links (FLOW QUAD.)", digestFile, append = T)
  write("\tFrom...", digestFile, append = T)
  write.table(digest$quad, digestFile, row.names = F, quote = F, sep = "\t", append = T)
  
  ## add empty first column
  lines <- readLines(digestFile)
  write(x = paste0("\t",lines[1]), file = digestFile, sep = "\t")
  for (line in lines[-1]){
    write(x = paste0("\t",line), file = digestFile, sep = "\t", append = T)
  }
}