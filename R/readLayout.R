#' Read nodes layout from the HMI
#'
#' This function imports the current node layout from the human machine interface (HMI).
#' It is useful for plotting the network.
#'
#' @inheritParams readAntares
#'
#' @return A list with two elements
#' \itemize{
#'    \item{nodes: }{A data.frame containing the name, the color and the coordinate
#'                 of the nodes}
#'    \item{links: }{A data.frame containing the name, the coordinates of the origin
#'                 and the destination of the link}
#' }
#'
#' @examples
#' readLayout()
#'
#' @export
#'
readLayout <- function(opts = getOption("antares")) {
  
  # nodes
  path <- file.path(opts$path, "../../input/areas")
  nodes <- ldply(list.files(path), function(f) {
    if (!dir.exists(file.path(path, f))) return(NULL)

    res <- as.data.frame(readIniFile(file.path(path, f, "ui.ini"))$ui)
    res$node <- f
    res$color <- rgb(res$color_r, res$color_g, res$color_b, maxColorValue = 255)

    res[, c("node", "x", "y", "color")]
  })

  # liens
  path <- file.path(opts$path, "../../input/links")
  links <- ldply(list.files(path), function(f) {
    if (!dir.exists(file.path(path, f))) return(NULL)
    to <- list.files(file.path(path, f))
    to <- to[to != "properties.ini"]
    to <- gsub(".txt", "", to)

    if (length(to) == 0) return(NULL)

    data.frame(from = f, to = to)
  })

  links <- merge(links, nodes[,c("node", "x", "y")], by.x = "from", by.y="node")
  links <- merge(links, nodes[,c("node", "x", "y")], by.x = "to", by.y="node",
                 suffixes = c("0", "1"))

  list(nodes = nodes, links=links)
}

# Fonction pour vérifier visuellement qu'importLayout fonctionne
plotLayout <- function(layout) {
  if (missing(layout)) layout <- importLayout()
  plot(layout$nodes$x, layout$nodes$y, type="n",xaxt='n',yaxt='n',pch='',ylab='',xlab='')

  with(layout$links, segments(x0, y0, x1, y1, col = gray(0.85)))

  with(layout$nodes, points(x, y, pch=21, col=gray(0.6), bg = color, cex = 2))
}
