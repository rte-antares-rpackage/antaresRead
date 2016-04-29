#' Read nodes layout
#'
#' @description 
#' This function reads in the input files of an antares study the current nodes
#' layout, ie. the position of the nodes It may be useful for plotting the
#' network. 
#' 
#' Be aware that the layout is read in the input files so they may have
#' changed since a simulation has been run.
#' 
#' @inheritParams readAntares
#'
#' @return A list with two elements:
#' \item{nodes: }{A data.frame containing the name, the color and the coordinate
#'    of each node}
#' \item{links: }{A data.frame containing the name, the coordinates of the origin
#'    and the destination of each link}
#'    
#' By default, \code{readLayout} reads the layout for the current default
#' antares study. It is possible to specify another study with the parameter
#' \code{opts}.
#'
#' @examples
#' \dontrun{
#' readLayout()
#' 
#' # By default, the function reads layout for the default study,
#' # but it is possible to specify another study with parameter "opts"
#' sim1 <- setSimulationPath()
#' 
#' #[... code that modifies the default antares study]
#' 
#' readLayout(sim1)
#'
#' }
#' 
#' @export
#'
readLayout <- function(opts = simOptions()) {
  
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
