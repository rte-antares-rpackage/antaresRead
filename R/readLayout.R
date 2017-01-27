#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read areas layout
#'
#' @description 
#' This function reads in the input files of an antares study the current areas
#' layout, ie. the position of the areas It may be useful for plotting the
#' network. 
#' 
#' Be aware that the layout is read in the input files so they may have
#' changed since a simulation has been run.
#' 
#' @inheritParams readAntares
#'
#' @return A list with three elements:
#' \item{areas: }{A data.frame containing the name, the color and the coordinate
#'    of each area}
#' \item{district: }{A data.frame containing the name, the color and the coordinate
#'    of each district}
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
  
  # areas
  path <- file.path(opts$inputPath, "areas")
  areas <- ldply(list.files(path), function(f) {
    if (!dir.exists(file.path(path, f))) return(NULL)

    res <- as.data.frame(readIniFile(file.path(path, f, "ui.ini"))$ui)
    res$area <- f
    res$color <- rgb(res$color_r, res$color_g, res$color_b, maxColorValue = 255)

    res[, c("area", "x", "y", "color")]
  })
  areas <- data.table(areas)
  
  # districts
  districts <- merge(areas, opts$districtsDef, by = "area", allow.cartesian=TRUE)
  meanCol <- function(cols) {
    meanrgb <- apply(col2rgb(c("#DF8848", "#DF8848")), 1, mean)
    rgb(meanrgb[1], meanrgb[2], meanrgb[3], maxColorValue = 255)
  }
  districts <- districts[, .(x = mean(x), y = mean(y), color = meanCol(color)), 
                         by = district]

  # links
  if (nrow(opts$linksDef) == 0) {
    links <- NULL
    districtLinks <- NULL
  } else {
    links <- copy(opts$linksDef)
    # Merge with areas two times to add coordinates or origin and destination
    links[areas, `:=`(x0=x, y0=y), on = c(from="area")]
    links[areas, `:=`(x1=x, y1=y), on = c(to="area")]
    
    # Links districts
    
    # Identify the connexions between two districts. If two areas in distincts 
    # districts are connected then the corresponding districts are connected too.
    districtLinks <- merge(links[, .(to, from)], 
                           opts$districtsDef[, .(to=area, toDistrict=district)],
                           by = "to", allow.cartesian=TRUE)
    districtLinks <- merge(districtLinks, 
                           opts$districtsDef[, .(from=area, fromDistrict=district)],
                           by = "from", allow.cartesian=TRUE)
    districtLinks <- unique(districtLinks[fromDistrict != toDistrict,
                                          .(fromDistrict, toDistrict)])
    
    # Add coordinates of origin and destination
    districtLinks <- merge(districtLinks, districts[, .(district, x, y)], 
                           by.x = "toDistrict", by.y = "district")
    districtLinks <- merge(districtLinks, districts[, .(district, x, y)], 
                           by.x = "fromDistrict", by.y = "district", 
                           suffixes = c("0", "1")) 
  }

  list(areas = areas, districts = districts, links=links, districtLinks = districtLinks)
}
