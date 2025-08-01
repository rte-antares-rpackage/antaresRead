

# v920 ----
# st-storage ----

# build structure directory
# input/st-storage/clusters/list.ini
# input/st-storage/series/{area}/{cluster}/{ts.txt}

# input/st-storage/constraints/{area}/{cluster}/additional-constraints.ini
# input/st-storage/constraints/{area}/{cluster}/rhs_{constaints}.txt

# mock study
areas <- c("fr", "be")
opts <- list(
  "studyPath" = tempdir(),
  "inputPath" = file.path(tempdir(), "input"),
  "typeLoad"= "txt",
  "areaList" = areas,
  "antaresVersion" = 920
)

class(opts) <- c("simOptions")

# properties
fr <- c(
  "[withdrawal-1]",
  "variable = withdrawal",
  "operator = equal",
  "hours = [1,3,5], [120,121,122,123,124,125,126,127,128]",
  "",
  "[withdrawal-2]",
  "variable = withdrawal",
  "operator = equal",
  "hours = [1,3,5], [120,121,122,123,124]"
  )

be <- c(
  "[netting-1]",
  "variable = netting",
  "operator = less",
  "hours = [1, 168]",
  "",
  "[netting-2]",
  "variable = netting",
  "operator = less",
  "hours = [1, 167,168,169]"
  )

list_properties <- list(fr, be)

# TS (8760, 1 column)
TS_VALUE <- matrix(2, 8760)

# create dir with properties
cluster_names <- c("storage_1", "storage_2")
comb <- merge(areas, cluster_names)

dir_path <- file.path(tempdir(),
                      "input",
                      "st-storage",
                      "constraints",
                      areas,
                      paste0(areas, "_", cluster_names))

# dir_path <- file.path(tempdir(),
#                       "st-storage",
#                       "constraints",
#                       areas,
#                       paste0(comb$x, "_", comb$y))

lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)


# write properties
lapply(seq(1, length(list_properties)), function(x){
  writeLines(list_properties[[x]],
             file.path(dir_path[x], "additional-constraints.ini"))
})

# write TS
names_constraints_1 <- c("withdrawal-1", "withdrawal-2")
names_constraints_2 <- c("netting-1", "netting-2")

lapply(names_constraints_1, function(x){
  write.table(TS_VALUE,
              file = file.path(dir_path[1],
                               paste0("rhs_",
                                      x,
                                      ".txt")),
              row.names = FALSE,
              col.names = FALSE)
})

lapply(names_constraints_2, function(x){
  write.table(TS_VALUE,
              file = file.path(dir_path[2],
                               paste0("rhs_",
                                      x,
                                      ".txt")),
              row.names = FALSE,
              col.names = FALSE)
})


test_that("Read without dir 'constraints'",{

  # when
  read_storages_constraints(opts = opts)
})



