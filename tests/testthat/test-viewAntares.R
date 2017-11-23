context("viewAntares")

sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

# describe("viewAntares", {
#   with_mock(
#     `utils::View` = function(x, title) {print(title)},
#     {
#       it("views an antaresDataTable", {
#         data <- readAntares(timeStep = "annual", select = "LOAD", showProgress = FALSE)
#         expect_output(viewAntares(data), "data")
#       })
#       
#       it("views an antaresDataList", {
#         data <- readAntares("all", "all", timeStep = "annual", 
#                             select = c("LOAD", "FLOW LIN."), showProgress = FALSE)
#         expect_output(viewAntares(data), "data\\$areas")
#         expect_output(viewAntares(data), "data\\$links")
#       })
#     }
#   )
#   
#   with_mock(
#     `utils::View` = function(x, title) {return(NULL)},
#     {
#       it("displays a warning if there is more than 100 columns", {
#         data <- readAntares(timeStep = "annual", showProgress = FALSE)
#         expect_warning(viewAntares(data), "100 columns")
#       })
#     }
#   )
# })
})
if(requireNamespace("rhdf5")){
  rhdf5::H5close()
}
if(dir.exists(tpDir))
{
  unlink(tpDir, recursive = TRUE)
}
