require(antaresRead)

# Q : modalités de  opts$parameters$`other preferences`$`renewable-generation-modelling`

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", "input")
  
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-1", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-2", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-3", "input")

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-ts-prod-factor-cluster-disabled", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-2-clusters-ts-prod-factor", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-1-cluster-ts-prod-factor", "input")

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\test_study_bug_aggregate")

# 2.1.1 : 2.1.1	Lecture du paramètre « Renewable generation modelling »
# déjà ok en mode input & output
opts$parameters$`other preferences`$`renewable-generation-modelling`

# 2.1.2	: Lecture des paramètres intra-modal et inter-modal pour la catégorie « Renewables » 
# Q : pas de cas dans les études tests à priori
opts$parameters$general$`inter-modal`
opts$parameters$general$`intra-modal`

# 2.1.3 : readAntares / output

# 2.1.3.2	Lecture des résultats généraux : immédiat
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", 1)
readAntares(areas  = "all", clusters = "all", clustersRes = "all", mcYears = 1)

# RQ : il faut faire tourner une étude, ici renewables-2 pour avoir des résultats !
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-2", 1)
readAntares(areas  = "all", cluster = "all", clustersRes = "all", mcYears = 1)
readAntares(areas  = "all", mcYears = 1, select = c("RENW. 4", "H. STOR"))

# Q : nom de la colonne ? resProduction ?

# 2.1.3.3	Lecture des résultats par cluster ENR
# Q : aggregateResult / mcWeight ?

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", 1)
readAntares(areas  = "all", clusters = "all", clustersRes = "all", mcYears = 1:2, mcWeights = c(0.5, 0.5))

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-2", 1)
readAntares(areas  = "all", clustersRes = "all", clusters = "all", mcYears = 1, mcWeights = 0.5)

# Q : ADQPatch ? Actuellement pas gérer quand on écrit les fichiers

# 2.1.4	Lecture des paramètres des clusters ENR

?readClusterDesc

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", 1)
opts$areasWithClusters
opts$areasWithResClusters

readClusterDesc(opts)
readClusterResDesc(opts)

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-2", 1)
opts$areasWithClusters
opts$areasWithResClusters

readClusterDesc(opts)
readClusterResDesc(opts)

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-1", "input")
opts$areasWithClusters
opts$areasWithResClusters

readClusterDesc(opts)
readClusterResDesc(opts)

# 2.1.5	Lecture des paramètres de modulation des clusters ENR
# Q : on attend

# 2.1.6	Lecture des Time-Series des clusters ENR
# Q : nom de la colonne resProduction ?
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", 1)
d <- readInputTS(thermalAvailabilities = "all", resProduction = "all", load = "all")
d

d <- readInputTS(thermalAvailabilities = "all", load = "all")
d

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-ts-prod-factor-cluster-disabled", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-2-clusters-ts-prod-factor", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewable-1-cluster-ts-prod-factor", "input")
opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\renewables-1", "input")
d <- readInputTS(thermalAvailabilities = "all", resProduction = "all", load = "all")
d
