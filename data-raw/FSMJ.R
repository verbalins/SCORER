## code to prepare `FSMJ` dataset goes here
inputs <- c("Shift","safety_A1","safety_A2","safety_A3","safety_A4",
              "safety_B1","safety_B2","safety_B3","safety_B4","safety_B5","safety_B6","safety_B7",
              "safety_C1","safety_C2","safety_C3","safety_C4",
              "safety_D1","safety_D2","safety_D3","safety_D4",
              "batch_C1","batch_C2","batch_C3","batch_C4")
outputs <- c("WaitingParts","WIP_A","WIP_B","WIP_C","WIP_D","WIP_AA","WIP_FA","LT_A","LT_B","LT_C","LT_D","LT_AA","LT_FA","Out","LT_Plant","LeanBuffer")
FSMJ <- SCORER::loaddataset("inst/extdata/FSMJ_data.csv", inputs = inputs, outputs = outputs)

usethis::use_data(FSMJ, compress="xz", overwrite = TRUE)

FSMJ_dist <- FSMJ %>%
  addDistances(parallelCores = 10)

usethis::use_data(FSMJ_dist, compress="xz", overwrite = TRUE)

FSMJ_clusters <- FSMJ_dist %>%
  partitioning(nrClusters = 6, parameters = c("Distance",names(attr(.,"objectives"))))

usethis::use_data(FSMJ_clusters, compress="xz", overwrite = TRUE)


inputs <- c("B1","B2","S1","A1","A2","A3","A4","A5","S2")

FMC <- SCORER::loaddataset("inst/extdata/WSC11.csv", inputs = inputs, objectives = c("Investment", "TP"))

usethis::use_data(FMC, compress="xz", overwrite = TRUE)
