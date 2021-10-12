# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
#SCORER::run_app()
library(dplyr)
ACM <- SCORER::ACM
form <- as.formula(paste(paste(c("RunningCost","InvestmentCost","BufferCapacity"), collapse="+"),
                         " ~ ",
                         paste(ACM$inputs,collapse = "+"),collapse=" "))

rpart_fit <- rpart::rpart(form,
                          ACM,
                          control = rpart::rpart.control(cp=0.05, maxdepth = 4))

tree_party <- partykit::as.party(rpart_fit)
tree_pred <- predict(tree_party, type = "node")
clust <- as.numeric(forcats::fct_recode(factor(tree_pred),
                                        !!!as.list(setNames(as.character(unique(tree_pred)),
                                                            seq_len(length(unique(tree_pred)))))))
# Assign the clusters back to the dataset
ACM <- ACM %>% mutate(Cluster = clust)

#options(shiny.error = browser)

SCORER::run_app(dataset=ACM)
