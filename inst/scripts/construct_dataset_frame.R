# This script search for all datasets and plot them by repository.
library(ddiR)
library(ggplot2)

resultDatasetFrame <- data.frame(Idenfier  = character(),
                                 Database  = character(),
                                 omicsType = character(),
                                 Taxonomy  = character(),
                                 Organism  = character(),
                                 Disease   = character(),
                                 Tissue    = character(),
                                 Cell      = character(),
                                 Instrument = character(),
                                 stringsAsFactors=FALSE)
colnames(resultDatasetFrame) <- c("Dataset Identifier", "Database",
                                  "omicsType", "Taxonomy", "Organism",
                                  "Disease", "Tissue", "Cell", "Instrument")

load(file="inst/data/datasets-list.RData")

for(datIndex in 1:length(datasetList)){
    currentDataset <- datasetList[[datIndex]]
    omicsDF <- list.to.data.frame(currentDataset@omicsType)
}
