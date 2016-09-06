library(ddiR)
library(myTAI)
library(ggplot2)
library(reshape)

model_organism <- read.table(file="inst/data/model_organism.tsv", sep = "\t", header = TRUE)


load(file="inst/data/datasets-list.RData")

count <- length(datasetList) * 100
resultDatasetFrame <- data.frame(Idenfier  = character(count),
                                 Database  = character(count),
                                 omicsType = character(count),
                                 Taxonomy  = character(count),
                                 Organism  = character(count),
                                 Model     = character(count),
                                 stringsAsFactors=FALSE)
colnames(resultDatasetFrame) <- c("Dataset Identifier", "Database", "omicsType", "Taxonomy", "Organism", "Model Organism")

modelOrganismFrame <- data.frame(childtaxa_id = character(),
                                                   childtaxa_name = character(),
                                                   childtaxa_rank = character(),
                                                   parent_id      = character(),
                                                   parent_name    = character(),
                                                   stringsAsFactors=FALSE)
colnames(modelOrganismFrame) <- c("childtaxa_id", "childtaxa_name", "childtaxa_rank", "parent_id", "parent_name")

# Model organism dataframe building algorithm

for(orgIndex in 1:nrow(model_organism)){
  currentName <- model_organism[orgIndex, "name"]
  chield <- taxonomy( organism = currentName, db = "ncbi", output   = "children")
  if(!is.null(chield) && nrow(chield) > 0){
    chield$parent_id <- model_organism[orgIndex, "taxonomyID"]
    chield$parent_name <- model_organism[orgIndex, "name"]
  }
  modelOrganismFrame[nrow(modelOrganismFrame)+1,] <- c(as.character(model_organism[orgIndex, "taxonomyID"]), as.character(model_organism[orgIndex, "name"]), "no rank", as.character(model_organism[orgIndex, "taxonomyID"]), as.character(model_organism[orgIndex, "name"]))
  if(!is.null(chield) && nrow(chield) > 0){
    modelOrganismFrame <- rbind(modelOrganismFrame, chield)
  }
}

count <- 0
for(datIndex in 1:length(datasetList)){
    currentDataset <- datasetList[[datIndex]]
    if(!is.null(currentDataset)){
        if(is.null(currentDataset@organisms) || currentDataset@organisms == "Not available"){
            for(omicsIndex in 1: length(currentDataset@omicsType)){
                resultDatasetFrame[count + 1, ] <- c(currentDataset@dataset.id,
                                                     currentDataset@database,
                                                     currentDataset@omicsType[[omicsIndex]],
                                                     "NA","NA", "NA")
                count <- count + 1;
            }
        }else{
            for(taxonomyId in 1:length(currentDataset@organisms)){
                currentTaxonomy <- currentDataset@organisms[[taxonomyId]]
        if(!is.null(currentTaxonomy) && !is.null(currentTaxonomy@accession) && currentTaxonomy@accession != "9606" && (nrow(modelOrganismFrame[grep(as.character(currentTaxonomy@accession),modelOrganismFrame['childtaxa_id']),]) > 0)){
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            resultDatasetFrame[count + 1,] <- c(currentDataset@dataset.id,
                                    currentDataset@database,
                                    currentDataset@omicsType[[omicsIndex]],
                                    currentTaxonomy@accession,
                                    currentTaxonomy@name,
                                    "Model Organism");
            count <- count + 1;
          }
        }else if(!is.null(currentTaxonomy) && !is.null(currentTaxonomy@accession) && currentTaxonomy@accession == "9606"){
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            resultDatasetFrame[count+1,] <- c(currentDataset@dataset.id,
                                                                 currentDataset@database,
                                                                 currentDataset@omicsType[[omicsIndex]],
                                                                 currentTaxonomy@accession,
                                                                 currentTaxonomy@name,
                                                                 "Human");
            count <- count + 1;
          }
        }else if(is.null(currentTaxonomy) || is.null(currentTaxonomy@accession)){
          for(omicsIndex in 1: length(currentDataset@omicsType)){

            resultDatasetFrame[count+1,] <- c(currentDataset@dataset.id,
                                                                 currentDataset@database,
                                                                 currentDataset@omicsType[[omicsIndex]],
                                                                 "NA",
                                                                 currentTaxonomy@name,
                                                                 "NA");
            count <- count + 1;
          }
        }else{
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            resultDatasetFrame[count + 1,] <- c(currentDataset@dataset.id,
                                                currentDataset@database,
                                                currentDataset@omicsType[[omicsIndex]],
                                                currentTaxonomy@accession,
                                                currentTaxonomy@name,
                                                "Non Model Organism")
            count <- count + 1;
          }
        }
    }
        }
        print(paste(datIndex, currentDataset@dataset.id, sep=" "))
    }
}

resultDatasetFrameWithoutNULL<-resultDatasetFrame[1:count,]
resultDatasetFrameWithoutNULL <- resultDatasetFrameWithoutNULL[!duplicated(resultDatasetFrameWithoutNULL), ]

#Plot of database by Model Organism

database <- as.vector(resultDatasetFrameWithoutNULL$Database)
type <- as.vector(resultDatasetFrameWithoutNULL$`Model Organism`)
omicsType <- as.vector(resultDatasetFrameWithoutNULL$omicsType)
omicsType <- gsub("transcriptomics", "Transcriptomics", omicsType)

to_plot <- data.frame(database=database,type=type, omicsType = omicsType)

to_plot <- to_plot[to_plot$type != "NA", ]
to_plot <- to_plot[to_plot$omicsType != "Not available", ]


modelPlot <- ggplot(aes(database, fill=type), data=to_plot) +
    geom_bar(alpha=.5, position = "dodge") +
    coord_flip() +
    scale_y_sqrt(breaks = c(10, 200, 1000, 2000, 5000, 10000, 15000, 20000, 30000)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Number of datasets by omics type and model organism category", y = "Number of datasets (sqrt scale)",  x= NULL) +
    scale_fill_discrete(guide = guide_legend(NULL), labels = c("Human", "Model Organism", "Non Model Organism")) +
    scale_x_discrete(labels = c("ArrayExpress", "ExpressionAtlas", "EGA", "GNPS", "MassIVE", "Metabolomics Workbench", "PeptideAtlas", "PRIDE")) + theme(legend.position = "bottom")

modelPlot <- modelPlot + facet_grid(. ~omicsType)

ggsave(modelPlot, file = "inst/imgs/model-organism-plot.png", width=8, height=4)
