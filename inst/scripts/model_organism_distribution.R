library(ddiR)
library(myTAI)
library(ggplot2)
library(reshape)

model_organism <- read.table(file="inst/data/model_organism.tsv", sep = "\t", header = TRUE)


load(file="inst/data/datasets-list.RData")

resultDatasetFrame <- data.frame(Idenfier  = character(),
                                 Database  = character(), 
                                 omicsType = character(), 
                                 Taxonomy  = character(), 
                                 Organism  = character(), 
                                 Model     = character(), 
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

  
for(datIndex in 1:length(datasetList)){
  currentDataset <- datasetList[[datIndex]]
  if(!is.null(currentDataset)){
    if(is.null(currentDataset@organisms) || currentDataset@organisms == "Not available"){
      for(omicsIndex in 1: length(currentDataset@omicsType)){
        resultDatasetFrame <- rbind(resultDatasetFrame, c(currentDataset@dataset.id,
                                                            currentDataset@database,
                                                            currentDataset@omicsType[[omicsIndex]],
                                                            "NA",
                                                            "NA",
                                                            "NA"))
      }
    }else{
      for(taxonomyId in 1:length(currentDataset@organisms)){
        currentTaxonomy <- currentDataset@organisms[[taxonomyId]]
        if(!is.null(currentTaxonomy) && !is.null(currentTaxonomy@accession) && (nrow(modelOrganismFrame[grep(as.character(currentTaxonomy@accession),modelOrganismFrame['childtaxa_id']),]) > 0)){
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            
            resultDatasetFrame[nrow(resultDatasetFrame)+1,] <- c(currentDataset@dataset.id,
                                    currentDataset@database,
                                    currentDataset@omicsType[[omicsIndex]],
                                    currentTaxonomy@accession,
                                    currentTaxonomy@name,
                                    "Model Organism");
          }   
        }else if(is.null(currentTaxonomy) || is.null(currentTaxonomy@accession)){
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            
            resultDatasetFrame[nrow(resultDatasetFrame)+1,] <- c(currentDataset@dataset.id,
                                                                 currentDataset@database,
                                                                 currentDataset@omicsType[[omicsIndex]],
                                                                 "NA",
                                                                 currentTaxonomy@name,
                                                                 "NA");
          }
        }else{
          for(omicsIndex in 1: length(currentDataset@omicsType)){
            resultDatasetFrame[nrow(resultDatasetFrame)+1,] <- c(currentDataset@dataset.id,
                                                                 currentDataset@database,
                                                                 currentDataset@omicsType[[omicsIndex]],
                                                                 currentTaxonomy@accession, 
                                                                 currentTaxonomy@name,
                                                                 "Non Model Organism")
          }
        }
    }
    }
    print(currentDataset@dataset.id)
  }
}

#Plot of database by Model Organism

database <- as.vector(resultDatasetFrame$Database)
type <- as.vector(resultDatasetFrame$`Model Organism`)

to_plot <- data.frame(database=database,type=type)


modelPlot <- ggplot(aes(database, fill=type), data=to_plot) + 
  geom_bar(alpha=.5, position = "dodge")+ coord_flip()  +
  scale_y_sqrt(breaks = c(100, 1000, 2000, 4000, 10000, 20000, 30000, 65000)) + 
  labs(title = "Number of Datasests by Respoitory and Model Organism Category", y = "Number of Datasests (sqrt scale)",  x= "Repositories/Databases") +
  scale_fill_discrete(guide = guide_legend(NULL), labels = c("Model Organism", "Not Annotated", "Non Model Organism")) + 
  scale_x_discrete(labels = c("ArrayExpress", "ExpressionAtlas", "EGA", "GNPS", "GPMDB", "MassIVE", "Metabolights", "MetabolomeExpress", "MetabolomicsWorkbench", "PeptideAtlas", "PRIDE")) +
  theme(panel.background = element_blank())


png(file = "inst/imgs/model-organism-plot.png", width = 800, height = 600)
plot(modelPlot)
dev.off()

#Plot of omicsType by Model Organism

omicsType <- as.vector(resultDatasetFrame$omicsType)
omicsType <- gsub("transcriptomics", "Transcriptomics", omicsType)
typeModel <- as.vector(resultDatasetFrame$`Model Organism`)

omicsTypeToPlot <- data.frame(database=omicsType,type=typeModel)


omicsTypeModelPlot <- ggplot(aes(database, fill=type), data=omicsTypeToPlot) + 
  geom_bar(alpha=.5, position = "dodge")+ coord_flip()  +
  scale_y_sqrt(breaks = c(100, 1000, 2000, 4000, 10000, 20000, 30000, 65000)) + 
  labs(title = "Number of Datasests by OmicsType and Model Organism Category", y = "Number of Datasests (sqrt scale)",  x= "Omics Type") +
  scale_fill_discrete(guide = guide_legend(NULL), labels = c("Model Organism", "Not Annotated", "Non Model Organism")) + 
  scale_x_discrete(labels = c("Genomics", "Metabolomics", "Not Annotated", "Proteomics", "Transcriptomics")) +
  theme(panel.background = element_blank())


png(file = "inst/imgs/model-organism-omicsType-plot.png", width = 800, height = 600)
plot(omicsTypeModelPlot)
dev.off()

#Plot of omicsType by Model Organism
