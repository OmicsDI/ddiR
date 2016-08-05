library(ddiR)
library(ggplot2)
library(ggfortify)


count <- length(datasetList) * 100
resultDatasetFrameSimilars <- data.frame(Idenfier   = character(count),
                                         Database   = character(count),
                                         SimilarID  = character(count),
                                         SimilarDB  = character(count),
                                         score      = character(count),
                                         stringsAsFactors=FALSE)
colnames(resultDatasetFrameSimilars) <- c("Identifier", "Database", "SimilarID", "SimilarDB", "score")


count <- 0
for(datIndex in 1:length(datasetList)){
    currentDataset <- datasetList[[datIndex]]
    if(!is.null(currentDataset) && (currentDataset@database != "arrayexpress-repository")){
        DatasetSimilars <- get.BiologicalSimilars(currentDataset@dataset.id, currentDataset@database)
        if(is.null(DatasetSimilars@similars) || length(DatasetSimilars@similars) == 0){
            resultDatasetFrameSimilars[count+1, ] <- c(DatasetSimilars@dataset.id, DatasetSimilars@database,
                                                       "NA", "NA", "NA")
            count <- count + 1

        }else{
            for( similarIndex in 1: length(DatasetSimilars@similars)){
                currentSimilar = DatasetSimilars@similars[[similarIndex]]
                resultDatasetFrameSimilars[count+1, ] <- c(DatasetSimilars@dataset.id, DatasetSimilars@database, currentSimilar@dataset.id, currentSimilar@database, currentSimilar@score)
                count <- count + 1

            }
        }
        print(paste(currentDataset@dataset.id, currentDataset@database, length(DatasetSimilars@similars), sep = " "))
    }
}

resultDatasetFrameSimilarsWithoutNULL <- resultDatasetFrameSimilars[1:count,]

save(resultDatasetFrameSimilarsWithoutNULL, file = "resultBiologicalSimilars.RData")

# metadata similars

count <- length(datasetList) * 100
resultDatasetFrameMetaSimilars <- data.frame(Idenfier   = character(count),
                                         Database   = character(count),
                                         SimilarID  = character(count),
                                         SimilarDB  = character(count),
                                         score      = character(count),
                                         stringsAsFactors=FALSE)
colnames(resultDatasetFrameMetaSimilars) <- c("Identifier", "Database", "SimilarID", "SimilarDB", "score")


count <- 0
for(datIndex in 74002:length(datasetList)){
    currentDataset <- datasetList[[datIndex]]
    if(!is.null(currentDataset) && (currentDataset@database != "arrayexpress-repository") && (currentDataset@database != "EGA")){
        DatasetSimilars <- get.MetadataSimilars(currentDataset@dataset.id, currentDataset@database)
        if(is.null(DatasetSimilars@similars) || length(DatasetSimilars@similars) == 0){
            resultDatasetFrameMetaSimilars[count+1, ] <- c(DatasetSimilars@dataset.id, DatasetSimilars@database,
                                                       "NA", "NA", "NA")
            count <- count + 1

        }else{
            for( similarIndex in 1: length(DatasetSimilars@similars)){
                currentSimilar = DatasetSimilars@similars[[similarIndex]]
                resultDatasetFrameMetaSimilars[count+1, ] <- c(DatasetSimilars@dataset.id, DatasetSimilars@database, currentSimilar@dataset.id, currentSimilar@database, currentSimilar@score)
                count <- count + 1

            }
        }
        print(paste(currentDataset@dataset.id, currentDataset@database, length(DatasetSimilars@similars), sep = " "))
    }
}

resultDatasetFrameMetaSimilarsWithoutNULL <- resultDatasetFrameMetaSimilars[1:count,]

save(resultDatasetFrameMetaSimilarsWithoutNULL, file = "resultMetadataSimilars.RData")

totalSimilars <- merge(resultDatasetFrameSimilarsWithoutNULL,resultDatasetFrameMetaSimilarsWithoutNULL,by=c("Identifier","Database", "SimilarID", "SimilarDB"))

#Add similar Type
#

totalSimilars$score.x <- as.numeric(totalSimilars$score.x)
totalSimilars$score.y <- as.numeric(totalSimilars$score.y)

totalSimilars$OmicsType <- ifelse(totalSimilars$Database == "atlas-experiments", "Transcriptomics", totalSimilars$OmicsType)
totalSimilars$OmicsType <- ifelse(totalSimilars$Database == "metabolomics_workbench", "Metabolomics", totalSimilars$OmicsType)
totalSimilars$OmicsType <- ifelse(totalSimilars$Database == "metabolights_dataset", "Metabolomics", totalSimilars$OmicsType)
totalSimilars$OmicsType <- ifelse(totalSimilars$Database == "pride", "Proteomics", totalSimilars$OmicsType)
totalSimilars$OmicsType <- ifelse(totalSimilars$Database == "metabolome_express", "Metabolomics", totalSimilars$OmicsType)
RText <- paste("R = ", round(cor(totalSimilars$score.x, totalSimilars$score.y), digits = 3))

correlationSimilars <- ggplot(totalSimilars, aes(score.x, score.y))
correlationSimilars <- correlationSimilars + geom_point(aes(colour = factor(OmicsType)), alpha = 1/3)  + geom_smooth(method=lm) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(title = "Correlation between the Metadata Score and shared molecules Score", y = "Metadata Similarity Score",  x= "Shared Molecules Similarity Score") + theme_bw()  + theme(plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank()) +theme(axis.line.x = element_line(color="black", size = 1/4), axis.line.y = element_line(color="black", size = 1/4), legend.key = element_blank()) + scale_fill_discrete("") + scale_colour_discrete("") + annotate(geom = "text", x=1, y=20, label=RText, color = "black", size=4, fontface="bold.italic")

ggsave(correlationSimilars, file = "inst/imgs/simlars-correlation.png", width=10, height=5)

totalSimilars50 <- subset(totalSimilars, totalSimilars$score.x > 0.5)
totalSimilars50M <- subset(totalSimilars50, totalSimilars50$OmicsType == "Metabolomics")
totalSimilars50P <- subset(totalSimilars50, totalSimilars50$OmicsType == "Proteomics")
totalSimilars50T <- subset(totalSimilars50, totalSimilars50$OmicsType == "Transcriptomics")


RText50 <- paste("R = ", round(cor(totalSimilars50$score.x, totalSimilars50$score.y), digits = 2))
RText50M <- paste("R(M) = ", round(cor(totalSimilars50M$score.x, totalSimilars50M$score.y), digits = 2))
RText50P <- paste("R(P) = ", round(cor(totalSimilars50P$score.x, totalSimilars50P$score.y), digits = 2))
RText50T <- paste("R(T) = ", round(cor(totalSimilars50T$score.x, totalSimilars50T$score.y), digits = 2))

correlationSimilars50 <- ggplot(totalSimilars50, aes(score.x, score.y))
correlationSimilars50 <- correlationSimilars50 + geom_point(aes(colour = factor(OmicsType)), alpha = 1/3)  + geom_smooth(method=lm) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(title = "Correlation between the Metadata Score and shared molecules score", y = "Metadata Similarity score",  x= "Shared Molecules similarity score") + theme_bw()  + theme(plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank()) +theme(axis.line.x = element_line(color="black", size = 1/4), axis.line.y = element_line(color="black", size = 1/4), legend.key = element_blank()) + scale_fill_discrete("") + scale_colour_discrete("") + annotate(geom = "text", x=1, y=20, label=RText50, color = "black", size=3, fontface="bold.italic", hjust = 1) + annotate(geom = "text", x=1, y=18, label=RText50M, color = "black", size=3, fontface="bold.italic", hjust = 1) + annotate(geom = "text", x=1, y=16, label=RText50P, color = "black", size=3, fontface="bold.italic", hjust = 1) + annotate(geom = "text", x=1, y=14, label=RText50T, color = "black", size=3, fontface="bold.italic", hjust = 1) + theme(legend.position = "bottom") +
    geom_text( data = totalSimilars50[totalSimilars50$Identifier == "PXD000637" ,], aes(score.x,score.y, label = Identifier), vjust=0, hjust=-0.1, size = 2.5)  +
    geom_text( data = totalSimilars50[totalSimilars50$Identifier == "ST000189" ,], aes(score.x,score.y, label = Identifier), vjust=0, hjust=1, size = 2.5)  +
    geom_text( data = totalSimilars50[totalSimilars50$Identifier == "E-MTAB-3839" ,], aes(score.x,score.y, label = Identifier), vjust=0, hjust=-0.1, size = 2.5)



ggsave(correlationSimilars50, file = "inst/imgs/simlars-correlation-50.png", width=8, height=8)

