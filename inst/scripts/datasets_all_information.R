library(ddiR)
library(ggplot2)



load(file="inst/data/datasets-list.RData")

resultDatasetFrame <- data.frame(ID  = character(),
                                 Database  = character(), 
                                 omicsType = character(), 
                                 Taxonomy  = character(),
                                 Organism  = character(),
                                 Disease   = character(),
                                 Tissue    = character(),
                                 Cell      = character(),
                                 Instrument = character(),
                                 stringsAsFactors=FALSE)
colnames(resultDatasetFrame) <- c("ID", "Database", 
                                  "omicsType", "Taxonomy", "Organism",
                                  "Disease", "Tissue",  "Instrument")
for(datIndex in 1:length(datasetList)){
  
  currentDataset <- datasetList[[datIndex]];
  
  if(!is.null(currentDataset)){
    if(currentDataset@dataset.id == "E-TABM-185"){
      print(currentDataset)
    }
    omicsDF <- data.frame(ID = character(), Database = character(), Taxonomy = character(), Organism = character(), stringsAsFactors=FALSE);
    colnames(omicsDF) <- c("ID", "Database","Taxonomy", "Organism")
    if(is.null(currentDataset@organisms) || currentDataset@organisms == "Not available"){
      omicsDF[nrow(omicsDF)+1, ] <- c(currentDataset@dataset.id, currentDataset@database, "NA", "NA"); 
    }else{
      for(index in 1: length(currentDataset@organisms)){
        currentOrganism <- currentDataset@organisms[[index]];
        omicsDF[nrow(omicsDF)+1, ] <- c(as.character(currentDataset@dataset.id), as.character(currentDataset@database), as.character(currentOrganism@accession), as.character(currentOrganism@name));
      }
    } 
    #print(omicsDF)
    
    instrumentDF <- data.frame(ID = character(), Database = character(), Instrument = character(), stringsAsFactors=FALSE);
    colnames(instrumentDF) <- c("ID", "Database","Instrument")
    if(is.null(currentDataset@instruments) || currentDataset@instruments == "Not available"){
      instrumentDF[nrow( instrumentDF)+1, ] <- c(currentDataset@dataset.id, currentDataset@database, "NA"); 
    }else{
      for(index in 1: length(currentDataset@instruments)){
        currentInstrument <- currentDataset@instruments[[index]];
        instrumentDF[nrow(instrumentDF)+1, ] <- c(as.character(currentDataset@dataset.id), as.character(currentDataset@database), as.character(currentInstrument));
      }
    } 
    #print(instrumentDF)
    df <- merge(omicsDF, instrumentDF, by = c("ID", "Database"));
    
    tissuesDF <- data.frame(ID = character(), Database = character(), Tissue = character(), stringsAsFactors=FALSE);
    colnames(tissuesDF) <- c("ID", "Database","Tissue")
    if(is.null(currentDataset@tissues) || currentDataset@tissues == "Not available"){
      tissuesDF[nrow(tissuesDF)+1, ] <- c(currentDataset@dataset.id, currentDataset@database, "NA"); 
    }else{
      for(index in 1: length(currentDataset@tissues)){
        currentTissue <- currentDataset@tissues[[index]];
        tissuesDF[nrow(tissuesDF)+1, ] <- c(as.character(currentDataset@dataset.id), as.character(currentDataset@database), as.character(currentTissue));
      }
    }
    df <- merge(df, tissuesDF, by = c("ID", "Database"));
    
    diseaseDF <- data.frame(ID = character(), Database = character(), Disease = character(), stringsAsFactors=FALSE);
    colnames(tissuesDF) <- c("ID", "Database","Disease")
    if(is.null(currentDataset@diseases) || currentDataset@diseases == "Not available"){
      diseaseDF[nrow(diseaseDF)+1, ] <- c(currentDataset@dataset.id, currentDataset@database, "NA"); 
    }else{
      for(index in 1: length(currentDataset@diseases)){
        currentDisease <- currentDataset@tissues[[index]];
        diseaseDF[nrow(diseaseDF)+1, ] <- c(as.character(currentDataset@dataset.id), as.character(currentDataset@database), as.character(currentDisease));
      }
    }
    df <- merge(df, diseaseDF, by = c("ID", "Database"));
    
    print(df) 
  }
  
  
  
}

