# This script search for all datasets and plot them by repository. 
library(ddiR)

datasets <- search.DatasetsSummary(query = "*:*")
d = NULL
for(datasetCount in seq(from = 0, to = datasets@count, by = 100)){
  datasets <- search.DatasetsSummary(query = "*:*", start = datasetCount, size = 100)
  for(dataset in datasets@datasets){
    DatasetDetail = get.DatasetDetail(accession = dataset@dataset.id, database = dataset@database)
    if(!is.null(DatasetDetail)){
      d = rbind(d, data.frame(DatasetDetail@dataset.id, DatasetDetail@database))
    }
  }
}


