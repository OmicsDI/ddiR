# This script search for all datasets and plot them by repository. 
library(ddiR)
library(ggplot2)

datasets <- search.DatasetsSummary(query = "*:*")
d = NULL
index <- 1
datasetList <- vector("list", datasets@count)
for(datasetCount in seq(from = 0, to = datasets@count, by = 100)){
  datasets <- search.DatasetsSummary(query = "*:*", start = datasetCount, size = 100)
  for(dataset in datasets@datasets){
    DatasetDetail = get.DatasetDetail(accession = dataset@dataset.id, database = dataset@database)
    if(!is.null(DatasetDetail)){
      d = rbind(d, data.frame(DatasetDetail@dataset.id, DatasetDetail@database))
      datasetList[[index]] <- DatasetDetail
      index <- index + 1
    }
  }
}

save(d, file="inst/data/datasets-d.RData")
save(datasetList, file="inst/data/datasets-list.RData")

plot <- ggplot(d, aes(factor(d$DatasetDetail.database), fill = factor(d$DatasetDetail.database))) +
  geom_bar(alpha = .5) + scale_y_sqrt(breaks = c(100, 1000, 4000, 10000, 20000, 40000, 65000)) + labs(title = "Number of Omics Datasests by Respoitory", x = "Repositories/Databases", y = "Number of Datasests (sqrt scale)") +
  scale_fill_discrete(guide = guide_legend(NULL), labels = c("ArrayExpress", "ExpressionAtlas", "EGA", "GNPS", "GPMDB", "MassIVE", "Metabolights", "MetabolomeExpress", "MetabolomicsWorkbench", "PeptideAtlas", "PRIDE")) + 
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), panel.background = element_blank())

png(file = "inst/imgs/datasets-by-repository.png", width = 800, height = 600)
plot(plot)
dev.off()




