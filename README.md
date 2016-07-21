[ddiR](https://github.com/BD2K-DDI/ddiR)
======

An [R package](https://github.com/BD2K-DDI/ddiR) to obtain data from the Omics Discovery Index ([OmicsDI](http://www.omicsdi.org). It uses its RESTful Web Services at [OmicsDI WS](http://www.omicsdi.org/ws/) for that purpose.  

Currently, the following domain entities are supported:  

* Dataset as S4 objects, including methods to get them from OmicsDI by accession and `as.data.frame`  
* Publication as S4 objects, including methods to get them from OmicsDI by accession and `as.data.frame`  
* Term as S4 objects, including methods to get them from OmicsDI by term and `as.data.frame`  

### Installation  

First, we need to install `devtools`:  

    install.packages("devtools")
    library(devtools)
   
Then we just call  

    install_github("BD2K-DDI/ddiR")
    library(prideR)

### Examples     

Retrieve all the metadata similarity scores from OmicsDI:

```R
library(ddiR)
datasets <- search.DatasetsSummary(query = "*:*")
sink("outfile.txt")
for(datasetCount in seq(from = 0, to = datasets@count, by = 100)){
    datasets <- search.DatasetsSummary(query = "*:*", start = datasetCount, size = 100)
    for(dataset in datasets@datasets){
        DatasetDetail = get.DatasetDetail(accession = dataset@dataset.id, database = dataset@database)
        Similar = get.MetadataSimilars(accession = dataset@dataset.id, database = dataset@database)
        rank = 0
        for(similarDataset in Similar@datasets){
            print(paste(dataset@dataset.id, similarDataset@dataset.id, similarDataset@score, dataset@omics.type, rank))
            rank = rank + 1
        }
    }
}
sink()
```

### About us   

Find out about us in our GitHub profiles:  

[Yasset Perez-Riverol](https://github.com/ypriverol)  
[Ariana Barbera Betancourt](http://github.com/abb44)
