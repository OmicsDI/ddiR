[![Travis build status](https://travis-ci.org/enriquea/ddiR.svg?branch=master)](https://travis-ci.org/enriquea/ddiR)

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

    install_github("enriquea/ddiR")
    library(ddiR)

### Examples

- This example retrives all dataset details given accession and database identifier

```R

library(ddiR)

dataset = get.DatasetDetail(accession="PXD000210", database="pride")

# print dataset full name
get.dataset.name(dataset)

# print dataset omics type
get.dataset.omics(dataset)

```

- Access to all datasets for NOTCH1 gene

```R

library(ddiR)

datasets <- search.DatasetsSummary(query = "NOTCH1")

sink("outfile.txt")
for(datasetCount in seq(from = 0, to = datasets@count, by = 100)){

    datasets <- search.DatasetsSummary(query = "NOTCH1", start = datasetCount, size = 100)

    for(dataset in datasets@datasets){
             dataset = get.DatasetDetail(accession=dataset.id(dataset), database=database(dataset))
             print(paste(dataset.id(dataset), get.dataset.omics(dataset), get.dataset.link(dataset)))
            }
    }
}
sink()

```

- This exmaple shows how retrieve all the metadata similarity scores by using the R-package ddiR. 

```R

library(ddiR)
datasets <- search.DatasetsSummary(query = "*:*")
i  = 0
sink("outfile.txt")
for(datasetCount in seq(from = 0, to = datasets@count, by = 100)){

    datasets <- search.DatasetsSummary(query = "*:*", start = datasetCount, size = 100)

    for(dataset in datasets@datasets){
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

# Maintainers

[Yasset Perez-Riverol](https://github.com/ypriverol)   
[Ariana Barbera Betancourt](http://github.com/abb44)   
[Enrique Audain](https://github.com/enriquea)

