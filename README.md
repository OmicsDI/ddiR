[ddiR](https://github.com/BD2K-DDI/ddiR)
======

An [R package](https://github.com/BD2K-DDI/ddiR) to obtain data from the Omics Discovery Index ([OmicsDI](http://wwwdev.ebi.ac.uk/Tools/ddi/). It uses its RESTful Web Services at [OmicsDI WS](http://wwwdev.ebi.ac.uk/Tools/ddi/ws/) for that purpose.  

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

This exmaple shows how retrieve all the metadata similarity scores by using the R-package ddiR. 

```R

library(ddiR)
datasets <- search.DatasetsSummary(query = "*:*")
i  = 0
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
#### Omics Discovery Index  

Get project `PXD000001` summary:  

    get.ProjectSummary("PXD000001")

Search for at most 20 projects by term `blood`. The results are returned as a `list` of `ProjectSummary` objects:  

    search.list.ProjectSummary("blood",0,20)

Get the list of results from it:  

    project.list(search.list.ProjectSummary("blood",0,20))

Get them as a `data.frame`:  

    as.data.frame(search.list.ProjectSummary("blood",0,20))

Get the first 50 Proteins for project `PXD000001` as a list of `ProteinDetail` objects:  

    protein.list(list.ProteinDetailList("PXD000001", 0, 50))

Or as a `data.frame`:  

    as.data.frame(list.ProteinDetailList("PXD000001",0, 50))

Plot some counts:  

    plot(list.ProteinDetailList("PXD000001",0, 50))

Get 5 PSMs for project `PXD000001` as a list of `PsmDetail` objects:  

    get.list.PsmDetail("PXD000001", 5)

There are also count methods for each of the PRIDE Archive entitites.  

#### PRIDE Cluster  

Get page 0 with a size of 20 clusters for peptide sequence *LSVDYGK*:  

    search.ClusterSearchResults("LSVDYGK", 0, 20)

As a data frame:  

    as.data.frame(search.ClusterSearchResults("LSVDYGK", 0, 20))

Plot results:

    plot(search.ClusterSearchResults("LSVDYGK", 0, 20))

### Future Works  

Some things to be done, sooner than later:  

- Check mandatory parameters  
- Deal with `SpectrumDetail` entities when available  


### About us   

Find out about us in our GitHub profiles:  
[Yasset Perez-Riverol](https://github.com/ypriverol)  

