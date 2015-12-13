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

### About us   

Find out about us in our GitHub profiles:  
[Yasset Perez-Riverol](https://github.com/ypriverol)  

