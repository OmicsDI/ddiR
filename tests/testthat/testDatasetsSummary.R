library(ddiR)
context("Datasets Summary")

# Getting all Datasets with 'NOTCH1' in its meta-data
datasets <- search.DatasetsSummary(query = "NOTCH1")

test_that("count attribute returns found all NOTCH1 datsets", {
    expect_gt(datasets@count, 867)
})

# Getting 10 datasets with 'NOTCH1' in its meta-data and check
datasets <- search.DatasetsSummary(query = "NOTCH1", size = 10)

match_query = vector()

for(dataset in datasets@datasets){
    dataset = get.DatasetDetail(accession=dataset.id(dataset),
                                database=database(dataset))
    match_query[dataset.id(dataset)] <- any(grepl('notch1',
                                          tolower(c(get.description(dataset),
                                          get.dataset.name(dataset)))))

}

test_that("check match of 'NOTCH1' in the meta-data of all 10 dataset", {
    expect_true(all(match_query))
})



# Getting Proteomics studies in Heart tissue from PRIDE database
datasets <- search.DatasetsSummary(query = "Heart")

match_query = vector()

for(dataset in datasets@datasets){
    dataset = get.DatasetDetail(accession=dataset.id(dataset), database=database(dataset))
    if(database(dataset)=='pride')
        match_query[dataset.id(dataset)] <- get.dataset.tissues(dataset)
}

test_that("check that the query term 'Heart' match in all retrived dataset", {
    expect_true(all(match_query == 'Heart'))
})
