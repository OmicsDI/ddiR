library(ddiR)
context("Dataset Detail")

# Getting a specific dataset from PRIDE
dataset <- get.DatasetDetail(accession = "PXD000210", database = "pride")

test_that("get.dataset.omics return type of omics study", {
    expect_equal(get.dataset.omics(dataset), "Proteomics")
})

test_that("get.instruments return instrument used in the study", {
    expect_equal(get.instruments(dataset), "LTQ Orbitrap,instrument model")
})

test_that("get.data.link return the study full link", {
    expect_equal(get.dataset.link(dataset), "http://www.ebi.ac.uk/pride/archive/projects/PXD000210")
})

