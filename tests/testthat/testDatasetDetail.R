library(ddiR)
context("Dataset Detail")

# Getting a specific dataset from PRIDE
dataset <- get.DatasetDetail(accession = "PXD010062", database = "pride")

test_that("get.dataset.omics return type of omics study", {
    expect_true("Proteomics" %in% get.dataset.omics(dataset))
})

test_that("get.instruments return instrument used in the study", {
    expect_true("TripleTOF 5600" %in% get.instruments(dataset))
})

test_that("get.data.link return the study full link", {
    expect_equal(get.dataset.link(dataset), "http://www.ebi.ac.uk/pride/archive/projects/PXD010062")
})

test_that("get.diseases return the diseases under study", {
    expect_true("Infertility" %in% get.diseases(dataset))
})

test_that("get.keywords return the keywords described in the study", {
    expect_true("iTRAQ" %in% get.keywords(dataset))
})

test_that("get.experiments.type return detailed information of the type of experiment", {
    expect_true("Mass Spectrometry" %in% get.experiments.type(dataset))
})

test_that("get.organisms return list of organisms described in the study", {
    expect_true("Sus scrofa domesticus" %in% get.organisms(dataset))
})

test_that("get.publication.ids return list of publication IDs associated to the study", {
    expect_true("30257877" %in% get.publication.ids(dataset))
})

test_that("get.dataset.tissues return list of tissues under study", {
    expect_true("Spermatozoid" %in% get.dataset.tissues(dataset))
})

test_that("get.publication.date return the study publication date", {
    expect_equal(get.publication.date(dataset), "2018-10-02")
})
