context("test connect to ElementR")
library(elementR)
library(RSelenium)
library(testthat)


remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:6012"

test_that("can connect to app", {  
	remDr$navigate(appURL)
	appTitle <- remDr$getTitle()[[1]]
	expect_equal(appTitle, "elementR")  
})

remDr$close()