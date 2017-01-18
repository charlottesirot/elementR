context("test elementR R6 classes")
library(elementR)
#library(RSelenium)
library(testthat)

test_that("can create & manipulate elementR_data object from xls data file", {
	
	filePath <- system.file("Example_Session/standards/Stand3.xls", package="elementR")
	
	obj <- elementR_data$new(filePath)
	
	#data is 2 dimension
	expect_equal(length(dim(obj$data)), 2)
	
	#class is "elementR_data" & "R6"
	expect_equal(class(obj), c("elementR_data","R6"))
	
	#setDataSupLOD triggers a long chain if testing hopefuly
	obj$setDataSupLOD(bins=c(1,16),plat=c(17,37),rempl="NA")
	expect_equal(length(obj$LOD),(dim(obj$data)[2]-1))
	
	
	
	
})

test_that("can create elementR_standard object from csv data file", {
	
	filePath <- system.file("Example_Session/standards/Stand1.csv", package="elementR")
	
	standard <- elementR_standard$new(filePath)
	
	#data is 2 dimension
	expect_equal(length(dim(standard$data)), 2)
	
	#class is "elementR_data" & "R6"
	expect_equal(class(standard), c("elementR_standard","elementR_data","R6"))
})

test_that("can create elementR_sample object from csv data file", {
	
	filePath <- system.file("Example_Session/samples/Sample_1/Sample1_Rep1.csv", package="elementR")
	
	obj <- elementR_sample$new(filePath)
	
	
	#data is 2 dimension
	expect_equal(length(dim(obj$data)), 2)
	
	#class is "elementR_data" & "R6"
	expect_equal(class(obj), c("elementR_sample","elementR_data","R6"))
})

test_that("can create elementR_repStrandard object from 3 csv,xls,ods data files", {
	
	filePath <- system.file("Example_Session/standards", package="elementR")
	
	obj <- elementR_repStandard$new(rep_folderPath = filePath)
	
	#data is 2 dimension
	expect_equal(sum(sapply(obj$rep_data,function(x){length(dim(x$data))})), 6)
	
	#class is "elementR_data" & "R6"
	expect_equal(class(obj), c("elementR_repStandard","elementR_rep","R6"))
})

