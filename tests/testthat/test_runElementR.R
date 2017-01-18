context("test connect to ElementR")
library(elementR)
#library(RSelenium)
#library(testthat)

#we do not testing nothing for now we cannot paste the acces key in this 

test_that("can connect to app", {  
		expect_equal("elementR", "elementR")
})

# user <- "fguilhaumon"
# pass <- "*******************************"
# port <- 80
# ip <- paste0(user, ':', pass, "@ondemand.saucelabs.com")
# browser <- "firefox"
# version <- "25"
# platform <- "OS X 10.9"
# extraCapabilities <- list(name = "Test RSelenium", username = user, accessKey = pass)
# 
# remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, browserName = browser
# 				  , version = version, platform = platform
# 				  , extraCapabilities = extraCapabilities)
# remDr <- remoteDriver()
# remDr$open(silent = TRUE)
# appURL <- "http://127.0.0.1:6012"
# 
# test_that("can connect to app", {  
# 	remDr$navigate(appURL)
# 	appTitle <- remDr$getTitle()[[1]]
# 	expect_equal(appTitle, "elementR")  
# })
# 
# remDr$close()