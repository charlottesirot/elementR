##############################################################
#
# elementR 1.3.3
# 
# charlott.sirot@gmail.com
# francois.guilhaumon@ird.fr
#
#####################################################################

readData <- function(x, sep = ";", dec = "."){
  
  if(str_detect(x, ".xls")){
    df <- as.data.frame(read_excel(x, sheet = 1, col_names = TRUE))
  } else {}
  
  if(str_detect(x, ".csv")){
    df <- read.table(x, header = TRUE, sep = sep, dec = dec)
  } else {}
  
  if(str_detect(x, ".ods")){
    
    df <- read.ods(x)[[1]]
    
    colnames(df) <- df[1,]
    df <- df[-1,]
    
    col <- seq(from = 1, to = ncol(df), by = 1)
    
    err <- 0
    
    for(i in col){
      
      for(j in seq(from = 1, to = nrow(df), by = 1)){
        
        if(is.na(df[j,i]) | is.null(df[j,i])) {
          
        } else {
          
          if(suppressWarnings(is.na(as.numeric(as.character(df[j,i]))))) {
            
            err <- 1
            
          } else {
            
          }
          
        }
        
      }
      
    }
    
    if(err == 0){
      df <- as.matrix(as.data.frame(lapply(df, as.numeric)))
    } else {
      
    }
    
  } else {}
  return(df)
}

############################################################
############################################################
##################################### elementR_data Class
############################################################
############################################################
{
elementR_data <- R6Class("elementR_data",
                         public = list(
                           name = NA, # A character string corresponding to the name of the considered replicate
                           data = NA, # A matrix corresponding to the raw data of the considered replicate
                           fPath = NA, # A character string corresponding the path of the raw data
                           bins = c(NA,NA), # A numerical value corresponding to the time at which end the blank values
                           plat = c(NA,NA), # A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           dataBlank = NA, # A matrix corresponding to the blank data
                           dataPlateau = NA, # A matrix corresponding to the plateau data
                           dataSuppBlank = NA, # A matrix corresponding to the data obtained by substracting the averaged blank value (here, self$BlankAverarge) from the self$dataPlateau
                           dataSupLOD = NA, # A matrix of data corresponding to the values of self$dataSuppBlank up to the limit of detection (here self$LOD)
                           dataNorm = NA, # A matrix of data corresponding to the values of self$dataSupLOD normalized by the chemical element chosen as internal standard    (here, self$elemstand)
                           elemstand = NA, # A character string corresponding to the name of the chemical element chosen as internal standard                     
                           CustomLOD = 3, # the number of sd of the blank to calculate the LOD
                           LOD = NA, # A vector of numerical values corresponding to the limit of detection for each chemical element of the considered replicate
                           BlankAverarge = NA, # A vector of numerical values corresponding to the averaged blank values for each chemical element of the considered replicate
                           remplaceValue = NA, # A character string corresponding to the value replacing the self$dataSuppBlank below the limit of detection
                           
                           ##################################################################################################
                           # Name: setCustomLOD
                           # Function: set self$CustomLOD
                           # Input: x = a integer corresponding to the number n: LOD = n*sd(blank)
                           ##################################################################################################
                           
                           setCustomLOD = function(x){
                             self$CustomLOD <- x
                           },
                           
                           ##################################################################################################
                           # Name: setElemStand
                           # Function: set self$elemstand
                           # Input: x = a character string corresponding to the name of the chosen intern standard chemical element
                           ##################################################################################################
                           
                           setElemStand = function(x){
                             self$elemstand <- x
                           },
                           
                           ##################################################################################################
                           # Name: initialize
                           ##################################################################################################
                           
                           initialize = function(fPath=NULL, sep = ";", dec = ".") {
                             if(is.null(fPath)) stop("error, fPath missing !")
                             charStrings <- unlist(lapply(strsplit(fPath,"[.]"),strsplit,split="/"))
                             self$name <- charStrings[length(charStrings)-1] 
                             self$fPath <- fPath
                             d <- readData(fPath, sep = sep, dec = dec)
                             self$data <- d
                           },#initialize
                           
                           ##################################################################################################
                           # Name: setBins
                           # Function: set self$bins
                           # Input:  bins = A vector of numerical values corresponding to the time at which begins and ends the blank values
                           ##################################################################################################
                           
                           setBins = function(bins) {
                             
                              self$bins <- bins
                             
                           }, 
                           
                           ##################################################################################################
                           # Name: setPlat
                           # Function: set self$plat
                           # Input: plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           ##################################################################################################

                           setPlat = function(plat) {
                              
                              self$plat <- plat
                              
                            }, #setPlat
                                                      
                           ##################################################################################################
                           # Name: setDataBlanc
                           # Function: set self$dataBlank 
                           # Input: bins = A vector of numerical values corresponding to the time at which begins and ends the blank values
                           ##################################################################################################
                           
                           setDataBlanc = function(bins) {
                             
                             subDat <- self$data[bins[1]:bins[2],]       
                             
                             self$dataBlank <- subDat
                             
                             self$LOD <- self$CustomLOD*apply(self$dataBlank[,-1], 2, sd, na.rm =TRUE)
                             
                             self$LOD[is.na(self$LOD)] <- 0
                             
                             self$BlankAverarge <- apply(self$dataBlank[,-1], 2, mean, na.rm =TRUE)
                             
                           }, 
                           
                           ##################################################################################################
                           # Name: setDataPlateau
                           # Function: set self$dataPlateau
                           # Input: plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           ##################################################################################################
                           
                           setDataPlateau = function(plat = plat, bins = bins) {
                             
                             self$setDataBlanc(bins = bins)
                             
                             subDat <- self$data[plat[1]:plat[2],]
                             
                             self$dataPlateau <- subDat
                             
                           }, # setDataPlateau
                           
                           ##################################################################################################
                           # Name: setDataSuppBlank
                           # Function: set self$dataSuppBlank
                           # Input: bins = A numerical value corresponding to the time at which end the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           ##################################################################################################
                           
                           setDataSuppBlank = function(bins,plat) {
                             
                             self$setDataPlateau(plat = plat, bins = bins)
                                                         
                             tempo <- apply(self$dataBlank[,-1], 2, mean, na.rm = TRUE)
                                                          
                             subDat <- vapply(seq(from = 1, to = length(apply(self$dataBlank[,-1], 2, mean, na.rm = TRUE)), by = 1),
                             		     
                             		     function(x){       
                             		     	
                             		     	self$dataPlateau[,x+1] - tempo[x]  
                             		     	
                             		     	},
                             		     FUN.VALUE = double(nrow(self$dataPlateau))
                             		     )
                                                          
                             subDat <- cbind(as.matrix(self$dataPlateau[,1]), subDat)
                             
                             colnames(subDat) <- colnames(self$dataPlateau)
                             
                             self$dataSuppBlank <- subDat
                             
                           }, # setDataSuppBlank
                           
                           ##################################################################################################
                           # Name: setDataSupLOD
                           # Function: set self$dataSupLOD
                           # Input: bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           ##################################################################################################
                           
                           setDataSupLOD = function(bins, plat, rempl) { 
                             
                             self$setDataSuppBlank(bins = bins,plat = plat)
                             
                             if(is.null(rempl)){
                               self$remplaceValue <- rep(NA, nrow(self$dataSuppBlank))
                             } else if(is.na(rempl)){
                                 self$remplaceValue <- rep(NA, nrow(self$dataSuppBlank))
                               } else if(rempl == 0){
                                 self$remplaceValue <- rep(0, nrow(self$dataSuppBlank))
                               } else {
                                 self$remplaceValue <- self$BlankAverarge
                               }
                             
                             subDat <- do.call(cbind,lapply(2:ncol(self$dataSuppBlank),function(x){ l <- self$dataSuppBlank[,x]
                                                                                                            
                                                                                                    l[ l< self$LOD[x-1] ] <- as.numeric(as.character(self$remplaceValue[x-1]))
                                                                                                            
                                                                                                    l

                                                                                                            
                             })) 
                             
                             subDat <- cbind(as.matrix(self$dataSuppBlank[,1]),subDat)
                             
                             colnames(subDat) <- colnames(self$dataSuppBlank)
                             
                             self$dataSupLOD <- subDat
                             
                           }, 
                           
                           ##################################################################################################
                           # Name: setDataNorm
                           # Function: set self$dataNorm
                           # Input: bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = A vector containing two numerical values corresponding respectively to the time at which begin and end the plateau values
                           ##################################################################################################
                           
                           setDataNorm = function(bins,plat, rempl) {
                             
                             self$setDataSupLOD(bins = bins,plat = plat, rempl = rempl)
                             
                             subDat <- vapply(seq(from = 2, to = ncol(self$dataSupLOD), by = 1),
                             		     
                             		     function(x){ 
                             		     	
                             		     	self$dataSupLOD[,x]/self$dataSupLOD[,grep(self$elemstand,colnames(self$dataSupLOD))]
                             		     	
                             		     	},
                             		     FUN.VALUE = double(nrow(self$dataSupLOD))
                             		     )
                                                          
                             subDat <- cbind(as.matrix(self$dataSupLOD[,1]),subDat)                             
                             
                             colnames(subDat) <- colnames(self$dataSupLOD)
                             
                             self$dataNorm <- subDat
                             
                           },#setDataNorm  
                           
                           ##################################################################################################
                           # Name: OutlierDetectTietjen
                           # Function: return the place of the outlier according to Tietjen and outlier methods
                           # Input: 
                           # 	x: a vector of data
                           # 	nbOutliers: number of oulier to detect
                           #################################################################################################
                           
                           OutlierDetectTietjen = function(x, nbOutliers){
                           	
                           	flag <- 0
                           	
                           	for(i in nbOutliers:1){
                           		
                           		test <- FindOutliersTietjenMooreTest(x, i)
                           		
                           		if(test$T < test$Talpha & flag == 0){
                           			
                           			datTemp <- x
                           			
                           			posOutlier <- NULL
                           			
                           			for(j in seq(from = 1, to = i, by = 1)){ 
                           				
                           				Outlier <- outlier(datTemp)
                           				
                           				positionX <- which(x == Outlier)
                           				
                           				positionTemp <- which(datTemp == Outlier)
                           				
                           				datTemp <- datTemp[-positionTemp]
                           				
                           				posOutlier <- c(posOutlier, positionX)
                           			}
                           			
                           			flag <- 1
                           			
                           		} else {posOutlier <- NULL}
                           	}
                           	
                           	return(posOutlier)
                           },
                           
                           ##################################################################################################
                           # Name: outlierDetection
                           # Function: return the place of the outlier 
                           # Input: 
                           # 	dat: a vector of data
                           # 	method: method of detection of the outlier (sd, Tietjen's test (generalization of the grubb's test) or Rosner test)
                           # 	nbOutliers: number of oulier to detect
                           #################################################################################################
                           
                           outlierDetection = function(dat, method, nbOutliers){
                           	
                           	if(method == "SD criterion"){
                           		
                           		ValMax <- mean(dat, na.rm = TRUE) + 2*sd(dat,na.rm = TRUE)
                           		
                           		ValMin <- mean(dat, na.rm = TRUE) - 2*sd(dat,na.rm = TRUE)
                           		
                           		position <- which(dat > ValMax | dat < ValMin)[seq(from = 1, to = nbOutliers, by = 1)]
                           		
                           	} else if(method == "Tietjen.Moore Test"){
                           		
                           		position <- self$OutlierDetectTietjen(x = dat, nbOutliers)
                           		
                           	} else if(method == "Rosner's test"){
                           		
                           		test <- suppressWarnings(rosnerTest(dat, k = nbOutliers, alpha = 0.05, warn = TRUE))
                           		
                           		Outliers <- test$all.stats[which(test$all.stats[,8] == TRUE), 4]
                           		
                           		if(length(Outliers) != 0){
                           			position <- vapply(seq(from = 1, to = length(Outliers), by = 1),

                           						 function(x){

                           						 	which(dat == Outliers[x])

                           						 },
                           						 FUN.VALUE = numeric(1)
                           			)
                           		} else {
                           			position <- NULL
                           		}
                           		
                           	} else {}
                           	
                           	return(position)
                           	
                           },
                           
                           ##################################################################################################
                           # Name: is.integer0
                           # Function: test the value integer(0)
                           # Input: x = the vector to test
                           # Output: TRUE or FALSE
                           ##################################################################################################
                           
                           is.integer0 = function(x){
                           	is.integer(x) && length(x) == 0L
                           },
                           
                           ##################################################################################################
                           # Name: detectOutlierMatrix
                           # Function: return the place of the outlier for each column of a matrix
                           # Input: 
                           # 		dat: a matrix of data
                           # 		method: method of detection of the outlier (sd, Tietjen's test (generalization of the grubb's test) or Rosner test)
                           # 		nbOutliers: number of oulier to detect
                           #################################################################################################
                           
                           detectOutlierMatrix = function(dat, method, nbOutliers){
                           
                           	if(method == "Tietjen.Moore Test"){
                           		pb <- tkProgressBar("Outlier detection", "Detection in %",
                           					  0, 100, 0)
                           	} else {}
                           
                           	if(!is.null(ncol(dat))){
                           	res <- lapply(seq(from = 1, to = ncol(dat), by = 1), function(x){
                           		
                           		if(method == "Tietjen.Moore Test"){
                           			info <- sprintf("%d%% done", round(x/ncol(dat)) * 100)
                           			setTkProgressBar(pb, round(x/ncol(dat)) * 100, sprintf("OUtlier detection (%s)", info), info)
                           		} else {}
                           		
                           		
                           		if(x == 1){
                           			
                           			NULL
                           			
                           		} else if(!self$is.integer0(which(!is.na(dat[,x]) == TRUE))){
                           			
                           			if(self$is.possibleOutlier(dat = dat[,x])){
                           				
                           				self$outlierDetection(dat = dat[,x], method = method, nbOutliers)
                           				
                           			}else {NULL}
                           			
                           		} else {NULL}
                           		
                           		
                           		
                           	})
                           }
                           	
                           	if(method == "Tietjen.Moore Test"){
                           		close(pb)
                           	} else {}
                           	
                           	return(res)
                           	
                           },
                           
                           ##################################################################################################
                           # Name: outlierReplace
                           # Function: replace the outliers value of a matrix by rempl
                           # Input: 
                           # 	dat: a matrix of data
                           # 	outlierList: a list showing the place of the outlier for each column
                           # 	rempl: the value to replace if outliers
                           #################################################################################################
                           
                           outlierReplace = function(dat, outlierList, rempl){
                           	
                           	subDat <- vapply(seq(from = 1, to = ncol(dat), by = 1),
                           			     
                           			     function(x){
                           			     	
                           			     	if(!is.null(outlierList[[x]])){
                           			     		
                           			     		dat[outlierList[[x]],x] <- rempl
                           			     		
                           			     		return(dat[,x])
                           			     		
                           			     	} else {dat[,x]}
                           			     	
                           			     }, 
                           			     FUN.VALUE = double(nrow(dat))
                           			     )
                           	
                           	colnames(subDat) <- colnames(dat)
                           	
                           	return(subDat)
                           },
                           
                           ##################################################################################################
                           # Name: is.possibleOutlier
                           # Function: check that the vector fits with the needs for outlier detection (length of data > 30...)
                           # Input: dat: a vector of data
                           ##################################################################################################
                           
                           is.possibleOutlier = function(dat){
                           	
                           	temp <- dat[which(!is.na(dat))]
                           	
                           	if(length(temp) < 30){
                           		FALSE
                           	} else if(length(which(duplicated(dat) == F)) == 1){
                           		FALSE
                           	} else {TRUE}
                           	
                           },
                           
                           ##################################################################################################
                           # Name: reset
                           # Function: Reset the dataConcCorr
                           ##################################################################################################
                           
                           reset = function(){
                             self$dataConcCorr <- NA
                           } # reset table
                         )

)#elementR_data
}
############################################################
############################################################
################################# elementR_standard Class
############################################################
############################################################
{
elementR_standard <- R6Class("elementR_standard",
                                inherit = elementR_data,
                                public = list(                                  
                                  dataOutlierFree = NA, # A matrix corresponding to the self$dataNorm without abnomalities
                                  data_standFinalMean = NA, # A vector corresponding to the average of self$dataOutlierFree per chemical element
                                  data_standFinalSD = NA, # A vector corresponding to the standard deviation of self$dataOutlierFree per chemical element
                                  type = "standard", # A character string indicating the type of replicate (here, "standard")   
                                  
                                  ##################################################################################################
                                  # Name: setDataOutlierFree
                                  # Function: set self$dataOutlierFree
                                  # Input: 
                                  # 	bins = A vector of numerical values corresponding to the time at which begins and ends the blank values
                                  # 	plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau
                                  # 	rempl = value to replace outliers
                                  # 	method = the method used to detect outlier (sd criterion, Rosner test, Grubbs test (generalization of the grubb's test))
                                  # 	nbOutliers = nb of outlier to detect
                                  ##################################################################################################
                                  
                                  setDataOutlierFree = function(bins, plat, rempl, method, nbOutliers){
                                  	
                                  	self$setDataNorm(bins,plat, rempl)
                                  	
                                  	dat <- self$dataNorm
                                  	
                                  	if(is.null(method)){
                                  		method <- "Rosner's test"
                                  	} else {}
                                  	
                                  	if(is.null(rempl)){
                                  		rempl <- NA
                                  	} else {}
                                  	
                                  	outlierList <- self$detectOutlierMatrix(dat, method = method, nbOutliers)

                                  	self$dataOutlierFree <- self$outlierReplace(dat, outlierList, rempl = rempl)
                                  	
                                  	suppressWarnings(mode(self$dataOutlierFree) <- "numeric")

                                  },
                                  
                                  ##################################################################################################
                                  # Name: setdata_standFinal
                                  # Function: set sel$data_standFinalMean and self$data_standFinalSD
                                  ##################################################################################################
                                  
                                  setdata_standFinal = function(){
                                    self$data_standFinalMean <- apply(self$dataOutlierFree,2,mean, na.rm = TRUE)[-1]  
                                    self$data_standFinalSD <- apply(self$dataOutlierFree,2,sd, na.rm = TRUE)[-1]
                                  }, 
                                  
                                  ##################################################################################################
                                  # Name: getData
                                  # Function: a fonction to calculate and render data 
                                  # Input: curve = a character string corresponding to the type of data to render, bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau
                                  # Output:  a matrix of the required data
                                  ##################################################################################################
                                  
                                  getData = function(curve, bins, plat, rempl, method, nbOutliers){
                                    
                                    if(curve =="Blank") {self$setDataBlanc(bins = bins)
                                                         return(self$dataBlank)} else {}
                                    
                                    if(curve =="Raw") {self$setDataBlanc(bins = bins)
                                                        return(self$data) } else {}
                                    
                                    if(curve =="Plateau") {self$setDataPlateau(plat = plat, bins = bins)
                                                           return(self$dataPlateau)} else {}
                                    
                                    if(curve =="Blank removed") {self$setDataSuppBlank(bins = bins,plat = plat)
                                                                 return(self$dataSuppBlank) } else {}
                                    
                                    if(curve =="> LOD") {self$setDataSupLOD(bins = bins,plat = plat, rempl = rempl)
                                                         return(self$dataSupLOD) } else {}
                                    
                                    if(curve =="Normalized") {self$setDataNorm(bins = bins,plat = plat, rempl = rempl)
                                                              return(self$dataNorm) } else {}
                                    
                                    if(curve =="Outliers free") {self$setDataOutlierFree(bins = bins,plat = plat, rempl = rempl, method, nbOutliers)
                                                                 return(self$dataOutlierFree) } else {}
                                  },
                                  
                                  ##################################################################################################
                                  # Name: renderData
                                  # Function: render data without proceding to their calculation (compared to getData)
                                  # Input: a character string corresponding to the type of data to render  
                                  # Output: a matrix of the required data
                                  ##################################################################################################
                                  
                                  renderData = function(curve){
                                    
                                    if(curve =="Blank") {return(self$dataBlank)} else {}
                                    
                                    if(curve =="Raw") {return(self$data)} else {}
                                    
                                    if(curve =="Plateau") {return(self$dataPlateau)} else {}
                                    
                                    if(curve =="Blank removed") {return(self$dataSuppBlank)} else {}
                                    
                                    if(curve =="> LOD") {return(self$dataSupLOD) } else {}
                                    
                                    if(curve =="Normalized") {return(self$dataNorm) } else {}
                                    
                                    if(curve =="Outliers free") {return(self$dataOutlierFree) } else {}
                                  }
                                  
                                )
)#elementR_standard
}

############################################################
############################################################
################################# elementR_sample Class
############################################################
############################################################
{
elementR_sample <- R6Class("elementR_sample",
                           inherit = elementR_data,
                           public = list(
                             type = "sample", # A character string corresponding to the type of replicate (here, "sample")
                             dataConc = NA, # A matrix corresponding to the self$dataNorm converted in concentration
                             dataConcCorr = NA, # A matrix corresponding to the self$dataConc corrected (or not) from the machine drift
                             
                             ##################################################################################################
                             # Name: setDataConc
                             # Function: set self$dataConc
                             # Input:  
                             #	bins = A vector of numerical values corresponding to the time at which begins and ends the blank values
                             #	plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau
                             #	calibFile = a matrix corresponding to the data of the calibration file
                             #	meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session
                             #	rempl = the value replacing data if below the limit of detection
                             ##################################################################################################
                             
                             setDataConc = function(bins, plat, calibFile, meanStand, rempl){
                               
                               self$setDataNorm(bins = bins, plat = plat, rempl = rempl)
                             	
                               temp <- vapply(seq(from = 2, to = ncol(self$dataNorm), by = 1),
                               		   
                               		   function(x){
                               		   	
                               		   	self$dataNorm[,x] * calibFile[1,x]/ meanStand[nrow(meanStand)-1, x-1]},
                               		   
                               		   FUN.VALUE = double(nrow(self$dataNorm))
                               		   
                               		   )
                               
                               self$dataConc <- cbind(as.matrix(self$dataNorm[,1]),temp)
                               
                               colnames(self$dataConc) <- colnames(self$dataNorm)
                               
                             }, #setDataConc
                             
                             ##################################################################################################
                             # Name: setDataConcCorr
                             # Function: set self$dataConcCorr
                             # Input: 
                             #	bins = a numerical value corresponding to the time at which end the blank values
                             #	plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau
                             # 	name = a character string corresponding to the name of the sample replicates
                             #	calibFile = a matrix corresponding to the the calibration file
                             # 	meanStand = a table containing the averaged signal intensity per chemical element for all standard replicates of the running session
                             # 	rankSample = a vector containing the rank of each sample in ICPMS analysis
                             # 	rankStandard = a vector containing the rank of each standard in ICPMS analysis
                             # 	correction = a vector indicating the chemical elements to correct from machine drift
                             # 	model = the matrix containing the parameters of the linear regression corresponding to machine drift for all chemical elements
                             ##################################################################################################

                             setDataConcCorr = function(bins, plat, name, calibFile, meanStand, rankSample, rankStandard, model, correction, rempl,threshold){

                             	if(is.null(threshold)){
                             		threshold <- 0.75
                             	} else {}
                               
                               self$setDataConc(bins = bins, plat = plat, calibFile = calibFile, meanStand = meanStand, rempl = rempl)
                               
                               rankSampleConsidered <- rankSample[which(names(rankSample) == name)]
                               
                               temp <- vapply(seq(from = 2, to = ncol(self$dataNorm), by = 1),
                               		   
                               		   function(x){
                               		   	
                               		   	if(correction[x-1] == FALSE){ # No correction
                               		   		
                               		   		return(self$dataNorm[,x] * calibFile[1,x]/ meanStand[nrow(meanStand)-1, x-1])
                               		   		
                               		   	}
                               		   	
                               		   	if(correction[x-1] == TRUE){ # correction asked by user
                               		   		
                               		   		# reminder model[x-1,7] == R2
                               		   		if(!is.na(model[x-1,7])){
                               		   			
                               		   			if(model[x-1,7] < threshold){ # the model is not a linear regression
                               		   				
                               		   				# Standard1 is number of the closest standard of the rankSampleConsidered
                               		   				Standard1 <- which(abs(rankStandard - rankSampleConsidered) == min(abs(rankStandard - rankSampleConsidered)))[1]
                               		   				
                               		   				#if the considered sample is analyzed after the last standard, Standard2 will be the number of the last standard analyzed
                               		   				if(rankSampleConsidered > max(rankStandard)){
                               		   					
                               		   					Standard2 <- max(rankStandard)
                               		   					names(Standard2) <- names(rankStandard)[which(rankStandard == max(rankStandard))]
                               		   					
                               		   				} else if(rankSampleConsidered < min(rankStandard)){ #if the considered sample is analyzed before the first standard, Standard2 will be the number of the first standard analyzed
                               		   					
                               		   					Standard2 <- min(rankStandard)
                               		   					names(Standard2) <- names(rankStandard)[which(rankStandard == min(rankStandard))]
                               		   					
                               		   				} else if(rankSampleConsidered < rankStandard[Standard1]){
                               		   					
                               		   					Standard2 <- rankStandard[Standard1-1]
                               		   					names(Standard2) <- names(rankStandard)[which(rankStandard == rankStandard[Standard1-1])]
                               		   					
                               		   				} else {
                               		   					Standard2 <- rankStandard[Standard1+1]
                               		   					names(Standard2) <- names(rankStandard)[which(rankStandard == rankStandard[Standard1+1])]
                               		   				}
                               		   				
                               		   				stand1Value <- meanStand[which(rownames(meanStand) == paste0(names(Standard1), " Mean")), x-1]
                               		   				stand2Value <- meanStand[which(rownames(meanStand) == paste0(names(Standard2), " Mean")), x-1]
                               		   				
                               		   				if(Standard1 == Standard2){
                               		   					
                               		   					StandTheoric <- meanStand[which(rownames(meanStand) == paste0(names(Standard2), " Mean")), x-1]
                               		   					
                               		   				} else if(stand1Value == stand2Value){
                               		   					
                               		   					StandTheoric <- 1
                               		   					
                               		   				} else {
                               		   					
                               		   					modelneighbor <- lm(c(stand1Value, stand2Value) ~ c(Standard1, Standard2))
                               		   					
                               		   					StandTheoric <- modelneighbor$coefficients[1] + rankSampleConsidered * modelneighbor$coefficients[2]
                               		   				}
                               		   				
                               		   				return(self$dataNorm[,x] * calibFile[1,x] / StandTheoric)
                               		   				
                               		   			} else { # the model seems to be a linear regression
                               		   				
                               		   				StandTheoric <- model[x-1,5] + rankSampleConsidered * model[x-1, 6]
                               		   				
                               		   				return(self$dataNorm[,x] * calibFile[1,x] / StandTheoric)	
                               		   				
                               		   			}
                               		   			
                               		   		} else {
                               		   		  
                               		   		  StandTheoric <- model[x-1,5] + rankSampleConsidered * model[x-1, 6]
                               		   		  
                               		   		  return(self$dataNorm[,x] * calibFile[1,x] / StandTheoric)	
                               		   		}
                               		   		
                               		   	}
                               		   	
                               		   	},
                               		   FUN.VALUE = double(nrow(self$dataNorm))
                               		   )
                               
                               tabTemp <- cbind(as.matrix(self$dataNorm[,1]),temp)
                               
                               colnames(tabTemp) <- colnames(self$dataNorm)
                               
                               self$dataConcCorr <- tabTemp
                               
                             }, #setDataConc
                            
                             ##################################################################################################
                             # Name: getData
                             # Function: a fonction to calculate and render data 
                             # Input: curve = a character string corresponding to the type of data to calculate, bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begins and ends the plateau, name = a character string corresponding to the name of the sample replicate, calibFile = a matrix corresponding to the the calibration file, meanStand = a vector containing the averaged signal intensity for all standard replicates and for each chemical element, rank = a vector containing the rank of each sample in ICPMS analysis, correction = a vector indicating which chemical element has to be corrected from machine drift, model = the matrix containing the parameters of the linear regression corresponding to machine drift for all chemical elements
                             # Output: a matrix of the required data
                             ##################################################################################################
                             
                             getData = function(curve, bins, plat, name, meanStand, rankSample, rankStandard, model, calibFile, correction, rempl,threshold){
                              
                               if(curve =="Blank") {self$setDataBlanc(bins = bins)
                                                         return(self$dataBlank)}
                               
                               if(curve =="Raw") {self$setDataBlanc(bins = bins)
                                                  return(self$data) }
                               
                               if(curve =="Plateau") {self$setDataPlateau(plat = plat, bins = bins)
                                                           return(self$dataPlateau)}
                                                              
                               if(curve =="Blank removed") {self$setDataSuppBlank(bins = bins, plat = plat)
                                                                   return(self$dataSuppBlank) }
                                                              
                               if(curve =="> LOD") {self$setDataSupLOD(bins = bins, plat = plat, rempl = rempl)
                                                         return(self$dataSupLOD) }
                                                              
                               if(curve =="Normalized") {self$setDataNorm(bins = bins, plat = plat, rempl = rempl)
                                                             return(self$dataNorm) } 
                                                              
                               if(curve =="Concentration") {self$setDataConc(bins = bins, plat = plat, calibFile = calibFile, meanStand = meanStand, rempl = rempl)
                                                            return(self$dataConc) 
                               					}
                               
                               if(curve == "Conc. corrected") {self$setDataConcCorr(bins = bins, plat, name, calibFile = calibFile, meanStand = meanStand, rankSample = rankSample, rankStandard = rankStandard, model = model, correction = correction, rempl = rempl, threshold = threshold)
                                                                    return(self$dataConcCorr)}
                               
                             },
                             
                             ##################################################################################################
                             # Name: renderData
                             # Function: render data without proceding to their calculation 
                             # Input: curve = a character string corresponding to the type of data to render
                             # Output: a matrix of the required data
                             ##################################################################################################
                            
                             renderData = function(curve){
                               
                               if(curve =="Blank") {return(self$dataBlank)}
                               
                               if(curve =="Raw") {return(self$data)}
                               
                               if(curve =="Plateau") {return(self$dataPlateau)}
                               
                               if(curve =="Blank removed") {return(self$dataSuppBlank) }
                               
                               if(curve =="> LOD") {return(self$dataSupLOD) }
                               
                               if(curve =="Normalized") {return(self$dataNorm) } 
                               
                               if(curve =="Concentration") {return(self$dataConc) }
                               
                               if(curve == "Conc. corrected") {return(self$dataConcCorr)}
                               
                             }

                           )#public
)#elementR_Sample
}

############################################################
############################################################
################################# elementR_project Class
############################################################
############################################################
{
elementR_project <- R6Class("elementR_project",
                            public = list(
                              name = NA,  # A character string corresponding to the name of the project                        
                              folderPath = NA, # A character string corresponding to the path of the project
                              standardsPath = NA, # A character string corresponding to the path of the standard folder
                              standardsFiles = NA, # A vector containing the names of each standard file
                              standards = NA, # A list containing the self$elementR_repStandard of each type of standard
                              samplesPath = NA, # A character string corresponding to the path of the sample folder
                              samplesFiles = NA, # A vector containing the names of each sample file
                              samples = NA, # A list containing the self$elementR_repSample of each sample
                              EtalonPath = NA, # A character string corresponding to the path of the calibration file
                              EtalonData = NA,  # A matrix corresponding to the calibration data
                              listeElem = NA, # A vector containing the names of the chemical elements included in the project
                              flag_stand = NA, # A vector indicating which standards have been filtered
                              flag_Sample = NA,  # A vector indicating which samples have been filtered
                              flagRealign = list(), # A list vectors indicating which samples have been realigned or averaged (transect and spot mode)
                              standardRank = NA, # A vector corresponding to the standard rank in ICPMS analysis
                              sampleRank = NA, # A vector corresponding to the sample rank in ICPMS analysis
                              elementChecking = list(), # A list indicating the number and the location of the error(s) of structure within data included in the project
                              regressionModel = matrix(), # A matrix summarizing, for each chemical element, the parameters of the linear regression corresponding to the machine drift
                              machineCorrection = NA, # A vector summarizing the chemical element(s) to correct from machine drift
                              flagMachineCorrection = 0, # A numerical value indicating the validation of the machine correction step
                              errorSession = NA, # A numerical value indicating the non numeric error(s) within data included in the project
                              nbCalib = vector(), # A vector corresponding to the number of standard values available for each chemical element to proceed the linear regression
                              elemStand = NA, # A character string indicating the chemical element considered as internal standard (by default = Ca)
                              summarySettings = matrix(), # A matrix summarizing all the parameters set by user for each replicate (sample and standard)
                              ChoiceUserCorr = NA, # a logical value corresponding to the choice of the user to correct or no the session based on the first step of configuration
                              R2Threshold = NA, #the threshold to pass from a machien drift correction from a linear to a neighbor correction
                              valRemplace = NA, #the value to replace in the case of value < LOD
                              literatureConcentration = NA, #Concentration of the reference material
                              precisionTable = NA, #table with %RSD and LOD of the standard material
                              correctnessTable  = NA, #the value of the reference materials + the mean of those + the literatureConcentration + diffrenec between the observed mean  and the literature
                              
                              ##################################################################################################
                              # Name: setPrecisionTable
                              # Function: set precisionTable
                              # inputs: x the table to set
                              ##################################################################################################
                              
                              setPrecisionTable = function(x){
                              	self$precisionTable <- x
                              },
                              
                              ##################################################################################################
                              # Name: setCorrectnessTable
                              # Function: set correctnessTable
                              # inputs: x the table to set
                              ##################################################################################################
                              
                              setCorrectnessTable = function(x){
                              	self$correctnessTable <- x
                              },
                              
                              ##################################################################################################
                              # Name: setLiteratureConcentration
                              # Function: set literatureConcentration
                              # inputs: x the path of the file
                              ##################################################################################################

                              setLiteratureConcentration = function(x, sep, dec){
                              	self$literatureConcentration <- readData(x, sep = sep, dec = dec)
                              },
                              
                              ##################################################################################################
                              # Name: setvalRemplace
                              # Function: set valRemplace
                              # inputs: x the value to replace of values < LOD
                              ##################################################################################################
                              
                              setvalRemplace = function(x){
                              	
                              	if(x == "Averaged value of the blank"){
                              		self$valRemplace <- x
                              	} else {
                              		self$valRemplace <- eval(parse(text = x))
                              	}
                              },
                              
                              ##################################################################################################
                              # Name: setR2Threshold
                              # Function: set R2Threshold
                              # inputs: x a value between 0 and 1
                              ##################################################################################################
                              
                              setR2Threshold = function(x){
                              	self$R2Threshold <- x
                              },
                              
                              ##################################################################################################
                              # Name: insert.at
                              # Function: insert values in vectors
                              # inputs: a = a vector, pos =  the position to insert,  toInsert = a vector to insert
                              ##################################################################################################
                              
                              insert.at = function(a, pos, toInsert){
                              	dots <- list(toInsert)
                              	stopifnot(length(dots)==length(pos))
                              	result <- vector("list",2*length(pos)+1)
                              	result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos)))
                              	result[c(FALSE,TRUE)] <- dots
                              	unlist(result)
                              },
                              
                              ##################################################################################################
                              # Name: detectPlateau
                              # Function: detection of the plateau limits
                              # inputs: dat = the data to proceed, col =  the column used for the detection 
                              ##################################################################################################
                              
                              detectPlateau = function(dat, col){
                              	
                              	if(!is.null(dat)){
                              		naLines <- which(is.na(dat[,col]))
                              		
                              		kmean <- kmeans(na.omit(dat[,col]),2, algorithm = "Hartigan-Wong")
                              		
                              		if(!self$is.integer0(naLines)){
                              			temp <- self$insert.at(kmean$cluster, naLines, rep(NA, length(naLines)))
                              		} else {
                              			temp <- kmean$cluster
                              		}
                              		
                              		dat1 <- cbind(dat, temp)
                              		
                              		datList <- list(dat1[which(dat1[,ncol(dat1)] == 1),], dat1[which(dat1[,ncol(dat1)] == 2),])
                              		
                              		meanStand <- vapply(seq(from = 1, to = length(datList), by = 1),
                              					  
                              					  function(x){
                              					  	
                              					  	mean(datList[[x]][,col])
                              					  	
                              					  },
                              					  FUN.VALUE = numeric(1)
                              		)
                              		
                              		plateau <- which(meanStand == max(meanStand))
                              		
                              		limitPlateau <- c(dat1[which(kmean$cluster == plateau)[1],1], dat1[which(kmean$cluster == plateau)[length(which(kmean$cluster == plateau))],1])
                              		
                              		return(limitPlateau)
                              	} else {return(c(NA, NA))}
                              	

                              	
                              },
                              
                              ##################################################################################################
                              # Name: detectBlank
                              # Function: detection of the blank limits
                              # inputs: dat = the data to proceed, col =  the column which is used for the detection 
                              ##################################################################################################
                              
                              detectBlank = function(dat, col){
                              	
                              	if(!is.null(dat)){
                              		
                              		rolMedian <- rollmedian(dat[,col], 3)
                              		
                              		deriv1 <- vapply(seq(from = 1, to = length(rolMedian), by = 1), 
                              				     
                              				     function(x){
                              				     	
                              				     	(rolMedian[x+1] - rolMedian[x])/(dat[x+1,1] - dat[x,1])
                              				     	
                              				     },
                              				     FUN.VALUE = numeric(1)
                              		)
                              		
                              		maxDeriv1 <- max(deriv1, na.rm = TRUE)
                              		
                              		endBlank <- which(deriv1 == maxDeriv1)[1] - 1
                              		
                              		return(c(1,dat[endBlank,1]))	
                              	} else {
                              		return(c(NA,NA))
                              	}
                              	

                              	
                              },
                              
                              ##################################################################################################
                              # Name: set_ChoiceUserCorr
                              # Function: set self$ChoiceUserCorr
                              # inputs: x = TRUE (for checking machine drift), F (for not checking machine drift)
                              ##################################################################################################
                              
                              set_ChoiceUserCorr = function(x){
                                
                                self$ChoiceUserCorr <- x
                                
                              },
                              
                              ##################################################################################################
                              # Name: set_summarySettings
                              # Function: set self$summarySettings
                              # inputs: name = a character string corresponding to the name of the replicate to set, rank= its rank in ICPMS analysis, bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat1 = a numerical value corresponding to the time at which begin the plateau values, plat2 = a numerical value corresponding to the time at which end the plateau values, average =  a vector corresponding to the blank averaged value (here, self$BlankAverarge) for each chemical element of the considered replicate, LOD = a vector corresponding to the limit of detection (here, self$LOD) for each chemical element of the considered replicate
                              ##################################################################################################
                              
                              set_summarySettings = function(name, rank, bins1, bins2, plat1, plat2, average, LOD){

                                self$summarySettings[which(rownames(self$summarySettings) == name),] <- c(name, rank, bins1, bins2, plat1, plat2, average, LOD)
                     
                              },
                              
                              ##################################################################################################
                              # Name: is.integer0
                              # Function: test the value integer(0)
                              # Input: x = the vector to test
                              # Output: TRUE or FALSE
                              ##################################################################################################
                              
                              is.integer0 = function(x){
                                is.integer(x) && length(x) == 0L
                              },
                              
                              ##################################################################################################
                              # Name: closest
                              # Function: find the nearest value among a vector of numerical data
                              # Input: x = a vector of numerical values, y = the investigated value
                              # Output: val = a list of two values: the nearest value and its place within the vector
                              ##################################################################################################
                              
                              closest = function(x,y){
                                val = list()
                                if(is.null(y)){}
                                else if(is.na(y)){}
                                else{
                                  val[[2]] <- which(abs(x-y) == min(abs(x-y), na.rm = TRUE))
                                  val[[1]] <- x[val[[2]]]
                                  
                                  if (length(val[[1]])!=1){val[[2]] <- min(val[[2]], na.rm = TRUE)
                                                            val[[1]] <- x[val[[2]]]
                                  } else {}
                                  
                                  names(val) <- c("the nearest", "place")
                                  
                                  return(val)
                                }
                                
                              },
                              
                              ############################################################################################
                              # Name: setElemStand 
                              # fonction: define self$elemStand and transmit this value to all elementR_rep and elementR_data objects included in the project
                              # Input: elem = a character string corresponding to the element considered as intern standard
                              ############################################################################################
                              
                              setElemStand = function(elem){
                                
                                self$elemStand <- elem
                                
                                #transmit to standards
                                lapply(seq(from = 1, to = length(self$standards[[1]]$rep_data), by = 1), function(x){
                                  self$standards[[1]]$rep_data[[x]]$setElemStand(elem)
                                })
                                
                                #transmit to samples
                                lapply(seq(from = 1, to = length(self$samples), by = 1), function(x){
                                  lapply(seq(from = 1, to = length(self$samples[[x]]$rep_data), by = 1), function(y){
                                    self$samples[[x]]$rep_data[[y]]$setElemStand(elem)
                                  })                                  
                                })
                              }, 
                              
                              ##################################################################################################
                              # Name: set_flagRealign 
                              # Function: set self$flagRealign
                              # Input: replicate = a numerical value corresponding to the number of the considered replicate, type = a character string indicating the transect or spot mode, value = the numerical value to set
                              ##################################################################################################
                              
                              set_flagRealign = function(replicate, type, value){
                                
                                if(type == "spot"){
                                  
                                  self$flagRealign[[replicate]][1] <- value
                                  
                                } else if(type == "transect"){ 
                                  
                                  self$flagRealign[[replicate]][2] <-  value
                                  
                                } else {}
                                
                              },
                              
                              ##################################################################################################
                              # Name: PlotIC
                              # Function: #plot mean +/- SD
                              # Input: name = a vector of the names to display on xaxis, Mean = a vector of mean, SD = a vector of SD, coord = a vector of coordonnates to place xticks, lengthSeg = a numeric value cooresponding to the length of the top segment of the SD bar, xlim & ylim = the limits of plots, xlab & ylab = the labels of axis
                              ##################################################################################################
                              
                              PlotIC = function(name, Mean,SD, coord, lengthSeg, xlim, ylim, type = "p", xlab, ylab){

                                plot(1, xaxt='n', yaxt = 'n', type="n", ylim = ylim, xlim = xlim, xlab = xlab, ylab = ylab)
                                axis(2)
                                axis(1, at = coord, labels = name)
                                points(coord,Mean)
                                invisible(lapply(seq(from = 1, to = length(Mean), by = 1), function(x){
                                  segments(coord[x], Mean[x]-SD[x], coord[x], Mean[x]+SD[x])
                                }))
                                invisible(lapply(seq(from = 1, to = length(Mean), by = 1), function(x){
                                  segments((coord[x]-lengthSeg),Mean[x]+SD[x],(coord[x]+lengthSeg),Mean[x]+SD[x])
                                }))
                                invisible(lapply(seq(from = 1, to = length(Mean), by = 1), function(x){
                                  segments((coord[x]-lengthSeg),Mean[x]-SD[x],(coord[x]+lengthSeg),Mean[x]-SD[x])
                                }))
                              }, 
                              
                              ##################################################################################################
                              # Name: setEtalon
                              # Function: ddefine self$EtalonPath and self$EtalonData and check the validity of their data structure
                              # Input:  x = a character string corresponding to the path of the calibration file
                              ##################################################################################################
                              
                              setEtalon = function(x, sep, dec){
                                
                                temp <- readData(x, sep = sep, dec = dec)

                                Num <- vapply(seq(from = 2, to = ncol(temp), by = 1),
                                			   
                                			   function(x){
                                  
                                			   	if(is.numeric(temp[1,x])){TRUE} else {FALSE}
                                			   	
                                			   	},
                                			   FUN.VALUE = logical(1)
                                			   )
                                
                                if(identical(colnames(temp)[2:ncol(temp)],colnames(self$standards[[1]]$rep_data[[1]]$data)[2:ncol(temp)]) & length(which(Num == FALSE)) == 0){
                                  
                                  self$EtalonPath <- x                                  
                                  self$EtalonData <- readData(x, sep = sep, dec = dec)
                                  
                                } else {
                                  
                                  tkmessageBox(message = "This calibration file has not the correct structure", icon = "error", type = "ok")
                                  
                                }
                                
                              }, 
                              
                              ##################################################################################################
                              # Name: setflagMachineCorrection
                              # Function: set self$flagMachineCorrection
                              # Input: x = the numerical value to set
                              ##################################################################################################
                                                            
                              setflagMachineCorrection = function(x){
                                
                                self$flagMachineCorrection <- x
                                
                              }, 
                              
                              ##################################################################################################
                              # Name: NonNumericCheck
                              # Function: check non numeric characters of data
                              # Input: data = a dataframe or a matrix, col = a vector of numerical values corresponding to the number columns to investigate
                              # Output: errB = a numerical value corresponding to the number of cells containing non numeric characters
                              ##################################################################################################
                              
                              NonNumericCheck = function(data, col){
                                                                  
                                  errB <- 0
                                  
                                  for(i in col){

                                    for(j in seq(from = 1, to = nrow(data), by = 1)){
                                      
                                      if(!is.numeric(data[j,i])){
                                        
                                        errB <- errB +1
                                                              
                                      } else {}
                                      
                                    }
                                    
                                  }
                                
                                return(errB)
                                
                              },  
                                                          
                              ##################################################################################################
                              # Name: setflagStand  
                              # Function: set self$flag_stand
                              # Input: place = a numerical value corresponding to the considered replicate, value = the numerical value to set
                              ##################################################################################################
                              
                              setflagStand   = function(place, value){
                                
                                self$flag_stand[place] <- value
                                
                                return(self$flag_stand)
                                
                                
                              }, 
                              
                              ##################################################################################################
                              # Name: setflagSample
                              # Function: set self$flag_Sample
                              # Input: sample = a numerical value corresponding to the considered sample, replicate = a numerical value corresponding to the considered replicate, value = the numerical value to set
                              ##################################################################################################
                              
                              setflagSample = function(sample, replicate, value){
                                
                                self$flag_Sample[[sample]][replicate] <- value
                                
                                return(self$flag_Sample)
                                
                              }, 
                                                            
                              ##################################################################################################
                              # Name: setCorrection
                              # Function: set self$machineCorrection
                              # Input: x = a vector indicating the chemical elements to correct from machine drift
                              ##################################################################################################
                              
                              setCorrection = function(x){
                                
                                self$machineCorrection <- x
                                
                                
                              },
                              
                              ##################################################################################################
                              # Name: correction
                              # Function: proceed to the linear regression on standards replicates and set self$nbCalib & self$regressionModel
                              ##################################################################################################                              
                              
                              correction = function(){
                                
                                temporaryTab <- self$standards[[1]]$rep_dataFinale                              
                                
                                Nbelem <- length(self$listeElem)
                                
                                # creation of self$regressionModel, i.e. final table with all regression parameters
                                self$regressionModel <- matrix(data = NA, nrow = Nbelem, ncol = 7)
                                colnames(self$regressionModel) <- c("Norm.", "Homosc.","Indep.", "Regress.Test", "intercept","A", "R2")
                                rownames(self$regressionModel) <- self$listeElem
                                
                                # Building the linear regression
                                temp <- str_sub(rownames(temporaryTab), 1, -6)
                                
                                # creation of X (i.e.the xcoordinates), Y (i.e. the ycoordinates) of the standards values
                                X <- vector()
                                for (i in seq(from = 1, to = length(self$standardsFiles), by = 1)){
                                  X[i] <- self$standardRank[which(names(self$standardRank) == temp[i])] 
                                  
                                }
                                
                                for(j in seq(from = 1, to = Nbelem, by = 1)){
                                  Y <- temporaryTab[seq(from = 1, to = length(self$standardsFiles), by = 1),j]
                                  
                                  tempoR <- vapply(seq(from = 1, to = length(Y), by = 1), 
                                  		     function(x){  
                                  	
                                  		     	if(is.finite(Y[x])){TRUE} else {FALSE}
                                  		     	}, 
                                  		     FUN.VALUE = logical(1)
                                  		     )
                                  
                                  # self$nbCalib, i.e. a vector indicating for each element how many standard value is available
                                  self$nbCalib[j] <- length(which(tempoR == TRUE))
                                  
                                  if(self$nbCalib[j] == 0){ 
                                    
                                    self$regressionModel[j, 1:7] <- rep(NA, 7)
                                    
                                  } else if(self$nbCalib[j] == 1){
                                    
                                    res_test <- vector()
                                    
                                    toDo <- which(vapply(seq(from = 1, to = length(Y), by = 1), 
                                    			   
                                    			   function(x){
                                    			   	
                                    			   	if(is.finite(Y[x])){TRUE} else {FALSE}
                                    			   	
                                    			   	}, 
                                    			   FUN.VALUE = logical(1)
                                    			   ) == TRUE)
                                    
                                    y <- Y[toDo]
                                    
                                    slope <- 0
                                    
                                    intercept <- y
                                    
                                    res_test[1:4] <- NA
                                    res_test[5:6] <- c(intercept , slope)
                                    res_test[7] <- NA
                                    
                                    self$regressionModel[j, 1:7] <- res_test
                                    
                                  } else if(self$nbCalib[j] == 2){
                                    
                                    res_test <- vector()
                                    
                                    toDo <- which(vapply(seq(from = 1, to = length(Y), by = 1), 
                                    			   
                                    			   function(x){
                                    			   	
                                    			   	if(is.finite(Y[x])){TRUE} else {FALSE}
                                    			   	
                                    			   }, 
                                    			   FUN.VALUE = logical(1)
                                    ) == TRUE)
                                    
                                    y <- Y[toDo] # the real yvalues to build the model
                                    x <- X[toDo] # the real xvalues to build the model
                                    
                                    slope <- (y[2] - y[1])/(x[2] - x[1])
                                    
                                    intercept <- y[1] - slope*x[1]
                                    
                                    res_test[1:4] <- NA
                                    res_test[5:6] <- c(intercept , slope)
                                    res_test[7] <- NA
                                    
                                    self$regressionModel[j, 1:7] <- res_test
                                    
                                  } else if(self$nbCalib[j] == 3){ # need to differentiate self$nbCalib[j] == 3 and self$nbCalib[j] > 3 because of the hmctest
                                    
                                    if(length(which(Y != 1)) == 0){ # check that at least one value is different than the other (avoid internal standard problem)
                                      res_test <- c(NA,NA,NA,NA,1, 0, NA)
                                      self$regressionModel[j, 1:7] <- res_test
                                      
                                    } else { 
                                      model <- lm(Y~X)
                                      
                                      # tests 
                                      model.res <- model$res
                                      
                                      res_test <- vector()
                                      
                                      res_test[1] <- shapiro.test(model.res)$p.value
                                      res_test[2] <-NA
                                      res_test[3] <- dwtest(model)$p.value
                                      res_test[4] <- summary(model)$coefficients[2,4]                                      
                                      res_test[5:6] <- summary(model)$coefficients[,1]
                                      res_test[7] <- summary(model)$r.squared
                                      
                                      self$regressionModel[j, 1:7] <- res_test
                                    }
                                    
                                  } else if(self$nbCalib[j] > 3){ 
                                    
                                    if(length(which(Y != 1)) == 0){
                                      res_test <- c(NA,NA,NA,NA,1, 0, NA)
                                      self$regressionModel[j, 1:7] <- res_test
                                    } else {
                                      
                                      model <- lm(Y~X)
                                      
                                      # tests 
                                      model.res <- model$res
                                      
                                      res_test <- vector()
                                      
                                      res_test[1] <- shapiro.test(model.res)$p.value
                                      res_test[2] <- hmctest(model)$p.value                                    
                                      res_test[3] <- dwtest(model)$p.value
                                      res_test[4] <- summary(model)$coefficients[2,4]                                      
                                      res_test[5:6] <- summary(model)$coefficients[,1]
                                      res_test[7] <- summary(model)$r.squared

                                      self$regressionModel[j, 1:7] <- res_test
                                    }
                                    
                                  } else {}
                                  
                                }
                                
                                names(self$nbCalib) <- self$listeElem
                                
                              },
                              
                              ##################################################################################################
                              # Name: setRank
                              # Function: set the order in which ICPMS runs each standard (self$standardRank) and sample (self$sampleRank) replicates
                              # Input: type = a character string indicating the type of replicate standard ("standard") or sample ("sample"), value = a numerical value corresponding to the rank of the considered replicate
                              ##################################################################################################
                              
                              setRank = function(type, value){
                                
                                if(type == "standard"){
                                  self$standardRank <- value
                                }
                                if(type == "sample"){
                                  self$sampleRank <- value
                                }
                                
                              },
                              
                              ##################################################################################################
                              # Name: initialize
                              ##################################################################################################
                                                            
                              initialize = function(folderPath=NULL,  sep = ";", dec = ".") {   
                                
                                pb <- tkProgressBar("Progress bar", "Some information in %",
                                                    0, 100, 20)
                                
                                self$folderPath <- folderPath
                                charStrings <- unlist(strsplit(folderPath,"/"))                                
                                self$name <- charStrings[length(charStrings)]
                                
                                k <- 1 # a provisory flag
                                
                                ########## STEP 1: Verification of the structure and of the numerical feature of the data ######
                                
                                # Check element names and order 
                                dirTemp <- getwd()
                                
                                #variable for the structure error
                                structureError <- 0
                                structreLocation <- vector() # in which file R find an error 
                                
                                # variable for the nonNum error
                                
                                nbNumError <- 0
                                nonNumPlace <- NULL
                                
                                info <- sprintf("%d%% done", round(30))
                                setTkProgressBar(pb, 30, sprintf("Data loading (%s)", info), info)
                                
                                setwd(paste0(folderPath, "/standards"))
                                files <- list.files(, recursive = TRUE) 
                                
                                dat <- readData(files[1], sep = sep, dec = dec)
                                
                                if(ncol(dat) == 1){
                                  self$errorSession <- 1
                                  
                                } 
                                toCheck <- colnames(dat)[-1]
                                
                                self$listeElem <- toCheck
                                
                                for (i in seq(from = 1, to = length(files), by = 1)){
                                  
                                  dat <- readData(files[i], sep = sep, dec = dec)
                                  
                                  nbNumError <- self$NonNumericCheck(data = dat, col = seq(from = 1, to = ncol(dat), by = 1))
                                  
                                  if(nbNumError != 0){nonNumPlace <- c(nonNumPlace, files[i])}
                                  
                                  temp <- colnames(dat)[-1]
                                  
                                  if(!identical(toCheck, temp)){structureError <- 1; structreLocation[k] <- files[i]; k <- k+1;} else {}   
                                }
                                
                                info <- sprintf("%d%% done", round(40))
                                setTkProgressBar(pb, 40, sprintf("Data loading (%s)", info), info)
                                
                                setwd(paste0(folderPath, "/samples"))
                                files <- list.files(, recursive = TRUE)
                                
                                for (i in seq(from = 1, to = length(files), by = 1)){                                  
                                  dat <- readData(files[i], sep = sep, dec = dec)
                                  nbNumError <- self$NonNumericCheck(data = dat, col = seq(from = 1, to = ncol(dat), by = 1))
                                  
                                  if(nbNumError != 0){nonNumPlace <- c(nonNumPlace, files[i])}
                                  
                                  temp <- colnames(dat)[-1]
                                  
                                  if(!identical(toCheck, temp)){structureError <- 1; structreLocation[k] <- files[i]; k <- k+1;} else {}                                   
                                }  
                                
                                info <- sprintf("%d%% done", round(50))
                                setTkProgressBar(pb, 50, sprintf("Data loading (%s)", info), info)
                                
                                self$elementChecking <- list(structureError, structreLocation)
                                
                                self$errorSession <- nonNumPlace
                                
                                info <- sprintf("%d%% done", round(70))
                                setTkProgressBar(pb, 70, sprintf("Data loading (%s)", info), info)
                                
                                setwd(dirTemp) 
                                
                                ########## STEP 2: Creation of the project object ######
                                
                                # a. Standards
                                
                                self$standardsPath <- paste0(folderPath,"/standards")
                                calFiles <- dir(self$standardsPath)                                
                                self$standardsFiles <- calFiles
                                
                                calList <- lapply(paste0(self$standardsPath, sep=""),function(f){elementR_repStandard$new(f, sep = sep, dec = dec)})
                                names(calList) <- "Rep_standard"                          
                                self$standards <- calList
                                
                                info <- sprintf("%d%% done", round(80))
                                setTkProgressBar(pb, 80, sprintf("Data loading (%s)", info), info)
                                
                                #b. samples
                                self$samplesPath <- paste0(folderPath,"/samples")
                                sampFiles <- dir(self$samplesPath)
                                self$samplesFiles <- sampFiles    
                                
                                sampList <- lapply(paste0(self$samplesPath,"/",sampFiles),function(f){elementR_repSample$new(f, sep = sep, dec = dec)})
                                
                                
                                names(sampList) <- sampFiles
                                self$samples <- sampList 
                                
                                info <- sprintf("%d%% done", round(90))
                                setTkProgressBar(pb, 90, sprintf("Data loading (%s)", info), info)
                                
                                # c. Flags
                                self$flag_stand <- rep(0, length(self$standardsFiles))
                                names(self$flag_stand) <- self$standardsFiles
                                
                                flagTemp <- lapply(seq(from = 1, to = length(self$samplesFiles), by = 1), function(x){dir(paste0(folderPath,"/samples/",self$samplesFiles[x]))})
                                self$flag_Sample <- lapply(seq(from = 1, to = length(flagTemp), by = 1), function(x){ r <- rep(0, length(flagTemp[[x]])) ; names(r) <- flagTemp[[x]] ; r})              
                                
                                self$flagRealign <- lapply(seq(from = 1, to = length(self$samplesFiles), by = 1),function(x){
                                  temp1 <- c(0,0)
                                  names(temp1) <- c("spot", "transect")
                                  return(temp1)
                                }) # lapply
                                
                                self$summarySettings <- matrix(NA, nrow = length(self$standardsFiles)+sum(unlist(lapply(seq(from = 1, to = length(self$samplesFiles), by = 1), function(x){length(self$samples[[x]]$rep_data)}))), ncol = (ncol(dat)-1)*2 + 6)
                                
                                toInsert <- vapply(seq(from = 1, to = length(str_split(list.files(paste0(folderPath,"/samples"), recursive = TRUE),"/")), by = 1), 
                                			 function(k){
                                			 	str_split(list.files(paste0(folderPath,"/samples"), recursive = TRUE),"/")[[k]][2]
                                			 	},
                                			 FUN.VALUE = character(1))
                                
                                rownames(self$summarySettings) <- c(self$standardsFiles, toInsert)
                                if(ncol(dat) != 1){
                                  colnames(self$summarySettings) <- c("name", "Rank in analysis", "blank beginning", "blank end", "plateau beginning", "plateau end", paste("Blank average", colnames(dat)[2:ncol(dat)]), paste("LOD", colnames(dat)[2:ncol(dat)]))
                                  
                                } 
                                
                                
                                info <- sprintf("%d%% done", round(100))
                                setTkProgressBar(pb, 100, sprintf("Data loading (%s)", info), info)
                                
                                close(pb)
                                
                                

                              }
                              
                            ),#public
                            private = list(
                              aMethod = function() self$name
                            )#private
)#elementR_project
}

################################################################################
################################################################################
#################################### ElementR repertoire class ####
################################################################################
{
elementR_rep <- R6Class("elementR_rep",
                        public = list(
                          rep_name = NA, # A character string corresponding to the name of the considered folder
                          rep_folderPath = NA, # A character string corresponding to the path of the considered folder
                          rep_Files = NA, # A vector containing the name of the files within the considered folder
                          rep_data = NA, # A list containing the self$elementR_data corresponding to the replicates included the considered folder                        
                          rep_pas = NA, # A numerical value corresponding to the time between two consecutive analysis within data of the considered folder
                          sep = NA,
                          dec = NA,
                          
                          ##################################################################################################
                          # Name: setRep_pas
                          # Function: set self$rep_pas
                          ##################################################################################################
                          
                          setRep_pas = function(){
                          	
                            self$rep_pas <- round(mean(unlist(lapply(seq(from = 1, to = length(self$rep_data), by = 1),
                            						     function(x){
                            						     		vapply(seq(from = 1, to = (length(self$rep_data[[x]])-1), by = 1), 
                            						     			 function(i){
                            						     			 	self$rep_data[[x]]$data[i+1,1]-self$rep_data[[x]]$data[i,1]},
                            						     			 FUN.VALUE = numeric(1)
                            						     			 )
                            						     }
                            						     )), na.rm = TRUE),4)
                            
                            
                          },
                          
                          ##################################################################################################
                          # Name: initialize
                          ##################################################################################################
                          
                          initialize = function(rep_folderPath=NULL,  sep = ";", dec = ".") {
                            
                            charStrings <- unlist(strsplit(rep_folderPath,"/"))
                            self$rep_name <- charStrings[length(charStrings)]
                            self$rep_folderPath <- rep_folderPath
                            
                            Files <- dir(self$rep_folderPath)
                            self$rep_Files <- Files 
                            
                            self$sep <- sep
                            self$dec <- dec
                            
                            self$create()
                          }
                          
                        )
)
}

################################################################################
################################################################################
#################################### ElementR rep_Standard class ####
################################################################################
{
elementR_repStandard <- R6Class("elementR_repStandard",
                             inherit = elementR_rep,
                             public = list(                               
                               rep_type = "standard", # A character string indicating the type of the batch considered (here, "standard")
                               rep_dataFinaleMean = NA, # A vector containing the average per chemical element of the self$rep_dataFinale
                               rep_dataFinaleSD = NA,  # A vector containing the standard deviation per chemical element of the self$rep_dataFinale
                               rep_dataFinale = NA, # A matrix containing self$data_standFinalMean and self$data_standFinalSD for all standard replicates included in the considered batch
                               
                               ##################################################################################################
                               # Name: setrep_FinalMeanSD
                               # Function: define and set self$rep_dataFinaleMean and self$rep_dataFinaleSD
                               ##################################################################################################
                               
                               setrep_FinalMeanSD = function(){ 
                                                                  
                                 listTemp <- list()
                                 
                                 for(i in seq(from = 1, to = length(self$rep_Files), by = 1)){listTemp[[i]] <- self$rep_data[[i]]$data_standFinalMean}
                                                                  
                                 dataTemp <- do.call(rbind,listTemp)
                                 
                                 self$rep_dataFinaleMean <- apply(dataTemp,2, mean, na.rm = TRUE)
                                 self$rep_dataFinaleSD <- apply(dataTemp,2, sd, na.rm = TRUE)
                                 
                               }, 
                               
                               ##################################################################################################
                               # Name: setRep_table
                               # Function: set  self$rep_dataFinale
                               # Input: nelem = a vector containing the names of the chemical elements to include in the self$rep_dataFinale
                               ##################################################################################################
                               
                               setRep_table = function(nelem) {
                                 
                                 tab = matrix(0, length(self$rep_Files)*2+2, length(nelem))
                                 
                                 colnames(tab) <- nelem
                                 
                                 rownames(tab) <- c(paste(self$rep_Files, "Mean"),paste(self$rep_Files, "SD"),"Total Mean", "Total SD")
                                 
                                 for(i in seq(from = 1, to = length(self$rep_Files), by = 1)){
                                   
                                   self$rep_data[[i]]$setdata_standFinal()
                                   
                                   tab[i,] <- self$rep_data[[i]]$data_standFinalMean
                                 }                                  
                                 
                                 for(i in seq(from = 1, to = length(self$rep_Files), by = 1)){tab[i+length(self$rep_Files),] <- self$rep_data[[i]]$data_standFinalSD }
                                 
                                 self$setrep_FinalMeanSD()
                                 
                                 tab[2*length(self$rep_Files)+1,] <-self$rep_dataFinaleMean
                                 
                                 tab[2*length(self$rep_Files)+2,] <-self$rep_dataFinaleSD
                                 
                                 self$rep_dataFinale <- tab
                                 
                               },                                
                               
                               ##################################################################################################
                               # Name: create
                               # Function: create standard data and set self$rep_data
                               ##################################################################################################
                               
                               create = function(){
                                 
                                 temp <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_standard$new(f, sep = self$sep, dec = self$dec)})
                                 
                                 names(temp) <- self$rep_Files
                                 
                                 self$rep_data <- temp
                                 
                               }
                             ) # list
) # elementR_repstand
}

################################################################################
################################################################################
#################################### ElementR rep_Sample class ####
################################################################################
{
elementR_repSample <- R6Class("elementR_repSample",
                             inherit = elementR_rep,
                             public = list(
                               rep_type = "Sample", # A character string indicating the type of the considered batch (here, "sample")
                               rep_type2 = NA, # A character string corresponding to the processing mode of averaging ("transect" or "spot")
                               rep_dataFiltre = NA, # A list containing the data to average for each replicate of the considered sample (self$dataOutlierFree for spot mode and self$dataNorm for transect mode)
                               rep_dataFinalSpot = NA, # A matrix containing the average and the standard deviation per chemical element of the final replicates (i.e. chosen to be part of the final calculation)
                               rep_dataIntermRaster = NA, # A list containing the realigned self$dataNorm of the final replicates (i.e. chosen to be part of the final calculation)
                               rep_dataFinalRaster = NA, # A matrix corresponding to the averaging of the data contained in self$rep_dataIntermRaster
                               rep_autoCorrel = NA, # a vector whcth (1) laser diameter, (2) laser speed, (3) which point to keep
                               rep_dataFinalRasterNonCorr = NA, # a matrix of the final data without correlated points
                               
                               ##################################################################################################
                               # Name: set_rep_autoCorrel
                               # Function: to set the self$rep_autoCorrel
                               # Input: x = a vector whcth (1) laser diameter, (2) laser speed, (3) which point to keep
                               ##################################################################################################
                               
                               set_rep_autoCorrel = function(x){
                               	self$rep_autoCorrel <- x
                               },
                               
                               ##################################################################################################
                               # Name: set_rep_dataFinalRasterNonCorr
                               # Function: to set the self$rep_dataFinalRasterNonCorr
                               ##################################################################################################
                               
                               set_rep_dataFinalRasterNonCorr = function(){
                               	
                               	k <- self$rep_autoCorrel[3] -1
                               	autoCorrel <- self$rep_autoCorrel[1]/self$rep_autoCorrel[2]/self$rep_pas
                               	
                               	matOutput <- matrix(NA, ncol = ncol(self$rep_dataFinalRaster))
                               	
                               	for(i in seq(from = 1, to = nrow(self$rep_dataFinalRaster), by = 1)){
                               		
                               		if((i - k) %% ceiling(autoCorrel) == 0){
                               			matOutput <- rbind(matOutput, self$rep_dataFinalRaster[i,])
                               		} else {}
                               		
                               	}
                               	
                               	colnames(matOutput) <- colnames(self$rep_dataFinalRaster)
                               	
                               	self$rep_dataFinalRasterNonCorr <- matOutput[-1,]
                               	
                               	
                               },
                               
                               ##################################################################################################
                               # Name: setrep_type2
                               # Function: to set the self$rep_type2
                               # Input: x = a character string indicating spot or transect mode
                               ##################################################################################################
                               
                               setrep_type2 = function(x){
                                 self$rep_type2 <- x
                               },     
                               
                               ##################################################################################################
                               # Name: create
                               # Function: create sample data and set self$rep_data
                               ##################################################################################################
                             
                               create = function(){
                                 
                                 self$rep_data <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_sample$new(f, sep = self$sep, dec = self$dec)})
                                 
                                 names(self$rep_data) <- self$rep_Files
                               },
                               
                               ##################################################################################################
                               # Name: closest
                               # Function: find the nearest value among a vector of numerical data
                               # Input: x = a vector of numerical values, y = the investigated value
                               # Output: val = a list of two values: the nearest value and its place within the vector
                               ##################################################################################################
                               
                               closest = function(x,y){
                               	val = list()
                               	if(is.null(y)){}
                               	else if(is.na(y)){}
                               	else{
                               		val[[2]] <- which(abs(x-y) == min(abs(x-y), na.rm = TRUE))
                               		val[[1]] <- x[val[[2]]]
                               		
                               		if (length(val[[1]])!=1){val[[2]] <- min(val[[2]], na.rm = TRUE)
                               		val[[1]] <- x[val[[2]]]
                               		} else {}
                               		
                               		names(val) <- c("the nearest", "place")
                               		
                               		return(val)
                               	}
                               	
                               },
                               
                               ##################################################################################################
                               # Name: Realign2
                               # Function: Realign sequences of data
                               # Input: data = a list of matrix corresponding to the data to realign, pas = the step of time between two consecutive analysis within data of the considered sample
                               # Output: data = a list of matrix containing the realigned data
                               ##################################################################################################
                           
                               Realign2 = function(data, pas){
                                 
                                 min <- min(do.call(rbind,data)[,1]) # the miniumum time of the replicates 
                                 
                                 minPlace <- which(vapply(seq(from = 1, to = length(data), by = 1), 
                                 				function(x){
                                 					if(length(which(data[[x]][,1] == min)) == 1) {TRUE} else {FALSE}
                                 					},
                                 				FUN.VALUE = logical(1)
                                 				) == TRUE) # is the number of the repliacte that owns the min
                                 
                                 if(length(minPlace) != 1){minPlace = minPlace[1]} else{}
                                 
                                 max <- max(do.call(rbind,data)[,1]) # the maximum time of the replicates 
                                 
                                 maxPlace <- which(vapply(seq(from = 1, to = length(data), by = 1), 
                                 				function(x){
                                 					if(length(which(data[[x]][,1] == max)) == 1) {TRUE}else {FALSE}
                                 				}, 
                                 				FUN.VALUE = logical(1)
                                 				) == TRUE) # is the number of the repliacte that owns the max
                                 
                                 if(length(maxPlace) != 1){maxPlace = maxPlace[length(maxPlace)]} else {}
                                 
                                  #dataMin and dataMax the data of the replicates that owns Min and Max
                                 
                                 dataMin <- data[[minPlace]]
                                 
                                 dataMax <- data[[maxPlace]]
                                 
                                 dimMax <- NULL
                                 
                                 for(i in seq(from = 1, to = length(data), by = 1)){
                                   
                                   temp <- data[[i]]
                                   
                                   #replace the missing values by NA
                                   ## Here Mark suggests to change by the round of the ||A[0] - B[0]|| / 2 assuming that A[0] > B[0]
                                   
                                   while(abs(dataMin[1,1] - temp[1,1]) < abs(dataMin[1,1] - temp[2,1])){
                                     
                                     temp <- rbind(c(temp[1,1]-pas,rep(NA,dim(dataMin)[2]-1)),temp)
                                     
                                   }
                                   
                                   if(self$closest(temp[,1], dataMin[1,1])$place != 1){
                                     
                                     temp <- temp[-1,]
                                     
                                   }
                                   
                                   data[[i]] <- temp
                                   
                                   dimMax <- c(dimMax, dim(temp)[1])
                                   
                                 }
                                 
                                 dimMax <- max(dimMax)
                                 
                                 for(j in seq(from = 1, to = length(data), by = 1)){
                                   
                                   if(dim(data[[j]])[1] < dimMax){
                                     
                                     ToAdd <-dimMax - dim(data[[j]])[1]
                                     
                                     for (i in seq(from = 1, to = ToAdd, by = 1)){
                                       
                                       temp <- rbind(data[[j]], c((data[[j]][(dim(data[[j]])[1]),1]+pas),rep(NA,(ncol(data[[1]])[1]-1))))
                                       
                                       data[[j]] <- temp
                                     }
                                     
                                   }
                                   
                                 }
                                 
                                 return(data)                              
                               },
                               
                               ##################################################################################################
                               # Name: setRep_dataFiltre
                               # Function: set self$rep_dataFiltre
                               # Input: x = the choice of user to correct or not the machine drift
                               ##################################################################################################
                               
                               setRep_dataFiltre = function(x){
                                 
                                 if(x == TRUE){
                                   self$rep_dataFiltre <- lapply(seq(from = 1, to = length(self$rep_Files), by = 1),function(x){
                                   	self$rep_data[[x]]$dataConcCorr
                                   	})
                                 } else {
                                   self$rep_dataFiltre <- lapply(seq(from = 1, to = length(self$rep_Files), by = 1),function(x){self$rep_data[[x]]$dataConc})
                                 }
                                 
                                 names(self$rep_dataFiltre) <- self$rep_Files
                                                                  
                               },
                               
                               ##################################################################################################
                               # Name: setRep_dataFinalSpot
                               # Function: set self$rep_dataFinalSpot
                               # Input: x = the matrix to set
                               ##################################################################################################
                               
                               setRep_dataFinalSpot = function(x){
                                 
                                 self$rep_dataFinalSpot <- x
                               },
                               
                               ##################################################################################################
                               # Name: intermStepSpot
                               # Function: create and return an intermediate matrix containing the average and the standard deviation per chemical element for all sample replicates
                               # Output: outputTab = a matrix with two lines corresponding to the average and the standard deviation per chemical element for all sample replicates
                               ##################################################################################################
                               
                               intermStepSpot = function(){
                                 
                                 outputTab <- rbind(t(as.matrix(vapply(seq(from = 1, to = length(self$rep_Files), by = 1), 
                                 						  function(x){apply(self$rep_dataFiltre[[x]][,-1],2, mean,na.rm = TRUE)},
                                 						  FUN.VALUE = double(ncol(self$rep_dataFiltre[[1]])-1)
                                 						  )
                                 					 )
                                 			   ),t(as.matrix(vapply(seq(from = 1, to = length(self$rep_Files), by = 1),
                                 			   			   function(x){apply(self$rep_dataFiltre[[x]][,-1],2, sd,na.rm = TRUE)},
                                 			   			   FUN.VALUE  = double(ncol(self$rep_dataFiltre[[1]])-1)
                                 			   			   	)
                                 			   		  )
                                 			       )
                                 			 )
                                 
                                 namesCol <- c(paste0("Mean_", self$rep_Files),paste0("SD_", self$rep_Files))                                   
                                 
                                 rownames(outputTab) <- namesCol                      
                                 
                                 return(outputTab) 
                               },
                               
                               ##################################################################################################
                               # Name: intermStepRaster
                               # Function: create and return an intermediate matrix containing realigned data for all sample replicates
                               # (i.e. realignment done but the matrix has to be put at the same time afterward to average them)
                               # decalage = vector with the shift 
                               # Input = the replicates to keep 
                               # outliers = list of the outliers to replace
                               # replace =  the value that replace the outliers
                               # Output: outputList = a list of matrix containing realigned data
                               ##################################################################################################
                               
                               intermStepRaster = function(decalage, input, outliers, replace){
                                 
                                 self$setRep_pas()
                                 
                                 #  Create the shift
                                 
                                 tabTemp <-lapply(seq(from = 1, to = length(self$rep_dataFiltre), by = 1), function(x){
                                   
                                   temp <- self$rep_dataFiltre[[x]]
                                   
                                   if(length(which(names(self$rep_dataFiltre)[x] == names(decalage))) != 0){
                                   	
                                   	temp[,1] <- temp[,1] + decalage[which(names(self$rep_dataFiltre)[x] == names(decalage))] * self$rep_pas
                                   	
                                   	return(temp) 
                                   	
                                   } else {}
                                   
                                 })
                                 
                                 names(tabTemp) <- names(self$rep_dataFiltre)    
                                 
                                 # Only keeps the replicates chosen by the user 
                                                          
                                 outputList <- lapply(seq(from = 1, to = length(input), by = 1), function(x){
                                 	
                                 	if(length(which(names(tabTemp) == input[x])) != 0){
                                 	  
                                 		tabTemp[[which(names(tabTemp) == input[x])]]
                                 	  
                                 	} else {}
                                 	
                                 })
                                 
                                 if(!is.null(tabTemp[[1]])){
                                   
                                 	names(outputList) <- vapply(seq(from = 1, to = length(input), by = 1), 
                                 	                            
                                 					    function(x){
                                 					    	names(tabTemp)[which(names(tabTemp) == input[x])]
                                 					    }, 
                                 					    FUN.VALUE = character(1)
                                 	)
                                 }
                                 
                                 # Remove the outliers 
                                 
                                 if(!is.null(outliers)){
                                 
                                 	for(x in seq(from = 1, to = length(outputList), by = 1)){
                                 		
                                 		for(i in seq(from = 2, to  = length(outliers), by = 1)){
                                 			if(length(outliers[[i]]) != 0){
                                 				for(j in seq(from = 1, to = length(outliers[[i]]), by = 1)){
                                 					
                                 					if(length(outliers[[i]][j]) != 0){
                                 						
                                 						if(is.numeric(outliers[[i]][j]) & !is.na(outliers[[i]][j])){
                                 							
                                 							toDelete <- which(round(outputList[[x]][,i],16) == round(outliers[[i]][j], 16))
                                 							
                                 							outputList[[x]][toDelete,i] <- replace
                                 							
                                 						}
                                 						
                                 					}
                                 				}
                                 			}
                                 			
                                 		
                                 			
                                 		}
                                 	}

                                 	
                                 } else {}
                                 
                                 return(outputList)
                               },
                               
                               ##################################################################################################
                               # Name: setRep_dataIntermRaster
                               # Function: set self$setRep_dataIntermRaster
                               # Input:  x = the list of matrix to set
                               ##################################################################################################
                               
                               setRep_dataIntermRaster = function(x){
                                 self$rep_dataIntermRaster <- x
                               },
                               
                               ##################################################################################################
                               # Name: setRep_dataFinalRaster
                               # Function: set self$rep_dataFinalRaster
                               ##################################################################################################
                               
                               setRep_dataFinalRaster = function(){
                                 
                                 MatTemp <- self$Realign2(data = self$rep_dataIntermRaster, pas = self$rep_pas)
                                 
                                 MatTemp <- abind(MatTemp,along=3)                                   
                                 
                                 MatTemp <- apply(MatTemp,c(1,2),mean,na.rm=TRUE)
                                 
                                 colnames(MatTemp) <- colnames(self$rep_data[[1]]$data)
                                 
                                 self$rep_dataFinalRaster <- MatTemp
                               },                               

                               ##################################################################################################
                               # Name: RealignCol
                               # Function: realign two tables according to one column
                               # Input: 
                               # 	dat1 & dat2: matrix to realign
                               # 	col: the column to realign
                               # 	step: the step between two consecutive analysis
                               ##################################################################################################
                               
                               RealignCol = function(dat1, dat2, col, step){
                               	
                               	dat1[is.na(dat1[,col]),col] <- 0
                               	dat2[is.na(dat2[,col]),col] <- 0
                               	
                               	save1 <- dat2[1,1]
                               	
                               	N <- which(convolve(dat2[,col], dat1[,col], type = "open") == max(convolve(dat2[,col], dat1[,col], type = "open")))[1] - 1
                               	
                               	essN <- dat2[,1] + (length(min(dat2[,1]) : max(dat1[,1])) - 1) - N*step
                               	
                               	dat2[,1] <- essN
                               	
                               	save2 <- dat2[1,1]
                               	
                               	saveDisplace <- round((save2 - save1)/step)
                               	
                               	data <- list(dat1, dat2, saveDisplace)
                               	
                               	return(data)
                               },
                               
                               ##################################################################################################
                               # Name: RealignColList
                               # Function: realign many tables according to one column
                               # Input: 
                               # 	listRealig a list of matrix to realign
                               # 	col: the column to realign
                               # 	step: the step between two consecutive analysis
                               ##################################################################################################
                               
                               RealignColList = function(listRealig, col, step){
                               	
                               	realignList <- lapply(seq(from = 1, to = length(listRealig), by = 1), function(x){
                               		
                               		if(x == 1){
                               			
                               			dat1 <- listRealig[[1]]
                               			dat2 <- listRealig[[1]]
                               			
                               			self$RealignCol(dat1, dat2, col, step)[[2]]
                               			
                               		}else {
                               			
                               			dat1 <- listRealig[[1]]
                               			dat2 <- listRealig[[x]]
                               			
                               			self$RealignCol(dat1, dat2, col, step)[[2]]
                               		}
                               	})
                               	
                               	names(realignList) <- self$rep_Files
                               	
                               	realignDisplacement <- vapply(seq(from = 1, to = length(listRealig), by = 1), 
                               						function(x){
                               							if(x == 1){0} else {
                               								dat1 <- listRealig[[1]]
                               								dat2 <- listRealig[[x]]
                               								
                               								self$RealignCol(dat1, dat2, col, step)[[3]]
                               								}
                               							}, 
                               						FUN.VALUE = numeric(1)
                               		)
                               	
                               	names(realignDisplacement) <- self$rep_Files
                               	
                               	return(list(realignList, realignDisplacement))
                               },
                               
                               ##################################################################################################
                               # Name: RealignAll
                               # Function: realign two tables according to all columns
                               # Input: 
                               # 	dat1 & dat2: matrix to realign
                               # 	step: the step between two consecutive analysis
                               ##################################################################################################
                               
                               RealignAll = function(dat1, dat2, step){
                               	
                               	listConv <- vapply(seq(from = 2, to = ncol(dat1), by = 1),
                               				 function(x){                               		
                               				 	dat1[is.na(dat1[,x]),x] <- 0
                               				 	dat2[is.na(dat2[,x]),x] <- 0
                               				 	
                               				 	convolve(dat2[,x], dat1[,x], type = "open")
                               				 	},
                               				 FUN.VALUE = numeric(1)
                               				 )
                               	
                               	convResult <- apply(listConv, 1, sum)
                               	
                               	N <- which(convResult == max(convResult))[1] -1
                               	
                               	essN <- dat2[,1] + (length(min(dat2[,1]) : max(dat1[,1])) - 1) - N*step
                               	
                               	dat2[,1] <- essN
                               	
                               	data <- list(dat1, dat2)
                               	
                               	return(data)
                               },
                               
                               ##################################################################################################
                               # Name: RealignListAll
                               # Function: realign many tables according to all columns
                               # Input: 
                               # 	listRealig a list of matrix to realign
                               # 	step: the step between two consecutive analysis
                               ##################################################################################################
                               
                               RealignListAll = function(listRealig, step){
                               	
                               	realignList <- lapply(seq(from = 1, to = length(listRealig), by = 1), function(x){
                               		
                               		if(x == 1){
                               			
                               			dat1 <- listRealig[[1]]
                               			dat2 <- listRealig[[1]]
                               			
                               			self$RealignAll(dat1, dat2, step)[[2]]
                               			
                               		}else {
                               			
                               			dat1 <- listRealig[[1]]
                               			dat2 <- listRealig[[x]]
                               			
                               			self$RealignAll(dat1, dat2, step)[[2]]
                               		}
                               	})
                               	
                               	names(realignList) <- self$rep_Files
                               	
                               	return(realignList)
                               	
                               }
                               
                               
                               
                             ) # list
) # elementR_repstand
}