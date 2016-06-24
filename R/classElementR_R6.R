##############################################################
#
# elementR 1.0 - 10/04/2016
# 
# charlottesirot@free.fr
# francois.guilhaumon@ird.fr
#
#####################################################################

readData <- function(x){
  
  if(str_detect(x, ".xls")){
    df = read.xls (x, sheet = 1, header = TRUE, dec = ".")
  } else {}
  if(str_detect(x, ".csv")){
    df = read.table(x, header = TRUE, sep = ";", dec = ".")
  } else {}
  if(str_detect(x, ".ods")){
    df = read.gnumeric.sheet(x, head = TRUE, sheet.name="data", dec = ".")
  } else {}
  return(df)
}

############################################################
############################################################
##################################### elementR_data Class
############################################################
############################################################

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
                           LOD = NA, # A vector of numerical values corresponding to the limit of detection for each chemical element of the considered replicate
                           BlankAverarge = NA, # A vector of numerical values corresponding to the averaged blank values for each chemical element of the considered replicate
                           remplaceValue = NA, # A character string corresponding to the value replacing the self$dataSuppBlank below the limit of detection
                           
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
                           
                           initialize = function(fPath=NULL) {
                             if(is.null(fPath)) stop("error, fPath missing !")
                             charStrings <- unlist(lapply(strsplit(fPath,"[.]"),strsplit,split="/"))
                             self$name <- charStrings[length(charStrings)-1] 
                             self$fPath <- fPath
                             d <- readData(fPath)
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
                             
                             self$LOD <- 3*apply(self$dataBlank[,-1], 2, sd, na.rm =TRUE)
                             
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
                                                          
                             subDat <- sapply(1:length(apply(self$dataBlank[,-1], 2, mean, na.rm = TRUE)),function(x){                              
                               
                               self$dataPlateau[,x+1] - tempo[x]  
                               
                             })
                                                          
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
                             } else if(rempl == "NA"){
                                 self$remplaceValue <- rep(NA, nrow(self$dataSuppBlank))
                               } else if(rempl == "0"){
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
                             
                             subDat <- sapply(2:ncol(self$dataSupLOD),function(x){ 
                               
                               self$dataSupLOD[,x]/self$dataSupLOD[,grep(self$elemstand,colnames(self$dataSupLOD))]
                               
                             })
                                                          
                             subDat <- cbind(as.matrix(self$dataSupLOD[,1]),subDat)                             
                             
                             colnames(subDat) <- colnames(self$dataSupLOD)
                             
                             self$dataNorm <- subDat
                             
                           }#setDataNorm   
                         )

)#elementR_data

############################################################
############################################################
################################# elementR_standard Class
############################################################
############################################################

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
                                  # Input: bins = bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau
                                  ##################################################################################################
                                  
                                  setDataOutlierFree = function(bins, plat, rempl){
                                    
                                    self$setDataNorm(bins,plat, rempl)
                                    
                                    ValMax <- apply(self$dataNorm[,-1], 2, function(k){mean(k, na.rm = TRUE) + 2*sd(k,na.rm = TRUE)})
                                                                        
                                    ValMin <- apply(self$dataNorm[,-1], 2, function(k){mean(k, na.rm = TRUE) - 2*sd(k,na.rm = TRUE)})
                                                                        
                                    subDat <- do.call(rbind,lapply(1:dim(self$dataNorm[,-1])[1], function(z){
                                      
                                      l <- self$dataNorm[z,-1]
                                      l[l < ValMin | l > ValMax] <- NA
                                      l
                                      
                                    }))
                                    
                                    self$dataOutlierFree <- cbind(as.matrix(self$dataNorm[,1]),subDat)
                                    
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
                                  
                                  getData = function(curve, bins, plat, rempl){
                                    
                                    if(curve =="Blank") {self$setDataBlanc(bins = bins)
                                                         return(self$dataBlank)} else {}
                                    
                                    if(curve =="Raw") {return(self$data) } else {}
                                    
                                    if(curve =="Plateau") {self$setDataPlateau(plat = plat, bins = bins)
                                                           return(self$dataPlateau)} else {}
                                    
                                    if(curve =="Blank removed") {self$setDataSuppBlank(bins = bins,plat = plat)
                                                                 return(self$dataSuppBlank) } else {}
                                    
                                    if(curve =="> LOD") {self$setDataSupLOD(bins = bins,plat = plat, rempl = rempl)
                                                         return(self$dataSupLOD) } else {}
                                    
                                    if(curve =="Normalized") {self$setDataNorm(bins = bins,plat = plat, rempl = rempl)
                                                              return(self$dataNorm) } else {}
                                    
                                    if(curve =="Outliers free") {self$setDataOutlierFree(bins = bins,plat = plat, rempl = rempl)
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


############################################################
############################################################
################################# elementR_sample Class
############################################################
############################################################

elementR_sample <- R6Class("elementR_sample",
                           inherit = elementR_data,
                           public = list(
                             type = "sample", # A character string corresponding to the type of replicate (here, "sample")
                             dataConc = NA, # A matrix corresponding to the self$dataNorm converted in concentration
                             dataConcCorr = NA, # A matrix corresponding to the self$dataConc corrected (or not) from the machine drift
                             
                             ##################################################################################################
                             # Name: setDataConc
                             # Function: set self$dataConc
                             # Input:  bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau, calibFile = a matrix corresponding to the data of the calibration file, meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session, rempl = the value replacing data if below the limit of detection
                             ##################################################################################################
                             
                             setDataConc = function(bins, plat, calibFile, meanStand, rempl){
                               
                               self$setDataNorm(bins = bins, plat = plat, rempl = rempl)
                                                           
                               temp <- sapply(2:ncol(self$dataNorm), function(x){

                                 self$dataNorm[,x] * calibFile[1,x]/ meanStand[x-1]
                                 
                                 })
                               
                               self$dataConc <- cbind(as.matrix(self$dataNorm[,1]),temp)
                               
                               colnames(self$dataConc) <- colnames(self$dataNorm)
                               
                             }, #setDataConc
                             
                             ##################################################################################################
                             # Name: setDataConcCorr
                             # Function: set self$dataConcCorr
                             # Input: bins = a numerical value corresponding to the time at which end the blank values, plat = a vector of two numerical values corresponding respectively to the time at which begin and end the plateau, name = a character string corresponding to the name of the sample replicates, calibFile = a matrix corresponding to the the calibration file, meanStand = a vector containing the averaged signal intensity per chemical element for all standard replicates of the running session, rank = a vector containing the rank of each sample in ICPMS analysis, correction = a vector indicating the chemical elements to correct from machine drift, model = the matrix containing the parameters of the linear regression corresponding to machine drift for all chemical elements
                             ##################################################################################################

                             setDataConcCorr = function(bins, plat, name, calibFile, meanStand, rank, model, correction, rempl){
                               
                               self$setDataConc(bins = bins, plat = plat, calibFile = calibFile, meanStand = meanStand, rempl = rempl)
                               
                               rank <- rank[which(names(rank) == name)]
                               
                               temp <- sapply(2:ncol(self$dataNorm), function(x){
                                 
                                 if(correction[x-1] == FALSE){
                                   
                                   return(self$dataNorm[,x] * calibFile[1,x]/ meanStand[x-1])
                                   
                                 }
                                 if(correction[x-1] == TRUE){
                                   
                                   StandTheoric <- model[x-1,5] + rank * model[x-1, 6]
                                   
                                   return(self$dataNorm[,x] * calibFile[1,x] / StandTheoric)
                                   
                                 }           
                               })
                               
                               
                               
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
                             
                             getData = function(curve, bins, plat, name, meanStand, rank, model, calibFile, correction, rempl){
                              
                               if(curve =="Blank") {self$setDataBlanc(bins = bins)
                                                         return(self$dataBlank)}
                               
                               if(curve =="Raw") {return(self$data) }
                               
                               if(curve =="Plateau") {self$setDataPlateau(plat = plat, bins = bins)
                                                           return(self$dataPlateau)}
                                                              
                               if(curve =="Blank removed") {self$setDataSuppBlank(bins = bins, plat = plat)
                                                                   return(self$dataSuppBlank) }
                                                              
                               if(curve =="> LOD") {self$setDataSupLOD(bins = bins, plat = plat, rempl = rempl)
                                                         return(self$dataSupLOD) }
                                                              
                               if(curve =="Normalized") {self$setDataNorm(bins = bins, plat = plat, rempl = rempl)
                                                             return(self$dataNorm) } 
                                                              
                               if(curve =="Concentration") {self$setDataConc(bins = bins, plat = plat, calibFile = calibFile, meanStand = meanStand, rempl = rempl)
                                                                 return(self$dataConc) }
                               
                               if(curve == "Conc. corrected") {self$setDataConcCorr(bins = bins, plat, name, calibFile = calibFile, meanStand = meanStand, rank = rank, model = model, correction = correction, rempl = rempl)
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

############################################################
############################################################
################################# elementR_project Class
############################################################
############################################################

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
                              flagRealign = list(), # A list vectors indicating which samples have been realigned or averaged (raster and spot mode)
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
                              
                              ##################################################################################################
                              # Name: set_summarySettings
                              # Function: set self$summarySettings
                              # inputs: name = a character string corresponding to the name of the replicate to set, rank= its rank in ICPMS analysis, bins = A vector of numerical values corresponding to the time at which begins and ends the blank values, plat1 = a numerical value corresponding to the time at which begin the plateau values, plat2 = a numerical value corresponding to the time at which end the plateau values, average =  a vector corresponding to the blank averaged value (here, self$BlankAverarge) for each chemical element of the considered replicate, LOD = a vector corresponding to the limit of detection (here, self$LOD) for each chemical element of the considered replicate
                              ##################################################################################################
                              
                              set_summarySettings = function(name, rank, bins1, bins2, plat1, plat2, average, LOD){

                                self$summarySettings[which(rownames(self$summarySettings) == name),] <- c(name, rank, bins1, bins2, plat1,plat2, average, LOD)
                     
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
                                  val[[2]] = which(abs(x-y) == min(abs(x-y), na.rm = TRUE))
                                  val[[1]] = x[val[[2]]]
                                  
                                  if (length(val[[1]])!=1){val[[2]] = min(val[[2]], na.rm = TRUE)
                                                            val[[1]] = x[val[[2]]]
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
                                lapply(1:length(self$standards[[1]]$rep_data), function(x){
                                  self$standards[[1]]$rep_data[[x]]$setElemStand(elem)
                                })
                                
                                #transmit to samples
                                lapply(1:length(self$samples), function(x){
                                  lapply(1: length(self$samples[[x]]$rep_data), function(y){
                                    self$samples[[x]]$rep_data[[y]]$setElemStand(elem)
                                  })                                  
                                })
                              }, 
                              
                              ##################################################################################################
                              # Name: set_flagRealign 
                              # Function: set self$flagRealign
                              # Input: replicate = a numerical value corresponding to the number of the considered replicate, type = a character string indicating the raster or spot mode, value = the numerical value to set
                              ##################################################################################################
                              
                              set_flagRealign = function(replicate, type, value){
                                
                                if(type == "spot"){
                                  
                                  self$flagRealign[[replicate]][1] <- value
                                  
                                } else if(type == "raster"){ 
                                  
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
                                invisible(lapply(1:length(Mean), function(x){
                                  segments(coord[x], Mean[x]-SD[x], coord[x], Mean[x]+SD[x])
                                }))
                                invisible(lapply(1:length(Mean), function(x){
                                  segments((coord[x]-lengthSeg),Mean[x]+SD[x],(coord[x]+lengthSeg),Mean[x]+SD[x])
                                }))
                                invisible(lapply(1:length(Mean), function(x){
                                  segments((coord[x]-lengthSeg),Mean[x]-SD[x],(coord[x]+lengthSeg),Mean[x]-SD[x])
                                }))
                              }, 
                              
                              ##################################################################################################
                              # Name: setEtalon
                              # Function: ddefine self$EtalonPath and self$EtalonData and check the validity of their data structure
                              # Input:  x = a character string corresponding to the path of the calibration file
                              ##################################################################################################
                              
                              setEtalon = function(x){
                                
                                temp <- readData(x)

                                Num <- unlist(sapply(2:ncol(temp), function(x){
                                  
                                  if(is.numeric(temp[1,x])){
                                    
                                  } else {FALSE}
                                    
                                }))
                                
                                if(identical(colnames(temp)[2:ncol(temp)],colnames(self$standards[[1]]$rep_data[[1]]$data)[2:ncol(temp)]) & is.null(Num)){
                                  
                                  self$EtalonPath <- x                                  
                                  self$EtalonData <- readData(x)
                                  
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

                                    for(j in 1:nrow(data)){
                                      
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
                                
                                self$regressionModel <- matrix(data = NA, nrow = Nbelem, ncol = 6)
                                colnames(self$regressionModel) <- c("Norm.", "Homosc.","Indep.", "Regress.Test", "intercept","A")
                                rownames(self$regressionModel) <- self$listeElem
                                
                                # construction du model
                                temp <- str_sub(rownames(temporaryTab), 1, -6)
                                
                                X <- vector()
                                for (i in 1:length(self$standardsFiles)){
                                  X[i] <- self$standardRank[which(names(self$standardRank) == temp[i])] 
                                  
                                }
                                
                                for(j in 1:(Nbelem)){
                                  Y <- temporaryTab[1:length(self$standardsFiles),j]
                                  
                                  tempoR <- sapply(1:length(Y), function(x){                                    
                                    if(is.finite(Y[x])){TRUE
                                    } else {FALSE}
                                  })
                                  
                                  self$nbCalib[j] <- length(which(tempoR == TRUE))
                                  
                                  if(self$nbCalib[j] == 0){ 
                                    
                                    self$regressionModel[j, 1:6] <- rep(NA, 6)
                                    
                                  } else if(self$nbCalib[j] == 1){
                                    
                                    res_test <- vector()
                                    
                                    tempNum <- which(sapply(1:length(Y), function(x){
                                      
                                      if(is.finite(Y[x])){TRUE
                                      } else {FALSE}
                                      
                                    }) == TRUE)
                                    
                                    toDo <- tempNum
                                    
                                    y <- Y[toDo]
                                    
                                    slope <- 0
                                    
                                    intercept <- y
                                    
                                    res_test[1:4] <- NA
                                    res_test[5:6] <- c(intercept , slope)
                                    
                                    self$regressionModel[j, 1:6] <- res_test
                                    
                                  } else if(self$nbCalib[j] == 2){
                                    
                                    res_test <- vector()
                                    
                                    tempNum <- which(sapply(1:length(Y), function(x){
                                      
                                      if(is.finite(Y[x])){TRUE}
                                      else{FALSE}                                    
                                      
                                    }) == TRUE)
                                    
                                    toDo <- tempNum
                                    
                                    y <- Y[toDo]
                                    x <- X[toDo]
                                    
                                    slope <- (y[2] - y[1])/(x[2] - x[1])
                                    
                                    intercept <- y[1] - slope*x[1]
                                    
                                    res_test[1:4] <- NA
                                    res_test[5:6] <- c(intercept , slope)
                                    
                                    self$regressionModel[j, 1:6] <- res_test
                                    
                                  } else if(self$nbCalib[j] == 3){
                                    
                                    if(length(which(Y != 1)) == 0){
                                      res_test <- c(NA,NA,NA,NA,1, 0)
                                      self$regressionModel[j, 1:6] <- res_test
                                      
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
                                      
                                      self$regressionModel[j, 1:6] <- res_test
                                    }
                                    
                                  } else if(self$nbCalib[j] > 3){
                                    
                                    if(length(which(Y != 1)) == 0){
                                      res_test <- c(NA,NA,NA,NA,1, 0)
                                      self$regressionModel[j, 1:6] <- res_test
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
                                      
                                      self$regressionModel[j, 1:6] <- res_test
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
                                                            
                              initialize = function(folderPath=NULL) {   
                                
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
                                
                                dat <- readData(files[1])
                                toCheck <- colnames(dat)[-1]
                                
                                self$listeElem <- toCheck
                                
                                for (i in 1: length(files)){
                                  
                                  dat <- readData(files[i])
                                  nbNumError <- self$NonNumericCheck(data = dat, col = 1:ncol(dat))
                                  
                                  if(nbNumError != 0){nonNumPlace <- c(nonNumPlace, files[i])}
                                  
                                  temp <- colnames(dat)[-1]
                                  
                                  if(!identical(toCheck, temp)){structureError <- 1; structreLocation[k] <- files[i]; k <- k+1;} else {}   
                                }

                                info <- sprintf("%d%% done", round(40))
                                setTkProgressBar(pb, 40, sprintf("Data loading (%s)", info), info)
                                
                                setwd(paste0(folderPath, "/samples"))
                                files <- list.files(, recursive = TRUE)
                                
                                for (i in 1: length(files)){                                  
                                  dat <- readData(files[i])
                                  nbNumError <- self$NonNumericCheck(data = dat, col = 1:ncol(dat))
                                  
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
                                
                                calList <- lapply(paste0(self$standardsPath, sep=""),function(f){elementR_repStandard$new(f)})
                                names(calList) <- "Rep_standard"                          
                                self$standards <- calList
                                
                                info <- sprintf("%d%% done", round(80))
                                setTkProgressBar(pb, 80, sprintf("Data loading (%s)", info), info)
                                
                                #b. samples
                                self$samplesPath <- paste0(folderPath,"/samples")
                                sampFiles <- dir(self$samplesPath)
                                self$samplesFiles <- sampFiles                                
                                
                                sampList <- lapply(paste0(self$samplesPath,"/",sampFiles),function(f){elementR_repSample$new(f)})
                                names(sampList) <- sampFiles
                                self$samples <- sampList 
                                
                                info <- sprintf("%d%% done", round(90))
                                setTkProgressBar(pb, 90, sprintf("Data loading (%s)", info), info)
                                
                                # c. Flags
                                self$flag_stand <- rep(0, length(self$standardsFiles))
                                names(self$flag_stand) <- self$standardsFiles
                                
                                flagTemp <- lapply(1:length(self$samplesFiles), function(x){dir(paste0(folderPath,"/samples/",self$samplesFiles[x]))})
                                self$flag_Sample <- lapply(1: length(flagTemp), function(x){ r <- rep(0, length(flagTemp[[x]])) ; names(r) <- flagTemp[[x]] ; r})              
                                
                                self$flagRealign <- lapply(1:length(self$samplesFiles),function(x){
                                  temp1 <- c(0,0)
                                  names(temp1) <- c("spot", "raster")
                                  return(temp1)
                                }) # lapply
                                
                                self$summarySettings <- matrix(NA, nrow = length(self$standardsFiles)+sum(unlist(lapply(1:length(self$samplesFiles), function(x){length(self$samples[[x]]$rep_data)}))), ncol = (ncol(dat)-1)*2 + 6)
                                
                                toInsert <- sapply(1:length(str_split(list.files(paste0(folderPath,"/samples"), recursive = T),"/")), function(k){
                                  str_split(list.files(paste0(folderPath,"/samples"), recursive = T),"/")[[k]][2]
                                })
                                
                                rownames(self$summarySettings) <- c(self$standardsFiles, toInsert)
                                colnames(self$summarySettings) <- c("name", "Rank in analysis", "blank beginning", "blank end", "plateau beginning", "plateau end", paste("Blank average", colnames(dat)[2:ncol(dat)]), paste("LOD", colnames(dat)[2:ncol(dat)]))

                                info <- sprintf("%d%% done", round(100))
                                setTkProgressBar(pb, 100, sprintf("Data loading (%s)", info), info)
                                
                                close(pb)
                              }
                            ),#public
                            private = list(
                              aMethod = function() self$name
                            )#private
)#elementR_project

####################################################################
####################################################################
#################################### ElementR repertoire class ####
####################################################################

elementR_rep <- R6Class("elementR_rep",
                        public = list(
                          rep_name = NA, # A character string corresponding to the name of the considered folder
                          rep_folderPath = NA, # A character string corresponding to the path of the considered folder
                          rep_Files = NA, # A vector containing the name of the files within the considered folder
                          rep_data = NA, # A list containing the self$elementR_data corresponding to the replicates included the considered folder                        
                          rep_pas = NA, # A numerical value corresponding to the time between two consecutive analysis within data of the considered folder
                          
                          ##################################################################################################
                          # Name: setRep_pas
                          # Function: set self$rep_pas
                          ##################################################################################################
                          
                          setRep_pas = function(){
                            
                            self$rep_pas <- round(mean(unlist(lapply(1:length(self$rep_data),function(x){sapply(1:(length(self$rep_data[[x]])-1), function(i){self$rep_data[[x]]$data[i+1,1]-self$rep_data[[x]]$data[i,1]})})), na.rm = TRUE),4)
                            
                          },
                          
                          ##################################################################################################
                          # Name: initialize
                          ##################################################################################################
                          
                          initialize = function(rep_folderPath=NULL) {
                            
                            charStrings <- unlist(strsplit(rep_folderPath,"/"))
                            self$rep_name <- charStrings[length(charStrings)]
                            self$rep_folderPath <- rep_folderPath
                            
                            Files <- dir(self$rep_folderPath)
                            self$rep_Files <- Files 
                            
                            self$create()
                          }
                          
                        )
)

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
                                 
                                 for(i in 1:length(self$rep_Files)){listTemp[[i]] <- self$rep_data[[i]]$data_standFinalMean}
                                                                  
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
                                 
                                 for(i in 1:length(self$rep_Files)){
                                   
                                   self$rep_data[[i]]$setdata_standFinal()
                                   
                                   tab[i,] <- self$rep_data[[i]]$data_standFinalMean
                                 }                                  
                                 
                                 for(i in 1:length(self$rep_Files)){tab[i+length(self$rep_Files),] <- self$rep_data[[i]]$data_standFinalSD }
                                 
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
                                 
                                 temp <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_standard$new(f)})
                                 
                                 names(temp) <- self$rep_Files
                                 
                                 self$rep_data <- temp
                                 
                               }
                             ) # list
) # elementR_repstand

elementR_repSample <- R6Class("elementR_repSample",
                             inherit = elementR_rep,
                             public = list(
                               rep_type = "Sample", # A character string indicating the type of the considered batch (here, "sample")
                               rep_type2 = NA, # A character string corresponding to the processing mode of averaging ("raster" or "spot")
                               rep_dataFiltre = NA, # A list containing the data to average for each replicate of the considered sample (self$dataOutlierFree for spot mode and self$dataNorm for raster mode)
                               rep_dataFinalSpot = NA, # A matrix containing the average and the standard deviation per chemical element of the final replicates (i.e. chosen to be part of the final calculation)
                               rep_dataIntermRaster = NA, # A list containing the realigned self$dataNorm of the final replicates (i.e. chosen to be part of the final calculation)
                               rep_dataFinalRaster = NA, # A matrix corresponding to the averaging of the data contained in self$rep_dataIntermRaster
                               
                               ##################################################################################################
                               # Name: setrep_type2
                               # Function: to set the self$rep_type2
                               # Input: x = a character string indicating spot or raster mode
                               ##################################################################################################
                               
                               setrep_type2 = function(x){
                                 self$rep_type2 <- x
                               },     
                               
                               ##################################################################################################
                               # Name: create
                               # Function: create sample data and set self$rep_data
                               ##################################################################################################
                             
                               create = function(){
                                 
                                 self$rep_data <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_sample$new(f)})
                                 
                                 names(self$rep_data) <- self$rep_Files
                               },
                               
                               ##################################################################################################
                               # Name: Realign2
                               # Function: Realign sequences of data
                               # Input: data = a list of matrix corresponding to the data to realign, pas = the step of time between two consecutive analysis within data of the considered sample
                               # Output: data = a list of matrix containing the realigned data
                               ##################################################################################################
                           
                               Realign2 = function(data, pas){
                                 
                                 min = min(do.call(rbind,data)[,1])
                                 
                                 minPlace = which(sapply(1:length(data), function(x){
                                   if(length(which(data[[x]][,1] == min)) == 1) {
                                     TRUE                                   
                                   } else {FALSE}
                                 }) == TRUE)
                                 
                                 if(length(minPlace) != 1){minPlace = minPlace[1]} else{}
                                 
                                 max = max(do.call(rbind,data)[,1]) 
                                 
                                 maxPlace = which(sapply(1:length(data), function(x){
                                   if(length(which(data[[x]][,1] == max)) == 1) {
                                     TRUE
                                   }else {FALSE}
                                 }) == TRUE)
                                 
                                 if(length(maxPlace) != 1){maxPlace = maxPlace[length(maxPlace)]} else {}
                                 
                                 dataMin <- data[[minPlace]]
                                 
                                 dataMax <- data[[maxPlace]]
                                 
                                 dimMax <- NULL
                                 
                                 for(i in 1:length(data)){
                                   
                                   temp <- data[[i]]
                                   
                                   while(round(dataMin[1,1]) < round(temp[1,1])){temp = rbind(c(temp[1,1]-pas,rep(NA,dim(dataMin)[2]-1)),temp)}
                                   
                                   data[[i]] <- temp
                                   
                                   dimMax <- c(dimMax, dim(temp)[1])
                                   
                                 }
                                 
                                 dimMax <- max(dimMax)
                                 
                                 for(j in 1:length(data)){
                                   
                                   if(dim(data[[j]])[1] < dimMax){
                                     
                                     ToAdd <-dimMax - dim(data[[j]])[1]
                                     
                                     for (i in 1:ToAdd){
                                       
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
                               ##################################################################################################
                               
                               setRep_dataFiltre = function(){
                                 
                                 self$rep_dataFiltre <- lapply(1:length(self$rep_Files),function(x){self$rep_data[[x]]$dataConcCorr})
                                 
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
                                   
                                 outputTab <- rbind(t(as.matrix(sapply(1:length(self$rep_Files), function(x){apply(self$rep_dataFiltre[[x]][,-1],2, mean,na.rm = TRUE)}))),t(as.matrix(sapply(1:length(self$rep_Files), function(x){apply(self$rep_dataFiltre[[x]][,-1],2, sd,na.rm = TRUE)}))))
                                 
                                 namesCol <- c(paste0("Mean_", self$rep_Files),paste0("SD_", self$rep_Files))                                   
                                 
                                 rownames(outputTab) <- namesCol                      
                                 
                                 return(outputTab) 
                               },
                               
                               ##################################################################################################
                               # Name: intermStepRaster
                               # Function: create and return an intermediate matrix containing realigned data for all sample replicates
                               # Output: outputList = a list of matrix containing realigned data
                               ##################################################################################################
                               
                               intermStepRaster = function(decalage, input){
                                 
                                 self$setRep_pas()
                                 
                                 tabTemp <-lapply(1:length(self$rep_dataFiltre), function(x){
                                   
                                   temp <- self$rep_dataFiltre[[x]]
                                   
                                   temp[,1] <- temp[,1] + decalage[x] * self$rep_pas
                                   
                                   return(temp)                                   
                                   
                                 })
                                 
                                 names(tabTemp) <- names(self$rep_dataFiltre)
                                                                                                   
                                 outputList <- lapply(1:length(input), function(x){
                                   tabTemp[[which(names(tabTemp) == input[x])]]
                                 })
                                 
                                 names(outputList) <- sapply(1:length(input), function(x){
                                   names(tabTemp)[which(names(tabTemp) == input[x])]
                                 })
                                 
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
                               }                               

                               
                             ) # list
) # elementR_repstand