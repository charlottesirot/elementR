convertMol_to_PPM <- function(dat, AtomicMass, InternStand){
	
	#remove the first column and make sure that the table is numeric
	datToConvert <- apply(dat[,2:ncol(dat)], 2, as.numeric)
	
	#remove the numerical part of the element names
	element <- gsub("[[:digit:]]","", colnames(dat[,2:ncol(dat)]))
	
	#remove the numerical part of the element considered as the internal standard
	InternStand <- gsub("[[:digit:]]","", InternStand)
	
	#Conversion
	temp <- sapply(1:ncol(datToConvert), function(x){
		
		elementConsidered <- element[x]
		
		factor <- eval(parse(text = paste0("AtomicMass$", elementConsidered)))/ eval(parse(text = paste0("AtomicMass$", InternStand)))
		
		return(datToConvert[,x]*factor)
	})
	
	toReturn <- data.frame(dat[,1], temp)
	
	colnames(toReturn) <- colnames(dat)
	
	return(toReturn)
	
}
