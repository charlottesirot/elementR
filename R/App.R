##############################################################
#
# elementR 1.3.3
#
# charlott.sirot@gmail.com
# francois.guilhaumon@ird.fr
#
#####################################################################

runElementR <- function(){ # nocov start
	
	
	######################
	############### GLOBAL
	######################
	
	
	#skyn
	skin <- Sys.getenv("DASHBOARD_SKIN")
	skin <- tolower(skin)
	if (skin == "") skin <- "blue"
	
	menuIconClass <- "fa-lg" 
	
	######################
	############ FUNCTIONS
	######################
	
	##################################################################################################
	# Name: checkFormat
	# function: check the format (i.e. the extension) of the file uploaded
	# input : files = the path of the session uploaded
	# output: A logical value, FALSE = error, TRUE = no error
	##################################################################################################
	
	checkFormat <- function(files){
		
		files <- list.files(files, recursive = TRUE)
		
		ref <- NULL
		
		for(x in seq(from = 1, to = length(files), by = 1)){
			
			if(str_detect(files[x], ".xlsx")){
				ref <- c(ref, TRUE)
			} else if(str_detect(files[x], ".xls")){
				ref <- c(ref, TRUE)
			} else if(str_detect(files[x], ".ods")){
				ref <- c(ref, TRUE)
			} else if(str_detect(files[x], ".csv")){
				ref <- c(ref, TRUE)
			} else {
				ref <- c(ref, FALSE)
			}
		}
		
		if(length(which(ref == FALSE)) == !0){
			res <- FALSE
		} else {
			res <- TRUE
		}
		return(res)
	}
	
	##################################################################################################
	# Name: readData
	# function: detect the format of the data and read the table
	# input : x = a character string of the path of the data
	# output: a matrix
	##################################################################################################
	
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
	
	##################################################################################################
	# Name: geneR
	# function: gener randomly a vector of character string all differents from each other
	# input : choice = the constituants of the character string (here letters), lengthComb = length of the character string, NBComb = number of needed combinations, toAvoid = combinaison to avoid
	##################################################################################################
	
	geneR = function(choice, lengthComb, NBComb, toAvoid = NA){
		
		if(is.null(toAvoid)){
			toAvoid = NA
		} else {}
		
		temp <- vector()
		
		nombreMax = length(choice)^lengthComb
		
		if(NBComb > nombreMax | (NBComb + length(toAvoid)) > nombreMax){
			
			tkmessageBox(message = "Saturated memory. Please re-launch elementR.", icon = "error", type = "ok")
			
			stop()
			
		} else {
			
			while(length(temp) != NBComb){
				
				nom <- paste(sample(choice, lengthComb, replace = TRUE), collapse = "")
				
				if(length(grep(nom, temp)) != 0 | length(grep(nom, toAvoid)) !=0){
					
				} else {
					
					temp <- c(temp, nom)
					
				}
			}
			
			return(temp)
		}
		
		
	}
	
	##################################################################################################
	# Name: dir.exists (from https://github.com/hadley/staticdocs/issues/33)
	# function: check if a directory exists
	# input : d: directory to test
	##################################################################################################
	
	dir.exists <- function(d) {
		de <- file.info(d)$isdir
		ifelse(is.na(de), FALSE, de)
	}
	
	######################
	################### UI
	######################
	
	sidebar <-   dashboardSidebar(
		useShinyjs(),
		sidebarMenu(id = "tab",
				div(style = "background: rgb(60, 141, 188); height: 50px",
				    p(icon("star-half-o"),"element-R", 
				      style = "font-size: 200%; padding-left:50px;padding-top:5px")),
				
				menuItem("Project setup", tabName = "start", 
					   icon = icon("thermometer-0", class=menuIconClass), 
					   badgeLabel="Step 1", badgeColor="blue"),
				
				div(align="center",icon("arrow-down",class="fa-2x")),
				
				menuItem("Reduce standards", tabName = "Standards", 
					   icon = icon("thermometer-1", class=menuIconClass), 
					   badgeLabel="Step 2", badgeColor="green"),
				
				uiOutput("renderProgress2"),
				
				div(align="center",icon("arrow-down",class="fa-2x")),
				
				menuItem("Drift verification", tabName = "MachDrift", 
					   icon = icon("thermometer-2", class=menuIconClass), 
					   badgeLabel="Step 3", badgeColor="purple"),
				
				div(align="center",icon("arrow-down",class="fa-2x")),
				
				menuItem("Reduce samples", tabName = "Samples", 
					   icon = icon("thermometer-3", class=menuIconClass), 
					   badgeLabel="Step 4", badgeColor="maroon"),
				
				uiOutput("renderProgress4"),
				
				div(align="center",icon("arrow-down",class="fa-2x")),
				
				menuItem("Average samples", tabName = "realign", 
					   icon = icon("thermometer-4", class=menuIconClass), 
					   badgeLabel="Optional", badgeColor="orange"),
				
				uiOutput("renderProgress5"),
				
				hr(style ="width: 70%; color: white; align: center"),
				
				menuItem("Settings", icon = icon("sliders"), tabName = "Config"),
				
				menuItem("Precision and accuracy", icon = icon("sliders"), tabName = "SessionConfig"),
				
				menuItem("Source code for app", icon = icon("file-code-o"),
					   href = "https://github.com/charlottesirot/elementR"
				),
				div(uiOutput("Export"), style = "text-align: center")
		)
	)
	
	body <- dashboardBody(
		
		includeCSS(system.file("www/elementR.css", package="elementR")),
		
		div(style = "min-height:100vh; min-width: (100vw - 230); display:flex",
		    div(style = "background-color: #666666; width: 31px;",
		        div(style = "background-color: #666666; width: 30px;position:fixed",
		            div(style = "background: rgb(60, 141, 188); height: 50px"),
		            uiOutput('ValidFlag1'),
		            div(style = "background-color: #666666; height: 30px; width: 30px"),
		            uiOutput('ValidFlag2'),
		            div(style = "background-color: #666666; height: 30px; width: 30px"),
		            uiOutput('ValidFlag3'),
		            div(style = "background-color: #666666; height: 30px; width: 30px"),
		            uiOutput('ValidFlag4'),
		            div(style = "background-color: #666666; height: 30px; width: 30px"),
		            uiOutput('ValidFlag5')
		        )
		    ),
		    div(style = "width: 100%; margin-top:10px; margin-left:10px;margin-bottom:10px;margin-right:0px;",
		        uiOutput("TopBar"),
		        tabItems(
		        	
		        	tabItem("start",
		        		  uiOutput("start1"),
		        		  
		        		  fluidRow(
		        		  	uiOutput("start2")
		        		  )
		        		  
		        		  
		        		  
		        	), #eo tab start
		        	
		        	
		        	tabItem("Standards", style = "padding-right: 0px; padding-left: 0px",
		        		  uiOutput("Standards1"),
		        		  uiOutput("Standards2")
		        		  
		        		  
		        		  
		        	), #eo tab Standards
		        	tabItem("MachDrift", style = "padding-right: 0px; padding-left: 0px",
		        		  uiOutput("MachDrift1"),
		        		  uiOutput("MachDrift2"),
		        		  uiOutput('MachDrift3')
		        		  
		        	), #eo tab MachDrift
		        	
		        	tabItem("Samples", style = "padding-right: 0px; padding-left: 0px",
		        		  box(width = 12,background = "aqua", style = "background-color: #85735D;margin-bottom:10px",
		        		      column(5,
		        		      	 uiOutput("sample1")
		        		      ),
		        		      column(2,
		        		      	 uiOutput("sample2")
		        		      ),
		        		      column(3,
		        		      	 uiOutput("sample3")
		        		      ),
		        		      column(1, class = "class2",
		        		      	 br(),
		        		      	 uiOutput("sample4")
		        		      )
		        		      
		        		  ),
		        		  column(12,
		        		  	 uiOutput("Sample5"))
		        		  
		        	), #eo tab Samples
		        	
		        	tabItem("realign",
		        		  uiOutput("realign1"),
		        		  uiOutput("realign2"),
		        		  uiOutput("realign8"),
		        		  uiOutput("realign10"),
		        		  fluidRow(
		        		  	column(3, uiOutput("realign3")),
		        		  	column(9, uiOutput("realign5"))
		        		  )
		        		  
		        		  
		        		  
		        		  
		        	), #eo tab realign
		        	tabItem("Config",
		        		  uiOutput("config0"),
		        		  uiOutput("config4"),
		        		  uiOutput("config2"),
		        		  uiOutput("config3"),
		        		  uiOutput("config1")
		        		  
		        	), #eo tab Config
		        	tabItem("SessionConfig",
		        		  uiOutput("Precision1"),
		        		  uiOutput("Precision4")
		        	) #eo tab Config
		        	
		        )
		    )
		)
	)#dashboardBody
	
	header <- dashboardHeader(
		title = list(icon("star-half-o"),"element-R"), disable = TRUE, titleWidth = 260
	)
	
	ui <- dashboardPage(header, sidebar, body, skin = skin)
	
	server <- function(input, output, session) {
		
		currentPage <- reactiveValues(temp = c("start", "start")) #marker of the current step
		
		observe({
			
			if(flagStart$temp[1] != 3 & flagStart$temp[2] != 3){
				
				if(input$tab == "Standards" | input$tab == "MachDrift" | input$tab == "Samples" | input$tab == "realign"){
					
					updateTabItems(session, "tab", selected = "start")
					
					Message <- "You need to finish the first step for handling the rest of the filtration procedure!"
					
					tkmessageBox(message = Message, icon = "error", type = "ok")
					
				} else {}
				
			} else if(length(which(currentProject()$flag_stand != 1)) != 0){
				
				if(input$tab == "MachDrift" | input$tab == "Samples" | input$tab == "realign"){
					
					updateTabItems(session, "tab", selected = "Standards")
					
					Message <- "You need to finish reducing standards for continuing the filtration procedure!"
					
					tkmessageBox(message = Message, icon = "error", type = "ok")
					
				} else {}
				
			} else if(currentProject()$flagMachineCorrection != 1){
				
				if(input$tab == "Samples" | input$tab == "realign"){
					
					updateTabItems(session, "tab", selected = "MachDrift")
					
					Message <- "You need to validate machine drift for continuing the filtration procedure!"
					
					tkmessageBox(message = Message, icon = "error", type = "ok")
					
				} else {}
				
			} else if(length(which(flagSample$temp == TRUE)) == 0){
				
				if(input$tab == "realign"){
					
					updateTabItems(session, "tab", selected = "Samples")
					
					Message <- "You need to validate all the sample replicate to access the last step !"
					
					tkmessageBox(message = Message, icon = "error", type = "ok")
					
				} else {}
				
			} else {}
			
		})
		
		# How to come back to the current page
		observe({
			
			currentPage$temp <- c(input$tab, isolate(currentPage$temp)[1])
			
		})
		
		#go to next step
		observe({
			if(is.null(input$nextStep)){
			} else {
				if(input$nextStep > 0){
					isolate({
						if(input$tab == "start"){
							updateTabItems(session, "tab", selected = "Standards")
						} else if(input$tab == "Standards"){
							updateTabItems(session, "tab", selected = "MachDrift")
						} else if(input$tab == "MachDrift"){
							updateTabItems(session, "tab", selected = "Samples")
						} else if(input$tab == "Samples"){
							updateTabItems(session, "tab", selected = "realign")
						} else {}
					})
					
				} else {}
			}
		})
		
		# go to previous step
		observe({
			if(is.null(input$prevStep)){
			} else {
				if(input$prevStep > 0){
					isolate({
						if(input$tab == "realign"){
							updateTabItems(session, "tab", selected = "Samples")
						} else if(input$tab == "Standards"){
							updateTabItems(session, "tab", selected = "start")
						} else if(input$tab == "MachDrift"){
							updateTabItems(session, "tab", selected = "Standards")
						} else if(input$tab == "Samples"){
							updateTabItems(session, "tab", selected = "MachDrift")
						} else {}
					})
					
				} else {}
			}
		})
		
		#define top bar
		output$TopBar <- renderUI({
			
			input$saveNists
			input$SuppDonne
			input$saveSample
			input$validDrift
			input$SauvegarderSpot
			input$SauvegarderReal
			
			if(is.null(currentProject())){
				div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ")
			} else{
				if(input$tab == "start"){
					if(flagStart$temp[1] == 3 | flagStart$temp[2] == 3){
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        div(style ="float: right;padding-right: 20px",
						            actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), 
						            		 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px")
						        )
						        
						    )
						)
					} else{
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ")
					}
				}
				else if(input$tab == "Standards"){
					if(length(which(currentProject()$flag_stand != 1)) == 0){
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        
						        div(style ="float: left;padding-left: 20px",
						            actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), 
						            		 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px")),
						        
						        div(style ="float: right;padding-right: 20px",
						            actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), 
						            		 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"))
						        
						    ))
					} else{
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), 
						        		     style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"),
						            style ="float: left;padding-left: 20px")
						    )
						)
					}
				}
				else if(input$tab == "MachDrift"){
					if((validCorrection$temp%%2) == 1){
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        
						        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", 
						        					 style="margin-bottom:5px"), 
						        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"),
						            style ="float: left;padding-left: 20px"),
						        
						        div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", 
						        					 style="margin-bottom:5px"), 
						        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), 
						            style ="float: right;padding-right: 20px")
						    )
						)
						
					} else{
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", 
						        					 style="margin-bottom:5px"), 
						        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), 
						            style ="float: left;padding-left: 20px")
						    )
						)
					}
				}
				else if(input$tab == "Samples"){
					
					temp <- vapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1),
							   
							   function(x){
							   	
							   	if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
							   		return(1)
							   	}else{return(0)}
							   	
							   }, 
							   FUN.VALUE = numeric(1)
					)
					
					if(length(temp) == 0){
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", 
						        					 style="margin-bottom:5px"), 
						        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), 
						            style ="float: left;padding-left: 20px")
						    )
						)
						
					} else{
						
						div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
						    div(style = "overflow: auto;",
						        
						        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", 
						        					 style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), 
						            style ="float: left;padding-left: 20px"),
						        
						        div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", 
						        					 style="margin-bottom:5px"), 
						        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"),
						            style ="float: right;padding-right: 20px")
						    )
						)
					}
					
				}
				else if(input$tab == "realign"){
					
					div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
					    div(style = "overflow: auto;",
					        div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", 
					        					 style="margin-bottom:5px"), 
					        					 style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), 
					            style ="float: left;padding-left: 20px")
					    )
					)
					
				} else{ div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ") }
			}
			
			
			
		})
		
		#####################################
		#####################################
		########### elementR formatting  ####
		#####################################
		#####################################
		{
			
			##################
			
			output$myImageProgressBar1 <- renderImage({
				
				if(flagStart$temp[1] == 3 | flagStart$temp[2] == 3){
					list(src = system.file("www/2.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else {
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				}
				
			}, deleteFile = FALSE )  # eo  output$myImageProgressBar1
			
			output$ValidFlag1 <- renderUI({
				if(input$tab == "start"){
					div(imageOutput("myImageProgressBar1"),style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive")
				} else {
					div(imageOutput("myImageProgressBar1"),style = "height: 44px; width: 30px; padding-top: 8px", class = "bar")
				}
				
			}) #eo output$ValidFlag1
			
			##################
			
			output$renderProgress2 <- renderUI({
				input$saveNists
				input$SuppDonne
				flagStandard$temp
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					if(input$tab == "Standards"){
						div(class = "progress",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
					} else {
						div(class = "progressActive",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
					}
				}
				else{
					if(input$tab == "Standards"){
						div(class = "progress", style = "overflow: auto;",
						    p(paste0("Standard(s) reduced: ", sum(currentProject()$flag_stand), " / ", 
						    	   length(currentProject()$flag_stand)), style = "line-height:1px; text-align:center")
						    
						)
					} else {
						div(class = "progressActive", style = "overflow: auto;",
						    p(paste0("Standard(s) reduced: ", sum(currentProject()$flag_stand), " / ", 
						    	   length(currentProject()$flag_stand)), style = "line-height:1px; text-align:center")
						    
						)
					}
				}
				
			}) #eo output$renderProgress2
			
			output$myImageProgressBar2 <- renderImage({
				input$saveNists
				input$SuppDonne
				flagStandard$temp
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else if(length(which(currentProject()$flag_stand != 1)) == 0){
					list(src = system.file("www/2.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else {
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				}
				
				
			}, deleteFile = FALSE ) #eo output$myImageProgressBar2
			
			output$ValidFlag2 <- renderUI({
				if(input$tab == "Standards"){
					div(
						div(imageOutput("myImageProgressBar2"), style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive"),
						div(style = "width: 30px;", class = "barActive")
					)
					
				} else {
					div(
						div(imageOutput("myImageProgressBar2"),style = "height: 44px; width: 30px; padding-top: 8px", class = "bar"),
						div(style = "width: 30px;", class = "bar")
					)
					
				}
				
			}) #eo output$ValidFlag2
			
			#############
			
			output$myImageProgressBar3 <- renderImage({
				input$saveNists
				input$SuppDonne
				input$saveSample
				input$validDrift
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else if(is.null(validCorrection$temp)){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else if((validCorrection$temp%%2) == 1){
					list(src = system.file("www/2.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else{
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				}
				
			}, deleteFile = FALSE ) #eo output$myImageProgressBar3
			
			output$ValidFlag3 <- renderUI({
				
				if(input$tab == "MachDrift"){
					div(imageOutput("myImageProgressBar3"),style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive")
				} else {
					div(imageOutput("myImageProgressBar3"),style = "height: 44px; width: 30px; padding-top: 8px", class = "bar")
				}
				
			}) #eo output$ValidFlag3
			
			#############
			
			output$renderProgress4 <- renderUI({
				input$saveNists
				input$SuppDonne
				input$saveSample
				input$validDrift
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					if(input$tab == "Samples"){
						
						div(class = "progress",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
						
					} else {
						
						div(class = "progressActive",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
						
					}
				} else if(input$tab == "Samples"){
					
					if(!is.null(currentProject()$flag_Sample)){
						div(class = "progress", style = "overflow: auto;",
						    p(paste0("Sample repl. reduced: ", do.call(sum, currentProject()$flag_Sample), " / ", 
						    	   length(unlist(currentProject()$flag_Sample))), style = "line-height:1px; text-align:center")
						)
					}
					
					
				} else {
					if(!is.null(currentProject()$flag_Sample)){
						div(class = "progressActive", style = "overflow: auto;",
						    p(paste0("Sample repl. reduced: ", do.call(sum, currentProject()$flag_Sample), " / ", 
						    	   length(unlist(currentProject()$flag_Sample))), style = "line-height:1px; text-align:center")
						)
					}
					
				}
				
			}) #eo output$renderProgress4
			
			output$myImageProgressBar4 <- renderImage({
				input$saveNists
				input$SuppDonne
				input$saveSample
				input$validDrift
				input$SauvegarderSpot
				input$SauvegarderReal
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else{
					
					temp <- vapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), 
							   function(x){
							   	if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
							   		return(1)
							   	}else{return(0)}
							   },
							   FUN.VALUE = numeric(1)
					)
					
					if(length(temp) == 0){
						list(src = system.file("www/3.png", package="elementR"),
						     contentType = 'image/png',
						     width = 15,
						     height = 15,
						     alt = "This is alternate text")
					} else if(length(which(temp == 0)) != 0){
						list(src = system.file("www/3.png", package="elementR"),
						     contentType = 'image/png',
						     width = 15,
						     height = 15,
						     alt = "This is alternate text")
					} else {
						list(src = system.file("www/2.png", package="elementR"),
						     contentType = 'image/png',
						     width = 15,
						     height = 15,
						     alt = "This is alternate text")
					}
				}
				
			}, deleteFile = FALSE ) #eo output$myImageProgressBar4
			
			output$ValidFlag4 <- renderUI({
				
				if(input$tab == "Samples"){
					
					div(
						div(imageOutput("myImageProgressBar4"), style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive"),
						div(style = "width: 30px;", class = "barActive")
					)
					
				} else {
					
					div(
						div(imageOutput("myImageProgressBar4"),style = "height: 44px; width: 30px; padding-top: 8px", class = "bar"),
						div(style = "width: 30px;", class = "bar")
					)
				}
				
			}) #eo  output$ValidFlag4
			
			#############
			
			output$renderProgress5 <- renderUI({
				
				input$saveNists
				input$SuppDonne
				input$saveSample
				input$validDrift
				input$SauvegarderSpot
				input$SauvegarderReal
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					if(input$tab == "realign"){
						div(class = "progress",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
					} else {
						div(class = "progressActive",
						    p("Waiting for data", style = "line-height:1px; text-align:center")
						)
					}
				} else{
					if(input$tab == "realign"){
						
						temp <- sum(vapply(seq(from = 1, to = length(flagRealign$temp), by = 1), 
									 function(x){
									 	if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
									 		return(1)
									 	} else{return(0)}
									 }, 
									 FUN.VALUE = numeric(1)
						)
						)
						
						div(class = "progress", style = "overflow: auto;",
						    p(paste0("Samples handled: ", temp, " / ", length(flagRealign$temp)), 
						      style = "line-height:1px; text-align:center")
						)
					} else{
						
						if(is.null(flagRealign$temp)){
							temp <- 0
						} else {
							temp <- sum(vapply(seq(from = 1, to = length(flagRealign$temp), by = 1), 
										 function(x){
										 	if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
										 		return(1)
										 	} else{return(0)}
										 }, 
										 FUN.VALUE = numeric(1)
							)
							)
						}
						
						
						div(class = "progressActive", style = "overflow: auto;",
						    p(paste0("Samples handled: ", temp, " / ", length(flagRealign$temp)), style = "line-height:1px; text-align:center")
						)
					}
				}
				
			}) #eo output$renderProgress5
			
			output$myImageProgressBar5 <- renderImage({
				input$saveNists
				input$SuppDonne
				input$saveSample
				input$validDrift
				input$SauvegarderSpot
				input$SauvegarderReal
				
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				} else if(is.null(flagRealign$temp)){
					list(src = system.file("www/3.png", package="elementR"),
					     contentType = 'image/png',
					     width = 15,
					     height = 15,
					     alt = "This is alternate text")
				}else{
					temp <- vapply(seq(from = 1, to = length(flagRealign$temp), by = 1), 
							   function(x){
							   	if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
							   		return(1)
							   	} else{return(0)}
							   }, 
							   FUN.VALUE = numeric(1)
					)
					
					if(length(which(temp == 0)) == 0){
						list(src = system.file("www/2.png", package="elementR"),
						     contentType = 'image/png',
						     width = 15,
						     height = 15,
						     alt = "This is alternate text")
					} else{
						list(src = system.file("www/3.png", package="elementR"),
						     contentType = 'image/png',
						     width = 15,
						     height = 15,
						     alt = "This is alternate text")
					}
				}
				
			}, deleteFile = FALSE) #eo output$myImageProgressBar5
			
			output$ValidFlag5 <- renderUI({
				
				if(input$tab == "realign"){
					div(
						div(imageOutput("myImageProgressBar5"),style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive"),
						div(style = "width: 30px;", class = "barActive")
					)
				}else{
					div(
						div(imageOutput("myImageProgressBar5"),style = "height: 44px; width: 30px; padding-top: 8px", class = "bar"),
						div(style = "width: 30px;", class = "bar")
					)
				}
				
			}) #eo output$ValidFlag5
		}
		
		#################################
		#################################
		########### Project Export   ####
		#################################
		#################################
		{
			#############################################
			# define output$Export, i.e. the div allowing 
			# to rename the project and to export it
			#############################################
			observe({
				if(flagStart$temp[1] == 3 | flagStart$temp[2] == 3){
					output$Export <-  renderUI({
						div(
							textInput("text", label = "", value = "Name of your project..."),
							actionButton("export","Export Project")
						)
						
					})
				}else{
					output$Export <- renderUI({NULL})
				}
			}) #observe
			
			#############################################
			# Export the data
			#############################################
			observe({
				if(!is.null(input$export)){
					
					if(input$export > 0){
						
						isolate({
							
							espace1 <- getwd()
							
							setwd(paste0(projPath$temp,"/Results"))
							
							pb <- tkProgressBar("Progress bar", "Project export in %",
										  0, 100, 0)
							
							myProject <- currentProject()
							temp <- str_split(projPath$temp, "/")[[1]]
							nameToInsert <- temp[length(temp)]
							
							if(input$text == "Name of your project..."){
								
								if(flagStart$temp[2] == 3){
									if(WhatLoaded$temp == "notExample"){
										save(myProject, file = paste0(nameToInsert, ".RData"))
									} else {
										save(myProject, file = "Example_Session.RData")
									}
								} else {
									save(myProject, file = paste0(nameToInsert, ".RData"))
								}
							} else {
								save(myProject, file = paste0(input$text, ".RData"))
							}
							if(!is.null(input$exportseptData)){
								if(input$exportseptData == "Tab key"){
									sep <-  "\t"
								} else if(input$exportseptData == "Blank"){
									sep <- " "
								} else{sep <- input$exportseptData}
							}
							
							if(currentProject()$flagMachineCorrection == 1){
								
								tempo <- currentProject()$regressionModel
								if(is.null(input$exportFormatData)){
									write.csv(tempo, file = "regression_parameters.csv")
								} else if(input$exportFormatData == ".csv"){
									write.table(tempo, file = "regression_parameters.csv", sep = sep)
								} else{
									write.table(as.data.frame(tempo), file = paste0("regression_parameters", input$exportFormatData), sep = sep)
								}
								if(is.null(input$exportFormatData)){
									write.csv(currentProject()$standards[[1]]$rep_dataFinale, file = "SummaryStandard.csv")
								} else{
									if(input$exportFormatData == ".csv"){
										write.table(currentProject()$standards[[1]]$rep_dataFinale, file = "SummaryStandard.csv", sep = sep)
									}
									else{
										write.table(as.data.frame(currentProject()$standards[[1]]$rep_dataFinale), 
												file = paste0("SummaryStandard",input$exportFormatData), sep = sep)
									}
								}
								
							} else {
								invisible(file.remove(list.files(,pattern = "Drift")))
								invisible(file.remove(list.files(,pattern = "regression_parameters")))
								invisible(file.remove(list.files(,pattern = "SummaryStandard")))
								invisible(file.remove(list.files(,pattern = "PrecisionTable")))
								invisible(file.remove(list.files(,pattern = "CorrectnessTable")))
							}
							
							if(is.matrix(currentProject()$summarySettings)){
								if(is.null(input$exportFormatData)){
									write.csv(currentProject()$summarySettings, file = "SummarySettings.csv")
								} else{
									if(input$exportFormatData == ".csv"){
										write.table(currentProject()$summarySettings, file = "SummarySettings.csv", sep = sep)
									}
									else{
										write.table(as.data.frame(currentProject()$summarySettings), file = paste0("SummarySettings",input$exportFormatData), sep = sep)
									}
								}
							}
							
							if(is.matrix(currentProject()$precisionTable)){
								if(is.null(input$exportFormatData)){
									write.csv(currentProject()$precisionTable, file = "PrecisionTable.csv")
								} else{
									if(input$exportFormatData == ".csv"){
										write.table(currentProject()$precisionTable, file = "PrecisionTable.csv", sep = sep)
									}
									else{
										write.table(as.data.frame(currentProject()$precisionTable), file = paste0("PrecisionTable",input$exportFormatData), sep = sep)
									}
								}
							}
							
							if(is.matrix(currentProject()$correctnessTable) | is.data.frame(currentProject()$correctnessTable)){
								if(is.null(input$exportFormatData)){
									write.csv(currentProject()$correctnessTable, file = "CorrectnessTable.csv")
								} else{
									if(input$exportFormatData == ".csv"){
										write.table(currentProject()$correctnessTable, file = "CorrectnessTable.csv", sep = sep)
									}
									else{
										write.table(as.data.frame(currentProject()$correctnessTable), file = paste0("CorrectnessTable",input$exportFormatData), sep = sep)
									}
								}
							}
							
							info <- sprintf("%d%% done", round(20))
							setTkProgressBar(pb, 20, sprintf("Export (%s)", info), info)
							
							setwd(espace1)
							setwd(paste0(projPath$temp,"/Results/standards"))
							
							lapply(seq(from = 1, to = length(currentProject()$standards[[1]]$rep_Files), by = 1), function(x){
								suppressWarnings(dir.create(paste0(projPath$temp,"/Results/standards/", currentProject()$standards[[1]]$rep_Files[x])))
							})
							
							lapply(seq(from = 1, to = length(currentProject()$standards[[1]]$rep_Files), by = 1),function(x){
								
								setwd(paste0(projPath$temp,"/Results/standards/", currentProject()$standards[[1]]$rep_Files[x]))
								
								info <- sprintf("%d%% done", round(20 + x*10/length(currentProject()$standards[[1]]$rep_Files)))
								setTkProgressBar(pb, round(20 + x*10/length(currentProject()$standards[[1]]$rep_Files)), sprintf("Export (%s)", info), info)
								
								if(currentProject()$flag_stand[x] == 0){
									
									ToRemove <- list.files(, pattern = ".csv")
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".xls")
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".jpg", recursive = TRUE)
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".jpeg", recursive = TRUE)
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".bmp", recursive = TRUE)
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".png", recursive = TRUE)
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".tiff", recursive = TRUE)
									invisible(file.remove(ToRemove))
								}
								
								if(currentProject()$flag_stand[x] != 0){
									
									dat <- currentProject()$standards[[1]]$rep_data[[x]]
									
									if(is.null(input$exportFormatData)){
										
										write.csv(dat$dataBlank, file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										write.csv(dat$dataPlateau, file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										write.csv(dat$dataSuppBlank, file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										write.csv(dat$dataSupLOD, file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										write.csv(dat$dataNorm, file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										write.csv(dat$dataOutlierFree, file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
										
									} else if(input$exportFormatData == ".csv"){
										
										write.table(dat$dataBlank, file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										write.table(dat$dataPlateau, file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										write.table(dat$dataSuppBlank, file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										write.table(dat$dataSupLOD, file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										write.table(dat$dataNorm, file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										write.table(dat$dataOutlierFree, file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
										
									} else {
										
										format <- input$exportFormatData
										
										write.table(as.data.frame(dat$dataBlank), file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										write.table(as.data.frame(dat$dataPlateau), file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										write.table(as.data.frame(dat$dataSuppBlank), file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										write.table(as.data.frame(dat$dataSupLOD), file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										write.table(as.data.frame(dat$dataNorm), file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										write.table(as.data.frame(dat$dataOutlierFree), file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],format), sep = sep)
										
									}
								}
								
							}) # eo lapply
							
							lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
								
								setwd(espace1)
								suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x])))
								
								lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_Files), by = 1), function(y){
									setwd(espace1)
									
									info <- sprintf("%d%% done", round(30 + (x*70/length(currentProject()$samplesFiles))*y/length(currentProject()$samples[[x]]$rep_Files)))
									setTkProgressBar(pb, round(30 + (x*70/length(currentProject()$samplesFiles))*y/length(currentProject()$samples[[x]]$rep_Files)), 
											     sprintf("Export (%s)", info), info)
									
									temporaire <- currentProject()$samples[[x]]$rep_Files[y]
									suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x],"/",temporaire)))
									setwd(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x],"/",temporaire))
									
									if(currentProject()$flag_Sample[[x]][y] == 0){
										ToRemove <- list.files(, pattern = ".csv")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".xls")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".jpg", recursive = TRUE)
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".jpeg", recursive = TRUE)
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".bmp", recursive = TRUE)
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".png", recursive = TRUE)
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".tiff", recursive = TRUE)
										invisible(file.remove(ToRemove))
									}
									if(currentProject()$flag_Sample[[x]][y] != 0){
										
										dat <- currentProject()$samples[[x]]$rep_data[[y]]
										
										if(is.null(input$exportFormatData)){
											
											write.csv(dat$dataBlank, file = paste0("data_Blank_",temporaire,".csv"))
											write.csv(dat$dataPlateau, file = paste0("data_Plateau_",temporaire,".csv"))
											write.csv(dat$dataSuppBlank, file = paste0("data_SuppBlank_",temporaire,".csv"))
											write.csv(dat$dataSupLOD, file = paste0("data_SupLOD_",temporaire,".csv"))
											write.csv(dat$dataNorm, file = paste0("data_Norm_",temporaire,".csv"))
											write.csv(dat$dataConc, file = paste0("data_Conc_",temporaire,".csv"))
											write.csv(dat$dataConcCorr, file = paste0("data_ConcCorr_",temporaire,".csv"))
											
										} else if(input$exportFormatData == ".csv"){
											
											write.table(dat$dataBlank, file = paste0("data_Blank_",temporaire,".csv"), sep = sep)
											write.table(dat$dataPlateau, file = paste0("data_Plateau_",temporaire,".csv"), sep = sep)
											write.table(dat$dataSuppBlank, file = paste0("data_SuppBlank_",temporaire,".csv"), sep = sep)
											write.table(dat$dataSupLOD, file = paste0("data_SupLOD_",temporaire,".csv"), sep = sep)
											write.table(dat$dataNorm, file = paste0("data_Norm_",temporaire,".csv"), sep = sep)
											write.table(dat$dataConc, file = paste0("data_Conc_",temporaire,".csv"), sep = sep)
											write.table(dat$dataConcCorr, file = paste0("data_ConcCorr_",temporaire,".csv"), sep = sep)
											
										} else {
											
											write.table(as.data.frame(dat$dataBlank), file = paste0("data_Blank_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataPlateau), file = paste0("data_Plateau_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataSuppBlank), file = paste0("data_SuppBlank_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataSupLOD), file = paste0("data_SupLOD_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataNorm), file = paste0("data_Norm_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataConc), file = paste0("data_Conc_",temporaire,input$exportFormatData), sep = sep)
											write.table(as.data.frame(dat$dataConcCorr), file = paste0("data_ConcCorr_",temporaire,input$exportFormatData), sep = sep)
											
										}
									}
									
								}) #eo lapply
								
								setwd(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x]))
								
								if((flagRealign$temp[[x]][2]%%4) == 1|(flagRealign$temp[[x]][2]%%4) == 3|(flagRealign$temp[[x]][1]%%2) == 1){
									
									if(!is.na(currentProject()$samples[[x]]$rep_type2)){
										
										if(currentProject()$samples[[x]]$rep_type2 == "spot"){
											
											invisible(file.remove(list.files()[which(str_detect(list.files(), "finalReplicates") == TRUE)]))
											
											if(is.null(input$exportFormatData)){
												
												write.csv(currentProject()$samples[[x]]$rep_dataFinalSpot, file = paste0("final_",currentProject()$samplesFiles[x],".csv"))
												
											} else if(input$exportFormatData == ".csv"){
												
												write.table(currentProject()$samples[[x]]$rep_dataFinalSpot, file = paste0("final_",currentProject()$samplesFiles[x],".csv"), sep = sep)
												
											} else {
												
												write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataFinalSpot), 
														file = paste0("final_",currentProject()$samplesFiles[x],input$exportFormatData), sep = sep)
												
											}
										} else if(currentProject()$samples[[x]]$rep_type2 == "transect"){
											
											if(is.null(input$exportFormatData)){
												
												lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_dataIntermRaster), by = 1), function(k){
													
													write.csv(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]], 
														    file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],".csv"))
													
												})
												
												write.csv(currentProject()$samples[[x]]$rep_dataFinalRaster, 
													    file = paste0("final_",currentProject()$samplesFiles[x],".csv"))
												write.csv(currentProject()$samples[[x]]$rep_dataFinalRasterNonCorr, 
													    file = paste0("finalCorr_",currentProject()$samplesFiles[x],".csv"))
												
											} else if(input$exportFormatData == ".csv"){
												
												lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_dataIntermRaster), by = 1), function(k){
													
													write.table(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]], 
															file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],".csv"), 
															sep = sep)
												})
												
												write.table(currentProject()$samples[[x]]$rep_dataFinalRaster, 
														file = paste0("final_",currentProject()$samplesFiles[x],".csv"), sep = sep)
												write.table(currentProject()$samples[[x]]$rep_dataFinalRasterNonCorr, 
														file = paste0("finalCorr_",currentProject()$samplesFiles[x],".csv"))
												
											} else {
												
												lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_dataIntermRaster), by = 1), function(k){
													
													write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]]), 
															file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],input$exportFormatData),
															sep = sep)
												})
												
												write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataFinalRaster), 
														file = paste0("final_",currentProject()$samplesFiles[x],input$exportFormatData), sep = sep)
												write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataFinalRasterNonCorr), 
														file = paste0("finalCorr_",currentProject()$samplesFiles[x],input$exportFormatData), sep = sep)
												
											}
										} else {}
									}
								} else {
									
									ToRemove <- list.files(, pattern = ".csv")[-match(currentProject()$samples[[x]]$rep_Files, list.files(, pattern = ".csv"))]
									invisible(file.remove(ToRemove))
									ToRemove <- list.files(, pattern = ".xls")[-match(currentProject()$samples[[x]]$rep_Files, list.files(, pattern = ".xls"))]
									invisible(file.remove(ToRemove))
									
									if(dir.exists(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x], "/graphics")) == TRUE){
										
										setwd(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[x], "/graphics"))
										ToRemove <- list.files(, pattern = ".jpg")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".jpeg")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".bmp")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".png")
										invisible(file.remove(ToRemove))
										ToRemove <- list.files(, pattern = ".tiff")
										invisible(file.remove(ToRemove))
									} else {}
									
								}
								
							}) #eo lapply
							
							info <- sprintf("%d%% done", round(100))
							setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
							
							setwd(espace1)
							
							close(pb)
							
							res <- tkmessageBox(title = "INFO !",message = "Project exported", icon = "info", type = "ok")
							
						})
						
					} else {}
					
				}
			}) #observe
		}
		
		#################################
		#################################
		########### Graphics Export  ####
		#################################
		#################################
		{
			observe({
				if(!is.null(input$ExportGraph)){
					if(input$ExportGraph > 0){
						isolate({
							
							espace1 <- getwd()
							
							temporaire <- input$standardIn
							
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results/standards/", temporaire, "/graphics")))
							
							setwd(paste0(projPath$temp,"/Results/standards/", temporaire, "/graphics"))
							
							if(!is.null(length(input$courveToExport)) & length(input$courveToExport) != 0){
								if(!is.null(length(input$ElementToExport)) & length(input$ElementToExport) != 0){
									
									pb <- tkProgressBar("Progress bar", "Graphic export in %",
												  0, 100, 0)
									
									#### Raw Data exporting #####
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("RawData_",temporaire ,".jpg"), width = 760, height = 400)
									} else {
										
										if(input$exportFormat == ".jpeg"){
											
											jpeg(filename = paste0("RawData_",temporaire ,".jpg"), 
											     width = input$exportwidth, height = input$exportheight)
											
										} else {}
										
										if(input$exportFormat == ".bpm"){
											
											bmp(filename = paste0("RawData_",temporaire ,".bmp"),
											    width = input$exportwidth, height = input$exportheight)
											
										} else {}
										
										if(input$exportFormat == ".png"){
											
											png(filename = paste0("RawData_",temporaire ,".png"), 
											    width = input$exportwidth, height = input$exportheight)
											
										} else {}
										
										if(input$exportFormat == ".tiff"){
											
											tiff(filename = paste0("RawData_",temporaire ,".tiff"), 
											     width = input$exportwidth, height = input$exportheight)
											
										} else {}
									}
									
									mat<- matrix(c(1,1,1,1,1,1,1,1,1,2),1)
									
									layout(mat)
									
									par(mar = c(5.1,5,4.1,1))
									
									if(length(currentNumber$temp) != 0 & !is.null(currentNISTData$temp)){
										
										maxY <- max(currentNISTData$temp, na.rm = TRUE)
										
										minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
										maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
										
										plot(currentNISTData$temp[,1], 
										     currentNISTData$temp[,input$ElementToExport[1]],
										     type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExport[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
										
										mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
										mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
										mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
										
										lapply(seq(from = 1, to = length(input$ElementToExport), by = 1), function(x){
											
											par(new = TRUE)
											
											plot(currentNISTData$temp[,1], 
											     currentNISTData$temp[,input$ElementToExport[x]],
											     type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExport[x] == names(color$temp))], 
											     xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
											
										})
										
										if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
											
											Temp$t  <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[1])[[2]]
											Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[2])[[2]]
											Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[1]])[[2]]
											Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[2]])[[2]]
											
										} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
											
											Temp$t <-  currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[1])[[2]]
											Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[2])[[2]]
											Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[1])[[2]]
											Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[2])[[2]]
											
										} else {}
										
										rect(currentNISTData$temp[Temp$t,1],-maxY,currentNISTData$temp[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
										rect(currentNISTData$temp[Temp1$t,1],-maxY,currentNISTData$temp[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
										
										abline(v = currentNISTData$temp[Temp$t,1], lty = "dashed", col = "grey", lwd = 2)
										abline(v = currentNISTData$temp[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
										abline(v = currentNISTData$temp[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
										abline(v = currentNISTData$temp[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
										
										lapply(input$ElementToExport, function(x){points(currentNISTData$temp[Temp$t,1], currentNISTData$temp[Temp$t,x], cex = 3, col ="grey")})
										lapply(input$ElementToExport, function(x){points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,x], cex = 3, col ="grey")})
										lapply(input$ElementToExport, function(x){points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
										lapply(input$ElementToExport, function(x){points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
										# }
										
									} else {}
									
									par(mar = c(0,0,2,1))
									plot(0,0, axes = FALSE, type = "n")
									
									legend(-1,1, legend = input$ElementToExport, bty = "n", 
										 col = color$temp[vapply(seq(from = 1, to = length(input$ElementToExport), by = 1), 
										 				function(x) {which(input$ElementToExport[x] == names(color$temp))},
										 				FUN.VALUE = numeric(1)
										 )
										 ], 
										 pch = 16, cex = 1.5)
									
									dev.off()
									
									info <- sprintf("%d%% done", round(10))
									setTkProgressBar(pb, 10, sprintf("Export (%s)", info), info)
									
									nbGraph <- floor(length(input$ElementToExport)/6)
									
									nRest <- length(input$ElementToExport)%%6
									
									if(nbGraph != 0){
										for(i in 1: nbGraph){
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("RawData_All_graph",i,".jpg"), width = 760, height = 400)
											} else{
												
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = paste0("RawData_All_graph",i,".jpg"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = paste0("RawData_All_graph",i,".bmp"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = paste0("RawData_All_graph",i,".png"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = paste0("RawData_All_graph",i,".tiff"), width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											
											par(mfrow = c(2,3), mar = c(3,4.1,2,2),  oma=c(0,0,1,0))
											
											for(j in (6*(i-1)+1):(6*i)){
												
												maxY <- max(currentNISTData$temp[,input$ElementToExport[j]], na.rm = TRUE)
												
												minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
												maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
												
												plot(currentNISTData$temp[,1], currentNISTData$temp[,input$ElementToExport[j]],type ="b", ylab = "", xlab = "", 
												     main = paste0("RawData_",input$ElementToExport[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
												
												mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
												mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												
												if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
													
													Temp$t  <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[1])[[2]]
													Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[2])[[2]]
													Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[1]])[[2]]
													Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[2]])[[2]]
													
												} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
													
													Temp$t <-  currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[1])[[2]]
													Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[2])[[2]]
													Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[1])[[2]]
													Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[2])[[2]]
												} else {}
												
												rect(currentNISTData$temp[Temp$t,1],-maxY,
												     currentNISTData$temp[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
												
												rect(currentNISTData$temp[Temp1$t,1],-maxY,
												     currentNISTData$temp[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
												
												abline(v = currentNISTData$temp[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
												abline(v = currentNISTData$temp[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
												abline(v = currentNISTData$temp[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
												abline(v = currentNISTData$temp[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
												
												points(currentNISTData$temp[Temp$t,1],  currentNISTData$temp[Temp$t,input$ElementToExport[j]],  cex = 3, col ="grey")
												points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,input$ElementToExport[j]], cex = 3, col ="grey")
												points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
												points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
												
											}
											
											title(input$standardIn, outer=TRUE, cex = 1.5)
											
											dev.off()
										}
									} else {}
									
									if(nRest != 0){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("RawData_All_graph",nbGraph+1,".jpg"), width = 760, height = 400)
										} else {
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("RawData_All_graph",nbGraph+1,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("RawData_All_graph",nbGraph+1,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("RawData_All_graph",nbGraph+1,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("RawData_All_graph",nbGraph+1,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,4.1,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*nbGraph+1): (6*nbGraph + nRest)){
											
											maxY <- max(currentNISTData$temp[,input$ElementToExport[j]], na.rm = TRUE)
											
											minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
											maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
											
											plot(currentNISTData$temp[,1], currentNISTData$temp[,input$ElementToExport[j]],type ="b", ylab = "", xlab = "",
											     main = paste0("RawData_",input$ElementToExport[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
											
											mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
											mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
											
											if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
												
												Temp$t  <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[1])[[2]]
												Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$bins[2])[[2]]
												Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[1]])[[2]]
												Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = input$plat[[2]])[[2]]
												
											} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
												
												Temp$t <-  currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[1])[[2]]
												Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$bins[2])[[2]]
												Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[1])[[2]]
												Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1], y = currentNISTRep$temp$plat[2])[[2]]
												
											} else {}
											
											rect(currentNISTData$temp[Temp$t,1], -maxY,currentNISTData$temp[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
											rect(currentNISTData$temp[Temp1$t,1],-maxY,currentNISTData$temp[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
											
											abline(v = currentNISTData$temp[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
											abline(v = currentNISTData$temp[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
											abline(v = currentNISTData$temp[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
											abline(v = currentNISTData$temp[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
											
											points(currentNISTData$temp[Temp$t,1],  currentNISTData$temp[Temp$t,input$ElementToExport[j]],  cex = 3, col ="grey")
											points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,input$ElementToExport[j]], cex = 3, col ="grey")
											points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
											points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
											
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
										
									}
									
									info <- sprintf("%d%% done", round(40))
									setTkProgressBar(pb, 40, sprintf("Export (%s)", info), info)
									
									#### reduced Data exporting #####
									
									for(i in seq(from = 1, to = length(input$ElementToExport), by = 1)){
										for(j in seq(from = 1, to = length(input$courveToExport), by = 1)){
											
											suppressWarnings(dir.create(paste0(projPath$temp,"/Results/standards/", temporaire, "/graphics/", input$ElementToExport[i])))
											
											setwd(paste0(projPath$temp,"/Results/standards/", temporaire, "/graphics/", input$ElementToExport[i]))
											
											if(input$courveToExport[j] == "Blank removed"){tempName <- "Blank_removed"
											} else if(input$courveToExport[j] == "> LOD"){tempName <- "Supp_LOD"
											} else if(input$courveToExport[j] == "Outliers free"){tempName <- "Outliers_free"
											} else{tempName <- input$courveToExport[j]}
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("ReducedData",tempName,".jpg"), width = 760, height = 400)
											} else {
												
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = paste0("ReducedData",tempName,".jpg"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = paste0("ReducedData",tempName,".bmp"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = paste0("ReducedData",tempName,".png"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = paste0("ReducedData",tempName,".tiff"), width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											
											if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
												if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
													if(is.finite(Temp$t)){
														
														curve <- currentNISTRep$temp$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), 
																				 plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
														
													} else {}
												} else {}
											} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
												curve <- currentNISTRep$temp$renderData(curve = input$courveToExport[j])
											} else {}
											
											if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
												plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
												text(1,0.5, labels = "No data different from NA", cex = 2)
											} else{
												par(mar = c(5.1,4.1,4.1,2))
												plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
												mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
												mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												mtext(paste("Data Reduced",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
											}
											
											dev.off()
										}
										
									}
									
									info <- sprintf("%d%% done", round(70))
									setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)
									
									for(i in seq(from = 1, to = length(input$ElementToExport), by = 1)){
										
										setwd(paste0(projPath$temp,"/Results/standards/", temporaire, "/graphics/", input$ElementToExport[i]))
										
										if(length(input$courveToExport) <= 6) {
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("ReducedData_All.jpg"), width = 760, height = 400)
											} else {
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All.jpg", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All.bmp", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All.png", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All.tiff", width = input$exportwidth, height = input$exportheight)
												} else{}
											}
											par(mfrow = c(2,3))
											for(j in seq(from = 1, to = length(input$courveToExport), by = 1)){
												
												if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
													if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
														if(is.finite(Temp$t)){
															
															curve <- currentNISTRep$temp$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), 
																					 plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
														}
													}
												} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
													curve <- currentNISTRep$temp$renderData(curve = input$courveToExport[j])
												} else {}
												
												if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												} else{
													par(mar = c(5.1,4.1,4.1,2))
													plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste("Data Reduced",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
												}
											}
											dev.off()
											
										} else {
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("ReducedData_All.jpg"), width = 760, height = 400)
											} else {
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All.jpg", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All.bmp", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All.png", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All.tiff", width = input$exportwidth, height = input$exportheight)
												} else{}
											}
											par(mfrow = c(2,3))
											for(j in seq(from = 1, to = (length(input$courveToExport)-1), by = 1)){
												
												if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
													if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
														if(is.finite(Temp$t)){
															
															curve <- currentNISTRep$temp$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), 
																					 plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
														}
													}
												} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
													curve <- currentNISTRep$temp$renderData(curve = input$courveToExport[j])
												} else {}
												
												if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												} else{
													par(mar = c(5.1,4.1,4.1,2))
													plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste("Data Reduced",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
												}
											}
											dev.off()
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("ReducedData_All2.jpg"), width = 760, height = 400)
											} else{
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All2.jpg", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All2.bmp", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All2.png", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All2.tiff", width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											par(mfrow = c(2,3))
											if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
												if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
													if(is.finite(Temp$t)){
														curve <- currentNISTRep$temp$getData(curve = input$courveToExport[length(input$courveToExport)], 
																				 bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
													} else {}
												}
											} else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
												curve <- currentNISTRep$temp$renderData(curve = input$courveToExport[length(input$courveToExport)])
											} else {}
											if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
												plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
												text(1,0.5, labels = "No data different from NA", cex = 2)
											} else{
												par(mar = c(5.1,4.1,4.1,2))
												plot(curve[,1], curve[,grep(input$ElementToExport[length(input$ElementToExport)], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
												mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
												mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												mtext(paste("Data Reduced",input$ElementToExport[i], input$courveToExport[length(input$courveToExport)]),side=3,line=0.75, cex=1.2, font = 2)
											}
											dev.off()
											
										}
										
									}
									
									info <- sprintf("%d%% done", round(90))
									setTkProgressBar(pb, 90, sprintf("Export (%s)", info), info)
									
									setwd(espace1)
									
									info <- sprintf("%d%% done", round(100))
									setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
									
									close(pb)
									
									res <- tkmessageBox(title = "INFO !",message = "Graphics exported", icon = "info", type = "ok")
									
								} else {tkmessageBox(message = "You need to select at least one element to export!", icon = "error", type = "ok")}
							} else {tkmessageBox(message = "You need to select at least one curve to export!", icon = "error", type = "ok")}
							
						})
					}
					
				} else {}
			})
			
			observe({
				if(!is.null(input$ExportGraphS)){
					
					if(input$ExportGraphS > 0){
						isolate({
							
							espace1 <- getwd()
							
							temporaire <- input$SampleIn2
							
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/", input$SampleIn, "/", temporaire, "/graphics")))
							
							setwd(paste0(projPath$temp,"/Results/samples/", input$SampleIn, "/", temporaire, "/graphics"))
							
							if(!is.null(length(input$courveToExportS)) & length(input$courveToExportS) != 0){
								if(!is.null(length(input$ElementToExportS)) & length(input$ElementToExportS) != 0){
									
									pb <- tkProgressBar("Progress bar", "Graphic export in %",
												  0, 100, 0)
									
									#### Raw Data exporting #####
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("RawData_",temporaire ,".jpg"), width = 760, height = 400)
									} else{
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("RawData_",temporaire ,".jpg"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".bpm"){
											bmp(filename = paste0("RawData_",temporaire ,".bmp"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("RawData_",temporaire ,".png"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("RawData_",temporaire ,".tiff"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
									}
									
									mat<- matrix(c(1,1,1,1,1,1,1,1,1,2),1)
									
									layout(mat)
									
									par(mar = c(5.1,4.1,4.1,2))
									
									if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) != 0){
										
										maxY <- max(currentSampleData$temp, na.rm = TRUE)
										
										minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
										maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
										
										plot(currentSampleData$temp[,1], currentSampleData$temp[,input$ElementToExportS[1]],type ="b", ylab = "", xlab = "", main = "", 
										     col = color$temp[which(input$ElementToExportS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
										
										mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
										mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
										mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
										
										lapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), function(x){
											
											par(new = TRUE)
											
											plot(currentSampleData$temp[,1], currentSampleData$temp[,input$ElementToExportS[x]],type ="b", 
											     ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExportS[x] == names(color$temp))], 
											     xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
											
										})
										
										if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
											TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[1])[[2]]
											Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[2])[[2]]
											Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[1])[[2]]
											Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[2])[[2]]
										} else {
											TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[1])[[2]]
											Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[2])[[2]]
											Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[1])[[2]]
											Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[2])[[2]]
											
										}
										
										
										rect(currentSampleData$temp[TempS$t,1],-maxY,currentSampleData$temp[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
										rect(currentSampleData$temp[Temp1S$t,1],-maxY,currentSampleData$temp[Temp2S$t,1],(1+10/100)*maxY, col = "#4F3CBC30", border = NA)
										
										abline(v = currentSampleData$temp[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
										abline(v = currentSampleData$temp[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
										abline(v = currentSampleData$temp[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
										abline(v = currentSampleData$temp[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
										
										lapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), function(x){points(currentSampleData$temp[TempS$t,1],  currentSampleData$temp[TempS$t,x],  cex = 3, col ="grey")})
										lapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), function(x){points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,x], cex = 3, col ="grey")})
										lapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), function(x){points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
										lapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), function(x){points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
										
									} else {}
									
									par(mar = c(0,0,2,1))
									plot(0,0, axes = FALSE, type = "n")
									legend(-1,1, legend = input$ElementToExportS, bty = "n", col = color$temp[vapply(seq(from = 1, to = length(input$ElementToExportS), by = 1), 
																						   function(x) {which(input$ElementToExportS[x] == names(color$temp))
																						   }, 
																						   FUN.VALUE = numeric(1)
									)
									], pch = 16, cex = 1.5)
									
									dev.off()
									
									info <- sprintf("%d%% done", round(10))
									setTkProgressBar(pb, 10, sprintf("Export (%s)", info), info)
									
									nbGraph <- floor(length(input$ElementToExportS)/6)
									
									nRest <- length(input$ElementToExportS)%%6
									
									if(nbGraph > 0){
										for(i in seq(from = 1, to = nbGraph, by = 1)){
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("RawData_All_graph",i,".jpg"), width = 760, height = 400)
											} else {
												
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = paste0("RawData_All_graph",i,".jpg"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = paste0("RawData_All_graph",i,".bmp"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = paste0("RawData_All_graph",i,".png"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = paste0("RawData_All_graph",i,".tiff"), width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											
											par(mfrow = c(2,3), mar = c(3,4.1,2,2),  oma=c(0,0,1,0))
											
											for(j in (6*(i-1)+1):(6*i)){
												
												maxY <- max(currentSampleData$temp[, input$ElementToExportS[j]], na.rm = TRUE)
												
												minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
												maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
												
												plot(currentSampleData$temp[,1], currentSampleData$temp[,input$ElementToExportS[j]],type ="b", ylab = "", xlab = "", main = paste0("RawData_",input$ElementToExportS[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
												mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
												mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												
												if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
													TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[1])[[2]]
													Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[2])[[2]]
													Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[1])[[2]]
													Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[2])[[2]]
												} else {
													TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[1])[[2]]
													Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[2])[[2]]
													Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[1])[[2]]
													Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[2])[[2]]
													
												}
												rect(currentSampleData$temp[TempS$t,1],-maxY,currentSampleData$temp[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
												rect(currentSampleData$temp[Temp1S$t,1],-maxY,currentSampleData$temp[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
												
												abline(v = currentSampleData$temp[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleData$temp[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleData$temp[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												abline(v = currentSampleData$temp[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												
												points(currentSampleData$temp[TempS$t,1],  currentSampleData$temp[TempS$t,input$ElementToExportS[j]],  cex = 3, col ="grey")
												points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,input$ElementToExportS[j]], cex = 3, col ="grey")
												points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
												points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
												
											}
											
											title(temporaire, outer=TRUE, cex = 1.5)
											
											dev.off()
										}
									} else {}
									
									if(nRest != 0){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("RawData_All_graph",nbGraph+1,".jpg"), width = 760, height = 400)
										}else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("RawData_All_graph",nbGraph+1,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("RawData_All_graph",nbGraph+1,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("RawData_All_graph",nbGraph+1,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("RawData_All_graph",nbGraph+1,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,4.1,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*nbGraph+1): (6*nbGraph + nRest)){
											
											maxY <- max(currentSampleData$temp[, input$ElementToExportS[j]], na.rm = TRUE)
											
											minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
											maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
											
											plot(currentSampleData$temp[,1], currentSampleData$temp[,input$ElementToExportS[j]],type ="b", ylab = "", xlab = "", main = paste0("RawData_",input$ElementToExportS[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
											mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
											mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
											
											if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
												TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[1])[[2]]
												Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[2])[[2]]
												Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[1])[[2]]
												Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[2])[[2]]
											} else {
												TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[1])[[2]]
												Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[2])[[2]]
												Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[1])[[2]]
												Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[2])[[2]]
												
											}
											
											rect(currentSampleData$temp[TempS$t,1],-maxY,currentSampleData$temp[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
											rect(currentSampleData$temp[Temp1S$t,1],-maxY,currentSampleData$temp[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
											
											abline(v = currentSampleData$temp[TempS$t,1], lty = "dashed", col = ("grey"), lwd = 2)
											abline(v = currentSampleData$temp[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
											abline(v = currentSampleData$temp[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
											abline(v = currentSampleData$temp[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
											
											points(currentSampleData$temp[TempS$t,1],  currentSampleData$temp[TempS$t,input$ElementToExportS[j]],  cex = 3, col ="grey")
											points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,input$ElementToExportS[j]], cex = 3, col ="grey")
											points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
											points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
											
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
										
									}
									
									info <- sprintf("%d%% done", round(40))
									setTkProgressBar(pb, 40, sprintf("Export (%s)", info), info)
									
									#### Reduced Data exporting #####
									
									for(i in seq(from = 1, to = length(input$ElementToExportS), by = 1)){
										for(j in seq(from = 1, to = length(input$courveToExportS), by = 1)){
											
											suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i])))
											
											setwd(paste0(projPath$temp,"/Results/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i]))
											
											if(input$courveToExportS[j] == "Blank removed"){tempNameS <- "Blank_removed"
											} else if(input$courveToExportS[j] == "> LOD"){tempNameS <- "Supp_LOD"
											} else if(input$courveToExportS[j] == "Conc. corrected"){tempNameS <- "Conc._corrected"
											} else{tempNameS <- input$courveToExportS[j]}
											
											if(is.null(input$exportFormat)){
												jpeg(filename = paste0("ReducedData",tempNameS,".jpg"), width = 760, height = 400)
											} else{
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = paste0("ReducedData",tempNameS,".jpg"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = paste0("ReducedData",tempNameS,".bmp"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = paste0("ReducedData",tempNameS,".png"), width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = paste0("ReducedData",tempNameS,".tiff"), width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											
											if(length(currentSampleRep$temp) != 0){
												
												if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) == 0){
													
												} else {
													if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
														if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
															if(is.finite(TempS$t)){
																
																curveS <- currentSampleRep$temp$getData(curve = input$courveToExportS[j], 
																						    bins = c(TempS$t, Temp0S$t), 
																						    plat = c(Temp1S$t,Temp2S$t), 
																						    name = input$SampleIn2, 
																						    meanStand = currentProject()$standards[[1]]$rep_dataFinale, 
																						    rankSample = currentProject()$sampleRank, 
																						    rankStandard = currentProject()$standardRank,
																						    model = currentProject()$regressionModel, 
																						    calibFile = currentProject()$EtalonData, 
																						    correction = currentProject()$machineCorrection, 
																						    rempl = currentProject()$valRemplace,threshold = currentProject()$R2Threshold)
																
															} else {}
														}
													} else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 1){
														if(is.finite(TempS$t)){
															curveS <- currentSampleRep$temp$renderData(curve = input$courveToExportS[j])
														} else {}
														
													} else {}
												}
												
											} else {}
											
											if(!is.null(curveS)){
												if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												} else{
													par(mar = c(5.1,4.1,4.1,2))
													plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
												}
											} else{}
											dev.off()
										}
										
									}
									
									info <- sprintf("%d%% done", round(70))
									setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)
									
									for(i in seq(from = 1, to = length(input$ElementToExportS), by = 1)){
										
										setwd(paste0(projPath$temp,"/Results/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i]))
										
										if(length(input$courveToExportS) <= 6) {
											
											if(is.null(input$exportFormat)){
												jpeg(filename = "ReducedData_All.jpg", width = 760, height = 400)
											} else{
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All.jpg", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All.bmp", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All.png", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All.tiff", width = input$exportwidth, height = input$exportheight)
												} else{}
											}
											par(mfrow = c(2,3))
											for(j in seq(from = 1, to = length(input$courveToExportS), by = 1)){
												
												if(length(currentSampleRep$temp) != 0){
													if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) == 0){
														
													}else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
														if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
															if(is.finite(TempS$t)){curveS <- currentSampleRep$temp$getData(curve = input$courveToExportS[j], 
																									   bins = c(TempS$t, Temp0S$t), 
																									   plat = c(Temp1S$t,Temp2S$t), 
																									   name = input$SampleIn2, 
																									   meanStand = currentProject()$standards[[1]]$rep_dataFinale, 
																									   rankSample = currentProject()$sampleRank, 
																									   rankStandard = currentProject()$standardRank,
																									   model = currentProject()$regressionModel, 
																									   calibFile = currentProject()$EtalonData, 
																									   correction = currentProject()$machineCorrection, 
																									   rempl = currentProject()$valRemplace, 
																									   threshold = currentProject()$R2Threshold)
															} else {}
														}
													} else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 1){
														
														if(is.finite(TempS$t)){
															curveS <- currentSampleRep$temp$renderData(curve = input$courveToExportS[j])
														} else {}
														
													} else {}
													
												} else {}
												
												if(!is.null(curveS)){
													if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													} else {
														par(mar = c(3.5,3.7,1.75,1))
														plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													}
												}else{}
												
											}
											dev.off()
											
										} else{
											
											if(is.null(input$exportFormat)){
												jpeg(filename = "ReducedData_All.jpg", width = 760, height = 400)
											} else{
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All.jpg", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All.bmp", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All.png", width = input$exportwidth, height = input$exportheight)
												} else{}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All.tiff", width = input$exportwidth, height = input$exportheight)
												} else{}
											}
											par(mfrow = c(2,3))
											for(j in seq(from = 1, to = length(input$courveToExportS), by = 1)){
												
												if(length(currentSampleRep$temp) != 0){
													if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) == 0){
														
													}else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
														if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
															if(is.finite(TempS$t)){curveS <- currentSampleRep$temp$getData(curve = input$courveToExportS[j], 
																									   bins = c(TempS$t, Temp0S$t), 
																									   plat = c(Temp1S$t,Temp2S$t), 
																									   name = input$SampleIn2,
																									   meanStand = currentProject()$standards[[1]]$rep_dataFinale, 
																									   rankSample = currentProject()$sampleRank, 
																									   rankStandard = currentProject()$standardRank,
																									   model = currentProject()$regressionModel, 
																									   calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, 
																									   rempl = currentProject()$valRemplace, 
																									   threshold = currentProject()$R2Threshold)
															} else {}
														}
													} else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 1){
														
														if(is.finite(TempS$t)){
															curveS <- currentSampleRep$temp$renderData(curve = input$courveToExportS[j])
														} else {}
														
													} else {}
													
												} else {}
												
												if(!is.null(curveS)){
													if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													} else {
														par(mar = c(5.1,4.1,4.1,2))
														plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													}
												}else{}
												
											}
											dev.off()
											
											if(is.null(input$exportFormat)){
												jpeg(filename = "ReducedData_All2.jpg", width = 760, height = 400)
											}  else{
												if(input$exportFormat == ".jpeg"){
													jpeg(filename = "ReducedData_All2.jpg", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".bmp"){
													bmp(filename = "ReducedData_All2.bmp", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".png"){
													png(filename = "ReducedData_All2.png", width = input$exportwidth, height = input$exportheight)
												} else {}
												
												if(input$exportFormat == ".tiff"){
													tiff(filename = "ReducedData_All2.tiff", width = input$exportwidth, height = input$exportheight)
												} else {}
											}
											par(mfrow = c(2,3))
											for(j in (length(input$courveToExportS)-2): (length(input$courveToExportS))){
												
												if(length(currentSampleRep$temp) != 0){
													if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) == 0){
													}else{
														if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 0){
															if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
																if(is.finite(TempS$t)){curveS <- currentSampleRep$temp$getData(curve = input$courveToExportS[j], 
																										   bins = c(TempS$t, Temp0S$t), 
																										   plat = c(Temp1S$t,Temp2S$t), 
																										   name = input$SampleIn2, 
																										   meanStand = currentProject()$standards[[1]]$rep_dataFinale, 
																										   rankSample = currentProject()$sampleRank, 
																										   rankStandard = currentProject()$standardRank,
																										   model = currentProject()$regressionModel, 
																										   calibFile = currentProject()$EtalonData, 
																										   correction = currentProject()$machineCorrection, 
																										   rempl = currentProject()$valRemplace,
																										   threshold = currentProject()$R2Threshold)
																} else {}
															}
															
														} else if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)]%%2) == 1){
															if(is.finite(TempS$t)){curveS <- currentSampleRep$temp$renderData(curve = input$courveToExportS[j])
															}
															
														} else {}
													}
												} else {}
												
												if(!is.null(curveS)){
													if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													} else{
														par(mar =  c(5.1,4.1,4.1,2))
														plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste("Data Reduced",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
													}
												} else{}
												
											}
											dev.off()
											
										}
										
									}
									
									info <- sprintf("%d%% done", round(90))
									setTkProgressBar(pb, 90, sprintf("Export (%s)", info), info)
									
									setwd(espace1)
									
									info <- sprintf("%d%% done", round(100))
									setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
									
									close(pb)
									
									res <- tkmessageBox(title = "INFO !",message = "Graphics exported", icon = "info", type = "ok")
								} else {tkmessageBox(message = "You need to select at least one element to export!", icon = "error", type = "ok")}
							} else {tkmessageBox(message = "You need to select at least one curve to export!", icon = "error", type = "ok")}
						})
					} else {}
					
				} else {}
				
			})
			
			observe({
				if(!is.null(input$MachDriftExportGraph)){
					
					if(input$MachDriftExportGraph>0){
						
						isolate({
							espace1 <- getwd()
							
							setwd(paste0(projPath$temp,"/Results/"))
							
							if(is.null(length(input$MachDriftElementToExport)) | length(input$MachDriftElementToExport) == 0){
								tkmessageBox(message = "You need to select at least one element to export!", icon = "error", type = "ok")
							}else{
								
								pb <- tkProgressBar("Progress bar", "Graphic export in %",
											  0, 100, 0)
								
								threeTemp <-intersect(which(currentProject()$nbCalib >= 3), vapply(seq(from = 1, to = length(input$MachDriftElementToExport), by = 1), 
																			 function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))},
																			 FUN.VALUE = numeric(1)))
								
								three <- input$MachDriftElementToExport[threeTemp]
								
								nbGraph <- floor(length(three)/6)
								
								nRest <- length(three)%%6
								
								temporaryTab <- currentProject()$standards[[1]]$rep_dataFinale
								
								temp <- str_sub(rownames(temporaryTab), 1, -6)
								
								X <- vector()
								for (i in seq(from = 1, to = length(currentProject()$standardsFiles), by = 1)){
									X[i] <- currentProject()$standardRank[which(names(currentProject()$standardRank) == temp[i])]
									
								}
								
								if(nbGraph > 0){
									
									for(i in seq(from = 1, to = nbGraph, by = 1)){
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("Machine_Drift_3_&_Sup",i,".jpg"), width = 760, height = 400)
										} else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("Machine_Drift_3_&_Sup",i,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("Machine_Drift_3_&_Sup",i,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("Machine_Drift_3_&_Sup",i,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("Machine_Drift_3_&_Sup",i,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*(i-1)+1):(6*i)){
											
											par(mar = c(7,4.1,2.1,2.1), bg = NA)
											
											min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]], na.rm = TRUE)*3
											
											max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]], na.rm = TRUE)*3
											
											currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[threeTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
											
											abline(a = currentProject()$regressionModel[threeTemp[j],5], b= currentProject()$regressionModel[threeTemp[j],6], col ="red", lty = 2)
											
											mtext(side = 3, line = 1, text = currentProject()$listeElem[threeTemp[j]])
											
											mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[threeTemp[j]],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[threeTemp[j],5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[threeTemp[j],6],3)))
											mtext(side = 1, cex = 0.7, line = 4.5, text = paste0("slope test: ", round(currentProject()$regressionModel[threeTemp[j],4], 2)))
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
									}
								} else {}
								
								if(nRest != 0){
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("Machine_Drift_3_&_Sup",nbGraph + 1,".jpg"), width = 760, height = 400)
									} else {
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("Machine_Drift_3_&_Sup",nbGraph + 1,".jpg"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".bmp"){
											bmp(filename = paste0("Machine_Drift_3_&_Sup",nbGraph + 1,".bmp"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("Machine_Drift_3_&_Sup",nbGraph + 1,".png"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("Machine_Drift_3_&_Sup",nbGraph + 1,".tiff"), width = input$exportwidth, height = input$exportheight)
										} else {}
									}
									
									par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
									
									for(j in (6*nbGraph+1): (6*nbGraph+nRest)){
										
										par(mar = c(7,4.1,2.1,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]], na.rm = TRUE)*3
										
										max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]], na.rm = TRUE)*3
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),threeTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),threeTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[threeTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
										
										abline(a = currentProject()$regressionModel[threeTemp[j],5], b= currentProject()$regressionModel[threeTemp[j],6], col ="red", lty = 2)
										
										mtext(side = 3, line = 1, text = currentProject()$listeElem[threeTemp[j]])
										
										mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[threeTemp[j]],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[threeTemp[j],5],3), " + X (Stand. order) * ", round(currentProject()$regressionModel[threeTemp[j],6],3)))
										mtext(side = 1, cex = 0.7, line = 4.5, text = paste0("slope test: ", round(currentProject()$regressionModel[threeTemp[j],4], 2)))
										
									}
									
									title(input$standardIn, outer=TRUE, cex = 1.5)
									
									dev.off()
									
								} else {}
								
								info <- sprintf("%d%% done", round(50))
								setTkProgressBar(pb, 50, sprintf("Export (%s)", info), info)
								
								######
								
								twoTemp <-intersect(which(currentProject()$nbCalib == 2), vapply(seq(from = 1, to = length(input$MachDriftElementToExport), by = 1), 
																		     function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))},
																		     FUN.VALUE = numeric(1)
								)
								)
								
								two <- input$MachDriftElementToExport[twoTemp]
								
								nbGraph <- floor(length(two)/6)
								
								nRest <- length(two)%%6
								
								if(nbGraph > 0){
									
									for(i in seq(from = 1, to = nbGraph, by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("Machine_Drift_2",i,".jpg"), width = 760, height = 400)
										} else {
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("Machine_Drift_2",i,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("Machine_Drift_2",i,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("Machine_Drift_2",i,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("Machine_Drift_2",i,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*(i-1)+1):(6*i)){
											
											par(mar = c(6,4.1,2.1,2.1), bg = NA)
											
											min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]], na.rm = TRUE)*3
											
											max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]], na.rm = TRUE)*3
											
											currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[twoTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
											
											abline(a = currentProject()$regressionModel[twoTemp[j],5], b= currentProject()$regressionModel[twoTemp[j],6], col ="red", lty = 2)
											
											mtext(side = 3, line = 1, text = currentProject()$listeElem[twoTemp[j]])
											
											mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[twoTemp[j]],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[twoTemp[j],5],3), " + X (Stand. order) * ", round(currentProject()$regressionModel[twoTemp[j],6],3)))
											
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
									}
								} else {}
								
								if(nRest != 0){
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("Machine_Drift_2",nbGraph + 1,".jpg"), width = 760, height = 400)
									} else{
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("Machine_Drift_2",nbGraph + 1,".jpg"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".bmp"){
											bmp(filename = paste0("Machine_Drift_2",nbGraph + 1,".bmp"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("Machine_Drift_2",nbGraph + 1,".png"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("Machine_Drift_2",nbGraph + 1,".tiff"), width = input$exportwidth, height = input$exportheight)
										} else {}
									}
									
									par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
									
									for(j in (6*nbGraph+1): (6*nbGraph+nRest)){
										
										par(mar = c(6,4.1,2.1,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]], na.rm = TRUE)*3
										
										max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]], na.rm = TRUE)*3
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),twoTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),twoTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[twoTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
										
										abline(a = currentProject()$regressionModel[twoTemp[j],5], b= currentProject()$regressionModel[twoTemp[j],6], col ="red", lty = 2)
										
										mtext(side = 3, line = 1, text = currentProject()$listeElem[twoTemp[j]])
										
										mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[twoTemp[j]],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[twoTemp[j],5],3), " + X (Stand. order) * ", round(currentProject()$regressionModel[twoTemp[j],6],3)))
										
									}
									
									title(input$standardIn, outer=TRUE, cex = 1.5)
									
									dev.off()
									
								} else {}
								
								info <- sprintf("%d%% done", round(70))
								setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)
								
								######
								
								oneTemp <-intersect(which(currentProject()$nbCalib == 1), vapply(seq(from = 1, to = length(input$MachDriftElementToExport), by = 1), 
																		     function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))},
																		     FUN.VALUE = numeric(1)
								))
								
								one <- input$MachDriftElementToExport[oneTemp]
								
								nbGraph <- floor(length(one)/6)
								
								nRest <- length(one)%%6
								
								if(nbGraph > 0){
									
									for(i in seq(from = 1, to = nbGraph, by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("Machine_Drift_1",i,".jpg"), width = 760, height = 400)
										} else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("Machine_Drift_1",i,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("Machine_Drift_1",i,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("Machine_Drift_1",i,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("Machine_Drift_1",i,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*(i-1)+1):(6*i)){
											
											par(mar = c(4.1,4.1,2,2.1), bg = NA)
											
											min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE) - 20/100 * min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE)
											
											max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE) + 20/100 * max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE)
											
											currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),oneTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[oneTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
											
											abline(a = currentProject()$regressionModel[oneTemp[j],5], b= currentProject()$regressionModel[oneTemp[j],6], col ="red", lty = 2)
											
											mtext(side = 3, line = 1, text = currentProject()$listeElem[oneTemp[j]])
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
									}
								} else {}
								
								if(nRest != 0){
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("Machine_Drift_1",nbGraph + 1,".jpg"), width = 760, height = 400)
									} else{
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("Machine_Drift_1",nbGraph + 1,".jpg"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".bmp"){
											bmp(filename = paste0("Machine_Drift_1",nbGraph + 1,".bmp"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("Machine_Drift_1",nbGraph + 1,".png"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("Machine_Drift_1",nbGraph + 1,".tiff"), width = input$exportwidth, height = input$exportheight)
										} else {}
									}
									
									par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
									
									for(j in (6*nbGraph+1): (6*nbGraph+nRest)){
										
										par(mar = c(4.1,4.1,2,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE) - 20/100 * min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE)
										
										max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE) + 20/100 * max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], na.rm = TRUE)
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),oneTemp[j]], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),oneTemp[j]],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[oneTemp[j]],"/Cps_", currentProject()$elemStand), xlab = "")
										
										abline(a = currentProject()$regressionModel[oneTemp[j],5], b= currentProject()$regressionModel[oneTemp[j],6], col ="red", lty = 2)
										
										mtext(side = 3, line = 1, text = currentProject()$listeElem[oneTemp[j]])
									}
									
									title(input$standardIn, outer=TRUE, cex = 1.5)
									
									dev.off()
									
								} else {}
								
								info <- sprintf("%d%% done", round(80))
								setTkProgressBar(pb, 80, sprintf("Export (%s)", info), info)
								
								######
								
								zeroTemp <-intersect(which(currentProject()$nbCalib == 0), vapply(seq(from = 1, to = length(input$MachDriftElementToExport), by = 1), 
																			function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))},
																			FUN.VALUE = numeric(1)
								)
								)
								
								zero <- input$MachDriftElementToExport[zeroTemp]
								
								nbGraph <- floor(length(zero)/6)
								
								nRest <- length(zero)%%6
								
								if(nbGraph > 0){
									for(i in seq(from = 1, to = nbGraph, by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("Machine_Drift_0",i,".jpg"), width = 760, height = 400)
										} else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("Machine_Drift_0",i,".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bmp"){
												bmp(filename = paste0("Machine_Drift_0",i,".bmp"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("Machine_Drift_0",i,".png"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("Machine_Drift_0",i,".tiff"), width = input$exportwidth, height = input$exportheight)
											} else {}
										}
										
										par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
										
										for(j in (6*(i-1)+1):(6*i)){
											
											par(mar = c(4.1,4.1,2,2.1), bg = NA)
											
											plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
											text(1,0.5, labels = "No data different from NA", cex = 2)
											
											mtext(side = 3, line = 1, text = currentProject()$listeElem[zeroTemp[j]])
											
										}
										
										title(input$standardIn, outer=TRUE, cex = 1.5)
										
										dev.off()
									}
								} else {}
								
								if(nRest != 0){
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("Machine_Drift_0",nbGraph + 1,".jpg"), width = 760, height = 400)
									} else{
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("Machine_Drift_0",nbGraph + 1,".jpg"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".bmp"){
											bmp(filename = paste0("Machine_Drift_0",nbGraph + 1,".bmp"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("Machine_Drift_0",nbGraph + 1,".png"), width = input$exportwidth, height = input$exportheight)
										} else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("Machine_Drift_0",nbGraph + 1,".tiff"), width = input$exportwidth, height = input$exportheight)
										} else {}
									}
									
									par(mfrow = c(2,3), mar = c(3,3.8,2,2),  oma=c(0,0,1,0))
									
									for(j in (6*nbGraph+1): (6*nbGraph+nRest)){
										
										par(mar = c(4.1,4.1,2,2.1), bg = NA)
										
										par(mar = c(4.1,4.1,2,2.1), bg = NA)
										
										plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
										text(1,0.5, labels = "No data different from NA", cex = 2)
										
										mtext(side = 3, line = 1, text = currentProject()$listeElem[zeroTemp[j]])
										
									}
									
									title(input$standardIn, outer=TRUE, cex = 1.5)
									
									dev.off()
									
								} else {}
								
								info <- sprintf("%d%% done", round(100))
								setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
								
								setwd(espace1)
								
								close(pb)
								
								res <- tkmessageBox(title = "INFO !",message = "Graphics exported", icon = "info", type = "ok")
							}
						})
						
					} else {}
				}
				else{}
			})
			
			observe({
				if(!is.null(input$RealignExportGraph)){
					
					if(input$RealignExportGraph>0){
						
						isolate({
							espace1 <- getwd()
							
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/", input$selectRealign, "/graphics")))
							
							setwd(paste0(projPath$temp,"/Results/samples/", input$selectRealign, "/graphics"))
							
							if(is.null(length(input$RealignElementToExport)) | length(input$RealignElementToExport) == 0){
								tkmessageBox(message = "You need to select at least one element to export!", icon = "error", type = "ok")
							}else{
								pb <- tkProgressBar("Progress bar", "Graphic export in %",
											  0, 100, 0)
								
								#### single graphic /elmenet #####
								
								setwd(paste0(projPath$temp,"/Results/samples/", input$selectRealign, "/graphics"))
								
								if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
									
									for(i in seq(from = 1, to = length(input$RealignElementToExport), by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[i] ,".jpg"), width = 480, height = 400)
										} else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[i],".jpg"), width = input$exportwidth, height = input$exportheight)
											} else {}
											
											if(input$exportFormat == ".bpm"){
												bmp(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[i] ,".bmp"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[i] ,".png"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[i] ,".tiff"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
										}
										
										if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]}))))) == 0){
											plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
											text(1,0.5, labels = "No data different from NA for this element", cex = 2)
										} else {
											
											ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]})), na.rm = TRUE))
											
											xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(j){tabProvSample$temp[[j]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(j){tabProvSample$temp[[j]][,1]}))))
											
											lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
												
												plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[i] , col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
												
												par(new = TRUE)
												
												
											})
											
											legend("topright", legend = input$ReplicateSample, col = vapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), 
																					    function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]},
																					    FUN.VALUE = character(1)
											), lty = c(1,1))
											
										}
										
										dev.off()
									}
									
								}else{
									
									for(y in seq(from = 1, to = length(input$RealignElementToExport), by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[y] ,".jpg"), width = 480, height = 400)
										} else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[y],".jpg"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".bpm"){
												bmp(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[y] ,".bmp"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[y] ,".png"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("RealignData_",input$selectRealign,"_", input$RealignElementToExport[y] ,".tiff"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
										}
										
										if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
											plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
											text(1,0.5, labels = "No data different from NA for this element", cex = 2)
										}else{
											
											ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
											
											xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
											
											lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
												
												plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
												
												par(new = TRUE)
												
											})
											
											plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
											
											legend("topright", legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), 
																																							     function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																							     FUN.VALUE = character(1)
											), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
										}
										
										dev.off()
									}
									
								}
								
								info <- sprintf("%d%% done", round(50))
								setTkProgressBar(pb, 50, sprintf("Export (%s)", info), info)
								
								#### whole graphic #####
								
								setwd(paste0(projPath$temp,"/Results/samples/", input$selectRealign, "/graphics"))
								
								nbGraph <- floor(length(input$RealignElementToExport)/5)
								
								nRest <- length(input$RealignElementToExport)%%5
								
								if(nbGraph > 0){
									
									for(i in seq(from = 1, to = nbGraph, by = 1)){
										
										if(is.null(input$exportFormat)){
											jpeg(filename = paste0("RealignData_",i ,".jpg"), width = 760, height = 400)
										}else{
											if(input$exportFormat == ".jpeg"){
												jpeg(filename = paste0("RealignData_",i ,".jpg"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".bpm"){
												bmp(filename = paste0("RealignData_",i  ,".bmp"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".png"){
												png(filename = paste0("RealignData_",i  ,".png"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
											if(input$exportFormat == ".tiff"){
												tiff(filename = paste0("RealignData_",i  ,".tiff"), width = input$exportwidth, height = input$exportheight)
											}  else {}
											
										}
										
										par(mfrow = c(2,3))
										
										for(y in (5*(i-1)+1):(5*i)){
											
											par(new = FALSE)
											
											if(y == (5*(i-1)+1)){
												
												if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
													
													plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
													
													legend(0,1, legend = input$ReplicateSample, col = vapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), 
																						   function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]},
																						   FUN.VALUE = character(1)
													), lty = 1, pch = 1, lwd = 1)
													
													if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]}))))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
														text(1,0.5, labels = "No data different from NA for this element", cex = 2)
													} else{
														ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
														
														xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))))
														
														lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
															
															plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] , col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
															
															par(new = TRUE)
															
														})
														
														
													}
													
												} else{
													
													plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
													
													legend(0,1, legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), 
																																								    function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																								    FUN.VALUE = character(1)
													), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
													
													if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "", main = input$RealignElementToExport[y])
														text(1,0.5, labels = "No data different from NA for this element", cex = 2)
													} else{
														
														ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
														
														xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
														
														lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
															
															plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
															
															par(new = TRUE)
															
														})
														
														plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
														
													}
													
												}
											} else{
												
												if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
													
													if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]}))))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
														text(1,0.5, labels = "No data different from NA for this element", cex = 2)
													} else{
														ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
														
														xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))))
														
														lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
															
															plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] , col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
															
															par(new = TRUE)
															
														})
														
														
													}
													
												} else{
													
													if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
														text(1,0.5, labels = "No data different from NA for this element", cex = 2)
													} else{
														
														ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
														
														xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
														
														lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
															
															plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
															
															par(new = TRUE)
															
														})
														
														plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
														
													}
													
												}
												
											}
										}
										
										title(input$selectRealign, outer=TRUE, cex = 1.5)
										
										dev.off()
										
									}
									
								}
								
								if(nRest != 0){
									
									if(is.null(input$exportFormat)){
										jpeg(filename = paste0("RealignData_",nbGraph+1,".jpg"), width = 760, height = 400)
									} else{
										if(input$exportFormat == ".jpeg"){
											jpeg(filename = paste0("RealignData_",nbGraph+1,".jpg"), width = input$exportwidth, height = input$exportheight)
										}  else {}
										
										if(input$exportFormat == ".bmp"){
											bmp(filename = paste0("RealignData_",nbGraph+1,".bmp"), width = input$exportwidth, height = input$exportheight)
										}  else {}
										
										if(input$exportFormat == ".png"){
											png(filename = paste0("RealignData_",nbGraph+1,".png"), width = input$exportwidth, height = input$exportheight)
										}  else {}
										
										if(input$exportFormat == ".tiff"){
											tiff(filename = paste0("RealignData_",nbGraph+1,".tiff"), width = input$exportwidth, height = input$exportheight)
										}  else {}
									}
									
									par(mfrow = c(2,3))
									
									for(y in (5*nbGraph+1): (5*nbGraph + nRest)){
										
										par(new = FALSE)
										
										if(y == 5*nbGraph+1){
											
											if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
												
												plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
												
												legend(0,1, legend = input$ReplicateSample, col = vapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), 
																					   function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]},
																					   FUN.VALUE = character(1)
												), lty = 1, pch = 1, lwd = 1)
												
												if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]}))))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
													text(0,1, labels = "No data different from NA for this element", cex = 2)
												} else{
													ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
													
													xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))))
													
													lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
														
														plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] , col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
														
														par(new = TRUE)
														
														
													})
												}
												
											} else {
												
												plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
												
												legend(0,1, legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), 
																																							    function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																							    FUN.VALUE = character(1)
												), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
												
												if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
													text(0,1, labels = "No data different from NA for this element", cex = 2)
												}else{
													
													ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
													
													xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
													
													lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
														
														plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
														
														par(new = TRUE)
														
													})
													
													plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
													
												}
												
												
											}
											
											
										} else{
											if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
												
												par(mar = c(5.1,4.1,1.5,1.5))
												
												if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]}))))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
													text(0,1, labels = "No data different from NA for this element", cex = 2)
												} else{
													ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
													
													xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){tabProvSample$temp[[x]][,1]}))))
													
													lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
														
														plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] , col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
														
														par(new = TRUE)
														
														
													})
												}
												
											}else{
												
												if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
													text(0,1, labels = "No data different from NA for this element", cex = 2)
												}else{
													
													ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
													
													xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
													
													lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
														
														plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
														
														par(new = TRUE)
														
													})
													
													plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
													
												}
												
											}
										}
										
										
									}
									
									title(input$selectRealign, outer=TRUE, cex = 1.5)
									
									dev.off()
								}
								
								info <- sprintf("%d%% done", round(100))
								setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
								
								close(pb)
								
								res <- tkmessageBox(title = "INFO !",message = "Graphics exported", icon = "info", type = "ok")
							}
						})
						
					}
				}
				else{}
			})
		}
		
		#################################
		#################################
		########### Creation Donnees ####
		#################################
		#################################
		{
			tempProj <- reactiveValues(temp = NULL) # temporary elementR project
			projPath <- reactiveValues(temp = NULL) # path of the project
			projChar <- reactiveValues(temp = NULL) # a vector to display project features
			
			rankStandard <- reactiveValues(temp = NULL) # to generate the order of standard file
			rankSample <- reactiveValues(temp = NULL) # to generate the rank of sample file
			geneRMachineCorr <- reactiveValues(temp = NULL) # a vector of string created by geneR for evaluate teh machine correction chosen by user
			
			generRRealign <- reactiveValues(temp = 0) # a vector of string created by geneR for the numericInput in the realign step
			
			flagStart <- reactiveValues(temp = c(0,0)) # Flag the first is for create a project (creation, loading): 0, 1 already created or loaded but not validated, 2 validated
			startSession <- reactiveValues(temp = 0) # flag for the validation of first step
			flagStandard <- reactiveValues(temp = NULL) # flag standards
			flagSampleDetail <- reactiveValues(temp = NULL) # flag for sample: a list of vector for each sample, flagSampleDetail = list(c(flag for spot mode, flag for transect mode), ....)
			flagSample <- reactiveValues(temp = NULL) # flag summarizing the filtration of sample (a vector with the length corresponding to the number of samples in the project)
			flagRealign <- reactiveValues(temp = NULL) # flag for realignment
			validCorrection <- reactiveValues(temp = NULL) #flag indicating the number of time user validate and de-validate the calibration step
			
			runEx <- reactiveValues(temp = 0) #flag to indicate that user run example
			
			flagSupp <- reactiveValues(temp = 0) #flag to indicate that user create another project
			
			standardFile <- reactiveValues(temp = NA) # a temporary vector containing the name of standards
			SampleFile <- reactiveValues(temp = NA)  # a temporary vector containing the name of samples
			
			color <- reactiveValues(temp = 0) # a vector containing the colors for the chemical elements
			colorReplicate <- reactiveValues(temp = 0) #color for each replicate for the realignment step
			
			ElemStand <- reactiveValues(temp = 0) #standard element chosen by user
			calibFile <- reactiveValues(temp = NA) # name of the calibration file
			
			waste <- reactiveValues(temp = NULL) # character strings not to pick up for the next project (in geneR: toAvoid argument)
			
			load <- reactiveValues(temp = 0)
			loadS <- reactiveValues(temp = 0)
			
			DirToCreate <- reactiveValues(temp = 0) # to tell if the original directory of the project loaded is still on the computer, 
			# 0: don't need to create a new folder, 1 the folder does not exist anymore new folder to create
			WhatLoaded <- reactiveValues(temp = NA) # tell if the loaded project is the example included in teh package or not.
			
			##############################################################
			# create a project with a upload directory 
			# e.g. set flags and elements color for the rest of the procedure
			##############################################################
			observe({
				if(!is.null(input$createProjButton)){
					if(input$createProjButton != 0){
						isolate({
							
							waste$temp <- unlist(c(waste$temp, 
										     valeurColor$temp, 
										     rankStandard$temp, 
										     rankSample$temp, 
										     geneRMachineCorr$temp, 
										     generRRealign$temp))
							
							sauvegarde <- getwd()
							
							if(Sys.info()[1] == "Windows"){
								
								projPath$temp <- choose.dir()
								
							} else  {
								
								projPath$temp <- tk_choose.dir()
								
							}
							
							runEx$temp <- 0
							calibFile$temp <- NA
							
							if(is.na(projPath$temp)){
								
								flagStart$temp[2] <- 0
								flagStart$temp[1] <- 0
								
							} else if(sum(c("standards","samples")%in%dir(projPath$temp))!=2){
								
								tkmessageBox(message = "A folder should contain two subfolder 'standards' & 'samples' to create an elementR project '[^_-]'", icon = "error", type = "ok")
								
							} else if(checkFormat(paste0(projPath$temp,"/samples")) == FALSE | checkFormat(paste0(projPath$temp,"/standards")) == FALSE){
								
								tkmessageBox(message = "Problem in file extension (format)", icon = "error", type = "ok")
								
							} else {
								
								flagStart$temp[1] <- 1
								flagStart$temp[2] <- 0
								
								updateCheckboxGroupInput(session,"checkbox", selected = FALSE)
								
								tempProj$temp <- elementR_project$new(projPath$temp, sep = valSep$temp, dec = valDec$temp)
								
								projChar$temp <- list(1, "Type of action: Project creation", 
											    projPath$temp, 
											    projPath$temp, 
											    dir(paste(projPath$temp,"/standards",sep="")), 
											    dir(paste(projPath$temp,"/samples",sep="")), 
											    tempProj$temp$listeElem)
								
								currentProject <- reactive({
									tempProj$temp
								})
								
								flagStandard$temp <- currentProject()$flag_stand
								flagSampleDetail$temp <- currentProject()$flag_Sample
								flagRealign$temp <- currentProject()$flagRealign
								validCorrection$temp <- currentProject()$flagMachineCorrection
								
								flagSample$temp <- vapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), 
												  function(x){
												  	if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
												  		return(1)
												  	}else{return(0)}
												  },
												  FUN.VALUE = numeric(1)
								)
								
								tempO <- list()
								
								for (i in seq(from = 1, to  = length(currentProject()$flag_Sample), by = 1)){
									tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
									names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
								}
								
								colorReplicate$temp <- tempO
								
								currentProject()$setRank(type = "sample", value = NA)
								currentProject()$setRank(type = "standard", value = NA)
							}
						})
					} else {}
				} else {}
			}) #observe
			
			##############################################################
			# create a project based on the example
			##############################################################
			observe({
				if(!is.null(input$runExampleNew)){
					if(input$runExampleNew != 0){
						isolate({
							
							waste$temp <- unlist(c(waste$temp, 
										     valeurColor$temp, 
										     rankStandard$temp, 
										     rankSample$temp, 
										     geneRMachineCorr$temp, 
										     generRRealign$temp))
							
							sauvegarde <- getwd()
							
							if(Sys.info()[1] == "Windows"){
								projPath$temp <- paste0(system.file("", package="elementR"), "/Example_Session")
							} else {
								projPath$temp <- paste0(system.file("", package="elementR"), "Example_Session")
							}
							
							runEx$temp <- 1
							calibFile$temp <- NA
							
							if(is.na(projPath$temp)){
								
								flagStart$temp[2] <- 0
								flagStart$temp[1] <- 0
								
							} else {
								
								if(sum(c("standards","samples")%in%dir(projPath$temp))!=2){
									
									tkmessageBox(message = "A folder should contain two subfolder 'standards' & 'samples' to create an elementR project '[^_-]'", icon = "error", type = "ok")
									
								} else {
									flagStart$temp[1] <- 1
									flagStart$temp[2] <- 0
									
									updateCheckboxGroupInput(session,"checkbox", selected = FALSE)
									
									tempProj$temp <- elementR_project$new(projPath$temp, sep = valSep$temp, dec = valDec$temp)
									
									projChar$temp <- list(1, "Type of action: Project creation", 
												    projPath$temp, 
												    projPath$temp, 
												    dir(paste(projPath$temp,"/standards",sep="")), 
												    dir(paste(projPath$temp,"/samples",sep="")), 
												    tempProj$temp$listeElem)
									
									currentProject <- reactive({
										tempProj$temp
									})
									
									flagStandard$temp <- currentProject()$flag_stand
									flagSampleDetail$temp <- currentProject()$flag_Sample
									flagRealign$temp <- currentProject()$flagRealign
									validCorrection$temp <- currentProject()$flagMachineCorrection
									
									flagSample$temp <- vapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), 
													  function(x){
													  	if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
													  		return(1)
													  	}else{return(0)}
													  }, 
													  FUN.VALUE = numeric(1)
									)
									
									tempO <- list()
									
									for (i in seq(from = 1, to = length(currentProject()$flag_Sample), by = 1)){
										tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
										names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
									}
									
									colorReplicate$temp <- tempO
									
									currentProject()$setRank(type = "sample", value = NA)
									currentProject()$setRank(type = "standard", value = NA)
								}
							}
						})
					} else {}
				} else {}
			}) #observe
			
			##############################################################
			# upload a project already proceed 
			# e.g. set flags and elements color for the rest of the procedure
			##############################################################
			observe({
				if(!is.null(input$loadProjButton)){
					if(input$loadProjButton !=0){
						isolate({
							
							if(Sys.info()[1] == "Windows"){
								
								tempoR1 <- choose.files(default = getwd())
								
							} else  {
								
								tempoR1 <- tk_choose.files(default = getwd(), 
												   caption = "Select files", 
												   multi = FALSE, 
												   filters = matrix(c("R data", ".RData"), 1,2), 
												   index = 1)
								
							}
							
							if(length(tempoR1) != 0){
								if(str_detect(tempoR1, ".RData")){
									flagStart$temp[2] <- 1
									flagStart$temp[1] <- 0
									
									load(tempoR1)
									
									tempProj$temp <- myProject
									
									projChar$temp <- list(2, "Type of action: Modification of an existing elementR project", 
												    tempoR1, 
												    tempProj$temp$name, 
												    tempProj$temp$folderPath, 
												    tempProj$temp$standardsFiles, 
												    tempProj$temp$samplesFiles, 
												    tempProj$temp$listeElem)
									
									currentProject <- reactive({
										tempProj$temp
									})
									
									DIRECTORY <- currentProject()$folderPath
									
									if(dir.exists(paste0(DIRECTORY,"/Results")) == TRUE){
										projPath$temp <- DIRECTORY
										DirToCreate$temp <- 0
										
									} else {
										DirToCreate$temp <- 1
										
										d <- paste0(str_split(tempoR1, pattern = ".RData")[[1]][1], "__", Sys.Date())
										
										projPath$temp <- d
										
										tkmessageBox(title = "INFO !",message = paste("The path of the project is not included in this machine. The project will be exported in:",d), icon = "info", type = "ok")
										
										suppressWarnings(dir.create(d))
										
										suppressWarnings(dir.create(paste0(d,"/Results")))
										suppressWarnings(dir.create(paste0(d,"/Results/standards")))
										suppressWarnings(dir.create(paste0(d,"/Results/samples")))
										
										lapply(seq(from = 1, to = length(currentProject()$standardsFiles), by = 1), function(y){
											temporaire <- currentProject()$standardsFiles[y]
											suppressWarnings(dir.create(paste0(d,"/Results/standards/", temporaire)))
										})
										
										lapply(seq(from = 1, to = length(currentProject()$samples), by = 1), function(y){
											suppressWarnings(dir.create(paste0(d,"/Results/samples/",currentProject()$samplesFiles[y])))
											lapply(seq(from = 1, to = length(currentProject()$samples[[y]]$rep_Files), by = 1), function(x){
												temporaire <-currentProject()$samples[[y]]$rep_Files[x]
												suppressWarnings(dir.create(paste0(d,"/Results/samples/", currentProject()$samplesFiles[y],"/",temporaire)))
											})
										})
										
									}
									
									tempO <- list()
									
									for (i in seq(from = 1, to = length(currentProject()$flag_Sample), by = 1)){
										tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
										names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
									}
									
									colorReplicate$temp <- tempO
									
									flagStandard$temp <- currentProject()$flag_stand
									flagSampleDetail$temp <- currentProject()$flag_Sample
									flagRealign$temp <- currentProject()$flagRealign
									validCorrection$temp <- currentProject()$flagMachineCorrection
								} else { tkmessageBox(message = "WARNING: you must upload an object saved on a .RData format", icon = "error", type = "ok")}
							} else {
								flagStart$temp[2] <- 0
								flagStart$temp[1] <- 0
							}
							
							WhatLoaded$temp <- "notExample"
						})
					} else {}
				}else {}
			}) #observe
			
			##############################################################
			# upload the example 
			##############################################################
			observe({
				if(!is.null(input$runExampleLoad)){
					if(input$runExampleLoad != 0){
						isolate({
							
							if(Sys.info()[1] == "Windows"){
								
								tempoR1 <- paste0(system.file("", package="elementR"), "/Results/Example_Session.RData")
								
							} else  {
								
								tempoR1 <- paste0(system.file("", package="elementR"), "Results/Example_Session.RData")
								
							}
							
							if(length(tempoR1) != 0 & file.exists(tempoR1)){
								
								flagStart$temp[2] <- 1
								flagStart$temp[1] <- 0
								
								load(tempoR1)
								
								tempProj$temp <- myProject
								
								projChar$temp <- list(2, "Type of action: Modification of an existing elementR project", tempoR1, tempProj$temp$name, tempProj$temp$folderPath, tempProj$temp$standardsFiles, tempProj$temp$samplesFiles, tempProj$temp$listeElem)
								
								currentProject <- reactive({
									tempProj$temp
								})
								
								if(Sys.info()[1] == "Windows"){
									
									DIRECTORY <- paste0(system.file("", package="elementR"), "/Example_Session")
									
								} else  {
									
									DIRECTORY <- paste0(system.file("", package="elementR"), "Example_Session")
									
								}
								
								DirToCreate$temp <- 1
								
								projPath$temp <- system.file("", package="elementR")
								
								d <- paste0(projPath$temp, "/Example_Session_", Sys.Date())
								
								projPath$temp <- d
								
								tkmessageBox(title = "INFO !",message = paste("The path of the project is not included in this machine. The project will be exported in:",d), icon = "info", type = "ok")
								
								suppressWarnings(dir.create(d))
								
								suppressWarnings(dir.create(paste0(d,"/Results")))
								suppressWarnings(dir.create(paste0(d,"/Results/standards")))
								suppressWarnings(dir.create(paste0(d,"/Results/samples")))
								
								lapply(seq(from = 1, to = length(currentProject()$standardsFiles), by = 1), function(y){
									temporaire <- currentProject()$standardsFiles[y]
									suppressWarnings(dir.create(paste0(d,"/Results/standards/", temporaire)))
								})
								
								lapply(seq(from = 1, to = length(currentProject()$samples), by = 1), function(y){
									suppressWarnings(dir.create(paste0(d,"/Results/samples/",currentProject()$samplesFiles[y])))
									lapply(seq(from = 1, to = length(currentProject()$samples[[y]]$rep_Files), by = 1), function(x){
										temporaire <-currentProject()$samples[[y]]$rep_Files[x]
										suppressWarnings(dir.create(paste0(d,"/Results/samples/", currentProject()$samplesFiles[y],"/",temporaire)))
									})
								})
								
								tempO <- list()
								
								for (i in seq(from = 1, to = length(currentProject()$flag_Sample), by = 1)){
									tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
									names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
								}
								
								colorReplicate$temp <- tempO
								
								flagStandard$temp <- currentProject()$flag_stand
								flagSampleDetail$temp <- currentProject()$flag_Sample
								flagRealign$temp <- currentProject()$flagRealign
								validCorrection$temp <- currentProject()$flagMachineCorrection
								
							} else {
								flagStart$temp[2] <- 0
								flagStart$temp[1] <- 0
								tkmessageBox(message = "It seems that you delete or deplace the example, please reinstall the elementR package", icon = "error", type = "ok")
							}
							
							WhatLoaded$temp <- "Example"
						})
					} else {}
				} else {}
			}) #observe
			
			##############################################################
			# finalize the creation of the project 
			# e.g.setting elements, calibration files, order
			##############################################################
			observe({
				if(!is.null(input$validDonne)){
					isolate({
						
						if(input$validDonne != 0){
							
							currentProject()$set_ChoiceUserCorr(input$checkbox)
							
							flagStart$temp[1] <- 3
							flagStart$temp[2] <- 0
							startSession$temp <- 1
							
							geneRMachineCorr$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$listeElem), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, generRRealign$temp))
							
							tempoR <- list()
							
							for(i in seq(from = 1, to = length(currentProject()$samplesFiles), by = 1)){
								
								tempoR[[i]] <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$samples[[i]]$rep_Files), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, geneRMachineCorr$temp, unlist(tempoR)))
								
								names(tempoR[[i]]) <- currentProject()$samples[[i]]$rep_Files
							}
							
							generRRealign$temp <- tempoR
							
							colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
							
							color$temp <- colfunc(length(currentProject()$listeElem))
							names(color$temp) <- currentProject()$listeElem
							
							espace1 <- getwd()
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results")))
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results/standards")))
							suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples")))
							
							lapply(seq(from = 1, to = length(currentProject()$standardsFiles), by = 1), function(y){
								temporaire <- currentProject()$standardsFiles[y]
								suppressWarnings(dir.create(paste0(projPath$temp,"/Results/standards/", temporaire)))
							})
							
							lapply(seq(from = 1, to = length(currentProject()$samples), by = 1), function(y){
								suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/",currentProject()$samplesFiles[y])))
								lapply(seq(from = 1, to = length(currentProject()$samples[[y]]$rep_Files), by = 1), function(x){
									temporaire <-currentProject()$samples[[y]]$rep_Files[x]
									suppressWarnings(dir.create(paste0(projPath$temp,"/Results/samples/", currentProject()$samplesFiles[y],"/",temporaire)))
								})
							})
							
							setwd(espace1)
							
							currentProject()$setElemStand(elem = input$internStand)
							
							
						} else {}
						
					})
				} else {}
			}) #observe
			
			##############################################################
			# finalize the loading of the project 
			##############################################################
			observe({
				if(!is.null(input$validDonne2)){
					
					isolate({
						
						if(input$validDonne2 != 0){
							
							flagStart$temp[2] <- 3
							flagStart$temp[1] <- 0
							startSession$temp <- 1
							
							geneRMachineCorr$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$listeElem), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, generRRealign$temp))
							
							tempoR <- list()
							
							for(i in seq(from = 1, to = length(currentProject()$samplesFiles), by = 1)){
								
								tempoR[[i]] <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$samples[[i]]$rep_Files), toAvoid = c(waste$temp, valeurColor$temp, geneRMachineCorr$temp, rankStandard$temp, rankSample$temp, unlist(tempoR)))
								
								names(tempoR[[i]]) <- currentProject()$samples[[i]]$rep_Files
							}
							
							generRRealign$temp <- tempoR
							
							colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
							
							color$temp <- colfunc(length(currentProject()$listeElem))
							names(color$temp) <- currentProject()$listeElem
							
							if(DirToCreate$temp == 1){
								
							} else {}
							
						} else {}
						
					})
				} else {}
			}) #observe
			
			##############################################################
			# to begin another project
			# Reinitialize flags...
			##############################################################
			observe({
				if(!is.null(input$SuppDonne)){
					if(input$SuppDonne != 0){
						
						isolate({
							flagSupp$temp <- 1
							
							waste$temp <- unlist(c(waste$temp, 
										     valeurColor$temp, 
										     rankStandard$temp, 
										     rankSample$temp, 
										     geneRMachineCorr$temp, 
										     generRRealign$temp))
							
							flagStart$temp[c(1,2)] <- 0
							
							currentProject()$setflagMachineCorrection(x = 0)
							currentProject()$setflagStand (place = seq(from = 1, to = length(currentProject()$standardsFiles), by = 1), value = 0)
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate= x, type = "transect",value = 0)})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
								lapply(seq(from = 1, to = length(currentProject()$flag_Sample[[x]]), by = 1), function(i){
									currentProject()$setflagSample(sample = x, replicate = i, value = 0)
								})
							})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
								currentProject()$samples[[x]]$setrep_type2(NA)
							})
							startSession$temp <- 0
							flagStandard$temp <- NULL
							flagSampleDetail$temp <- NULL
							flagRealign$temp <- NULL
							validCorrection$temp <- NULL
							calibFile$temp <- NA
							projPath$temp <- NA
							runEx$temp <- 0
							DirToCreate$temp <- 0
							color$temp <- NULL
							WhatLoaded$temp <- NA
							valeurColor$temp <- NULL
							Temp$t <- NULL
							Temp0$t <- NULL
							Temp1$t <- NULL
							Temp2$t <- NULL
							
							TempS$t <- NULL
							Temp0S$t <- NULL
							Temp1S$t <- NULL
							Temp2S$t <- NULL
							
							zero$temp <- 0
							one$temp <- 0
							two$temp <- 0
							three$temp <- 0
							
							updateCheckboxGroupInput(session,"checkbox", selected = FALSE)
						})
						
					} else {}
				} else {}
			}) #observe
			
			##############################################################
			# choose and set the calibration file for the rest of the procedure
			##############################################################
			observe({
				if(!is.null(input$calibFile)){
					if(input$calibFile > 0){
						
						isolate({
							if(runEx$temp == 0){
								Filters <- matrix(c("Text", ".csv", "OO sheet", ".ods", "Excel sheet", ".xls", "Excel sheet", ".xlsx"), 4, 2, byrow = TRUE)
								
								if(Sys.info()[1] == "Windows"){
									
									temp <- choose.files(default = paste0(projChar$temp[[3]], "/calibration file"))
									
								} else  {
									
									temp <- tk_choose.files(default = paste0(projChar$temp[[3]], "/calibration file"), caption = "Select files",multi = FALSE, filters = Filters, index = 1)
									
								}
								
								if(length(temp) != 0){
									if(str_detect(temp, ".csv") | str_detect(temp, ".xls") | str_detect(temp, ".ods")){
										currentProject()$setEtalon(x = temp, sep = valSep$temp, dec = valDec$temp)
									} else { tkmessageBox(message = "WARNING: you must upload an file saved on a .csv, .ods, .xls or .xlsx format", icon = "error", type = "ok")}
								} else {
									
								}
								
								
							} else {
								
								if(Sys.info()[1] == "Windows"){
									
									temp <- paste0(system.file("", package="elementR"), "/Example_Session/Calibration_File_NIST612.csv")
									
								} else  {
									
									temp <- paste0(system.file("", package="elementR"), "Example_Session/Calibration_File_NIST612.csv")
									
								}
								
								
								
								currentProject()$setEtalon(x = temp, sep = valSep$temp, dec = valDec$temp)
								
							}
						})
						
						
						
						
						
					} else {}
				} else {}
			}) #observe
			
			##############################################################
			# diplay the name of the calibration file
			##############################################################
			observe({
				input$calibFile
				output$InformCalib <- renderUI({
					if(!is.null(currentProject())){
						if(is.na(currentProject()$EtalonPath)){
							NULL
						} else {
							calibFile$temp <- unlist(str_split(currentProject()$EtalonPath, "/"))[length(unlist(str_split(currentProject()$EtalonPath, "/")))]
							return(p(paste0("Calibration file loaded: ", calibFile$temp)))
						}
					}
				})
			}) #observe
			
			##############################################################
			# set the order of standards and samples
			##############################################################
			observe({
				input$calibFile
				calibFile$temp
				input$checkbox
				if(!is.null(rankStandard$temp)){
					if(!is.null(rankSample$temp)){
						if(!is.na(rankStandard$temp[length(standardFile$temp)]) & !is.na(rankSample$temp[length(SampleFile$temp)])){
							if(!is.null(eval(parse(text = paste0("input$",rankStandard$temp[1]))))){
								
								tempCalib <- vapply(seq(from = 1, to = length(standardFile$temp), by = 1), 
											  
											  function(x){
											  	
											  	eval(parse(text = paste0("input$",isolate({rankStandard$temp})[x])))
											  	
											  }, 
											  FUN.VALUE = integer(1)
								)
								
								names(tempCalib) <- standardFile$temp
								
								tempSample <- vapply(seq(from = 1, to = length(SampleFile$temp), by = 1), 
											   
											   function(x){
											   	
											   	eval(parse(text = paste0("input$",rankSample$temp[x])))
											   	
											   },
											   FUN.VALUE = integer(1)
								)
								names(tempSample) <- SampleFile$temp
								
								isolate({
									
									if(input$checkbox == TRUE & flagStart$temp[1] > 0 & !is.na(input$calibFile)) {
										
										if(length(which(is.na(tempCalib))) == 0 & length(which(is.na(tempSample))) == 0 & length(which(tempCalib == 0)) == 0 & length(which(tempSample == 0)) == 0 & length(which(duplicated(c(tempSample,tempCalib))) == TRUE) == 0 & !is.na(calibFile$temp)){
											flagStart$temp[1] <- 2.5
											currentProject()$setRank(type = "standard", value = tempCalib)
											currentProject()$setRank(type = "sample",  value = tempSample)
										}else{
											flagStart$temp[1] <- 1.5
											currentProject()$setRank(type= "standard", value = tempCalib)
											currentProject()$setRank(type = "sample", value = tempSample)
										}
										
									} else {
										
									}
									
								})
							} else {}
						} else {}
					} else {}	
				} else {}
			}) #observe
			
			##############################################################
			# choice of user for correcting the machine drift
			##############################################################
			observe({
				input$runExampleNew
				if(!is.null(input$checkbox)){
					if(!is.null(input$calibFile)){
						calibFile$temp
						isolate({
							if(flagStart$temp[1] > 0) {
								if(input$checkbox == TRUE){
									flagStart$temp[1] <- 1.5
								} else{
									if(!is.na(currentProject()$EtalonPath)){
										
										flagStart$temp[1] <- 2
									} else {
										flagStart$temp[1] <- 1
									}
								}
							} else {}
							
						})
					} else {}
				} else {}
			})
			
			##############################################################
			# Gives random orders for samples and standards when correction == FALSE
			##############################################################
			observe({
				if(!is.null(input$validDonne)){
					isolate({
						
						if(input$validDonne != 0){
							
							if(input$checkbox == FALSE & flagStart$temp[1] > 0 & !is.na(currentProject()$EtalonPath)) {
								
								NB <- length(standardFile$temp)+length(SampleFile$temp)
								
								tempCalib <- c((seq(from = 1, to = NB, by = 1))[1],sort(sample((seq(from = 1, to = NB, by = 1))[-c(1, NB)], (length(standardFile$temp)-2), replace = FALSE)),(seq(from = 1, to = NB, by = 1))[NB])
								names(tempCalib) <- standardFile$temp
								
								tempSample <- (seq(from = 1, to = NB, by = 1))[-tempCalib]
								names(tempSample) <- SampleFile$temp
								
								currentProject()$setRank(type= "standard", value = tempCalib)
								currentProject()$setRank(type = "sample", value = tempSample)
								
							}
						}
					})
				} else {}
			})
			
			##############################################################
			#geneR rankStandard$temp & rankSample$temp
			##############################################################
			observe({
				input$createProjButton
				input$runExampleNew
				
				if(flagStart$temp[1] == 1){
					
					isolate({
						standardFile$temp <- dir(paste0(projPath$temp, "/standards"))
						temp <- dir(paste0(projPath$temp, "/samples"), recursive = TRUE)
						SampleFile$temp <- vapply(seq(from = 1, to = length(temp), by = 1), 
										  function(x){str_split(temp, "/")[[x]][2]},
										  FUN.VALUE = character(1)
						)
						
						rankStandard$temp <- geneR(choice = letters, 
										   lengthComb = 5, 
										   NBComb = length(standardFile$temp), 
										   toAvoid = c(waste$temp, valeurColor$temp, rankSample$temp, geneRMachineCorr$temp, generRRealign$temp))
						rankSample$temp <- geneR(choice = letters, 
										 lengthComb = 5, 
										 NBComb = length(SampleFile$temp), 
										 toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, geneRMachineCorr$temp, generRRealign$temp))
						
					})
					
				} else {}
			}) #observe
			
			##############################################################
			# set the ElemStand$temp, i.e. the internal standard element
			##############################################################
			observe({
				if(!is.null(input$internStand)){
					ElemStand$temp <- input$internStand
				}else{
					if(is.null(currentProject())){
						
					}else{
						ElemStand$temp <- currentProject()$elemStand
					}
				}
			}) #observe
			
			##############################################################
			# Go to the "config" tab when user click on the actionButton with id = SetParam
			##############################################################
			observe({
				if(!is.null(input$SetParam)){
					if(input$SetParam > 0){
						updateTabItems(session, "tab", selected = "Config")
					} else {}
				} else {}
			})
			
			##############################################################
			# all displayed elements to users according to flagStart$temp
			# the flag to indicate the improvement of the user
			# output$start1: top div
			# output$start2: bottom boxes
			##############################################################
			observe({
				if(flagStart$temp[1] == 0 & flagStart$temp[2] == 0){
					
					output$start1 <-  renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					output$start2 <- renderUI({
						
						div(
							box(
								title = list(icon("folder-o"),"New Project"),
								width = 6,
								status="primary",
								solidHeader = TRUE,
								p("1. Choose the project folder"),
								actionButton("createProjButton", "Create your project !"),
								actionButton("runExampleNew", "Run Example")
							),
							box(
								title = list(icon("folder"),"Load Project"),
								width = 6,
								solidHeader = TRUE,
								status="primary",
								p("1. Choose a project to load"),
								actionButton("loadProjButton","Load your Project"),
								actionButton("runExampleLoad", "Load Example")
								
							)
							
						)
						
						
						
					})
				} else {}
				
				if(flagStart$temp[1] == 1){
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					if(currentProject()$elementChecking[[1]] == 0 & is.null(currentProject()$errorSession)){
						
						if(runEx$temp == 0){
							
							if(is.na(currentProject()$standardRank[1])){
								elem <- colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1]
								if(length(which(str_detect(elem, "Ca") == TRUE)) != 0){ElemStand$temp <- elem[(which(str_detect(elem, "Ca") == TRUE))[1]]} else {}
								if(length(which(str_detect(elem, "Ca") == TRUE)) == 0){ElemStand$temp <- elem[1]}else {}
								
								output$start2 <- renderUI({
									div(
										box(
											title = list(icon("folder-o"),"New Project"),
											width = 6,
											status="primary",
											solidHeader = TRUE,
											p("1. Choose the project folder"),
											actionButton("createProjButton", "Create your project !"),
											actionButton("runExampleNew", "Run Example"),
											br(),
											br(),
											p("2. Choose the internal standard element"),
											selectInput("internStand", label = "",
													choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
													selected = "Ca43"),
											p("3. Choose the calibration file"),
											div(
												div(actionButton("calibFile", "Search"), style = "display:inline-block"),
												div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
											),
											br(),
											div(style = "display:flex",
											    p("4. Do you want to check the machine drift ?"),
											    div(checkboxInput("checkbox", label = "Yes", value = FALSE), style="margin-left: 10px")
											),
											p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis")
										),
										box(
											title = list(icon("folder"),"Load Project"),
											width = 6,
											solidHeader = TRUE,
											status="primary",
											p("1. Choose a project to load"),
											actionButton("loadProjButton","Load your Project"),
											actionButton("runExampleLoad", "Load Example")
										)
									)
									
								})
								
								
							} else {
								
								output$start2 <- renderUI({
									div(
										box(
											title = list(icon("folder-o"),"New Project"),
											width = 6,
											status="primary",
											solidHeader = TRUE,
											p("1. Choose the project folder"),
											actionButton("createProjButton", "Create your project !"),
											actionButton("runExampleNew", "Run Example"),
											br(),
											br(),
											p("2. Choose the internal standard element"),
											selectInput("internStand", label = "",
													choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
													selected = "Ca43"),
											p("3. Choose the calibration file"),
											div(
												div(actionButton("calibFile", "Search"), style = "display:inline-block"),
												div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
											),
											br(),
											div(style = "display:flex",
											    p("4. Do you want to check the machine drift ?"),
											    div(checkboxInput("checkbox", label = "Yes", value = FALSE), style="margin-left: 10px")
											),
											p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis")
										),
										box(
											title = list(icon("folder"),"Load Project"),
											width = 6,
											solidHeader = TRUE,
											status="primary",
											p("1. Choose a project to load"),
											actionButton("loadProjButton","Load your Project"),
											actionButton("runExampleLoad", "Load Example")
										)
									)
									
								})
							}
						} else {
							
							output$start2 <- renderUI({
								div(
									box(
										title = list(icon("folder-o"),"New Project"),
										width = 6,
										status="primary",
										solidHeader = TRUE,
										p("1. Choose the project folder"),
										actionButton("createProjButton", "Create your project !"),
										actionButton("runExampleNew", "Run Example"),
										br(),
										br(),
										p("2. Choose the internal standard element"),
										selectInput("internStand", label = "",
												choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
												selected = "Ca43"),
										p("3. Choose the calibration file"),
										div(
											div(actionButton("calibFile", "Search"), style = "display:inline-block"),
											div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
										),
										br(),
										div(style = "display:flex",
										    p("4. Do you want to check the machine drift ?"),
										    div(checkboxInput("checkbox", label = "Yes", value = FALSE), style="margin-left: 10px")
										),
										p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis")
									),
									box(
										title = list(icon("folder"),"Load Project"),
										width = 6,
										solidHeader = TRUE,
										status="primary",
										p("1. Choose a project to load"),
										actionButton("loadProjButton","Load your Project"),
										actionButton("runExampleLoad", "Load Example")
									)
								)
								
							})
							
							
						}
						
					} else {}
					if(currentProject()$elementChecking[[1]] != 0 & is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									br(),
									p("2. Checking elements"),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[[2]],sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
						})
						
					} else {}
					if(currentProject()$elementChecking[[1]] != 0 & !is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									p("2. Checking elements"),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[[2]],sep="", collapse =" "))),
									br(),
									p("3. Verification of the non-numeric character of the data"),
									br(),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$errorSession,sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
						})
						
					} else {}
					if(currentProject()$elementChecking[[1]] == 0 & !is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									p("2. Verification of the non-numeric character of the data"),
									br(),
									p(icon("times"), paste0("Problem in ", paste0(currentProject()$errorSession, sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
							
						})
						
					}  else {}
					
					
				} else {}
				
				if(flagStart$temp[1] == 1.5){
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					if(currentProject()$elementChecking[[1]] == 0 & is.null(currentProject()$errorSession)){
						
						if(runEx$temp == 0){
							
							if(length(currentProject()$standardRank) == 1){
								placeNIST <- rep(0,length(standardFile$temp))
								placeSAMPLE <- rep(0,length(SampleFile$temp))
							} else {
								placeNIST <- currentProject()$standardRank
								placeSAMPLE <- currentProject()$sampleRank
							}
							
							if(is.na(currentProject()$standardRank[1])){
								
								elem <- colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1]
								if(length(which(str_detect(elem, "Ca") == TRUE)) != 0){ElemStand$temp <- elem[(which(str_detect(elem, "Ca") == TRUE))[1]]} else {}
								if(length(which(str_detect(elem, "Ca") == TRUE)) == 0){ElemStand$temp <- elem[1]}else {}
								
								output$start2 <- renderUI({
									div(
										box(
											title = list(icon("folder-o"),"New Project"),
											width = 6,
											status="primary",
											solidHeader = TRUE,
											p("1. Choose the project folder"),
											actionButton("createProjButton", "Create your project !"),
											actionButton("runExampleNew", "Run Example"),
											br(),
											br(),
											p("2. Choose the internal standard element"),
											selectInput("internStand", label = "",
													choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
													selected = "Ca43"),
											p("3. Choose the calibration file"),
											div(
												div(actionButton("calibFile", "Search"), style = "display:inline-block"),
												div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
											),
											br(),
											div(style = "display:flex",
											    p("4. Do you want to check the machine drift ?"),
											    div(checkboxInput("checkbox", label = "Yes", value = TRUE), style="margin-left: 10px")
											),
											p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis"),
											br(),
											p("Sort the files in their analyzed order or indicate the time elapse since the beginning of the analysis (min)"),
											column(6,
												 p("Standard files:"),
												 lapply(seq(from = 1, to = length(standardFile$temp), by = 1), function(i){
												 	div(style="height: 50px;",
												 	    column(8,br(),p(standardFile$temp[i])), #column
												 	    column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", placeNIST[i], min = 0))) #column
												 	)
												 })
											),
											column(6,
												 p("Samples files:"),
												 lapply(seq(from = 1, to = length(SampleFile$temp), by = 1), function(i){
												 	div(style="height: 50px;",
												 	    column(8,br(),p(SampleFile$temp[i])), #column
												 	    column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "",placeSAMPLE[i], min = 0))) #column
												 	)
												 })
											)
										),
										box(
											title = list(icon("folder"),"Load Project"),
											width = 6,
											solidHeader = TRUE,
											status="primary",
											p("1. Choose a project to load"),
											actionButton("loadProjButton","Load your Project"),
											actionButton("runExampleLoad", "Load Example")
										)
									)
									
								})
								
								
							} else {
								
								output$start2 <- renderUI({
									div(
										box(
											title = list(icon("folder-o"),"New Project"),
											width = 6,
											status="primary",
											solidHeader = TRUE,
											p("1. Choose the project folder"),
											actionButton("createProjButton", "Create your project !"),
											actionButton("runExampleNew", "Run Example"),
											br(),
											br(),
											p("2. Choose the internal standard element"),
											selectInput("internStand", label = "",
													choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
													selected = "Ca43"),
											p("3. Choose the calibration file"),
											div(
												div(actionButton("calibFile", "Search"), style = "display:inline-block"),
												div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
											),
											br(),
											div(style = "display:flex",
											    p("4. Do you want to check the machine drift ?"),
											    div(checkboxInput("checkbox", label = "Yes", value = TRUE), style="margin-left: 10px")
											),
											p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis"),
											br(),
											p("Sort the files in their analyzed order or indicate the time elapse since the beginning of the analysis (min)"),
											column(6,
												 p("Standard files:"),
												 lapply(seq(from = 1, to = length(standardFile$temp), by = 1), function(i){
												 	div(style="height: 50px;",
												 	    column(8,br(),p(standardFile$temp[i])), #column
												 	    column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", placeNIST[i], min = 0))) #column
												 	)
												 })
											),
											column(6,
												 p("Samples files:"),
												 lapply(seq(from = 1, to = length(SampleFile$temp), by = 1), function(i){
												 	div(style="height: 50px;",
												 	    column(8,br(),p(SampleFile$temp[i])), #column
												 	    column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "",placeSAMPLE[i], min = 0))) #column
												 	)
												 })
											)
										),
										box(
											title = list(icon("folder"),"Load Project"),
											width = 6,
											solidHeader = TRUE,
											status="primary",
											p("1. Choose a project to load"),
											actionButton("loadProjButton","Load your Project"),
											actionButton("runExampleLoad", "Load Example")
										)
									)
									
								})
							}
						} else {
							if(length(currentProject()$standardRank) == 1){
								placeNIST <- c(1,5,9)
								placeSAMPLE <- c(2,3,4,6,7,8,10,11,12)
							} else {
								placeNIST <- currentProject()$standardRank
								placeSAMPLE <- currentProject()$sampleRank
							}
							
							output$start2 <- renderUI({
								div(
									box(
										title = list(icon("folder-o"),"New Project"),
										width = 6,
										status="primary",
										solidHeader = TRUE,
										p("1. Choose the project folder"),
										actionButton("createProjButton", "Create your project !"),
										actionButton("runExampleNew", "Run Example"),
										br(),
										br(),
										p("2. Choose the internal standard element"),
										selectInput("internStand", label = "",
												choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
												selected = "Ca43"),
										p("3. Choose the calibration file"),
										div(
											div(actionButton("calibFile", "Search"), style = "display:inline-block"),
											div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
										),
										br(),
										div(style = "display:flex",
										    p("4. Do you want to check the machine drift ?"),
										    div(checkboxInput("checkbox", label = "Yes", value = TRUE), style="margin-left: 10px")
										),
										p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis"),
										br(),
										p("Sort the files in their analyzed order or indicate the time elapse since the beginning of the analysis (min)"),
										column(6,
											 p("Standard files:"),
											 lapply(seq(from = 1, to = length(standardFile$temp), by = 1), function(i){
											 	div(style="height: 50px;",
											 	    column(8,br(),p(standardFile$temp[i])), #column
											 	    column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", placeNIST[i], min = 0))) #column
											 	)
											 })
										),
										column(6,
											 p("Samples files:"),
											 lapply(seq(from = 1, to = length(SampleFile$temp), by = 1), function(i){
											 	div(style="height: 50px;",
											 	    column(8,br(),p(SampleFile$temp[i])), #column
											 	    column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "",placeSAMPLE[i], min = 0))) #column
											 	)
											 })
										)
									),
									box(
										title = list(icon("folder"),"Load Project"),
										width = 6,
										solidHeader = TRUE,
										status="primary",
										p("1. Choose a project to load"),
										actionButton("loadProjButton","Load your Project"),
										actionButton("runExampleLoad", "Load Example")
									)
								)
								
							})
							
							
						}
						
					} else {}
					if(currentProject()$elementChecking[[1]] != 0 & is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									br(),
									p("2. Checking elements"),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[[2]],sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
						})
						
					} else {}
					if(currentProject()$elementChecking[[1]] != 0 & !is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									p("2. Checking elements"),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[[2]],sep="", collapse =" "))),
									br(),
									p("3. Verification of the non-numeric character of the data"),
									br(),
									p(icon("times"), paste0("Problem in ", paste(currentProject()$errorSession,sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
						})
						
					} else {}
					if(currentProject()$elementChecking[[1]] == 0 & !is.null(currentProject()$errorSession)){
						
						output$start2 <- renderUI({
							div(
								box(
									title = list(icon("folder-o"),"New Project"),
									width = 6,
									status="primary",
									solidHeader = TRUE,
									p("1. Choose the project folder"),
									actionButton("createProjButton", "Create your project !"),
									actionButton("runExampleNew", "Run Example"),
									br(),
									p("2. Verification of the non-numeric character of the data"),
									br(),
									p(icon("times"), paste0("Problem in ", paste0(currentProject()$errorSession, sep="", collapse =" ")))
								),
								box(
									title = list(icon("folder"),"Load Project"),
									width = 6,
									solidHeader = TRUE,
									status="primary",
									p("1. Choose a project to load"),
									actionButton("loadProjButton","Load your Project"),
									actionButton("runExampleLoad", "Load Example")
								)
							)
							
						})
						
					}  else {}
					
					
				} else {}
				
				if(flagStart$temp[1] == 2.5){
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					output$start2 <- renderUI({
						div(
							
							box(
								title = list(icon("folder-o"),"New Project"),
								width = 6,
								status="primary",
								solidHeader = TRUE,
								p("1. Choose the project folder"),
								actionButton("createProjButton", "Create your project !"),
								actionButton("runExampleNew", "Run Example"),
								br(),
								br(),
								p("2. Choose the internal standard element"),
								selectInput("internStand", label = "",
										choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
										selected = ElemStand$temp),
								p("3. Choose the calibration file"),
								div(
									div(actionButton("calibFile", "Search"), style = "display:inline-block"),
									div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
								),
								br(),
								div(style = "display:flex",
								    p("4. Do you want to check the machine drift ?"),
								    div(checkboxInput("checkbox", label = "Yes", value = TRUE), style="margin-left: 10px")
								),
								p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis"),
								br(),
								p("Sort the files in their analyzed order or indicate the time elapse since the beginning of the analysis (min)"),
								column(6,
									 p("Standard files:"),
									 lapply(seq(from = 1, to = length(standardFile$temp), by = 1), function(i){
									 	div(style="height: 50px;",
									 	    column(8,br(),p(standardFile$temp[i])), #column
									 	    column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", currentProject()$standardRank[i], min = 0))) #column
									 	)
									 })
								),
								column(6,
									 p("Samples files:"),
									 lapply(seq(from = 1, to = length(SampleFile$temp), by = 1), function(i){
									 	div(style="height: 50px;",
									 	    column(8,br(),p(SampleFile$temp[i])), #column
									 	    column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "", currentProject()$sampleRank[i], min = 0))) #column
									 	)
									 })
								),
								br(),
								br(),
								p("5. Validate  the created project"),
								column(3, offset = 4, actionButton("validDonne","Go Reducing !"))
								
								
							),
							box(
								title = list(icon("folder"),"Load Project"),
								width = 6,
								solidHeader = TRUE,
								status="primary",
								p("1. Choose a project to load"),
								actionButton("loadProjButton","Load your Project"),
								actionButton("runExampleLoad", "Load Example")
							)
						)
						
					})
					
					
				} else {}
				
				if(flagStart$temp[1] == 2){
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					output$start2 <- renderUI({
						div(
							
							box(
								title = list(icon("folder-o"),"New Project"),
								width = 6,
								status="primary",
								solidHeader = TRUE,
								p("1. Choose the project folder"),
								actionButton("createProjButton", "Create your project !"),
								actionButton("runExampleNew", "Run Example"),
								br(),
								br(),
								p("2. Choose the internal standard element"),
								selectInput("internStand", label = "",
										choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/"), sep = valSep$temp, dec = valDec$temp))[-1],
										selected = ElemStand$temp),
								p("3. Choose the calibration file"),
								div(
									div(actionButton("calibFile", "Search"), style = "display:inline-block"),
									div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
								),
								br(),
								div(style = "display:flex",
								    p("4. Do you want to check the machine drift ?"),
								    div(checkboxInput("checkbox", label = "Yes", value = FALSE), style="margin-left: 10px")
								),
								p("WARNING: This step requires the order of each standard and sample in the ICPMS analysis"),
								br(),
								br(),
								p("5. Validate  the created project"),
								column(3, offset = 4, actionButton("validDonne","Go Reducing !"))
								
								
							),
							box(
								title = list(icon("folder"),"Load Project"),
								width = 6,
								solidHeader = TRUE,
								status="primary",
								p("1. Choose a project to load"),
								actionButton("loadProjButton","Load your Project"),
								actionButton("runExampleLoad", "Load Example")
							)
						)
						
					})
					
					
				} else {}
				
				if(flagStart$temp[1] == 3){
					
					## explanation for standards
					
					x <- paste0(lapply(seq(from = 1, to = length(projChar$temp[[5]]), by = 1), function(i){paste0(projChar$temp[[5]][i], "(",currentProject()$standardRank[i], ")", collapse = " ")}))
					
					temp <- paste(x, collapse = " ", sep = " ")
					
					## explanation for samples
					
					x2 <- currentProject()$samplesFiles
					
					temp2 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						
						lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_Files), by = 1), function(i){
							
							temp5 <- currentProject()$sampleRank[which(names(currentProject()$sampleRank) == currentProject()$samples[[x]]$rep_Files[i])]
							
							paste0(currentProject()$samples[[x]]$rep_Files[i], "(", temp5, ")", collapse = " ")
							
						})
						
					})
					
					temp3 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						paste(unlist(temp2[[x]]), sep = " ", collapse = " ")
					})
					
					temp4 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						paste0(x2[x], ": ", temp3[[x]])
					})
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								),
								column(3,
									 br(),
									 actionButton("SuppDonne","Start another project")
								)
							)
						)
					})
					
					output$start2 <- renderUI({
						fluidRow(style="margin-left:0px;margin-right:0px",
							   box(
							   	title = list(icon("folder-o"),"Your project:"),
							   	width = 12,
							   	status="primary",
							   	solidHeader = TRUE,
							   	p(icon("share "), "You are creating a new project"),
							   	p(icon("share "), paste0("Path of the session to be reduced: ", projChar$temp[[4]])),
							   	p(icon("share "), paste0("Standard files (and their order) within the project: ", temp)),
							   	p(icon("share"), "Sample files (and their order) within the project: "),
							   	lapply(seq(from = 1, to = length(x2), by = 1), function(i){
							   		column(12, offset = 1, p(temp4[[i]]))
							   	}),
							   	p(icon("share"), paste0("Calibration file: ", calibFile$temp, collapse = " ")),
							   	p(icon("share"), paste0("Internal standard element: ", currentProject()$elemStand)),
							   	p(icon("share"), paste0("Detection of a possible machine drift: ", currentProject()$ChoiceUserCorr))
							   ),
							   box(
							   	title = list(icon("folder-o"),"Important information"),
							   	width = 12,
							   	status="primary",
							   	solidHeader = TRUE,
							   	height=100,
							   	div(
							   		div(p("To set optional parameters (also in the 'Configuration' tab)"), style = "display: inline-block"),
							   		div(actionButton("SetParam", "Configure Session"), style = "display: inline-block; margin-left:20px")
							   	)
							   )
							   
						)
						
						
					})
					
				} else {}
				
				if(flagStart$temp[2] == 1){
					
					if(!is.na(WhatLoaded$temp)){
						if(WhatLoaded$temp == "notExample"){
							replace <- str_split(currentProject()$folderPath, pattern = "/")[[1]][length(str_split(currentProject()$folderPath, pattern = "/")[[1]])]
						} else {
							replace <- "Example_Session"
						}
					} else {}
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								)
							)
						)
					})
					
					output$start2 <- renderUI({
						
						div(
							box(
								title = list(icon("folder-o"),"New Project"),
								width = 6,
								status="primary",
								solidHeader = TRUE,
								p("1. Choose the project folder"),
								actionButton("createProjButton", "Create your project !"),
								actionButton("runExampleNew", "Run Example")
							),
							box(
								title = list(icon("folder"),"Load Project"),
								width = 6,
								solidHeader = TRUE,
								status="primary",
								p("1. Choose a project to load"),
								actionButton("loadProjButton","Load your Project"),
								actionButton("runExampleLoad", "Load Example"),
								br(),
								br(),
								p(paste0("Project loaded: ", replace)),
								br(),
								column(3, offset = 4, actionButton("validDonne2","Go reducing !"))
							)
							
						)
						
						
						
					})
				} else {}
				
				if(flagStart$temp[2] == 3){
					
					output$start1 <- renderUI({
						fluidRow(
							box(
								background = "light-blue",
								height = 85,
								width = 12,
								column(9,
									 div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
								),
								column(3,
									 br(),
									 actionButton("SuppDonne","Start another project")
								)
							)
						)
					})
					
					## explanation for standards
					
					x <- paste0(lapply(seq(from = 1, to = length(currentProject()$standardsFiles), by = 1), function(i){paste0(currentProject()$standardsFiles[i], "(",currentProject()$standardRank[i], ")", collapse = " ")}))
					
					temp <- paste(x, collapse = " ", sep = " ")
					
					## explanation for samples
					
					x2 <- currentProject()$samplesFiles
					
					temp2 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						
						lapply(seq(from = 1, to = length(currentProject()$samples[[x]]$rep_Files), by = 1), function(i){
							
							temp5 <- currentProject()$sampleRank[which(names(currentProject()$sampleRank) == currentProject()$samples[[x]]$rep_Files[i])]
							
							paste0(currentProject()$samples[[x]]$rep_Files[i], "(", temp5, ")", collapse = " ")
							
						})
						
					})
					
					temp3 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						paste(unlist(temp2[[x]]), sep = " ", collapse = " ")
					})
					
					temp4 <- lapply(seq(from = 1, to = length(currentProject()$samplesFiles), by = 1), function(x){
						paste0(x2[x], ": ", temp3[[x]])
					})
					
					calibFile$temp <- unlist(str_split(currentProject()$EtalonPath, "/"))[length(unlist(str_split(currentProject()$EtalonPath, "/")))]
					
					output$start2 <- renderUI({
						fluidRow(style="margin-left:0px;margin-right:0px",
							   box(
							   	title = list(icon("folder-o"),"Your project:"),
							   	width = 12,
							   	status="primary",
							   	solidHeader = TRUE,
							   	p(icon("share"), "You are editing or finishing a Project"),
							   	p(icon("share"), paste0("Path of the project: ", projChar$temp[[3]])),
							   	p(icon("share"), paste0("Standard files (and their order) within the project: ", temp)),
							   	p(icon("share"), "Sample files (and their order) within the project: "),
							   	lapply(seq(from = 1, to = length(x2), by = 1), function(i){
							   		column(12, offset = 1, p(temp4[[i]]))
							   	}),
							   	p(icon("share"), paste0("Calibration file: ", calibFile$temp, collapse = " ")),
							   	p(icon("share"), paste0("Internal standard element: ", currentProject()$elemStand)),
							   	p(icon("share"), paste0("Detection of a possible machine drift: ", currentProject()$ChoiceUserCorr))
							   ),
							   box(
							   	title = list(icon("folder-o"),"Important information"),
							   	width = 12,
							   	status="primary",
							   	solidHeader = TRUE,
							   	div(
							   		div(p("To set optional parameters (also in the 'Configuration' tab)"), style = "display: inline-block"),
							   		div(actionButton("SetParam", "Configure Session"), style = "display: inline-block; margin-left:20px")
							   	)
							   )
						)
						
						
					})
					
				} else {}
				
			}) #observe
			
			##############################################################
			# create the currentProject element
			##############################################################
			currentProject <- reactive({
				if(!is.null(input$createProjButton)){
					if(!is.null(input$loadProjButton)){
						input$create
						input$load
						tempProj$temp
						
					} else {}
				} else {}
			})
			
			##############################################################
			# set R2Threshold
			##############################################################
			observe({
				if(!is.null(startSession$temp)){
					if(startSession$temp == 1){
						if(!is.null(input$R2)){
							currentProject()$setR2Threshold(input$R2)
						} else {
							currentProject()$setR2Threshold(0.75)
						}
					} else {}
				} else {}
			})
		}
		
		#################################
		#################################
		##### NISTS reduction ###########
		#################################
		#################################
		{    
			Temp <- reactiveValues(t = NULL)
			Temp0 <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$bins
			Temp1 <- reactiveValues(t = NULL)  # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$plat[1]
			Temp2 <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$plat[2]
			dataPlot2 <- reactiveValues(dat = NULL) # a matrix corresponding to the reduced
			
			currentNISTData <- reactiveValues(temp = NULL)
			currentNISTRep <- reactiveValues(temp = NULL)
			currentNumber <- reactiveValues(temp = NULL)
			
			elemUsed <- reactiveValues(temp = 0) # elements to display in plot
			
			BAV <- reactiveValues(temp = 0)
			LOD <- reactiveValues(temp = 0)
			
			######################################################################################
			# Create currentNumber$temp, i.e. the number of the standard currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(input$standardIn)){
						currentNumber$temp <-  match(input$standardIn, currentProject()$standardsFiles)
					}
				}
			})
			
			######################################################################################
			# Create currentNISTData$temp, i.e. the data of the standard currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(input$standardIn)){
						if(!is.null(match(input$standardIn, currentProject()$standardsFiles))){
							if(length(match(input$standardIn, currentProject()$standardsFiles)) != 0){
								currentNISTData$temp <-  currentProject()$standards[[1]]$rep_data[[match(input$standardIn, currentProject()$standardsFiles)]]$data
							} else {}
						} else {}	
					} else {}
				} else {}
			})
			
			######################################################################################
			# Create currentNISTRep$temp, i.e. the repository of the standard currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(input$standardIn)){
						if(!is.null(match(input$standardIn, currentProject()$standardsFiles))){
							if(length(match(input$standardIn, currentProject()$standardsFiles)) != 0){
								currentNISTRep$temp <-  currentProject()$standards[[1]]$rep_data[[match(input$standardIn, currentProject()$standardsFiles)]]
							} else {}
						} else {}
					} else {}
				} else {}
				
			})
			
			######################
			# set elemUsed
			######################
			observe({
				if(!is.null(input$selectall)){
					if(is.null(input$checkGroup)){
						elemUsed$temp <- ElemStand$temp
					}else{
						if(input$selectall%%2 == 0 & length(input$checkGroup) != length(currentProject()$listeElem)){
							isolate({
								elemUsed$temp <- input$checkGroup
								updateSliderInput(session, "bins", value = input$bins)
								updateSliderInput(session, "plat", value = input$plat)
							})
						} else {}
					} 
				}else {}
			}) #observe
			
			########################################################################################
			# define output$Standards1, i.e. the top div
			########################################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(startSession$temp)){
						
						if(startSession$temp == 0){
							output$Standards1 <- renderUI({NULL})
							output$Standards2 <- renderUI({NULL})
						} else {}
						
						if(startSession$temp == 1){
							
							output$Standards1 <- renderUI({
								
								fluidRow(
									box(
										width=12,
										background = "green",
										height=85,
										column(5, class = "class3",
											 div(h3(icon("flask"),"Step 2. Standard sample reducing"), style = "display: inline-block;")
										),
										column(3, class = "class1",
											 p(icon("eye"), "Select standard sample"),
											 selectInput("standardIn", "" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%')
										),
										column(2,
											 div(style="height: 25px"),
											 actionButton("saveNists", "Save")
										)
									)
								)
								
							})
							
						} else {}
						
					} else {}
				} else {}	
			}) #observe
			
			################################################################
			# create signi, i.e. the information about the values below BAV
			################################################################
			output$signi <- renderUI({
				
				input$standardIn
				input$listeElem
				input$Courbe
				input$bins
				
				if(!is.null(input$CourbeNIST)){
					if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
						div(
							div(style = "display: inline-block;",
							    div(p(paste0("B.A.V.*: ", " ", " ", round(BAV$temp[grep(input$listeElem, names(BAV$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px;"),
							    div(p(paste0("L.O.D.**: ", " ",  " ",round(LOD$temp[grep(input$listeElem, names(LOD$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							div(style = "display: inline-block; vertical-align: top; margin-top: 10px",
							    div(style = "width:70px; vertical-align:top; margin-left: 50px; height: 30px; display: inline-block; background-color: rgba(232,26,29,0.5); border-style:solid; border-top: dotted 1px rgb(232,26,29); border-bottom: white; border-right: white; border-left:white"),
							    div(p("Under B.A.V."), style = "display: inline-block; margin-left:10px;")
							),
							br(),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
						
					} else if(input$CourbeNIST == "Blank removed"){
						div(
							div(style = "display: inline-block;",
							    div(p(paste0("B.A.V.*: ", " ",  " ", round(BAV$temp[grep(input$listeElem, names(BAV$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px"),
							    div(p(paste0("L.O.D.**: ", " ",  " ", round(LOD$temp[grep(input$listeElem, names(LOD$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							div(style = "display: inline-block; vertical-align: top; margin-top: 10px",
							    div(style = "width:70px; vertical-align:top; margin-left: 50px; height: 30px; display: inline-block; background-color: rgba(232,26,29,0.5); border-style:solid; border-top: dotted 1px rgb(232,26,29); border-bottom: white; border-right: white; border-left:white"),
							    div(p("Under L.O.D."), style = "display: inline-block; margin-left:10px;")
							),
							br(),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
					} else {
						div(
							div(
								div(p(paste0("B.A.V.: ", " ",  " ",round(BAV$temp[grep(input$listeElem, names(BAV$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px"),
								div(p(paste0("L.O.D.: ", " ",  " ",round(LOD$temp[grep(input$listeElem, names(LOD$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
					}
				} else {}
			})
			
			################################################################
			# set output$Standards1 (i.e. the top div)
			# define:
			# 	output$Standards2: the raw data box
			#	output$distPlot: teh raw data
			#	output$distPlot2: the reduced data
			################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(input$standardIn)){
						if(!is.null(startSession$temp)){
							if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0 & length(currentNumber$temp) != 0){
								if(startSession$temp == 0){
									output$Standards1 <- renderUI({NULL})
									output$Standards2 <-  renderUI({NULL})
								} else {}
								if(startSession$temp == 1){
									
									if(is.null(input$standardIn)){
										
										output$Standards1 <- renderUI({
											
											fluidRow(
												box(
													width=12,
													background = "green",
													column(5, class = "class3",
														 div(h3(icon("flask"),"Step 2. Standard sample reducing"), style = "display: inline-block;")
													),
													column(3,
														 selectInput("standardIn", "Select standard sample" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%')
													),
													column(2,
														 br(),
														 actionButton("saveNists", "Save")
													)
												)#box
											)
											
										})
										
									}else{
										
										if(flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] == 0){
											
											output$Standards1 <- renderUI({
												
												fluidRow(
													box(
														width=12,
														background = "green",
														column(5, class = "class3",
															 div(h3(icon("flask"),"Step 2. Standard sample reducing"), style = "display: inline-block;")
														),
														column(3, class = "class1",
															 p(icon("eye"), "Select standard sample"),
															 selectInput("standardIn", "" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%')
														),
														column(2,
															 div(style="height: 25px"),
															 actionButton("saveNists", "Save")
														)
													)#box
												)
												
											})
											
											output$Standards2 <- renderUI({
												
												if(length(currentNumber$temp) != 0){
													
													minB <- currentNISTData$temp[1,1]
													maxB <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													minP <- currentNISTData$temp[1,1]
													maxP <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													value1 <- currentProject()$detectBlank(currentNISTData$temp, col = grep(currentProject()$elemStand, colnames(currentNISTData$temp)))
													value2 <- currentProject()$detectPlateau(currentNISTData$temp, col = grep(currentProject()$elemStand, colnames(currentNISTData$temp)))
													step <- currentProject()$standards[[1]]$setRep_pas()
													
													if(!is.na(value1[2]) &  !is.na(value2[2])){
														fluidRow(
															column(8, style  = "padding-right: 5px"  ,
																 box(
																 	title = list(icon("share"),"Blank and plateau limits selection"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	height = "640px",
																 	fluidRow(
																 		column(10,
																 			 plotOutput("distPlot"),
																 			 column(1),
																 			 column(11,
																 			 	 div(style="height: 82px",
																 			 	     sliderInput("bins","Background limits", value = value1, min = minB, max = maxB, step = step, width = '100%', round = TRUE)
																 			 	 ),
																 			 	 div(style="height: 27px",
																 			 	     sliderInput("plat","Plateau limits", value = value2, min = minP, max = maxP,step = step, width = '100%')
																 			 	 )
																 			 	 
																 			 	 
																 			 )
																 		),
																 		column(2,
																 			 div(style = "height: 10px",
																 			     actionLink("selectall","Select All"),
																 			     div(style ="",
																 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
																 			             div(style = "height:6px; width: 20px;"),
																 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
																 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
																 			             	
																 			             })
																 			             
																 			         ),
																 			         div(checkboxGroupInput("checkGroup", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
																 			         
																 			     )
																 			 )
																 		)
																 		
																 		
																 		
																 		
																 	)
																 )
																 
															),
															column(4, style  = "padding-left: 5px",
																 box(
																 	title = list(icon("share"),"Reduced data verification"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	collapsible = TRUE,
																 	column(6,class = "class1",
																 		 p(icon("cubes"),"Element plotted"),
																 		 selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
																 	),
																 	column(6, class = "class1",
																 		 p(icon("area-chart"),"Curve plotted"),
																 		 selectInput("CourbeNIST", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), selected  = "Plateau", width = '100%')
																 	),
																 	div(plotOutput("distPlot2", height = '350px'), style = "height:430px"),
																 	uiOutput("signi")
																 ),
																 box(
																 	title = list(icon("floppy-o"),"Graphic export"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	collapsible = TRUE,
																 	collapsed = TRUE,
																 	selectizeInput("ElementToExport", label = "Element(s) to export",
																 			   choices = currentProject()$listeElem,
																 			   selected = currentProject()$listeElem, multiple = TRUE),
																 	checkboxGroupInput("courveToExport", label = "Curve(s) to export",
																 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"),
																 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), inline = TRUE),
																 	div(actionButton("ExportGraph","Export graphics"), align="center")
																 )
																 
																 
																 
															)
															
														)	
													} else {
														fluidRow(
															column(8, style  = "padding-right: 5px"  ,
																 box(
																 	title = list(icon("share"),"Blank and plateau limits selection"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	height = "640px",
																 	fluidRow(
																 		column(10,
																 			 plotOutput("distPlot")
																 		),
																 		column(2,
																 			 div(style = "height: 10px",
																 			     actionLink("selectall","Select All"),
																 			     div(style ="",
																 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
																 			             div(style = "height:6px; width: 20px;"),
																 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
																 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
																 			             	
																 			             })
																 			             
																 			         ),
																 			         div(checkboxGroupInput("checkGroup", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
																 			         
																 			     )
																 			 )
																 		)
																 		
																 		
																 		
																 		
																 	)
																 )
																 
															),
															column(4, style  = "padding-left: 5px",
																 box(
																 	title = list(icon("share"),"Reduced data verification"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	collapsible = TRUE,
																 	column(6,class = "class1",
																 		 p(icon("cubes"),"Element plotted"),
																 		 selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
																 	),
																 	column(6, class = "class1",
																 		 p(icon("area-chart"),"Curve plotted"),
																 		 selectInput("CourbeNIST", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), selected  = "Plateau", width = '100%')
																 	),
																 	div(plotOutput("distPlot2", height = '350px'), style = "height:430px"),
																 	uiOutput("signi")
																 ),
																 box(
																 	title = list(icon("floppy-o"),"Graphic export"),
																 	status="success",
																 	solidHeader = TRUE,
																 	width = "100%",
																 	collapsible = TRUE,
																 	collapsed = TRUE,
																 	selectizeInput("ElementToExport", label = "Element(s) to export",
																 			   choices = currentProject()$listeElem,
																 			   selected = currentProject()$listeElem, multiple = TRUE),
																 	checkboxGroupInput("courveToExport", label = "Curve(s) to export",
																 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"),
																 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), inline = TRUE),
																 	div(actionButton("ExportGraph","Export graphics"), align="center")
																 )
																 
																 
																 
															)
															
														)
													}
													
													
													
													
												} else {}
											})
											
											output$distPlot <- renderPlot({
												
												par(mar = c(3,3.5,1.75,0))
												
												if(length(currentNumber$temp) != 0  & !is.null(currentNISTData$temp)){
													
													maxY <- max(currentNISTData$temp, na.rm = TRUE)
													
													minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
													maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
													
													if(is.null(input$checkGroup)){}
													else{
														plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
														mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
														mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
														
														if(length(input$checkGroup) > 1){
															lapply(seq(from = 2, to = length(input$checkGroup), by = 1), function(x){
																par(new = TRUE)
																plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
															})
														}
														
														if(!is.null(input$bins) & !is.null(input$plat)){
															if(is.na(input$bins[1]) | is.na(input$bins[2])| is.na(input$plat[1]) | is.na(input$plat[2])){}
															else{
																Temp$t  <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$bins[1])[[2]]
																Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$bins[2])[[2]]
																Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$plat[1])[[2]]
																Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$plat[2])[[2]]
																
															}
														}
														
														if(!is.null(Temp2$t)){
															
															rect(currentNISTData$temp[Temp$t,1],-maxY,currentNISTData$temp[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
															rect(currentNISTData$temp[Temp1$t,1],-maxY,currentNISTData$temp[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
															
															abline(v = currentNISTData$temp[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
															abline(v = currentNISTData$temp[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
															abline(v = currentNISTData$temp[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
															abline(v = currentNISTData$temp[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
															
															lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp$t,1],  currentNISTData$temp[Temp$t,x],  cex = 3, col ="grey")})
															lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,x], cex = 3, col ="grey")})
															lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
															lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
															
														} else {}
													}
													
												}
												
											}, height = 400)
											
											output$distPlot2 <- renderPlot({
												currentProject()$valRemplace
												BAV$temp
												LOD$temp
												if(is.null(dataPlot2$dat)){}
												else{
													if(length(which(!is.na(dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
													} else{
														par(mar = c(3.5,3.7,1.75,1))
														plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste0("Data ",input$CourbeNIST),side=3,line=0.75, cex=1.2, font = 2)
														
														if(length(currentNumber$temp) != 0 & !is.null(currentNISTRep$temp$BlankAverarge)){
															if(length(currentNISTRep$temp$BlankAverarge) == 1){
																if(!is.na(currentNISTRep$temp$BlankAverarge)){
																	if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																		abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeNIST == "Blank removed"){
																		abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																	abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeNIST == "Blank removed"){
																	abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
														} else {}
													}
													
												}
											})
											
										} else {}
										if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0 & flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] != 0){
											
											output$Standards1 <- renderUI({
												
												fluidRow(
													box(
														width=12,
														background = "green",
														column(5, class = "class3",
															 div(h3(icon("flask"),"Step 2. Standard sample reducing"), style = "display: inline-block;")
														),
														column(3,  class = "class1",
															 p(icon("eye"), "Select standard sample"),
															 selectInput("standardIn", "" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%')
														),
														column(2,
															 div(style="height: 25px"),
															 actionButton("saveNists", "Save")
														)
													)
												)
												
											})
											
											output$Standards2 <- renderUI({
												
												if(length(currentNumber$temp) != 0){
													
													input$standardIn
													
													minB <- currentNISTData$temp[1,1]
													maxB <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													minP <- currentNISTData$temp[1,1]
													maxP <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													value1 <- currentNISTRep$temp$bins
													value2 <- currentNISTRep$temp$plat
													step <- currentProject()$standards[[1]]$rep_pas
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlot"),
															 			 column(1),
															 			 column(11,
															 			 	 div(style="height: 82px",
															 			 	     sliderInput("bins","Background limits", value = value1, min = minB, max = maxB, step = step, width = '100%', round = TRUE)
															 			 	 ),
															 			 	 div(style="height: 27px",
															 			 	     sliderInput("plat","Plateau limits", value = value2, min = minP, max = maxP,step = step, width = '100%')
															 			 	 )
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectall","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroup", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 		
															 	)
															 )
															 
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6,class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeNIST", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2", height = '350px'), style = "height:430px"),
															 	uiOutput("signi")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExport", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExport", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), inline = TRUE),
															 	div(actionButton("ExportGraph","Export graphics"), align="center")
															 )
															 
															 
															 
														)
														
													)
												} else {}
											})
											
											output$distPlot <- renderPlot({
												par(mar = c(3,3.5,1.75,0))
												
												if(length(currentNumber$temp) != 0  & !is.null(currentNISTData$temp) ){
													
														maxY <- max(currentNISTData$temp, na.rm = TRUE)
														
														minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
														maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
														
														if(is.null(input$checkGroup)){
															
														}else{
															plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
															mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
															mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
															mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
															
															if(length(input$checkGroup) > 1){
																lapply(seq(from = 2, to = length(input$checkGroup), by = 1), function(x){
																	par(new = TRUE)
																	plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
																})
															} else {}
															
															if(is.null(input$bins) | is.null(input$plat)){
																
															}else{
																if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
																	
																}else{
																	Temp$t  <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$bins[1])[[2]]
																	Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$bins[2])[[2]]
																	Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$plat[1])[[2]]
																	Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = input$plat[2])[[2]]
																}
															}
															
															if(length(currentNISTData$temp[Temp0$t,1]) != 0){
																rect(currentNISTData$temp[Temp$t,1],-maxY,currentNISTData$temp[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
																rect(currentNISTData$temp[Temp1$t,1],-maxY,currentNISTData$temp[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
																
																abline(v = currentNISTData$temp[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
																abline(v = currentNISTData$temp[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
																abline(v = currentNISTData$temp[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
																abline(v = currentNISTData$temp[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
																
																lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp$t,1],  currentNISTData$temp[Temp$t,x],  cex = 3, col ="grey")})
																lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,x], cex = 3, col ="grey")})
																lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
																lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
															}   else {}
														}
														
													
												} else {}
												
											}, height = 400)
											
											output$distPlot2 <- renderPlot({
												currentProject()$valRemplace
												BAV$temp
												LOD$temp
												if(is.null(dataPlot2$dat)){
												}else{
													if(length(which(!is.na(dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
													} else{
														par(mar = c(3.5,3.7,1.75,1))
														plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste0("Data ",input$CourbeNIST),side=3,line=0.75, cex=1.2, font = 2)
														
														if(length(currentNumber$temp) != 0  & !is.null(currentNISTRep$temp$BlankAverarge)){
															if(length(currentNISTRep$temp$BlankAverarge) == 1){
																if(!is.na(currentNISTRep$temp$BlankAverarge)){
																	if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																		abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeNIST == "Blank removed"){
																		abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																	abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeNIST == "Blank removed"){
																	abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
														} else {}
														
													}
												}
											})
											
										} else {}
										if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
											
											output$Standards1 <- renderUI({
												
												fluidRow(
													box(
														width=12,
														background = "green",
														column(5, class = "class3",
															 div(h3(icon("flask"),"Step 2. Standard sample reducing"), style = "display: inline-block;")
														),
														column(3,  class = "class1",
															 p(icon("eye"), "Select standard sample"),
															 selectInput("standardIn", "" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%')
														),
														column(2,
															 div(style="height: 25px"),
															 actionButton("saveNists", "Delete")
														)
													)
												)
												
											})
											
											output$Standards2 <- renderUI({
												
												input$standardIn
												
												if(length(currentNumber$temp) == 0){}
												else{
													
													minB <- currentNISTData$temp[1,1]
													maxB <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													minP <- currentNISTData$temp[1,1]
													maxP <- currentNISTData$temp[dim(currentNISTData$temp)[1],1]
													
													value1 <- currentNISTRep$temp$bins
													value2 <- currentNISTRep$temp$plat
													
													step <- currentProject()$standards[[1]]$rep_pas
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlot"),
															 			 column(1),
															 			 column(11
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectall","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroup", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 	)
															 )
															 
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeNIST", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2", height = '350px'), style = "height:430px"),
															 	uiOutput("signi")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="success",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExport", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExport", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized", "Outliers free"), inline = TRUE),
															 	div(actionButton("ExportGraph","Export graphics"), align="center")
															 )
															 
															 
															 
														)
														
													)
												}
											})
											
											output$distPlot <- renderPlot({
												
												input$saveNists
												input$standardIn
												
												par(mar = c(3,3.5,1.75,0))
												
												if(length(currentNumber$temp) != 0  & !is.null(currentNISTData$temp)){
													maxY <- max(currentNISTData$temp, na.rm = TRUE)
													minX <- min(currentNISTData$temp[,1], na.rm = TRUE)
													maxX <- max(currentNISTData$temp[,1], na.rm = TRUE)
													
													if(is.null(input$checkGroup)){
														
													}else{
														plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
														mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
														mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
														
														if(length(input$checkGroup) > 1){
															lapply(seq(from = 2, to = length(input$checkGroup), by = 1), function(x){
																par(new = TRUE)
																plot(currentNISTData$temp[,1], currentNISTData$temp[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
																
															})
														}  else {}
														
														if(is.null(input$bins) | is.null(input$plat)){
															
														}else{
															if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
																
															}else{
																Temp$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = currentNISTRep$temp$bins[1])[[2]]
																Temp0$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = currentNISTRep$temp$bins[2])[[2]]
																Temp1$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = currentNISTRep$temp$plat[1])[[2]]
																Temp2$t <- currentProject()$closest(x = currentNISTData$temp[,1],y = currentNISTRep$temp$plat[2])[[2]]
															}
														}
														
														rect(currentNISTRep$temp$bins[1],-maxY,currentNISTRep$temp$bins[2],(1+10/100)*maxY, col = "#8B735564", border = NA)
														rect(currentNISTRep$temp$plat[1],-maxY,currentNISTRep$temp$plat[2],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
														
														abline(v = currentNISTRep$temp$bins[1], lty = "dashed", col = "grey", lwd = 2)
														abline(v = currentNISTRep$temp$bins[2], lty = "dashed", col = "grey", lwd = 2)
														abline(v = currentNISTRep$temp$plat[1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
														abline(v = currentNISTRep$temp$plat[2], lty = "dashed", col = "#4F3CBC50", lwd = 2)
														
														lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp$t,1], currentNISTData$temp[Temp$t,x], cex = 3, col ="grey")})
														lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp0$t,1], currentNISTData$temp[Temp0$t,x], cex = 3, col ="grey")})
														lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp1$t,1], currentNISTData$temp[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
														lapply(input$checkGroup, function(x){points(currentNISTData$temp[Temp2$t,1], currentNISTData$temp[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
														
													}
													# }
												}
											}, height = 400)
											
											output$distPlot2 <- renderPlot({
												currentProject()$valRemplace
												BAV$temp
												LOD$temp
												if(is.null(dataPlot2$dat)){
												}else{
													if(length(which(!is.na(dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))]))) == 0){
														plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
														text(1,0.5, labels = "No data different from NA", cex = 2)
													} else{
														par(mar = c(3.5,3.7,1.75,1))
														plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "", xlab = "", main = "")
														mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
														mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
														mtext(paste0("Data ",input$CourbeNIST),side=3,line=0.75, cex=1.2, font = 2)
														
														if(length(currentNumber$temp) != 0  & !is.null(currentNISTRep$temp$BlankAverarge)){
															if(length(currentNISTRep$temp$BlankAverarge) == 1){
																if(!is.na(currentNISTRep$temp$BlankAverarge)){
																	if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																		abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeNIST == "Blank removed"){
																		abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
																	abline(a = currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$BlankAverarge[grep(input$listeElem, names(currentNISTRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeNIST == "Blank removed"){
																	abline(a = currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentNISTRep$temp$LOD[grep(input$listeElem, names(currentNISTRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
														} else {}
														
													}
												}
											})
											
										} else {}
										
									}
								} else {}
								
							} else {}
						} else {}
					} else {}
				}else {}
			}) #observe
			
			################################################################
			# calculate and render dataPlot2
			################################################################
			observe({
				currentProject()$valRemplace
				if(!is.null(currentProject()) & !is.null(input$standardIn) & !is.null(input$CourbeNIST) & !is.null(flagStandard$temp) & !is.null(currentNISTRep)){
					if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0){
						if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
							if(!is.null(input$bins) & !is.null(input$plat) & !is.null(Temp$t) & !is.null(Temp0$t) & !is.null(Temp1$t) & !is.null(Temp2$t)){
								if(is.finite(Temp$t)){
									dataPlot2$dat <- currentNISTRep$temp$getData(curve = input$CourbeNIST, bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
									BAV$temp <- currentNISTRep$temp$BlankAverarge
									LOD$temp <- currentNISTRep$temp$LOD
								} else{}
							} else{}
						} else{}
						if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
							dataPlot2$dat <- currentNISTRep$temp$renderData(curve = input$CourbeNIST)
							BAV$temp <- currentNISTRep$temp$BlankAverarge
							LOD$temp <- currentNISTRep$temp$LOD
						} else{}
					} else{}
				} else{}
			}) #observe
			
			################################################################
			#updateCheckboxGroupInput & checkGroup
			################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$selectall)){
					if(input$selectall == 0){
						return(NULL)
					} else if(input$selectall%%2 == 0) {
						updateCheckboxGroupInput(session,"checkGroup","",choices=currentProject()$listeElem, selected = elemUsed$temp)
					}  else {
						updateCheckboxGroupInput(session,"checkGroup","",choices=currentProject()$listeElem,selected = currentProject()$listeElem)
					}
				} else {}
			}) #observe
			
			################################################################
			# set flagStandard when the standard is saved
			################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$saveNists) & !is.null(input$standardIn)){
					
					if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0){
						if(input$saveNists > 0){
							isolate({
								flagStandard$temp[which(currentProject()$standardsFiles == input$standardIn)] <- flagStandard$temp[which(currentProject()$standardsFiles == input$standardIn)] + 1
								updateSelectInput(session, "listeElem", selected = input$listeElem)
								updateSelectInput(session, "CourbeNIST", selected = input$CourbeNIST)
								load$temp <- load$temp +1
							})
						} else {}
					} else {}
					updateSelectInput(session, "standardIn", selected = input$standardIn)
				} else {}
			}) #observe
			
			################################################################
			# to avoid elementR saving data when first delete loaded project
			################################################################
			observe({
				if(!is.null(input$standardIn)){
					isolate({
						if(load$temp == 1){
							load$temp <- load$temp +1
						}
					})
				} else {}
			}) #observe
			
			################################################################
			# Save all the data when the flagStandard is in saved position &
			# delete all data if the flag is in the position of delete
			################################################################
			observe({
				if(!is.null(currentProject())){
					if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0 & length((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2)) != 0){
						
						if(projChar$temp[1] == 2 & load$temp == 1){
							
							if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 0 & flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] != 0){
								isolate({
									currentProject()$setflagMachineCorrection(x = 0)
									currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 0)
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
										currentProject()$samples[[x]]$setrep_type2(NA)
									})
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "transect", value = 0)})
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
										lapply(seq(from = 1, to = length(currentProject()$flag_Sample[[x]]), by = 1), function(i){
											currentProject()$setflagSample(sample = x, replicate = i, value = 0)
										})
									})
									flagSampleDetail$temp <- currentProject()$flag_Sample
									flagRealign$temp <- currentProject()$flagRealign
									validCorrection$temp <- currentProject()$flagMachineCorrection
									currentProject()$set_summarySettings(name = input$standardIn, 
															 rank = NA, 
															 bins1 = NA, 
															 bins2 = NA, 
															 plat1 = NA, 
															 plat2 = NA, 
															 average = rep(NA, length(currentProject()$listeElem)), 
															 LOD = rep(NA, length(currentProject()$listeElem)))
								})
								
								
							}  else {}
							
							if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1){
								isolate({
									currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 1)
									currentNISTRep$temp$setBins(bins = c(currentNISTData$temp[Temp$t,1], currentNISTData$temp[Temp0$t,1]))
									currentNISTRep$temp$setPlat(plat = c(currentNISTData$temp[Temp1$t,1],currentNISTData$temp[Temp2$t,1]))
									currentNISTRep$temp$setDataOutlierFree(bins = c(Temp$t, Temp0$t), 
															   plat = c(Temp1$t,Temp2$t), 
															   rempl = currentProject()$valRemplace, 
															   method = input$outlierDetect, 
															   nbOutliers = 3)
									currentProject()$set_summarySettings(name = input$standardIn,
															 rank = currentProject()$standardRank[which(names(currentProject()$standardRank) == input$standardIn)], 
															 bins1 = currentNISTData$temp[Temp$t,1], 
															 bins2 = currentNISTData$temp[Temp0$t,1], 
															 plat1 = currentNISTData$temp[Temp1$t,1], 
															 plat2 = currentNISTData$temp[Temp2$t,1], 
															 average = currentNISTRep$temp$BlankAverarge, 
															 LOD = currentNISTRep$temp$LOD)
									currentProject()$standards[[1]]$setRep_pas()
								})
								
							}  else {}
							
						} else {
							if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 0 & flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] != 0 & input$saveNists >0){
								isolate({
									currentProject()$setflagMachineCorrection(x = 0)
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
										currentProject()$samples[[x]]$setrep_type2(NA)
									})
									currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 0)
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "transect", value = 0)})
									lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
										lapply(seq(from = 1, to = length(currentProject()$flag_Sample[[x]]), by = 1), function(i){
											currentProject()$setflagSample(sample = x, replicate = i, value = 0)
										})
									})
									flagSampleDetail$temp <- currentProject()$flag_Sample
									flagRealign$temp <- currentProject()$flagRealign
									validCorrection$temp <- currentProject()$flagMachineCorrection
									currentProject()$set_summarySettings(name = input$standardIn, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = rep(NA, length(currentProject()$listeElem)), LOD = rep(NA, length(currentProject()$listeElem)))
								})
								
								
							}  else {}
							
							if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1 & input$saveNists >0){
								isolate({
									currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 1)
									currentNISTRep$temp$setBins(bins = c(currentNISTData$temp[Temp$t,1], currentNISTData$temp[Temp0$t,1]))
									currentNISTRep$temp$setPlat(plat = c(currentNISTData$temp[Temp1$t,1],currentNISTData$temp[Temp2$t,1]))
									currentNISTRep$temp$setDataOutlierFree(bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = currentProject()$valRemplace, method = input$outlierDetect, nbOutliers = 3)
									currentProject()$set_summarySettings(name = input$standardIn, rank = currentProject()$standardRank[which(names(currentProject()$standardRank) == input$standardIn)], bins1 = currentNISTData$temp[Temp$t,1], bins2 = currentNISTData$temp[Temp0$t,1], plat1 = currentNISTData$temp[Temp1$t,1], plat2 = currentNISTData$temp[Temp2$t,1], average = currentNISTRep$temp$BlankAverarge, LOD = currentNISTRep$temp$LOD)
									currentProject()$standards[[1]]$setRep_pas()
								})
								
							}  else {}
						}
						
						
						
					} else {}
				}
			}) #observe
			
			################################################################
			# Go to the next standard sample
			################################################################
			observe({
				if(!is.null(currentProject())){
					if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0 & length((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2)) != 0){
						if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1 & input$saveNists > 0){
							isolate({
								passage <- currentProject()$standardsFiles[flagStandard$temp%%2 == 0][1]
								if(!is.na(passage)){
									delay(2000,updateSelectInput(session, "standardIn", selected = passage))
								} else {}
							})
						}  else {}
						
					} else {}
				} else {}
			}) #observe
			
		}
		
		#######################
		#######################
		## VERIF STANDARDS ####
		#######################
		#######################
		{
			tableauStat <- reactiveValues(temp = NULL) # the matrix with all linear regression parameters
			machineCorrection <- reactiveValues(temp = 0)  # the vector corresponding to the choic of the user to correct or not the maichine drift for each element
			
			zero <- reactiveValues(temp = 0) # the number of element which has 0 standard samples in the linear regression
			one <- reactiveValues(temp = 0) # the number of element which has 1 standard samples in the linear regression
			two <- reactiveValues(temp = 0) # the number of element which has 2 standard samples in the linear regression
			three <- reactiveValues(temp = 0) # the number of element which has at least 3 standard samples in the linear regression
			
			elemChosen <- reactiveValues(temp = 0) # the element which will be displayed (all the element which need to be corrected + all the element which have two, one or zero sample)
			
			coord <- reactiveValues(temp = NULL) # order of the standards in the ICPMS run
			
			flagHR <- reactiveValues(temp = 0) # a value to set graphical parameters (adjust the bar between each element)
			
			###############################################################################
			# set the elemChosen$temp
			# i.e. which elements have a pvalue < 0.05, which have less than 2 values 
			# (-> hereafter called deviated elements)
			# if deviated elements < 6, print random element to show at least 6 elements
			###############################################################################
			observe({
				input$saveNists
				if(!is.null(currentProject()) & !is.null(input$SuppDonne)){
					
					if(length(which(currentProject()$flag_stand != 1)) == 0){
						
						temp <- c(which(tableauStat$temp[,4]< 0.05), which(currentProject()$nbCalib == 2), which(currentProject()$nbCalib == 1), which(currentProject()$nbCalib == 0))
						
						tempOR <- length(temp)
						
						# if deviated elements < 0
						if(length(temp) !=0 & length(temp) < 6){
							temp <- c(temp, sample(seq(from = 1, to = length(currentProject()$listeElem), by = 1)[-temp],6-tempOR,replace = FALSE))
							names(temp) <- currentProject()$listeElem[temp]
						} else {}
						
						# if deviated elements == 0
						if(length(temp) == 0){
							temp <- sample(seq(from = 1, to = length(currentProject()$listeElem), by = 1),6,replace = FALSE)
							names(temp) <- currentProject()$listeElem[temp]
						} else {}
						
						elemChosen$temp <- temp
						
					} else {}
					
				} else {}
				
			}) #observe
			
			#################################################################################################
			# proceed to the linear regression for the creation mode 
			# and set tableauStat$temp & machineCorrection$temp
			# reminder: 
			#    projChar$temp[[1]] inform if the project is a modification or the creation of a project
			#################################################################################################
			observe({
				input$saveNists
				input$validDonne
				input$validDonne2
				
				if(!is.null(currentProject())){
					if(!is.null(input$SuppDonne)){
						
						if(length(which(currentProject()$flag_stand != 1)) == 0){
							
							if(projChar$temp[[1]] == 1){
								
								machineCorrection$temp <- rep(FALSE, length(currentProject()$listeElem))
								
								currentProject()$standards[[1]]$setRep_table(nelem = currentProject()$listeElem) # table providing the mean and SD of each standard and the averaged (and SD) value of the whole session
								
								currentProject()$correction()
								
								tableauStat$temp <- currentProject()$regressionModel
								
							} else {}
							
							if(projChar$temp[[1]] == 2){
								
								if((validCorrection$temp%%2) == 0){
									machineCorrection$temp <- rep(FALSE, length(currentProject()$listeElem))
								} else{
									machineCorrection$temp <- currentProject()$machineCorrection
								}
								
								tableauStat$temp <- currentProject()$regressionModel
								
							} else {}
						} else {}
					}
				}
				
			}) #observe
			
			########################################################################
			# set  coord$temp, ie. the order of each standard
			########################################################################
			observe({
				input$saveNists
				input$tab
				
				if(!is.null(currentProject()) & !is.null(input$SuppDonne) & !is.null(validCorrection$temp)){
					
					if(length(which(currentProject()$flag_stand != 1)) == 0){
						
						if(length(currentProject()$standards[[1]]$rep_dataFinale) != 1){
							
							temporaryTab <- currentProject()$standards[[1]]$rep_dataFinale
							
							temp <- str_sub(rownames(temporaryTab), 1, -6)
							
							X <- vector()
							
							for (i in seq(from = 1, to = length(currentProject()$standardsFiles), by = 1)){
								X[i] <- currentProject()$standardRank[which(names(currentProject()$standardRank) == temp[i])]
								
							}
							
							coord$temp <- X
							
						} else {}
						
						
					} else {}
				} else {}
			})
			
			###############################################################################
			# set zero$temp one$temp two$temp three$temp, i.e. a list of elemnt that have
			# 0, 1, 2, or more than 2 vales and that have to be corrected
			# set flagHR$temp a flag for the formatting of elementR 
			###############################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$ElementChosen)){
					
					if(length(currentProject()$nbCalib) !=0){
						zero$temp <-intersect(which(currentProject()$nbCalib == 0), vapply(seq(from = 1, to = length(input$ElementChosen), by = 1), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}, FUN.VALUE = numeric(1)))
						one$temp <-intersect(which(currentProject()$nbCalib == 1), vapply(seq(from = 1, to = length(input$ElementChosen), by = 1), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}, FUN.VALUE = numeric(1)))
						two$temp <-intersect(which(currentProject()$nbCalib == 2), vapply(seq(from = 1, to = length(input$ElementChosen), by = 1), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}, FUN.VALUE = numeric(1)))
						three$temp <-intersect(which(currentProject()$nbCalib >= 3), vapply(seq(from = 1, to = length(input$ElementChosen), by = 1), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}, FUN.VALUE = numeric(1)))
						
						if(length(zero$temp) != 0){
							flagHR$temp <- 0
						}else if(length(one$temp) != 0){
							flagHR$temp <- 1
						}else if(length(two$temp) != 0){
							flagHR$temp <- 2
						} else{
							flagHR$temp <- 3
						}
					}
					
					
				} else {}
			})
			
			###################################################################################################
			# define output$MachDrift3, the whole div with all the elements
			# according to output$MachDrift3_0, the div with the element containing no standard value
			#              output$MachDrift3_1, the div with the element containing 1 standard value
			#			 output$MachDrift3_2, the div with the element containing 2 standard value
			#			 output$MachDrift3_3, the div with the element containing more than 2 standard values
			###################################################################################################
			observe({
				input$saveNists
				if(!is.null(currentProject()) & !is.null(input$SuppDonne) & !is.null(validCorrection$temp) & !is.null(coord$temp)){
					
					if(length(coord$temp) != 1 | coord$temp[1] != 0){
						if(length(which(currentProject()$flag_stand != 1)) == 0){
							
							output$MachDrift3 <- renderUI({
								
								fluidRow(
									box(
										class = "box1",
										title = "Machine drift verification", status = "danger", solidHeader = TRUE,
										collapsible = TRUE,
										width = 12,
										uiOutput('MachDrift3_3'),
										uiOutput('MachDrift3_2'),
										uiOutput('MachDrift3_1'),
										uiOutput('MachDrift3_0')
									)
								)
								
							}) #eo  output$MachDrift3
							
							output$MachDrift3_0 <- renderUI({
								
								lapply(zero$temp, function(x){
									
									tablename <- paste("tableSession", x, sep="")
									
									output[[tablename]] <- renderUI({
										div(
											div(
												div("", style ="display: inline-block;width: 370px;text-align: center")
											),
											div(
												div("No value for this chemical element", style ="display: inline-block;width: 370px;text-align: center")
											)
										)
									})
									
									if(flagHR$temp == 1){
										if(length(zero$temp) == 1){
											barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
										} else{
											if((sum(length(two$temp), length(three$temp), length(one$temp), length(zero$temp))%%2) == 0){
												
												if(x != zero$temp[length(zero$temp)] | x != zero$temp[length(zero$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))
												} else {}
												
												if(x == zero$temp[length(zero$temp)] | x == zero$temp[length(zero$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))} else {}
												
											} else {}
											if((sum(length(two$temp), length(three$temp), length(one$temp), length(zero$temp))%%2) == 1){
												
												if(x == zero$temp[length(zero$temp)]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
												} else{
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))}
											} else {}
											
										}
										
									}else{
										barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')"))
									}
									
									box(
										width = 6,
										height = 335,
										column(2,
											 p(currentProject()$listeElem[x], style = "font-size: 50px")),
										column(10,
											 uiOutput(tablename)
										),
										div(style ="200px"),
										br(),
										br(),
										br(),
										column(9,offset = 2),
										br(),
										barre
										
										
									)
								})
							}) #eo  output$MachDrift3_0
							
							output$MachDrift3_1 <- renderUI({
								
								lapply(one$temp, function(x){
									
									plotname2 <- paste("plotSession", x, sep="")
									
									output[[plotname2]] <- renderPlot({
										
										par(mar = c(4.1,4.1,0,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) - min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) *20/100
										
										max <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) + min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) *20/100
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], coord = coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)), ylim=c(min, max),ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand),xlab = "")
										
									})
									
									tablename <- paste("tableSession", x, sep="")
									
									output[[tablename]] <- renderUI({
										div(
											div(
												div("", style ="display: inline-block;width: 370px;text-align: center")
											),
											div(
												div("Only one value for this chemical element", style ="display: inline-block;width: 370px;text-align: center")
											)
										)
									})
									
									if(flagHR$temp == 1){
										
										if(length(one$temp) == 1){
											barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
										}else{
											
											if((sum(length(two$temp), length(three$temp), length(one$temp))%%2) == 0){
												
												if(x != one$temp[length(one$temp)] | x != one$temp[length(one$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))
												} else {}
												
												if(x == one$temp[length(one$temp)] | x == one$temp[length(one$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))} else {}
												
											} else {}
											if((sum(length(two$temp), length(three$temp), length(one$temp))%%2) == 1){
												if(x == one$temp[length(one$temp)]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
													
												}else{
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))}
											} else {}
											
										}
										
									}else{
										barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')"))
									}
									
									box(
										width = 6,
										height = 335,
										column(2,
											 p(currentProject()$listeElem[x], style = "font-size: 40px")),
										column(10,
											 uiOutput(tablename)
										),
										plotOutput(plotname2, height = "200px"),
										br(),
										br(),
										br(),
										column(9,offset = 2),
										br(),
										barre
									)
									
								})
								
							}) #eo  output$MachDrift3_1
							
							output$MachDrift3_2 <- renderUI({
								
								lapply(two$temp, function(x){
									
									tablename <- paste("tableSession", x, sep="")
									
									output[[tablename]] <- renderUI({
										
										if(currentProject()$ChoiceUserCorr == TRUE){
											if(!is.null(input$CorrectAll)){
												
												if(input$CorrectAll == TRUE){
													valCheck <- TRUE
												} else{
													valCheck <- machineCorrection$temp[x]
												}
											}else{
												valCheck <- machineCorrection$temp[x]
											}
											
											if(is.null(validCorrection$temp)){
												val <- "Correction"
												valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
											} else if((validCorrection$temp%%2) == 0){
												val <- "Correction"
												valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
											}else{
												val <- "Correction"
												valB <- machineCorrection$temp[x]
											}
											
											
											#########
											
											div(
												div(
													div("", style ="display: inline-block;width: 370px;text-align: center"),
													div(val, style ="display: inline-block;width: 70px;text-align: center")
												),
												div(
													div("Only two values for this chemical element", style ="display: inline-block;width: 370px;text-align: center"),
													div(valB, style ="display: inline-block;width: 70px;height : 30px; text-align: center;vertical-align: bottom;")
												)
											)
											
										} else {
											p("You chose not to check for machine drift", style = "font-size:medium;font-weight: bold; text-align: center;")
										}
										
										
									})
									
									plotname2 <- paste("plotSession", x, sep="")
									
									output[[plotname2]] <- renderPlot({
										
										par(mar = c(4.1,4.1,0,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) - (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
										
										max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x],na.rm = TRUE) + (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],coord = coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand), xlab = "")
										
										abline(a = currentProject()$regressionModel[x,5], b= currentProject()$regressionModel[x,6], col ="red", lty = 2)
									})
									
									if(flagHR$temp == 2){
										
										if(length(two$temp) == 1){
											barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
										}else{
											
											if((sum(length(two$temp), length(three$temp))%%2) == 0){
												if(x != two$temp[length(two$temp)] | x != two$temp[length(two$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))
												}else {}
												if(x == two$temp[length(two$temp)] | x == two$temp[length(two$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))}else {}
											} else {}
											if((sum(length(two$temp), length(three$temp))%%2) == 1){
												if(x == two$temp[length(two$temp)]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
												}else{
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))}
											} else {}
											
										}
									} else{
										barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')"))
									}
									
									box(
										width = 6,
										height = 335,
										column(2,
											 p(currentProject()$listeElem[x], style = "font-size: 40px")),
										column(10, uiOutput(tablename)
										),
										plotOutput(plotname2, height = "200px"),
										br(),
										br(),
										br(),
										column(9,offset = 2,
											 p(paste0("Y (Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[x,5],3), " + X (Stand. order) * ", round(currentProject()$regressionModel[x,6],3)), style = "font-size:medium;font-weight: bold;text-align: center;")
										),
										br(),
										barre
									)
									
									
								})
							}) #eo output$MachDrift3_2
							
							output$MachDrift3_3 <- renderUI({
								
								lapply(three$temp, function(x){
									
									machineCorrection$temp[x]
									
									plotname2 <- paste("plotSession", x, sep="")
									
									output[[plotname2]] <- renderPlot({
										
										par(mar = c(4.1,4.1,0,2.1), bg = NA)
										
										min <- min(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) - (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], na.rm = TRUE))*3
										
										max <- max(currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], na.rm = TRUE) + (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], na.rm = TRUE))*3
										
										currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[seq(from = 1, to = length(currentProject()$flag_stand), by = 1),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],coord =  coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand), xlab = "")
										
										if(is.na(tableauStat$temp[x,7])){
											abline(a = currentProject()$regressionModel[x,5], b= currentProject()$regressionModel[x,6], col ="red", lty = 2)
										} else {
											if(tableauStat$temp[x,4] <0.05 & tableauStat$temp[x,7] >= currentProject()$R2Threshold){
												abline(a = currentProject()$regressionModel[x,5], b= currentProject()$regressionModel[x,6], col ="red", lty = 2)
											} else {NULL}
										}
										
										
										
										
									}) #renderPlot
									
									tablename <- paste("tableSession", x, sep="")
									
									output[[tablename]] <- renderUI({
										
										if(currentProject()$ChoiceUserCorr == TRUE){
											if(!is.na(tableauStat$temp[x,7])){
												
												if(tableauStat$temp[x,4] <0.05 & tableauStat$temp[x,7] < currentProject()$R2Threshold){
													
													if(currentProject()$ChoiceUserCorr == TRUE){
														if(!is.null(input$CorrectAll)){
															
															if(input$CorrectAll == TRUE){
																valCheck <- TRUE
															} else{
																valCheck <- machineCorrection$temp[x]
															}
														}else{
															valCheck <- machineCorrection$temp[x]
														}
														
														if(is.null(validCorrection$temp)){
															val <- "Correction"
															valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
														} else if((validCorrection$temp%%2) == 0){
															val <- "Correction"
															valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
														}else{
															val <- "Correction"
															valB <- machineCorrection$temp[x]
														}
														
														
														#########
														
														div(
															div(
																div("", style ="display: inline-block;width: 370px;text-align: center"),
																div(val, style ="display: inline-block;width: 70px;text-align: center")
															),
															div(
																p("Not linear model, neightbore regression", style ="display: inline-block;width: 370px;text-align: center"),
																div(valB, style ="display: inline-block;width: 70px;height : 30px; text-align: center;vertical-align: bottom;")
															)
														)
													}
													
												} else {
													##################################################################################################
													# 1. create and format (i.e. valeur1) the value (i.e. valeur1Bis) corresponding to the normality 
													# of the residuals of the linear regression
													##################################################################################################
													if(is.na(tableauStat$temp[x,1])){
														valeur1 <- 'font-weight:bold; color:red'
													}else{
														if(tableauStat$temp[x,1] < 0.05){
															valeur1 <- 'font-weight:bold; color:red'
														}else{valeur1 <- 'font-weight:normal; color:black'}
													}
													
													if(is.na(tableauStat$temp[x,1])){
														valeur1Bis <- tableauStat$temp[x,1]
													} else{
														if(tableauStat$temp[x,1] < 0.001){
															valeur1Bis <- format(tableauStat$temp[x,1], scientific = TRUE)
														}else{
															valeur1Bis <- format(round(tableauStat$temp[x,1], digits = 2), scientific = FALSE)
														}
													}
													
													########################################################################################################
													# 2. create and format (i.e. valeur2) the value (i.e. valeur2Bis) corresponding to the homoscedasticity 
													# of the residuals of the linear regression
													########################################################################################################
													if(is.na(tableauStat$temp[x,2])){
														valeur2 <- 'font-weight:bold; color:red'
													}else{
														if(tableauStat$temp[x,2] < 0.05){
															valeur2 <- 'font-weight:bold; color:red'
														}else{valeur2 <- 'font-weight:normal; color:black'}
													}
													
													if(is.na(tableauStat$temp[x,2])){
														valeur2Bis <- tableauStat$temp[x,2]
													}else{
														if(tableauStat$temp[x,2] < 0.001){
															valeur2Bis <- format(tableauStat$temp[x,2], scientific = TRUE)
														}else{
															valeur2Bis <- format(round(tableauStat$temp[x,2], digits = 2), scientific = FALSE)
														}
													}
													
													########################################################################################################
													# 3. create and format (i.e. valeur3) the value (i.e. valeur3Bis) corresponding to the independance 
													# of the residuals of the linear regression
													########################################################################################################
													if(is.na(tableauStat$temp[x,3])){
														valeur3 <- 'font-weight:bold; color:red'
													}else{
														if(tableauStat$temp[x,3] < 0.05){
															valeur3 <- 'font-weight:bold; color:red'
														}else{valeur3 <- 'font-weight:normal; color:black'}
													}
													
													if(is.na(tableauStat$temp[x,3])){
														valeur3Bis <- tableauStat$temp[x,3]
													}else{
														if(tableauStat$temp[x,3] < 0.001){
															valeur3Bis <- format(tableauStat$temp[x,3], scientific = TRUE)
														}else{
															valeur3Bis <- format(round(tableauStat$temp[x,3], digits = 2), scientific = FALSE)
														}
													}
													
													########################################################################################################
													# 4. create and format (i.e. val) the value (i.e. valB) corresponding to the choice of the user
													# to correct the standard values according to the linear regression
													########################################################################################################
													
													if(!is.null(input$CorrectAll)){
														
														if(input$CorrectAll == TRUE){
															valCheck <- TRUE
														} else{
															valCheck <- machineCorrection$temp[x]
														}
													}else{
														valCheck <- machineCorrection$temp[x]
													}
													
													if(is.na(tableauStat$temp[x,4])){
														valeur4 <- 'font-weight:bold; color:red'
														val <- "Correction"
														valB <- "NS"
													} else {
														
														if(tableauStat$temp[x,4] < 0.05){
															valeur4 <- 'font-weight:bold; color:red'
															
															if(is.null(validCorrection$temp)){
																val <- "Correction"
																valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
															} else if((validCorrection$temp%%2) == 0){
																val <- "Correction"
																valB <- eval(parse(text = "checkboxInput(geneRMachineCorr$temp[x],label = '', value = valCheck)"))
															}else{
																val <- "Correction"
																valB <- machineCorrection$temp[x]
															}
															
															
															
														}else{valeur4 <- 'font-weight:normal; color:black'
														val <- "Correction"
														valB <- "NS"
														}
													}
													
													if(is.na(tableauStat$temp[x,4])){
														valeur4Bis <- tableauStat$temp[x,4]
													}else{
														
														if(tableauStat$temp[x,4] < 0.001){
															valeur4Bis <- format(tableauStat$temp[x,4], scientific = TRUE)
														}else{
															valeur4Bis <- format(round(tableauStat$temp[x,4],digits = 2), scientific = FALSE)
														}
													}
													
													########################################################################################################
													# 5. Build the final table
													########################################################################################################
													
													div(
														div(
															div(style ="display: inline-block;width: 70px;text-align: center; "),
															div("Norm.Res", style ="display: inline-block;width: 70px;text-align: center"),
															div("Homosc.Res", style ="display: inline-block;width: 90px;text-align: center"),
															div("Indep.Res", style ="display: inline-block;width: 70px;text-align: center"),
															div("Slope", style ="display: inline-block;width: 70px;text-align: center"),
															div(val, style ="display: inline-block;width: 70px;text-align: center")
														),
														div(
															div("pvalue", style ="display: inline-block;width: 70px;height : 30px; text-align: center;font-style: italic;"),
															div(valeur1Bis, style ="display: inline-block;width: 70px;height : 30px; text-align: center;font-style: italic;", style = valeur1),
															div(valeur2Bis, style ="display: inline-block;width: 90px;height : 30px; text-align: center;font-style: italic;", style = valeur2),
															div(valeur3Bis, style ="display: inline-block;width: 70px;height : 30px; text-align: center;font-style: italic;", style = valeur3),
															div(valeur4Bis, style ="display: inline-block;width: 70px;height : 30px; text-align: center;font-style: italic;", style = valeur4),
															div(valB, style ="display: inline-block;width: 70px;height : 30px; text-align: center;vertical-align: bottom;")
														)
													)
												} 
											} else {}
											
										} else{
											p("You chose not to check for machine drift", style = "font-size:medium;font-weight: bold; text-align: center;text-align: center;")	
										}
										
									})
									
									if(flagHR$temp == 3){
										
										if(length(three$temp) == 1){
											barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
										}else{
											
											if((length(three$temp)%%2) == 0){
												if(x != three$temp[length(three$temp)] | x != three$temp[length(three$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))
												} else {}
												if(x == three$temp[length(three$temp)] | x == three$temp[length(three$temp)-1]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))} else {}
											} else {}
											if((length(three$temp)%%2) == 1){
												if(x == three$temp[length(three$temp)]){
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')" ))
												} else{
													barre <- eval(parse(text = "hr(style='height: 1px; background-color: #9B6CA8; width: 70%; border: none;')" ))}
											} else {}
											
										}
									}else{
										barre <- eval(parse(text = "hr(style='height: 1px; background-color: white; width: 70%; border: none;')"))
									}
									
									box(
										width = 6,
										height = 335,
										column(2,
											 p(currentProject()$listeElem[x], style = "font-size: 40px")),
										column(10,
											 uiOutput(tablename)
										),
										plotOutput(plotname2, height = "200px"),
										br(),
										br(),
										br(),
										column(9,offset = 2,
											 p(paste0("Y (Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[x,5],3), " + X (Stand. order) * ", round(currentProject()$regressionModel[x,6],3)), style = "font-size:medium;font-weight: bold;text-align: center;")
										),
										br(),
										barre
									)
								})
								
							}) #eo output$MachDrift3_3
							
						} else{
							
							output$MachDrift3 <- renderUI({NULL}) #eo  output$MachDrift3
							
							output$MachDrift3_3 <- renderUI({NULL}) # output$MachDrift3_3
							
							output$MachDrift3_0 <- renderUI({NULL}) # output$MachDrift3_0
							
							output$MachDrift3_1 <- renderUI({NULL}) # output$MachDrift3_1
							
							output$MachDrift3_2 <- renderUI({NULL}) # output$MachDrift3_2
							
						}
					} else {}
					
				} else {}
				
			})
			
			##########################################################
			# define output$MachDrift1: First div to the top
			# 	 output$MachDrift2: graphic export
			##########################################################
			observe({
				input$saveNists
				if(!is.null(currentProject()) & !is.null(input$SuppDonne) & !is.null(validCorrection$temp)){
					if(length(which(currentProject()$flag_stand != 1)) == 0){
						if((validCorrection$temp%%2) == 0){
							if(currentProject()$ChoiceUserCorr == TRUE){
								
								output$MachDrift1 <- renderUI({
									fluidRow(
										box(
											width=12,
											background = "olive",
											column(5, style = "margin-top:10px",
												 div(h3(icon("plug"),"Step 3. Machine drift verification"), style = "display: inline-block;")
											),
											column(4,
												 p(icon("eye"), "Element to plot"),
												 div(selectizeInput("ElementChosen", label = "", choices = currentProject()$listeElem, selected = currentProject()$listeElem[elemChosen$temp], multiple = TRUE), style = "margin-top: -20px")
												 
											),
											column(2,
												 style = "width:120px;margin-top:15px",
												 br(),
												 checkboxInput("CorrectAll",label = "Correct all", value = FALSE)
											),
											column(2,
												 style = "width:100px;margin-top:20px",
												 actionButton("validDrift", "Save machine drift", style = "margin-top:10px")
											)
											
										)
									)
								}) #output$MachDrift1
								
							} else {
								output$MachDrift1 <- renderUI({
									
									fluidRow(
										box(
											width=12,
											background = "olive",
											column(5, style = "margin-top:10px",
												 div(h3(icon("plug"),"Step 3. Machine drift verification"), style = "display: inline-block;")
											),
											column(4,
												 p(icon("eye"), "Element to plot"),
												 div(selectizeInput("ElementChosen", label = "", choices = currentProject()$listeElem, selected = currentProject()$listeElem[elemChosen$temp], multiple = TRUE), style = "margin-top: -20px")
												 
											),
											column(2,
												 style = "width:100px;margin-top:20px",
												 actionButton("validDrift", "Save machine drift", style = "margin-top:10px")
											)
											
										)
									)
								}) #output$MachDrift1
							}
							
							output$MachDrift2 <- renderUI({
								fluidRow(
									box(
										title = list(icon("floppy-o"),"Graphic export"),
										status = "danger",
										solidHeader = TRUE,
										collapsible = TRUE,
										collapsed = TRUE,
										width = 12,
										column(10, selectizeInput("MachDriftElementToExport", label = "Element(s) to export",
														  choices = currentProject()$listeElem,
														  selected = currentProject()$listeElem, multiple = TRUE)),
										column(2, br(), actionButton("MachDriftExportGraph","Export graphics"))
									)
								)
							}) #output$MachDrift2
							
						} else {}
						if((validCorrection$temp%%2) == 1){
							if(!is.null(zero$temp)){
								if(length(zero$temp) != 0){
									if(zero$temp[1] != 0){
										tkmessageBox(message = "WARNING: NO STANDARD VALUE FOR AT LEAST ONE CHEMICAL ELEMENT", icon = "error", type = "ok")
									} else {}
								} else{}
							} else {}
							
							
							output$MachDrift1 <- renderUI({
								
								fluidRow(
									box(
										width=12,
										background = "olive",
										column(5, style = "margin-top:10px",
											 div(h3(icon("plug"),"Step 3. Machine drift verification"), style = "display: inline-block;")
										),
										column(4,
											 p(icon("eye"), "Element to plot"),
											 div(selectizeInput("ElementChosen", label = "", choices = currentProject()$listeElem, selected = currentProject()$listeElem[elemChosen$temp], multiple = TRUE), style = "margin-top: -20px")
											 
										),
										column(2,
											 style = "width:120px;margin-top:15px"
										),
										column(2,
											 style = "width:100px;margin-top:20px",
											 actionButton("validDrift", "Change machine drift", style = "margin-top:10px")
										)
										
									)
								)
								
							}) #output$MachDrift1
							
							output$MachDrift2 <- renderUI({
								fluidRow(
									box(
										title = list(icon("floppy-o"),"Graphic export"),
										status = "danger",
										solidHeader = TRUE,
										collapsible = TRUE,
										collapsed = TRUE,
										width = 12,
										column(10, selectizeInput("MachDriftElementToExport", label = "Element(s) to export",
														  choices = currentProject()$listeElem,
														  selected = currentProject()$listeElem, multiple = TRUE)),
										column(2, br(), actionButton("MachDriftExportGraph","Export graphics"))
									)
								)
							}) #output$MachDrift2
						} else {}
					} else {}
					
					
				}else{
					output$MachDrift1 <- renderUI({NULL})#output$MachDrift1
					output$MachDrift2 <- renderUI({NULL})#output$MachDrift2
				}
			})
			
			##########################################################
			# set machineCorrection$temp, i.e. the choice of the user
			# to correct or not the deviated elements
			##########################################################
			observe({
				if(!is.null(input$CorrectAll)){
					isolate({
						
						if(input$CorrectAll == TRUE & (validCorrection$temp%%2) == 0){
							for(i in seq(from = 1, to = length(currentProject()$listeElem), by = 1)){
								
								if(!is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[i]))))){
									machineCorrection$temp[i] <- TRUE
								} else {}
								
							} #eo for loop
						} else {}
						
						if(input$CorrectAll == FALSE & (validCorrection$temp%%2) == 0){
							for(i in seq(from = 1, to = length(currentProject()$listeElem), by = 1)){
								
								if(is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[i]))))){
									machineCorrection$temp[i] <- FALSE
								} else {}
								
							} #eo for loop
						} else {}
						
					}) # eo isolate
				} else {}
			}) #observe
			
			##########################################################
			# set validCorrection$temp (i.e. the validation flag)
			# when input$validDrift changes
			##########################################################
			observe({
				if(!is.null(input$validDrift)){
					
					isolate({
						
						if((input$validDrift%%2) == 1){
							
							validCorrection$temp <- validCorrection$temp + 1
							updateSelectInput(session, 'ElementChosen', selected = input$ElementChosen)
							
						} else {}
						
					}) # eo isolate
				} else {}
			}) #observe
			
			##############################################################################################
			# observe to save de data when the validCorrection$temp is in "saved" position (i.e. %%2 == 1)
			# and delete the data if validCorrection$temp is in the delete position (i.e. %%2 == 0)
			##############################################################################################
			observe({
				if(!is.null(input$validDrift)){
					if(!is.null(validCorrection$temp)){
						if((validCorrection$temp%%2) == 1 & input$validDrift > 0){
							
							currentProject()$setflagMachineCorrection(x = 1)
							
							machineCorrection$temp <- vapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
								
								if(is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[x]))))){
									FALSE
								}else{
									eval(parse(text = paste0("input$",geneRMachineCorr$temp[x])))
								}
								
							}, FUN.VALUE = logical(1))
							
							currentProject()$setCorrection(x = machineCorrection$temp)
							
						}  else {}
						if((validCorrection$temp%%2) == 0 & input$validDrift > 0){
							currentProject()$setflagMachineCorrection(x = 0)
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){currentProject()$set_flagRealign(replicate = x, type = "transect",value = 0)})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
								lapply(seq(from = 1, to = length(currentProject()$flag_Sample[[x]]), by = 1), function(i){
									currentProject()$setflagSample(sample = x, replicate = i, value = 0)
								})
							})
							lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
								currentProject()$samples[[x]]$setrep_type2(NA)
							})
							flagSampleDetail$temp <- currentProject()$flag_Sample
							flagRealign$temp <- currentProject()$flagRealign
							
						}  else {}
					} else {}
				} else {}
			}) #observe
			
		}
		
		##############################################
		##############################################
		##### Sample reduction #######################
		##############################################
		##############################################
		{
			TempS <-  reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$binsSample[1]
			Temp0S <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$binsSample[2]
			Temp1S <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$platSample[1]
			Temp2S <- reactiveValues(t = NULL)  # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$platSample[2]
			dataPlot2Sample <- reactiveValues(datS = NULL)   # a matrix corresponding to the Reduced
			
			currentSampleData <- reactiveValues(temp = NULL)
			currentSampleRep <- reactiveValues(temp = NULL)
			currentSampleNumberRep <- reactiveValues(temp = NULL)
			currentSampleNumberSam <- reactiveValues(temp = NULL)
			
			BAV_Sample <- reactiveValues(temp = 0)
			LOD_Sample <- reactiveValues(temp = 0)
			
			######################################################################################
			# Create currentSampleNumberRep$temp, i.e. the number of the sample currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject())){
					if(!is.null(input$SampleIn)){
						currentSampleNumberRep$temp <- match(input$SampleIn,currentProject()$samplesFiles)
						
					}
				}
			})
			
			######################################################################################
			# Create currentSampleNumberSam$temp, i.e. the number of the sample currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$SampleIn) & !is.null(input$SampleIn2)){
					if(!is.null(match(input$SampleIn,currentProject()$samplesFiles))){
						if(length(match(input$SampleIn,currentProject()$samplesFiles)) != 0){
							currentSampleNumberSam$temp <-  match(input$SampleIn2,currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)
						} else {}
					} else {}
				} else {}
			})
			
			######################################################################################
			# Create currentSampleRep$temp, i.e. the name of the sample currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$SampleIn) & !is.null(input$SampleIn2)){
					if(!is.null(match(input$SampleIn,currentProject()$samplesFiles))){
						if(length(match(input$SampleIn,currentProject()$samplesFiles)) != 0){
							if(!currentProject()$is.integer0(match(input$SampleIn2,currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files))){
								currentSampleRep$temp <-  currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[match(input$SampleIn2,currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]	
							} else {}
						} else {}
					} else {}
				} else {}
			})
			
			######################################################################################
			# Create currentSampleData$temp, i.e. the data of the sample currently reduced
			######################################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$SampleIn) & !is.null(input$SampleIn2)){
					if(!is.null(match(input$SampleIn,currentProject()$samplesFiles))){
						if(length(match(input$SampleIn,currentProject()$samplesFiles)) != 0){
							if(!currentProject()$is.integer0(match(input$SampleIn2,currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files))){
								currentSampleData$temp <-  currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[match(input$SampleIn2,currentProject()$samples[[match(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data 
							} else {}
						} else {}
					} else {}
				} else {}
			})
			
			################################################################
			# create signiS, i.e. the information about the values below BAV
			################################################################
			observe({
				validCorrection$temp
				output$signiS <- renderUI({
					
					if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
						div(
							div(style = "display: inline-block;",
							    div(p(paste0("B.A.V.*: ", " ",  " ",round(BAV_Sample$temp[grep(input$listeElemSample, names(BAV_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px;"),
							    div(p(paste0("L.O.D.**: ", " ",  " ",round(LOD_Sample$temp[grep(input$listeElemSample, names(LOD_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							div(style = "display: inline-block; vertical-align: top; margin-top: 10px",
							    div(style = "width:70px; vertical-align:top; margin-left: 50px; height: 30px; display: inline-block; background-color: rgba(232,26,29,0.5); border-style:solid; border-top: dotted 1px rgb(232,26,29); border-bottom: white; border-right: white; border-left:white"),
							    div(p("Under B.A.V."), style = "display: inline-block; margin-left:10px;")
							),
							br(),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
						
					} else if(input$CourbeSample == "Blank removed"){
						div(
							div(style = "display: inline-block;",
							    div(p(paste0("B.A.V.*: ", " ",  " ", round(BAV_Sample$temp[grep(input$listeElemSample, names(BAV_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px;"),
							    div(p(paste0("L.O.D.**: ", " ",  " ", round(LOD_Sample$temp[grep(input$listeElemSample, names(LOD_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							div(style = "display: inline-block; vertical-align: top; margin-top: 10px",
							    div(style = "width:70px; vertical-align:top; margin-left: 50px; height: 30px; display: inline-block; background-color: rgba(232,26,29,0.5); border-style:solid; border-top: dotted 1px rgb(232,26,29); border-bottom: white; border-right: white; border-left:white"),
							    div(p("Under L.O.D."), style = "display: inline-block; margin-left:10px;")
							),
							br(),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
					} else {
						div(
							div(
								div(p(paste0("B.A.V.*: ", " ",  " ",round(BAV_Sample$temp[grep(input$listeElemSample, names(BAV_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px;"),
								div(p(paste0("L.O.D.**: ", " ",  " ",round(LOD_Sample$temp[grep(input$listeElemSample, names(LOD_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
							),
							br(),
							p("* Blank averaged value ** Limit of detection", style = "margin-left:20px")
						)
					}
				})
			})
			
			################################################################
			#set elemUsed$temp, i.e. the element chosen for the graphic
			################################################################
			observe({
				if(!is.null(input$selectallS)){
					if(is.null(input$checkGroupS)){
						elemUsed$temp <- ElemStand$temp
					}else{
						if(input$selectallS%%2 == 0 & length(input$checkGroupS) != length(currentProject()$listeElem)){
							isolate({
								elemUsed$temp <- input$checkGroupS
								
							})
						} else {}
					} 
				} else {}
			}) #observe
			
			################################################################
			# set output$sample1
			# set output$sample2, i.e. selectInput of the sample reduced
			# set output$sample3, i.e. selectInput of the repliacte reduced
			################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$validDrift) & !is.null(input$saveNists) & !is.null(input$SuppDonne)){
					if(!is.na(currentProject()$flagMachineCorrection)){					
						if(currentProject()$flagMachineCorrection == 1){
							
							output$sample1 <- renderUI({
								div(class = "class4",
								    div(h3("Step 4. Sample sample reducing"), style = "display: inline-block;")
								)
							}) # eo output$sample1
							
							output$sample2 <- renderUI({
								div(
									p(icon("eye"),"Select sample"),
									div(selectInput("SampleIn", "",  as.matrix(currentProject()$samplesFiles), selected = as.matrix(currentProject()$samplesFiles)[1], multiple = FALSE, width = '100%'), style = "margin-top: -20px")
									
								)
							}) # eo output$sample2
							
							output$sample3 <- renderUI({
								
								if(!is.null(input$SampleIn) & !is.null(currentSampleNumberRep$temp)){
									if(!is.null(currentSampleNumberRep$temp) & !is.null(currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)){
										
										if(length(currentSampleNumberRep$temp) != 0){
											div(class = "class1",
											    p(icon("eye"),"Select sample sample"),
											    div( selectInput("SampleIn2", "", as.matrix(currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files), multiple = FALSE, width = '100%' ), style = "margin-top: -20px")
											    
											)	
										} else {}
									} else {}
								} else {}
								
							})  # eo output$sample3
							
						}else{
							output$sample1 <- renderUI({NULL}) # eo output$sample1
							
							output$sample2 <- renderUI({NULL})  # eo output$sample2
							
							output$sample3 <- renderUI({NULL}) # eo output$sample3
							
							
						} 
						
					} else {}
					
				} else {}
			}) # observe
			
			################################################################
			# set output$sample4, i.e. save button
			# set output$sample5, i.e. div with distPlotSample
			# set output$distPlotSample, i.e. raw data
			# set output$distPlot2Sample, i.e. reduced data
			################################################################
			observe({
				input$validDrift
				input$saveNists
				input$SuppDonne
				if(!is.null(currentProject()) & !is.null(input$SampleIn) & !is.null(input$SampleIn2) & !is.null(flagSampleDetail$temp)){
					if(!is.na(currentProject()$flagMachineCorrection)){
						
						if(currentProject()$flagMachineCorrection == 1){
							
							if(length(currentSampleRep$temp) != 0 & length(currentSampleNumberRep$temp) != 0){
								
								if(length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) == 0){
									
									output$sample4 <- renderUI({NULL}) # eo input$Sample4
									
									output$Sample5 <- renderUI({p("Loading data") }) # eo input$Sample5
									
									output$distPlotSample <- renderPlot({}) # eo input$distPlotSample
									
									output$distPlot2Sample <- renderPlot({})# eo input$distPlot2Sample
									
								}else{
									if(flagSampleDetail$temp[[currentSampleNumberRep$temp]][grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)] == 0){
										
										output$sample4 <- renderUI({
											actionButton("saveSample", "Save")
										}) # eo input$Sample4
										
										if(currentProject()$ChoiceUserCorr == TRUE & length(which(currentProject()$machineCorrection == TRUE)) != 0) {
											
											output$Sample5 <- renderUI({
												
												if(length(currentSampleRep$temp) != 0 & length(grep(input$SampleIn2,currentProject()$samples[[currentSampleNumberRep$temp]]$rep_Files)) != 0){
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													value1S <- currentProject()$detectBlank(currentSampleData$temp, col = grep(currentProject()$elemStand, colnames(currentSampleData$temp)))
													value2S <- currentProject()$detectPlateau(currentSampleData$temp, col = grep(currentProject()$elemStand, colnames(currentSampleData$temp)))
													step <- currentProject()$samples[[currentSampleNumberRep$temp]]$setRep_pas()
													
													fluidRow(
														column(8,style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11,
															 			 	 div(style="height: 82px",
															 			 	     sliderInput("binsSample","Background limits", value = value1S, min = minBS, max = maxBS, step = step, width = '100%', round = TRUE)
															 			 	 ),
															 			 	 div(style="height: 27px",
															 			 	     sliderInput("platSample","Plateau limits", value = value2S, min = minPS, max = maxPS,step = step, width = '100%')
															 			 	 )
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 	)
															 )
															 
														),
														column(4,style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
															 
														)
														
													)
												} else {}
												
											}) # eo input$Sample5
											
										} else {
											
											output$Sample5 <- renderUI({
												
												if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													value1S <- currentProject()$detectBlank(currentSampleData$temp, col = grep(currentProject()$elemStand, colnames(currentSampleData$temp)))
													value2S <- currentProject()$detectPlateau(currentSampleData$temp, col = grep(currentProject()$elemStand, colnames(currentSampleData$temp)))
													step <- currentProject()$samples[[currentSampleNumberRep$temp]]$setRep_pas()
													
													fluidRow(
														column(8,style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11,
															 			 	 div(style="height: 82px",
															 			 	     sliderInput("binsSample","Background limits", value = value1S, min = minBS, max = maxBS, step = step, width = '100%', round = TRUE)
															 			 	 ),
															 			 	 div(style="height: 27px",
															 			 	     sliderInput("platSample","Plateau limits", value = value2S, min = minPS, max = maxPS,step = step, width = '100%')
															 			 	 )
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 	)
															 )
															 
														),
														column(4,style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
															 
														)
														
													)
												}
												
											}) # eo input$Sample5
										}
										
										output$distPlotSample <- renderPlot({
											
											par(mar = c(3,3.5,1.75,0))
											
											if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
												
												maxY <- max(currentSampleData$temp, na.rm = TRUE)
												
												minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
												maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
												
												plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
												mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
												mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
												
												if(length(input$checkGroupS) > 1){
													lapply(seq(from = 2, to = length(input$checkGroupS), by = 1), function(x){
														par(new = TRUE)
														plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
													})
												}  else {}
												
												if(!is.null(input$binsSample) & !is.null(input$platSample)){
													if(is.na(input$binsSample[1]) | is.na(input$binsSample[2])| is.na(input$platSample[1]) | is.na(input$platSample[2])){}
													else{
														TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[1])[[2]]
														Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[2])[[2]]
														Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[1])[[2]]
														Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[2])[[2]]
													}
												}
												
												if(!is.null(Temp2S$t)){
													rect(currentSampleData$temp[TempS$t,1], -maxY,currentSampleData$temp[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
													rect(currentSampleData$temp[Temp1S$t,1],-maxY,currentSampleData$temp[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
													
													abline(v = currentSampleData$temp[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
													abline(v = currentSampleData$temp[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
													abline(v = currentSampleData$temp[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
													abline(v = currentSampleData$temp[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
													
													lapply(input$checkGroupS, function(x){points(currentSampleData$temp[TempS$t,1], currentSampleData$temp[TempS$t,x],  cex = 3, col ="grey")})
													lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,x], cex = 3, col ="grey")})
													lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
													lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
												} else {}
												
											}
											
										}, height = 400) # eo input$distPlotSample
										
										output$distPlot2Sample <- renderPlot({
											currentProject()$valRemplace
											BAV_Sample$temp
											LOD_Sample$temp
											if(is.null(dataPlot2Sample$datS)){
												
											}else{
												if(length(which(!is.na(dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												} else{
													
													par(mar = c(3.5,3.7,1.75,1))
													plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste0("Data ",input$CourbeSample),side=3,line=0.75, cex=1.2, font = 2)
													
													if(length(currentSampleRep$temp) != 0){
														if(length(currentSampleRep$temp) != 0){
															if(length(currentSampleRep$temp$BlankAverarge) == 1){
																if(!is.na(currentSampleRep$temp$BlankAverarge)){
																	if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																		abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeSample == "Blank removed"){
																		abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																	abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeSample == "Blank removed"){
																	abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
														} else {}
													} else {}
													
												}
											}
										})# eo input$distPlot2Sample
										
									} else {}
									if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 0 & flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp] != 0){
										
										output$sample4 <- renderUI({
											actionButton("saveSample", "Save")
										})  # eo input$Sample4
										
										if(currentProject()$ChoiceUserCorr == TRUE & length(which(currentProject()$machineCorrection == TRUE)) != 0) {
											
											output$Sample5 <- renderUI({
												
												if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													value1S <- currentSampleRep$temp$bins
													value2S <- currentSampleRep$temp$plat
													step <- currentProject()$samples[[currentSampleNumberRep$temp]]$setRep_pas()
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11,
															 			 	 div(style="height: 82px",
															 			 	     sliderInput("binsSample","Background limits", value = value1S, min = minBS, max = maxBS, step = step, width = '100%', round = TRUE)
															 			 	 ),
															 			 	 div(style="height: 27px",
															 			 	     sliderInput("platSample","Plateau limits", value = value2S, min = minPS, max = maxPS,step = step, width = '100%')
															 			 	 )
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 	)
															 )
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
														)
														
													)
													
												}
												
											}) # eo input$Sample5
											
										} else {
											
											output$Sample5 <- renderUI({
												
												input$SampleIn
												input$SampleIn2
												
												if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){ 
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													value1S <- currentSampleRep$temp$bins
													value2S <- currentSampleRep$temp$plat
													step <- currentProject()$samples[[currentSampleNumberRep$temp]]$setRep_pas()
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11,
															 			 	 div(style="height: 82px",
															 			 	     sliderInput("binsSample","Background limits", value = value1S, min = minBS, max = maxBS, step = step, width = '100%', round = TRUE)
															 			 	 ),
															 			 	 div(style="height: 27px",
															 			 	     sliderInput("platSample","Plateau limits", value = value2S, min = minPS, max = maxPS,step = step, width = '100%')
															 			 	 )
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			         
															 			     )
															 			 )
															 		)
															 	)
															 )
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
														)
														
													)
													
												}
												
												
											}) # eo input$Sample5
										}
										
										output$distPlotSample <- renderPlot({
											
											par(mar = c(3,3.5,1.75,0))
											
											if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
												
												maxY <- max(currentSampleData$temp, na.rm = TRUE)
												
												minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
												maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
												
												plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
												mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
												mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
												
												if(length(input$checkGroupS) > 1){
													
													lapply(seq(from = 2, to = length(input$checkGroupS), by = 1), function(x){
														par(new = TRUE)
														plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
													})
													
												} else {}
												
												if(is.null(input$binsSample)){
												}else{
													TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[1])[[2]]
													Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$binsSample[2])[[2]]
													Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[1])[[2]]
													Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = input$platSample[2])[[2]]
												}
												
												rect(currentSampleData$temp[TempS$t,1], -maxY,currentSampleData$temp[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
												rect(currentSampleData$temp[Temp1S$t,1],-maxY,currentSampleData$temp[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
												
												abline(v = currentSampleData$temp[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleData$temp[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleData$temp[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												abline(v = currentSampleData$temp[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[TempS$t,1], currentSampleData$temp[TempS$t,x],  cex = 3, col ="grey")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,x], cex = 3, col ="grey")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
											} else {}
											
										}, height = 400) # eo input$distPlotSample
										
										output$distPlot2Sample <- renderPlot({
											currentProject()$valRemplace
											BAV_Sample$temp
											LOD_Sample$temp
											if(!is.null(dataPlot2Sample$datS)){
												
												if(length(which(!is.na(dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												} else{
													par(mar = c(3.5,3.7,1.75,1))
													plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste0("Data ",input$CourbeSample),side=3,line=0.75, cex=1.2, font = 2)
													
													if(length(currentSampleRep$temp) != 0){
														if(length(currentSampleRep$temp) != 0){
															if(length(currentSampleRep$temp$BlankAverarge) == 1){
																if(!is.na(currentSampleRep$temp$BlankAverarge)){
																	if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																		abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeSample == "Blank removed"){
																		abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																	abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeSample == "Blank removed"){
																	abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
														} else {}
													} else {}
													
												}
											} else {}
											
										})# eo input$distPlot2Sample
										
										
									}  else {}
									if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 1){
										
										currentProject()$setflagSample(sample = currentSampleNumberRep$temp, replicate = currentSampleNumberSam$temp, value = 1)
										
										output$sample4 <- renderUI({
											actionButton("saveSample", "Delete")
										}) # eo input$Sample
										
										if(currentProject()$ChoiceUserCorr == TRUE & length(which(currentProject()$machineCorrection == TRUE)) != 0) {
											
											output$Sample5 <- renderUI({
												
												if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			     )
															 			 )
															 		)
															 	)
															 )
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration", "Conc. corrected"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
														)
														
													)
													
												} else {}
												
											}) # eo input$Sample5
											
										} else {
											output$Sample5 <- renderUI({
												
												if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
													
													minBS <- currentSampleData$temp[1,1]
													maxBS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													minPS <- currentSampleData$temp[1,1]
													maxPS <- currentSampleData$temp[dim(currentSampleData$temp)[1],1]
													
													fluidRow(
														column(8, style  = "padding-right: 5px",
															 box(
															 	title = list(icon("share"),"Blank and plateau limits selection"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	height = "640px",
															 	fluidRow(
															 		column(10,
															 			 plotOutput("distPlotSample"),
															 			 column(1),
															 			 column(11
															 			 	 
															 			 	 
															 			 )
															 		),
															 		column(2,
															 			 div(style = "height: 10px",
															 			     actionLink("selectallS","Select All"),
															 			     div(style ="",
															 			         div(style = "height:100px; width: 22px; text-align: center;display: inline-block; vertical-align: top;",
															 			             div(style = "height:6px; width: 20px;"),
															 			             lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
															 			             	eval(parse(text = paste0("div(align='center', style = 'width:25px; height:25px',div(style = 'height:10px;'),div(style = 'background-color:", color$temp[x], ";width:10px; height:10px; border-radius: 50%;'))")))
															 			             	
															 			             })
															 			             
															 			         ),
															 			         div(checkboxGroupInput("checkGroupS", label = "", choices = currentProject()$listeElem, selected = elemUsed$temp), style = "display: inline-block; width: 40px; vertical-align: top;")
															 			     )
															 			 )
															 		)
															 	)
															 )
														),
														column(4, style  = "padding-left: 5px",
															 box(
															 	title = list(icon("share"),"Reduced data verification"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	column(6, class = "class1",
															 		 p(icon("cubes"),"Element plotted"),
															 		 selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = currentProject()$elemStand, width = '100%')
															 	),
															 	column(6, class = "class1",
															 		 p(icon("area-chart"),"Curve plotted"),
															 		 selectInput("CourbeSample", label = "", choices =  c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), selected  = "Plateau", width = '100%')
															 	),
															 	div(plotOutput("distPlot2Sample", height = '350px'), style = "height:430px"),
															 	uiOutput("signiS")
															 ),
															 box(
															 	title = list(icon("floppy-o"),"Graphic export"),
															 	status="info",
															 	solidHeader = TRUE,
															 	width = "100%",
															 	collapsible = TRUE,
															 	collapsed = TRUE,
															 	selectizeInput("ElementToExportS", label = "Element(s) to export",
															 			   choices = currentProject()$listeElem,
															 			   selected = currentProject()$listeElem, multiple = TRUE),
															 	checkboxGroupInput("courveToExportS", label = "Curve(s) to export",
															 				 choices = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"),
															 				 selected = c("Blank","Raw", "Plateau","Blank removed","> LOD", "Normalized","Concentration"), inline = TRUE),
															 	div(actionButton("ExportGraphS","Export graphics"), align="center")
															 )
														)
														
													)
													
												} else {}
												
											}) # eo input$Sample5
										}
										
										output$distPlotSample <- renderPlot({
											
											par(mar = c(3,3.5,1.75,0))
											
											if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
												
												maxY <- max(currentSampleData$temp, na.rm = TRUE)
												
												minX <- min(currentSampleData$temp[,1], na.rm = TRUE)
												maxX <- max(currentSampleData$temp[,1], na.rm = TRUE)
												
												plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
												mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
												mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
												mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
												
												if(length(input$checkGroupS) > 1){
													
													lapply(seq(from = 2, to = length(input$checkGroupS), by = 1), function(x){
														par(new = TRUE)
														plot(currentSampleData$temp[,1], currentSampleData$temp[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
													})
												} else {}
												
												TempS$t  <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[1])[[2]]
												Temp0S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$bins[2])[[2]]
												Temp1S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[1])[[2]]
												Temp2S$t <- currentProject()$closest(x = currentSampleData$temp[,1],y = currentSampleRep$temp$plat[2])[[2]]
												
												rect(currentSampleRep$temp$bins[1],-maxY,currentSampleRep$temp$bins[2],(1+10/100)*maxY, col = "#8B735564", border = NA)
												rect(currentSampleRep$temp$plat[1],-maxY,currentSampleRep$temp$plat[2],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
												
												abline(v = currentSampleRep$temp$bins[1], lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleRep$temp$bins[2], lty = "dashed", col = ("grey"), lwd = 2)
												abline(v = currentSampleRep$temp$plat[1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												abline(v = currentSampleRep$temp$plat[2], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
												
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[TempS$t,1],  currentSampleData$temp[TempS$t,x],  cex = 3, col ="grey")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp0S$t,1], currentSampleData$temp[Temp0S$t,x], cex = 3, col ="grey")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp1S$t,1], currentSampleData$temp[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
												lapply(input$checkGroupS, function(x){points(currentSampleData$temp[Temp2S$t,1], currentSampleData$temp[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
											} else {}
										}) # eo input$distPlotSample
										
										output$distPlot2Sample <- renderPlot({
											currentProject()$valRemplace
											BAV_Sample$temp
											LOD_Sample$temp
											if(!is.null(dataPlot2Sample$datS)){
												if(length(which(!is.na(dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))]))) == 0){
													plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
													text(1,0.5, labels = "No data different from NA", cex = 2)
												}else{
													par(mar = c(3.5,3.7,1.75,1))
													plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "", xlab = "", main = "")
													mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
													mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
													mtext(paste0("Data ",input$CourbeSample),side=3,line=0.75, cex=1.2, font = 2)
													
													if(length(currentSampleRep$temp) != 0){
														if(length(currentSampleRep$temp) != 0){
															if(length(currentSampleRep$temp$BlankAverarge) == 1){
																if(!is.na(currentSampleRep$temp$BlankAverarge)){
																	if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																		abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																	} else if(input$CourbeSample == "Blank removed"){
																		abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																		rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																	} else {}
																} else {}
															} else {
																if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
																	abline(a = currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$BlankAverarge[grep(input$listeElem, names(currentSampleRep$temp$BlankAverarge))], col = "#FF000064", border = NA)
																} else if(input$CourbeSample == "Blank removed"){
																	abline(a = currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
																	rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentSampleRep$temp$LOD[grep(input$listeElem, names(currentSampleRep$temp$LOD))], col = "#FF000064", border = NA)
																} else {}
															}
															
														}
														
													} else {}
												}
											} else {}
										})# eo input$distPlot2Sample
										
									}  else {}
									
								}
							} else {}
							
							
						}else{
							output$sample4 <- renderUI({NULL}) # eo input$Sample
							
							output$Sample5 <- renderUI({NULL})  # eo input$Sample5
							
							output$signiS <- renderUI({NULL})
							
							output$distPlotSample <- renderPlot({NULL}) # eo input$distPlotSample
							
							output$distPlot2Sample <- renderPlot({NULL}) # eo input$distPlot2Sample
						}
					} else {}
				} else {}
			}) # observe
			
			################################################################
			# calculate and render dataPlot2Sample$datS
			################################################################
			observe({
				currentProject()$valRemplace
				input$validDrift
				input$saveNists
				input$SuppDonne
				if(!is.null(currentProject())){
					
					if(!is.null(input$SampleIn) & !is.null(input$SampleIn2) & !is.null(input$CourbeSample) & !is.null(flagSampleDetail$temp) & !currentProject()$is.integer0(currentSampleNumberRep$temp) & !currentProject()$is.integer0(currentSampleNumberSam$temp)){
						
						if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0 & !is.na(currentSampleNumberRep$temp)){
							
							if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 0){
								
								if(!is.null(input$binsSample) & !is.null(input$platSample) & !is.null(TempS$t) & !is.null(Temp0S$t) & !is.null(Temp1S$t) & !is.null(Temp2S$t)){
									if(is.finite(TempS$t)){
										
										meanStandTable <- currentProject()$standards[[1]]$rep_dataFinale
										
										LineNumber <- nrow(currentProject()$standards[[1]]$rep_dataFinale)-1
										
										meanStand <- meanStandTable[LineNumber, ]
										
										dataPlot2Sample$datS <- currentSampleRep$temp$getData(curve = input$CourbeSample, 
																			bins = c(TempS$t, Temp0S$t), 
																			plat = c(Temp1S$t,Temp2S$t), 
																			name = input$SampleIn2, 
																			meanStand = currentProject()$standards[[1]]$rep_dataFinale,
																			rankSample = currentProject()$sampleRank, 
																			rankStandard = currentProject()$standardRank,
																			model = currentProject()$regressionModel, 
																			calibFile = currentProject()$EtalonData, 
																			correction = currentProject()$machineCorrection, 
																			rempl = currentProject()$valRemplace, 
																			threshold = currentProject()$R2Threshold
										)
										BAV_Sample$temp <- currentSampleRep$temp$BlankAverarge
										LOD_Sample$temp <- currentSampleRep$temp$LOD
									} else {}
								}
							} else {}
							if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 1){
								if(!is.null(TempS$t)){
									if(is.finite(TempS$t)){
										dataPlot2Sample$datS <- currentSampleRep$temp$renderData(curve = input$CourbeSample)
										BAV_Sample <- currentSampleRep$temp$BlankAverarge
										LOD_Sample <- currentSampleRep$temp$LOD
									} else {}
								}
							} else {}
							
							
						} else {}
					}else {}
				}else {}
			}) # observe
			
			################################################################
			# updateCheckboxGroupInput of checkGroupS
			################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$validDrift) & !is.null(input$saveNists) & !is.null(input$SuppDonne) & !is.null(input$selectallS)){
					if(input$selectallS == 0) {
						return(NULL)
					}else if(input$selectallS%%2 == 0){
						updateCheckboxGroupInput(session,"checkGroupS","",choices=currentProject()$listeElem, selected = currentProject()$listeElem)
					}else{
						updateCheckboxGroupInput(session,"checkGroupS","",choices=currentProject()$listeElem,selected=currentProject()$listeElem)
					}
				} else {}
			}) # observe
			
			################################################################
			# set flagSampleDetail when input$saveSample is pressed
			################################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$validDrift) & !is.null(input$saveNists) & !is.null(input$SuppDonne) & !is.null(input$saveSample)){
					if(length(currentSampleRep$temp) != 0){
						if(input$saveSample > 0){
							isolate({
								flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp] <- flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp] + 1
								updateSelectInput(session, "listeElemSample", selected = input$listeElemSample)
								updateSelectInput(session, "CourbeSample", selected = input$CourbeSample)
								loadS$temp <- loadS$temp +1
							}) # eo isolate
						} else {}
					} else {}
				} else {}
			}) # observe
			
			################################################################
			# to avoid elementR saving data when first delete loaded project
			################################################################
			observe({
				if(!is.null(input$SampleIn2)){
					isolate({
						if(loadS$temp == 1){
							loadS$temp <- loadS$temp +1
						} else {}
					}) # eo isolate
				} else {}
			}) #observe
			
			################################################################
			# Observe to :
			# 	save the data when flagSampleDetail is in the save position
			# 	delete the data when the flag is in the delete position
			################################################################
			observe({
				input$validDrift
				input$saveNists
				input$SuppDonne
				input$binsSample
				input$platSample
				if(!is.null(currentProject()) & !is.null(input$SampleIn) & !is.null(input$SampleIn2) & !is.null(input$saveSample) & !is.null(TempS$t) & !is.null(Temp0S$t) & !is.null(Temp1S$t) & !is.null(Temp2S$t)){
					if(!currentProject()$is.integer0(currentSampleNumberRep$temp) & !currentProject()$is.integer0(currentSampleNumberSam$temp)){
						if(length(currentSampleRep$temp) != 0 & length(currentSampleRep$temp) != 0){
							
							if(projChar$temp[1] == 2 & loadS$temp == 1){
								
								if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 0){
									isolate({
										currentProject()$setflagSample(sample = currentSampleNumberRep$temp, replicate = currentSampleNumberSam$temp, value = 0)
										currentProject()$set_flagRealign(replicate = currentSampleNumberRep$temp, type = "spot", value = 0)
										currentProject()$set_flagRealign(replicate = currentSampleNumberRep$temp, type = "transect", value = 0)
										lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
											currentProject()$samples[[x]]$setrep_type2(NA)
										})
										flagRealign$temp <- currentProject()$flagRealign
										currentProject()$set_summarySettings(name = input$SampleIn2, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = rep(NA, length(currentProject()$listeElem)), LOD = rep(NA, length(currentProject()$listeElem)))
									})
								} else {}
								
								if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 1 & is.finite(TempS$t)){
									isolate({
										currentProject()$setflagSample(sample = currentSampleNumberRep$temp, replicate = currentSampleNumberSam$temp, value = 1)
										currentSampleRep$temp$setBins(bins = c(currentSampleData$temp[TempS$t,1], currentSampleData$temp[Temp0S$t,1]))
										currentSampleRep$temp$setPlat(plat = c(currentSampleData$temp[Temp1S$t,1],currentSampleData$temp[Temp2S$t,1]))
										currentSampleRep$temp$setDataConcCorr(bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), 
																  name = input$SampleIn2,
																  meanStand = currentProject()$standards[[1]]$rep_dataFinale,
																  rankSample = currentProject()$sampleRank, 
																  rankStandard = currentProject()$standardRank,
																  model = currentProject()$regressionModel, 
																  calibFile = currentProject()$EtalonData,
																  correction = currentProject()$machineCorrection, 
																  rempl = currentProject()$valRemplace, 
																  threshold = currentProject()$R2Threshold)
										
										if(currentProject()$ChoiceUserCorr == FALSE) {
											currentSampleRep$temp$reset()
										} else {}
										currentProject()$set_summarySettings(name = input$SampleIn2, rank = currentProject()$sampleRank[which(names(currentProject()$sampleRank) == input$SampleIn2)], bins1 = currentSampleData$temp[TempS$t,1], bins2 = currentSampleData$temp[Temp0S$t,1], plat1 = currentSampleData$temp[Temp1S$t,1], plat2 = currentSampleData$temp[Temp2S$t,1], average = currentSampleRep$temp$BlankAverarge, LOD = currentSampleRep$temp$LOD)
									})
								} else {}
								
								
							} else {
								if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 0 & input$saveSample > 0){
									isolate({
										currentProject()$setflagSample(sample = currentSampleNumberRep$temp, replicate = currentSampleNumberSam$temp, value = 0)
										currentProject()$set_flagRealign(replicate = currentSampleNumberRep$temp, type = "spot", value = 0)
										currentProject()$set_flagRealign(replicate = currentSampleNumberRep$temp, type = "transect", value = 0)
										flagRealign$temp <- currentProject()$flagRealign
										currentProject()$set_summarySettings(name = input$SampleIn2, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = rep(NA, length(currentProject()$listeElem)), LOD = rep(NA, length(currentProject()$listeElem)))
										lapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1), function(x){
											currentProject()$samples[[x]]$setrep_type2(NA)
										})
									})
								} else {}
								
								if((flagSampleDetail$temp[[currentSampleNumberRep$temp]][currentSampleNumberSam$temp]%%2) == 1 & is.finite(TempS$t) & input$saveSample > 0){
									isolate({
										
										currentProject()$setflagSample(sample = currentSampleNumberRep$temp, replicate = currentSampleNumberSam$temp, value = 1)
										currentSampleRep$temp$setBins(bins = c(currentSampleData$temp[TempS$t,1], currentSampleData$temp[Temp0S$t,1]))
										currentSampleRep$temp$setPlat(plat = c(currentSampleData$temp[Temp1S$t,1],currentSampleData$temp[Temp2S$t,1]))
										currentSampleRep$temp$setDataConcCorr(bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), 
																  name = input$SampleIn2,
																  meanStand = currentProject()$standards[[1]]$rep_dataFinale,
																  rankSample = currentProject()$sampleRank, 
																  rankStandard = currentProject()$standardRank,
																  model = currentProject()$regressionModel, 
																  calibFile = currentProject()$EtalonData, 
																  correction = currentProject()$machineCorrection, 
																  rempl = currentProject()$valRemplace, 
																  threshold =  currentProject()$R2Threshold)
										
										if(currentProject()$ChoiceUserCorr == FALSE) {
											
											currentSampleRep$temp$reset()
											
										} else {}
										
										currentProject()$set_summarySettings(name = input$SampleIn2, rank = currentProject()$sampleRank[which(names(currentProject()$sampleRank) == input$SampleIn2)], bins1 = currentSampleData$temp[TempS$t,1], bins2 = currentSampleData$temp[Temp0S$t,1], plat1 = currentSampleData$temp[Temp1S$t,1], plat2 = currentSampleData$temp[Temp2S$t,1], average = currentSampleRep$temp$BlankAverarge, LOD = currentSampleRep$temp$LOD)
										passageS <- names(currentProject()$flag_Sample[[currentSampleNumberRep$temp]] == 0)[currentProject()$flag_Sample[[currentSampleNumberRep$temp]] == 0][1]
										
										if(!is.na(passageS)){
											
											delay(2000,updateSelectInput(session, "SampleIn2", selected = passageS))
											
										} else {}
										
									})
								} else {}
								
							}
							
							
						} else {}
						
					} else {}
				} else {}
			}) # observe
			
		}
		
		##############################################
		##############################################
		##### sample replicate realignment ##########
		##############################################
		##############################################
		{
			deplace <- reactiveValues(val = NULL) # a vector of temporal shift of sample replicate
			deplace2 <- reactiveValues(val = NULL) # a vector of temporal shift of sample replicate
			tabProvSpot <- reactiveValues(temp = NULL) # a temporary matrix for calculation (spot mode)
			tabProvSample <- reactiveValues(temp = NULL) # a temporary list of data for calculation (transect mode)
			
			tabSpotDisplay <- reactiveValues(temp = NULL) # a matrix to display (spot mode) before validating the data
			tabSpotSave <- reactiveValues(temp = NULL)  # a matrix to save (spot mode) before validating the data
			
			ChosenElement <- reactiveValues(temp = NULL) # the element chosen for the realignement (transect mode)
			
			outlierValues <- reactiveValues(temp = NULL)
			outlierSuggested <- reactiveValues(temp = NULL)
			outlierChosen <- reactiveValues(temp = NULL)
			
			autoCorrel <- reactiveValues(temp = 1)
			
			########################################################
			# set flagSample$temp, i.e. the flag for realignment
			########################################################
			observe({
				if(!is.null(input$saveNists) & !is.null(currentProject()) & !is.null(input$saveSample) & !is.null(input$validDrift) & !is.null(input$SuppDonne)){
					flagSample$temp <- vapply(seq(from = 1, to = length(currentProject()$flag_Sample), by = 1),
									  function(x){
									  	if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
									  		return(1)
									  	}else{return(0)}
									  }, FUN.VALUE = numeric(1))
				} else {}
			}) # observe
			
			########################################################
			#  set output$realign1, i.e. the top div
			########################################################
			observe({
				if(length(which(flagSample$temp == TRUE)) != 0){
					
					output$realign1 <- renderUI({
						fluidRow(
							box(background = "yellow", width = 12, height = 85,
							    column(8,
							    	 div(h3("Step 5. Sample replicates averaging procedure"), style = "display: inline-block;")
							    ),
							    column(2,
							    	 selectInput("selectRealign",label = "", choices = currentProject()$samplesFiles[which(flagSample$temp == TRUE)], multiple = FALSE )
							    ),
							    column(2,
							    	 radioButtons("typeTraitement", label = "", choices = c("spot","transect"), inline = TRUE)
							    )
							)
						)
						
					}) # eo output$realign1
					
					
				}else{
					output$realign1 <- renderUI({NULL}) # eo output$realign1
					output$realign3 <- renderUI({NULL}) # eo output$realign3
					output$realign2 <- renderUI({NULL}) # eo output$realign2
				}
			}) # observe
			
			########################################################
			# set declare deplace$val, i.e. a vector of
			# the temporal translation for each replicate
			########################################################
			observe({
				if(length(which(flagSample$temp == TRUE)) != 0){
					temp <- rep(0,length(input$ReplicateSample))
					names(temp) <- input$ReplicateSample
					deplace$val <- temp
				}else{}
			}) # observe
			
			########################################################
			# set ChosenElement$temp
			########################################################
			observe({
				if(!is.null(input$elemRaster)){
					ChosenElement$temp <- input$elemRaster
				} else {}
			}) # observe
			
			###############################################################
			# set output$realign5, i.e. the table or the plot of the page
			###############################################################
			output$realign5 <- renderUI({
				
				if(!is.null(input$selectRealign)){
					if(!is.null(input$typeTraitement) & length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						
						if(input$typeTraitement == "transect"){
							
							if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
								div(
									box(
										
										solidHeader = TRUE,
										status = "warning",
										width = 12,
										style = "margin-top: 10px",
										plotOutput("plotRealign")
									)
								)
							} else if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 2 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
								
								div(
									box(
										
										solidHeader = TRUE,
										status = "warning",
										width = 12,
										style = "margin-top: 10px",
										plotOutput("plotRealign")
									)
								)
							}else if(((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3) & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
								
								div(
									box(
										
										solidHeader = TRUE,
										status = "warning",
										width = 12,
										style = "margin-top: 10px",
										plotOutput("plotRealign")
									)
								)
							} else if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1){
								div(
									style='overflow-y: hidden',
									dataTableOutput("realign4")
								)
							} else {}
							
							
						} else if(input$typeTraitement == "spot"){
							
							if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
								
								div(
									style='overflow-y: hidden',
									dataTableOutput("realign4")
								)
							} else if((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
								
								div(
									style='overflow-y: hidden',
									dataTableOutput("realign4")
								)
							} else if(((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3)){
								
								div(
									box(
										
										solidHeader = TRUE,
										status = "warning",
										width = 12,
										style = "margin-top: 10px",
										plotOutput("plotRealign")
									)
								)
							}
							
							
						} else {}
					}
				} else {}
				
			}) # eo output$realign5
			
			###############################################################
			# set output$plotRealign, i.e. the plot if type = transect
			###############################################################
			output$plotRealign <- renderPlot({
				if(!is.null(input$selectRealign) & !is.null(tabProvSample$temp)){
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & length(tabProvSample$temp) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						
						if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
							
							deplace$val
							
							par(mar = c(5.1,4.1,1.5,1.5))
							
							if(!is.null(input$ReplicateSample) & is.list(tabProvSample$temp) & length(which(is.element(input$ReplicateSample, names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre)) == FALSE)) == 0 & !is.null(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$elemRaster]})))){
								if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(tabProvSample$temp), by = 1), function(x){tabProvSample$temp[[x]][,input$elemRaster]}))))) == 0){
									plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
									text(1,0.5, labels = "No data different from NA for this element", cex = 2)
								} else {
									if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0){
										
										ylim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i){tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[i])]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i){tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[i])]][,input$elemRaster]})), na.rm = TRUE))
										
										xlim <- c(min(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i){tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[i])]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i){tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[i])]][,1]}))))
										
										temp <- lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i){tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[i])]][,1]})
										
										lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(x){
											
											if(length(which(names(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])) == 0){
												
											}else{
												plot(tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[x])]][,1],tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[x])]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = "", col = colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
												
												par(new = TRUE)
											}
											
										})
										
										legend("topright", legend = input$ReplicateSample, col = vapply(seq(from = 1, to = length(input$ReplicateSample), by = 1),
																				    function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]},
																				    FUN.VALUE = character(1)
										), pch = 1, lwd=1, lty=1)
										
										if(!is.null(outlierChosen$temp)){
											
											if(length(outlierChosen$temp[[grep(input$elemRaster, colnames(tabProvSample$temp[[1]]))]]) != 0){
												
												lapply(seq(from = 1, to = length(outlierChosen$temp[[grep(input$elemRaster, colnames(tabProvSample$temp[[1]]))]]), by = 1), function(x){
													
													points(names(outlierChosen$temp[[grep(input$elemRaster, colnames(tabProvSample$temp[[1]]))]])[x], outlierChosen$temp[[grep(input$elemRaster, colnames(tabProvSample$temp[[1]]))]][x], cex = 3, col ="deeppink")
													
												})
											}
											
											
											
										} else{}
									}
								}
							} else {}
							
						} else {}
						
						if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 2 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
							
							par(mar = c(5.1,4.1,1.5,1.5))
							
							if(!currentProject()$is.integer0(match(input$selectRealign,currentProject()$samplesFiles)) & is.list(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster) & !is.null(input$elemRaster)){
								if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
									plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
									text(1,0.5, labels = "No data different from NA for this element", cex = 2)
								}else{
									
									ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
									
									xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
									
									lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
										
										plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col =colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
										
										par(new = TRUE)
										
									})
									
									plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b", lwd = 2)
									
									if(!is.null(input$sliderAutoCorrel) & !is.null(input$diameter) & !is.null(input$speed)){
										k <- input$sliderAutoCorrel
									} else {
										k <- 0
									}
									
									for(j in seq(from = 1, to = nrow(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster), by = 1)){
										
										if(!is.null(input$sliderAutoCorrel) & !is.null(input$diameter) & !is.null(input$speed)){
											
											if(k %% ceiling(autoCorrel$temp) == 0){
												points(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[j,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[j,input$elemRaster], pch = 21, col = "red", cex = 1.5, bg = "red")
											} else {}
											
										} else {}
										
										
										k <- k+1
									}
									
									legend("topright", legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1),
																																					     function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																					     FUN.VALUE = character(1)
									), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
								}
							} else {}
							
						} else {}
						
						if(input$typeTraitement == "transect" &  ((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3) & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
							
							par(mar = c(5.1,4.1,1.5,1.5))
							
							if(!currentProject()$is.integer0(match(input$selectRealign,currentProject()$samplesFiles)) & is.list(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)){
								if(!is.null(input$elemRaster)){
									if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
										plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
										text(1,0.5, labels = "No data different from NA for this element", cex = 2)
									}else{
										
										ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
										
										xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
										
										lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
											
											plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col =colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
											
											par(new = TRUE)
											
										})
										
										plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b", lwd = 2)
										
										points(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRasterNonCorr[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRasterNonCorr[,input$elemRaster], pch = 21, col = "red", cex = 1.5, bg = "red")
										
										legend("topright", legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1),
																																						     function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																						     FUN.VALUE = character(1)
										), "black"), lty = 1, pch = c(rep(1, length(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])), NA), lwd = c(rep(1, length(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])), 2))
										
										
									}
								} else {}
							} else {}
						} else {}
						
						if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1){NULL}
						
						if(input$typeTraitement == "spot" & ((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3)){
							par(mar = c(5.1,4.1,1.5,1.5))
							
							if(!currentProject()$is.integer0(match(input$selectRealign,currentProject()$samplesFiles))){
								if(is.list(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)){
									if(!is.null(input$elemRaster)){
										if(length(which(!is.na(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
											plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
											text(1,0.5, labels = "No data different from NA for this element", cex = 2)
										}else{
											
											ylim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
											
											xlim <- c(min(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(i){currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
											
											lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1), function(x){
												
												plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col =colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
												
												par(new = TRUE)
												
											})
											
											plot(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b", lwd = 2)
											
											points(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRasterNonCorr[,1],currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRasterNonCorr[,input$elemRaster], pch = 21, col = "red", cex = 1.5, bg = "red")
											
											legend("topright", legend = c(names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(vapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), by = 1),
																																							     function(x){colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]},
																																							     FUN.VALUE = character(1)
											), "black"), lty = 1, pch = c(rep(1, length(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])), NA), lwd = c(rep(1, length(colorReplicate$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])), 2))
											
											
										}
									} else {}
								} else {}
							} else {}
						}
						
					} else {}
				} else {}
				
				
			})
			
			###############################################################
			# set output$realign4, i.e. the table if type = spot
			###############################################################
			output$realign4 <- renderDataTable({
				
				input$typeTraitement
				
				if(!is.null(input$selectRealign)){
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						
						if(input$typeTraitement == "spot" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
							return(tabSpotDisplay$temp)
							
						} else {}
						
						if(input$typeTraitement == "spot" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
							if(!currentProject()$is.integer0(match(input$selectRealign,currentProject()$samplesFiles)) & is.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot)){
								temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot
								
								temp <- format(temp, scientific=TRUE, digits = 2)
								
								temp <- cbind(rownames(temp), temp)
								
								return(temp)
							} else {}
						} else {}
						
						if(input$typeTraitement == "spot" & ((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3)){NULL} else {}
						
						if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1){
							if(!currentProject()$is.integer0(match(input$selectRealign,currentProject()$samplesFiles)) & is.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot)){
								temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot
								
								temp <- format(temp, scientific=TRUE, digits = 2)
								
								temp <- cbind(rownames(temp), temp)
								
								return(temp)
							} else {}
						}
					} else {}
				} else {}
			}
			, options = list(paging= FALSE, searching = FALSE, rowCallback = I(
				'function(row, data) {
				if (data[0].includes ("total"))
				$("td", row).css("background", "rgba(176, 205, 214, 0.9)");
				else if (data[0].includes ("SD"))
				$("td", row).css("background", "rgba(214, 185, 176, 0.9)");
				else if (data[0].includes ("Mean"))
				$("td", row).css("background", "rgba(212, 222, 20, 0.9)");
		}'
    			))
			) # eo output$realign4
			
			###############################################################
			# set output$realign2, i.e. the div of the board
			# set output$realign3, i.e. the div for export graphics
			###############################################################
			observe({
				input$ValiderSample
				input$SuppDonne
				input$validDonne
				input$validDonne2
				if(!is.null(currentProject()) & !is.null(input$typeTraitement) & !is.null(input$selectRealign) & !is.null(flagRealign$temp)){
					if(!is.null(match(input$selectRealign,currentProject()$samplesFiles))){
						if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
							isolate({
								if(input$typeTraitement == "spot" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
									
									currentProject()$set_flagRealign(replicate = match(input$selectRealign,currentProject()$samplesFiles), type = "spot", value = 0)
									
									output$realign3 <- renderUI({
										fluidRow(
											box(
												solidHeader = TRUE,
												status = "warning",
												width = 12,
												title = list(icon("spinner"), "Spot averaging"),
												style="padding-right: 0px",
												div(style = "",
												    div(style =  "width: 20px; height: 20px; background-color: rgba(212, 222, 20, 0.9); display: inline-block"),
												    div("Mean", style = "display: inline-block"),
												    div(style =  "width: 5px; height: 20px; display: inline-block"),
												    div(style =  "width: 20px; height: 20px; background-color: rgba(214, 185, 176, 0.9); display: inline-block"),
												    div("SD", style = "display: inline-block"),
												    div(style =  "width: 5px; height: 20px; display: inline-block"),
												    div(style =  "width: 20px; height: 20px; background-color: rgba(176, 205, 214, 0.9); display: inline-block"),
												    div("Averaged", style = "display: inline-block")
												),
												br(),
												h3("1. Replicates to average"),
												div(checkboxGroupInput("ReplicateSpot", label = "",
															     choices = as.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files),
															     selected = as.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)),style = "margin-top: -10px"),
												br(),
												p("Concentration displayed"),
												br(),
												column(2, actionButton("SauvegarderSpot", "Save averaging"))
											)
											
										)
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({NULL}) # eo output$realign2
									
									
								} else {}
								
								if(input$typeTraitement == "spot" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
									
									currentProject()$set_flagRealign(replicate = match(input$selectRealign,currentProject()$samplesFiles), type = "spot", value = flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1])
									currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2(x = "spot")
									
									output$realign3 <- renderUI({
										if(is.numeric(nrow(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot))){
											fluidRow(
												box(
													solidHeader = TRUE,
													status = "warning",
													width = 12,
													title = list(icon("spinner"), "Spot averaging"),
													style="padding-right: 0px",
													# br(),
													div(style = "",
													    div(style =  "width: 20px; height: 20px; background-color: rgba(212, 222, 20, 0.9); display: inline-block"),
													    div("Mean", style = "display: inline-block"),
													    div(style =  "width: 5px; height: 20px; display: inline-block"),
													    div(style =  "width: 20px; height: 20px; background-color: rgba(214, 185, 176, 0.9); display: inline-block"),
													    div("SD", style = "display: inline-block"),
													    div(style =  "width: 5px; height: 20px; display: inline-block"),
													    div(style =  "width: 20px; height: 20px; background-color: rgba(176, 205, 214, 0.9); display: inline-block"),
													    div("Averaged", style = "display: inline-block")
													),
													br(),
													p("Concentration displayed"),
													br(),
													column(2, actionButton("SauvegarderSpot", "Delete averaging"))
												)
												
											)
										} else {}
										
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({NULL}) # eo output$realign2
									
									
								} else {}
								
								if(input$typeTraitement == "spot" & ((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3)){
									
									output$realign3 <- renderUI({
										
										fluidRow(
											box(
												solidHeader = TRUE,
												status = "warning",
												width = 12,
												title = list(icon("spinner"), "Spot averaging (WARNING, already saved in transect procedure)"),
												selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp)
											)
											
										)
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({
										fluidRow(
											box(
												title = list(icon("floppy-o"),"Graphic export"),
												status = "warning",
												solidHeader = TRUE,
												collapsible = TRUE,
												collapsed = TRUE,
												width = 12,
												column(10, selectizeInput("RealignElementToExport", label = "Element(s) to export",
																  choices = currentProject()$listeElem,
																  selected = currentProject()$listeElem, multiple = TRUE)),
												column(2, br(), actionButton("RealignExportGraph","Export graphics"))
											)
										)
									}) # eo output$realign2
									
									
								} else {}
								
								if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
									
									currentProject()$set_flagRealign(replicate = match(input$selectRealign,currentProject()$samplesFiles), type = "transect", value = 0)
									
									output$realign3 <- renderUI({
										
										fluidRow(
											box(
												solidHeader = TRUE,
												status = "warning",
												width = 12,
												
												title = list(icon("area-chart"), "transect realignment"),
												# column(3,
												h3("1. Replicates to average"),
												div(checkboxGroupInput("ReplicateSample", label = "",
															     choices = as.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files),
															     selected = as.matrix(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)),
												    style = "margin-top: -10px"),
												h3("2. Outlier detection"),
												uiOutput("DetectOutlier"),
												uiOutput("PrintOutlier"),
												h3("3. Realignment of replicates"),
												selectInput("elemRaster","", choices = currentProject()$listeElem, selected = currentProject()$listeElem[1]),
												column(width = 6, actionButton("ReplicatCol", "This element")),
												column(width = 6, actionButton("ReplicatAll", "All elements")),
												br(),
												br(),
												br(),
												br(),
												uiOutput('replicates'),
												br(),
												actionButton("MoyenneRaster", "Mean")
											)
											
										)
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({
										fluidRow(
											box(
												title = list(icon("floppy-o"),"Graphic export"),
												status = "warning",
												solidHeader = TRUE,
												collapsible = TRUE,
												collapsed = TRUE,
												width = 12,
												column(10, selectizeInput("RealignElementToExport", label = "Element(s) to export",
																  choices = currentProject()$listeElem,
																  selected = currentProject()$listeElem, multiple = TRUE)),
												column(2, br(), actionButton("RealignExportGraph","Export graphics"))
											)
										)
									}) # eo output$realign2
									
									
								} else {}
								
								if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 2 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
									
									updateSelectInput(session, "elemRaster", selected = input$elemRaster)
									updateSelectInput(session, "nbOutliers", selected = input$nbOutliers)
									
									output$realign3 <- renderUI({
										
										fluidRow(
											box(
												solidHeader = TRUE,
												status = "warning",
												width = 12,
												title = list(icon("area-chart"), "transect realignment"),
												selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp),
												uiOutput("autoCorrel"),
												uiOutput("autoCorrel2"),
												br(),
												actionButton("MoyenneRaster","Delete averaging"),
												actionButton("SauvegarderReal","Save averaging")
											)
											
										)
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({
										fluidRow(
											box(
												title = list(icon("floppy-o"),"Graphic export"),
												status = "warning",
												solidHeader = TRUE,
												collapsible = TRUE,
												collapsed = TRUE,
												width = 12,
												column(10, selectizeInput("RealignElementToExport", label = "Element(s) to export",
																  choices = currentProject()$listeElem,
																  selected = currentProject()$listeElem, multiple = TRUE)),
												column(2, br(), actionButton("RealignExportGraph","Export graphics"))
											)
										)
									}) # eo output$realign2
									
								} else {}
								
								if(input$typeTraitement == "transect" &  ((flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3) & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){

									currentProject()$set_flagRealign(replicate = match(input$selectRealign,currentProject()$samplesFiles), type = "transect", value = flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2])
									currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2(x = "transect")
									
									output$realign3 <- renderUI({
										
										fluidRow(
											box(
												solidHeader = TRUE,
												status = "warning",
												width = 12,
												title = list(icon("area-chart"), "transect realignment"),
												selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp),
												actionButton("SauvegarderReal","Delete Realignment")
											)
											
										)
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({
										fluidRow(
											box(
												title = list(icon("floppy-o"),"Graphic export"),
												status = "warning",
												solidHeader = TRUE,
												collapsible = TRUE,
												collapsed = TRUE,
												width = 12,
												column(10, selectizeInput("RealignElementToExport", label = "Element(s) to export",
																  choices = currentProject()$listeElem,
																  selected = currentProject()$listeElem, multiple = TRUE)),
												column(2, br(), actionButton("RealignExportGraph","Export graphics"))
											)
										)
									}) # eo output$realign2
									
								} else {}
								
								if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1){
									
									currentProject()$set_flagRealign(replicate = match(input$selectRealign,currentProject()$samplesFiles), type = "transect", value = flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2])
									currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2(x = "transect")
									
									output$realign3 <- renderUI({
										
										if(is.numeric(nrow(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot))){
											fluidRow(
												box(
													solidHeader = TRUE,
													status = "warning",
													width = 12,
													title = list(icon("area-chart"), "transect realignment (WARNING: already saved in spot procedure)"),
													style="padding-right: 0px",
													div(style = "",
													    div(style =  "width: 20px; height: 20px; background-color: rgba(212, 222, 20, 0.9); display: inline-block"),
													    div("Mean", style = "display: inline-block"),
													    div(style =  "width: 5px; height: 20px; display: inline-block"),
													    div(style =  "width: 20px; height: 20px; background-color: rgba(214, 185, 176, 0.9); display: inline-block"),
													    div("SD", style = "display: inline-block"),
													    div(style =  "width: 5px; height: 20px; display: inline-block"),
													    div(style =  "width: 20px; height: 20px; background-color: rgba(176, 205, 214, 0.9); display: inline-block"),
													    div("Averaged", style = "display: inline-block")
													),
													br(),
													p("Concentration displayed"),
													br(),
													br()
													
												)#box
												
											)#fluidRow
										} else {}
										
									}) # eo output$realign3
									
									output$realign2 <- renderUI({}) # eo output$realign2
									
								} else {}
								
							})
						} else {}
					}
					
				} else {}
				
			})
			
			###############################################################
			# set output$autoCorrel, i.e. the div displaying the diameter
			# and speed of the laser for avoiding autocorrelation
			###############################################################
			output$autoCorrel <- renderUI({
				div(
					h3("4. Remove laser autocorrelation"),
					numericInput("diameter", label = paste0("transect diameter (", "\u03BC","m)"), value = 50, min = 1, step = 1),
					numericInput("speed", label = paste0("transect speed (", "\u03BC","m/s)"), value = 15, min = 1, step = 1),
					br()
					
				)
			})
			
			###############################################################
			# Observe to calculate the number of analysis to keep
			# for a transect analysis to avoid autocorrelation
			###############################################################
			observe({
				if(!is.null(input$diameter)){
					if(!is.null(input$speed) & !is.null(match(input$selectRealign,currentProject()$samplesFiles))){
						if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0){
							autoCorrel$temp <- input$diameter/	input$speed / currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_pas
						} else {}
					}  else {}
				}  else {}
			})
			
			###############################################################
			# output$autoCorrel2 to decide what is the first point of the transect
			###############################################################
			output$autoCorrel2 <- renderUI({
				div(
					p(icon("arrow-circle-right"), paste0("These settings will keep 1/", ceiling(autoCorrel$temp), " analysis")),
					sliderInput("sliderAutoCorrel", label = "Which points to keep", min = 1, max = ceiling(autoCorrel$temp), value = 1, step = 1)
				)
				
			})
			
			###############################################################
			# output$DetectOutlier to determine how many outliers
			# the user want to detect
			###############################################################
			output$DetectOutlier <- renderUI({
				if(!is.null(input$outlierDetect)){
					
					if(input$outlierDetect != "SD criterion"){
						div(
							numericInput("nbOutliers", label = "Number of outlier(s)", value = 1, min = 1, max = 10, step = 1)
						)
					} else {NULL}
				} else {
					div(
						numericInput("nbOutliers", label = "Number of outlier(s)", value = 1, min = 1, max = 10, step = 1)
						
					)
				}
			})
			
			###############################################################
			# Observe for setting outlierSuggested$temp, i.e. the outliers that are suggested
			###############################################################
			observe({
				input$selectRealign
				input$nbOutliers
				outlierValues$temp
				deplace$val
				if(!is.null(currentProject()) & !is.null(input$selectRealign)){
					if(!is.null(match(input$selectRealign,currentProject()$samplesFiles))){
						if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0  & !is.null(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])){
							if(!is.null(eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)],sep=""))))){
								
								lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), by = 1), function(x){
									eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][x],sep="")))
								})
								
								isolate({
									OutputToPrint <- NULL
									
									if(!is.null(outlierValues$temp)){
										for(i in seq(from = 2, to = length(outlierValues$temp), by = 1)){
											if(length(outlierValues$temp[[i]]) > 0){
												for(j in seq(from = 1, to = length(outlierValues$temp[[i]]), by = 1)){
													
													if(length(outlierValues$temp[[i]][j]) != 0){
														
														if(is.numeric(outlierValues$temp[[i]][j]) & !is.na(outlierValues$temp[[i]][j])){
															
															temp <- paste0(names(outlierValues$temp)[i], ": ", outlierValues$temp[[i]][j])
															
															OutputToPrint <- c(OutputToPrint, temp)
														}
														
													}
												}
											}
											
										}
										outlierSuggested$temp <- OutputToPrint
									} else {}
								})
							}
						} else {}
					} else {}
				} else {}
			})
			
			###############################################################
			# output$DetectOutlier to suggest to user outlier to delete
			###############################################################
			output$PrintOutlier <- renderUI({
				div(style = 'height: 90px;overflow-y: scroll',
				    
				    checkboxGroupInput("ChosenOUtlier", label = "Outlier(s) to keep",
				    			 choices = outlierSuggested$temp,
				    			 selected = outlierSuggested$temp)
				)
			})
			
			###############################################################
			# set the outlierValues$temp by researching the outliers
			###############################################################
			observe({
				input$nbOutliers
				deplace$val
				input$ReplicateSample
				if(!is.null(currentProject()) & !is.null(input$selectRealign)){
					if(!is.null(match(input$selectRealign,currentProject()$samplesFiles))){
						if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0  & !is.null(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])){
							if(!is.null(eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)],sep=""))))){
								
								lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), by = 1), function(x){
									eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][x],sep="")))
								})
								
								isolate({
									
									tabProvSample$temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$intermStepRaster(decalage = deplace$val, input = input$ReplicateSample, outliers = NULL, replace = NA)
									
									if(!is.null(input$nbOutliers) & !is.null(tabProvSample$temp) & !is.null(unlist(tabProvSample$temp))){
										
										dat <- do.call(what = rbind, args = tabProvSample$temp)
										
										if(is.null(input$outlierDetect)){
											method <- "Rosner's test"
										} else {
											method <- input$outlierDetect
										}
										
										Outliers <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_data[[1]]$detectOutlierMatrix(dat, method = method, nbOutliers = input$nbOutliers)
										
										OutlierValues <- lapply(seq(from = 1, to = length(Outliers), by = 1), function(x){
											
											temp <- dat[Outliers[[x]], x]
											
											names(temp) <- dat[Outliers[[x]], 1]
											
											return(temp)
										})
										
										names(OutlierValues) <- colnames(tabProvSample$temp[[1]])
										
										outlierValues$temp <- OutlierValues
									} else {}
								})
							}
							
						} else {}
					} else {}
				} else {}
			})
			
			###############################################################
			# Observe for realign the replicate according to the displayed column
			###############################################################
			observe({
				if(!is.null(input$ReplicatCol)){
					if(input$ReplicatCol > 0){
						
						isolate({
							tabProvSample$temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$RealignColList(
								listRealig = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre,
								col = input$elemRaster,
								step = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_pas)[[1]]
							
							
							
							temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$RealignColList(
								listRealig = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre,
								col = input$elemRaster,
								step = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_pas)[[2]]
							
							names(temp) <- input$ReplicateSample
							
							deplace$val <- temp
						})
						
						
					} else {}
				} else {}
			})
			
			###############################################################
			# Observe for realign the replicate according to all columns
			###############################################################
			observe({
				if(!is.null(input$ReplicatAll)){
					if(input$ReplicatAll > 0){
						isolate({
							tabProvSample$temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$RealignListAll(
								listRealig = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre,
								step = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_pas)
							
							updateSelectInput(session, "elemRaster", selected = input$elemRaster)
						})
					} else {}
				} else {}
			})
			
			###############################################################
			# output$replicates defines the numericInput widgets
			# for the realignement (transect mode)
			###############################################################
			output$replicates <- renderUI({
				
				if(!is.null(input$ReplicateSample)){
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0){
						
						if(length(which(is.element(input$ReplicateSample, names(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])) == FALSE)) == 0){
							
							plot_output_list <- lapply(seq(from = 1, to = length(input$ReplicateSample), by = 1), function(i) {
								plotname <- paste("plot", i, sep="")
								numericInput(inputId = generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][which(names(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[i])],
										 label = currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files[which(names(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[i])],
										 value = 0)
							})
							
							return(plot_output_list)
							
						} else {}
					}else{}
				} else {}
				
			})
			
			################################################################
			#set deplace$val
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$selectRealign)){
					
					if(length(match(input$selectRealign,currentProject()$samplesFiles))!= 0 & !is.null(generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]])){
						
						if(!is.null(eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)],sep=""))))){
							
							lapply(seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), by = 1), function(x){
								eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][x],sep="")))
							})
							
							isolate({
								temp <- NULL
								for (i in seq(from = 1, to = length(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), by = 1)){
									temp[i] <- eval(parse(text = paste("input$",generRRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][i],sep="")))
									
								}
								names(temp) <- input$ReplicateSample
								
								deplace$val <- temp
							})
							
						} else {}
					} else {}
				} else {}
				
			}) # observe
			
			################################################################
			# set the flagRealign$temp
			# save the data when input$SauvegarderSpot is pressed (spot mode)
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$SauvegarderSpot)){
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						isolate({
							if(input$SauvegarderSpot > 0){
								currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFinalSpot(tabSpotSave$temp)
								flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1] <- flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][1] + 1
							} else {}
						})
					} else {}
				} else {}
			}) # observe
			
			################################################################
			# calculate tabSpotDisplay$temp & tabSpotSave$temp (spot mode)
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(isolate(flagRealign$temp)) & !is.null(input$ReplicateSpot) & !is.null(input$typeTraitement) & !is.null(input$selectRealign)){
					
					if(input$typeTraitement == "spot"){
						
						if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
							if(!is.null((isolate(flagRealign$temp)[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2))){
								if((isolate(flagRealign$temp)[[match(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
									
									if(is.matrix(tabProvSpot$temp) | is.data.frame(tabProvSpot$temp)){
										
										vect <- vector()
										
										mat <- matrix(nrow = 1, ncol = ncol(tabProvSpot$temp))
										
										for(i in seq(from = 1, to = nrow(tabProvSpot$temp), by = 1)){
											if(length(which(str_detect(rownames(tabProvSpot$temp)[i], input$ReplicateSpot) == TRUE)) >0){mat <- rbind(mat, tabProvSpot$temp[i,]); vect <- c(vect, i)}
										}
										
										tempMatrix <- mat[-1,]
										rownames(tempMatrix) <- rownames(tabProvSpot$temp)[vect]
										
										if(nrow(tempMatrix) != 0){
											
											if(length(input$ReplicateSpot) == 1){
												
												tabMean <- tempMatrix[1,]
												
												tabSD <- tempMatrix[2,]
											}else{
												
												tabMean <- apply(tempMatrix[seq(from = 1, to = length(input$ReplicateSpot), by = 1),], 2, mean, na.rm = TRUE)
												
												tabSD <- apply(tempMatrix[(seq(from = 1, to = length(input$ReplicateSpot), by = 1)),], 2, sd, na.rm = TRUE)
												
											}
											
											temp <- rbind(tempMatrix, tabMean, tabSD)
											
											rownames(temp) <- c(rownames(tempMatrix), "total_mean", "total_SD")
											
											tabSpotSave$temp <- temp
											
											tabProv <- format(temp,scientific=TRUE, digits = 2)
											
											tabSpotDisplay$temp <- cbind(rownames(tabProv), tabProv)
										} else {}
									} else {}
									
								} else {}
							}
							
						} else {}
					} else {}
				}
				
			}) # observe
			
			################################################################
			# declare outlierChosen$temp (transect mode), i.e. the list of the
			# outliers to keep
			###############################################################
			observe({
				if(!is.null(input$ChosenOUtlier)){
					
					OutliersToKeep <- list()
					
					for(i in seq(from = 1, to = length(input$ChosenOUtlier), by = 1)){
						
						if (i == 1){
							
							OutliersToKeep[[1]] <-  as.numeric(as.character(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][2]))
							
							names(OutliersToKeep) <- str_split(input$ChosenOUtlier[[i]], ": ")[[1]][1]
							
							k <- 2
							
						} else {
							
							if(length(grep(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][1], names(OutliersToKeep))) != 0){
								
								OutliersToKeep[[grep(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][1], names(OutliersToKeep))]] <- c(OutliersToKeep[[grep(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][1], names(OutliersToKeep))]], as.numeric(as.character(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][2])))
								
							} else {
								OutliersToKeep[[k]] <- as.numeric(as.character(str_split(input$ChosenOUtlier[[i]], ": ")[[1]][2]))
								
								names(OutliersToKeep)[[k]] <- str_split(input$ChosenOUtlier[[i]], ": ")[[1]][1]
								
								k <- k + 1
							}
							
						}
					}
					
					tempToExport <- lapply(seq(from = 1, to = length(outlierValues$temp), by = 1), function(x){
						
						if(!is.null(names(outlierValues$temp))){
							if(length(grep(names(outlierValues$temp)[[x]], names(OutliersToKeep))) != 0){
								
								elem <- which(names(OutliersToKeep) == names(outlierValues$temp)[[x]])
								
								toReturn <- OutliersToKeep[[elem]]
								
								tempoR <- outlierValues$temp[[x]]
								
								names(tempoR) <- NULL
								
								if(length(OutliersToKeep[[elem]]) <= input$nbOutliers){
									
									positionMatch <- sapply(seq(from = 1, to = length(OutliersToKeep[[elem]]), by = 1), function(i){
										
										which(round(OutliersToKeep[[elem]][i], digits = 16) == round(tempoR, digits = 16))
										
									})
									
									if(length(outlierValues$temp[[x]]) != 0 &  !is.list(positionMatch)){
										
										names(toReturn) <- names(outlierValues$temp[[x]][positionMatch])
										
									} else {}
									
								} else {}
								
								return(toReturn)
								
							} else {NULL}	
						}
						
						
						
					})
					
					names(tempToExport) <- names(outlierValues$temp)
					
					outlierChosen$temp <- tempToExport
					
				}
			})
			
			################################################################
			# set tabProvSample$temp (transect mode)
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(deplace$val) & !is.null(flagRealign$temp) & !is.null(input$selectRealign) & !is.null(input$ReplicateSample)){
					
					deplace$val
					input$ChosenOUtlier
					
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) != 0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						if(input$typeTraitement == "transect" & (flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
							if(length(which(is.element(input$ReplicateSample, names(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre)) == FALSE)) == 0){
								
								if(!is.null(outlierValues$temp)){
									
									OutliertoDelete <- lapply(seq(from = 1, to = length(outlierValues$temp), by = 1), function(x){
										
										if(length(outlierValues$temp[[x]]) != 0){
											
											toReturn <- sapply(seq(from = 1, to = length(outlierValues$temp[[x]]), by = 1), function(i){
												
												if(!is.null(outlierValues$temp[[x]][i])){
													
													if(length(outlierValues$temp[[x]][i]) != 0 & !is.na(outlierValues$temp[[x]][i])){
														if(is.null(outlierChosen$temp[[x]])){
															outlierValues$temp[[x]][i]
														} else {
															
															if(length(which(round(outlierValues$temp[[x]][i], 16) == round(outlierChosen$temp[[x]], 16))) == 0){
																outlierValues$temp[[x]][i]
															} else {NA}
														}
													} else {NA}
												} else {NA}
											})
											
										}
									})
									
									OutliertoDelete <- lapply(OutliertoDelete, function(x){
										if(!is.null(x)){
											x[!is.na(x)]
										} else {}
									})
									
								} else {OutliertoDelete <- NULL}
								
								isolate({
									tabProvSample$temp <- currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$intermStepRaster(decalage = deplace$val, input = input$ReplicateSample, outliers = OutliertoDelete, replace = NA)
									
								})
								
							} else {}
						} else {}
					}  else {}
				}  else {}
				
				
			}) # observe
			
			################################################################
			# initilize tabProvSpot$temp & tabProvSample$temp
			###############################################################
			observe({
				if(!is.null(input$selectRealign) & length(which(flagSample$temp == TRUE)) != 0){
					if(length(match(input$selectRealign,currentProject()$samplesFiles)) !=0 & !is.na(match(input$selectRealign,currentProject()$samplesFiles))){
						currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFiltre(x = currentProject()$ChoiceUserCorr)
						if(all(is.na(unlist(currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre))) == FALSE){
							tabProvSpot$temp <-  currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$intermStepSpot()
							tabProvSample$temp <-  currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre
						} else {}
					} else {}
				}  else {}
			}) # observe
			
			################################################################
			# set flagRealign$temp
			# average the data
			# when input$MoyenneRaster is pressed (transect mode)
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$MoyenneRaster) & !is.null(isolate(flagRealign$temp))){
					if(length(grep(isolate(input$selectRealign),currentProject()$samplesFiles)) != 0){
						if(input$MoyenneRaster > 0){
							isolate({
								if(length(input$ReplicateSample) == 0){
									tkmessageBox(message = "You need to select at least one replicate to average!", icon = "error", type = "ok")
								} else {
									currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataIntermRaster(tabProvSample$temp)
									currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFinalRaster()
									flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2] <- flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2] + 2
								}
								
							})
						} else {}
					} else {}
				} else {}
			}) # observe
			
			################################################################
			# set flagRealign$temp
			# save the data
			# when input$SauvegarderReal is pressed (transect mode)
			###############################################################
			observe({
				if(!is.null(currentProject()) & !is.null(input$SauvegarderReal)){
					if(length(grep(isolate(input$selectRealign),currentProject()$samplesFiles)) != 0){
						if(input$SauvegarderReal > 0){
							isolate({
								
								currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$set_rep_autoCorrel(c(input$diameter, input$speed, input$sliderAutoCorrel))
								currentProject()$samples[[match(input$selectRealign,currentProject()$samplesFiles)]]$set_rep_dataFinalRasterNonCorr()
								flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2] <- flagRealign$temp[[match(input$selectRealign,currentProject()$samplesFiles)]][2] + 1

							})
						} else {}
					} else {}
				} else {}
			})
			
			
	}
		
		##############################################
		##############################################
		##### CONFIGURATION ##########################
		##############################################
		##############################################
		{
			valeurColor <- reactiveValues(temp = NULL) # a vector of character string created by generRansd which correspond to the id of the chosen color for each element
			
			valDec <- reactiveValues(temp = ".")
			valSep <- reactiveValues(temp = ";")
			
			################################################################
			# create valeurColor$temp
			################################################################
			observe({
				if(!is.null(currentProject()) | !is.null(color$temp)){
					valeurColor$temp <- geneR(letters, 4, length(color$temp), c(waste$temp, geneRMachineCorr$temp, generRRealign$temp, rankStandard$temp, rankSample$temp))
				} else {}
			})
			
			################################################################
			# define output$config0, i.e. the top div
			################################################################
			output$config0 <- renderUI({
				fluidRow(
					box(
						background = "light-blue",
						height = 85,
						width = 12,
						column(9,
							 div(h3(icon("cogs"),"Project customization"), style = "display: inline-block;")
						),
						column(3,
							 br(),
							 actionButton("returnButton", "Return to the current step"))
					)
				)
			}) # eo output$config0
			
			################################################################
			# observe to return to previous step
			################################################################
			observe({
				if(!is.null(input$returnButton)){
					if(input$returnButton != 0){
						isolate({
							updateTabItems(session, "tab", selected = currentPage$temp[2])
						})
					}
				} else {}
			})
			
			################################################################
			# define output$config1, i.e. the div for graphic export format
			################################################################
			output$config1 <- renderUI({
				fluidRow(
					box(
						solidHeader = TRUE,
						collapsible = FALSE,
						width = 6,
						status = "primary",
						title = "graphic export",
						br(),
						radioButtons("exportFormat", label = "Export format", choices = c(".jpeg", ".bmp", ".png", ".tiff"),selected = ".png", inline = TRUE),
						column(6,
							 numericInput("exportwidth", label = "Plot width", value = 760, min = 0)
						),
						column(6,
							 numericInput("exportheight", label = "Plot height", value = 400, min = 0)
						)
					),
					box(
						solidHeader = TRUE,
						collapsible = FALSE,
						width = 6,
						status = "primary",
						title = "Data export",
						br(),
						radioButtons("exportFormatData", label = "Format of exported data", choices = c(".csv", ".xls", ".xlsx"),selected = ".csv", inline = TRUE),
						br(),
						radioButtons("exportseptData", label = "Sep of exported data", choices = c(",", ";", ":", "Tab key", "Blank"),selected = ";", inline = TRUE)
					)
				)
			}) # eo output$config1
			
			################################################################
			# define output$config2, i.e. the div for data import format
			################################################################
			observe({
				if(is.null(currentProject()) | is.null(startSession$temp)){
					output$config2 <- renderUI({
						fluidRow(
							box(
								solidHeader = TRUE,
								collapsible = FALSE,
								width = 12,
								status = "primary",
								title = "Data Import",
								div(
									p("If you import csv data, we can customize the format of data imported"),
									uiOutput("dec"),
									uiOutput("sep")
								)
							)
						)
						
					}) # eo output$config2
				} else {
					if(startSession$temp == 0){
						output$config2 <- renderUI({
							fluidRow(
								box(
									solidHeader = TRUE,
									collapsible = FALSE,
									width = 12,
									status = "primary",
									title = "Calculation settings",
									div(
										div(p("Choose the value with which replace the plateau value under the limit of detection"), style = "display: inline-block;vertical-align: top;margin-top:15px; margin-right:20px"),
										div(selectInput("valRemplace", "", choices = c("NA", "0", "Averaged value of the blank"), selected = "NA", width = '100%'), style = "display: inline-block; margin-top:-10px; width: 300px")
									),
									div(
										div(p("Number of sd to calculate the LOD"), style = "display: inline-block;vertical-align: top;margin-top:15px; margin-right:20px"),
										div(numericInput("sdCustom", label = "", value = 3), style = "display: inline-block; margin-top:-10px; width: 300px")
									)
									
								)
							)
						}) # eo output$config2
					} else {
						output$config2 <- renderUI({NULL})
					}
				}
			})
			
			observe({
				if(!is.null(startSession$temp)){
					if(startSession$temp != 0){
						isolate({
							if(!is.null(input$valRemplace)){
								currentProject()$setvalRemplace(input$valRemplace)
								
								lapply(1:length(currentProject()$standards), function(x){
									lapply(1:length(currentProject()$standards[[x]]$rep_data), function(y){
										currentProject()$standards[[x]]$rep_data[[y]]$setCustomLOD(input$sdCustom)
									})
								})
								
								lapply(1:length(currentProject()$samples), function(x){
									lapply(1:length(currentProject()$samples[[x]]$rep_data), function(y){
										currentProject()$samples[[x]]$rep_data[[y]]$setCustomLOD(input$sdCustom)
									})
								})
							} else {NULL}
							
						})
						
						
					}
				} else {}
				
			})
			
			################################################################
			# define valDec$temp, i.e. the decimal value of the imported data
			################################################################
			observe({
				if(!is.null(input$dec)){
					valDec$temp <- input$dec
				} else {}
			})
			
			################################################################
			# define valSep$temp, i.e. the sep value of the imported data
			################################################################
			observe({
				if(!is.null(input$sep)){
					if(input$sep == "Tab"){
						valSep$temp <- "\t"
					} else if(input$sep == "Blank"){
						valSep$temp <- " "
					} else {
						valSep$temp <- input$sep
					}
					
				} else {}
			})
			
			################################################################
			# define output$dec, i.e. the div to define decimal
			# value of the imported data
			################################################################
			output$dec <- renderUI({
				radioButtons("dec", label = "Decimal",
						 choices = list(".", ","),
						 selected = ".", inline = TRUE)
			})
			
			################################################################
			# define output$dec, i.e. the div to define separator
			# value of the imported data
			################################################################
			output$sep <- renderUI({
				if(!is.null(input$dec)){
					
					if(input$dec == ","){
						radioButtons("sep", label = "Separator",
								 choices = list(".", ";", "Tab", "Blank"),
								 selected = ";", inline = TRUE)
					} else {
						radioButtons("sep", label = "Separator",
								 choices = list(".", ";", "Tab", "Blank", ","),
								 selected = ";", inline = TRUE)
					}
					
				} else {NULL}
				
			})
			
			################################################################
			# define output$config3, i.e. the div to define element color
			################################################################
			observe({
				if(is.null(currentProject())){
					output$config3 <- renderUI({NULL}) # eo output$config3
				}else if(is.null(color$temp)){
					output$config3 <- renderUI({NULL}) # eo output$config3
				} else{
					if(color$temp[1] == 0){
						output$config3 <- renderUI({NULL}) # eo output$config3
					}else{
						output$config3 <- renderUI({
							fluidRow(
								box(
									solidHeader = TRUE,
									collapsible = FALSE,
									width = 12,
									status = "primary",
									title = "Color of chemical element in plots",
									p("Pick the color by clicking on the colored rectangle"),
									div(style = "margin-left: 20px",
									    lapply(seq(from = 1, to = length(currentProject()$listeElem), by = 1), function(x){
									    	div(style = "display: inline-block; width: 50px",
									    	    div(p(currentProject()$listeElem[x])),
									    	    div(colourInput(valeurColor$temp[x], NULL, isolate(color$temp)[x],showColour = "background"), style = "width: 20px")
									    	)
									    })
									)
								)
							)
						}) # eo output$config3
					}
					
				}
			})
			
			################################################################
			# set color$temp, i.e. the value for element color
			################################################################
			observe({
				if(!is.null(valeurColor$temp)){
					if(length(valeurColor$temp) != 0 & !is.na(valeurColor$temp[1])){
						if(!is.null(eval(parse(text = paste0("input$",valeurColor$temp[length(valeurColor$temp)]))))){
							
							for (i in seq(from = 1, to = length(valeurColor$temp), by = 1)){
								color$temp[i] <- eval(parse(text = paste0("input$",valeurColor$temp[i])))
							}
							
						} else {}	
					} else {}
					
				} else{}
				
			})
			
			################################################################
			# define output$config4, i.e. the div to choose:
			# 1. R2 threshold
			# 2. Outlier detection method
			################################################################
			output$config4 <- renderUI({
				if(startSession$temp == 0){
					fluidRow(
						box(
							solidHeader = TRUE,
							collapsible = FALSE,
							width = 6,
							status = "primary",
							title = "Machine drift verification & correction",
							p("Choose the R2 treshold to change the machine drift correction from a linear to a neighbor correction"),
							numericInput("R2", label = "", value = 0.75, min = 0, max = 1, step = 0.01, width = '15%')
						),
						box(
							solidHeader = TRUE,
							collapsible = FALSE,
							width = 6,
							status = "primary",
							title = "Outlier detection",
							p("Choose the method to detect outliers"),
							radioButtons("outlierDetect", label = "",
									 choices = c("SD criterion", "Tietjen.Moore Test", "Rosner's test"),
									 selected = "Rosner's test", inline = TRUE)
						)
					)
				} else {NULL}
				
			})
			
			
			
		}
		
		############################################################
		############################################################
		##### SESSION PRECISON & ACCURACY ##########################
		############################################################
		############################################################
		{
			
			spotDone <- reactiveValues(temp = 0)
			spotPlace <- reactiveValues(temp = vector())
			
			precisionTable <- reactiveValues(temp = matrix())
			correctnessTable <- reactiveValues(temp = matrix())
			 
			output$Precision1 <- renderUI({
				input$saveNists
				input$validDonne
				input$validDonne2
				
				if(!is.null(currentProject()$flag_stand) & length(which(currentProject()$flag_stand != 1)) == 0){
					
					box(
						solidHeader = TRUE,
						collapsible = FALSE,
						width = 12,
						status = "primary",
						title = "Precision (conducted on the standard samples)",
						div(style = "overflow: auto;", dataTableOutput("Precision2")),
						br(),
						br(),
						div(style = "overflow: auto;", dataTableOutput("Precision3"))
					)
				} else {NULL}
			})

			output$Precision2 <- renderDataTable({
				
				input$saveNists
				input$validDonne
				input$validDonne2
				
				if(!is.null(currentProject()$flag_stand) & length(which(currentProject()$flag_stand != 1)) == 0){
					
					tabTemp <- currentProject()$summarySettings
					
					ncoltabTemp <- ncol(tabTemp)
					
					toPrintTemp1 <- sapply(((ncoltabTemp - length(currentProject()$listeElem) +1): ncoltabTemp), function(x){
						round(mean(as.numeric(tabTemp[1:length(currentProject()$standardsFiles),x])), 2)
					})
					
					toPrintTemp1 <- t(as.matrix(toPrintTemp1))
					
					tabTemp <- currentProject()$standards[[1]]$rep_dataFinale
					
					toPrintTemp2 <- sapply(1: ncol(tabTemp), function(x){
						round(tabTemp[nrow(tabTemp),x]/tabTemp[nrow(tabTemp)-1,x]*100, 2)
					})
					
					toPrintTemp2 <- t(as.matrix(toPrintTemp2))
					
					toPrint <- rbind(toPrintTemp2, toPrintTemp1)
					
					colnames(toPrint) <- colnames(currentProject()$standards[[1]]$rep_dataFinale)
					
					toPrint <- cbind(c("%RSD", "LOD cps/sec"), toPrint)
					
					currentProject()$setPrecisionTable(toPrint)
					
					return(toPrint)
				}
			}, options = list(paging = FALSE, searching = FALSE)) # eo output$Precision3
			
			observe({
				input$saveNists
				input$validDrift
				input$SuppDonne
				input$SauvegarderSpot
				input$saveSample
				input$validDonne
				input$validDonne2
				isolate({
					
					spotDoneSimple <- 0
					spotPlaceSimple <- vector()
					
					for(i in 1:length(currentProject()$samplesFiles)){
						if(!is.null(currentProject()$samples[[i]]$rep_type2)){
							if(!is.na(currentProject()$samples[[i]]$rep_type2)){
								if(currentProject()$samples[[i]]$rep_type2 == "spot"){
									
									spotDoneSimple <- spotDoneSimple + 1
									spotPlaceSimple <- c(spotPlaceSimple, i)
									
								} else {}
							} else {}
						}
					}
					
					spotDone$temp <- spotDoneSimple
					spotPlace$temp <- spotPlaceSimple
					
				})
				
			})
			
			output$Precision4 <- renderUI({
				
				input$saveNists
				input$validDrift
				input$SuppDonne
				input$SauvegarderSpot
				input$saveSample
				input$validDonne
				input$validDonne2
				
				if(spotDone$temp > 0) {
					box(
						solidHeader = TRUE,
						collapsible = FALSE,
						width = 12,
						status = "primary",
						title = "Correctness (conducted on the reference materials)",
						h4("Pick the reference standard sample(s) used for assessing the correctness"),
						div(style = "display:flex",
						    checkboxGroupInput("referenceSample", label = "", choices = currentProject()$samplesFiles[spotPlace$temp], selected = currentProject()$samplesFiles[spotPlace$temp[1]], inline = TRUE),
						    actionButton("referenceConcentration", label = "Literature values")
						),
						div(style = "overflow: auto;", dataTableOutput("Precision5"))
						
					)
				} else {NULL}
				
				
				
				
			})
			
			output$Precision5 <- renderDataTable({
				input$saveNists
				input$validDrift
				input$SuppDonne
				input$SauvegarderSpot
				input$saveSample
				input$validDonne
				input$validDonne2
				input$referenceConcentration

				sampleToConsider <- sapply(1:length(input$referenceSample), function(x) which(currentProject()$samplesFiles == input$referenceSample[x]))				

				if(spotDone$temp > 0) {
					matrixToDisplay <- sapply(sampleToConsider, function(x){
						
						tempMat <- currentProject()$samples[[x]]$rep_dataFinalSpot
						
						return(tempMat[nrow(tempMat) -1,])
					})
					
					matrixToDisplay <- t(matrixToDisplay)
					
					if(!is.null(currentProject()$literatureConcentration)){
						if(!is.na(currentProject()$literatureConcentration)){
							
							matrixToDisplayFinal <- rbind(matrixToDisplay, apply(matrixToDisplay, 2, mean, na.rm = T), currentProject()$literatureConcentration)
							
							difference <- matrixToDisplayFinal[nrow(matrixToDisplayFinal),] - matrixToDisplayFinal[nrow(matrixToDisplayFinal)-1,]
							
							matrixToDisplayFinal <- rbind(matrixToDisplayFinal, difference)
							
							rownames(matrixToDisplayFinal) <- c(input$referenceSample, 'Observed mean', 'Literature mean', "Difference")
						} else {
							matrixToDisplayFinal <- rbind(matrixToDisplay, apply(matrixToDisplay, 2, mean, na.rm = T))
							
							rownames(matrixToDisplayFinal) <- c(input$referenceSample, 'Observed mean')	
						}
					} else {NULL}
					
					currentProject()$setCorrectnessTable(matrixToDisplayFinal)
					
					matrixToDisplayFinal <- format(matrixToDisplayFinal, scientific=TRUE, digits = 2)
					
					matrixToDisplayFinal <- cbind(rownames(matrixToDisplayFinal), matrixToDisplayFinal)
					
				} else {}
			}, options = list(paging = FALSE, searching = FALSE, rowCallback = I(
				'function(row, data) {
				if (data[0].includes ("Observed"))
				$("td", row).css("background", "rgba(176, 205, 214, 0.9)");
				else if (data[0].includes ("Literature"))
				$("td", row).css("background", "rgba(214, 185, 176, 0.9)");
				else if (data[0].includes ("Difference"))
				$("td", row).css("background", "rgba(212, 222, 20, 0.9)");
		}'
			)))
			
			observe({
				if(!is.null(input$referenceConcentration)){
					if(input$referenceConcentration > 0){
						isolate({
							Filters <- matrix(c("Text", ".csv", "OO sheet", ".ods", "Excel sheet", ".xls", "Excel sheet", ".xlsx"), 4, 2, byrow = TRUE)
							
							if(Sys.info()[1] == "Windows"){
								
								temp <- choose.files(default = paste0(projChar$temp[[3]]))
								
							} else  {
								
								temp <- tk_choose.files(default = paste0(projChar$temp[[3]]), caption = "Select files",multi = FALSE, filters = Filters, index = 1)
								
							}
							
							if(length(temp) != 0){
								currentProject()$setLiteratureConcentration(temp, sep = valSep$temp, dec = valDec$temp)
							} else {}
						})
					}
				}
			})
		}


}#eo server
	
	
	######################
	######## CALL shinyApp
	######################
	app <- shinyApp(ui, server)
	runApp(app, launch.browser = TRUE)
	} # nocov end
