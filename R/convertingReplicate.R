convertingReplicate <- function(){
  
  ui <- shinyUI(dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      tags$style(HTML("
                      .form-control.shiny-bound-input.shinyjs-resettable{
                      margin-bottom:30px
                      }"
    )),
    useShinyjs(),
    uiOutput("step1"),
    uiOutput("step2"),
    uiOutput("step3")
      )
      ))
  
  server <- shinyServer(function(input, output, session) {
    addClass(selector = "body", class = "sidebar-collapse")
    
  	projPath <- reactiveValues(temp = NULL) #project path
  	listFiles <- reactiveValues(temp = NULL) # vector of the path of the files from the project
  	listNames <- reactiveValues(temp = NULL) # vector of the names of the files from the project
  	listData <- reactiveValues(temp = NULL)# list of the tables of the files from the project
  	elementConsidered <-reactiveValues(temp = NULL) # a vector of the element to convert
  	unitToCOnvert <- reactiveValues(temp = NULL)# the unit of the final data
  	atomicWeight <- reactiveValues(temp = NULL)# the table of the atomic weight 
  	
  	## Read the atomic weight table (included in the package)
  	observe({

  		path <- paste0(system.file("", package="elementR"), "/AtomicMass.csv")

  		atomicWeight$temp <- readData(path, sep = ",", dec = ".")
  	})

  	# First step (choose the repository and the setting - dec and sep - of the uploaded files)
  	output$step1 <-  renderUI({
  		fluidRow(
  			box(
  				background = "light-blue",
  				width = 12,
  				column(4,
  					 div(h4(icon("folder-o"),"1. Choose the folder containing the files to convert"))
  				),
  				column(3,
  					 radioButtons("sep", label = "Separator",
  					 		 choices = list(".", ";", "Tab", "Blank", ","),
  					 		 selected = ",", inline = TRUE)
  					 ),
  				column(2,
  					 radioButtons("dec", label = "Decimal",
  					 		 choices = list(".", ","),
  					 		 selected = ".", inline = TRUE)
  					 ),
  				column(1,
  					 actionButton("repLoad", "Load repository")
  					 ),
  				column(1,
  					 actionButton("runExampleNew", "Load example")
  				)
  			)
  		)
  	})

  	# Upload and read the files of the repository
  	observe({
  		if(!is.null(input$repLoad)){
  			if(input$repLoad > 0){
  				isolate({
  					if(Sys.info()[1] == "Windows"){

  						projPath$temp <- choose.dir()

  					} else  {

  						projPath$temp <- tk_choose.dir()

  					}

  					if(!is.na(projPath$temp)){

  						listFiles$temp <- list.files(projPath$temp, recursive = T, full.names = T)
  						listNames$temp <- list.files(projPath$temp, recursive = T)

  						listProv <- lapply(1:length(listFiles$temp), function(x){
  							readData(listFiles$temp[[x]], sep = input$sep, dec = input$dec)
  						})

  						columnName <- colnames(listProv[[1]])

  						elementConsidered$temp <- gsub("[[:digit:]]","", columnName)

  						listData$temp <- listProv

  					} else{}
  				})
  			}
  		}
  	})
  	
  	# Upload and read the files of the repository
  	observe({
  		if(!is.null(input$runExampleNew)){
  			if(input$runExampleNew > 0){
  				isolate({
  					
  					if(Sys.info()[1] == "Windows"){
  						projPath$temp <- paste0(system.file("", package="elementR"), "/Example_conversion")
  					} else {
  						projPath$temp <- paste0(system.file("", package="elementR"), "Example_conversion")
  					} 
  					
  					if(!is.na(projPath$temp)){
  						
  						listFiles$temp <- list.files(projPath$temp, recursive = T, full.names = T)
  						listNames$temp <- list.files(projPath$temp, recursive = T)
  						
  						listProv <- lapply(1:length(listFiles$temp), function(x){
  							readData(listFiles$temp[[x]], sep = input$sep, dec = input$dec)
  						})
  						
  						columnName <- colnames(listProv[[1]])
  						
  						elementConsidered$temp <- gsub("[[:digit:]]","", columnName)
  						
  						listData$temp <- listProv
  						
  					} else{}
  				})
  			}
  		}
  	})

  	# Step 2, display the name of the uploaded files, choose the internal standard and determine the unit of the final data
  	output$step2 <- renderUI({
  		if(length(listData$temp) > 0){
  			
  			tempElement <- which(str_detect(colnames(listData$temp[[1]]), "Ca") == TRUE)
  			
  			fluidRow(
  				box(
  					title = list(icon("cogs"), "2. Concentration conversion"),
  					width = 12,
  					status="primary",
  					solidHeader = TRUE,
  					column(5,
  						 h3("Loaded files"),
  						 lapply(1:length(listNames$temp), function(x){
  						 	p(listNames$temp[x])
  						 	})
  					),
  					column(3,
  						 selectInput("InternStand", label = h3("Internal Standard"),
  						 		choices = colnames(listData$temp[[1]])[-1],
  						 		selected = colnames(listData$temp[[1]])[tempElement])
  						 ),
  					column(2,
  						 uiOutput("help1")
  						 ),
  					column(2,
  						 uiOutput("help2")
  					)
  				)
  			)

  		} else {NULL}

  	})

  	# define help1
  	output$help1 <- renderUI({
  		radioButtons("concentration", label = h3("Your sample unit"),
  				 choices = c("ppm/ppm", "Mol/Mol"),
  				 selected = "ppm/ppm")

  	})

  	# define help2
  	output$help2 <- renderUI({
  		h3(paste0("Conversion in ", unitToCOnvert$temp))
  	})

  	# set unitToCOnvert$temp
  	observe({
  		if(!is.null(input$concentration)){
  				if(input$concentration == "ppm/ppm"){
  					unitToCOnvert$temp <- "Mol/Mol"
  				} else {
  					unitToCOnvert$temp <- "ppm/ppm"
  				}

  		}
  	})

  	#step 3, exporting the converted data
  	output$step3 <-  renderUI({
  		if(length(listData$temp) > 0){
  			fluidRow(
  				box(
  					background = "light-blue",
  					width = 12,
  					column(4,
  						 div(h4(icon("folder-o"),"3. Export your data"))
  					),
  					column(3,
  						 actionButton("ExportData", "Export converted data")
  					)
  				)
  			)
  		} else {NULL}
  	})

  	#conversion of the data and export
  	observe({
  		if(!is.null(input$ExportData)){
  			if(input$ExportData > 0){
  				isolate({
  					lapply(1:length(listNames$temp), function(x){

  						if(unitToCOnvert$temp == "ppm/ppm"){
  							tabTemp <- convertMol_to_PPM(listData$temp[[x]], AtomicMass = atomicWeight$temp, InternStand = input$InternStand)

  							write.table(tabTemp, paste0(projPath$temp, "/", listNames$temp[x],"_ppm", ".csv"), sep = input$sep, dec = input$dec, col.names=NA)

  						} else {
  							tabTemp <- convertPPM_to_Mol(listData$temp[[x]], AtomicMass = atomicWeight$temp, InternStand = input$InternStand)

  							write.table(tabTemp, paste0(projPath$temp, "/", listNames$temp[x],"_Mol", ".csv"), sep = input$sep, dec = input$dec, col.names=NA)
  						}
  					})
  				})
  			}
  		}
  	})

  }) 
  

  
  app <- shinyApp(ui, server)
  runApp(app, launch.browser = TRUE)
  
}
