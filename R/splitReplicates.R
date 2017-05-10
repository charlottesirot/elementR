splitReplicate <- function(){
  
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
    uiOutput("start"),
    uiOutput("graphics")
      )
      ))
  
  server <- shinyServer(function(input, output, session) {
    addClass(selector = "body", class = "sidebar-collapse")
    
    sampleNumber <- reactiveValues(temp = 0) #Number of hypothetized sample
    fileSplitName <- reactiveValues(temp = NULL)
    fileSplitData <- reactiveValues(temp = NULL)
    plateauDelin <- reactiveValues(temp = NULL)
    sampleLimits <- reactiveValues(temp = NULL)
    colReplicates <- reactiveValues(temp = NULL)
    standElem <- reactiveValues(temp = NULL)
    
    output$start <- renderUI({
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
          title = list(icon("folder"),"Settings"),
          width = 6,
          solidHeader = TRUE,
          status="primary",
          uiOutput("choiceStandardElement"),
          uiOutput("numberSample")
        )
        
      )
      
    })#end of start
    
    output$numberSample <- renderUI({
      numericInput("num", label = h4("Number of sampling"), value = sampleNumber$temp)
    })
    
    output$choiceStandardElement <- renderUI({
    	
    	tempElement <- which(str_detect(colReplicates$temp, "Ca") == TRUE)
    	
      selectInput("standardElement", label = h4("Select your standard element"), 
                  choices = colReplicates$temp, 
                  selected = colReplicates$temp[tempElement])
    })
    
    ##############################################################
    # upload the file to split
    ##############################################################
    observe({
      if(!is.null(input$createProjButton)){
        if(input$createProjButton != 0){
          isolate({
            
            if(Sys.info()[1] == "Windows"){
              
              fileSplitName$temp <- choose.files(default = getwd())
              
            } else  {
              
              fileSplitName$temp <- tk_choose.files(default = getwd(), 
                                                    caption = "Select files", 
                                                    multi = FALSE, 
                                                    filters = matrix(c(".csv", ".csv"), 1,2), 
                                                    index = 1)
              
            }
            
          })
        }
      }
      
    }) #observe
    
    observe({
      if(!is.null(input$runExampleNew)){
        if(input$runExampleNew != 0){
          isolate({
            
            if(Sys.info()[1] == "Windows"){
              fileSplitName$temp <- paste0(system.file("", package="elementR"), "/splitReplicate_example.csv")
            } else {
              fileSplitName$temp <- paste0(system.file("", package="elementR"), "splitReplicate_example.csv")
            }  
            
          })
        }
      }
    }) #observe
    
    ##############################################################
    # read the file to split
    ##############################################################
    observe({
      if(!is.null(fileSplitName$temp)){
        isolate({
          fileSplitData$temp <- readData(fileSplitName$temp, sep = ";", dec = ".")
          
          colReplicates$temp <- colnames(fileSplitData$temp)
          
        })
      }
    })
    
    observe({
      if(!is.null(input$standardElement)){
        isolate({
          standElem$temp <- which(colnames(fileSplitData$temp) == input$standardElement)
        })
        
      }
    })
    
    observe({
      if(!is.null(fileSplitName$temp) & length(standElem$temp) != 0){
        isolate({
          sampleNumber$temp <- 0
          sampleLimits$temp <- NULL
          
          plateauDelin$temp <- kmeans(fileSplitData$temp[, standElem$temp], 2, algorithm = "Hartigan-Wong")$cluster
          
          deux <- fileSplitData$temp[which(plateauDelin$temp == 2)[1], standElem$temp]
          
          un <- fileSplitData$temp[which(plateauDelin$temp == 1)[1], standElem$temp]
          
          if(un > deux){
            plateauDelin$temp <- 2 - plateauDelin$temp
          } else {
            plateauDelin$temp <- plateauDelin$temp - 1
          }
          
          tempLimits <- vector()
          
          if(plateauDelin$temp[1] == 1){
            tempLimits <- c(tempLimits, 1)
            sampleNumber$temp <- 1
          } else {
            sampleNumber$temp <- 0
          }
          
          for(i in 3:(length(plateauDelin$temp)-1)){
            if(plateauDelin$temp[i] == 1 & plateauDelin$temp[i-1] == 0 & plateauDelin$temp[i-2] == 0 & plateauDelin$temp[i+1] == 1 & plateauDelin$temp[i+2] == 1){
              sampleNumber$temp <- sampleNumber$temp +1
              tempLimits <- c(tempLimits, i)
            } else if(plateauDelin$temp[i] == 0 & plateauDelin$temp[i-1] == 1 & plateauDelin$temp[i-2] == 1 & plateauDelin$temp[i+1] == 0 & plateauDelin$temp[i+2] == 0){
              tempLimits <- c(tempLimits, i)
            } else {}
            
          }
          
          new <- tempLimits
          
          if(sampleNumber$temp > 2){
            
            for(i in 2:(length(tempLimits)-1)){
              if(!is.na(tempLimits[i]) & !is.na(tempLimits[i+1])){
                if(tempLimits[i] == (tempLimits[i+1]-1)){
                  new <- new[-c(i,i+1)]
                  sampleNumber$temp <- sampleNumber$temp - 1
                } else {}
              } else{}
            }
            
            for(i in 1:length(new)){
              if(i == 1){
                if(new[1] != 1){
                  
                  new[1] <- new[1] - floor(new[1]/3)
                }
              } else {
                if(i%%2 == 0){
                  new[i] <- new[i] + floor((new[i+1]-new[i])/3)
                } else {
                  new[i] <- new[i] - floor((new[i]-new[i-1])/3)
                }
                
              }
            }
            
            sampleLimits$temp <- new
            
            
            
          } else {}
        })
      }
    })
    
    ##############################################################
    # plot the graphics
    ##############################################################
    output$graphics <- renderUI({
      
      input$standardElement
      
      if(!is.null(fileSplitName$temp) & length(standElem$temp) != 0){
        
        minX <- min(fileSplitData$temp[,1], na.rm = TRUE)
        maxX <- max(fileSplitData$temp[,1], na.rm = TRUE)
        
        column(12, style  = "padding-right: 5px",
               box(
                 title = list(icon("share"),"Data to split"),
                 status="primary",
                 solidHeader = TRUE,
                 width = "100%",
                 fluidRow(
                   column(10,
                          plotOutput("rawDataPlot"),
                          lapply(1:input$num, function(i){
                            
                            limitLow <- sampleLimits$temp[2*i-1]
                            limitHigh <- sampleLimits$temp[2*i]
                            
                            div(style="height: 82px;padding-left: 40px",
                                sliderInput(paste0("tag",i),paste0("Replicates", i, "limits"), value = c(fileSplitData$temp[limitLow,1],fileSplitData$temp[limitHigh,1] ), min = minX, max = maxX, step = 1, width = '100%', round = TRUE)
                            )
                            
                          })
                   ),
                   column(2, style = "margin-top:370px",
                          actionButton("exportReplicates", "Export the replicates"),
                          lapply(1:input$num, function(i){
                            textInput(paste0("name",i), label = "", value = paste0("Replicate_", i))
                          })
                          
                   )
                   
                 )
                 
               )
        )
      } else { NULL}
    })# end of graphics
    
    output$rawDataPlot <- renderPlot({
      
      input$standardElement
      
      par(mar = c(3,3.5,1.75,0))
      
      if(!is.null(fileSplitName$temp) & !is.null(sampleNumber$temp) & length(standElem$temp) != 0){
        
        maxY <- max(fileSplitData$temp[,standElem$temp], na.rm = TRUE)
        
        minX <- min(fileSplitData$temp[,1], na.rm = TRUE)
        maxX <- max(fileSplitData$temp[,1], na.rm = TRUE)
        
        plot(fileSplitData$temp[, standElem$temp] ~ fileSplitData$temp[,1], xlim = c(minX, maxX), ylim = c(0,maxY), ylab = "cps/s", type = "l")
        
        lapply(1:input$num, function(i){
          
          abline(v = eval(parse(text = paste0("input$tag", i)))[1],  lty = "dashed", col = "grey", lwd = 2)
          abline(v = eval(parse(text = paste0("input$tag", i)))[2],  lty = "dashed", col = "grey", lwd = 2)
          
          if(!is.null(eval(parse(text = paste0("input$tag", i))))){
            rect(eval(parse(text = paste0("input$tag", i)))[1], -maxY, eval(parse(text = paste0("input$tag", i)))[2], (1+10/100)*maxY, col = "#8B735564", border = NA)
          }
          
        })
        
        
      } else {NULL}
      
    })
    
    ##############################################################
    # Export the split data
    ##############################################################
    
    observe({
      if(!is.null(input$exportReplicates)){
        if(input$exportReplicates > 0){
          isolate({
            
            if(Sys.info()[1] == "Windows"){
              
              projPath <- choose.dir()
              
            } else  {
              
              projPath <- tk_choose.dir()
              
            }
          	
          	if(!is.na(projPath)){
          		lapply(1:input$num, function(i){
          			limits <- which(fileSplitData$temp[,1] >= eval(parse(text = paste0("input$tag", i)))[1] & fileSplitData$temp[,1] <= eval(parse(text = paste0("input$tag", i)))[2])
          			tabTemp <- fileSplitData$temp[limits,]
          			write.csv(tabTemp, file = paste0(projPath, "/", eval(parse(text = paste0("input$name", i))), ".csv"))
          			
          		})
          		
          		res <- tkmessageBox(title = "INFO !",message = "data exported", icon = "info", type = "ok")
          	}
            
          })
        }
      }
    })
    
  }) 
  
  app <- shinyApp(ui, server)
  runApp(app, launch.browser = TRUE)
  
}