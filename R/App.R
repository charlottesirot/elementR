##############################################################
#
# elementR 1.0 - 10/04/2016
# 
# charlottesirot@free.fr
# francois.guilhaumon@ird.fr
#
#####################################################################

runElementR <- function(){
  
  
  ######################
  ############### GLOBAL
  ######################
  
  
  #skyn
  skin <- Sys.getenv("DASHBOARD_SKIN")
  skin <- tolower(skin)
  if (skin == "") skin <- "blue"
  
  ######################
  ############ FUNCTIONS
  ######################
  
  ##################################################################################################
  # Name: readData
  # function: detect the format of the data and read the table
  # input : x = a character string of the path of the data
  # output: a matrix
  ################################################################################################## 
  
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
  
  
  ######################
  ################### UI
  ######################
  
  sidebar <-   dashboardSidebar(
    useShinyjs(),
    sidebarMenu(id = "tab", 
                div(p(icon("star-half-o"),"element-R", style = "font-size: 200%; padding-left:50px;padding-top:5px"), style = "background: rgb(60, 141, 188); height: 50px"),
                menuItem("Step 1. Project Settings", tabName = "start", icon = icon("flask")),
                div(style = "height: 30px",
                    div(imageOutput("myImage1"), style = "padding-left: 100px; padding-top: 5px")
                ),
                menuItem("Step 2. Filtering standart data", tabName = "Standards", icon = icon("flask")),
                uiOutput("renderProgress2"),
                div(style = "height: 30px",
                    div(imageOutput("myImage2"), style = "padding-left: 100px; padding-top: 5px")
                ),
                menuItem("Step 3. Drift verification", tabName = "MachDrift", icon = icon("flask")),
                div(style = "height: 30px",
                    div(imageOutput("myImage3"), style = "padding-left: 100px; padding-top: 5px")
                ),
                menuItem("Step 4. Filtering sample data", tabName = "Samples", icon = icon("flask")),
                uiOutput("renderProgress4"),
                div(style = "height: 30px",
                    div(imageOutput("myImage4"), style = "padding-left: 100px; padding-top: 5px")
                ),
                menuItem("Step 5. Sample Repl. averaging", tabName = "realign", icon = icon("flask")),
                uiOutput("renderProgress5"),
                hr(style ="width: 70%; color: white; align: center"),
                menuItem("Configuration", icon = icon("sliders"), tabName = "Config"),
                menuItem("Source code for app", icon = icon("file-code-o"),
                         href = "https://github.com/charlottesirot/elementR"
                ),
                div(uiOutput("Export"), style = "text-align: center")
    )
  )
  
  body <- dashboardBody(
    tags$style(HTML("
                    
                    .col-sm-6.class1>.form-group.shiny-input-container{
                    margin-top:-20px;
                    margin-bottom:0px;
                    }

                    .col-sm-3.class1>.form-group.shiny-input-container{
                    margin-top:-25px;
                    margin-bottom:0px;
                    }
                    
                    .col-sm-2.class1>.form-group.shiny-input-container{
                    margin-top:-25px;
                    margin-bottom:0px;
                    }
                    
                    .col-sm-6>.box.box-solid.box-primary>.box-body>.form-group.shiny-input-container{
                    margin-top:-20px
                    }
                    
                    .skin-blue>.wrapper{
                    min-height:100%;
                    }
                    
                    .content{
                    padding-left: 0px;
                    padding-top:0px;
                    padding-right:0px;
                    padding-bottom:0px;
                    } 
                    
                    .skin-blue>.wrapper>.content-wrapper{
                    min-height:100%;
                    }
                    
                    .skin-blue>.wrapper>.content-wrapper>.content{
                    min-height: 100vh;
                    }                  
                    
                    .box.box-solid.bg-green>.box-body{
                    background-color:#429926;}

                    .box.box-solid.bg-green{margin-bottom:10px}
                    
                    .col-sm-12>.box.box-solid.bg-olive>.box-body{
                    background-color: #9B6CA8;
                    } 

                    .box.box-solid.bg-olive{
                    margin-bottom:0px
                    }
                    
                    .col-sm-12>.box.box-solid.bg-aqua>.box-body{
                    background-color: #85735D;
                    } 
                    
                    .box.box-solid.bg-aqua{
                    background-color: #85735D;
                    margin-bottom:10px} 
                    
                  
                    .box.box-solid.box-success>.box-header {
                    color:#fff;
                    background:#429926
                    }


                    
                    .box.box-solid.box-info>.box-header {
                    color:#fff;
                    background:#85735D;
                    }
                    
                    .box.box-solid.box-info{
                    border-bottom-color:#85735D;
                    border-left-color:#85735D;
                    border-right-color:#85735D;
                    border-top-color:#85735D;
                    margin-bottom:10px
                    }
                    
                    .box.box-solid.box-danger>.box-header {
                    background:#9B6CA8
                    }
                    
                    .box.box-solid.box-danger{
                    margin-top: 0px
                    }
                    
                    .box.box-solid.box-danger{
                    border-bottom-color:#9B6CA8;
                    border-left-color:#9B6CA8;
                    border-right-color:#9B6CA8;
                    border-top-color:#9B6CA8;
                    }
                    
                    
                    .box.box-solid.box-success{
                    border-bottom-color:#429926;
                    border-left-color:#429926;
                    border-right-color:#429926;
                    border-top-color:#429926;
                    margin-bottom:10px
                    }
                    
                    .box1{
                    padding-left:0px;
                    padding-right:0px;
                    padding-top: 0px;
                    padding-bottom: 0px;
                    }
                    
                    .col-sm-1.class2{
                    margin-top:10px
                    }
                    
                    .col-sm-7.class3{
                    margin-top:5px
                    }
                    
                    .col-sm-5>#sample1.shiny-html-output.shiny-bound-output{
                    margin-top:10px
                    }
                    
                    .box-body.box1>#MachDrift3_3.shiny-html-output.shiny-bound-output>.col-sm-6{
                    margin-bottom:0px;
                    padding-left:0px;
                    padding-right:0px;
                    }
                    
                    .box-body.box1>#MachDrift3_3.shiny-html-output.shiny-bound-output>.col-sm-6>.box{
                    
                    box-shadow:0 0px 0px rgba(0,0,0,0);
                    border-top-color: white;
                    
                    }
                    
                    .box-body.box1>#MachDrift3_2.shiny-html-output.shiny-bound-output>.col-sm-6{
                    margin-bottom:0px;
                    padding-left:0px;
                    padding-right:0px;
                    }
                    
                    .box-body.box1>#MachDrift3_2.shiny-html-output.shiny-bound-output>.col-sm-6>.box{
                    
                    box-shadow:0 0px 0px rgba(0,0,0,0);
                    border-top-color: white;
                    
                    }
                    
                    .box-body.box1>#MachDrift3_1.shiny-html-output.shiny-bound-output>.col-sm-6{
                    margin-bottom:0px;
                    padding-left:0px;
                    padding-right:0px;
                    }
                    
                    .box-body.box1>#MachDrift3_1.shiny-html-output.shiny-bound-output>.col-sm-6>.box{
                    
                    box-shadow:0 0px 0px rgba(0,0,0,0);
                    border-top-color: white;
                    
                    }
                    
                    .box-body.box1>#MachDrift3_0.shiny-html-output.shiny-bound-output>.col-sm-6{
                    margin-bottom:0px;
                    padding-left:0px;
                    padding-right:0px;
                    }
                    
                    .box-body.box1>#MachDrift3_0.shiny-html-output.shiny-bound-output>.col-sm-6>.box{
                    
                    box-shadow:0 0px 0px rgba(0,0,0,0);
                    border-top-color: white;
                    
                    }
                    
                    .checkbox{
                    margin-top:0px;
                    }
                    
                    #realign4.shiny-datatable-output.shiny-bound-output{
                    width:170px
                    }
                    
                    #DataTables_Table_4_info.dataTables_info{
                    color:white;
                    }
                    #DataTables_Table_5_info.dataTables_info{
                    color:white;
                    }
                    #DataTables_Table_6_info.dataTables_info{
                    color:white;
                    }
                    #DataTables_Table_8_info.dataTables_info{
                    color:white;
                    }
                    
                    .col-sm-12>.box.box-solid.box-danger{
                    margin-bottom: 0px; margin-top: 10px
                    }

                    #shiny-tab-Samples.tab-pane.active>.col-sm-12{
                    padding-left:0px;
                    padding-right: 0px
                    }
                    
                    .box.box-solid.box-danger.collapsed-box{
                    margin-bottom: 0px; margin-top: 10px
                    }
                    
                    .box.box-solid.box-warning{
                    margin-bottom: 0px; margin-top: 10px
                    }
                    
                    .box.box-solid.box-warning.collapsed-box{
                    margin-bottom: 0px; margin-top: 10px
                    }
                    
                    .tab-content{
                    margin-top: 10px;
                    margin-right: 10px;
                    }
                    
                    .box.box-solid.bg-yellow{
                    margin-bottom: 0px
                    }
                    
                    
                    ")),
    
    tags$head(tags$style(HTML('
                              /* main sidebar */
                              .skin-blue .main-sidebar {                           
                              background-color: #666666;
                              font-size: 100%;
                              
                              }
                              
                              #tab.sidebar-menu.shiny-bound-input{
                              position:fixed;
                              width:230px
                              }
                              
                              .main-sidebar{
                              padding-top:0px
                              }
                              
                              .sidebar{
                              padding-top:0px
                              }
                              
                              .skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li:hover>a {
                              background-color: #444444;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #444444;
                              } 
                              
                              .progress{height: 30px;background-color: #444444;padding-left: 5px; padding-top: 5px; padding-right: 5px; padding-bottom: 5px; margin-bottom:0px;border-left-color:rgb(60, 141, 188);border-left-style: solid; border-left-width:3px}
                              .progressActive{height: 30px;background-color: #666666;padding-left: 5px; padding-top: 5px; padding-right: 5px; padding-bottom: 5px; margin-bottom:0px; color: rgb(184, 199, 206);}
                              
                              .bar{background-color: #666666;height: 30px;}
                              .barActive{background-color: #444444;height: 30px;}
                              '))),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
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
                      uiOutput("realign3")
                      
                      
              ), #eo tab realign
              tabItem("Config",
                      uiOutput("config0"),
                      uiOutput("config2"),
                      uiOutput("config3"),
                      uiOutput("config1")
                      
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
          tkmessageBox(message = "You need to finish the first step for handling the rest of the filtration procedure!", icon = "error", type = "ok")
          
        } else {}
        
      } else if(length(which(currentProject()$flag_stand != 1)) != 0){
        
        if(input$tab == "MachDrift" | input$tab == "Samples" | input$tab == "realign"){
          
          updateTabItems(session, "tab", selected = "Standards")
          tkmessageBox(message = "You need to finish filtering standarts for continuing the filtration procedure!", icon = "error", type = "ok")
        } else {}
        
      } else if(currentProject()$flagMachineCorrection != 1){
        
        if(input$tab == "Samples" | input$tab == "realign"){
          
          updateTabItems(session, "tab", selected = "MachDrift")
          tkmessageBox(message = "You need to validate machine drift for continuing the filtration procedure!", icon = "error", type = "ok")
          
        } else {}
        
      } else if(length(which(flagSample$temp == TRUE)) == 0){
        
        if(input$tab == "realign"){
          
          updateTabItems(session, "tab", selected = "Samples")
          tkmessageBox(message = "You need to validate all the sample replicate to access the last step !", icon = "error", type = "ok")
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
                    div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: right;padding-right: 20px")
                    
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
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px"),
                    div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: right;padding-right: 20px")
                    
                )) 
          } else{
            div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
                div(style = "overflow: auto;",
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px")
                    
                )) 
          }
        } 
        else if(input$tab == "MachDrift"){
          if((validCorrection$temp%%2) == 1){
            div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
                div(style = "overflow: auto;",
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px"),
                    div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: right;padding-right: 20px")
                    
                )) 
          } else{
            div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
                div(style = "overflow: auto;",
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px")
                    
                )) 
          }        
        }
        else if(input$tab == "Samples"){
          
          temp <- sapply(1: length(currentProject()$flag_Sample), function(x){
            if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
              return(1)
            }else{return(0)}
          })  
          
          if(length(temp) == 0){
            div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
                div(style = "overflow: auto;",
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px")
                    
                )) 
          } else{
            div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
                div(style = "overflow: auto;",
                    div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px"),
                    div(actionButton("nextStep", p(icon("arrow-circle-right"), "Next Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: right;padding-right: 20px")
                    
                )) 
          }
          
        } 
        else if(input$tab == "realign"){
          div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ",
              div(style = "overflow: auto;",
                  div(actionButton("prevStep", p(icon("arrow-circle-left"), "Previous Step", style="margin-bottom:5px"), style="padding-top: 5px;padding-bottom: 0px;margin-top: 7px"), style ="float: left;padding-left: 20px")
                  
              ))
        }else{div(style = "background: rgb(60, 141, 188); height: 50px; margin-top:-10px; margin-left: -10px; ") }
      }
      
      
      
    })
    
    #################################
    ########### Mise en Forme    ####
    #################################
    
    output$myImage1 <- renderImage({
      list(src = system.file("www/1.png", package="elementR"),
           contentType = 'image/png',
           width = 20,
           height = 20,
           alt = "This is alternate text")
    }, deleteFile = FALSE ) # eo output$myImage1 
    
    output$myImage2 <- renderImage({
      list(src = system.file("www/1.png", package="elementR"),
           contentType = 'image/png',
           width = 20,
           height = 20,
           alt = "This is alternate text")
    }, deleteFile = FALSE ) # eo output$myImage2
    
    output$myImage3 <- renderImage({
      list(src = system.file("www/1.png", package="elementR"),
           contentType = 'image/png',
           width = 20,
           height = 20,
           alt = "This is alternate text")
    }, deleteFile = FALSE ) # eo output$myImage3
    
    output$myImage4 <- renderImage({
      list(src = system.file("www/1.png", package="elementR"),
           contentType = 'image/png',
           width = 20,
           height = 20,
           alt = "This is alternate text")
    }, deleteFile = FALSE ) # eo output$myImage3
    
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
              p(paste0("Standard(s) filtred: ", sum(currentProject()$flag_stand), " / ", length(currentProject()$flag_stand)), style = "line-height:1px; text-align:center")
              
          )
        } else {
          div(class = "progressActive", style = "overflow: auto;",
              p(paste0("Standard(s) filtred: ", sum(currentProject()$flag_stand), " / ", length(currentProject()$flag_stand)), style = "line-height:1px; text-align:center")
              
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
          div(imageOutput("myImageProgressBar2"),style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive"),
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
        div(class = "progress", style = "overflow: auto;",
            p(paste0("Sample repl. filtred: ", do.call(sum, currentProject()$flag_Sample), " / ", length(unlist(currentProject()$flag_Sample))), style = "line-height:1px; text-align:center")            
        )
      } else {
        div(class = "progressActive", style = "overflow: auto;",
            p(paste0("Sample repl. filtred: ", do.call(sum, currentProject()$flag_Sample), " / ", length(unlist(currentProject()$flag_Sample))), style = "line-height:1px; text-align:center")        
        )
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
        
        temp <- sapply(1: length(currentProject()$flag_Sample), function(x){
          if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){
            return(1)
          }else{return(0)}
        })  
        
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
          div(imageOutput("myImageProgressBar4"),style = "height: 44px; width: 30px; padding-top: 8px", class = "barActive"),
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
          
          temp <- sum(sapply(1:length(flagRealign$temp), function(x){
            if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
              return(1)
            } else{return(0)}
          }))
          
          div(class = "progress", style = "overflow: auto;",
              p(paste0("Samples handled: ", temp, " / ", length(flagRealign$temp)), style = "line-height:1px; text-align:center")              
          )
        } else{
          
          temp <- sum(sapply(1:length(flagRealign$temp), function(x){
            if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
              return(1)
            } else{return(0)}
          }))
          
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
        temp <- sapply(1:length(flagRealign$temp), function(x){
          if(flagRealign$temp[[x]][1] == 1 | flagRealign$temp[[x]][2] == 3){
            return(1)
          } else{return(0)}
        })
        
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
    
    #################################
    ########### Project Export   ####
    #################################
    
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
    
    observe({
      if(!is.null(input$export)){  
        
        if(input$export > 0){
          isolate({
            
            espace1 <- getwd()
            setwd(paste0(projPath$temp,"/Done"))
            
            pb <- tkProgressBar("Progress bar", "Project export in %",
                                0, 100, 0)
            
            myProject <- currentProject()
            temp <- str_split(projPath$temp, "/")[[1]]
            nameToInsert <- temp[length(temp)]
            
            if(input$text == "Name of your project..."){
              save(myProject, file = paste0(projPath$temp,"/Done/", nameToInsert, ".RData"))
            } else {
              save(myProject, file = paste0(projPath$temp,"/Done/", input$text, ".RData"))
            }
            
            
            if(!is.null(input$exportseptData)){
              if(input$exportseptData == "Tab key"){
                sep <-  "\t"
              } else if(input$exportseptData == "Blank"){
                sep <- " "
              } else{sep <- input$exportseptData}
            }
            
            if(is.matrix(currentProject()$standards[[1]]$rep_dataFinale)){
              if(is.null(input$exportFormatData)){
                write.csv(currentProject()$standards[[1]]$rep_dataFinale, file = "SummaryStandard.csv")
              } else{
                if(input$exportFormatData == ".csv"){
                  write.table(currentProject()$standards[[1]]$rep_dataFinale, file = "SummaryStandard.csv", sep = sep)
                }
                else{                
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_dataFinale), file = paste0("SummaryStandard",input$exportFormatData), sep = sep) 
                }
              }            
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
            
            info <- sprintf("%d%% done", round(20))
            setTkProgressBar(pb, 20, sprintf("Export (%s)", info), info)
            
            setwd(espace1)
            setwd(paste0(projPath$temp,"/Done/standards"))
            
            lapply(1:length(currentProject()$standards[[1]]$rep_Files), function(x){suppressWarnings(dir.create(paste0(projPath$temp,"/Done/standards/", currentProject()$standards[[1]]$rep_Files[x])))})
            
            lapply(1:length(currentProject()$standards[[1]]$rep_Files),function(x){
              
              setwd(paste0(projPath$temp,"/Done/standards/", currentProject()$standards[[1]]$rep_Files[x]))
              
              info <- sprintf("%d%% done", round(20 + x*10/length(currentProject()$standards[[1]]$rep_Files)))
              setTkProgressBar(pb, round(20 + x*10/length(currentProject()$standards[[1]]$rep_Files)), sprintf("Export (%s)", info), info)
              
              if(currentProject()$flag_stand[x] != 0){
                if(is.null(input$exportFormatData)){
                  
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataBlank, file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataPlateau, file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataSuppBlank, file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataSupLOD, file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataNorm, file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],".csv"))
                  write.csv(currentProject()$standards[[1]]$rep_data[[x]]$dataOutlierFree, file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],".csv"))  
                  
                } else if(input$exportFormatData == ".csv"){
                  
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataBlank, file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataPlateau, file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataSuppBlank, file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataSupLOD, file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataNorm, file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  write.table(currentProject()$standards[[1]]$rep_data[[x]]$dataOutlierFree, file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],".csv"), sep = sep)
                  
                } else {
                  
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataBlank), file = paste0("data_Blank_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataPlateau), file = paste0("data_Plateau_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataSuppBlank), file = paste0("data_SuppBlank_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataSupLOD), file = paste0("data_SupLOD_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataNorm), file = paste0("data_Norm_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)
                  write.table(as.data.frame(currentProject()$standards[[1]]$rep_data[[x]]$dataOutlierFree), file = paste0("data_OutlierFree_",currentProject()$standards[[1]]$rep_Files[x],input$exportFormatData), sep = sep)    
                  
                }
              }
              
            }) # eo lapply  
            
            lapply(1:length(currentProject()$samplesFiles), function(x){
              
              setwd(espace1)
              suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/",currentProject()$samplesFiles[x])))
              
              lapply(1:length(currentProject()$samples[[x]]$rep_Files), function(y){
                setwd(espace1)
                
                info <- sprintf("%d%% done", round(30 + (x*70/length(currentProject()$samplesFiles))*y/length(currentProject()$samples[[x]]$rep_Files)))
                setTkProgressBar(pb, round(30 + (x*70/length(currentProject()$samplesFiles))*y/length(currentProject()$samples[[x]]$rep_Files)), sprintf("Export (%s)", info), info)
                
                temporaire <- currentProject()$samples[[x]]$rep_Files[y]
                suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/",currentProject()$samplesFiles[x],"/",temporaire)))
                setwd(paste0(projPath$temp,"/Done/samples/",currentProject()$samplesFiles[x],"/",temporaire))
                
                if(currentProject()$flag_Sample[[x]][y] != 0){
                  if(is.null(input$exportFormatData)){
                    
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataBlank, file = paste0("data_Blank_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateau, file = paste0("data_Plateau_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataSuppBlank, file = paste0("data_SuppBlank_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataSupLOD, file = paste0("data_SupLOD_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataNorm, file = paste0("data_Norm_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataConc, file = paste0("data_Conc_",temporaire,".csv"))
                    write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataConcCorr, file = paste0("data_ConcCorr_",temporaire,".csv"))
                    
                  } else if(input$exportFormatData == ".csv"){
                    
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataBlank, file = paste0("data_Blank_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateau, file = paste0("data_Plateau_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataSuppBlank, file = paste0("data_SuppBlank_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataSupLOD, file = paste0("data_SupLOD_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataNorm, file = paste0("data_Norm_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataConc, file = paste0("data_Conc_",temporaire,".csv"), sep = sep)
                    write.table(currentProject()$samples[[x]]$rep_data[[y]]$dataConcCorr, file = paste0("data_ConcCorr_",temporaire,".csv"), sep = sep)
                    
                  } else {
                    
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataBlank), file = paste0("data_Blank_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateau), file = paste0("data_Plateau_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataSuppBlank), file = paste0("data_SuppBlank_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataSupLOD), file = paste0("data_SupLOD_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataNorm), file = paste0("data_Norm_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataConc), file = paste0("data_Conc_",temporaire,input$exportFormatData), sep = sep)
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_data[[y]]$dataConcCorr), file = paste0("data_ConcCorr_",temporaire,input$exportFormatData), sep = sep)
                    
                  }
                }
                
              }) #eo lapply
              
              setwd(paste0(projPath$temp,"/Done/"))
              
              if(length(currentProject()$machineCorrection) != 1 | (length(currentProject()$machineCorrection) == 1 & !is.na(currentProject()$machineCorrection[1]))){
                
                tempo <- currentProject()$regressionModel
                if(is.null(input$exportFormatData)){
                  write.csv(tempo, file = "regression_parameters.csv")         
                } else if(input$exportFormatData == ".csv"){
                  write.table(tempo, file = "regression_parameters.csv", sep = sep) 
                } else{
                  write.table(as.data.frame(tempo), file = paste0("regression_parameters", input$exportFormatData), sep = sep) 
                }
                
              }
              
              setwd(paste0(projPath$temp,"/Done/samples/",currentProject()$samplesFiles[x]))
              
              if(!is.na(currentProject()$samples[[x]]$rep_type2)){
                if(currentProject()$samples[[x]]$rep_type2 == "spot"){
                  invisible(file.remove(list.files()[which(str_detect(list.files(), "finalReplicates") == TRUE)]))
                  if(is.null(input$exportFormatData)){
                    write.csv(currentProject()$samples[[x]]$rep_dataFinalSpot, file = paste0("final_",currentProject()$samplesFiles[x],".csv"))
                  } else if(input$exportFormatData == ".csv"){
                    write.table(currentProject()$samples[[x]]$rep_dataFinalSpot, file = paste0("final_",currentProject()$samplesFiles[x],".csv"), sep = sep)
                  } else {
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataFinalSpot), file = paste0("final_",currentProject()$samplesFiles[x],input$exportFormatData), sep = sep)
                  }
                } else if(currentProject()$samples[[x]]$rep_type2 == "raster"){
                  
                  if(is.null(input$exportFormatData)){
                    lapply(1:length(currentProject()$samples[[x]]$rep_dataIntermRaster), function(k){
                      write.csv(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]], file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],".csv"))
                    })
                    
                    write.csv(currentProject()$samples[[x]]$rep_dataFinalRaster, file = paste0("final_",currentProject()$samplesFiles[x],".csv"))
                  } else if(input$exportFormatData == ".csv"){
                    lapply(1:length(currentProject()$samples[[x]]$rep_dataIntermRaster), function(k){
                      write.table(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]], file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],".csv"), sep = sep)
                    })
                    write.table(currentProject()$samples[[x]]$rep_dataFinalRaster, file = paste0("final_",currentProject()$samplesFiles[x],".csv"), sep = sep)
                  } else {
                    lapply(1:length(currentProject()$samples[[x]]$rep_dataIntermRaster), function(k){
                      write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataIntermRaster[[k]]), file = paste0("finalReplicates_",names(currentProject()$samples[[x]]$rep_dataIntermRaster)[k],input$exportFormatData), sep = sep)
                    })
                    write.table(as.data.frame(currentProject()$samples[[x]]$rep_dataFinalRaster), file = paste0("final_",currentProject()$samplesFiles[x],input$exportFormatData), sep = sep)
                  }
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
    
    #################################
    ########### Graphics Export  ####
    #################################
    
    observe({
      if(!is.null(input$ExportGraph)){      
        if(input$ExportGraph > 0){
          isolate({
            
            espace1 <- getwd()
            
            temporaire <- input$standardIn
            
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done/standards/", temporaire, "/graphics")))
            
            setwd(paste0(projPath$temp,"/Done/standards/", temporaire, "/graphics")) 
            
            if(!is.null(length(input$courveToExport)) & length(input$courveToExport) != 0){
              if(!is.null(length(input$ElementToExport)) & length(input$ElementToExport) != 0){
                
                  pb <- tkProgressBar("Progress bar", "Graphic export in %",
                                      0, 100, 0)
                  
                  #### Raw Data exporting #####
                  if(is.null(input$exportFormat)){
                    jpeg(filename = paste0("RawData_",temporaire ,".jpg"), width = 760, height = 400)
                  } else {
                    
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
                  
                  par(mar = c(5.1,5,4.1,1))              
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                    
                    maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data, na.rm = TRUE)
                    
                    minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    
                    plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExport[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                    mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                    mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                    mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                    
                    lapply(1:length(input$ElementToExport), function(x){
                      
                      par(new = TRUE)
                      
                      plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExport[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                      
                    })
                    
                    if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
                      Temp$t  <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$bins[1])[[2]]
                      Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$bins[2])[[2]]
                      Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$plat[[1]])[[2]]
                      Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$plat[[2]])[[2]]                
                      
                    } else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
                      Temp$t <-  currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[1])[[2]]
                      Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[2])[[2]]
                      Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[1])[[2]]
                      Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[2])[[2]]
                    } else {}
                    
                    rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                    rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                    
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], lty = "dashed", col = "grey", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                    
                    lapply(input$ElementToExport, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,x], cex = 3, col ="grey")})
                    lapply(input$ElementToExport, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,x], cex = 3, col ="grey")})
                    lapply(input$ElementToExport, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
                    lapply(input$ElementToExport, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
                    
                    
                  } else {}
                  
                  par(mar = c(0,0,2,1))
                  plot(0,0, axes = FALSE, type = "n")
                  legend(-1,1, legend = input$ElementToExport, bty = "n", col = color$temp[sapply(1:length(input$ElementToExport), function(x) {which(input$ElementToExport[x] == names(color$temp))})], pch = 16, cex = 1.5)
                  
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
                        
                        maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[j]], na.rm = TRUE)
                        
                        minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                        maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                        
                        plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[j]],type ="b", ylab = "", xlab = "", main = paste0("RawData_",input$ElementToExport[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
                        mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                        mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                        
                        if(!is.null(input$bins) & !is.null(input$plat)){
                          if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
                            
                          }else{
                            Temp$t  <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$bins[1])[[2]]
                            Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$bins[2])[[2]]
                            Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$plat[1])[[2]]
                            Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], y = input$plat[2])[[2]]                
                            
                          }
                        } else {}
                        
                        rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                        rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                        
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                        
                        points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,input$ElementToExport[j]],  cex = 3, col ="grey")
                        points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,input$ElementToExport[j]], cex = 3, col ="grey")
                        points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
                        points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
                        
                      }   
                      
                      title(input$standardIn, outer=TRUE, cex = 1.5)
                      
                      dev.off()
                    }
                  } else {}
                  
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
                    
                    maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[j]], na.rm = TRUE)
                    
                    minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    
                    plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$ElementToExport[j]],type ="b", ylab = "", xlab = "",main = paste0("RawData_",input$ElementToExport[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
                    mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                    mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                    
                    if(!is.null(input$bins) & !is.null(input$plat)){
                      if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
                        
                      }else{
                        Temp$t  <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[1])[[2]]
                        Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[2])[[2]]
                        Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[1])[[2]]
                        Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[2])[[2]]                
                        
                      }
                    } else {}
                    
                    rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], -maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                    rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                    
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                    abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                    
                    points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,input$ElementToExport[j]],  cex = 3, col ="grey")
                    points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,input$ElementToExport[j]], cex = 3, col ="grey")
                    points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
                    points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,input$ElementToExport[j]], cex = 3, col ="#4F3CBC50")
                    
                  }
                  
                  title(input$standardIn, outer=TRUE, cex = 1.5)
                  
                  dev.off()
                  
                  info <- sprintf("%d%% done", round(40))
                  setTkProgressBar(pb, 40, sprintf("Export (%s)", info), info)
                  
                  #### Filtered Data exporting #####
                  
                  for(i in 1: length(input$ElementToExport)){
                    for(j in 1: length(input$courveToExport)){
                      
                      suppressWarnings(dir.create(paste0(projPath$temp,"/Done/standards/", temporaire, "/graphics/", input$ElementToExport[i])))
                      
                      setwd(paste0(projPath$temp,"/Done/standards/", temporaire, "/graphics/", input$ElementToExport[i]))
                      
                      if(input$courveToExport[j] == "Blank removed"){tempName <- "Blank_removed"
                      } else if(input$courveToExport[j] == "> LOD"){tempName <- "Supp_LOD"
                      } else if(input$courveToExport[j] == "Outliers free"){tempName <- "Outliers_free"
                      } else{
                        tempName <- input$courveToExport[j]
                      }
                      
                      if(is.null(input$exportFormat)){
                        jpeg(filename = paste0("FilteredData",tempName,".jpg"), width = 760, height = 400)
                      } else {
                        
                        if(input$exportFormat == ".jpeg"){
                          jpeg(filename = paste0("FilteredData",tempName,".jpg"), width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".bmp"){
                          bmp(filename = paste0("FilteredData",tempName,".bmp"), width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".png"){
                          png(filename = paste0("FilteredData",tempName,".png"), width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".tiff"){
                          tiff(filename = paste0("FilteredData",tempName,".tiff"), width = input$exportwidth, height = input$exportheight)
                        } else {}
                      }
                      
                      if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
                        if(is.null(input$bins)){
                        } else if(is.null(input$plat)){
                        } else if(is.null(Temp$t)){
                        } else if(is.null(Temp0$t)){
                        } else if(is.null(Temp1$t)){
                        } else if(is.null(Temp2$t)){
                        }else{
                          if(is.finite(Temp$t)){
                            curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                          } else {}
                        }
                      } else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
                        curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$renderData(curve = input$courveToExport[j])
                      } else {}
                      
                      if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                        text(1,0.5, labels = "No data different from NA", cex = 2)
                      } else{
                        par(mar = c(5.1,4.1,4.1,2))
                        plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
                        mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                        mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                        mtext(paste("Data filtered",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
                      }
                      
                      dev.off()
                    }
                    
                  }
                  
                  info <- sprintf("%d%% done", round(70))
                  setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)
                  
                  for(i in 1: length(input$ElementToExport)){
                    
                    setwd(paste0(projPath$temp,"/Done/standards/", temporaire, "/graphics/", input$ElementToExport[i]))
                    
                    if(length(input$courveToExport) <= 6) {
                      
                      if(is.null(input$exportFormat)){
                        jpeg(filename = paste0("FilteredData_All.jpg"), width = 760, height = 400)
                      } else {
                        if(input$exportFormat == ".jpeg"){
                          jpeg(filename = "FilteredData_All.jpg", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".bmp"){
                          bmp(filename = "FilteredData_All.bmp", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".png"){
                          png(filename = "FilteredData_All.png", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".tiff"){
                          tiff(filename = "FilteredData_All.tiff", width = input$exportwidth, height = input$exportheight)
                        } else{}
                      }
                      par(mfrow = c(2,3))
                      for(j in 1:length(input$courveToExport)){
                        
                        if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
                          if(is.null(input$bins)){
                          }else if(is.null(input$plat)){
                          }else if(is.null(Temp$t)){
                          }else if(is.null(Temp0$t)){
                          }else if(is.null(Temp1$t)){
                          }else if(is.null(Temp2$t)){
                          } else{
                            if(is.finite(Temp$t)){
                              curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                            }
                          }
                        } else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
                          curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$renderData(curve = input$courveToExport[j])
                        } else {}
                        
                        if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA", cex = 2)
                        } else{
                          par(mar = c(5.1,4.1,4.1,2))
                          plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
                        }
                      } 
                      dev.off()
                      
                    } else {
                      
                      if(is.null(input$exportFormat)){
                        jpeg(filename = paste0("FilteredData_All.jpg"), width = 760, height = 400)
                      } else {
                        if(input$exportFormat == ".jpeg"){
                          jpeg(filename = "FilteredData_All.jpg", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".bmp"){
                          bmp(filename = "FilteredData_All.bmp", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".png"){
                          png(filename = "FilteredData_All.png", width = input$exportwidth, height = input$exportheight)
                        } else{}
                        
                        if(input$exportFormat == ".tiff"){
                          tiff(filename = "FilteredData_All.tiff", width = input$exportwidth, height = input$exportheight)
                        } else{}
                      }
                      par(mfrow = c(2,3))
                      for(j in 1:(length(input$courveToExport)-1)){
                        
                        if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
                          if(is.null(input$bins)){
                          }else if(is.null(input$plat)){
                          }else if(is.null(Temp$t)){
                          }else if(is.null(Temp0$t)){
                          }else if(is.null(Temp1$t)){
                          }else if(is.null(Temp2$t)){
                          } else{
                            if(is.finite(Temp$t)){
                              curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$getData(curve = input$courveToExport[j], bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                            }
                          }
                        } else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
                          curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$renderData(curve = input$courveToExport[j])
                        } else {}
                        
                        if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA", cex = 2)
                        } else{
                          par(mar = c(5.1,4.1,4.1,2))
                          plot(curve[,1], curve[,grep(input$ElementToExport[i], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExport[i], input$courveToExport[j]),side=3,line=0.75, cex=1.2, font = 2)
                        }
                      }
                      dev.off()
                      
                      if(is.null(input$exportFormat)){
                        jpeg(filename = paste0("FilteredData_All2.jpg"), width = 760, height = 400)
                      } else{
                        if(input$exportFormat == ".jpeg"){
                          jpeg(filename = "FilteredData_All2.jpg", width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".bmp"){
                          bmp(filename = "FilteredData_All2.bmp", width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".png"){
                          png(filename = "FilteredData_All2.png", width = input$exportwidth, height = input$exportheight)
                        } else {}
                        
                        if(input$exportFormat == ".tiff"){
                          tiff(filename = "FilteredData_All2.tiff", width = input$exportwidth, height = input$exportheight)
                        } else {}
                      }
                      par(mfrow = c(2,3))
                      if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){
                        if(is.null(input$bins)){
                          
                        }else if(is.null(input$plat)){
                        } else if(is.null(Temp$t)){
                        } else if(is.null(Temp0$t)){
                        } else if(is.null(Temp1$t)){
                        } else if(is.null(Temp2$t)){
                        } else {
                          if(is.finite(Temp$t)){
                            curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$getData(curve = input$courveToExport[length(input$courveToExport)], bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                          } else {}
                        }
                      } else if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
                        curve <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$renderData(curve = input$courveToExport[length(input$courveToExport)])
                      } else {}
                      if(length(which(!is.na(curve[,grep(input$ElementToExport[i], colnames(curve))]))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                        text(1,0.5, labels = "No data different from NA", cex = 2)
                      } else{
                        par(mar = c(5.1,4.1,4.1,2))
                        plot(curve[,1], curve[,grep(input$ElementToExport[length(input$ElementToExport)], colnames(curve))],  type ="b", ylab = "", xlab = "", main = "")
                        mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                        mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                        mtext(paste("Data filtered",input$ElementToExport[i], input$courveToExport[length(input$courveToExport)]),side=3,line=0.75, cex=1.2, font = 2)
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
            
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/", input$SampleIn, "/", temporaire, "/graphics")))
            
            setwd(paste0(projPath$temp,"/Done/samples/", input$SampleIn, "/", temporaire, "/graphics"))                  
            
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
                
                if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) != 0){
                  
                  maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data, na.rm = TRUE) 
                  
                  minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                  maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                  
                  plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$ElementToExportS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExportS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                  mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                  mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                  mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                  
                  lapply(1:length(input$ElementToExportS), function(x){
                    
                    par(new = TRUE)
                    
                    plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$ElementToExportS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$ElementToExportS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                    
                  })
                    
                  
                  TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[1])[[2]]
                  Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[2])[[2]]
                  Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[1])[[2]]
                  Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[2])[[2]]
                  
                  rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                  rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col = "#4F3CBC30", border = NA)
                  
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                  
                  lapply(1:length(input$ElementToExportS), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,x],  cex = 3, col ="grey")})
                  lapply(1:length(input$ElementToExportS), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,x], cex = 3, col ="grey")})
                  lapply(1:length(input$ElementToExportS), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
                  lapply(1:length(input$ElementToExportS), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
                  
                } else {}
                
                par(mar = c(0,0,2,1))
                plot(0,0, axes = FALSE, type = "n")
                legend(-1,1, legend = input$ElementToExportS, bty = "n", col = color$temp[sapply(1:length(input$ElementToExportS), function(x) {which(input$ElementToExportS[x] == names(color$temp))})], pch = 16, cex = 1.5)
                
                dev.off()
                
                info <- sprintf("%d%% done", round(10))
                setTkProgressBar(pb, 10, sprintf("Export (%s)", info), info)
                
                nbGraph <- floor(length(input$ElementToExportS)/6)
                
                nRest <- length(input$ElementToExportS)%%6
                
                if(nbGraph != 0){
                  for(i in 1: nbGraph){
                  
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
                    
                    maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[, input$ElementToExportS[j]], na.rm = TRUE) 
                    
                    minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                    
                    plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$ElementToExportS[j]],type ="b", ylab = "", xlab = "", main = paste0("RawData_",input$ElementToExportS[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
                    mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                    mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                    
                    TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[1])[[2]]
                    Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[2])[[2]]
                    Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[1])[[2]]
                    Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[2])[[2]]
                    
                    rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                    rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                    
                    abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
                    abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                    abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                    abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                    
                    points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,input$ElementToExportS[j]],  cex = 3, col ="grey")
                    points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,input$ElementToExportS[j]], cex = 3, col ="grey")
                    points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
                    points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
                    
                  }   
                  
                  title(temporaire, outer=TRUE, cex = 1.5)
                  
                  dev.off()
                  }
                } else {}
                
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
                  
                  maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[, input$ElementToExportS[j]], na.rm = TRUE) 
                  
                  minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                  maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                  
                  plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$ElementToExportS[j]],type ="b", ylab = "", xlab = "", main = paste0("RawData_",input$ElementToExportS[j]), col = "black", xlim = c(minX, maxX), ylim =c(0,maxY))
                  mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                  mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                  
                  TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[1])[[2]]
                  Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[2])[[2]]
                  Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[1])[[2]]
                  Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[2])[[2]]
                  
                  rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                  rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                  
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                  abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                  
                  points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,input$ElementToExportS[j]],  cex = 3, col ="grey")
                  points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,input$ElementToExportS[j]], cex = 3, col ="grey")
                  points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
                  points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,input$ElementToExportS[j]], cex = 3, col ="#4F3CBC50")
                  
                }
                
                title(input$standardIn, outer=TRUE, cex = 1.5)
                
                dev.off()
                
                info <- sprintf("%d%% done", round(40))
                setTkProgressBar(pb, 40, sprintf("Export (%s)", info), info)
                
                #### Filtered Data exporting #####
                
                for(i in 1: length(input$ElementToExportS)){
                  for(j in 1: length(input$courveToExportS)){ 
                    
                    suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i])))
                    
                    setwd(paste0(projPath$temp,"/Done/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i]))
                    
                    if(input$courveToExportS[j] == "Blank removed"){tempNameS <- "Blank_removed"
                    } else if(input$courveToExportS[j] == "> LOD"){tempNameS <- "Supp_LOD"
                    } else if(input$courveToExportS[j] == "Conc. corrected"){tempNameS <- "Conc._corrected"
                    } else{
                      tempNameS <- input$courveToExportS[j]
                    }
                    
                    if(is.null(input$exportFormat)){
                      jpeg(filename = paste0("FilteredData",tempNameS,".jpg"), width = 760, height = 400)
                    } else{
                      if(input$exportFormat == ".jpeg"){
                        jpeg(filename = paste0("FilteredData",tempNameS,".jpg"), width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".bmp"){
                        bmp(filename = paste0("FilteredData",tempNameS,".bmp"), width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".png"){
                        png(filename = paste0("FilteredData",tempNameS,".png"), width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".tiff"){
                        tiff(filename = paste0("FilteredData",tempNameS,".tiff"), width = input$exportwidth, height = input$exportheight)
                      } else {}
                    }
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                      
                      if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                        
                      } else {
                        if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
                          if(is.null(input$bins)){                          
                          } else if(is.null(input$plat)){  
                          } else if(is.null(TempS$t)){   
                          } else if(is.null(Temp0S$t)){                          
                          } else if(is.null(Temp1S$t)){                          
                          } else if(is.null(Temp2S$t)){                          
                          } else {
                            if(is.finite(TempS$t)){
                              curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(curve = input$courveToExportS[j], bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2, meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                              
                            } else {}
                          }
                        } else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
                          if(is.finite(TempS$t)){
                            curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$renderData(curve = input$courveToExportS[j])
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
                        mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                      }
                    } else{}
                    dev.off()
                  }
                  
                }
                
                info <- sprintf("%d%% done", round(70))
                setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)

                
                for(i in 1: length(input$ElementToExportS)){
                  
                  setwd(paste0(projPath$temp,"/Done/samples/", input$SampleIn, "/", temporaire, "/graphics/", input$ElementToExportS[i]))
                  
                  if(length(input$courveToExportS) <= 6) {
                    
                    if(is.null(input$exportFormat)){
                      jpeg(filename = "FilteredData_All.jpg", width = 760, height = 400)
                    } else{
                      if(input$exportFormat == ".jpeg"){
                        jpeg(filename = "FilteredData_All.jpg", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".bmp"){
                        bmp(filename = "FilteredData_All.bmp", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".png"){
                        png(filename = "FilteredData_All.png", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".tiff"){
                        tiff(filename = "FilteredData_All.tiff", width = input$exportwidth, height = input$exportheight)
                      } else{}
                    }
                    par(mfrow = c(2,3))
                    for(j in 1:length(input$courveToExportS)){                
                      
                      if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                        if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                          
                        }else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
                          if(is.null(input$bins)){                          
                          } else if(is.null(input$plat)){ 
                          } else if(is.null(TempS$t)){ 
                          } else if(is.null(Temp0S$t)){                          
                          } else if(is.null(Temp1S$t)){                          
                          } else if(is.null(Temp2S$t)){                          
                          } else{
                            if(is.finite(TempS$t)){curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(curve = input$courveToExportS[j], bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2, meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                            } else {}
                          }
                        } else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
                          
                          if(is.finite(TempS$t)){
                            curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$renderData(curve = input$courveToExportS[j])
                          } else {}
                          
                        } else {}
                        
                      } else {}
                      
                      if(!is.null(curveS)){
                        if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA", cex = 2)
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                        } else {                    
                          par(mar = c(3.5,3.7,1.75,1))
                          plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")  
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                        }
                      }else{}
                      
                    }
                    dev.off()
                    
                  } else{
                    
                    if(is.null(input$exportFormat)){
                      jpeg(filename = "FilteredData_All.jpg", width = 760, height = 400)
                    } else{
                      if(input$exportFormat == ".jpeg"){
                        jpeg(filename = "FilteredData_All.jpg", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".bmp"){
                        bmp(filename = "FilteredData_All.bmp", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".png"){
                        png(filename = "FilteredData_All.png", width = input$exportwidth, height = input$exportheight)
                      } else{}
                      
                      if(input$exportFormat == ".tiff"){
                        tiff(filename = "FilteredData_All.tiff", width = input$exportwidth, height = input$exportheight)
                      } else{}
                    }
                    par(mfrow = c(2,3))
                    for(j in 1:length(input$courveToExportS)){                
                      
                      if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                        if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                          
                        }else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
                          if(is.null(input$bins)){                          
                          } else if(is.null(input$plat)){ 
                          } else if(is.null(TempS$t)){ 
                          } else if(is.null(Temp0S$t)){                          
                          } else if(is.null(Temp1S$t)){                          
                          } else if(is.null(Temp2S$t)){                          
                          } else{
                            if(is.finite(TempS$t)){curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(curve = input$courveToExportS[j], bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2, meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                            } else {}
                          }
                        } else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
                          
                          if(is.finite(TempS$t)){
                            curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$renderData(curve = input$courveToExportS[j])
                          } else {}
                          
                        } else {}
                        
                      } else {}
                      
                      if(!is.null(curveS)){
                        if(length(which(!is.na(curveS[,grep(input$ElementToExportS[i], colnames(curveS))]))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA", cex = 2)
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                        } else {                    
                          par(mar = c(5.1,4.1,4.1,2))
                          plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")  
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                        }
                      }else{}
                      
                    }
                    dev.off()
                    
                    if(is.null(input$exportFormat)){
                      jpeg(filename = "FilteredData_All2.jpg", width = 760, height = 400)
                    }  else{                
                      if(input$exportFormat == ".jpeg"){
                        jpeg(filename = "FilteredData_All2.jpg", width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".bmp"){
                        bmp(filename = "FilteredData_All2.bmp", width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".png"){
                        png(filename = "FilteredData_All2.png", width = input$exportwidth, height = input$exportheight)
                      } else {}
                      
                      if(input$exportFormat == ".tiff"){
                        tiff(filename = "FilteredData_All2.tiff", width = input$exportwidth, height = input$exportheight)
                      } else {}
                    }
                    par(mfrow = c(2,3)) 
                    for(j in (length(input$courveToExportS)-2): (length(input$courveToExportS))){
                      
                      if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                        if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){                      
                        }else{
                          if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
                            
                            if(is.null(input$bins)){
                            } else if(is.null(input$plat)){
                            } else if(is.null(TempS$t)){
                            } else if(is.null(Temp0S$t)){
                            } else if(is.null(Temp1S$t)){
                            } else if(is.null(Temp2S$t)){
                            }else{
                              if(is.finite(TempS$t)){curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(curve = input$courveToExportS[j], bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2, meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                              } else {}
                            }
                            
                          } else if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
                            if(is.finite(TempS$t)){curveS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$renderData(curve = input$courveToExportS[j])
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
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
                        } else{                    
                          par(mar =  c(5.1,4.1,4.1,2))
                          plot(curveS[,1], curveS[,grep(input$ElementToExportS[i], colnames(curveS))],  type ="b", ylab = "", xlab = "", main = "")
                          mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                          mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                          mtext(paste("Data filtered",input$ElementToExportS[i], input$courveToExportS[j]),side=3,line=0.75, cex=1.2, font = 2)
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
              } else {}
            } else {}
          })
        } else {}
        
      } else {}
      
    })
    
    observe({
      if(!is.null(input$MachDriftExportGraph)){
        
        if(input$MachDriftExportGraph>0){
          
          isolate({
            espace1 <- getwd()
            
            setwd(paste0(projPath$temp,"/Done/"))
            
            pb <- tkProgressBar("Progress bar", "Graphic export in %",
                                0, 100, 0)
            
            three <-intersect(which(currentProject()$nbCalib == 3), sapply(1: length(input$MachDriftElementToExport), function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))}))  
            
            three <- input$MachDriftElementToExport[three]
            
            nbGraph <- floor(length(three)/6)
            
            nRest <- length(three)%%6
            
            temporaryTab <- currentProject()$standards[[1]]$rep_dataFinale
            
            temp <- str_sub(rownames(temporaryTab), 1, -6)
            
            X <- vector()
            for (i in 1:length(currentProject()$standardsFiles)){
              X[i] <- currentProject()$standardRank[which(names(currentProject()$standardRank) == temp[i])] 
              
            }
            
            if(nbGraph > 0){
              
              for(i in 1: nbGraph){
                
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
                  
                  min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                  
                  max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                  
                  currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                  
                  abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                  
                  mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                  
                  mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[j,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[j,6],3)))
                  mtext(side = 1, cex = 0.7, line = 4.5, text = paste0("slope test: ", round(currentProject()$regressionModel[j,4], 2)))
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
                
                min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                
                max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                
                abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                
                mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                
                mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[j,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[j,6],3)))
                mtext(side = 1, cex = 0.7, line = 4.5, text = paste0("slope test: ", round(currentProject()$regressionModel[j,4], 2)))
                
              }
              
              title(input$standardIn, outer=TRUE, cex = 1.5)
              
              dev.off()
              
            } else {}
            
            info <- sprintf("%d%% done", round(50))
            setTkProgressBar(pb, 50, sprintf("Export (%s)", info), info)
            
            ######
            
            two <-intersect(which(currentProject()$nbCalib == 2), sapply(1: length(input$MachDriftElementToExport), function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))}))
            
            two <- input$MachDriftElementToExport[two]
            
            nbGraph <- floor(length(two)/6)
            
            nRest <- length(two)%%6
            
            if(nbGraph > 0){
              
              for(i in 1: nbGraph){
                
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
                  
                  min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                  
                  max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                  
                  currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                  
                  abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                  
                  mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                  
                  mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[j,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[j,6],3)))
                  
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
                
                min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                
                max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                
                abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                
                mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                
                mtext(side = 1, cex = 0.7, line = 3, text = paste0("Y (Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[j,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[j,6],3)))
                
              }
              
              title(input$standardIn, outer=TRUE, cex = 1.5)
              
              dev.off()
              
            } else {}
            
            info <- sprintf("%d%% done", round(70))
            setTkProgressBar(pb, 70, sprintf("Export (%s)", info), info)
            
            ######
            
            one <-intersect(which(currentProject()$nbCalib == 1), sapply(1: length(input$MachDriftElementToExport), function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))}))
            
            one <- input$MachDriftElementToExport[one]
            
            nbGraph <- floor(length(one)/6)
            
            nRest <- length(one)%%6
            
            if(nbGraph > 0){
              
              for(i in 1: nbGraph){
                
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
                  
                  min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                  
                  max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                  
                  currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                  
                  abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                  
                  mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
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
                
                min <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) - max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*0.5
                
                max <- (max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], na.rm = TRUE) + max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j], na.rm = TRUE))*1.5
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),j], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),j],coord =  X, lengthSeg = 0.1, xlim =c(min(X),max(X)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[j],"/Cps_", currentProject()$elemStand), xlab = "")
                
                abline(a = currentProject()$regressionModel[j,5], b= currentProject()$regressionModel[j,6], col ="red", lty = 2)
                
                mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
              }
              
              title(input$standardIn, outer=TRUE, cex = 1.5)
              
              dev.off()
              
            } else {}
            
            info <- sprintf("%d%% done", round(80))
            setTkProgressBar(pb, 80, sprintf("Export (%s)", info), info)
            
            ######
            
            zero <-intersect(which(currentProject()$nbCalib == 0), sapply(1: length(input$MachDriftElementToExport), function(x){which(input$MachDriftElementToExport[x] == names(currentProject()$nbCalib))}))
            
            zero <- input$MachDriftElementToExport[zero]
            
            nbGraph <- floor(length(zero)/6)
            
            nRest <- length(zero)%%6
            
            if(nbGraph > 0){
              for(i in 1: nbGraph){
                
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
                  
                  mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                  
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
                
                mtext(side = 3, line = 1, text = currentProject()$listeElem[j])
                
              }
              
              title(input$standardIn, outer=TRUE, cex = 1.5)
              
              dev.off()
              
            } else {}
            
            info <- sprintf("%d%% done", round(100))
            setTkProgressBar(pb, 100, sprintf("Export (%s)", info), info)
            
            setwd(espace1)
            
            close(pb)
            
            res <- tkmessageBox(title = "INFO !",message = "Graphics exported", icon = "info", type = "ok")
            
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
            
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/", input$selectRealign, "/graphics")))
            
            setwd(paste0(projPath$temp,"/Done/samples/", input$selectRealign, "/graphics"))
            
            if(is.null(length(input$RealignElementToExport)) | length(input$RealignElementToExport) == 0){
              tkmessageBox(message = "You need to finish the first step for handling the rest of the filtration procedure!", icon = "error", type = "ok")
            }else{
              pb <- tkProgressBar("Progress bar", "Graphic export in %",
                                  0, 100, 0)
              
              #### single graphic /elmenet #####
              
              setwd(paste0(projPath$temp,"/Done/samples/", input$selectRealign, "/graphics"))
              
              if((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
                
                for(i in 1:length(input$RealignElementToExport)){
                  
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
                  
                  if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]}))))) == 0){
                    plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                    text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                  } else {
                    
                    ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(j){tabProvSample$temp[[j]][,input$RealignElementToExport[i]]})), na.rm = TRUE))
                    
                    xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(j){tabProvSample$temp[[j]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(j){tabProvSample$temp[[j]][,1]}))))

                    lapply(1:length(input$ReplicateSample), function(x){
                      
                      plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[i] , col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
                      
                      par(new = TRUE)
                      
                      
                    })            
                    
                    legend("topright", legend = input$ReplicateSample, col = sapply(1:length(input$ReplicateSample), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]}), lty = c(1,1))
                  }
                  
                  dev.off()
                }
                
              }else{
                
                for(y in 1:length(input$RealignElementToExport)){
                  
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
                  
                  if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
                    plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                    text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                  }else{
                    
                    ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))

                    xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                    
                    lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                      
                      plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])

                      par(new = TRUE)
                      
                    })
                    
                    plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
                    
                    legend("topright", legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
                  }
                  
                  dev.off()
                }
                
              }
              
              info <- sprintf("%d%% done", round(50))
              setTkProgressBar(pb, 50, sprintf("Export (%s)", info), info)
              
              #### whole graphic #####

              setwd(paste0(projPath$temp,"/Done/samples/", input$selectRealign, "/graphics"))

              nbGraph <- floor(length(input$RealignElementToExport)/5)

              nRest <- length(input$RealignElementToExport)%%5

              for(i in 1: nbGraph){

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
                    
                    if((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
                      
                      plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
                      
                      legend(0,1, legend = input$ReplicateSample, col = sapply(1:length(input$ReplicateSample), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]}), lty = 1, pch = 1, lwd = 1)
                      
                      if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]}))))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                        text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                      } else{
                        ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE))
                        
                        xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))))
                        
                        lapply(1:length(input$ReplicateSample), function(x){
                          
                          plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y-i] , col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
                          
                          par(new = TRUE)
                          
                        })
                        
                        
                      }
                      
                    } else{
                      
                      plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
                      
                      legend(0,1, legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
                      
                      if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "", main = input$RealignElementToExport[y])
                        text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                      } else{

                        ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))

                        xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))

                        lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){

                          plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])

                          par(new = TRUE)

                        })

                        plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )

                      }
                      
                    }
                  } else{
                    
                    if((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){

                      if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]}))))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                        text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                      } else{
                        ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE))

                        xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))))

                        lapply(1:length(input$ReplicateSample), function(x){

                          plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y-i] , col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )

                          par(new = TRUE)

                        })


                      }

                    } else{

                      if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                        text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                      } else{
                        
                        ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))

                        xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))

                        lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){

                          plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])

                          par(new = TRUE)

                        })

                        plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
                        
                      }

                    }

                  }
                }

                title(input$selectRealign, outer=TRUE, cex = 1.5)

                dev.off()

              }

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

                if(y == length(input$RealignElementToExport)){
                  
                  if((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
                    
                    plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")
                    
                    legend(0,1, legend = input$ReplicateSample, col = sapply(1:length(input$ReplicateSample), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]}), lty = 1, pch = 1, lwd = 1)
                    
                    if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]}))))) == 0){
                      plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                      text(0,1, labels = "No data different from NA for this element", cex = 2)
                    } else{
                      ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE))
                      
                      xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))))
                      
                      lapply(1:length(input$ReplicateSample), function(x){
                        
                        plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y-i] , col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
                        
                        par(new = TRUE)
                        
                        
                      })
                    }
                    
                  } else {
                    
                    plot(-1,-1, xlim = c(0,10), ylim = c(0,2), axes = FALSE, xlab = "", ylab = "")

                    legend(0,1, legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))

                    if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
                      plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                      text(0,1, labels = "No data different from NA for this element", cex = 2)
                    }else{
                      
                      ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
                      
                      xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                      
                      lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                        
                        plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
                        
                        par(new = TRUE)
                        
                      })
                      
                      plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
                      
                    }
                    
                    
                  }


                } else{
                  if((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){

                    par(mar = c(5.1,4.1,1.5,1.5))

                    if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]}))))) == 0){
                      plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                      text(0,1, labels = "No data different from NA for this element", cex = 2)
                    } else{
                      ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]]})), na.rm = TRUE))

                      xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(x){tabProvSample$temp[[x]][,1]}))))

                      lapply(1:length(input$ReplicateSample), function(x){

                        plot(tabProvSample$temp[[x]][,1],tabProvSample$temp[[x]][,input$RealignElementToExport[y-i]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y-i] , col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )

                        par(new = TRUE)


                      })
                    }

                  }else{

                    if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]]}))))) == 0){
                      plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "",main = input$RealignElementToExport[y])
                      text(0,1, labels = "No data different from NA for this element", cex = 2)
                    }else{

                      ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$RealignElementToExport[y]]})), na.rm = TRUE))
                      
                      xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                      
                      lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                        
                        plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$RealignElementToExport[y]] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = input$RealignElementToExport[y] ,col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])])
                        
                        par(new = TRUE)
                        
                      })
                      
                      plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$RealignElementToExport[y]], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2, main = input$RealignElementToExport[y] )
                      
                    }

                  }
                }


              }

              title(input$selectRealign, outer=TRUE, cex = 1.5)

              dev.off()
              
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
    
    #################################
    ########### Création Données ####
    #################################
    
    tempProj <- reactiveValues(temp = NULL) # temporary elementR project
    projPath <- reactiveValues(temp = NULL) # path of the project
    projChar <- reactiveValues(temp = NULL) # a vector to display project features  
    
    rankStandard <- reactiveValues(temp = NULL) # to generate the rank of standard file
    rankSample <- reactiveValues(temp = NULL) # to generate the rank of sample file
    geneRMachineCorr <- reactiveValues(temp = NULL) # a vector of string created by geneR for evaluate teh machine correction chosen by user
    
    generRRealign <- reactiveValues(temp = 0) # a vector of string created by geneR for the numericInput in the realign step
    
    flagStart <- reactiveValues(temp = c(0,0)) # Flag the first is for create a project (creation, loading): 0, 1 already created or loaded but not validated, 2 validated
    startSession <- reactiveValues(temp = 0) # flag for the validation of first step
    flagStandard <- reactiveValues(temp = NULL) # flag standards
    flagSampleDetail <- reactiveValues(temp = NULL) # flag for sample: a list of vector for each sample, flagSampleDetail = list(c(flag for spot mode, flag for raster mode), ....)
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
    
    # create a project with a upload directory -> set the flag and the color for the rest of the procedure
    observe({
      if(is.null(input$createProjButton)){      
      }else{

        if(input$createProjButton != 0){
          isolate({
            
            sauvegarde <- getwd() 
            
            projPath$temp <- tk_choose.dir()
            
            runEx$temp <- 0
            
            if(is.na(projPath$temp)){
              
              flagStart$temp[2] <- 0
              flagStart$temp[1] <- 0
              
            } else {
              
              if(sum(c("standards","samples")%in%dir(projPath$temp))!=2){
                
                tkmessageBox(message = "A folder should contain two subfolder 'standards' & 'samples' to create an elementR project '[^_-]'", icon = "error", type = "ok")
                
              } else {
                
                flagStart$temp[1] <- 1
                flagStart$temp[2] <- 0  
                
                tempProj$temp <- elementR_project$new(projPath$temp)
                
                projChar$temp <- list(1, "Type of action: Projet creation", projPath$temp, projPath$temp, dir(paste(projPath$temp,"/standards",sep="")), dir(paste(projPath$temp,"/samples",sep="")), tempProj$temp$listeElem)
                
                currentProject <- reactive({     
                  tempProj$temp
                })
                
                flagStandard$temp <- currentProject()$flag_stand
                flagSampleDetail$temp <- currentProject()$flag_Sample
                flagRealign$temp <- currentProject()$flagRealign 
                validCorrection$temp <- currentProject()$flagMachineCorrection
                
                flagSample$temp <- sapply(1: length(currentProject()$flag_Sample), function(x){
                  if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){return(1)}
                  else{return(0)}
                })  
                
                tempO <- list()
                
                for (i in 1: length(currentProject()$flag_Sample)){
                  tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
                  names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
                }
                
                colorReplicate$temp <- tempO
              }
            }
          })
        } else {}
      }
    }) #observe
    
    # create a project based on the example
    observe({
      if(is.null(input$runExampleNew)){      
      }else{
        
        if(input$runExampleNew != 0){
          isolate({
            
            sauvegarde <- getwd() 
            
            projPath$temp <- paste0(system.file("", package="elementR"), "Example_Session")
            
            runEx$temp <- 1
            
            if(is.na(projPath$temp)){
              
              flagStart$temp[2] <- 0
              flagStart$temp[1] <- 0
              
            } else {
              
              if(sum(c("standards","samples")%in%dir(projPath$temp))!=2){
                
                tkmessageBox(message = "A folder should contain two subfolder 'standards' & 'samples' to create an elementR project '[^_-]'", icon = "error", type = "ok")
                
              } else {
                
                flagStart$temp[1] <- 1
                flagStart$temp[2] <- 0  
                
                tempProj$temp <- elementR_project$new(projPath$temp)
                
                projChar$temp <- list(1, "Type of action: Projet creation", projPath$temp, projPath$temp, dir(paste(projPath$temp,"/standards",sep="")), dir(paste(projPath$temp,"/samples",sep="")), tempProj$temp$listeElem)
                
                currentProject <- reactive({     
                  tempProj$temp
                })
                
                flagStandard$temp <- currentProject()$flag_stand
                flagSampleDetail$temp <- currentProject()$flag_Sample
                flagRealign$temp <- currentProject()$flagRealign 
                validCorrection$temp <- currentProject()$flagMachineCorrection
                
                flagSample$temp <- sapply(1: length(currentProject()$flag_Sample), function(x){
                  if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){return(1)}
                  else{return(0)}
                })  
                
                tempO <- list()
                
                for (i in 1: length(currentProject()$flag_Sample)){
                  tempO[[i]] <- rainbow(length(currentProject()$samples[[i]]$rep_Files))
                  names(tempO[[i]]) <- currentProject()$samples[[i]]$rep_Files
                }
                
                colorReplicate$temp <- tempO
              }
            }
          })
        } else {}
      }
    }) #observe
    
    # upload a project already filtered and set color and flags
    observe({
      if(is.null(input$loadProjButton)){
      }else{
        if(input$loadProjButton !=0){
          isolate({
              
            tempoR1 <- tk_choose.files(default = getwd(), caption = "Select files",
                                         multi = FALSE, filters = matrix(c("R data", ".RData"), 1,2), index = 1)

            
            if(length(tempoR1) != 0){
              
              flagStart$temp[2] <- 1
              flagStart$temp[1] <- 0
              
              load(tempoR1)
              
              tempProj$temp <- myProject
              
              projChar$temp <- list(2, "Type of action: Modification of an existing elementR project", tempoR1, tempProj$temp$name, tempProj$temp$folderPath, tempProj$temp$standardsFiles, tempProj$temp$samplesFiles, tempProj$temp$listeElem)
              
              currentProject <- reactive({
                tempProj$temp
              })
              
              projPath$temp <- currentProject()$folderPath
              
              tempO <- list()
              
              for (i in 1: length(currentProject()$flag_Sample)){
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
            }
          })
        } else {}


        } 
    }) #observe
    
    # upload the example
    observe({
      if(is.null(input$runExampleLoad)){
      }else{
        if(input$runExampleLoad != 0){
          isolate({
            
            tempoR1 <- paste0(system.file("", package="elementR"), "Done/Example_Session.RData")
            
            if(length(tempoR1) != 0){
              
              flagStart$temp[2] <- 1
              flagStart$temp[1] <- 0
              
              load(tempoR1)
              
              tempProj$temp <- myProject
              
              projChar$temp <- list(2, "Type of action: Modification of an existing elementR project", tempoR1, tempProj$temp$name, tempProj$temp$folderPath, tempProj$temp$standardsFiles, tempProj$temp$samplesFiles, tempProj$temp$listeElem)
              
              currentProject <- reactive({
                tempProj$temp
              })
              
              projPath$temp <- currentProject()$folderPath
              
              tempO <- list()
              
              for (i in 1: length(currentProject()$flag_Sample)){
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
            }
          })
        } else {}
        
        
      } 
    }) #observe
    
    # finalize the creation of the project by setting elements, calibration files, rank etc
    observe({
      if(is.null(input$validDonne)){
        } else {

        isolate({

          if(input$validDonne != 0){

            flagStart$temp[1] <- 3
            flagStart$temp[2] <- 0
            startSession$temp <- 1

            geneRMachineCorr$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$listeElem), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, generRRealign$temp))

            tempoR <- list()

            for(i in 1:length(currentProject()$samplesFiles)){

              tempoR[[i]] <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$samples[[i]]$rep_Files), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, geneRMachineCorr$temp, unlist(tempoR)))

              names(tempoR[[i]]) <- currentProject()$samples[[i]]$rep_Files
            }

            generRRealign$temp <- tempoR

            colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))

            color$temp <- colfunc(length(currentProject()$listeElem))
            names(color$temp) <- currentProject()$listeElem

            espace1 <- getwd()
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done")))
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done/standards")))
            suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples")))

            lapply(1:length(currentProject()$standardsFiles), function(y){
              temporaire <- currentProject()$standardsFiles[y]
              suppressWarnings(dir.create(paste0(projPath$temp,"/Done/standards/", temporaire)))
            })

            lapply(1:length(currentProject()$samples), function(y){
              suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/",currentProject()$samplesFiles[y])))
              lapply(1:length(currentProject()$samples[[y]]$rep_Files), function(x){
                temporaire <-currentProject()$samples[[y]]$rep_Files[x]
                suppressWarnings(dir.create(paste0(projPath$temp,"/Done/samples/", currentProject()$samplesFiles[y],"/",temporaire)))
              })
            })

            setwd(espace1)

            currentProject()$setElemStand(elem = input$internStand)


          } else {}

        })

      }
    }) #observe

    # finalize the loading of the project 
    observe({
      if(is.null(input$validDonne2)){
        
      }else{
        
        isolate({
          
          if(input$validDonne2 != 0){
            
            flagStart$temp[2] <- 3
            flagStart$temp[1] <- 0
            startSession$temp <- 1  
            
            geneRMachineCorr$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$listeElem), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, generRRealign$temp))
            
            tempoR <- list()
            
            for(i in 1:length(currentProject()$samplesFiles)){
              
              tempoR[[i]] <- geneR(choice = letters, lengthComb = 5, NBComb = length(currentProject()$samples[[i]]$rep_Files), toAvoid = c(waste$temp, valeurColor$temp, geneRMachineCorr$temp, rankStandard$temp, rankSample$temp, unlist(tempoR)))
              
              names(tempoR[[i]]) <- currentProject()$samples[[i]]$rep_Files
            }
            
            generRRealign$temp <- tempoR  
            
            colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))        
            
            color$temp <- colfunc(length(currentProject()$listeElem))
            names(color$temp) <- currentProject()$listeElem
          } else {}
          
        })
      }
    }) #observe
    
    # indicate to elementR that user wants to begin another project ->  reinitilize flag ...
    observe({
      if(is.null(input$SuppDonne)){
        
      } else {
        
        if(input$SuppDonne != 0){
          
          isolate({
            flagSupp$temp <- 1
            
            waste$temp <- unlist(c(waste$temp, valeurColor$temp, rankStandard$temp, rankSample$temp, geneRMachineCorr$temp, generRRealign$temp))
            
            flagStart$temp[c(1,2)] <- 0
            
            currentProject()$setflagMachineCorrection(x = 0)
            currentProject()$setflagStand (place = 1:length(currentProject()$standardsFiles), value = 0)
            lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
            lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate= x, type = "raster",value = 0)})       
            lapply(1:length(currentProject()$flag_Sample), function(x){
              lapply(1:length(currentProject()$flag_Sample[[x]]), function(i){
                currentProject()$setflagSample(sample = x, replicate = i, value = 0)
              })                
            })
            startSession$temp <- NULL
            flagStandard$temp <- NULL
            flagSampleDetail$temp <- NULL
            flagRealign$temp <- NULL 
            validCorrection$temp <- NULL
            calibFile$temp <- NA
            
            runEx$temp <- 0
            
            color$temp <- NULL
            
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

            
          })
          
        } else {}
      }
      
    }) #observe
    
    # choose and set the calibration file for the rest of the procedure
    observe({
      if(is.null(input$calibFile)){
        
      } else {
        
        if(input$calibFile > 0){
          
          isolate({
            if(runEx$temp == 0){
              Filters <- matrix(c("Text", ".csv", "OO sheet", ".ods", "Excel sheet", ".xls", "Excel sheet", ".xlsx"), 4, 2, byrow = TRUE)

              temp <- tk_choose.files(default = paste0(projChar$temp[[3]], "/calibration file"), caption = "Select files",
                                      multi = FALSE, filters = Filters, index = 1)
              
              if(length(temp) == 0){
                
              } else {
                currentProject()$setEtalon(x = temp)
              }
            } else {
              
              temp <- paste0(system.file("", package="elementR"), "Example_Session/Calibration_File_NIST612.csv")
              
              currentProject()$setEtalon(x = temp)
              
            }
          })
          
         
          

          
        } else {}
      }
    }) #observe
    
    # diplay the name of the calibration file
    observe({
      
      input$calibFile
      
      output$InformCalib <- renderUI({
        if(is.null(currentProject())){
          
        } else{
          if(is.na(currentProject()$EtalonPath)){
            NULL
          } else {
            
            calibFile$temp <- unlist(str_split(currentProject()$EtalonPath, "/"))[length(unlist(str_split(currentProject()$EtalonPath, "/")))]
            return(p(paste0("Calibration file loaded: ", calibFile$temp)))     
          }
        }
        
      })
    }) #observe
    
    # set the rank of standards and samples
    observe({
      input$calibFile
      calibFile$temp
      
      if(is.null(rankStandard$temp)){      
      } else if(is.null(rankSample$temp)){      
      } else {
        if(is.na(rankStandard$temp[length(standardFile$temp)]) | is.na(rankSample$temp[length(SampleFile$temp)])){
          
        }else{
          if(is.null(eval(parse(text = paste0("input$",rankStandard$temp[1]))))){
            
          }else{
            
            tempCalib <- sapply(1:length(standardFile$temp), function(x){
              
              eval(parse(text = paste0("input$",isolate({rankStandard$temp})[x])))
              
            })
            
            names(tempCalib) <- standardFile$temp
            
            tempSample <- sapply(1:length(SampleFile$temp), function(x){
              eval(parse(text = paste0("input$",rankSample$temp[x])))
            })
            names(tempSample) <- SampleFile$temp

            isolate({
              if(length(which(is.na(tempCalib))) == 0 & length(which(is.na(tempSample))) == 0 & length(which(tempCalib == 0)) == 0 & length(which(tempSample == 0)) == 0 & length(which(duplicated(c(tempSample,tempCalib))) == TRUE) == 0 & !is.na(calibFile$temp)){
                flagStart$temp[1] <- 2
                currentProject()$setRank(type = "standard", value = tempCalib)
                currentProject()$setRank(type = "sample",  value = tempSample)
              }else{
                
                currentProject()$setRank(type= "standard", value = tempCalib)
                currentProject()$setRank(type = "sample", value = tempSample)
              }
            })
          }
        }
      }
      
    }) #observe
    
    #geneR rankStandard$temp & rankSample$temp
    observe({
      if(flagStart$temp[1] == 1){
        
        isolate({
          standardFile$temp <- dir(paste0(projPath$temp, "/standards"))
          temp <- dir(paste0(projPath$temp, "/samples"), recursive = TRUE)
          SampleFile$temp <- sapply(1:length(temp), function(x){str_split(temp, "/")[[x]][2]})
          
          rankStandard$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(standardFile$temp), toAvoid = c(waste$temp, valeurColor$temp, rankSample$temp, geneRMachineCorr$temp, generRRealign$temp))
          rankSample$temp <- geneR(choice = letters, lengthComb = 5, NBComb = length(SampleFile$temp), toAvoid = c(waste$temp, valeurColor$temp, rankStandard$temp, geneRMachineCorr$temp, generRRealign$temp))

        })
        
      } else {}
    }) #observe
    
    # set the ElemStand$temp
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
    
    # Go to the "config" tab when user click on the actionButton with id = SetParam
    observe({
      if(!is.null(input$SetParam)){
        if(input$SetParam > 0){
          updateTabItems(session, "tab", selected = "Config")
        }
      } else {}
    })

    # all displayed elements to users
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
              
              elem <- colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1]
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
                    p("2. Choose the internal standart element"),
                    selectInput("internStand", label = "", 
                                choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1], 
                                selected = ElemStand$temp),
                    p("3. Choose the calibration file"),
                    div(
                      div(actionButton("calibFile", "Search"), style = "display:inline-block"),
                      div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
                    ),
                    br(),
                    p("4. Sort the files in their analyzed rank"),
                    column(6, 
                           p("Standard files:"),
                           lapply(1:length(standardFile$temp), function(i){
                             div(style="height: 50px;",
                                 column(8,br(),p(standardFile$temp[i])), 
                                 column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", 0, min = 0))) 
                             ) #div
                           })
                    ),
                    column(6, 
                           p("Samples files:"),
                           lapply(1:length(SampleFile$temp), function(i){
                             div(style="height: 50px;",
                                 column(8,br(),p(SampleFile$temp[i])), 
                                 column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "",0, min = 0))) 
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
              
            }else{
              if(currentProject()$standardRank[1] != 0){

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
                      p("2. Choose the internal standart element"),
                      selectInput("internStand", label = "", 
                                  choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1], 
                                  selected = ElemStand$temp),
                      p("3. Choose the calibration file"),
                      div(
                        div(actionButton("calibFile", "Search"), style = "display:inline-block"),
                        div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
                      ),
                      br(),
                      p("4. Sort the files in their analyzed rank"),
                      column(6, 
                             p("Standard files:"),
                             lapply(1:length(standardFile$temp), function(i){
                               div(style="height: 50px;",
                                   column(8,br(),p(standardFile$temp[i])), #column
                                   column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", currentProject()$standardRank[i], min = 0))) #column
                               )
                             })
                      ),
                      column(6, 
                             p("Samples files:"),
                             lapply(1:length(SampleFile$temp), function(i){
                               div(style="height: 50px;",
                                   column(8,br(),p(SampleFile$temp[i])), #column
                                   column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "", currentProject()$sampleRank[i], min = 0))) #column
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
                
              }else{

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
                      p("2. Choose the internal standart element"),
                      selectInput("internStand", label = "", 
                                  choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1], 
                                  selected = ElemStand$temp),
                      p("3. Choose the calibration file"),
                      div(
                        div(actionButton("calibFile", "Search"), style = "display:inline-block"),
                        div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
                      ),
                      br(),
                      p("4. Sort the files in their analyzed rank"),
                      column(6, 
                             p("Standard files:"),
                             lapply(1:length(standardFile$temp), function(i){
                               div(style="height: 50px;",
                                   column(8,br(),p(standardFile$temp[i])), #column
                                   column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", 0, min = 0))) #column
                               ) 
                             })
                      ),
                      column(6, 
                             p("Samples files:"),
                             lapply(1:length(SampleFile$temp), function(i){
                               div(style="height: 50px;",
                                   column(8,br(),p(SampleFile$temp[i])), #column
                                   column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "",0, min = 0))) #column
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
            } 
          } else {
            
            placeNIST <- c(1,5,9)
            placeSAMPLE <- c(2,3,4,6,7,8,10,11,12)
            
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
                  p("2. Choose the internal standart element"),
                  selectInput("internStand", label = "", 
                              choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1], 
                              selected = "Ca43"),
                  p("3. Choose the calibration file"),
                  div(
                    div(actionButton("calibFile", "Search"), style = "display:inline-block"),
                    div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
                  ),
                  br(),
                  p("4. Sort the files in their analyzed rank"),
                  column(6, 
                         p("Standard files:"),
                         lapply(1:length(standardFile$temp), function(i){
                           div(style="height: 50px;",
                               column(8,br(),p(standardFile$temp[i])), #column
                               column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", placeNIST[i], min = 0))) #column
                           ) 
                         })
                  ),
                  column(6, 
                         p("Samples files:"),
                         lapply(1:length(SampleFile$temp), function(i){
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
              p("2. Choose the internal standart element"),
              selectInput("internStand", label = "", 
                          choices = colnames(readData(paste(projPath$temp, "/standards",dir(paste0(projPath$temp, "/standards"))[1],sep="/")))[-1], 
                          selected = ElemStand$temp),
              p("3. Choose the calibration file"),
              div(
                div(actionButton("calibFile", "Search"), style = "display:inline-block"),
                div(uiOutput("InformCalib"), style = "display:inline-block; margin-left:10px")
              ),
              br(),
              p("4. Sort the files in their analyzed rank"),
              column(6, 
                     p("Standard files:"),
                     lapply(1:length(standardFile$temp), function(i){
                       div(style="height: 50px;",
                           column(8,br(),p(standardFile$temp[i])), #column
                           column(4,div(style="height: 10px;",numericInput(rankStandard$temp[i], "", currentProject()$standardRank[i], min = 0))) #column
                       ) 
                     })
              ),
              column(6, 
                     p("Samples files:"),
                     lapply(1:length(SampleFile$temp), function(i){
                       div(style="height: 50px;",
                           column(8,br(),p(SampleFile$temp[i])), #column
                           column(4,div(style="height: 10px;",numericInput(rankSample$temp[i], "", currentProject()$sampleRank[i], min = 0))) #column
                       ) 
                     })
              ),              
              p("5. Valid the created project"),
              column(3, offset = 4, actionButton("validDonne","Go filtering !"))
              
              
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
        
        x <- paste0(lapply(1:length(projChar$temp[[5]]), function(i){paste0(projChar$temp[[5]][i], "(",currentProject()$standardRank[i], ")", collapse = " ")}))
        
        temp <- paste(x, collapse = " ", sep = " ")
        
        ## explanation for samples
        
        x2 <- currentProject()$samplesFiles
        
        temp2 <- lapply(1:length(currentProject()$samplesFiles), function(x){
          
          lapply(1:length(currentProject()$samples[[x]]$rep_Files), function(i){
            
            temp5 <- currentProject()$sampleRank[which(names(currentProject()$sampleRank) == currentProject()$samples[[x]]$rep_Files[i])]
            
            paste0(currentProject()$samples[[x]]$rep_Files[i], "(", temp5, ")", collapse = " ")
            
          })
          
        })
        
        temp3 <- lapply(1:length(currentProject()$samplesFiles), function(x){
          paste(unlist(temp2[[x]]), sep = " ", collapse = " ")
        })
        
        temp4 <- lapply(1:length(currentProject()$samplesFiles), function(x){
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
                     p(icon("share "), paste0("Path of the session to be filtered: ", projChar$temp[[4]])),
                     p(icon("share "), paste0("Standard files (and their rank) within the project: ", temp)),
                     p(icon("share"), "Sample files (and their rank) within the project: "),
                     lapply(1:length(x2), function(i){
                       column(12, offset = 1, p(temp4[[i]]))
                     }),
                     p(icon("share"), paste0("Calibration file: ", calibFile$temp, collapse = " ")),
                     p(icon("share"), paste0("Internal standard element: ", currentProject()$elemStand))
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
              p(paste0("Project loaded: ",projPath$temp)),
              br(),
              column(3, offset = 4, actionButton("validDonne2","Go filtering !"))
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
        
        x <- paste0(lapply(1:length(currentProject()$standardsFiles), function(i){paste0(currentProject()$standardsFiles[i], "(",currentProject()$standardRank[i], ")", collapse = " ")}))
        
        temp <- paste(x, collapse = " ", sep = " ")
        
        ## explanation for samples
        
        x2 <- currentProject()$samplesFiles
        
        temp2 <- lapply(1:length(currentProject()$samplesFiles), function(x){
          
          lapply(1:length(currentProject()$samples[[x]]$rep_Files), function(i){
            
            temp5 <- currentProject()$sampleRank[which(names(currentProject()$sampleRank) == currentProject()$samples[[x]]$rep_Files[i])]
            
            paste0(currentProject()$samples[[x]]$rep_Files[i], "(", temp5, ")", collapse = " ")
            
          })
          
        })
        
        temp3 <- lapply(1:length(currentProject()$samplesFiles), function(x){
          paste(unlist(temp2[[x]]), sep = " ", collapse = " ")
        })
        
        temp4 <- lapply(1:length(currentProject()$samplesFiles), function(x){
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
                     p(icon("share"), "You are editing or finishing a projet"),
                     p(icon("share"), paste0("Path of the project: ", projChar$temp[[3]])),
                     p(icon("share"), paste0("Standard files (and their rank) within the project: ", temp)),
                     p(icon("share"), "Sample files (and their rank) within the project: "),
                     lapply(1:length(x2), function(i){
                       column(12, offset = 1, p(temp4[[i]]))
                     }),
                     p(icon("share"), paste0("Calibration file: ", calibFile$temp, collapse = " ")),
                     p(icon("share"), paste0("Internal standard element: ", currentProject()$elemStand))
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
    
    # create the currentProject element
    currentProject <- reactive({
      if(is.null(input$createProjButton)){      
      }else if(is.null(input$loadProjButton)){      
      }else{
        input$create
        input$load      
        tempProj$temp
        
      }        
    })
    
    #######################
    ##### NISTS ###########
    #######################
    
    Temp <- reactiveValues(t = NULL)
    Temp0 <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$bins
    Temp1 <- reactiveValues(t = NULL)  # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$plat[1]
    Temp2 <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$plat[2]
    dataPlot2 <- reactiveValues(dat = NULL) # a matrix corresponding to the filtered
    
    elemUsed <- reactiveValues(temp = 0) # elements to display in plot

    BAV <- reactiveValues(temp = 0)
    LOD <- reactiveValues(temp = 0)
    
    # set elemUsed
    observe({
      if(is.null(input$selectall)){      
      }else if(is.null(input$checkGroup)){elemUsed$temp <- ElemStand$temp
      }else{
        if(input$selectall%%2 == 0 & length(input$checkGroup) != length(currentProject()$listeElem)){
          isolate({
            elemUsed$temp <- input$checkGroup
            updateSliderInput(session, "bins", value = input$bins)
            updateSliderInput(session, "plat", value = input$plat)
          })
          
        } else {}
      }
    }) #observe  
    
    # define output$Standards1
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(startSession$temp)){      
      }else{
        if(startSession$temp == 0){
          output$Standards1 <- renderUI({NULL})        
          output$Standards2 <- renderUI({NULL})
        } else {}
        if(startSession$temp ==1){
          
          output$Standards1 <- renderUI({
            
            fluidRow(
              box(
                width=12,
                background = "green",            
                height=85,                
                column(5, class = "class3",
                       div(h3(icon("flask"),"Step 2. Standard replicate filtering"), style = "display: inline-block;")
                ),
                column(3, class = "class1",
                       p(icon("eye"), "Select standart replicate"),
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
      }
    }) #observe

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
                            div(p(paste0("B.A.V.*: ", " ",  " ",round(BAV$temp[grep(input$listeElem, names(BAV$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px"),
                            div(p(paste0("L.O.D.**: ", " ",  " ",round(LOD$temp[grep(input$listeElem, names(LOD$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
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
    
    # set output$Standards1 and define output$Standards2 output$distPlot & output$distPlot2
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$standardIn)){      
      }else if(is.null(startSession$temp)){      
      }else{
        if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0 & length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){ 
          
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
                           div(h3(icon("flask"),"Step 2. Standard replicate filtering"), style = "display: inline-block;")
                    ),
                    column(3,
                           selectInput("standardIn", "Select standart replicate" ,  as.matrix(currentProject()$standardsFiles),multiple = FALSE, width = '100%') 
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
                             div(h3(icon("flask"),"Step 2. Standard replicate filtering"), style = "display: inline-block;")
                      ),
                      column(3, class = "class1",
                             p(icon("eye"), "Select standart replicate"),
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
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                  
                  minB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                  maxB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]
                  
                  minP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                  maxP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]           
                  
                  value1 <- c(0, (maxB - minB)/6)
                  value2 <- c((maxP - minP)*2/6,(maxP - minP)*4/6)
                  step <- currentProject()$standards[[1]]$setRep_pas()              
                  
                  
                  fluidRow(
                    column(8, style  = "padding-right: 5px"  ,                       
                           box(
                             title = list(icon("share"),"Background and plateau limits selection"),
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
                                                  lapply(1:length(currentProject()$listeElem), function(x){
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
                             title = list(icon("share"),"Filtered data verification"),
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
                  
                  } else {}
                })             
                
                output$distPlot <- renderPlot({
                  
                  par(mar = c(3,3.5,1.75,0))              
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                    
                    maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data, na.rm = TRUE)
                    
                    minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    
                    if(is.null(input$checkGroup)){}
                    else{
                      plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                      mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                      mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                      mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                      
                      if(length(input$checkGroup) > 1){
                        lapply(2:length(input$checkGroup), function(x){
                          par(new = TRUE)
                          plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                        })
                      } 
                      
                      if(!is.null(input$bins) & !is.null(input$plat)){
                        if(is.na(input$bins[1]) | is.na(input$bins[2])| is.na(input$plat[1]) | is.na(input$plat[2])){}
                        else{
                          Temp$t  <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[1])[[2]]
                          Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[2])[[2]]
                          Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[1])[[2]]
                          Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[2])[[2]]                
                          
                        }
                      }
                      
                      rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                      rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                      
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                      
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,x],  cex = 3, col ="grey")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,x], cex = 3, col ="grey")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
                    }
                    
                  }                
                }, height = 400)
                
                output$distPlot2 <- renderPlot({
                  input$valRemplace
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
                      
                      if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                        if(length(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge) == 1){
                        if(!is.na(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge)){
                          if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                          } else if(input$CourbeNIST == "Blank removed"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
                          } else {}
                        } else {}
                      } else {
                        if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                          abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                          rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                        } else if(input$CourbeNIST == "Blank removed"){
                          abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                          rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
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
                             div(h3(icon("flask"),"Step 2. Standard replicate filtering"), style = "display: inline-block;")
                      ),
                      column(3,  class = "class1",
                             p(icon("eye"), "Select standart replicate"),
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
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                  
                  input$standardIn
                  
                  minB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                  maxB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]
                  
                  minP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                  maxP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]           
                  
                  value1 <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins
                  value2 <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat
                  step <- currentProject()$standards[[1]]$rep_pas          
                  
                  fluidRow(
                    column(8, style  = "padding-right: 5px",                     
                           box(
                             title = list(icon("share"),"Background and plateau limits selection"),
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
                                                  lapply(1:length(currentProject()$listeElem), function(x){
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
                             title = list(icon("share"),"Filtered data verification"),
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
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                    if(!is.matrix(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data) & !is.data.frame(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)){
                    
                  }else{
                    
                    maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data, na.rm = TRUE)
                    
                    minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    
                    if(is.null(input$checkGroup)){
                      
                    }else{
                      plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                      mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                      mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                      mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                      
                      if(length(input$checkGroup) > 1){                
                        lapply(2:length(input$checkGroup), function(x){
                          par(new = TRUE)
                          plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                        })                
                      } else {} 
                      
                      if(is.null(input$bins) | is.null(input$plat)){
                        
                      }else{
                        if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
                          
                        }else{
                          Temp$t  <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[1])[[2]]
                          Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$bins[2])[[2]]
                          Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[1])[[2]]
                          Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = input$plat[2])[[2]]
                        }
                      }
                      
                      if(length(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1]) != 0){
                        
                        rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)
                        rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                        
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  lty = "dashed", col = "grey", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "grey", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                        abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                        
                        lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1],  currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,x],  cex = 3, col ="grey")})
                        lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,x], cex = 3, col ="grey")})
                        lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
                        lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
                      }   else {}              
                    }
                    
                  }
                  } else {}
                  
                }, height = 400)
                
                output$distPlot2 <- renderPlot({
                  input$valRemplace
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
                      
                      if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                        if(length(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge) == 1){
                        if(!is.na(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge)){
                          if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                          } else if(input$CourbeNIST == "Blank removed"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
                          } else {}
                        } else {}
                      } else {
                        if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                          abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                          rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                        } else if(input$CourbeNIST == "Blank removed"){
                          abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                          rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
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
                             div(h3(icon("flask"),"Step 2. Standard replicate filtering"), style = "display: inline-block;")
                      ),
                      column(3,  class = "class1",
                             p(icon("eye"), "Select standart replicate"),
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
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) == 0){}
                  else{
                    
                    minB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                    maxB <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]
                    
                    minP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[1,1]
                    maxP <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[dim(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data)[1],1]           
                    
                    value1 <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins
                    value2 <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat

                    step <- currentProject()$standards[[1]]$rep_pas 
                    
                    fluidRow(
                      column(8, style  = "padding-right: 5px",                            
                             box(
                               title = list(icon("share"),"Background and plateau limits selection"),
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
                                                    lapply(1:length(currentProject()$listeElem), function(x){
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
                               title = list(icon("share"),"Filtered data verification"),
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
                  
                  if(length(grep(input$standardIn, currentProject()$standardsFiles)) == 0){
                    
                  }else{
                    
                    maxY <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data, na.rm = TRUE)                
                    minX <- min(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    maxX <- max(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], na.rm = TRUE)
                    
                    if(is.null(input$checkGroup)){
                      
                    }else{
                      plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                      mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                      mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                      mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                      
                      if(length(input$checkGroup) > 1){
                        lapply(2:length(input$checkGroup), function(x){
                          par(new = TRUE)
                          plot(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,input$checkGroup[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroup[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                          
                        })
                      }  else {}                
                      
                      if(is.null(input$bins) | is.null(input$plat)){
                        
                      }else{
                        if(is.na(input$bins[1]) | is.na(input$bins[2]) | is.na(input$plat[1]) | is.na(input$plat[2])){
                          
                        }else{
                          Temp$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[1])[[2]]
                          Temp0$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[2])[[2]]
                          Temp1$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[1])[[2]]
                          Temp2$t <- currentProject()$closest(x = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[,1],y = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[2])[[2]]
                        }
                      }
                      
                      rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[2],(1+10/100)*maxY, col = "#8B735564", border = NA)
                      rect(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[1],-maxY,currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[2],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                      
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[1], lty = "dashed", col = "grey", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$bins[2], lty = "dashed", col = "grey", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[1], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                      abline(v = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$plat[2], lty = "dashed", col = "#4F3CBC50", lwd = 2)
                      
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,x], cex = 3, col ="grey")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,x], cex = 3, col ="grey")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,x], cex = 3, col ="#4F3CBC50")})
                      lapply(input$checkGroup, function(x){points(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,x], cex = 3, col ="#4F3CBC50")})
                      
                    }
                    
                  }                
                }, height = 400)
                
                output$distPlot2 <- renderPlot({
                  input$valRemplace
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
                      
                      if(length(grep(input$standardIn, currentProject()$standardsFiles)) != 0){
                        if(length(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge) == 1){
                          if(!is.na(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge)){
                            if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                              abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                            } else if(input$CourbeNIST == "Blank removed"){
                              abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
                            } else {}
                          } else {}
                        } else {
                          if(input$CourbeNIST == "Raw" | input$CourbeNIST == "Plateau"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge))], col = "#FF000064", border = NA)
                          } else if(input$CourbeNIST == "Blank removed"){
                            abline(a = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2$dat[,1]),currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD[grep(input$listeElem, names(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD))], col = "#FF000064", border = NA)
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
      }
    }) #observe
    
    # calculate dataPlot2
    observe({
      input$valRemplace
      if(is.null(currentProject())){      
      }else if(is.null(input$standardIn)){      
      }else if(is.null(input$CourbeNIST)){      
      }else if(is.null(flagStandard$temp)){      
      }else{
        if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) == 0){        
        } else {        
          if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 0){          
            if(is.null(input$bins)){            
            }else if(is.null(input$plat)){  
            }else if(is.null(Temp$t)){
            }else if(is.null(Temp0$t)){            
            } else if(is.null(Temp1$t)){            
            } else if(is.null(Temp2$t)){            
            }else{
              if(is.finite(Temp$t)){dataPlot2$dat <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$getData(curve = input$CourbeNIST, bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                                     BAV$temp <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge
                                     LOD$temp <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD
              } else{}
            }
          } else{}
          if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] %%2) == 1){
            dataPlot2$dat <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$renderData(curve = input$CourbeNIST)
            BAV$temp <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge            
            LOD$temp <- currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD
          } else{}
          
        }    
      }
    }) #observe
    
    #updateCheckboxGroupInput  checkGroup
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$standardIn)){      
      }else if(is.null(input$selectall)){      
      }else{                  
        if(input$selectall == 0) {
          return(NULL)
        } else if(input$selectall%%2 == 0) {
          updateCheckboxGroupInput(session,"checkGroup","",choices=currentProject()$listeElem, selected = elemUsed$temp)
        }  else {
          updateCheckboxGroupInput(session,"checkGroup","",choices=currentProject()$listeElem,selected = currentProject()$listeElem)
        }
      }  
    }) #observe
    
    # set flagStandard when the standard is saved 
    observe({ 
      if(is.null(currentProject())){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$standardIn)){      
      }else{
        if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) == 0){        
        } else{
          if(input$saveNists > 0){
            isolate({
              flagStandard$temp[which(currentProject()$standardsFiles == input$standardIn)] <- flagStandard$temp[which(currentProject()$standardsFiles == input$standardIn)] + 1
              updateSelectInput(session, "listeElem", selected = input$listeElem)
              updateSelectInput(session, "CourbeNIST", selected = input$CourbeNIST)
              load$temp <- load$temp +1
            }) 
          } else {}

        }      
        updateSelectInput(session, "standardIn", selected = input$standardIn)
      } 
      
    }) #observe

    # to avoid elementR saving data when first delete loaded project
    observe({   
      if(is.null(input$standardIn)){      
      }else{
        isolate({
          if(load$temp == 1){
            load$temp <- load$temp +1
            }
        })
      }
    }) #observe
    
    # Save all the data when the flagStandard is in saved position and delete all data if the flag is in the position of delete
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$standardIn)){      
      }else if(is.null(input$saveNists)){
      }else if(is.null(Temp$t)){
      }else if(is.null(Temp0$t)){
      }else if(is.null(Temp1$t)){
      }else if(is.null(Temp2$t)){
      }else if(is.null(flagStandard$temp)){
      }else{
        if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0){
          
          if(projChar$temp[1] == 2 & load$temp == 1){
            
            if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 0 & flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] != 0){
              isolate({
                currentProject()$setflagMachineCorrection(x = 0)  
                currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 0)
                lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
                lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "raster", value = 0)})
                lapply(1:length(currentProject()$flag_Sample), function(x){
                  lapply(1:length(currentProject()$flag_Sample[[x]]), function(i){
                    currentProject()$setflagSample(sample = x, replicate = i, value = 0)
                  })                
                })
                flagSampleDetail$temp <- currentProject()$flag_Sample
                flagRealign$temp <- currentProject()$flagRealign 
                validCorrection$temp <- currentProject()$flagMachineCorrection 
                currentProject()$set_summarySettings(name = input$standardIn, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = NA, LOD = NA)
              })
              
              
            }  else {}
            
            if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1){
              isolate({
                currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 1)
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setBins(bins = c(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1]))     
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setPlat(plat = c(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1]))
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setDataOutlierFree(bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                currentProject()$set_summarySettings(name = input$standardIn, rank = currentProject()$standardRank[which(names(currentProject()$standardRank) == input$standardIn)], bins1 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], bins2 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], plat1 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], plat2 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], average = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge, LOD = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD)
                currentProject()$standards[[1]]$setRep_pas()
              })
              
            }  else {}
            
          } else {
            
            if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 0 & flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)] != 0 & input$saveNists >0){
              isolate({
                currentProject()$setflagMachineCorrection(x = 0)  
                currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 0)
                lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
                lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "raster", value = 0)})
                lapply(1:length(currentProject()$flag_Sample), function(x){
                  lapply(1:length(currentProject()$flag_Sample[[x]]), function(i){
                    currentProject()$setflagSample(sample = x, replicate = i, value = 0)
                  })                
                })
                flagSampleDetail$temp <- currentProject()$flag_Sample
                flagRealign$temp <- currentProject()$flagRealign 
                validCorrection$temp <- currentProject()$flagMachineCorrection 
                currentProject()$set_summarySettings(name = input$standardIn, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = NA, LOD = NA)
              })
              
              
            }  else {}
            
            if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1 & input$saveNists >0){
              isolate({
                currentProject()$setflagStand (place = which(as.matrix(currentProject()$standardsFiles) == input$standardIn),value = 1)
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setBins(bins = c(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1]))     
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setPlat(plat = c(currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1],currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1]))
                currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$setDataOutlierFree(bins = c(Temp$t, Temp0$t), plat = c(Temp1$t,Temp2$t), rempl = input$valRemplace)
                currentProject()$set_summarySettings(name = input$standardIn, rank = currentProject()$standardRank[which(names(currentProject()$standardRank) == input$standardIn)], bins1 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp$t,1], bins2 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp0$t,1], plat1 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp1$t,1], plat2 = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$data[Temp2$t,1], average = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$BlankAverarge, LOD = currentProject()$standards[[1]]$rep_data[[grep(input$standardIn, currentProject()$standardsFiles)]]$LOD)
                currentProject()$standards[[1]]$setRep_pas()
              })
              
            }  else {}
          }
          

              
         } else {}
      }
        
    }) #observe  
    
    # Go to the next standard replicate
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$standardIn)){      
      }else if(is.null(input$saveNists)){
      }else if(is.null(input$bins)){  
      }else if(is.null(input$plat)){ 
      }else if(is.null(Temp$t)){    
      }else if(is.null(Temp0$t)){      
      }else if(is.null(Temp1$t)){      
      }else if(is.null(Temp2$t)){
      }else if(is.null(flagStandard$temp)){
      }else{
        if(length(which(as.matrix(currentProject()$standardsFiles) == input$standardIn)) != 0){
          if((flagStandard$temp[which(as.matrix(currentProject()$standardsFiles) == input$standardIn)]%%2) == 1 & input$saveNists > 0){
            isolate({
              passage <- currentProject()$standardsFiles[flagStandard$temp%%2 == 0][1]
              if(!is.na(passage)){
                delay(2000,updateSelectInput(session, "standardIn", selected = passage))
              } else {}
            })
          }  else {}
          
        } else {}
      }
      
    }) #observe
    
    #######################
    ## VERIF STANDARDS ####
    #######################
    
    tableauStat <- reactiveValues(temp = NULL) # the matrix with all linear regression parameters
    machineCorrection <- reactiveValues(temp = 0)  # the vector corresponding to the choic of the user to correct or not the maichine drift for each element
    
    zero <- reactiveValues(temp = 0) # the number of element which has 0 standard replicates in the linear regression
    one <- reactiveValues(temp = 0) # the number of element which has 1 standard replicates in the linear regression
    two <- reactiveValues(temp = 0) # the number of element which has 2 standard replicates in the linear regression
    three <- reactiveValues(temp = 0) # the number of element which has at least 3 standard replicates in the linear regression
    
    elemChosen <- reactiveValues(temp = 0) # the element which will be displayed (all the element which need to be corrected + all the element which have two, one or zero replicate)
    
    coord <- reactiveValues(temp = NULL) # rank of the standards in the ICPMS run
    
    flagHR <- reactiveValues(temp = 0) # a value to set graphical parameters (adjust the bar between each element)
    
    # set the elemChosen$temp
    observe({
      input$saveNists
      if(is.null(currentProject())){      
      }else if(is.null(input$SuppDonne)){      
      }else{
        
        if(length(which(currentProject()$flag_stand != 1)) == 0){      
          
          temp <- c(which(tableauStat$temp[,4]< 0.054), which(currentProject()$nbCalib == 2), which(currentProject()$nbCalib == 1), which(currentProject()$nbCalib == 0))
          
          tempOR <- length(temp)  
          
          if(length(temp) !=0 & length(temp) < 6){
            temp <- c(temp, sample((1:length(currentProject()$listeElem))[-temp],6-tempOR,replace = FALSE))
            names(temp) <- currentProject()$listeElem[temp]
          } else {}
          if(length(temp) == 0){
            temp <- sample((1:length(currentProject()$listeElem)),6,replace = FALSE)
            names(temp) <- currentProject()$listeElem[temp]
          } else {}
          
          elemChosen$temp <- temp
        } else {}
      }
    }) #observe  
    
    # proceed to the linear regression for the creation mode and set tableauStat$temp & machineCorrection$temp
    observe({
      input$saveNists
      if(is.null(currentProject())){      
      }else if(is.null(input$SuppDonne)){      
      }else{
        input$validDonne
        input$validDonne2
        
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
      
    }) #observe
    
    # set  coord$temp 
    observe({
      input$saveNists
      input$tab
      if(is.null(currentProject())){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(validCorrection$temp)){      
      }else{
        
        if(length(which(currentProject()$flag_stand != 1)) == 0){
          
          if(length(currentProject()$standards[[1]]$rep_dataFinale) == 1){          
          } else {
            
            temporaryTab <- currentProject()$standards[[1]]$rep_dataFinale
            
            temp <- str_sub(rownames(temporaryTab), 1, -6)
            
            X <- vector()
            for (i in 1:length(currentProject()$standardsFiles)){
              X[i] <- currentProject()$standardRank[which(names(currentProject()$standardRank) == temp[i])] 
              
            }   
            
            coord$temp <- X
          }
          
          
        } else {}
        
      }
      
    })
    
    # set zero$temp one$temp two$temp three$temp
    observe({
      if(is.null(currentProject())){      
      } else if(!is.null(input$ElementChosen)){
        
        zero$temp <-intersect(which(currentProject()$nbCalib == 0), sapply(1: length(input$ElementChosen), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}))
        one$temp <-intersect(which(currentProject()$nbCalib == 1), sapply(1: length(input$ElementChosen), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}))
        two$temp <-intersect(which(currentProject()$nbCalib == 2), sapply(1: length(input$ElementChosen), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}))
        three$temp <-intersect(which(currentProject()$nbCalib >= 3), sapply(1: length(input$ElementChosen), function(x){which(input$ElementChosen[x] == names(currentProject()$nbCalib))}))            
        
        if(length(zero$temp) != 0){
          flagHR$temp <- 0
        }else{
          if(length(one$temp) != 0){
            flagHR$temp <- 1
          }else{
            if(length(two$temp) != 0){
              flagHR$temp <- 2
            } else{
              flagHR$temp <- 3
            }
          }
        }
        
      } else {}
    })
    
    # define output$MachDrift3, output$MachDrift3_0, output$MachDrift3_1, output$MachDrift3_2, output$MachDrift3_3
    observe({
      input$saveNists
      if(is.null(currentProject())){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(validCorrection$temp)){      
      }else if(is.null(coord$temp)){      
      }else if(length(coord$temp) == 1 & coord$temp[1] == 0){      
      }else{
        
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
                
                min <- min(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], na.rm = TRUE) - (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
                
                max <- max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x],na.rm = TRUE) + (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], coord = coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)), ylim=c(min, max),ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand),xlab = "")
                
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
                
                if(!is.null(input$CorrectAll)){
                  
                  if(input$CorrectAll == TRUE){
                    valCheck = TRUE
                  } else{
                    valCheck = machineCorrection$temp[x]
                  }
                }else{
                  valCheck = machineCorrection$temp[x]
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
              }) 
              
              plotname2 <- paste("plotSession", x, sep="")
              
              output[[plotname2]] <- renderPlot({
                
                par(mar = c(4.1,4.1,0,2.1), bg = NA)
                
                min <- min(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], na.rm = TRUE) - (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
                
                max <- max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x],na.rm = TRUE) + (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],na.rm = TRUE))*3
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],coord = coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand), xlab = "")
                
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
                       p(paste0("Y (Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[x,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[x,6],3)), style = "font-size:medium;font-weight: bold;")
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
                
                min <- min(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], na.rm = TRUE) - (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], na.rm = TRUE))*3
                
                max <- max(currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], na.rm = TRUE) + (max(currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x], na.rm = TRUE))*3
                
                currentProject()$PlotIC(name = currentProject()$standardsFiles, Mean = currentProject()$standards[[1]]$rep_dataFinale[1:length(currentProject()$flag_stand),x], SD = currentProject()$standards[[1]]$rep_dataFinale[(length(currentProject()$flag_stand)+1):(2*length(currentProject()$flag_stand)),x],coord =  coord$temp, lengthSeg = 0.1, xlim =c(min(coord$temp),max(coord$temp)),ylim=c(min, max), ylab = paste0("Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand), xlab = "")
                
                abline(a = currentProject()$regressionModel[x,5], b= currentProject()$regressionModel[x,6], col ="red", lty = 2)
              }) #renderPlot
              
              tablename <- paste("tableSession", x, sep="")
              
              output[[tablename]] <- renderUI({
                
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
                
                #########
                
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
                
                #########
                
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
                
                #########

                if(!is.null(input$CorrectAll)){
                  
                  if(input$CorrectAll == TRUE){
                    valCheck = TRUE
                  } else{
                    valCheck = machineCorrection$temp[x]
                  }
                }else{
                  valCheck = machineCorrection$temp[x]
                }
                
                if(is.na(tableauStat$temp[x,4])){valeur4 <- 'font-weight:bold; color:red'
                                                 val <- "Correction"
                                                 valB <- "NS"
                } else {
                  if(tableauStat$temp[x,4] < 0.05){valeur4 <- 'font-weight:bold; color:red'
                                                   
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
                
                #########
                
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
                       p(paste0("Y (Cps_",currentProject()$listeElem[x],"/Cps_", currentProject()$elemStand, ") = ", round(currentProject()$regressionModel[x,5],3), " + X (Stand. Rank) * ", round(currentProject()$regressionModel[x,6],3)), style = "font-size:medium;font-weight: bold;")
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
      }
    }) 
    
    # define output$MachDrift1  & output$MachDrift2
    observe({
      input$saveNists
      if(is.null(currentProject())){      
      }else if(is.null(input$SuppDonne)){      
      } else if(is.null(validCorrection$temp)){      
      } else{
        
        if(length(which(currentProject()$flag_stand != 1)) == 0){
          
          if(is.null(validCorrection$temp)){
            
          }else{
            
            if((validCorrection$temp%%2) == 0){
              
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
          }
          
        }else{
          output$MachDrift1 <- renderUI({NULL})#output$MachDrift1
          
          output$MachDrift2 <- renderUI({NULL})#output$MachDrift2
        }
      }
    })
    
    # set machineCorrection$temp
    observe({
      if(is.null(input$CorrectAll)){      
      }else{      
        isolate({
          
          if(input$CorrectAll == TRUE & (validCorrection$temp%%2) == 0){
            for(i in 1:length(currentProject()$listeElem)){
              if(is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[i]))))){              
              }else{machineCorrection$temp[i] <- TRUE}
            }
          } else {}
          if(input$CorrectAll == FALSE & (validCorrection$temp%%2) == 0){
            for(i in 1:length(currentProject()$listeElem)){
              if(is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[i]))))){
                
              }else{machineCorrection$temp[i] <- FALSE}
            }
          } else {}
          
        })
        
      }
    }) #observe  
    
    # set validCorrection$temp when input$validDrift changes
    observe({
      if(is.null(input$validDrift)){      
      }else{
        isolate({
          
          if((input$validDrift%%2) == 1){
            validCorrection$temp <- validCorrection$temp + 1          
            updateSelectInput(session, 'ElementChosen', selected = input$ElementChosen)
          } else {}
          
        })
      }
    }) #observe  
    
    # save de data when the validCorrection$temp is in position of save and delete the data  if validCorrection$temp is is the delete position 
    observe({
      if(is.null(input$validDrift)){      
      }else if(is.null(validCorrection$temp)){      
      } else{
        
        if((validCorrection$temp%%2) == 1 & input$validDrift > 0){
          
          currentProject()$setflagMachineCorrection(x = 1)
          
          machineCorrection$temp <- sapply(1:length(currentProject()$listeElem), function(x){
            
            if(is.null(eval(parse(text = paste0("input$",geneRMachineCorr$temp[x]))))){
              FALSE
            }else{
              eval(parse(text = paste0("input$",geneRMachineCorr$temp[x])))
            }
            
          })        
          
          currentProject()$setCorrection(x = machineCorrection$temp)  
          
        }  else {}
        
        if((validCorrection$temp%%2) == 0 & input$validDrift > 0){
          
          currentProject()$setflagMachineCorrection(x = 0)
          lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "spot", value = 0)})
          lapply(1:length(currentProject()$flag_Sample), function(x){currentProject()$set_flagRealign(replicate = x, type = "raster",value = 0)})    
          lapply(1:length(currentProject()$flag_Sample), function(x){
            lapply(1:length(currentProject()$flag_Sample[[x]]), function(i){
              currentProject()$setflagSample(sample = x, replicate = i, value = 0)
            })                
          })
          flagSampleDetail$temp <- currentProject()$flag_Sample
          flagRealign$temp <- currentProject()$flagRealign     
          
        }  else {}
      }
      
    }) #observe
    
    #######################
    ##### SAMPLES #########
    #######################
    
    TempS <-  reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$binsSample[1]
    Temp0S <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$binsSample[2]
    Temp1S <- reactiveValues(t = NULL) # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$platSample[1]
    Temp2S <- reactiveValues(t = NULL)  # a numerical value which corresponds to the nearest value in the time analysis for the slinderInput of input$platSample[2]
    dataPlot2Sample <- reactiveValues(datS = NULL)   # a matrix corresponding to the filtered
    
    BAV_Sample <- reactiveValues(temp = 0)
    LOD_Sample <- reactiveValues(temp = 0)
    
    output$signiS <- renderUI({
      input$SampleIn
      input$SampleIn2
      input$listeElemSample
      input$CourbeSample
      input$binsSample
      input$platSample
      if(!is.null(input$CourbeSample)){
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
                div(p(paste0("B.A.V.*: ", " ",  " ",round(BAV_Sample$temp[grep(input$listeElemSample, names(BAV_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px;"),
                div(p(paste0("L.O.D.**: ", " ",  " ",round(LOD_Sample$temp[grep(input$listeElemSample, names(LOD_Sample$temp))],0), " cps/sec"), style = "margin-bottom: 0px"), style = "margin-left:20px")
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
      } else {}
    })
    
    #set elemUsed$temp
    observe({
      if(is.null(input$selectallS)){      
      }else if(is.null(input$checkGroupS)){
        elemUsed$temp <- ElemStand$temp
      }else{
        if(input$selectallS%%2 == 0 & length(input$checkGroupS) != length(currentProject()$listeElem)){
          isolate({
            elemUsed$temp <- input$checkGroupS
            
          })        
        } else {}
      }
    }) #observe  
    
    # set output$sample1, output$sample2 and output$sample3
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else{ 
        if(is.na(currentProject()$flagMachineCorrection)){        
        }else{
          
          if(currentProject()$flagMachineCorrection == 1){
            
            output$sample1 <- renderUI({
              div(class = "class4",
                  div(h3("Step 4. Sample replicate filtering"), style = "display: inline-block;")      
              )
            }) # eo output$sample1
            
            output$sample2 <- renderUI({
              div(
                p(icon("eye"),"Select sample"),
                div(selectInput("SampleIn", "",  as.matrix(currentProject()$samplesFiles), selected = as.matrix(currentProject()$samplesFiles)[1], multiple = FALSE, width = '100%'), style = "margin-top: -20px")
                
              )     
            }) # eo output$sample2   
            
            output$sample3 <- renderUI({
              if(is.null(input$SampleIn)){}
              else{
                if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                  div(class = "class1",
                      p(icon("eye"),"Select sample replicate"),
                      div( selectInput("SampleIn2", "", as.matrix(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), multiple = FALSE, width = '100%' ), style = "margin-top: -20px")
                      
                  ) 
                }              
              }            
            })  # eo output$sample3  
            
          }else{
            output$sample1 <- renderUI({NULL}) # eo output$sample1
            
            output$sample2 <- renderUI({NULL})  # eo output$sample2
            
            output$sample3 <- renderUI({NULL}) # eo output$sample3
            
            
          }
        }
      }
      
    }) # observe
    
    # set output$sample4, output$sample5, output$distPlotSample, output$distPlot2Sample
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){    
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$SampleIn)){      
      }else if(is.null(input$SampleIn2)){      
      }else if(is.null(flagSampleDetail$temp)){      
      }else{
        if(is.na(currentProject()$flagMachineCorrection)){        
        }else{
          
          if(currentProject()$flagMachineCorrection == 1 & !is.null(input$SampleIn) & !is.null(input$SampleIn2)){  
            if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
              if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                
                output$sample4 <- renderUI({NULL}) # eo input$Sample4 
                
                output$Sample5 <- renderUI({p("Loading data") }) # eo input$Sample5
                
                output$distPlotSample <- renderPlot({}) # eo input$distPlotSample
                
                output$distPlot2Sample <- renderPlot({})# eo input$distPlot2Sample
              }else{
                
                if(flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] == 0){
                  
                  output$sample4 <- renderUI({
                    actionButton("saveSample", "Save")       
                  }) # eo input$Sample4 
                  
                  output$Sample5 <- renderUI({

                    input$SampleIn
                    input$SampleIn2
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                      if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){                    
                    }else{
                      
                      minBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                      maxBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                      
                      minPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                      maxPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                      
                      value1S <- c(0,(maxBS - minBS)/6)
                      value2S <- c((maxPS - minPS)*2/6,(maxPS - minPS)*4/6)
                      step <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$setRep_pas()
                      
                      fluidRow(
                        column(8,style  = "padding-right: 5px",                          
                               box(
                                 title = list(icon("share"),"Background and plateau limits selection"),
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
                                                      lapply(1:length(currentProject()$listeElem), function(x){
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
                                 title = list(icon("share"),"Filtered data verification"),
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
                    } else {}
                    
                  }) # eo input$Sample5
                  
                  output$distPlotSample <- renderPlot({
                    
                    par(mar = c(3,3.5,1.75,0))
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                      if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                      
                    }else{
                      
                      maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data, na.rm = TRUE) 
                      
                      minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                      maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                      
                      plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                      mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                      mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                      mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                      
                      if(length(input$checkGroupS) > 1){
                        lapply(2:length(input$checkGroupS), function(x){
                          par(new = TRUE)
                          plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                        })
                      }  else {}
                      
                      TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[1])[[2]]
                      Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[2])[[2]]
                      Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[1])[[2]]
                      Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[2])[[2]]
                      
                      rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], -maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)                      
                      rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                      
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                      
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,x],  cex = 3, col ="grey")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,x], cex = 3, col ="grey")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
                    }
                    } else {}
                    
                  }, height = 400) # eo input$distPlotSample
                  
                  output$distPlot2Sample <- renderPlot({
                    input$valRemplace
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
                        
                        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) != 0){
                            if(length(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge) == 1){
                          if(!is.na(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge)){
                            if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                              abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                            } else if(input$CourbeSample == "Blank removed"){
                              abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                            } else {}
                          } else {}
                        } else {
                          if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                            abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                          } else if(input$CourbeSample == "Blank removed"){
                            abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                          } else {}
                        }
                          } else {}
                        } else {}
                        
                      }
                    }
                  })# eo input$distPlot2Sample
                  
                } else {}
                if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0 & flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] != 0){
                  
                  
                  
                  output$sample4 <- renderUI({
                    actionButton("saveSample", "Save")       
                  })  # eo input$Sample4 
                  
                  output$Sample5 <- renderUI({
                    
                    input$SampleIn
                    input$SampleIn2
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                      if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){ }else{
                        
                        minBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                        maxBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                        
                        minPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                        maxPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                        
                        value1S <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins
                        value2S <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat
                        step <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$setRep_pas()
                        
                        fluidRow(
                          column(8, style  = "padding-right: 5px",                          
                                 box(
                                   title = list(icon("share"),"Background and plateau limits selection"),
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
                                                        lapply(1:length(currentProject()$listeElem), function(x){
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
                                   title = list(icon("share"),"Filtered data verification"),
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
                    } else {}                   
                    
                    
                  }) # eo input$Sample5
                  
                  output$distPlotSample <- renderPlot({
                    
                    par(mar = c(3,3.5,1.75,0))
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                      if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){
                        
                      }else{
                        
                        maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data, na.rm = TRUE) 
                        
                        minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                        maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                        
                        plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                        mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                        mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                        mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                        
                        if(length(input$checkGroupS) > 1){
                          
                          lapply(2:length(input$checkGroupS), function(x){
                            par(new = TRUE)
                            plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                          })
                          
                        } else {}
                        
                        if(is.null(input$binsSample)){                      
                        }else{
                          TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[1])[[2]]
                          Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$binsSample[2])[[2]]
                          Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[1])[[2]]
                          Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = input$platSample[2])[[2]]
                        }
                        
                        rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], -maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#8B735564", border = NA)                      
                        rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                        
                        abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  lty = "dashed", col = ("grey"), lwd = 2)
                        abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("grey"), lwd = 2)
                        abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                        abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                        
                        lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,x],  cex = 3, col ="grey")})
                        lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,x], cex = 3, col ="grey")})
                        lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
                        lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
                      }
                    } else {}
                    
                    
                  }, height = 400) # eo input$distPlotSample
                  
                  output$distPlot2Sample <- renderPlot({
                    input$valRemplace
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
                        
                        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) != 0){
                            if(length(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge) == 1){
                          if(!is.na(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge)){
                            if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                              abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                            } else if(input$CourbeSample == "Blank removed"){
                              abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                              rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                            } else {}
                          } else {}
                        } else {
                          if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                            abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                          } else if(input$CourbeSample == "Blank removed"){
                            abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                            rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                          } else {}
                        }
                          } else {}
                        } else {}
                        
                      }
                    }
                    
                  })# eo input$distPlot2Sample
                  
                  
                }  else {}
                if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
                  
                  currentProject()$setflagSample(sample = grep(input$SampleIn,currentProject()$samplesFiles), replicate = grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), value = 1)
                  
                  output$sample4 <- renderUI({
                    actionButton("saveSample", "Delete")       
                  }) # eo input$Sample
                  
                  output$Sample5 <- renderUI({
                    
                    input$SampleIn
                    input$SampleIn2
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) == 0){                    
                    }else if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){                    
                    }else{
                      
                      minBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                      maxBS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                      
                      minPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                      maxPS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                      
                      fluidRow(
                        column(8, style  = "padding-right: 5px",                          
                               box(
                                 title = list(icon("share"),"Background and plateau limits selection"),
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
                                                      lapply(1:length(currentProject()$listeElem), function(x){
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
                                 title = list(icon("share"),"Filtered data verification"),
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
                  
                  output$distPlotSample <- renderPlot({
                    
                    par(mar = c(3,3.5,1.75,0))
                    
                    if(length(grep(input$SampleIn,currentProject()$samplesFiles)) == 0){                    
                    }else if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){                    
                    }else{
                      
                      maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data, na.rm = TRUE) 
                      
                      minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                      maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = TRUE)
                      
                      plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[1]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[1] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY))
                      mtext("Signal intensity (cps)",side=2,line=2.4,  cex=1.2)
                      mtext("Time (s)",side=1,line=1.5, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                      mtext("Raw data",side=3,line=0.75, cex=1.2, font = 2)
                      
                      if(length(input$checkGroupS) > 1){
                        
                        lapply(2:length(input$checkGroupS), function(x){
                          par(new = TRUE)
                          plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,input$checkGroupS[x]],type ="b", ylab = "", xlab = "", main = "", col = color$temp[which(input$checkGroupS[x] == names(color$temp))], xlim = c(minX, maxX), ylim =c(0,maxY), axes = FALSE)
                        })      
                      } else {}
                      
                      TempS$t  <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[1])[[2]]
                      Temp0S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[2])[[2]]
                      Temp1S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[1])[[2]]
                      Temp2S$t <- currentProject()$closest(x = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],y = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[2])[[2]]
                      
                      rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[2],(1+10/100)*maxY, col = "#8B735564", border = NA)                      
                      rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[2],(1+10/100)*maxY, col ="#4F3CBC30", border = NA)
                      
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[1], lty = "dashed", col = ("grey"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$bins[2], lty = "dashed", col = ("grey"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[1], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                      abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$plat[2], lty = "dashed", col = ("#4F3CBC50"), lwd = 2)
                      
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1],  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,x],  cex = 3, col ="grey")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,x], cex = 3, col ="grey")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,x], cex = 3, col ="#4F3CBC50")})
                      lapply(input$checkGroupS, function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,x], cex = 3, col ="#4F3CBC50")})
                    }
                  }) # eo input$distPlotSample
                  
                  output$distPlot2Sample <- renderPlot({
                    input$valRemplace
                    if(is.null(dataPlot2Sample$datS)){                    
                    }else{
                      
                      if(length(which(!is.na(dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))]))) == 0){
                        plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                        text(1,0.5, labels = "No data different from NA", cex = 2)
                      }else{
                        par(mar = c(3.5,3.7,1.75,1))
                        plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "", xlab = "", main = "")  
                        mtext("Signal intensity (cps)",side=2,line=2.6,  cex=1.2)
                        mtext("Time (s)",side=1,line=2.3, at=par("usr")[2]-0.05*diff(par("usr")[1:2]), cex=1.2)
                        mtext(paste0("Data ",input$CourbeSample),side=3,line=0.75, cex=1.2, font = 2)
                        
                        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){
                          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) != 0){
                            if(length(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge) == 1){
                              if(!is.na(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge)){
                                if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                                  abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                                  rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                                } else if(input$CourbeSample == "Blank removed"){
                                  abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                                  rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                                } else {}
                              } else {}
                            } else {
                              if(input$CourbeSample == "Raw" | input$CourbeSample == "Plateau"){
                                abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], b = 0, lty = "dashed", col = "red", lwd = 2)
                                rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge))], col = "#FF000064", border = NA)
                              } else if(input$CourbeSample == "Blank removed"){
                                abline(a = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], b = 0, lty = "dashed", col = "red", lwd = 2)
                                rect(-10, -10^6, (1+10/100)*max(dataPlot2Sample$datS[,1]),currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD[grep(input$listeElem, names(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD))], col = "#FF000064", border = NA)
                              } else {}
                            }
                            
                          }
                          
                        } else {}
                      }
                    }
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
        }
      }
    }) # observe
    
    # calculate and render dataPlot2Sample$datS
    observe({
      input$valRemplace
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$SampleIn)){      
      }else if(is.null(input$SampleIn2)){      
      }else if(is.null(input$CourbeSample)){      
      }else if(is.null(flagSampleDetail$temp)){      
      }else{      
        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){          
          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){            
          }else{
            
            if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
              if(is.null(input$bins)){                
              }else if(is.null(input$plat)){   
              }else if(is.null(TempS$t)){
              }else if(is.null(Temp0S$t)){                
              }else if(is.null(Temp1S$t)){                
              }else if(is.null(Temp2S$t)){                
              }else{
                if(is.finite(TempS$t)){dataPlot2Sample$datS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(curve = input$CourbeSample, bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2, meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                                        BAV_Sample$temp <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge
                                        LOD_Sample$temp <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD
                } else {}
              }
            } else {}
            if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1){
              if(is.null(TempS$t)){                
              }else{
                if(is.finite(TempS$t)){dataPlot2Sample$datS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$renderData(curve = input$CourbeSample)
                                        BAV_Sample <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge
                                        LOD_Sample <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD
                } else {}
              } 
            } else {}
          } 
        } else {} 
      }
    }) # observe
    
    # updateCheckboxGroupInput of checkGroupS
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$selectallS)){      
      } else{  
        
        if(input$selectallS == 0) {
          return(NULL) 
        }else if(input$selectallS%%2 == 0){
          updateCheckboxGroupInput(session,"checkGroupS","",choices=currentProject()$listeElem, selected = currentProject()$listeElem)
        }else{
          updateCheckboxGroupInput(session,"checkGroupS","",choices=currentProject()$listeElem,selected=currentProject()$listeElem)
        }
      }
      
    }) # observe
    
    # set flagSampleDetail when input$saveSample is pressed
    observe({ 
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$saveSample)){      
      }else{      
        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){        
          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){          
          }else{          
            if(input$saveSample > 0){
              isolate({
                flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] <- flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] + 1
                updateSelectInput(session, "listeElemSample", selected = input$listeElemSample)
                updateSelectInput(session, "CourbeSample", selected = input$CourbeSample)
                loadS$temp <- loadS$temp +1
              })
            } else {}
          }
        } else {}
      }
    }) # observe 
    
    # to avoid elementR saving data when first delete loaded project
    observe({   
      if(is.null(input$SampleIn2)){      
      }else{
        isolate({
          if(loadS$temp == 1){
            loadS$temp <- loadS$temp +1
          }
        })
      }
    }) #observe
    
    # save the data when flagSampleDetail is in the save position and delete the data when the flag is in the delete position
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$binsSample)){      
      }else if(is.null(input$platSample)){      
      }else if(is.null(input$SampleIn)){      
      }else if(is.null(input$SampleIn2)){      
      }else if(is.null(input$SampleIn2)){      
      }else if(is.null(input$saveSample)){  
      }else if(is.null(TempS$t)){
      }else if(is.null(Temp0S$t)){      
      }else if(is.null(Temp1S$t)){      
      }else if(is.null(Temp2S$t)){      
      }else{ 
        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){        
          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){          
          }else{
            
            if(projChar$temp[1] == 2 & loadS$temp == 1){
              
              if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0){
                isolate({
                  currentProject()$setflagSample(sample = grep(input$SampleIn,currentProject()$samplesFiles), replicate = grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), value = 0)
                  currentProject()$set_flagRealign(replicate = grep(input$SampleIn,currentProject()$samplesFiles), type = "spot", value = 0)
                  currentProject()$set_flagRealign(replicate = grep(input$SampleIn,currentProject()$samplesFiles), type = "raster", value = 0)
                  flagRealign$temp <- currentProject()$flagRealign     
                  currentProject()$set_summarySettings(name = input$SampleIn2, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = NA, LOD = NA)
                })                          
              } else {}
              
              if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1 & is.finite(TempS$t)){
                isolate({
                  currentProject()$setflagSample(sample = grep(input$SampleIn,currentProject()$samplesFiles), replicate = grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), value = 1)
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setBins(bins = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1]))     
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setPlat(plat = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]))
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setDataConcCorr(bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2,meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                  currentProject()$set_summarySettings(name = input$SampleIn2, rank = currentProject()$sampleRank[which(names(currentProject()$sampleRank) == input$SampleIn2)], bins1 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], bins2 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], plat1 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], plat2 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], average = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge, LOD = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD)
                })                              
              } else {}
              
              
            } else {
              
              if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 0 & input$saveSample > 0){
                isolate({
                  currentProject()$setflagSample(sample = grep(input$SampleIn,currentProject()$samplesFiles), replicate = grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), value = 0)
                  currentProject()$set_flagRealign(replicate = grep(input$SampleIn,currentProject()$samplesFiles), type = "spot", value = 0)
                  currentProject()$set_flagRealign(replicate = grep(input$SampleIn,currentProject()$samplesFiles), type = "raster", value = 0)
                  flagRealign$temp <- currentProject()$flagRealign     
                  currentProject()$set_summarySettings(name = input$SampleIn2, rank = NA, bins1 = NA, bins2 = NA, plat1 = NA, plat2 = NA, average = NA, LOD = NA)
                })                          
              } else {}
              
              if((flagSampleDetail$temp[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]%%2) == 1 & is.finite(TempS$t) & input$saveSample > 0){
                isolate({
                  currentProject()$setflagSample(sample = grep(input$SampleIn,currentProject()$samplesFiles), replicate = grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), value = 1)
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setBins(bins = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1]))     
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setPlat(plat = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]))
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setDataConcCorr(bins = c(TempS$t, Temp0S$t), plat = c(Temp1S$t,Temp2S$t), name = input$SampleIn2,meanStand = currentProject()$standards[[1]]$rep_dataFinale[(nrow(currentProject()$standards[[1]]$rep_dataFinale)-1),], rank = currentProject()$sampleRank, model = currentProject()$regressionModel, calibFile = currentProject()$EtalonData, correction = currentProject()$machineCorrection, rempl = input$valRemplace)
                  currentProject()$set_summarySettings(name = input$SampleIn2, rank = currentProject()$sampleRank[which(names(currentProject()$sampleRank) == input$SampleIn2)], bins1 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[TempS$t,1], bins2 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], plat1 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], plat2 = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], average = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$BlankAverarge, LOD = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$LOD)
                  passageS <- names(currentProject()$flag_Sample[[grep(input$SampleIn,currentProject()$samplesFiles)]] == 0)[currentProject()$flag_Sample[[grep(input$SampleIn,currentProject()$samplesFiles)]] == 0][1]
                  if(!is.na(passageS)){
                    delay(2000,updateSelectInput(session, "SampleIn2", selected = passageS))
                  } else {}
                })                              
              } else {}
              
            }
            

          }
        }
      }                
    }) # observe

    # save the data when flagSampleDetail is in the save position and delete the data when the flag is in the delete position
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$saveNists)){      
      }else if(is.null(input$SuppDonne)){      
      }else if(is.null(input$binsSample)){      
      }else if(is.null(input$platSample)){      
      }else if(is.null(input$SampleIn)){   
      }else if(is.null(input$SampleIn2)){      
      }else if(is.null(input$saveSample)){  
      }else if(is.null(TempS$t)){
      }else if(is.null(Temp0S$t)){      
      }else if(is.null(Temp1S$t)){      
      }else if(is.null(Temp2S$t)){      
      }else{ 
        if(length(grep(input$SampleIn,currentProject()$samplesFiles)) != 0){        
          if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){          
          }else{

          }
        }
      }                
    }) # observe

    #######################
    ##### REALIGNEMENT ####
    #######################
    
    deplace <- reactiveValues(val = NULL) # a vector of temporal shift of sample replicate
    tabProvSpot <- reactiveValues(temp = NULL) # a temporary matrix for calculation (spot mode)
    tabProvSample <- reactiveValues(temp = NULL) # a temporary list of data for calculation (raster mode)
    
    tabSpotDisplay <- reactiveValues(temp = NULL) # a matrix to display (spot mode) before validating the data
    tabSpotSave <- reactiveValues(temp = NULL)  # a matrix to save (spot mode) before validating the data
    
    ChosenElement <- reactiveValues(temp = NULL) # the element chosen for the realignement (raster mode)
    
    # set flagSample$temp
    observe({
      if(is.null(input$saveNists)){      
      }else if(is.null(currentProject())){      
      }else if(is.null(input$saveSample)){      
      }else if(is.null(input$validDrift)){      
      }else if(is.null(input$SuppDonne)){      
      }else{
        
        flagSample$temp <- sapply(1: length(currentProject()$flag_Sample), function(x){
          if(sum(currentProject()$flag_Sample[[x]]) == length(currentProject()$flag_Sample[[x]])){return(1)}
          else{return(0)}
        })  
        
      }
      
    }) # observe
    
    # set output$realign1 
    observe({
      if(length(which(flagSample$temp == TRUE)) != 0){
        
        output$realign1 <- renderUI({
          fluidRow(
            box(background = "yellow", width = 12, height = 85,
                column(8,
                       div(h3("Step 5. Replicates averaging procedure"), style = "display: inline-block;")
                ),   
                column(2, 
                       selectInput("selectRealign",label = "", choices = currentProject()$samplesFiles[which(flagSample$temp == TRUE)], multiple = FALSE )
                ), 
                column(2,
                       radioButtons("typeTraitement", label = "", choices = c("spot","raster"), inline = TRUE)
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
    
    # set ChosenElement$temp
    observe({
      if(!is.null(input$elemRaster)){
        ChosenElement$temp <- input$elemRaster
      } else {}
    }) # observe
    
    # set output$realign2, output$realign3, output$realign4 & output$realign5
    observe({
      input$ValiderSample
      input$SuppDonne
      input$validDonne
      input$validDonne2
      if(!is.null(currentProject())){
        if(!is.null(input$typeTraitement)){
          if(!is.null(input$typeTraitement)){ 
            if(!is.null(input$selectRealign)){ 
              if(!is.null(flagRealign$temp)){
                if(length(grep(input$selectRealign,currentProject()$samplesFiles)) != 0){
                  
                  if(input$typeTraitement == "spot" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 0 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
                    
                    currentProject()$set_flagRealign(replicate = grep(input$selectRealign,currentProject()$samplesFiles), type = "spot", value = 0)   
                    
                    output$realign3 <- renderUI({
                      fluidRow(
                        box(
                          solidHeader = TRUE,
                          status = "warning",
                          width = 12, 
                          title = list(icon("spinner"), "Spot averaging"), 
                          column(2,  
                                 style="padding-right: 0px",
                                 br(), 
                                 div(style = "margin-left: -15px",
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
                                 p("Replicates to average"),
                                 div(checkboxGroupInput("ReplicateSpot", label = "", 
                                                        choices = as.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files),
                                                        selected = as.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)),style = "margin-top: -10px"),
                                 br(),
                                 p("Concentration displayed"),
                                 br(),
                                 column(2, actionButton("SauvegarderSpot", "Save averaging"))                          
                          ),
                          column(10, 
                                 div(style='overflow-y: hidden', 
                                     dataTableOutput("realign4")
                                 )
                          )
                        )
                        
                      )
                      
                    }) # eo output$realign3
                    
                    output$realign4 <- renderDataTable({
                      
                      return(tabSpotDisplay$temp)
                      
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
                  
                  
                  output$realign5 <- renderPlot({NULL}, bg = "transparent") # eo output$realign5
                  
                  output$realign2 <- renderUI({NULL}) # eo output$realign2
                  
                  
                } else {}
                
                  if(input$typeTraitement == "spot" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
                  
                  currentProject()$set_flagRealign(replicate = grep(input$selectRealign,currentProject()$samplesFiles), type = "spot", value = flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1])
                  currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2(x = "spot")
                  
                  output$realign3 <- renderUI({
                    if(is.numeric(nrow(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot))){
                      fluidRow(
                        box(
                          solidHeader = TRUE,
                          status = "warning",
                          width = 12, 
                          title = list(icon("spinner"), "Spot averaging"), 
                          column(2,
                                 style="padding-right: 0px",
                                 br(), 
                                 div(style = "margin-left: -15px",
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
                          ),
                          column(10, 
                                 div(style='overflow-y: hidden', 
                                     dataTableOutput("realign4")
                                 )
                          )
                        )
                        
                      )
                    } else {}
                   
                    
                  }) # eo output$realign3
                  
                  output$realign4 <- renderDataTable({
                    
                    if(!currentProject()$is.integer0(grep(input$selectRealign,currentProject()$samplesFiles))){
                      if(is.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot)){
                        temp <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot 
                        
                        temp <- format(temp, scientific=TRUE, digits = 2)    
                        
                        temp <- cbind(rownames(temp), temp)    
                        
                        return(temp)
                      } else {}
                    } else {}
                    
                    
                  }, options = list(paging= FALSE, searching = FALSE, rowCallback = I(
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
                  
                  output$realign5 <- renderPlot({NULL}, bg = "transparent") # eo output$realign5
                  
                  output$realign2 <- renderUI({NULL}) # eo output$realign2
                  
                  
              } else {}
              
                  if(input$typeTraitement == "spot" & ((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3)){
                
                output$realign3 <- renderUI({
                  
                  fluidRow(
                    box(
                      solidHeader = TRUE,
                      status = "warning",
                      width = 12, 
                      title = list(icon("spinner"), "Spot averaging (WARNING, already saved in raster procedure)"), 
                      column(3, 
                             br(),
                             selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp)
                      ),
                      column(9,
                             plotOutput("realign5")
                      )
                    )
                    
                  )
                  
                }) # eo output$realign3
                
                if(is.null(input$elemRaster)){              
                }else{              
                  output$realign5 <- renderPlot({
                    
                    par(mar = c(5.1,4.1,1.5,1.5))
                    
                    if(!currentProject()$is.integer0(grep(input$selectRealign,currentProject()$samplesFiles))){
                      if(is.list(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)){
                        if(!is.null(input$elemRaster)){
                          if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
                            plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                            text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                          }else{
                            
                            ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
                            
                            xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                            
                            lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                              
                              plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
                              
                              par(new = TRUE)
                              
                            })
                            
                            plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2)
                            
                            legend("topright", legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
                          }
                        } else {}
                      } else {}
                    } else {}
                    
                  }) # eo  output$realign5
                }
                
                output$realign4 <- renderTable({NULL}) # eo output$realign4
                
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
              
                  if(input$typeTraitement == "raster" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
                
                currentProject()$set_flagRealign(replicate = grep(input$selectRealign,currentProject()$samplesFiles), type = "raster", value = 0)
                
                output$realign4 <- renderTable({NULL}) # eo output$realign4
                
                output$realign3 <- renderUI({
                  
                  fluidRow(
                    box(
                      solidHeader = TRUE,
                      status = "warning",
                      width = 12, 
                      
                      title = list(icon("area-chart"), "Raster realignment"), 
                      column(3, 
                             br(),
                             p("Replicates to average"),
                             div(checkboxGroupInput("ReplicateSample", label = "", 
                                                    choices = as.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files),
                                                    selected = as.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)),style = "margin-top: -10px"),
                             selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = currentProject()$listeElem[1]),
                             uiOutput('replicates'),
                             br(),
                             actionButton("MoyenneRaster", "Mean")
                      ),
                      column(9,
                             plotOutput("realign5")
                      )
                    )
                    
                  )
                  
                }) # eo output$realign3
                
                for (i in 1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)) {
                  local({
                    my_i <- i
                    plotname <- paste("plot", my_i, sep="")
                    
                    output[[plotname]] <- renderUI({
                      numericInput(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][i],paste0("realign", currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files[i]),value = 0)
                    })
                  })
                } 
                
                output$realign5 <- renderPlot({
                  
                  deplace$val
                  
                  par(mar = c(5.1,4.1,1.5,1.5))
                  
                  if(!is.null(input$ReplicateSample)){
                    if(is.list(tabProvSample$temp)){
                      if(length(which(is.element(input$ReplicateSample, names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre)) == FALSE)) == 0){                  
                        if(length(which(!is.na(unlist(lapply(1:length(tabProvSample$temp), function(x){tabProvSample$temp[[x]][,input$elemRaster]}))))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                        } else {
                          if(length(grep(input$selectRealign,currentProject()$samplesFiles)) == 0){
                            
                          } else {
                            
                            ylim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(i){tabProvSample$temp[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(1:length(input$ReplicateSample), function(i){tabProvSample$temp[[i]][,input$elemRaster]})), na.rm = TRUE))
                            
                            xlim <- c(min(unlist(lapply(1:length(input$ReplicateSample), function(i){tabProvSample$temp[[i]][,1]}))),max(unlist(lapply(1:length(input$ReplicateSample), function(i){tabProvSample$temp[[i]][,1]}))))
                            
                            lapply(1:length(input$ReplicateSample), function(x){
                              
                              if(length(which(names(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])) == 0){
                                
                              }else{
                                plot(tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[x])]][,1],tabProvSample$temp[[which(names(tabProvSample$temp) == input$ReplicateSample[x])]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = "", col = colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])] )
                                
                                par(new = TRUE)
                              }
                              
                            })            
                            
                            legend("topright", legend = input$ReplicateSample, col = sapply(1:length(input$ReplicateSample), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[x])]}), pch = 1, lwd=1, lty=1)
                          }
                        }
                      } else {}
                    } else {}
                  } else {}
                }) # eo output$realign5
                
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
              
                  if(input$typeTraitement == "raster" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 2 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 1 & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) != 3){
                
                output$realign4 <- renderTable({NULL})  # eo output$realign4
                
                output$realign3 <- renderUI({
                  
                  fluidRow(
                    box(
                      solidHeader = TRUE,
                      status = "warning",
                      width = 12, 
                      title = list(icon("area-chart"), "Raster realignment"), 
                      column(3, 
                             br(),
                             selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp),
                             actionButton("MoyenneRaster","Delete averaging"),
                             actionButton("SauvegarderReal","Save averaging")
                      ),
                      column(9,
                             plotOutput("realign5")
                      )
                    )
                    
                  )
                  
                }) # eo output$realign3
                
                output$realign5 <- renderPlot({
                  
                  par(mar = c(5.1,4.1,1.5,1.5))
                  
                  if(!currentProject()$is.integer0(grep(input$selectRealign,currentProject()$samplesFiles))){
                    if(is.list(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)){
                      if(!is.null(input$elemRaster)){
                        if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                        }else{
                          
                          ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
                          
                          xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                          
                          lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                            
                            plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col =colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
                            
                            par(new = TRUE)
                            
                          })
                          
                          plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2)
                          
                          legend("topright", legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), NA), lwd = c(rep(1, length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)), 2))
                        }
                      } else {}
                    } else {}  
                  } else {}
                  
                }) # eo output$realign5
                
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
              
                  if(input$typeTraitement == "raster" &  ((flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 1 | (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 3) & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
                
                currentProject()$set_flagRealign(replicate = grep(input$selectRealign,currentProject()$samplesFiles), type = "raster", value = flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2])
                currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2(x = "raster")
                
                output$realign3 <- renderUI({
                  
                  fluidRow(
                    box(
                      solidHeader = TRUE,
                      status = "warning",
                      width = 12, 
                      title = list(icon("area-chart"), "Raster realignment"), 
                      column(3, 
                             br(),
                             selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = ChosenElement$temp),
                             actionButton("SauvegarderReal","Delete Realignment")
                      ),
                      column(9,
                             plotOutput("realign5")
                      )
                    )
                    
                  )
                  
                }) # eo output$realign3
                
                output$realign5 <- renderPlot({
                  
                  par(mar = c(5.1,4.1,1.5,1.5))
                  
                  if(!currentProject()$is.integer0(grep(input$selectRealign,currentProject()$samplesFiles))){
                    if(is.list(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)){
                      if(!is.null(input$elemRaster)){
                        if(length(which(!is.na(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster]}))))) == 0){
                          plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
                          text(1,0.5, labels = "No data different from NA for this element", cex = 2)
                        }else{
                          
                          ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = TRUE))
                          
                          xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = TRUE))
                          
                          lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){ 
                            
                            plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col =colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])] )
                            
                            par(new = TRUE)
                            
                          })
                          
                          plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalRaster[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "l", lwd = 2)
                          
                          legend("topright", legend = c(names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), "Averaging"), col = c(sapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster), function(x){colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster)[x])]}), "black"), lty = 1, pch = c(rep(1, length(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]])), NA), lwd = c(rep(1, length(colorReplicate$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]])), 2))
                        } 
                      } else {}
                    } else {}
                  } else {}
                  
                })  # eo output$realign5
                
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
              
                  if(input$typeTraitement == "raster" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) == 1){
                
                output$realign3 <- renderUI({
                  
                  if(is.numeric(nrow(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot))){
                    fluidRow(
                      box(
                        solidHeader = TRUE,
                        status = "warning",
                        width = 12,
                        title = list(icon("area-chart"), "Raster realignment (WARNING: already saved in spot procedure)"), 
                        column(2,
                               style="padding-right: 0px",
                               br(), 
                               div(style = "margin-left: -15px",
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
                        ),
                        column(10, 
                               div(style='overflow-y: hidden', 
                                   dataTableOutput("realign4")
                               )
                        )
                        
                      )#box
                      
                    )#fluidRow
                  } else {}
                  
                }) # eo output$realign3
                
                output$realign4 <- renderDataTable({
                  
                  if(!currentProject()$is.integer0(grep(input$selectRealign,currentProject()$samplesFiles))){
                    if(is.matrix(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot)){
                      
                      temp <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinalSpot
                      
                      temp <- format(temp, scientific=TRUE, digits = 2)    
                      
                      temp <- cbind(rownames(temp), temp)    
                      
                      return(temp)
                      
                    }else{}
                  }else{}
                  
                  
                  
                }, options = list(paging= FALSE, searching = FALSE, rowCallback = I(
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
                
                output$realign5 <- renderPlot({NULL}, bg = "transparent") # eo output$realign5
                
                output$realign2 <- renderUI({}) # eo output$realign2
                
            } else {}
            
            
          } else {}
        }
        }
        }
        }
            } 
      
          }) # observe
    
    # define the numericInput widgets for the realignement (raster mode)
    output$replicates <- renderUI({
      if(!is.null(input$ReplicateSample)){
        if(length(grep(input$selectRealign,currentProject()$samplesFiles)) != 0){
          if(length(which(is.element(input$ReplicateSample, names(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]])) == FALSE)) == 0){
            plot_output_list <- lapply(1:length(input$ReplicateSample), function(i) {
              plotname <- paste("plot", i, sep="")
              numericInput(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][which(names(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[i])],paste0("realign of", currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files[which(names(generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]]) == input$ReplicateSample[i])]),value = 0)
            })
          } else {}
        }else{}
      } else {}
      
    })
    
    # set deplace$val
    observe({
      if(is.null(currentProject())){      
      } else if(is.null(input$selectRealign)){      
      }else{    
        
        input$selectRealign
        
        if(length(grep(input$selectRealign,currentProject()$samplesFiles))!= 0){
          
          if(is.null(eval(parse(text = paste("input$",generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)],sep=""))))){ 
          } else{
            for (i in 1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)){ 
              deplace$val[i] <- eval(parse(text = paste("input$",generRRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][i],sep="")))
            } 
          }
        } else {}
      } 
    }) # observe
    
    # set the flagRealign$temp and save the data when input$SauvegarderSpot is pressed (spot mode)
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$SauvegarderSpot)){      
      }else{      
        if(length(grep(input$selectRealign,currentProject()$samplesFiles)) != 0){
          isolate({
            if(input$SauvegarderSpot > 0){ 
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFinalSpot(tabSpotSave$temp)
              flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1] <- flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][1] + 1 
            } else {}
          })
        } else {}
      }
    }) # observe
    
    # calculate tabSpotDisplay$temp & tabSpotSave$temp (spot mode)
    observe({
      if(is.null(currentProject())){      
      } else if(is.null(isolate(flagRealign$temp))){      
      }else if(is.null(input$ReplicateSpot)){      
      }else if(!is.null(input$typeTraitement)){
        
        if(input$typeTraitement == "spot"){
          
          if(length(grep(input$selectRealign,currentProject()$samplesFiles)) != 0){
            if((isolate(flagRealign$temp)[[grep(input$selectRealign,currentProject()$samplesFiles)]][1]%%2) != 1){
              
              if(is.matrix(tabProvSpot$temp) | is.data.frame(tabProvSpot$temp)){
                
                vect <- vector()
                
                mat <- matrix(nrow = 1, ncol = ncol(tabProvSpot$temp))
                
                for(i in 1:nrow(tabProvSpot$temp)){
                  if(length(which(str_detect(rownames(tabProvSpot$temp)[i], input$ReplicateSpot) == TRUE)) >0){mat <- rbind(mat, tabProvSpot$temp[i,]); vect <- c(vect, i)}
                }
                
                tempMatrix <- mat[-1,]
                rownames(tempMatrix) <- rownames(tabProvSpot$temp)[vect]
                
                if(nrow(tempMatrix) != 0){
                  
                  if(length(input$ReplicateSpot) == 1){
                    
                    tabMean <- tempMatrix[1,]
                    
                    tabSD <- tempMatrix[2,]
                  }else{
                    
                    tabMean <- apply(tempMatrix[(1:length(input$ReplicateSpot)),], 2, mean, na.rm = TRUE)
                    
                    tabSD <- apply(tempMatrix[(1:length(input$ReplicateSpot)),], 2, sd, na.rm = TRUE)
                    
                  }
                  
                  temp <- rbind(tempMatrix, tabMean, tabSD)
                  
                  rownames(temp) <- c(rownames(tempMatrix), "total_mean", "total_SD")
                  
                  tabSpotSave$temp <- temp
                  
                  tabProv <- format(temp,scientific=TRUE, digits = 2)    
                  
                  tabSpotDisplay$temp <- cbind(rownames(tabProv), tabProv)
                } else {}
              } else {}
              
            } else {}
          } else {}
        } else {}
      }
      
    }) # observe
    
    # set tabProvSample$temp (raster mode)
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(deplace$val)){      
      }else if(is.null(flagRealign$temp)){      
      }else if(is.null(input$selectRealign)){      
      }else if(is.null(input$ReplicateSample)){      
      }else{       
        deplace$val 
        if(length(grep(input$selectRealign,currentProject()$samplesFiles)) != 0){
          
          if(input$typeTraitement == "raster" & (flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2]%%4) == 0){
            if(length(which(is.element(input$ReplicateSample, names(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre)) == FALSE)) == 0){
              
              tabProvSample$temp <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$intermStepRaster(decalage = deplace$val, input = input$ReplicateSample)
            } else {}
            
          }    else {}     
        }  else {}
        
      }
    }) # observe
    
    # initilize tabProvSpot$temp & tabProvSample$temp
    observe({
      if(!is.null(input$selectRealign)){
        if(length(which(flagSample$temp == TRUE)) != 0){
          if(length(grep(input$selectRealign,currentProject()$samplesFiles)) !=0){
            currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFiltre() 
            tabProvSpot$temp <-  currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$intermStepSpot()
            tabProvSample$temp <-  currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre
          } else {}
          
        }  else {}
      } else {}
      
    }) # observe
    
    # set flagRealign$temp and average the data when input$MoyenneRaster is pressed (raster mode) 
    observe({
      if(is.null(currentProject())){      
      } else if(is.null(input$MoyenneRaster)){      
      } else if(is.null(isolate(flagRealign$temp))){ 
      } else{      
        if(length(grep(isolate(input$selectRealign),currentProject()$samplesFiles)) != 0){        
          if(input$MoyenneRaster > 0){        
            isolate({ 
              if(length(input$ReplicateSample) == 0){
                tkmessageBox(message = "You need to select at least one replicate to average!", icon = "error", type = "ok")
              } else {
                currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataIntermRaster(tabProvSample$temp)
                currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFinalRaster()
                flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2] <- flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2] + 2     
              }
       
            })
          } else {}
          
        } else {}
      }
    }) # observe
    
    # set flagRealign$temp and save the data when input$SauvegarderReal is pressed (raster mode) 
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(input$SauvegarderReal)){      
      }else{      
        if(length(grep(isolate(input$selectRealign),currentProject()$samplesFiles)) != 0){      
          if(input$SauvegarderReal > 0){  
            isolate({
              flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2] <- flagRealign$temp[[grep(input$selectRealign,currentProject()$samplesFiles)]][2] + 1
            })
          } else {}
          
        } else {}
      }
    }) # observe
    
    #######################
    ##### CONFIGURATION ##
    #######################
    
    valeurColor <- reactiveValues(temp = NULL) # a vector of character string created by generRansd which correspond to the id of the chosen color for each element
    
    # create valeurColor$temp
    observe({
      if(is.null(currentProject())){      
      }else if(is.null(color$temp)){      
      }else{
        valeurColor$temp <- geneR(letters, 4, length(color$temp), c(waste$temp, geneRMachineCorr$temp, generRRealign$temp, rankStandard$temp, rankSample$temp))
      }   
    })
    
    # define output$config0
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
    
    # return to previous step
    observe({
      if(!is.null(input$returnButton)){
        if(input$returnButton != 0){
          isolate({
            updateTabItems(session, "tab", selected = currentPage$temp[2])
          })
        }
      } else {}
    })
    
    # define output$config1
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
    
    # define output$config2
    observe({
      if(is.null(currentProject())){
        output$config2 <- renderUI({NULL}) # eo output$config2
      } else {
        output$config2 <- renderUI({
          fluidRow(
            box(
              solidHeader = TRUE,
              collapsible = FALSE,
              width = 12,
              status = "primary",
              title = "Calculation settings", 
              div(
                div(p("Choose the value with which remplace the plateau value under the limite of detection"), style = "display: inline-block;vertical-align: top;margin-top:15px; margin-right:20px"),
                div(selectInput("valRemplace", "", choices = c("NA", "0", "Averaged value of the blank"), selected = "NA", width = '100%'), style = "display: inline-block; margin-top:-10px; width: 300px")
              )
            )
          )
        }) # eo output$config2
      }
    })    
    
    # define output$config3
    observe({
      if(is.null(currentProject())){
        output$config3 <- renderUI({NULL}) # eo output$config3
      }else if(is.null(color$temp)){      
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
                    lapply(1:length(currentProject()$listeElem), function(x){
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
    
    # set color$temp
    observe({
      if(!is.null(valeurColor$temp)){
        if(is.null(eval(parse(text = paste0("input$",valeurColor$temp[length(valeurColor$temp)]))))){        
        }else{      
          for (i in 1: length(valeurColor$temp)){
            color$temp[i] <- eval(parse(text = paste0("input$",valeurColor$temp[i])))
            
          }
          
        } 
      } else {}
      
    })
    
    
    }#eo server
  
  
  ######################
  ######## CALL shinyApp
  ######################
  app <- shinyApp(ui, server)
  runApp(app, launch.browser = T)
    }
