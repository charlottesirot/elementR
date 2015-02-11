###################
shinyUI(navbarPage('Otolith raster Analysis',
                   tabPanel("Analysis parameters",
                            sidebarPanel(
                              h4("Session to load"),
                              wellPanel(
                                actionButton("go","Go!"),
                                br(),
                                br(),
                                radioButtons("Blanc","Choisir le type de blanc",choices = c("blanc pre-sample","un seul standardise"), selected = "blanc pre-sample"),
                                uiOutput('MethodeBlanc'),
                                uiOutput('renduBlancSession')
                              ),
                              wellPanel(
                                uiOutput('GeneralNISTelemchoiceMenu'),
                                
                                selectInput(inputId = "lineType",label = "Plot type:",choices = c("l", "b","p"),selected = "b"),
                                
                                sliderInput("LWD","Line width:",value = 2,min = 1,max = 4),
                                
                                sliderInput("LTY","Line 'lty':",value = 1,min = 1,max = 4),
                                
                                sliderInput("ticklsize","Size of ticks", value = 1.5,min = 0.5, max = 4,step = 0.5),
                                
                                sliderInput("axislegend","Size axis legend", value = 1.5,min = 0.5, max = 4,step = 0.5)
                                
                              ),
                              h4("Select your margines"),
                              wellPanel(
                                sliderInput("margine1","select your buttom margine", value = 5.1,min = 0, max = 10, step = 0.5),
                                sliderInput("margine2","select your left margine", value = 4.1,min = 0, max = 10, step = 0.5),
                                sliderInput("margine3","select your upper margine", value = 4.1,min = 0, max = 10, step = 0.5),
                                sliderInput("margine4","select your right margine", value = 2.1,min = 0, max = 10, step = 0.5)
                              )
                              
                            ),
                            mainPanel(
                              tableOutput('dataexploiT')  
                            )
                   ),
                   tabPanel("NIST",
                            sidebarPanel(
                              conditionalPanel(condition="input.conditionedPanels==1",
                                               h4("NIST Selection"),
                                               wellPanel(
                                                 uiOutput('NISTselection'),
                                                 uiOutput('SaveNIST')
                                               ),
                                               uiOutput('titre1'),
                                               uiOutput('titre2'),
                                               uiOutput('limit'),
                                               uiOutput('titre3'),
                                               uiOutput('retour'),
                                               uiOutput('Aide'),
                                               uiOutput('titre4'),
                                               uiOutput('retour2')
                              )
                              ,
                              conditionalPanel(condition="input.conditionedPanels==2", 
                                               uiOutput('Text'),
                                               uiOutput('choix'),
                                               uiOutput('selection'),
                                               uiOutput('BlancStandard'),
                                               wellPanel(
                                                 uiOutput('etalon'),
                                                 uiOutput('valeur4')                                                 
                                               ),
                                               uiOutput('boton')
                              )
                              
                            ),
                            mainPanel(
                              div(
                                tabsetPanel(
                                  tabPanel("Plot",
                                           plotOutput('Plot',width = "100%", height = "1500px"), value = 1
                                  )
                                  ,
                                  tabPanel("Data",
                                           tableOutput('NISTTableau'), value = 1
                                  ),
                                  tabPanel("Result Sum up" , value = 2 ,
                                           plotOutput('validation'), 
                                           tableOutput('TableMoy')
                                  ), id = "conditionedPanels"
                                ),class = "span12"
                              )
                              
                            )
                   ),
                   tabPanel("Samples", 
                            sidebarPanel(
                              conditionalPanel(condition="input.conditionedPanelsample==1",
                                               wellPanel(uiOutput('SAMPLEselection'),
                                                         uiOutput('Rep'),
                                                         uiOutput('SaveSample')),
                                               uiOutput('GeneralSAMPLEelemchoiceMenu'),
                                               uiOutput('titre2Samples'),
                                               uiOutput('limitSamples'),
                                               uiOutput('titre3Samples'),
                                               uiOutput('retourSamples'),
                                               uiOutput('titre4Samples'),
                                               uiOutput('retour2Samples')
                              ),
                              conditionalPanel(condition="input.conditionedPanelsample==2",
                                               uiOutput('echantillon'),
                                               uiOutput('facon'),
                                               uiOutput('text'),
                                               uiOutput('selectionFinal'),
                                               uiOutput('slider'),
                                               uiOutput('Moyenner'),
                                               br(),
                                               uiOutput('Transformer'),
                                               uiOutput('Param1'),
                                               uiOutput('Param2'),
                                               uiOutput('SaveFinal')
                                               
                              )),
                            
                            mainPanel(
                              div(tabsetPanel(
                                tabPanel("Plot", value = 1,
                                         plotOutput('PlotSamples',width = "100%", height = "1500px")
                                ),
                                tabPanel("Data",value = 1,
                                         tableOutput('SAMPLETableau')
                                ),
                                tabPanel("realignement",value = 2,
                                         plotOutput('graph2',clickId = "Cliquer"),
                                         tableOutput('graph')
                                ),id = "conditionedPanelsample"
                              ),class = "span12"
                              )
                              
                            )
                   )
                   
))