shinyServer(function(input, output,session) {
  
  ## Selection méthode de choix de blanc
  observe({
    if(input$Blanc == "blanc pre-sample"){output$MethodeBlanc <- renderUI({return()})}          
    if(input$Blanc == "un seul standardise"){output$MethodeBlanc <- renderUI({actionButton("Cherche","Blanc")})
    }
  })
  
  # Si l'utilisateur à choisit le blanc standardisé (i.e. un blanc pour toute la session) alors :
  BlancAUtiliser <- reactiveValues(N = NULL) # Tableau du blanc (Mean et LOD)
  BLancNom <- reactiveValues(N1 = NULL) # Nom du dossier de blanc
  
  ## recherche du fichier de blanc 
  observe({
    if(is.null(input$Cherche)){}
    else{
      if(input$Cherche > 0){          
        BLancNom$N1 <- file.choose()
        output$renduBlancSession = renderUI({h5(BLancNom$N1) })
        MeanLOD = as.matrix(read.csv(BLancNom$N1,dec = ".", sep=",",h = T))[,-1]              
        BlancAUtiliser$N = MeanLOD
      }
    }
  })
  
  observe({
      if(input$go > 0){
        
        directGen <- choose.dir() # Dossier de travail
        
        dir.create(paste(directGen,"/","Traités", sep = ""))
        dir.create(paste(directGen,"/","Traités/NISTs", sep = ""))
        dir.create(paste(directGen,"/","Traités/Samples", sep = ""))
        dir.create(paste(directGen,"/","Traités/Blanc", sep = ""))
        
        NomSample <- dir(paste(directGen, "\\Samples", sep = ""))
        
        for(m in 1:length(NomSample)){
          dir.create(paste(directGen,"\\Traités\\Samples\\",NomSample[m], sep = ""))
        } 
        
        # Création du tableau des NIST et samples à analyser
        output$dataexploiT = renderTable({
          listNIST <- NIST(directGen)
          listSamples <- Sample(directGen)
          
          M <- matrix(0,max(length(listNIST),length(listSamples)),2)
          colnames(M) <- c("NISTs","Samples")
          
          M[1:length(listNIST),1] <- listNIST
          M[1:length(listSamples),2] <- listSamples
          return(as.data.frame(M))
        })
        
        # verification des NISTs déjà traités
        wholeNIST <- reactiveValues(V = NIST(directGen)) # liste des NISTs à traiter
        done <- reactiveValues(vect = str_replace(dir(paste(directGen,"/","Traités/NISTs", sep = ""), pattern="\\.RData$"),".RData","")) # Liste des NISTs déjà traités
        
        Temoin = reactiveValues(valeur= NULL) # Témoin permettant de savoir si tous les NISTs ont été traités 1 tous les NISTs sont traités - 0 il y en a au moins un qui n'est pas traité
                    
        #mettre un observe ?
        if(isolate(done$vect) == isolate(wholeNIST$V)){Temoin$valeur <- 1}
        else {Temoin$valeur <- 0}  

        # fin de la vérification des NISTs déjà traités

        # Vérification des Samples déjà traités
        
        listeAfaire <- reactiveValues(Liste = list()) # liste des Samples à traiter  
        isolate({
          for(m in 1:length(NomSample)){
            listeAfaire$Liste[[m]] <- dir(paste(directGen, "\\Samples\\",NomSample[m], sep = ""), pattern = "\\.csv$")
          }
          names(listeAfaire$Liste) <- dir(paste(directGen, "\\Samples", sep = ""))
        })
        
        
        listedone <- reactiveValues(Liste2 = list()) # liste des Samples traités
        isolate({        
          for(m in 1:length(NomSample)){
            listedone$Liste2[[m]] <- str_replace(dir(paste(directGen, "\\Traités\\Samples\\",NomSample[m], sep = ""),pattern = "\\.RData$"),".RData","") 
          }
          names(listedone$Liste2) <- dir(paste(directGen, "\\Samples", sep = ""))
        })
        
        listeTemoin <- reactiveValues(Liste3 = lapply(1:length(listeAfaire$Liste),function(x){ identical(listeAfaire$Liste[[x]],listedone$Liste2[[x]])})) # Témoin permettant de savoir si tous les Samples ont été traités
        
        # fin de la vérification des Samples déjà traités
        
        output$GeneralNISTelemchoiceMenu = renderUI({
          if(is.null(NIST(directGen))) return()
          else{
            wellPanel(
              selectInput("GeneralElementChoice","Choose elements to consider in the analysis", elem(dat("NISTs",NIST(directGen)[1], directGen)), multiple = T,selected = elem(dat("NISTs",NIST(directGen)[1],directGen)))
            )
          }
          
        })
        
        TableEtalon <- reactiveValues(et = matrix(NA,1,1))
        tableResumNist <- reactiveValues(File = NA)
        TableauMeanTotNISTs <- reactiveValues(name = NA)
        TableauSDTotNISTs <- reactiveValues(name = NA)
        
        essai1S = reactiveValues(n1S = 0)
        essai2S = reactiveValues(n2S = 0)
        
        ##########################################
        ########  NISTS ##########################
        ##########################################
        output$NISTselection = renderUI({
          selectInput("NistFileSelection","Choose the NIST to analyze",NIST(directGen))
        })
        output$SaveNIST = renderUI({
          wellPanel(
            actionButton("sauvegarderNIST","sauvegarder!"),
            actionButton("supprimerNIST","supprimer!")
          )
        })
        
        observe({
          if(is.null(input$sauvegarderNIST)){}
          else{
            if(input$sauvegarderNIST > 0){
              isolate({
                
                donneprov = dat("NISTs",input$NistFileSelection, directGen)
                donne = cbind(donneprov[,1], donneprov[,recherche(colnames(donneprov), input$GeneralElementChoice)])
                
                LIMIT1_Basse <- donne[lePlusProche(y = input$limitStep1[[1]], x = donne[,1])[[2]],1]
                LIMIT1_Haute <- donne[lePlusProche(y = input$limitStep1[[2]],x = donne[,1])[[2]],1]
                LIMIT2_Basse <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[1]],x = donne[,1])[[2]],1]
                LIMIT2_Haute <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[2]],x = donne[,1])[[2]],1]
                
                if(input$Blanc == "blanc pre-sample"){MeanLOD = calculLOD(donne,c(LIMIT1_Basse,LIMIT1_Haute)); Fichier = NA}          
                if(input$Blanc == "un seul standardise"){MeanLOD = BlancAUtiliser$N; Fichier = BLancNom$N1}
                
                
                tableauBlancNIST <- tableauBlanc(donne,c(LIMIT1_Basse,LIMIT1_Haute))                
                
                donne2 <- tableauCentree(donne,c(LIMIT2_Basse,LIMIT2_Haute))
                
                donne3 <- tableauMoinsLOD(donne2,MeanLOD[,2:dim(MeanLOD)[2]]) 
                
                donne4 <- TableauSupprLOD(donne3,MeanLOD)
                
                donne5 <- tableauNormalise(donne4)
                
                donne6 <- TableauDesanomalise(donne5)
                
                
                Asauver <- list(
                  nom = input$NistFileSelection,
                  elem = input$GeneralElementChoice,
                  limitAnalyse= input$limitStep1,
                  elemBase = input$BaseElementChoice,
                  elemControl = input$ControleElementChoice,
                  limitPlateau = input$Valeur_Plateau_Base,
                  tableauBrut = donneprov,
                  tableauBrutElemChoisi = donne,
                  tableauBlancNIST = tableauBlancNIST,
                  MeanLOD = MeanLOD,
                  tableauBrutPlateau = donne2,
                  TableauPlateauMoinsLOD = donne3,
                  TableauPlateauMoinsLODSupLOD = donne4,
                  TableauPlateauMoinsLODSupLODNorm = donne5,
                  TableauPlateauMoinsLODSupLODNormSDesanomalized = donne6,
                  TableauPlateauMoinsLODSupLODNormConcentration = matrix(NA,dim(donne6)[1],dim(donne6)[2]),
                  Moyenne = apply(donne6, 2, mean, na.rm = T),
                  sd = apply(donne6, 2, sd,na.rm = T),
                  preuve = input$Blanc,
                  Fichier = Fichier
                )           
                setwd(paste(directGen,"/","Traités/NISTs", sep = ""))
                save(Asauver, file = paste(input$NistFileSelection,".RData", sep=""))
                write.csv(tableauBlancNIST,file = paste(input$NistFileSelection,"_tableauBlanc.csv", sep = ""))
                write.csv(donne6,file = paste(input$NistFileSelection,".csv", sep = ""))
                #jpeg(paste(input$NistFileSelection, ".jpg"),  width = 480, height = 480,quality = 100)
                #plot(donne[,1], donne[,2])
                #dev.off()
                setwd(directGen)
                done$vect <- str_replace(dir(paste(directGen,"/","Traités/NISTs", sep = ""), pattern="\\.RData$"),".RData","")
                if(length(done$vect) == length(wholeNIST$V)){
                  if(length(done$vect == wholeNIST$V) == length(done$vect)){Temoin$valeur <- 1}
                  else{Temoin$valeur <- 0}}
                if(length(done$vect) != length(wholeNIST$V)){Temoin$valeur <- 0}
              })
            }
          } 
        }) ## Sauvegarde données -> Temoin$valeur = 1 (ssi tous les nists sont traités)
        
        observe({
          if(is.null(input$supprimerNIST)){}
          else {
            if(input$supprimerNIST > 0){
              isolate({
                setwd(paste(directGen,"/","Traités/NISTs", sep = ""))
                unlink(paste(input$NistFileSelection,".RData", sep=""))
                setwd(getwd())
                done$vect = str_replace(dir(paste(directGen,"/","Traités/NISTs", sep = ""), pattern="\\.RData$"),".RData","")
                Temoin$valeur = 0
              })
            }
          }
        }) ## Suppression données -> Temoin$valeur = 0
        
        observe({
          if(length(which(done$vect == input$NistFileSelection)) != 0){
            output$titre1 = renderUI({return()})
            output$titre2 = renderUI({return()})
            output$limit = renderUI({return()})  
            output$titre3 = renderUI({return()})
            output$retour = renderUI({return()})
            output$titre4 = renderUI({return()})
            output$retour2 = renderUI({return()})
            output$NISTTableau = renderTable({return()})
            output$Plot = renderPlot({return()})
            output$retour3 = renderUI({return()})
          }
        })
        
        observe({
          if(length(which(done$vect == input$NistFileSelection)) == 0){            
            output$titre2 = renderUI({
              h4("General limits of time (Graphic 1)")
            })
            output$limit = renderUI({
              if(is.null(input$NistFileSelection)) return()
              else{
                donne <- dat("NISTs",input$NistFileSelection, directGen)
                stepTemps <- donne[3,1] - donne[2,1]
                wellPanel(
                  sliderInput("limitStep1","Choose the limit of time (s)",value = c(min(donne[,1], na.rm = T),max(donne[,1], na.rm = T)), min = min(donne[,1], na.rm = T), max = max(donne[,1], na.rm = T),step = stepTemps)
                )
              }
            })
            output$titre3 = renderUI({
              h4("Plateau definition (Graphic 2)")
            })
            output$retour = renderUI({
              donneprov <- dat("NISTs",input$NistFileSelection, directGen)
              donne <- cbind(donneprov[,1], donneprov[,recherche(colnames(donneprov), input$GeneralElementChoice)])
              stepTemps <- donne[3,1] - donne[2,1]
              Moy <- input$limitStep1[2]-input$limitStep1[1]
              wellPanel(
                selectInput("BaseElementChoice","Choose Base element", elem(dat("NISTs",input$NistFileSelection, directGen)),selected = "Ca43"),
                selectInput("ControleElementChoice","Choose Control element", elem(dat("NISTs",input$NistFileSelection, directGen)),selected = "Ba138"), 
                radioButtons("view", "your view", choices = list("Base element"= 1,"controle element"=2), selected = 1),
                sliderInput("Valeur_Plateau_Base","Base element Plateau", value= c(input$limitStep1[1], input$limitStep1[2]), min = input$limitStep1[1], max =input$limitStep1[2],step = stepTemps)
              )
            })
            output$titre4 = renderUI({
              h4("Plateau in itself (Graphic 3)")
            })
            output$retour2 = renderUI({
              wellPanel(
                selectInput("type_de_donne","Choose data to show",choices = c("raw","Blanc","Centred","- LOD","> LOD","normalized","de-anomalized"),selected = "Centred", multiple= F),
                selectInput("Elemchoisi","Element à visualiser", choices = input$GeneralElementChoice,selected = "Ca43")
              )
            })                        
            output$NISTTableau = renderTable({
              if(is.null(input$NistFileSelection)) return()
              else{
                donneprov <- dat("NISTs",input$NistFileSelection, directGen)
                donne <- cbind(donneprov[,1], donneprov[,recherche(colnames(donneprov), input$GeneralElementChoice)])
                
                LIMIT1_Basse <- donne[lePlusProche(y = input$limitStep1[[1]], x = donne[,1])[[2]],1]
                LIMIT1_Haute <- donne[lePlusProche(y = input$limitStep1[[2]],x = donne[,1])[[2]],1]
                LIMIT2_Basse <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[1]],x = donne[,1])[[2]],1]
                LIMIT2_Haute <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[2]],x = donne[,1])[[2]],1]
                
                if(input$Blanc == "blanc pre-sample"){MeanLOD = calculLOD(donne,c(LIMIT1_Basse,LIMIT1_Haute))}          
                if(input$Blanc == "un seul standardise"){MeanLOD = BlancAUtiliser$N}
                
                
                tableauBlancNIST <- tableauBlanc(donne,c(LIMIT1_Basse,LIMIT1_Haute))
                
                donne2 <- tableauCentree(donne,c(LIMIT2_Basse,LIMIT2_Haute))
                
                donne3 <- tableauMoinsLOD(donne2,MeanLOD[,2:dim(MeanLOD)[2]]) 
                
                donne4 <- TableauSupprLOD(donne3,MeanLOD)
                
                donne5 <- tableauNormalise(donne4)
                
                donne6 <- TableauDesanomalise(donne5)              
                
                if(input$type_de_donne == "Blanc"){return(tableauBlancNIST)}
                if(input$type_de_donne == "raw"){return(donne)}
                if(input$type_de_donne == "Centred"){return(donne2)}
                if(input$type_de_donne == "- LOD"){return(donne3)}              
                if(input$type_de_donne == "> LOD"){return(donne4)}              
                if(input$type_de_donne == "normalized"){return(donne5)}
                if(input$type_de_donne == "de-anomalized"){return(donne6)}
              }
            }, digits = 7)            
            output$Plot = renderPlot({
              if(is.null(input$NistFileSelection)) return()
              else{
                
                donneprov <- dat("NISTs",input$NistFileSelection, directGen)
                donne <- cbind(donneprov[,1], donneprov[,recherche(colnames(donneprov), input$GeneralElementChoice)])
                
                LIMIT1_Basse <- donne[lePlusProche(y = input$limitStep1[[1]], x = donne[,1])[[2]],1]
                LIMIT1_Haute <- donne[lePlusProche(y = input$limitStep1[[2]],x = donne[,1])[[2]],1]
                LIMIT2_Basse <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[1]],x = donne[,1])[[2]],1]
                LIMIT2_Haute <- donne[lePlusProche(y = input$Valeur_Plateau_Base[[2]],x = donne[,1])[[2]],1]
                
                if(input$Blanc == "blanc pre-sample"){MeanLOD = calculLOD(donne,c(LIMIT1_Basse,LIMIT1_Haute))}          
                if(input$Blanc == "un seul standardise"){MeanLOD = BlancAUtiliser$N}
                
                
                tableauBlancNIST <- tableauBlanc(donne,c(LIMIT1_Basse,LIMIT1_Haute))
                
                donne2 <- tableauCentree(donne,c(LIMIT2_Basse,LIMIT2_Haute))
                
                donne3 <- tableauMoinsLOD(donne2,MeanLOD[,2:dim(MeanLOD)[2]]) 
                
                donne4 <- TableauSupprLOD(donne3,MeanLOD)
                
                donne5 <- tableauNormalise(donne4)
                
                donne6 <- TableauDesanomalise(donne5) 
                
                color <- rainbow(length(input$GeneralElementChoice))
                maximum <- max(donne[,1:dim(donne)[2]], na.rm = T)
                par(mfrow = c(3,1), mar = c(input$margine1, input$margine2, input$margine3, input$margine4))
                
                plot(donne[,1], donne[,2], col = color[1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, ylim = c(0,maximum), axes = T, xlab = "Temps", ylab = "Cps",type= input$lineType)
                par(new = T)
                
                for (i in 3:(dim(donne)[2]-1)){
                  plot(donne[,1], donne[,i], col = color[i], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, ylim = c(0,maximum), axes = F, xlab = "", ylab = "",type= input$lineType)
                  par(new = T)
                } 
                plot(donne[,1], donne[,dim(donne)[2]], col = color[dim(donne)[2]], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, ylim = c(0,maximum), axes = F, xlab = "", ylab = "",type= input$lineType)
                legend("topright", legend = colnames(donne)[-1], cex = 1.5, col = color, pch = 16)
                abline(v = LIMIT1_Basse, lty = "dashed", col = "blue")
                abline(v = LIMIT1_Haute, lty = "dashed", col = "blue")
                
                if(input$view == 1){numero = recherche(colnames(donne2), input$BaseElementChoice); titre = "Base"}
                if(input$view == 2){numero = recherche(colnames(donne2), input$ControleElementChoice); titre = "Controle"}
                
                plot(donne[,numero]~donne[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",colnames(donne2)[numero]), main = titre,type= input$lineType)
                abline(v = LIMIT1_Basse, lty = "dashed", col = "blue")
                abline(v = LIMIT1_Haute, lty = "dashed", col = "blue")
                abline(v = LIMIT2_Basse, lty = "dashed", col = "red")
                abline(v = LIMIT2_Haute, lty = "dashed", col = "red")
                
                points(x = LIMIT2_Basse, y = donne[lePlusProche(donne[,1],input$Valeur_Plateau_Base[1])[[2]][1],numero], col="black", pch = 21,bg = "red", cex = 2.5)
                points(x = LIMIT2_Haute, y = donne[lePlusProche(donne[,1],input$Valeur_Plateau_Base[2])[[2]][1],numero], col="black", pch = 21, bg = "red",cex = 2.5)
                
                if(input$type_de_donne == "Blanc"){plot(tableauBlancNIST[,recherche(colnames(tableauBlancNIST),input$Elemchoisi)]~tableauBlancNIST[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType )
                }              
                if(input$type_de_donne == "raw"){plot(donne[,recherche(colnames(donne),input$Elemchoisi)]~donne[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType )
                }
                if(input$type_de_donne == "Centred"){plot(donne2[,recherche(colnames(donne2),input$Elemchoisi)]~donne2[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType )
                }
                if(input$type_de_donne == "- LOD"){plot(donne3[,recherche(colnames(donne3),input$Elemchoisi)]~donne3[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType)
                }
                if(input$type_de_donne == "> LOD"){plot(donne4[,recherche(colnames(donne4),input$Elemchoisi)]~donne4[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType)
                }
                if(input$type_de_donne == "normalized"){plot(donne5[,recherche(colnames(donne5),input$Elemchoisi)]~donne5[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType)
                }
                if(input$type_de_donne == "de-anomalized"){plot(donne6[,recherche(colnames(donne6),input$Elemchoisi)]~donne6[,1], lwd = input$LWD, lty = input$LTY,cex.lab= input$axislegend, cex.axis = input$ticklsize, col = "#246309", xlab = "Temps", ylab = paste("cps: ",input$Elemchoisi), main =input$type_de_donne,type= input$lineType)
                }
                
              }
              
            })
          }
          
        })
        
        observe({
          if(is.null(Temoin$valeur)){}
          else{
            if(Temoin$valeur == 1){
              
              setwd(paste(directGen,"/","Traités/NISTs", sep = ""))
              
              Afaire <-dir(, pattern="\\.RData$")
              AfaireNom <- sapply(strsplit(Afaire,".", fixed = T),function(x){x[1]})
              
              Moyenne = list()
              SD = list()                        
              ListeMoyenneNonNormalise = list()
              
              ListBlanc = list()
              vecteurnom = vector()
              
              for(i in 1:length(Afaire)){                
                load(Afaire[i])
                m <- dim(Asauver$tableauBlancNIST)[2]
                n <- dim(Asauver$tableauBlancNIST)[1]
                vecteurnom <- c(vecteurnom, rep(Afaire[i],n))
                
                Moyenne[[i]] = Asauver$Moyenne
                SD[[i]] = Asauver$sd
                ListeMoyenneNonNormalise[[i]] = Asauver$TableauPlateauMoinsLODSupLOD
                
                ListBlanc[[i]] <- Asauver$tableauBlancNIST
                colnames(ListBlanc[[i]]) <- names(Asauver$Moyenne)            
                
              }   
              
              #pb ici à remplacer directement par la réactiveValues --> à voir
              tableauMoyenne <- do.call(rbind, Moyenne)
              colnames(tableauMoyenne) <- names(Moyenne[[1]])
              rownames(tableauMoyenne) <- paste("Moy",AfaireNom)
              
              tableauSD <- do.call(rbind, SD)
              colnames(tableauSD) <- names(SD[[1]])
              rownames(tableauSD) <- paste("sd", AfaireNom)
              
              tableauBlancResumeSession <- do.call(rbind, ListBlanc)
              colnames(tableauBlancResumeSession) <- names(Moyenne[[1]])
              
              BlancMoyenne <- apply(tableauBlancResumeSession[,2:dim(tableauBlancResumeSession)[2]], 2, mean, na.rm = T)
              BlancSD <- 3*apply(tableauBlancResumeSession[,2:dim(tableauBlancResumeSession)[2]], 2, sd, na.rm = T)
              BlancAsauver <- rbind(BlancMoyenne,BlancSD)
              
              write.csv(tableauBlancResumeSession,file = paste(directGen,"/","Traités/Blanc/Blancs session.csv",sep = ""))
              write.csv(BlancAsauver,file = paste(directGen,"/","Traités/Blanc/Blanc moyenne session.csv",sep = ""))
              
              TableauMeanTotNISTs$name <- tableauMoyenne
              TableauSDTotNISTs$name <- tableauSD
              
              output$selection = renderUI({
                selectInput("elementSelection","selectionner element considerer", choices = names(Moyenne[[1]])[-1])
              })
              output$validation = renderPlot({
                Aploter = recherche(colnames(tableauMoyenne), input$elementSelection)
                max=max(tableauMoyenne[,Aploter], na.rm = T) + 10*max(tableauSD[,Aploter], na.rm = T)
                min = min(tableauMoyenne[,Aploter], na.rm = T) - 10*min(tableauSD[,Aploter], na.rm = T)
                plot(as.factor(AfaireNom),rep(-1,length(Afaire)),  xlim = c(1,length(Afaire)), type = "p", ylim = c(min,max), xlab= "Nom ", ylab = "cps moyen")
                par(new = T)
                plotCI(tableauMoyenne[,Aploter], xlim =c(1,length(Afaire)), ylim = c(min,max), uiw = tableauSD[,Aploter], liw = tableauSD[,Aploter], xlab = "", ylab = "", gap = 0.2, axes = F)
                
              })
              output$TableMoy = renderTable({
                return(rbind(tableauMoyenne, tableauSD))
              })          
              output$etalon = renderUI({
                actionButton("FichierEtalon","Fichier étalon")})
              
              # que veut dire cette réctive value ??
              bouton = reactiveValues(Val = 0)
                                          
              # Sauvegarde plots et du tableau des moyennes des NISTs (Normalisé)
              
              
              Min1 <- apply(TableauMeanTotNISTs$name, 2, min, na.rm = T) - 1.10*apply(TableauSDTotNISTs$name,2, max, na.rm = T)
              Max1 <- apply(TableauMeanTotNISTs$name, 2, max, na.rm = T) - 1.10*apply(TableauSDTotNISTs$name,2, max, na.rm = T)
              
              jpeg(file = paste(directGen,"/","Traités/Moyenne NISTs.jpg",sep = ""), width = 1500, height =1000, quality = 100)
              par(mfrow = c(round(sqrt(dim(TableauMeanTotNISTs$name)[2])),round(sqrt(dim(TableauMeanTotNISTs$name)[2]))))
              lapply(2:ncol(TableauMeanTotNISTs$name), function(x){
                plot(1,1)
                #niceplot(limX=c(0,dim(TableauMeanTotNISTs$name)[1]+1),limY=c(Min1[x] , Max1[x]), marg=c(4.5,5.5,2,0.5),tick=-0.4,nlabX=dim(TableauMeanTotNISTs$name)[1]+2,nlabY = 5,  labX=c(),labY=c(),nmlabX=c(),nmlabY=c(),lasX=1,lasY=1,    lineX=-0.1, lineY=0.1,   cexX=0.9, cexY=0.9,nmX="N°NIST",nmY="Moyenne",   cexXt=1, cexYt=1 , main = colnames(TableauMeanTotNISTs$name)[x], lineXt = 2, lineYt = 4)
                #meansey(1:dim(TableauMeanTotNISTs$name)[1],TableauMeanTotNISTs$name[,x],TableauSDTotNISTs$name[,x],pchp=22,colp="black",bgp="black",cexp=1.5,colb="black",lgt=0.01)
                #lapply(1:dim(TableauMeanTotNISTs$name)[1], function(k){ abline(v = k,lty = 3) })
              })
              dev.off()
              
              
              #for(z in 2:dim(TableauMeanTotNISTs$name)[2]){
               # min <- min(TableauMeanTotNISTs$name[,z], na.rm = T) - 1.10*max(TableauSDTotNISTs$name[,z], na.rm = T)
              #  max <- max(TableauMeanTotNISTs$name[,z], na.rm = T) + 1.10*max(TableauSDTotNISTs$name[,z], na.rm = T)
                
              #  niceplot(limX=c(0,dim(TableauMeanTotNISTs$name)[1]+1),limY=c(min , max), marg=c(4.5,5.5,2,0.5),tick=-0.4,nlabX=dim(TableauMeanTotNISTs$name)[1]+2,nlabY = 5,  labX=c(),labY=c(),nmlabX=c(),nmlabY=c(),lasX=1,lasY=1,    lineX=-0.1, lineY=0.1,   cexX=0.9, cexY=0.9,nmX="N°NIST",nmY="Moyenne",   cexXt=1, cexYt=1 , main = colnames(TableauMeanTotNISTs$name)[z], lineXt = 2, lineYt = 4)
              #  meansey(1:dim(TableauMeanTotNISTs$name)[1],TableauMeanTotNISTs$name[,z],TableauSDTotNISTs$name[,z],pchp=22,colp="black",bgp="black",cexp=1.5,colb="black",lgt=0.01)
              #  for(j in 1:dim(TableauMeanTotNISTs$name)[1]){
              #    abline(v = j, lty = 3)
              #  }
              # }
              
              
             
              
              write.csv(rbind(TableauMeanTotNISTs$name,TableauSDTotNISTs$name),file = paste(directGen,"/","Traités/TableauTotalitéNISTs.csv",sep = ""))
              
              # Sauvegarde plots et du tableau des moyennes des NISTs (Non normalisé)
              
              matrixMean <- do.call(rbind,lapply(ListeMoyenneNonNormalise, function(x){apply(x,2,mean, na.rm = T)}))
              colnames(matrixMean) = colnames(TableauMeanTotNISTs$name)
              
              matrixSD <- do.call(rbind,lapply(ListeMoyenneNonNormalise, function(x){apply(x,2,sd, na.rm = T)}))
              colnames(matrixSD) = colnames(TableauMeanTotNISTs$name)
                      
              jpeg(file = paste(directGen,"/","Traités/Moyenne NISTs non normalisé.jpg",sep = ""),width = 1500, height =1000, quality = 100)
              par(mfrow = c(round(sqrt(dim(ListeMoyenneNonNormalise[[1]])[2])),round(sqrt(dim(ListeMoyenneNonNormalise[[1]])[2]))))
              for(z in 2:dim(ListeMoyenneNonNormalise[[1]])[2]){
                min = min(matrixMean[,z], na.rm = T) - 1.10*max(matrixSD[,z], na.rm = T)
                max = max(matrixMean[,z], na.rm = T) + 1.10*max(matrixSD[,z], na.rm = T)
                
                niceplot(limX=c(0,dim(matrixMean)[1]+1),limY=c(min , max), marg=c(4.5,5.5,2,0.5),tick=-0.4,nlabX=dim(matrixMean)[1]+2,nlabY = 5,  labX=c(),labY=c(),nmlabX=c(),nmlabY=c(),lasX=1,lasY=1,    lineX=-0.1, lineY=0.1,   cexX=0.9, cexY=0.9,nmX="N°NIST",nmY="Moyenne",   cexXt=1, cexYt=1 , main = colnames(matrixMean)[z], lineXt = 2, lineYt = 4)
                meansey(1:dim(matrixMean)[1],matrixMean[,z],matrixSD[,z],pchp=22,colp="black",bgp="black",cexp=1.5,colb="black",lgt=0.01)
                for(j in 1:dim(matrixMean)[1]){
                  abline(v = j, lty = 3)
                }    
              }
              dev.off()
              write.csv(rbind(matrixMean,matrixSD),file = paste(directGen,"/","Traités/TableauTotalitéNISTs Non normalisé.csv",sep = ""))
              
              
              
              # Sauvegarde plots et du tableau des blancs             
              
              matrixMeanBlanc <- do.call(rbind,lapply(ListBlanc, function(x){apply(x,2,mean, na.rm = T)}))
              colnames(matrixMean) = colnames(TableauMeanTotNISTs$name)
              
              matrixSDBlanc <- do.call(rbind,lapply(ListBlanc, function(x){apply(x,2,sd, na.rm = T)}))
              colnames(matrixSD) = colnames(TableauMeanTotNISTs$name)
              
              jpeg(file = paste(directGen,"/","Traités/Moyenne blanc NISTs.jpg",sep = ""),width = 1500, height =1000, quality = 100)
              par(mfrow = c(round(sqrt(dim(ListBlanc[[1]])[2])),round(sqrt(dim(ListBlanc[[1]])[2]))))
              
              for(z in 2:dim(ListBlanc[[1]])[2]){
                min = min(matrixMeanBlanc[,z], na.rm = T) - 1.10*max(matrixSDBlanc[,z], na.rm = T)
                max = max(matrixMeanBlanc[,z], na.rm = T) + 1.10*max(matrixSDBlanc[,z], na.rm = T)
                
                niceplot(limX=c(0,dim(matrixMeanBlanc)[1]+1),limY=c(min , max), marg=c(4.5,5.5,2,0.5),tick=-0.4,nlabX=dim(matrixMeanBlanc)[1]+2,nlabY = 5,  labX=c(),labY=c(),nmlabX=c(),nmlabY=c(),lasX=1,lasY=1,    lineX=-0.1, lineY=0.1,   cexX=0.9, cexY=0.9,nmX="N°NIST",nmY="Moyenne",   cexXt=1, cexYt=1 , main = colnames(matrixMeanBlanc)[z], lineXt = 2, lineYt = 4)
                meansey(1:dim(matrixMeanBlanc)[1],matrixMeanBlanc[,z],matrixSDBlanc[,z],pchp=22,colp="black",bgp="black",cexp=1.5,colb="black",lgt=0.01)
                for(j in 1:dim(matrixMeanBlanc)[1]){
                  abline(v = j, lty = 3)
                }    
              }
              dev.off()
              
              if(is.null(input$FichierEtalon)){}
              else{
                observe({
                  isolate({
                    if(input$FichierEtalon == 0){
                      output$valeur4 = renderUI({
                        h4("A charger ...")
                      })
                    }
                    if(input$FichierEtalon > 0){EtalonFile = file.choose()
                                                  output$valeur4 = renderUI({
                                                    h4(paste(paste("Votre fichier étalon est :","\n",EtalonFile)))
                                                  })
                                                  setwd(RecupererDir(EtalonFile))
                                                  TableEtalon$et = read.csv(EtalonFile, header = T, sep = ";", dec = ".")
                                                  setwd(directGen)
                                                  tableResumNist$File = matrix(NA,2,(dim(tableauMoyenne)[2]-1))
                                                  
                                                  tableResumNist$File[1,1:(dim(tableauMoyenne)[2]-1)] = apply(tableauMoyenne[1:length(Afaire),2:dim(tableauMoyenne)[2]],2, mean, na.rm = T)
                                                  tableResumNist$File[2,1:(dim(tableauMoyenne)[2]-1)] = TableEtalon$et[1,2:dim(TableEtalon$et)[2]]
                                                  tableResumNist$File = matrix(as.matrix(tableResumNist$File), nrow = 2, byrow = F)
                                                  colnames(tableResumNist$File) = colnames(tableauMoyenne)[2:dim(tableauMoyenne)[2]]
                                                  rownames(tableResumNist$File) = c("cps", "standards")
                                                  
                                                  output$boton = renderUI({
                                                    wellPanel(
                                                      actionButton("GoII", "Valider NISTs"))
                                                    
                                                  })
                                                  
                                                  observe({
                                                    if(is.null(input$GoII)){}
                                                    if(!is.null(input$GoII)){bouton$Val =input$GoII }
                                                  })
                      }
                  })
                })
              }
              
              
            }
            else{
              output$selection = renderUI({NULL})
              output$validation = renderPlot({NULL})
              output$TableMoy = renderTable({NULL})
              output$etalon = renderUI({NULL})}
          }
          
        })

      }
  })

  ### Traitement SAMPLES
  Sample = function(x){
    direct = choixdossier(x,"Samples")
    myTabs = dir(direct,pattern="\\.csv$", include.dirs = FALSE)
    return(myTabs)
  }
   
  NIST = function(x){
    direct = choixdossier(x,"NISTs")
    myTabs = dir(direct,pattern="\\.csv$", include.dirs = FALSE)
    return(myTabs)
  }
  choixdossier <- function(x,Y){
    NUM = intersect(which(str_detect(list.dirs(x),Y) == T),which(str_detect(list.dirs(x),"Traités") == F))
    NIST = list.dirs(x)[NUM]
    return(NIST)
  }
  ### verification du caractère numeric des valeurs du tableau
  checkTab <- function(X){
    
    types <- sapply(X,class)
    colProbs <- which( !is.element(types,c("integer","numeric")))
    
    X2 <- X
    
    X2[,colProbs] <- as.numeric(as.character(X2[,colProbs]))
    
    X2
    
  }#eo checkTab
  
  ##### k = dirpour les NIST
  dat = function(y,x,k){
    tableauP = read.csv(paste(k,"/",y,"/",x,sep=""),header=T, sep=";",dec=".")
    tableau = checkTab(tableauP)
    return(tableau)
  }
  ###### data pour les samples (car il faut rentrer dans le repertoire pour le charger) k = repertoire géné; x = nom de fichier csv; y = nom du dossier
  datS = function(x,y,k){
    setwd(paste(k,"\\Samples\\", y, sep = ""))
    tableauP = read.csv(paste(k,"//Samples//", y,"//",x, sep = ""),header=T, sep=";",dec=".")
    tableau = checkTab(tableauP)
    setwd(k)
    return(tableau)
  }
  elem = function(x){
    elem = colnames(x)[colnames(x)!= "Temps"]
    return(elem)
  }
  tableauBlanc = function(x,y){
    blanc = x[which(x[,1] <= y[1]),]
    return(blanc)
  }
  calculLOD = function(x,y){
    #LOD = 3* moyenne?
    element = elem(x)
    blanc = tableauBlanc(x,y)
    MeanLOD = sapply(element, function(x){res = c(mean(blanc[,x], na.rm = T),3*sd(blanc[,x], na.rm = T))
                                          return(res)})
    
    return(MeanLOD)
  }
  
  # tableauCentree pour ne garder que les valeurs du plateau
  tableauCentree = function(x,y){
    centree = x[which(x[,1] >= y[1] & x[,1] <= y[2]),]
    return(centree)
  }
  # tableauMoinsLOD pour soustraire la LOD
  tableauMoinsLOD = function(x,y) {
    
    z <- do.call(rbind,lapply(1:dim(x)[1], function(k){
      
      l <- x[k,2:dim(x)[2]]
      
      l <- l - y[1,]
      l
      
    }))
    
    return(cbind(x[,1],z))
  }
  #Tableau où sont supprimer les valeurs < LOD
  TableauSupprLOD = function(x,y) {
    
    do.call(rbind,lapply(1:dim(x)[1], function(k){
            
      l <- x[k,]
      l[l<y[2,]] <- 0
      l
      
    }))
  }
  #Tableau où sont supprimer les anomalies
  TableauDesanomalise = function(x){
    
    ValMax <- apply(x, 2, function(k){mean(k, na.rm = T) + 2*sd(k,na.rm = T)})
    
    ValMin <- apply(x, 2, function(k){mean(k, na.rm = T) - 2*sd(k,na.rm = T)})
    
    do.call(rbind,lapply(1:dim(x)[1], function(z){
      
      l <- x[z,]
      l[l < ValMin | l > ValMax] <- NA
      l
      
    }))
  }

 
  
  tableauNormalise = function(x){
    tableauNorm = x[,-1]/x[,which(colnames(x)== "Ca43")]
    tableauNorm = cbind(x[,1],tableauNorm)
    colnames(tableauNorm)[1] = "Temps"
    return(tableauNorm)
  }
  ###### ici x = vecteur de donnees, y = elements à rechercher ######
  recherche = function(x, y){
    vect = NULL
    for(i in 1: length(y)){
      vect = c(vect, which(x == y[i]))
    }
    return(vect)
  }
  ###### ici x = vecteur de donnees, y = elements à rechercher, return  = list(l'élement le plus proche, place)######
  lePlusProche = function(x,y){
    LIST = list()
    LIST[[2]]= which(abs(x-y) == min(abs(x-y), na.rm = T))
    LIST[[1]] = x[LIST[[2]]]
    if (length(LIST[[1]])!=1){LIST[[2]] = min(LIST[[2]], na.rm = T)
                              LIST[[1]] = x[LIST[[2]]]
    }
    return(LIST)
  }
  #### RecupererDir recupère le dossier dans lequel est le fichier x
  RecupererDir = function(x){
    A = strsplit(x, split = "\\", fixed  =T)
    B = A[[1]][-length(A[[1]])]
    prov= B[1]
    for(i in 2:length(B)){
      prov = paste(prov, B[i], sep = "\\")
    }
    return(prov)
  }
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  ## Calcul combien de point entre la limite haute du blanc (x) et la valeur basse du plateau (y) d'un tableau (z)
  difference <- function(x,y,z){
    LIMIT1_Basse = z[lePlusProche(x,z[,1])[[2]],1]
    LIMIT2_Basse = z[lePlusProche(y,z[,1])[[2]],1]
    tableauProvisoireDiff = z[which(z[,1] > LIMIT1_Basse & z[,1] < LIMIT2_Basse),]
    return(dim(tableauProvisoireDiff)[1])
  }
  ########## Fonctions de base (SEB) #############
  niceplot<-function(limX=c(0,14),limY=c(0,1), marg=c(4.5,4.5,2,2),
                     tick=-0.4,   nlabX=7, nlabY = 5, labX=c(),labY=c(),    nmlabX=c(),nmlabY=c(),
                     lasX=1,lasY=1,    lineX=-0.1, lineY=0.1,   cexX=0.9, cexY=0.9,
                     nmX="X",nmY="Y", cexXt=1, cexYt=1 , lineXt, lineYt, main  ) {
    
    par(mar=marg)   # margins
    plot(limX,limY,type="n",axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=limX,ylim=limY, main = main) # window
    rect(limX[1],limY[1],limX[2],limY[2])   # border
    mtext(side=1,nmX,cex=cexXt,line=lineXt,font=2) # X title  
    mtext(side=2,nmY,cex=cexYt,line=lineYt,font=2) # Y title 
    
    # labels for X axis
    if (is.na(sum(labX))==F) {
      labx<-pretty(limX,n=nlabX) ; labx<-labx[which(labx>=min(limX) & labx<=max(limX))] ; nmlabx<-labx  # default
      if (length(labX)>0) { labx<-labX ; nmlabx<-nmlabX }                                                   # customized
      axis(side=1, at=labx, labels=F, tcl=tick, pos=limY[1])  # ticks
      mtext(side=1, nmlabx, at=labx, line=lineX, cex=cexX, las=lasX) # labels
    } # end of if labels
    # labels for Y axis
    if (is.na(sum(labY))==F) {
      laby<-pretty(limY,n=nlabY) ; laby<-laby[which(laby>=min(limY) & laby<=max(limY))] ; nmlaby<-laby  # default
      if (length(labY)>0) { laby<-labY ; nmlaby<-nmlabY }                                                   # customized
      axis(side=2, at=laby, labels=F, tcl=tick, pos=limX[1]) # ticks 
      mtext(side=2, nmlaby, at=laby, line=lineY, cex=cexY, las=lasY) # labels
      
    } # end of if labels
    
  } # end of function
  ##################################################################################################################################
  meansey<-function(x,meany,sey,pchp=22,colp="black",bgp="black",cexp=1.5,colb="black",lgt=0.01) {
    lgx<-lgt*(max(x,na.rm=T)-min(x,na.rm=T)) # tick width
    segments(x,meany-sey,x,meany+sey,col=colb) # error bar
    segments(x-lgx,meany-sey,x+lgx,meany-sey,col=colb) # top tick
    segments(x-lgx,meany+sey,x+lgx,meany+sey,col=colb) # bottom tick
    points(x,meany,pch=pchp,col=colp,bg=bgp,cex=cexp) # mean
  } # end of meansey
  ##################################################################################################################################
})
