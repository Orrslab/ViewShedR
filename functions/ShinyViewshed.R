Rver <- R.version
if (as.numeric(Rver$major)<4)
  stop(sprintf("The package requires version 4.0.0 or later. You have %s installed, please update !!",Rver$version.string))

InstallSourcePcks <- function(pcks){
  for( i in pcks ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      print(sprintf("Package %s not installed, downloading and installing it now!",i))
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

pcks <- list("dplyr","lubridate","leaflet","leaflet.extras",# "windfarmGA",
             "htmltools","doParallel",
             "raster","tiff","sp","shiny") # a list of external packages to source
InstallSourcePcks(pcks)

wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
Point <<- data.frame(LAT= numeric(0), LON= numeric(0))

linear <- function (x, observer, target) 
{
  v <- observer - target
  y <- ((x - observer[1])/v[1])*v[2]+observer[2]
  z <- ((x - observer[1])/v[1])*v[3]+observer[3]
  
  data.frame(x=x,y=y, z=z)
}

myTransperent <- rgb(0,0,0,alpha=0,maxColorValue = 255)
mygreen <- rgb(0,255,0,alpha=200,maxColorValue = 255)

myAnd <- function(a_list,some_indeces){
  if (length(some_indeces)==0)
  {return(a_list[[1]]==a_list[[1]])
    Break}
  AndVals <- a_list[[some_indeces[1]]]
  for (i in 1:length(some_indeces))
  {
    AndVals <- a_list[[some_indeces[i]]]&AndVals
  }
  return(AndVals)
}

myNot <- function(a_list,some_indeces){
  if (length(some_indeces)==0)
  {return(a_list[[1]]==a_list[[1]])
    Break}
  AndVals <- !a_list[[some_indeces[1]]]
  for (i in 1:length(some_indeces))
  {
    AndVals <- AndVals&!a_list[[some_indeces[i]]]
  }
  return(AndVals)
}
myAnd2 <- function(LOSLayer1,LOSLayer2)  
{
  r <- stack(LOSLayer1,LOSLayer2)
  return(r[[1]]&r[[2]])
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

cpal_C <- c(grey.colors(2,start = 0.8,end=1),rainbow(4, start = 0, end =0.2,rev=T),rainbow(14, start = 0.6, end =0.8,rev=T))
val_C = as.numeric(1:20)
pal_C = colorNumeric(cpal_C,val_C, na.color = "transparent")
cpal_I <- c("transparent","yellow") #colors = pal(0:1)grey.colors(2,start = 0,end=1)
val_I = as.numeric(0:1)
pal_I = colorNumeric(cpal_I,val_I, na.color = "transparent")

LOSfilesnames <- dir("LOSData")
LOSfilesnames <- LOSfilesnames[grep("LOSLayers",LOSfilesnames)]
LOSfilesnames <- LOSfilesnames[grep(".grd",LOSfilesnames)]
LOSfilesnames <- gsub("LOSLayers_","",LOSfilesnames)
LOSfilesnames <- gsub(".grd","",LOSfilesnames)
IconsVis <- iconList(
  NoSeeIcon = makeIcon("LOSData/NoSee.png", iconWidth = 15, iconHeight = 15),
  SeeIcon = makeIcon("LOSData/See.png", iconWidth = 15, iconHeight = 15))

ui = fluidPage(
     tabsetPanel( 
       tabPanel("Load data", fluid = TRUE,
                # titlePanel("Viewshed Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, selectInput(inputId="Filename_M",label ="filename",choices = LOSfilesnames,selected = LOSfilesnames[1] )),
                      column(6, textInput(inputId="mapheight_M", label="Map vertical size  on screen",value = "700px"))
                            ),
                    actionButton("Load_M",label =  "Load file"),
                    uiOutput("ANTControls1_M")
                  ), 
                  mainPanel(
                    uiOutput("LeafletMAP_M"),
                    h4("INPUT FILE:",align = "center"),
                    h4(textOutput("txtOutput2_M")),
                    textOutput ("txtOutput_coords_M")
                  )
                )
       ),
      tabPanel("Cumulative viewshed", fluid = TRUE,
                     # titlePanel("Viewshed Analysis"),
        sidebarLayout(
          sidebarPanel(
            p(),
            h4("INPUT FILE:",align = "center"),
            strong(h4(textOutput("txtOutput2_C"),align = "center")),
            textInput(inputId="mapheight_C", label="Map vertical size  on screen",value = "700px"),
            actionButton("recalc_C",label =  "Recalc"),
            uiOutput("ANTControls1_C"),
            fluidRow(
              column(6, h2(),actionButton("Savecumul",label =  "Save to kmz")),
              column(6, textInput(inputId="cumulFilesave", label="file Name",value = "ComulativeLayer.kmz"))
            ),
            strong(h4(textOutput("savedcumul"),align = "center")),
          ), 
           mainPanel(
            uiOutput("LeafletMAP_C"),
            h4("The choosen antennas are:",align = "center"),
            textOutput("txtOutput1_C"),
            p(),
          )
        )
      ),
      tabPanel("Intersective Viewshed", fluid = TRUE,
                     sidebarLayout(
                       sidebarPanel(
                           p(),
                           h4("INPUT FILE:",align = "center"),
                           strong(h4(textOutput("txtOutput2_I"),align = "center")),
                           textInput(inputId="mapheight_I", label="Map vertical size  on screen",value = "700px"),
                           actionButton("recalc_I",label =  "Recalc"),
                           fluidRow(
                             column(7, uiOutput("ANTControls1_I")),
                             column(5, uiOutput("ANTControls2_I"))
                              ),
                           fluidRow(
                             column(6, h2(), actionButton("SaveInter",label =  "Save to kmz")),
                             column(6, textInput(inputId="InterFilesave", label="file Name",value = "IntersectiveLayer.kmz"))
                           ),
                           strong(h4(textOutput("savedinter"),align = "center")),
                           ), 
                       mainPanel(
                         uiOutput("LeafletMAP_I"),

                       )
                     )
            ),
      tabPanel("Line of sight", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   h2("Antenna",align = "center"),
                   uiOutput("ANT_sel"),
                   fluidRow(
                     column(6, uiOutput("AntLAT_choice")),
                     column(6, uiOutput("AntLON_choice"))
                      ),
                   uiOutput("AntALT_choice"),
                   h4(textOutput("txtOutput_AntASL",inline = F), align = "center"),
                   # selectInput(inputId="ANT",label ="Antenna",choices = ANTS.df$ID,selected = ANTS.df$ID[1] ),
                   h2("Transmitter",align = "center"),
                   h4("Use Map marker to choose loaction", align = "center"),
                   fluidRow(
                     # column(6, numericInput(inputId = "transLAT",label =  "transmitter LAT:",value = 32.55000  )),
                     column(6, uiOutput("transLAT_choice")),
                     column(6, uiOutput("transLON_choice"))
                     # column(6, numericInput(inputId ="transLON" ,label ="transmitter LON:",value = 35.41000 )),
                   ),
                   numericInput(inputId ="transALT" ,label ="trans ALT AGL (m):",value = 0, min =0 ),
                   actionButton("calc_LOS",label =  "Calc", align = "center"),
                   # fluidRow(
                   #   column(6, actionButton("calc_LOS",label =  "Calc", align = "center")),
                   #   column(6, actionButton("PinOnMap",label=  "Pin on Map", align = "center"))
                   # ),
                   h4(textOutput("txtOutput_coords",inline = F), align = "center"),
                   h4(textOutput("txtOutput_TRNSASL",inline = F), align = "center"),
                   numericInput(inputId ="DEMshoulders" ,label ="DEM shoulders (m):",value = 0, min =0 ),
                   h3("Maps height on screen", align = "center"),
                   # textInput(inputId="mapheight_1", label="Map1 height on screen",value = "500px"),
                   textInput(inputId="mapheight_2", label="Map2 height on screen",value = "300px"),
                   textInput(inputId="mapheight_3", label="Map3 height on screen",value = "700px"),
                 ),
                 # tableOutput('table')),
                 mainPanel(
                   uiOutput("LeafletMAP_1"),
                   uiOutput("LOSplot"),
                   uiOutput("LeafletMAP_2")

                 )
               )
      )
          )
        ) 
server <- function(input, output, session) {
  
#  Load data
    observeEvent(input$Load_M, {
      input$Load_M
      isolate({
        ANTS.df <<- read.csv(paste0("LOSData/ANTS_",input$Filename_M,".csv"))
        LOSLayers <<- stack(paste0("LOSData/LOSLayers_",input$Filename_M,".gri")) 
        DEM <<- raster(paste0("LOSData/DEM_",input$Filename_M,".gri"))
        antID <- ANTS.df$ID    
          output$ANTControls1_M <- renderUI({
            checkboxGroupInput("antID_M", "Antennas",choiceValues=ANTS.df$ID,choiceNames = paste (ANTS.df$ID,"-",ANTS.df$ANTName),inline = F)})
        output$ANT_sel <- renderUI({selectInput(inputId="ANT",label ="Antenna",
                                                choices = setNames(as.list(ANTS.df$ID), paste0(ANTS.df$ID,", ",ANTS.df$ANTName)),
                                                selected = ANTS.df$ID[1], )})
        pal <- colorNumeric(palette = "Spectral",domain = seq(min(DEM@data@min),max(DEM@data@max),length.out = 50),reverse = T)
        output$mymap_M      <- renderLeaflet({
            leaflet() %>%
              addProviderTiles('Esri.WorldImagery') %>%
              addRasterImage(DEM,color = pal,  opacity = 0.8) %>%
              addLegend(pal = pal, values = values(DEM),  title = "height") %>%
              addCircles(data=ANTS.df[,], weight = 5, fillOpacity = 1,color = "red",
                         popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID),
                                                    ",",ANTS.df$ANTName,
                                                    ", Ground elevation=",ANTS.df$GroundASL,
                                                    "m, Tower height=",ANTS.df$Towerheight,
                                                    "m, (",ANTS.df$LAT,",",ANTS.df$LON,")")))%>%
              addMarkers(data=ANTS.df[ANT_indeces_M(),], icon = ~ IconsVis[[2]],
                         popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID[ANT_indeces_M()]),
                                                    ",",ANTS.df$ANTName[ANT_indeces_M()],
                                                    ", Ground elevation=",ANTS.df$GroundASL[ANT_indeces_M()],
                                                    "m, Tower height=",ANTS.df$Towerheight[ANT_indeces_M()],
                                                    "m, (",ANTS.df$LAT[ANT_indeces_M()],",",ANTS.df$LON[ANT_indeces_M()],")"
                                                    )))%>%
              addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = F))
          })
        # output$LeafletMAP_1 <- renderUI({leafletOutput("mymapL",height =input$mapheight_1 )})
        output$LeafletMAP_1 <- renderUI({leafletOutput("mymapL",height ="500px" )})
        output$mymapL <- renderLeaflet(
          leaflet() %>%
            addProviderTiles('Esri.WorldImagery') %>%
            addRasterImage(DEM,color = pal, opacity = 0.3) %>%
            addLegend(pal = pal, values = values(DEM),  title = "height") %>%
            # setView(lng = 35.43, lat = 32.54, zoom = 12) %>%
            addCircles(data=ANTS.df, weight = 5, fillOpacity = 1,color = "red",
                       popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID),",",ANTS.df$ANTName)))%>%
            addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = F)) %>% 
            addDrawToolbar(polylineOptions = F,polygonOptions = F,circleOptions = F,
                           rectangleOptions = F,circleMarkerOptions = F,
                           singleFeature = T, targetGroup='draw', editOptions = F)
          # editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
          # addLayersControl(overlayGroups = c('draw'), options =
          #                    layersControlOptions(collapsed=FALSE))
        )
        })}) 
  
    output$LeafletMAP_M <- renderUI({leafletOutput("mymap_M",height =input$mapheight_M )})
    output$txtOutput2_M <- renderText({      paste0( input$Filename_M)    })
    ANT_indeces_M       <- reactive({which(ANTS.df$ID %in% input$antID_M)}) 
    observeEvent(input$mymap_draw_new_feature,{
      feature <- input$mymap_draw_new_feature
      output$txtOutput_coords_M <- renderText({      paste("lat=",feature$geometry$coordinates[[2]],"lon=",feature$geometry$coordinates[[1]])    })
    })

# Cumulative tab
    observeEvent(input$Load_M, {
      input$Load_M
      isolate({
        output$ANTControls1_C <- renderUI({checkboxGroupInput("antID_C", "Choose viewing Antennas",choiceValues=ANTS.df$ID,choiceNames = paste (ANTS.df$ID,"-",ANTS.df$ANTName),inline = F)})
        output$txtOutput1_C   <-  renderText({  paste0(ANTS.df$ANTName[which(ANTS.df$ID %in% input$antID_C)],",") })
       })})
    
    # output$ANTControls1_C <- renderUI({checkboxGroupInput("antID_C", "Choose viewing Antennas",choiceValues=ANTS.df$ID,choiceNames = paste (ANTS.df$ID,"-",ANTS.df$ANTName),inline = F)})
    output$LeafletMAP_C   <- renderUI({leafletOutput("mymap_C",height =input$mapheight_C )})
    output$txtOutput1_C   <-  renderText({  paste0(ANTS.df$ANTName[which(ANTS.df$ID %in% input$antID_C)],",") })
    
    observeEvent(input$Load_M, {
      input$Load_M
      isolate({output$txtOutput2_C   <-  renderText({      paste0( input$Filename_M)    })})})
   
    ANT_indeces_C <- reactive({which(ANTS.df$ID %in% input$antID_C)}) 
    observeEvent(input$recalc_C, {
      output$mymap_C <- renderLeaflet({
        input$recalc_C
        isolate({
          leaflet() %>%
            addProviderTiles('Esri.WorldImagery') %>%
            addRasterImage(sum(LOSLayers[[ANT_indeces_C()]]), colors = pal_C(0:length(ANT_indeces_C())), opacity = 0.5)%>%
            addLegend(colors = pal_C(0:length(ANT_indeces_C())),labels =(0:length(ANT_indeces_C())), title = "Number of Antenas")%>%
            addMarkers(data=ANTS.df[ANT_indeces_C(),], icon = ~ IconsVis[[2]],
                       popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID[ANT_indeces_C()]),",",ANTS.df$ANTName[ANT_indeces_C()])))%>%
            addCircles(data=ANTS.df[ANT_indeces_C(),], weight = 5, fillOpacity = 1,color = "red",
                       popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID[ANT_indeces_C()]),",",ANTS.df$ANTName[ANT_indeces_C()])))%>%
            addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = F))
          
        })})
    })
    observeEvent(input$Savecumul, {
        input$Savecumul
        isolate({
        KML(sum(LOSLayers[[ANT_indeces_C()]]),col=pal_C(0:length(ANT_indeces_C())),input$cumulFilesave,overwrite=T)
        output$savedcumul <- renderText({  paste("Saved file", input$cumulFilesave ) })
         })})

 
# Intersective tab   
    observeEvent(input$Load_M, {
      input$Load_M
      isolate({
        output$ANTControls1_I <- renderUI({checkboxGroupInput("antID1_I", "clear Antennas",choiceValues=ANTS.df$ID,choiceNames = paste (ANTS.df$ID,"-",ANTS.df$ANTName),inline = F)})
        output$ANTControls2_I <- renderUI({checkboxGroupInput("antID2_I", "blind Antennas",choices =ANTS.df$ID,inline = F)})
        output$txtOutput2_I <-  renderText({  paste0( input$Filename_M) })
        })})
    
    output$LeafletMAP_I   <- renderUI({leafletOutput("mymap_I",height =input$mapheight_I )})
    # output$txtOutput1_I   <-  renderText({paste0(ANTS.df$ANTName[which(ANTS.df$ID %in% input$antID_I)],",") })
    

    ANT_indeces1_I <- reactive({which(ANTS.df$ID %in% input$antID1_I)}) 
    ANT_indeces2_I <- reactive({which(ANTS.df$ID %in% input$antID2_I)}) 
    
    observeEvent(input$recalc_I, {
      
      output$mymap_I <- renderLeaflet({
        input$recalc_I
        isolate({
          leaflet() %>%
            addProviderTiles('Esri.WorldImagery') %>%
            addRasterImage(myAnd2(myNot(LOSLayers,ANT_indeces2_I()),myAnd(LOSLayers,ANT_indeces1_I())),colors = cpal_I,opacity = 0.5)%>%
            addLegend(colors = cpal_I,labels = 0:1, title = "Yes / No")%>%
            addMarkers(data=ANTS.df[ANT_indeces1_I(),], icon = ~ IconsVis[[2]],
                       popup = ~htmlEscape(paste0("can see: ID=",as.character(ANTS.df$ID[ANT_indeces1_I()]),",",ANTS.df$ANTName[ANT_indeces1_I()])))%>%
            addMarkers(data=ANTS.df[ANT_indeces2_I(),], icon = ~ IconsVis[[1]],
                       popup = ~htmlEscape(paste0("cannot see: ID=",as.character(ANTS.df$ID[ANT_indeces2_I()]),",",ANTS.df$ANTName[ANT_indeces2_I()])))%>%
            addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = F))
        })})
    }) 
    observeEvent(input$SaveInter, {
      input$SaveInter
      isolate({
        KML(myAnd2(myNot(LOSLayers,ANT_indeces2_I()),myAnd(LOSLayers,ANT_indeces1_I())),col=c(myTransperent,mygreen),input$InterFilesave,overwrite=T)
      output$savedinter <- renderText({  paste("Saved file", input$InterFilesave ) })
      
      })})
    
  
# Line of sight

    observeEvent(input$mymapL_draw_new_feature,{
      feature <- input$mymapL_draw_new_feature

      # Point <<-rbind(Point,do.call(cbind,feature$geometry$coordinates))
      # Point <<-rbind(Point,c(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]]))
      # names(Point) <- c("LAT","LON")
      # output$table <- renderTable(Point,digits = 5 )
      output$txtOutput_coords <- renderText({      paste("lat=",feature$geometry$coordinates[[2]],"lon=",feature$geometry$coordinates[[1]])    })
      output$transLAT_choice <- renderUI({numericInput(inputId = "transLAT",label =  "transmitter LAT:",value = feature$geometry$coordinates[[2]]  )})
      output$transLON_choice <- renderUI({numericInput(inputId = "transLON",label =  "transmitter LON:",value = feature$geometry$coordinates[[1]]  )})
    }) 
      
      observeEvent(input$ANT, {
        output$AntLAT_choice <- renderUI({numericInput(inputId = "AntLAT",label =  "Antenna LAT:",value = ANTS.df$LAT[which(ANTS.df$ID==input$ANT)]  )})
        output$AntLON_choice <- renderUI({numericInput(inputId = "AntLON",label =  "Antenna LON:",value = ANTS.df$LON[which(ANTS.df$ID==input$ANT)]  )})
        output$AntALT_choice <- renderUI({numericInput(inputId = "AntAGL",label =  "Antenna ALT AGL (m):",value = ANTS.df$Towerheight[which(ANTS.df$ID==input$ANT)]  )})
      })
    
    output$transLAT_choice <- renderUI({numericInput(inputId = "transLAT",label =  "transmitter LAT:",value = 32.50  )})
    output$transLON_choice <- renderUI({numericInput(inputId = "transLON",label =  "transmitter LON:",value = 35.50  )})
    
    observeEvent(input$calc_LOS, {
      input$calc_LOS
      isolate(
        {
          i <- which(ANTS.df$ID==input$ANT)  
          LOSline <- matrix(data = c(input$AntLON,input$transLON+0.0000001,input$AntLAT,input$transLAT),ncol=2)
          SPline <- spLines(LOSline,crs=crs(DEM))
          LineDEM <- extract(DEM,SPline)
          LineDEM <- unlist(LineDEM)
          if(LOSline[3]<LOSline[4]) {LineDEM <- rev(LineDEM)}
          addHight <- c(input$AntALT,input$transALT)
          observer <<- c(LOSline[1,],LineDEM[1]+input$AntAGL)
          target   <<- c(LOSline[2,],LineDEM[length(LineDEM)]+input$transALT)
          x <- seq(from=observer[1], to=target[1], along.with=LineDEM)
          xyzline <- linear(x, observer, target)
          names(xyzline) <- c("LON","LAT","z")
          xyzline$DEM <- LineDEM
          # hightProfile <<- xyzline
          vis=ifelse(any(xyzline$z < xyzline$DEM, na.rm = TRUE),"red","green")
          DEMbox <- data.frame(LON=c(min(LOSline[,1]-input$DEMshoulders/111000),max(LOSline[,1]+input$DEMshoulders/111000)),
                               LAT=c(min(LOSline[,2]-input$DEMshoulders/111000),max(LOSline[,2]+input$DEMshoulders/111000)))
          coordinates(DEMbox) <- ~LON+LAT
          proj4string(DEMbox) <- CRS(wgs84)
          DEM_tight <- crop(DEM, extent(DEMbox), snap="out")
          pal <- colorNumeric(
            palette = "Spectral",reverse = T,
            domain = seq(min(DEM_tight@data@values),max(DEM_tight@data@values),length.out = 50)
          )
          
          output$txtOutput_TRNSASL <- renderText({paste("Transmitter ground height is",LineDEM[length(LineDEM)],"m")    })
          output$txtOutput_AntASL <- renderText({paste("Antenna ground height is",LineDEM[1],"m")    })
          
          output$LOSplot <- renderUI({plotOutput("plot1",height =input$mapheight_2 )})
          output$plot1 <- renderPlot({
            plot(x,LineDEM,type="l", col="black",lwd=4,
                 ylim=c(min(LineDEM),max(c(LineDEM,target[3],observer[3]))),
                 xlab = "Longtitude",ylab = "Altitude (m)",cex.lab=1.5, cex.axis=1.5,)
            points(c(observer[1],target[1]),c(observer[3],target[3]),type="l",col=vis,lwd = 4)
            text(observer[1],(target[3]+observer[3])/2,"ANT")
            text(target[1],(target[3]+observer[3])/2,"TRANS") 
            legend("bottomleft", legend = c("ground altitude", "direct line"), lty=1, lwd = 4,
                   col = c("black",vis), text.col = "black", horiz = F, inset = c(0.1, 0.1))
          })
          output$LeafletMAP_2 <- renderUI({leafletOutput("mymapDEM",height =input$mapheight_3 )})
          output$mymapDEM <- renderLeaflet(
            leaflet() %>%
              addProviderTiles('Esri.WorldImagery') %>%
              addRasterImage(DEM_tight,colors = pal, opacity = 0.8) %>%
              # setView(lng = 35.43, lat = 32.54, zoom = 12) %>%
              addLegend(pal = pal, values = DEM_tight@data@values,  title = "height") %>% 
              addCircles(data=ANTS.df, weight = 5, fillOpacity = 1,color = "red",
                         popup = ~htmlEscape(paste0("ID=",as.character(ANTS.df$ID),",",ANTS.df$ANTName)))%>%
              addCircles(data=xyzline, weight = 3, fillOpacity = 1,color = "green",
                         popup = ~htmlEscape(paste0("DEM=",as.character(round(xyzline$DEM,1)),
                                                    ", LOSline=",as.character(round(xyzline$z,1)),
                                                    ", LAT=",as.character(round(xyzline$LAT,3)),
                                                    ", LON=",as.character(round(xyzline$LON,3)))))%>%
              addScaleBar( position =  "bottomleft", options = scaleBarOptions(maxWidth = 250, metric = TRUE,imperial = F))
          )
        }) })
       
  }
  
  
  runApp(shinyApp(ui, server),launch.browser = TRUE)