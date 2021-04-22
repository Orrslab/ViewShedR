setEnv <- function()
{
  # install.packages("shiny")
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  pcks <- list("dplyr","lubridate","leaflet","leaflet.extras",# "windfarmGA",
               "htmltools","RSQLite","doParallel",
               "raster","tiff","sp","shiny") # a list of external packages to source
  InstallSourcePcks(pcks)
  # sapply(pcks, require, char = TRUE)        #sourcing these packages
  itm<<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
  wgs84 <<- "+proj=longlat +ellps=WGS84 +datum=WGS84"
}

InstallSourcePcks <- function(pcks){
  for( i in pcks ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
getDEM <- function(type="file",box,filename=DEM_name,resoluton=NULL)
{
# producing a DEM file in the "box" limits based on the provided data and type
  # other DEM files are available at http://sendimage.whu.edu.cn/res/DEM_share/SRTM1/N30,E30/

  # concatenate two raster files: 
  # imported_raster1 <- raster(str_name)
  # str_name<-"DEM_Files/N33E035/n33_e035_1arc_v3.tif" # DEM on geographic grid
  # imported_raster2 <- raster(str_name)
  # str_name<-"DEM_Files/from_michal/n32_e035_1arc_v3.tif.ovr" # DEM on 0:1800 grid 
  # imported_raster <- merge(imported_raster1,imported_raster2)
  # save a merged raster file locally for further use: 
  # writeRaster(imported_raster, filename="DEM_Harod.tif", format="GTiff", overwrite=TRUE)
  
  coordinates(box) <- ~lon+lat
  proj4string(box) <- CRS(wgs84)
  if (type=="file")
  {imported_raster <- raster(filename)
   cr <- crop(imported_raster, extent(box), snap="out")
   print(sprintf("the source resolution is (%3.1fm lat, %3.1fm lon)", res(cr)[1]*111e3,res(cr)[2]*94e3))
   if (!is.null(resoluton)) { 
     requiredRes=resoluton/111e3  #resolution in degrees
     cr <- projectRaster(cr,crs=CRS(wgs84),res=requiredRes)
     print(sprintf("the final resolution is (%3.1fm lat, %3.1fm lon)", res(cr)[1]*111e3,res(cr)[2]*94e3))
   }
  }
  if (type=="web")
  {
    # getData('ISO3')
    # getData("alt", country = 'ISR')
    DEM1 <- getData('SRTM', lon=box$lon[1], lat=box$lat[1])
    DEM2 <- getData('SRTM', lon=box$lon[2], lat=box$lat[2])
    cr <- merge(DEM1,DEM2)
    cr <- crop(cr, extent(box), snap="out")
    print(sprintf("the source resolution is (%3.1fm lat, %3.1fm lon)", res(cr)[1]*111e3,res(cr)[2]*94e3))
    if (!is.null(resoluton)) { 
      requiredRes=resoluton/111e3  #resolution in degrees
      cr <- projectRaster(cr,crs=CRS(wgs84),res=requiredRes)
      print(sprintf("the final resolution is (%3.1fm lat, %3.1fm lon)", res(cr)[1]*111e3,res(cr)[2]*94e3))
    }
  }
  return(cr)
}

setANTS <- function(ANTfilename,cr,visual=T)
{ANTS.df <- read.csv(ANTfilename)
  print(sprintf("Antenna file contains %i towers with variables: ",nrow(ANTS.df)))
  print(names(ANTS.df))
  print(sprintf("the file mast contain the variables \"ANTName\",\"ID\", \"LAT\", \"LON\",\"Towerheight\""))
  coordinates(ANTS.df) <- ~LON+LAT
  proj4string(ANTS.df) <- CRS(wgs84)
  ANTS <- SpatialPoints(ANTS.df, proj4string=CRS(wgs84))
  ANTS.df$GroundASL <- extract(cr,ANTS)
  return(ANTS.df)
  }

viewSetup <- function(ANTS.df,cr)
{
  pal <- colorNumeric(palette = "Spectral",domain = seq(min(cr@data@min),max(cr@data@max),length.out = 50))
  print(as.data.frame(ANTS.df))
  ll<-leaflet() %>% 
    addProviderTiles('Esri.WorldImagery') %>%
    addRasterImage(cr,color = pal,  opacity = 0.4) %>%
    addLegend(pal = pal, values = values(cr),  title = "height") %>%
    addCircles(data=ANTS.df, weight = 5, fillOpacity = 1,color = "red",
               popup = ~htmlEscape(paste0("ANTName=",ANTS.df$ANTName,
                                          ", ID=",as.character(ANTS.df$ID),
                                          ", Towerheight=",as.character(round(ANTS.df$Towerheight)),
                                          ", GroundASL=",as.character(round(ANTS.df$GroundASL)) 
               ))) # %>%
  ll
}

SerialComputeViewShed <- function(layername,cr,ANTS.df,transAltlist=2,ANTlist=NULL)
{
  if(is.null(ANTlist))
    ANTlist <-as.character(ANTS.df$ID)
  ANTS <- SpatialPoints(ANTS.df, proj4string=CRS(wgs84))
  for (transAlt in transAltlist)
  {
    FirstIter <- TRUE
    LOSLayers <- NULL
    print(sprintf("trasmittor height =%f",transAlt))
    for (ANT_ID in ANTlist)
    {
      print(ANT_ID)
      start.time <- Sys.time()
      ANT_index <- as.numeric(which(ANTS.df$ID==ANT_ID))
      res <- viewshed(r =cr, shape=box, turbine_locs = ANTS[ANT_index,],  h1=ANTS.df$Towerheight[ANT_index], h2=transAlt) #h1 is antena's hight, h2 is transmitor's hight
      if(FirstIter) LOSgrid <- res[[2]]
      LOSlogical <- res[[1]][1,]
      # collecting los in one list:
      df <- data.frame(value = LOSlogical, LON = LOSgrid[,1], LAT = LOSgrid[,2])
      s <- SpatialPixelsDataFrame(df[,c('LON', 'LAT')], data = df)
      crs(s) <- CRS(wgs84)
      r = raster(s)
      if (FirstIter)
        LOSLayers <- stack(r) #brick(r)
      else
        LOSLayers <- addLayer(LOSLayers,r)
      names(LOSLayers)[nlayers(LOSLayers)] <-   sprintf("ANT%s",ANT_ID)
      LOSLayers[[nlayers(LOSLayers)]]@title <- sprintf("ANT%s, coords: %f,%f, antAlt(Towerheight)=%.3fm, trnasAlt=%.3fm,",
                                                       ANT_ID,ANTS[ANT_index,]@coords[1],ANTS[ANT_index,]@coords[2],ANTS.df$Towerheight[ANT_index],transAlt)
      FirstIter <- FALSE
      end.time <- Sys.time()
      print(time.taken <- end.time - start.time)
    }
    
    # -----------------   Saving viewshed raster files -----------------------------
    names(LOSLayers)
    str_name <- paste0("TransAlt",as.character(transAlt),"m_", layername) #Res30m_24_33
    writeRaster(LOSLayers,paste0("LOSData/LOSLayers_",str_name)) # write the file
    write.csv(ANTS.df[which(ANTS.df$ID %in% ANTlist),],paste0("LOSData/ANTS_",str_name,".csv"))
    writeRaster(cr,paste0("LOSData/DEM_",str_name))
    print(paste("saved file",str_name))
    # # this file must be accompeneid by the DEM and ANTS file
  }
  
}

ParallelComputeViewShed <- function(layername,cr,ANTS.df,transAltlist=2,ANTlist=NULL)
{
  
  if(is.null(ANTlist))
    ANTlist <-as.character(ANTS.df$ID)
  ANTS <- SpatialPoints(ANTS.df, proj4string=CRS(wgs84))
  for (transAlt in transAltlist)
{
  print(sprintf("trasmittor height =%f",transAlt))
  start.time <- Sys.time()
  r <- raster()
  LOSLayers <- stack(r)
  LOSLayers <- foreach(i = 1:length(ANTlist), .combine=addLayer) %dopar% 
    {
      source('functions/Viewshed.R')
      setEnv()
      ANT_ID <- ANTS.df$ID[i]
      ANT_index <- as.numeric(which(ANTS.df$ID==ANT_ID))
      res <- viewshed(r =cr, shape=box, turbine_locs = ANTS[ANT_index,],  h1=ANTS.df$Towerheight[ANT_index], h2=transAlt) #h1 is antena's hight, h2 is transmitor's hight
      LOSgrid <- res[[2]]
      LOSlogical <- res[[1]][1,]
      df <- data.frame(value = LOSlogical, LON = LOSgrid[,1], LAT = LOSgrid[,2])
      s <- SpatialPixelsDataFrame(df[,c('LON', 'LAT')], data = df)
      crs(s) <- CRS(wgs84)
      raster(s)
    }
  end.time <- Sys.time()
  print(time.taken <- end.time - start.time)
  names(LOSLayers) <- sprintf("ANT%s",ANTlist)
  
  # -----------------   Saving viewshed raster files -----------------------------
  names(LOSLayers)
  str_name <- paste0("TransAlt",as.character(transAlt),"m_",layername)
  writeRaster(LOSLayers,paste0("LOSData/LOSLayers_",str_name)) # write the file
  write.csv(ANTS.df[which(ANTS.df$ID %in% ANTlist),],paste0("LOSData/ANTS_",str_name,".csv"))
  writeRaster(cr,paste0("LOSData/DEM_",str_name))
  print(paste("saved file",str_name))
  # # this file must be accompanied by the DEM and ANTS file
}
}

listLOSFiles <- function(dir="LOSData")
{gsub(".csv",'',gsub("ANTS_",'', list.files(dir,pattern = "*.csv")))}

listAnts <- function(str_name)
{
  # str_name <- paste0("TransAlt",as.character(transAlt), "m_",str_name)
  ANTS.df <- read.csv(paste0("LOSData/ANTS_",str_name,".csv"))
  print(sprintf("The CSV file %s contains the following data:",paste0("LOSData/ANTS_",str_name,".csv")))
  print(ANTS.df %>% dplyr::select(ANTName,ID,LAT,LON,Towerheight,GroundASL))
  print(sprintf("which corresponds to the following layers in the LOSLayers file:"))
  LOSLayers <- stack(paste0("LOSData/LOSLayers_",str_name,".gri")) # write the file
  print(names(LOSLayers))
}

AddRmAnts <- function (final_name,base_str_name,add_str_name=NULL,ANTID2rm=NULL,ANTID2add=NULL)
{
  rmFlag  <- !is.null(ANTID2rm)
  addFlag <- !is.null(ANTID2add)
  if (addFlag&is.null(add_str_name))
    {er <- errorCondition("Addion source file not specified (but ants to add are specified :(")
    stop(er)}
  if (!addFlag&(!is.null(add_str_name)))
  {er <- errorCondition("ANTS to add not specified (but addion source file is specified :(")
  stop(er)}
  if (!addFlag&(!rmFlag))
  {er <- errorCondition("Not sure what to do, you didn't specify ANTS :-)")
  stop(er)}
  
  base_str_name #<- paste0("TransAlt",as.character(transAlt), "m_",base_str_name)
  add_str_name #<- paste0("TransAlt",as.character(transAlt), "m_",add_str_name)
  LOSLayers <- stack(paste0("LOSData/LOSLayers_",base_str_name,".gri"))
  cr <- raster(paste0("LOSData/DEM_",base_str_name,".gri"))
  ANTS.df <- read.csv(paste0("LOSData/ANTS_",base_str_name,".csv")) %>% 
    dplyr::select(ANTName,ID,LAT,LON,Towerheight,GroundASL)
  
  if(rmFlag)
  {n_rm <- which(names(LOSLayers) %in% paste0("ANT",as.character(ANTID2rm)))
  print(sprintf("Removing %s ",names(LOSLayers)[n_rm]))
  print(sprintf("From file %s details:",base_str_name))
  print(ANTS.df[which(ANTS.df$ID %in% ANTID2rm),])
  LOSLayers <- dropLayer(LOSLayers,n_rm)
  ANTS.df <- ANTS.df[-which(ANTS.df$ID %in% ANTID2rm),]
  }
  
  if(addFlag)
  {LOSLayers_new <- stack(paste0("LOSData/LOSLayers_",add_str_name,".gri"))
  if(!all(res(LOSLayers_new)==res(LOSLayers)))
  {
    er <- errorCondition("the two LOSLayers are of different resolution")
    stop(er)
  }
  if(substr(base_str_name, start = 1, stop = 11)!=substr(add_str_name, start = 1, stop = 11))
  {
    er <- errorCondition("It seems that your source antenna file and the addition were calculated for differnt Transmitter heights")
    warning(er) 
  }
  ANTS.df_new <- read.csv(paste0("LOSData/ANTS_",add_str_name,".csv")) %>% 
    dplyr::select(ANTName,ID,LAT,LON,Towerheight,GroundASL)
    n_add <- which(names(LOSLayers_new) %in% paste0("ANT",as.character(ANTID2add)))
    print(sprintf("Adding %s ",names(LOSLayers_new)[n_add]))
    print(sprintf("From file %s with details:",add_str_name))
    print(ANTS.df_new[which(ANTS.df_new$ID %in% ANTID2add),])
    if(!rmFlag)
    {
      print(sprintf("To data from file %s details:",base_str_name))
      # print(ANTS.df)
    }  
    r <- LOSLayers_new[[n_add]]
    LOSLayers <- addLayer(LOSLayers,r)
    ANTS.df <- rbind(ANTS.df,ANTS.df_new[which(ANTS.df_new$ID %in% ANTID2add),]) 
    }

  #  reorder the layers
  LOSLayers <- LOSLayers[[order(ANTS.df$ID)]]
  ANTS.df <- ANTS.df[order(ANTS.df$ID),]
  # save and overwrite all files files using
  
  # str_name <- paste0("TransAlt",as.character(transAlt),"m_", final_name)
  print(sprintf("saving to files %s :",final_name))
  writeRaster(LOSLayers,paste0("LOSData/LOSLayers_",final_name),overwrite=T) 
  write.csv(ANTS.df,paste0("LOSData/ANTS_",final_name,".csv"))
  writeRaster(cr,paste0("LOSData/DEM_",final_name),overwrite=T) 
}

UpdateAntnames <- function (final_name,base_str_name,initial_ANTID,newANTID=NULL,newANTname=NULL)
{

  if(is.null(newANTID))
  {newANTID=initial_ANTID}
  if(is.null(newANTname))
  {
    newANTname=ANTS.df$ANTName[which(ANTS.df$ID==initial_ANTID[1]),]
    for (i in 2: lenght(initial_ANTID))
    cbind(newANTname,ANTS.df$ANTName[which(ANTS.df$ID==initial_ANTID[i]),])
  }
  if(length(initial_ANTID)!=length(newANTID))
  {
    er <- errorCondition("Antenna ID lists are of different length")
    stop(er)
  }
  if(length(initial_ANTID)!=length(newANTname))  
  {
    er <- errorCondition("Antenna ID list and Antenns name list are of different length")
    stop(er)
  }
  base_str_name #<- paste0("TransAlt",as.character(transAlt), "m_",base_str_name)
  LOSLayers <- stack(paste0("LOSData/LOSLayers_",base_str_name,".gri")) # write the file
  cr <- raster(paste0("LOSData/DEM_",base_str_name,".gri"))
  ANTS.df <- read.csv(paste0("LOSData/ANTS_",base_str_name,".csv")) %>% 
             dplyr::select(ANTName,ID,LAT,LON,Towerheight,GroundASL)
  
  
  for (i in 1: length(initial_ANTID))
  {
  n_change <- which(names(LOSLayers)==paste0("ANT",as.character(initial_ANTID[i])))

  print(sprintf("changing %s with details:",names(LOSLayers)[n_change]))
  print(ANTS.df[which(ANTS.df$ID==initial_ANTID[i]),])
  
  ANTS.df$ID[which(ANTS.df$ID==initial_ANTID[i])] <- newANTID[i]
  names(LOSLayers)[n_change] <- paste0("ANT",as.character(newANTID[i]))
  ANTS.df$ANTName[n_change] <-newANTname[i]
  
  print(sprintf("to %s with details:",names(LOSLayers)[n_change]))
  print(ANTS.df[which(ANTS.df$ID==newANTID[i]),])
  }

  #  reorder the layers
  LOSLayers <- LOSLayers[[order(ANTS.df$ID)]]
  ANTS.df <- ANTS.df[order(ANTS.df$ID),]
  # save and overwrite all files files using
  # str_name <- paste0("TransAlt",as.character(transAlt),"m_", final_name)
  print(sprintf("saving to files %s :",final_name))
  writeRaster(LOSLayers,paste0("LOSData/LOSLayers_",final_name),overwrite=T) 
  write.csv(ANTS.df,paste0("LOSData/ANTS_",final_name,".csv"))
  writeRaster(cr,paste0("LOSData/DEM_",final_name),overwrite=T) 
}




viewshed <- function (r, shape, turbine_locs, h1 = 0, h2 = 0) 
{
  print("local version")
  if (class(shape)[1] == "sf") {
    shape <- as(shape, "Spatial")
  }
  if (class(turbine_locs)[1] == "SpatialPoints") {
    turbine_locs <- sp::coordinates(turbine_locs)
  }
  mw <- methods::as(r, "SpatialPixelsDataFrame")
  mw <- methods::as(mw, "SpatialPolygons")
  sample_xy <- sp::coordinates(mw)
  rownames(sample_xy) <- NULL
  colnames(sample_xy) <- c("x1", "x2")
  reso <- min(raster::res(r))
  res <- t(apply(turbine_locs, 1, function(d) {
    viewTo(r, xy1 = d, xy2 = sample_xy, h1, h2, reso)
  }))
  return(list(Result = res, Raster_POI = sample_xy)) #, Area = sf::st_as_sf(shape), 
      #        DEM = r, Turbines = turbine_locs))
}

cansee <- function (r, xy1, xy2, h1 = 0, h2 = 0, reso) 
{
  xyz = rasterprofile(r, xy1, xy2, reso)
  np = length(xyz[, 1]) 
  h1 = xyz[, "z"][1] + h1
  h2 = xyz[, "z"][np] + h2
  hpath = h1 + (0:(np-1)) * (h2 - h1)/(np-1)
  invisible(!any(hpath < xyz[, "z"], na.rm = TRUE))
}

viewTo <- function (r, xy1, xy2, h1 = 0, h2 = 0, reso) 
{
  a <- t(apply(xy2, 1, function(d) {
    cansee(r[[1]], xy1 = xy1, xy2 = d, h1, h2, reso)
  }))
  a[is.na(a)] <- FALSE
  return(as.vector(a))
}

rasterprofile <- function (r, xy1, xy2, reso, plot = FALSE) 
{
  if (plot) {
    plot(r)
    points(x = xy2[1], y = xy2[2], col = "blue", pch = 20, 
           cex = 1.4)
    points(x = xy1[1], y = xy1[2], col = "red", pch = 20, 
           cex = 2)
  }
  dx = sqrt((xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2)
  nsteps = 1 + round(dx/reso)
  xc = xy1[1] + (0:nsteps) * (xy2[1] - xy1[1])/nsteps
  yc = xy1[2] + (0:nsteps) * (xy2[2] - xy1[2])/nsteps
  if (plot) {
    points(x = xc, y = yc, col = "red", pch = 20, cex = 1.4)
  }
  rasterVals <- raster::extract(x = r, y = cbind(xc, yc))
  pointsZ <- cbind(x = xc, y = yc, z = rasterVals)
  if (plot) {
    points(pointsZ[, "x"], pointsZ[, "y"], pch = 20, col = "black")
    text(pointsZ[, "x"], pointsZ[, "y"], pos = 1, pointsZ[, 
                                                          "z"], cex = 0.5)
  }
  if (anyNA(pointsZ)) {
    pointsZ <- pointsZ[stats::complete.cases(pointsZ), , 
                       drop = FALSE]
  }
  return(pointsZ)
}
