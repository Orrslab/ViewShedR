
#------- cleaning and setting preferences and loading DEM ----------------------------- 
{
  rm(list=ls()) # clean history 
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  source('functions/Viewshed.R')
  setEnv()
  # Sys.setenv(TZ = 'UTC')
}

#enter the geographical coordinates of two diagonal corners of the region
box <- data.frame(lat=c(32.38,32.68),lon=c(35.27, 35.68))
box <- data.frame(lat=c(24.548,24.73),lon=c(-83.11, -82.76))
#if you have available DEM faile, enter its path (otherwise you can automatically download)
DEM_name<-"DEM_Files/n32_e035_1arc_v3.tif" # DEM on geographic grid
DEM_name<-"DEM_Files/Tortugas_DEM_filled84_5m.tif" # DEM on geographic grid
# path for the antenna location csv file, the file must contain the variables: "ANTName","ID","LAT","LON","Towerheight"
ANTfilename <- "ANT_Table_Files/HarodANTSLoc.csv"
ANTfilename <- "ANT_Table_Files/TartugasANTSLoc.csv"

# ------ Antenna Positions ---------------
# DEM0 <- raster(DEM_name)
DEM1 <- getDEM(type="file",box,filename=DEM_name,resoluton = 30) # retrieve and cut DEM from "tif" file
# DEM2 <- crop(DEM1, alignExtent(extent(DEM0), DEM0, snap='near'), snap="near")
DEM <- DEM1
# DEM <- getDEM(type="web",box) # retrieve and cut DEM from the web
ANTS.df <- setANTS(ANTfilename,DEM)
viewSetup(ANTS.df,DEM)

# ------ Calculating viewshed for all Antennas serial version --------------------
ANTlist <-as.character(ANTS.df$ID)
ANTlist <- c("1","2","3",'4')
transAlts <-  c(0.3, 2, 5,10) # c(-2)# 
Layername <- "Res30Harod"
SerialComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F)#,seaLevel = 0)

# ------ Calculating viewshed for all Antennas parallel version--------------------
detectCores()
registerDoParallel(3)
ParallelComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F,seaLevel = 0)

# ------  substituting / adding a single layer in a LOSLayers file

listLOSFiles()
str_name_base <- "TransAlt0.3m_Res30m"
str_name_new <- "TransAlt0.3m_Res30m_Aug_unused"
str_name_out <- "TransAlt0.3m_Res30m_temp"
listAnts(str_name_base)
listAnts(str_name_new)

AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,add_str_name=str_name_new,ANTID2rm = c(7),ANTID2add = c(31,33))
AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,add_str_name=str_name_new,ANTID2add = c(50))
AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,ANTID2rm = c(2,3))

listAnts(str_name_out)

UpdateAntnames  (final_name=str_name_out,base_str_name=str_name_out,initial_ANTID=c(33),newANTID=c(18),newANTname=c("NauraRes"))
# UpdateAntnames  (final_name=str_name_out,base_str_name=str_name,initial_ANTID=c(33,48),newANTID=c(18,7),newANTname=c("N1-2","N5-15","N3-23"))
# test the layer:
listAnts(str_name_out)
source('functions/ShinyViewshed.R')


