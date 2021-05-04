
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
#if you have available DEM faile, enter its path (otherwise you can automatically download)
DEM_name<-"DEM_Files/DEM_Harod.tif" # DEM on geographic grid
# path for the antenna location csv file, the file must contain the variables: "ANTName","ID","LAT","LON","Towerheight"
ANTfilename <- "ANT_Table_Files/HarodANTSLoc.csv"

# ------ Antenna Positions ---------------
DEM <- getDEM(type="file",box,filename=DEM_name) # retrieve and cut DEM from "tif" file
# DEM <- getDEM(type="web",box) # retrieve and cut DEM from the web
ANTS.df <- setANTS(ANTfilename,DEM)
viewSetup(ANTS.df,DEM)

# ------ Calculating viewshed for all Antennas serial version --------------------
# ANT_ID <- "23"
ANTlist <-as.character(ANTS.df$ID)
ANTlist <- c("1")#,"2","3","4","5","6")
transAlts <- c(0.3, 2, 5,10)
Layername <- "Res900a1-6"
SerialComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist)

# ------ Calculating viewshed for all Antennas parallel version--------------------
detectCores()
registerDoParallel(3)
ParallelComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist)

# ------  substituting / adding a single layer in a LOSLayers file

listLOSFiles()
str_name <- "TransAlt0.3m_Res900a1-6"
str_name_new <- "TransAlt0.3m_Res900a11-16"
str_name_out <- "TransAlt0.3m_Res900new-a11-16"
listAnts(str_name)
listAnts(str_name_new)

AddRmAnts (final_name=str_name_out,base_str_name=str_name,add_str_name=str_name_new,ANTID2rm = c(2,3),ANTID2add = c(15,12))
AddRmAnts (final_name=str_name_out,base_str_name=str_name,add_str_name=str_name_new,ANTID2add = c(15,12))
AddRmAnts (final_name=str_name_out,base_str_name=str_name,ANTID2rm = c(2,3))
 
UpdateAntnames  (final_name=str_name_out,base_str_name=str_name,initial_ANTID=c(1,5,3),newANTID=c(12,15,23),newANTname=c("N1-2","N5-15","N3-23"))
# test the layer:
listAnts(str_name_out)
source('functions/ShinyViewshed.R')


