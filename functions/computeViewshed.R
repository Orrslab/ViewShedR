
#------- cleaning and setting preferences and loading DEM ----------------------------- 
{
  rm(list=ls()) # clean history 
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  source('functions/Viewshed.R')
  setEnv()
  # Sys.setenv(TZ = 'UTC')
}

#enter the geographical coordinates of two diagonal corners of the region


##For system harod 
box <- data.frame(lat=c(32.51,32.65),lon=c(35.38, 35.53))
DEM_name<-"DEM_Files/DEM_Harod.tif" # DEM on geographic grid
ANTfilename <- "ANT_Table_Files/HarodANTSLoc2.csv"

##for Tortugas region
#box <- data.frame(lat=c(24.60520,24.63502),lon=c(-82.9047, -82.8710))
#if you have available DEM faile, enter its path (otherwise you can automatically download)
#ANTfilename <- "ANT_Table_Files/TartugasANTSLoc.csv
#DEM_name<-"DEM_Files/Tortugas_DEM_filled84_30m.tif" # DEM on geographic grid
# path for the antenna location csv file, the file must contain the variables: "ANTName","ID","LAT","LON","Towerheight"


# ------ Antenna Positions ---------------
DEM <- getDEM(type="file",box,filename=DEM_name,resoluton = 30) # retrieve and cut DEM from "tif" file
# DEM2 <- crop(DEM1, alignExtent(extent(DEM0), DEM0, snap='near'), snap="near")
#DEM <- DEM1
# DEM <- getDEM(type="web",box) # retrieve and cut DEM from the web
ANTS.df <- setANTS(ANTfilename,DEM)
viewSetup(ANTS.df,DEM)

# ------ Calculating viewshed for all Antennas serial version --------------------
ANTlist <-as.character(ANTS.df$ID)
# ANTlist <-ANTlist[24:42]
# ANTlist <- c("7","8","33")
transAlts <-  c(2, 5) 
# for marine setup:
#transAlts <-  c(1, -2) 
Layername <- "Res30Harod_2023May_B2"
#Layername <- "Res30Range05_Tartugas"
# for terestrial setups:
SerialComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F) # terrestrial
# for marine setups:
# SerialComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F,seaLevel = 0) # marine 

# ------ Calculating viewshed for all Antennas parallel version--------------------
detectCores()
registerDoParallel(4)
ParallelComputeViewShed(layername=Layername,DEM,ANTS.df,transAlts,ANTlist=ANTlist,includeCurv=F,seaLevel = 0)

# ------  substituting / adding a single layer in a LOSLayers file

listLOSFiles()
str_name_base <- "TransAlt0.3m_Res30m_Aug"
str_name_new <- "TransAlt0.3m_Res30Harod_antID7833"
str_name_out <- "TransAlt0.3m_Res30m_2022Aug"
listAnts(str_name_base)
listAnts(str_name_new)

AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,add_str_name=str_name_new,ANTID2rm = c(7,36,45,46,48),ANTID2add = c(7,8,33))
AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,add_str_name=str_name_new,ANTID2add = c(50))
AddRmAnts (final_name=str_name_out,base_str_name=str_name_base,ANTID2rm = c(2,3))

listAnts(str_name_out)

UpdateAntnames  (final_name=str_name_out,base_str_name=str_name_out,initial_ANTID=c(33),newANTID=c(18),newANTname=c("NauraRes"))
# UpdateAntnames  (final_name=str_name_out,base_str_name=str_name,initial_ANTID=c(33,48),newANTID=c(18,7),newANTname=c("N1-2","N5-15","N3-23"))
# test the layer:
listAnts(str_name_out)
source('functions/ShinyViewshed.R')


