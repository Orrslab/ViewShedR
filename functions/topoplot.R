
#------- cleaning and setting preferences and loading DEM ----------------------------- 
{
  rm(list=ls()) # clean history 
  options(digits = 14) # Makes sure long numbers are not abbreviated.
  source('functions/Viewshed_.R')
  setEnv()
  # Sys.setenv(TZ = 'UTC')
}

#enter the geographical coordinates of two diagonal corners of the region
box <- data.frame(lat=c(32.44,32.62),lon=c(35.32, 35.55))
box <- data.frame(lat=c(24.54,24.78),lon=c(-83.20, -82.70))

#if you have available DEM faile, enter its path (otherwise you can automatically download)
DEM_name<-"DEM_Files/n32_e035_1arc_v3.tif" # DEM on geographic grid
DEM_name<-"DEM_Files/Tortugas_DEM_filled.tif" # DEM on geographic grid

# path for the antenna location csv file, the file must contain the variables: "ANTName","ID","LAT","LON","Towerheight"
ANTfilename <- "ANT_Table_Files/HarodANTSLoc.csv"
ANTfilename <- "ANT_Table_Files/TartugasANTSLoc.csv"
# ------ Antenna Positions ---------------
DEM <- getDEM(type="file",box,filename=DEM_name,resoluton = 300) # retrieve and cut DEM from "tif" file
# DEM <- getDEM(type="web",box) # retrieve and cut DEM from the web
ANTS.df <- setANTS(ANTfilename,DEM)
viewSetup(ANTS.df,DEM)

# ------ Calculating viewshed for all Antennas serial version --------------------

library(plotly)

elev.new <- t(as.matrix(DEM)) # use point extent
x <- seq(DEM@extent@xmin,DEM@extent@xmax,length.out = DEM@ncols)
y <- seq(DEM@extent@ymin,DEM@extent@ymax,length.out = DEM@nrows)

# volcano is a numeric matrix that ships with R


fig <- plot_ly(x = ~y, y = ~x, z = ~elev.new)%>%
  add_surface( contours = list( z = list(
                                show=TRUE,
                                usecolormap=TRUE,
                                highlightcolor="#ff0000",
                                project=list(z=TRUE))) ) 
# %>% 
#   layout(yaxis = list(autorange = "reversed"))
# 
# layout(xaxis = list(autorange = "reversed"))
# %>% 
  
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
xlab <- list(
  title = "x Axis",
  titlefont = f
)
ylab <- list(
  title = "y Axis",
  titlefont = f
)

fig <- plot_ly(x = ~x, y = ~rev(y), z = ~as.matrix(DEM), type = "contour", 
               contours = list(start = -200 ,end = 500,size = 50,showlabels = TRUE))

fig <- fig %>% colorbar(title = "Elevation \n in meters") %>% 
               layout(xaxis = xlab, yaxis = ylab)
fig
