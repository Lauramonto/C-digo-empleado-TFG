library(sp)
library(sf)
library(spatstat)
library(maptools)
library(raster)
library(ggplot2)

setwd("G:/Mi unidad/Investigacion/CompositionalAccidents/")

covariables=openxlsx::read.xlsx("Data/CovariablesAreasVulnerables.xlsx")
colnames(covariables)[1]="CODDISTSEC"
covariables2=openxlsx::read.xlsx("Data/CovariablesAreasVulnerables2.xlsx")
colnames(covariables2)[1]="CODDISTSEC"
covariables=rbind(covariables,covariables2)

#secciones=rgdal::readOGR("Data/secciones.shp")
secciones = st_read("Data/secciones.shp") #yo
secciones$CODDISTSEC=as.numeric(secciones$CODDISTSEC)

load("Data/X_2014_2017.rda")
lines_psp=as.psp(X$domain)
plot(lines_psp)
#lines=as.SpatialLines.psp(lines_psp)
lines=st_as_sf(lines_psp) #yo

mids_lines=midpoints.psp(lines_psp)
mids_coords=data.frame(x=mids_lines$x,y=mids_lines$y,Id=1:length(mids_lines$x))
coordinates(mids_coords)=~x+y

# Intersect
secciones_sf <- st_as_sf(secciones) # yo
mids_coords_sf <- st_as_sf(mids_coords) #yo
int_coords_secc <- st_intersection(secciones_sf, mids_coords_sf) #yo
#int_coords_secc=raster::intersect(secciones,mids_coords)
head(int_coords_secc)

# Covariates per segment
#covariables_segs=merge(covariables,int_coords_secc@data)
#covariables_segs=covariables_segs[order(covariables_segs$Id),]


int_coords_secc_data <- st_drop_geometry(int_coords_secc) #yo
covariables_segs <- merge(covariables, int_coords_secc_data, by ="CODDISTSEC") #yo
head(covariables) #yo
head(int_coords_secc_data) #yo

# Map check

load("Data/BD.rda")
lines_df=as.data.frame(lines_psp)
lines_df=cbind(lines_df,covariables_segs,ACC=BD$TOTAL_ACCIDENTES)

ggplot(lines_df) +
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1, color = Turismes.de.16.CV.i.més), size = 1) +
  theme_void() +
  coord_fixed() 

ggplot(lines_df) +
  geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1, color = ACC), size = 1) +
  theme_void() +
  coord_fixed() 

# Save
nrow(covariables_segs)
#save(covariables_segs,file="Data/covariables_segs.rda")
View(covariables_segs)
View(lines_df)

    