setwd("D:/Proyectos_GitHub/PISCO_Peligro/data/rasters")

library(raster)
library(rgdal)

capacidad=raster('capacidad99.tif')
dem=raster('dem.tif')
peli=raster('Peligro2001.tif')
pendiente=raster('slope.tif')
suelo=raster('soil_type.tif')


DF=as.data.frame(dem,xy=T)
DF_DEM=na.omit(DF)

n=33000

set.seed(123)
gridded(DF_DEM) = ~x+y
image(DF_DEM)
puntos_de_muestreo=spsample(DF_DEM,n,type="random")
points(puntos_de_muestreo, pch=3, cex=.5)


dem_value=extract(dem,puntos_de_muestreo)
slope_value=round(extract(pendiente,puntos_de_muestreo),2)
soil_value=extract(suelo,puntos_de_muestreo)
capacity_value=extract(capacidad,puntos_de_muestreo)
hazard_value=extract(peli,puntos_de_muestreo)

data=data.frame(dem_value,soil_value,slope_value,capacity_value,hazard_value)

range_capacity=function(x){
  if (x==1){
    return(round(runif(1, 2.5, 3.0),2))
  }else if(x==2){
    return(2.0)
  }else if(x==3){
    return(1.0)
  }else if(x==4){
    return(round(runif(1, 2.0, 2.5),2))
  }else{
    NA
  }
}
data=na.omit(data)
data$capacity_value=as.numeric(lapply(data[,4],range_capacity))


write.table(data,"D:/Proyectos_GitHub/PISCO_Peligro/data/CSV/data.csv",row.names = F,sep = ',')




