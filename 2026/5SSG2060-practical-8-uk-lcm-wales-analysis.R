#R version of Principles of SDS practical, Section 8 (Raster Manipulation)
#James D.A. Millington, March 2026

#load packages
library(terra)  #to handle raster data, see https://rspatial.github.io/terra/index.html
library(sf)  #to handle vector data, see https://r-spatial.github.io/sf/

library(dplyr)   #for data manip in Section 4, https://dplyr.tidyverse.org/ 
library(tidyr)   #for data manip in Section 4, https://tidyr.tidyverse.org/
library(ggplot2) #for plotting in Section 4, https://ggplot2.tidyverse.org/

#####
#Section 1
lcm1990 = rast("data/1990/data/gb1990lcm1km_dominant.tif")
lcm2000 = rast("data/2000/data/LCM2000_GB_1K_DOM_TAR.tif")
lcm2015 = rast("data/2015/data/lcm2015_gb_1km_dominant_target_class.img")

#equivalent of rasterio meta
describe(lcm1990)
describe(lcm2000)
describe(lcm2015)

plot(lcm1990)
plot(lcm2000)
plot(lcm2015)

levels(lcm1990)
levels(lcm2000)  #classified
levels(lcm2015)

levels(lcm2000) <- NULL  #unclassiffy

freq(lcm1990)  #0 is nodata
freq(lcm2000)  #1 is nodata
freq(lcm2015)  #0 is nodata

#set NA values
lcm1990 <- classify(lcm1990, cbind(0, NA))
lcm2000 <- classify(lcm2000, cbind(1, NA))
lcm2015 <- classify(lcm2015, cbind(0, NA))

#check CRS
compareGeom(lcm1990, lcm2000)
#compareGeom(lcm1990, lcm2015)  #throws an error 

#re-project 2015
lcm2015 <- project(lcm2015, lcm1990, method = "near")
compareGeom(lcm1990, lcm2015)  #should now return TRUE

#write to file
writeRaster(lcm1990, "data/lcm1990-BNG_r.tif", overwrite=T)
writeRaster(lcm2000, "data/lcm2000-BNG_r.tif", overwrite=T)
writeRaster(lcm2015, "data/lcm2015-BNG_r.tif", overwrite=T)

lcm1990_new = rast("data/lcm1990-BNG_r.tif")
lcm2000_new = rast("data/lcm2000-BNG_r.tif")
lcm2015_new = rast("data/lcm2015-BNG_r.tif")

#####
#Section 2.1

#change this to use sf

#Read and check the vector outline of GB
lads = st_read("data/Countries_(December_2022)_GB_BUC.geojson")
head(lads)
crs(lads)

#re-project the vector outline of GB
lads_reproj <- st_transform(lads, "epsg:27700")
crs(lads_reproj)  #check

#filter for  Wales and write to file (using base r, not dplyr/)
lads_reproj_wales <- lads_reproj[lads_reproj$CTRY22NM == "Wales",]

st_write(lads_reproj_wales, dsn="data/WalesBoundary_r.geojson", delete_dsn=TRUE)

#####
#Section2.2

#use snap="out" to get identical extent dims as python, use touches=F to get identical data cells
lcm2015_wales <- crop(lcm2015_new, lads_reproj_wales, 
                        mask=T, snap="out", touches=F)
dim(lcm2015_wales)

#TASK
lcm1990_wales <- crop(lcm1990_new, lads_reproj_wales, 
                        mask=T, snap="out", touches=F)
dim(lcm1990_wales)


lcm2000_wales <- crop(lcm2000_new, lads_reproj_wales, 
                        mask=T, snap="out", touches=F)
dim(lcm2000_wales)

#for 2000 we have an extra step in r here to handle the different NA values
#this is the same as the classification below, doing now means output tif works better
lcm2000_wales <- classify(lcm2000_wales, cbind(1, NA))

plot(lcm2015_wales, main="2015")

##TASK
plot(lcm2000_wales, main="2000")
plot(lcm1990_wales, main="1990")

#####
#Section 2.3

writeRaster(lcm2015_wales, "data/lcm2015_wales_r.tif", overwrite=T)
writeRaster(lcm1990_wales, "data/lcm1990_wales_r.tif", overwrite=T)
writeRaster(lcm2000_wales, "data/lcm2000_wales_r.tif", overwrite=T)


#####
#Section 3.1

lcm1990_wales = rast("data/lcm1990_wales_r.tif")
counts1990 <- freq(lcm1990_wales, bylayer=F)
counts1990

#TASK
lcm2000_wales = rast("data/lcm2000_wales_r.tif")
counts2000 <- freq(lcm2000_wales, bylayer=F)
counts2000


lcm2015_wales = rast("data/lcm2015_wales_r.tif")
counts2015 <- freq(lcm2015_wales, bylayer=F)
counts2015

#####
#Section 3.3

#create reclassification table (this is not a dictionary in R)
new_value_21 <- c(1,1,2,3,3,3,3,3,3,3,3,6,6,4,6,3,6,3,3,5,5)
rcl21 <- cbind(seq(1,21,1), new_value_21)
rcl21

#use the classify function to reclassify
lcm2015_wales6 <- classify(x=lcm2015_wales, rcl=rcl21)
lcm1990_wales6 <- classify(x=lcm1990_wales, rcl=rcl21)

#TASK
counts2015_6 <- freq(lcm2015_wales6)
counts1990_6 <- freq(lcm1990_wales6)

counts2015_6
counts1990_6

plot(lcm2015_wales6, main="2015")
plot(lcm1990_wales6, main="1990")


#reclassification for 2000
new_value_25 <- c(4,4,3,3,3,6,3,3,3,3,6,1,1,3,3,3,3,3,3,3,2,2,2,5,5)
rcl25 <- cbind(seq(1,25,1), new_value_25)
rcl25

lcm2000_wales6 <- classify(x=lcm2000_wales, rcl=rcl25)

counts2000_6 <- freq(lcm2000_wales6)
counts2000_6

plot(lcm2000_wales6, main="2000")


#####
#Section 3.4

writeRaster(lcm2015_wales6, "data/lcm2015_wales6_out_r.tif", overwrite=T)
writeRaster(lcm1990_wales6, "data/lcm1990_wales6_out_r.tif", overwrite=T)
writeRaster(lcm2000_wales6, "data/lcm2000_wales6_out_r.tif", overwrite=T)


#####
#Section 4


counts_df <- tibble("1990"=counts1990_6$count, 
                    "2015"=counts2015_6$count)
counts_df

lcs = c('Woodland', 'Cropland', 'Grassland', 'Water', 'Built-up', 'Other')

counts_df <- mutate(counts_df, "Land Cover"=lcs)
counts_df

#for ggplot we should pivot our table from wide to long
df_long <- counts_df |>
  pivot_longer(
    cols = where(is.double),   #columns to pivot
    names_to = "Year",         #column name for names
    values_to = "counts"       #column name for values
  )

df_long

df_long |>
  ggplot(aes(x = `Land Cover`, y = counts, fill=Year)) +
  geom_col(position="dodge")


#to calculate and plot differences we go back to wide
#first pivot
df_wide <- df_long |>
  pivot_wider(
    names_from = Year,
    values_from = counts
  )

#calculate differences
df_diff <- df_wide |>
  mutate(diff = `2015` - `1990`)
df_diff

#plot
df_diff |>
  ggplot(aes(x = `Land Cover`, y = diff)) +
  geom_col(fill='darkblue') +
  labs(x = "Land Cover", y = "Land Area (sq km)") +
  ggtitle("Wales Land Cover Change, 1990-2015")


#####
#Section 5

#now define the classes represented in the map
cls <- data.frame(id=1:6, 
                  cover=c("Woodland", "Cropland", "Grassland", "Water", "Built-up", "Other"),
                  cols=c('darkgreen', 'lightyellow', 'lightgreen', 'blue', 'black','grey' )
                  )

#set the raster to be categorical - terra handles categorical rasters better than numpy
levels(lcm2015_wales6) <- cls[,c('id','cover')]
#we could have done this earlier, but the above matches python more closely
#see https://rspatial.github.io/terra/reference/factors.html

#set the colours for each class
coltab(lcm2015_wales6) <- cls[,c('id','cols')]

#and plot 
plot(lcm2015_wales6, main="2015")


#1990
levels(lcm1990_wales6) <- cls[,c('id','cover')]
coltab(lcm1990_wales6) <- cls[,c('id','cols')]

#to plot to maps side-by-side
par(mfrow=c(1,2))  #set the graphics parameters to 1 row, 2 cols
plot(lcm1990_wales6, main="1990")
plot(lcm2015_wales6, main="2015")
par(mfrow=c(1,1))  #reset (remember to do this!)


#set zoom area  (in projection units)
e <- ext(250000, 300000, 250000, 300000)

par(mfrow=c(1,2))  #set the graphics parameters to 1 row, 2 cols
plot(crop(lcm1990_wales6, e), main="1990",)
plot(crop(lcm2015_wales6, e), main="2015")
par(mfrow=c(1,1))  #reset

