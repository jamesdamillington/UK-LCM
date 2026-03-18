#R version of Principles of SDS practical, Section 9 (Raster Pattern Analysis)
#James D.A. Millington, March 2026

#load packages
library(terra)  #to handle raster data, see https://rspatial.github.io/terra/index.html
library(sf)  #to handle vector data, see https://r-spatial.github.io/sf/

library(tidyr)  #for data manipulation (e.g. pivot_wider), https://tidyr.tidyverse.org/
library(dplyr)  #for data manipulation, https://dplyr.tidyverse.org/ 
library(ggplot2)  #for plotting, https://ggplot2.tidyverse.org/
library(landscapemetrics) #for pattern metrics, see https://r-spatialecology.github.io/landscapemetrics/

lcm2015_wales6 = rast("data/lcm2015_wales6_out_r.tif")
dim(lcm2015_wales6)

##TASK
lcm1990_wales6 = rast("data/lcm1990_wales6_out_r.tif")

#####
#Reading polygon data
GB = st_read("data/CTYUA_(Dec_2019)_Ultra_Generalised_Clipped_Boundaries_in_the_UK.geojson")
head(GB)

#check CRS and reproject 
crs(GB)
GB <- st_transform(GB, crs(lcm2015_wales6))
crs(GB)

#subset to wales only
plot(GB)
wales <- GB[startsWith(GB$ctyua19cd, "W"),]
plot(wales)  

#check
plot(lcm2015_wales6)
lines(wales, col='red', lwd=2)


#terra handles categorical rasters better than numpy in python
#see https://rspatial.github.io/terra/reference/factors.html
#we set our raster to be categorical here to support later analysis

#create a data.frame of classes represented (in the raster) with colours to plot)
cls <- data.frame(id=1:6, 
                  cover=c("Woodland", "Cropland", "Grassland", "Water", "Built-up", "Other"),
                  cols=c('darkgreen', 'lightyellow', 'lightgreen', 'blue', 'black','grey' )
)
#set the categories
levels(lcm2015_wales6) <- cls

#check
lcm2015_wales6
plot(lcm2015_wales6)

#and for 1990
levels(lcm1990_wales6) <- cls


#####
#Zonal Summary
#there is a zonal function in terra, but it cannot provide counts of categorical classes 
#per polygon. instead we use extract() with fun=table  
zs_wales15_6 <- terra::extract(lcm2015_wales6, wales, fun=table)
zs_wales15_6

#relabel with year indicator for intuitive description when comparing between years
lcs = c('Woodland', 'Cropland', 'Grassland', 'Water', 'Built-up', 'Other')
lcs15 <- paste0(lcs, "15")
names(zs_wales15_6) <- c("ID", lcs15)  #rename column for later merge
zs_wales15_6

#create column to join on in the vector data
wales$ID = seq(1:length(zs_wales15_6[,1]))

#join our data to the wales df
new_wales <- merge(wales, zs_wales15_6)
head(new_wales)

#simple plotting
plot(new_wales['Grassland15'])

ggplot(new_wales) +
  geom_sf(aes(fill=Grassland15)) +
  scale_fill_viridis_c()

# plotting with quantile bins
new_wales <- new_wales |>
  mutate(
    g15qbin = cut(
      Grassland15,
      breaks = quantile(Grassland15, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE
    )
  )

ggplot(new_wales) +
  geom_sf(aes(fill = g15qbin)) +
  scale_fill_viridis_d() +
  ggtitle("Grassland 2015")


##TASK
zs_wales90_6 <- terra::extract(lcm1990_wales6, wales, fun=table)
zs_wales90_6

lcs90 <- paste0(lcs, "90")
names(zs_wales90_6) <- c("ID", lcs90)

new_wales <- merge(new_wales, zs_wales90_6)
head(new_wales)

new_wales$'Grassland_90-15' = new_wales$'Grassland15' - new_wales$'Grassland90'

new_wales <- new_wales |>
  mutate(
    g9015qbin = cut(
      `Grassland_90-15`,
      breaks = quantile(`Grassland_90-15`, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE
    )
  )


ggplot(new_wales) +
  geom_sf(aes(fill = g9015qbin)) +
  scale_fill_viridis_d() +
  ggtitle("Grassland Difference (1990-2015)")



#####
#Improving maps of change




#####
#Landscape Pattern Analysis

#plot using landscapemetrics
show_patches(lcm1990_wales6)
show_patches(lcm1990_wales6, class = 1)

#simple calculation of landscape metrics
lsm_l_np(lcm1990_wales6)


lsp1990_lsp_df <- calculate_lsm(lcm1990_wales6,
                                level="landscape",
                                metric=c("np",
                                         "ed",
                                         "area",
                                         "enn"))

lsp1990_lsp_df


#TASK
lsp2015_lsp_df <- calculate_lsm(lcm2015_wales6,
                                level="landscape",
                                metric=c("np",
                                         "ed",
                                         "area",
                                         "enn"))

lsp2015_lsp_df


lsp1990_class_df <- calculate_lsm(lcm1990_wales6,
                                level="class",
                                metric=c("np",
                                         "ed",
                                         "area",
                                         "enn"))

print(lsp1990_class_df, n=50)

#unfortunately landscapemetrics does not detect our rasters are categorical
#so we record our output data to be more intuitive
#this code is for dplyr <1.2.0 (see below for version 1.2.0)
#check your version with packageVersion("dplyr")
lsp1990_class_df <- mutate(lsp1990_class_df,
                           class=recode_factor(class,
                                               `1`="Woodland",
                                               `2`="Cropland",
                                               `3`="Grassland",
                                               `4`="Water",
                                               `5`="Built-up",
                                               `6`="Other"))

print(lsp1990_class_df, n=50)

#with dplyr 1.2.0 the following should work (not tested)
#lcnames <- c('Woodland','Cropland','Grassland', 
#               'Water','Built-up','Other')
#
#lccodes <- seq(1,6,1)
#
#lsp1990_class_df2 <- mutate(lsp1990_class_df,
#                           class=dplyr::recode(lccodes,lcnames))


#landscapemetrics does not calculate proportion, so calculate ourselves
class_areas90 <- colSums(zs_wales90_6[,2:7])
class_props90 <- class_areas90 / sum(class_areas90)
class_props90

#add class props (we need to specify how many times to repeat for the long table)
#dim(lsp1990_class_df)  #48 rows so repeat 6 classes 8 times
lsp1990_class_df <- mutate(lsp1990_class_df, class_prop=rep(class_props90,8))
lsp1990_class_df

#make table wide
lsp1990_class_df_wide <- pivot_wider(lsp1990_class_df, 
                                     id_cols=class, 
                                     names_from=metric, 
                                     values_from=value,
                                     unused_fn=first)
lsp1990_class_df_wide


##TASK
lsp2015_class_df <- calculate_lsm(lcm2015_wales6,
                                  level="class",
                                  metric=c("np",
                                           "ed",
                                           "area",
                                           "enn"))

#with dplyr <1.2.0
lsp2015_class_df <- mutate(lsp2015_class_df,
                           class=recode_factor(class,
                                               `1`="Woodland",
                                               `2`="Cropland",
                                               `3`="Grassland",
                                               `4`="Water",
                                               `5`="Built-up",
                                               `6`="Other"))

print(lsp2015_class_df, n=50)

#landscapemetrics does not calculate proportion, so calculate ourselves
class_areas15 <- colSums(zs_wales15_6[,2:7])
class_props15 <- class_areas15 / sum(class_areas15)

lsp2015_class_df <- mutate(lsp2015_class_df, class_prop=rep(class_props15,8))
lsp2015_class_df

#make table wide
lsp2015_class_df_wide <- pivot_wider(lsp2015_class_df, 
                                     id_cols=class, 
                                     names_from=metric, 
                                     values_from=value,
                                     unused_fn=first)
lsp2015_class_df_wide

#Spatio Temporal Analysis
#landscapemetrics doesn't have this functionality so we just combine from the tables above

lsp1990_lsp_df <- mutate(lsp1990_lsp_df, Year="1990")
lsp2015_lsp_df <- mutate(lsp2015_lsp_df, Year="2015")

st9015_lsp_df <- bind_rows(lsp1990_lsp_df, lsp2015_lsp_df)
st9015_lsp_df <- pivot_wider(st9015_lsp_df, id_cols=Year, 
                             names_from=metric, values_from=value)
st9015_lsp_df

ggplot(st9015_lsp_df) +
  geom_col(aes(x=Year, y=np)) +
  ggtitle("Number of Patches")

##TASK

lsp1990_class_df <- mutate(lsp1990_class_df, Year="1990")
lsp2015_class_df <- mutate(lsp2015_class_df, Year="2015")

st9015_class_df <- bind_rows(lsp1990_class_df, lsp2015_class_df)

st9015_class_df <- pivot_wider(st9015_class_df, id_cols=(c(class,Year)), 
                             names_from=metric, values_from=value,
                             unused_fn=first)

select(st9015_class_df, class, Year, class_prop, np, ed, area_mn, enn_mn)


ggplot(st9015_class_df) +
  geom_col(aes(x=class, y=np, fill=Year), position="dodge") +
  ggtitle("Number of Patches")


##TASK
ggplot(st9015_class_df) +
  geom_col(aes(x=class, y=ed, fill=Year), position="dodge") +
  ggtitle("Edge Density")




######
#Analysing Pixel-by-Pixel Change

#create a single (two-layer) SpatRaster from two rasters 
stack9015 <- c(lcm1990_wales6, lcm2015_wales6)
names(stack9015) <- c("lcm1990", "lcm2015")  #add names so we can see them in contingency table

#cross tabulate (contingency table)
ct9015 <- terra::crosstab(stack9015)

#because we set our raster as categorical above, the output table already has labels
ct9015



