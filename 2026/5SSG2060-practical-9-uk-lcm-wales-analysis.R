library(terra)  #to handle raster data
library(sf)  #to handle vector data
library(exactextractr)  #for zonal stats
library(tidyr)  #for data manipulation (unnest_wider)
library(dplyr)  #for data manipulation 
library(ggplot2)  #for plotting
library(purrr)  #for map

lcm2015_wales6 = rast("data/lcm2015_wales6_out_r.tif")
dim(lcm2015_wales6)
##TASK
lcm1990_wales6 = rast("data/lcm1990_wales6_out_r.tif")

#####
#Reading polygon data
GB = st_read("data/CTYUA_(Dec_2019)_Ultra_Generalised_Clipped_Boundaries_in_the_UK.geojson")
head(GB)

crs(GB)
GB <- st_transform(GB, crs(lcm2015_wales6))
crs(GB)

plot(GB)

wales <- GB[startsWith(GB$ctyua19cd, "W"),]

plot(wales)  

plot(lcm2015_wales6)
lines(wales, col='red', lwd=2)



#####
#Zonal Summary
#there is a zonal function in terra, but it cannot provide counts of categorical classes 
#per polygon. instead we use extract() with fun=table  
zs_wales15_6 <- terra::extract(lcm2015_wales6, wales, fun=table)
zs_wales15_6

#relabel for intuitive description
lcs = c('Woodland', 'Cropland', 'Grassland', 'Water', 'Built-up', 'Other')
lcs15 <- paste0(lcs, "15")
names(zs_wales15_6) <- c("ID", lcs15)  #rename column for later merge


#write code to contine the non-tidy way? Gets a bit complicated below

#create column to join on
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
library(landscapemetrics)


show_patches(lcm1990_wales6)
show_patches(lcm1990_wales6, class = 1)


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

#with dplyr <1.2.0
lsp1990_class_df <- mutate(lsp1990_class_df,
                           class=recode_factor(class,
                                               `1`="Woodland",
                                               `2`="Cropland",
                                               `3`="Grassland",
                                               `4`="Water",
                                               `5`="Built-up",
                                               `6`="Other"))

print(lsp1990_class_df, n=50)

#in future (dplyr1.2.0) this should work
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

#add class props (we need to specify how many times to repeat for the long table)
#dim(lsp1990_class_df)  #48 rows so repeat 6 classes 8 times
lsp1990_class_df <- mutate(lsp1990_class_df, class_prop=rep(class_props90,8))

#make table wide
pivot_wider(lsp1990_class_df, id_cols=class, names_from=metric, values_from=value,
            unused_fn=first)



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

#make table wide
pivot_wider(lsp2015_class_df, id_cols=class, names_from=metric, values_from=value,
            unused_fn=first)


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




#tidyr way

#you might think we can use fun='count' but that simple counts the pixels in each polygon
z <- exact_extract(lcm2015_wales6, 
                   wales, 
                   fun = function(values, coverage_fractions) { table(values) })

lcs_rename = c('Woodland'="1",
               'Cropland'="2", 
               'Grassland'="3", 
               'Water'="4", 
               'Built-up'="5", 
               'Other'="6")


#z is a list of tables, convert to a list of numeric vectors
#names are the class types from the raster values
z_num <- z |> 
  map(~ as.numeric(.x) |> setNames(names(.x)))

#create a tibble from the (nested) listm, renaming from raster values to lc names  
z_wide <- tibble(
  OBJECTID = wales$OBJECTID,
  counts = z_num) |>
  unnest_wider(counts) |>
  rename(any_of(lcs_rename)) |>
  rename_with(~ paste0(.x, "_15"), .cols = -OBJECTID)
z_wide

#join the lc pixels counts to their respective polys 
wales <- wales |>
  left_join(z_wide, by = "OBJECTID")

#quick plot
plot(wales['Grassland_15'])

#better quick plot
ggplot(new_wales) +
  geom_sf(aes(fill=Grassland15)) +
  scale_fill_viridis_c()


