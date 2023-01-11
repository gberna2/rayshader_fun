library(sf)
library(R.utils)
library(tigris)
library(tidyverse)
library(rayshader)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

#https://www.youtube.com/watch?v=zgFXVhmKNbU

install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")

# unzip file
gunzip("kontur_population_US_20220630 (2).gpkg.gz", remove=FALSE)

# load kantur data
data<-st_read("kontur_population_US_20220630 (2).gpkg")

# load states
st<- states()

# filter for florida
florida<- st |>
  filter(NAME=="Florida") |>
  st_transform(crs=st_crs(data)) #transform coordinate reference system

#check with map
florida |>
  ggplot()+
  geom_sf()

# intersection on data to limit
st_florida <- st_intersection(data, florida)

# define aspect ratio based on bounding box

bb<- st_bbox(st_florida)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs=st_crs(data))

bottom_right<- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs=st_crs(data))

width <- st_distance(bottom_left, bottom_right)

top_left<- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of with or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}


# convert to raster so we can convert to matrix
size<- 1000

florida_rast <- st_rasterize(st_florida,
                             nx = floor(size * w_ratio),
                             ny=floor(size* h_ratio))

mat<- matrix(florida_rast$population, 
             nrow=floor(size * w_ratio),
             ncol=floor(size* h_ratio))


# plot 3d
mat |>
  height_shade() |>
  plot_3d(heightmap=mat)