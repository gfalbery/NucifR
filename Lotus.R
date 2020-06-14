
# Lotus ####

library(tidyverse); library(sf); library(spatial); library(sp); library(adehabitatHR)

N <- 1000

NPods <- 30

Size <- 1

X <- rnorm(N, 0, Size)
Y <- rnorm(N, 0, Size)

Locations <- data.frame(X, Y)

Dist <- dist(Locations) %>% as.matrix

Dist[which(!rowSums(Dist)<(Size/100)),
     which(!rowSums(Dist)<(Size/100))]

Locations %>% plot

Locations %>% 
  mutate_at(c("X", "Y"), ~round(.x*10)/10) %>% 
  group_by(X, Y) %>% count %>% 
  SpatialPointsDataFrame(data = .[,"n"], coords = .) -> SPDFRounded

SPDFRounded %>% 
  raster::rasterFromXYZ() %>% plot

SPDFRounded[SPDFRounded$n>2,] %>% 
  raster::rasterFromXYZ() %>% plot

# Creating probability density of lotus ####

SPDF <- SpatialPointsDataFrame(data = Locations, 
                               coords = Locations)

LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)

LifetimeKUDL %>% raster::raster() %>% 
  raster::extract(Locations) ->
  
  Locations$Density

# Create outer bounds of the lotus ####

Locations %<>% 
  filter(Density>0.025)

Locations %>% 
  chull -> HullWhich

Locations[HullWhich, c("X", "Y")] -> Hull

plot(Hull)

# Buffering the bounds of the lotus ####

Hull[,c("X", "Y")] %>%
  Polygon() %>% list %>%
  Polygons(ID = '0') %>%
  list %>% SpatialPolygons %>%
  st_as_sf %>% 
  st_buffer(Size/2) -> Buffered

plot(Buffered)

points(Locations)

# Creating subsections of the lotus ####



