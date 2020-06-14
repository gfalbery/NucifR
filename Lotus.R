
# Lotus ####

library(tidyverse); library(sf); library(spatial); library(sp); library(adehabitatHR);library(igraph)
library(ggforce)

N <- 1000

NPods <- 20

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
  st_buffer(Size/1.5) -> Buffered

plot(Buffered)

polygon(Hull)

points(Locations)

# Creating subsections of the lotus ####

M1 <- table(data.frame(1:NPods, 1:NPods))

diag(M1) <- 0

M1 %>% graph_from_adjacency_matrix() %>% layout.auto() -> PodLocations

PodLocations[,1] %<>% scales::rescale(c(min(Hull[,1]), max(Hull[,1])))

PodLocations[,2] %<>% scales::rescale(c(min(Hull[,2]), max(Hull[,2])))

PodLocations %<>% data.frame %>% rename(X = X1, Y = X2)

plot(Buffered)

#polygon(Hull)
#points(Locations)

points(Layout, col = "red")

Buffered$geometry %>% unlist %>% 
  matrix(ncol = 2) %>% data.frame %>% 
  rename(X = X1, Y = X2) -> 
  Outline

LineColour <- "dark grey"

PodLocations %>% ggplot(aes(X, Y)) + 
  geom_polygon(data = Outline, fill = "white", colour = LineColour) + coord_fixed() + theme_void() +
  geom_point() + 
  geom_voronoi_segment()
  
