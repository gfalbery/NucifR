
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

BufferWidth <- Size/1.2

Hull[,c("X", "Y")] %>%
  Polygon() %>% list %>%
  Polygons(ID = '0') %>%
  list %>% SpatialPolygons %>%
  st_as_sf %>% 
  st_buffer(BufferWidth) -> Buffered

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
  geom_polygon(data = Outline, fill = "white", colour = LineColour) + 
  coord_fixed() + theme_void() +
  geom_point() + 
  geom_voronoi_segment()

# The pod locations work; on with the aesthetics ####

OuterLimits <- Outline %>% ExtentGet()

OuterLimits[1:2] %>% unlist %>% 
  expand.grid(X = ., Y = OuterLimits[1:2+2] %>% unlist) %>% 
  as.matrix -> FlowerExtent

PodLocations %>% 
  dismo::voronoi(ext = FlowerExtent) -> 
  Voronoi

1:length(Voronoi@polygons) %>% 
  map(~Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% data.frame) %>% 
  bind_rows(.id = "Polygon") %>% 
  #mutate_at("Polygon", ~factor(.x, levels = 1:length(Voronoi@polygons))) %>% 
  rename(X = X1, Y = X2) -> VoronoiDF

VoronoiDF

VoronoiDF %>% 
  ggplot(aes(X, Y, fill = Polygon)) + 
  geom_polygon() +
  geom_polygon(data = Outline, fill = NA, colour = LineColour, size = 4) + 
  scale_fill_discrete_sequential(palette = AlberPalettes[[1]])

Voronoi %>% plot

Buffered %>% plot

Voronoi %>% lines

# Making shrunken pod shapes ####

.x <- 1

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% data.frame

1:length(Voronoi@polygons) %>% 
  map(~Voronoi@polygons[[.x]]@Polygons[[1]] %>% 
        list %>%
        Polygons(ID = '0') %>%
        list %>% SpatialPolygons %>%
        st_as_sf %>% st_centroid %>% 
        unlist %>% as_tibble %>% t %>% as_tibble) %>% 
  bind_rows(.id = "Polygon") %>% rename(X = V1, Y= V2) ->
  Centroids

Centroids[,2:3] %>% plot
points(PodLocations)
Voronoi %>% lines

# Centroids and pod locations are similar ####

Size <- 10

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% 
  data.frame %>% rename(X = X1, Y = X2) %>% 
  apply(1, function(a){
    
    #print(a)
    
    data.frame(1:Size, X = a[["X"]], Y = a[["Y"]]) %>% #bind_rows %>% 
      bind_rows(PodLocations[.x, ]) %>% 
      summarise_at(c("X", "Y"), mean)
    
  }) %>% bind_rows() %>% points

# Trying a different approach ####

Shrink <- 3

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% plot

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% 
  data.frame %>% rename(X = X1, Y = X2) %>% 
  apply(1, function(a){
    
    #print(a)
    
    data.frame(1:Shrink, 
               X = PodLocations[.x, "X"], 
               Y = PodLocations[.x, "Y"]) %>% #bind_rows %>% 
      bind_rows(data.frame(X = a[["X"]], Y = a[["Y"]]) ) %>% 
      summarise_at(c("X", "Y"), mean)
    
  }) %>% bind_rows() %>% points

# Adding buffer ####

Shrink <- 3

Grow <- 0.15

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% plot

1:length(Voronoi@polygons) %>% 
  map(~Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% 
        data.frame %>% rename(X = X1, Y = X2) %>% 
        apply(1, function(a){
          
          #print(a)
          
          data.frame(1:Shrink, 
                     X = PodLocations[.x, "X"], 
                     Y = PodLocations[.x, "Y"]) %>% #bind_rows %>% 
            bind_rows(data.frame(X = a[["X"]], Y = a[["Y"]]) ) %>% 
            summarise_at(c("X", "Y"), mean)
          
        }) %>% bind_rows() %>% 
        Polygon() %>% list %>%
        Polygons(ID = '0') %>%
        list %>% SpatialPolygons %>%
        st_as_sf %>% 
        st_buffer(Grow) %>% extract2("geometry") %>% unlist %>% 
        matrix(ncol = 2) %>% data.frame %>% 
        rename(X = X1, Y = X2) -> 
        PodOutline
  ) %>% bind_rows(.id = "Polygon") -> PodPolygons

PodPolygons[,2:3] %>% lines

VoronoiDF %>% 
  ggplot(aes(X, Y)) + #, fill = Polygon)) + 
  geom_polygon(fill = "white", colour = LineColour, aes(group = Polygon)) +
  geom_polygon(data = Outline, fill = NA, colour = LineColour, size = 4) + 
  scale_fill_discrete_sequential(palette = AlberPalettes[[1]]) +
  geom_polygon(data = PodPolygons, aes(group = Polygon))

# This sort-of works but need to mask the voronois slightly ####

Outline %>% Polygon() %>% list %>%
  Polygons(ID = '0') %>%
  list %>% SpatialPolygons -> SpatialBuffered

.x <- 10

Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% plot

1:length(Voronoi@polygons) %>% 
  map(~Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% 
        Polygon() %>% list %>%
        Polygons(ID = '0') %>%
        list %>% SpatialPolygons %>% crop(SpatialBuffered, .) %>% 
        (function(a){a@polygons[[1]]@Polygons[[1]]@coords}) %>% 
        data.frame %>% rename(X = x, Y = y) %>% 
        apply(1, function(a){
          
          #print(a)
          
          data.frame(1:Shrink, 
                     X = PodLocations[.x, "X"], 
                     Y = PodLocations[.x, "Y"]) %>% #bind_rows %>% 
            bind_rows(data.frame(X = a[["X"]], Y = a[["Y"]]) ) %>% 
            summarise_at(c("X", "Y"), mean)
          
        }) %>% bind_rows() %>% 
        Polygon() %>% list %>%
        Polygons(ID = '0') %>%
        list %>% SpatialPolygons %>%
        st_as_sf %>% 
        st_buffer(Grow) %>% extract2("geometry") %>% unlist %>% 
        matrix(ncol = 2) %>% data.frame %>% 
        rename(X = X1, Y = X2) -> 
        PodOutline
      
      #PodOutline %>% lines
      
  ) -> PodPolygonList

PodPolygonList %>% bind_rows(.id = "Polygon") -> PodPolygons

PodPolygons[,2:3] %>% lines

VoronoiDF %>% 
  ggplot(aes(X, Y, fill = Polygon)) + 
  #geom_polygon(fill = "white", colour = LineColour, aes(group = Polygon)) +
  geom_polygon(data = Outline, fill = NA, colour = LineColour) + 
  scale_fill_discrete_sequential(palette = AlberPalettes[[1]]) +
  geom_polygon(data = PodPolygons, aes(group = Polygon), 
               fill = "white", colour = LineColour)

# Adding some shading ####

XShadowOffset <- 0.1
YShadowOffset <- - 0.05

ShadingDirection = "CloseUp"

CameraDistance <- 0.5

ShadowNoise <- 0.025

if(ShadingDirection == "Uniform"){
  
  XShadowOffset <- rep(XShadowOffset, NPods)
  YShadowOffset <- rep(YShadowOffset, NPods)
  
} else if(ShadingDirection == "CloseUp"){
  
  XShadowOffset %<>% abs
  YShadowOffset %<>% abs
  
  -Centroids$X %>% 
    multiply_by(XShadowOffset) %>% 
    multiply_by(CameraDistance) ->
    
    XShadowOffset
  
  -Centroids$Y %>% 
    multiply_by(YShadowOffset) %>% 
    multiply_by(CameraDistance) ->
    
    YShadowOffset
  
}else if(ShadingDirection == "Varied"){
  
  XShadowOffset %<>% abs
  YShadowOffset %<>% abs
  
  XShadowOffset <- runif(NPods, -XShadowOffset, XShadowOffset)
  YShadowOffset <- runif(NPods, -YShadowOffset, YShadowOffset)
  
}

if(!is.null(ShadowNoise)){
  
  YShadowOffset <- YShadowOffset + rnorm(NPods, 0, ShadowNoise)
  XShadowOffset <- XShadowOffset + rnorm(NPods, 0, ShadowNoise)
  
}

1:length(Voronoi@polygons) %>% 
  map(~PodPolygonList[[.x]] %>% 
        mutate_at("X", function(x) x + XShadowOffset[.x]) %>% 
        mutate_at("Y", function(y) y + YShadowOffset[.x]) %>% 
        Polygon() %>% list %>%
        Polygons(ID = '0') %>%
        list %>% SpatialPolygons %>% 
        crop(., PodPolygonList[[.x]] %>% 
               Polygon() %>% list %>%
               Polygons(ID = '0') %>%
               list %>% SpatialPolygons) %>% 
        (function(a){a@polygons[[1]]@Polygons[[1]]@coords}) %>% 
        data.frame %>% rename(X = x, Y = y)
  ) %>% bind_rows(.id = "Polygon") -> 
  
  PodInteriors

VoronoiDF %>% 
  ggplot(aes(X, Y)) + #, fill = Polygon)) + 
  #geom_polygon(fill = "white", colour = LineColour, aes(group = Polygon)) +
  geom_polygon(data = Outline, fill = NA, colour = LineColour) + 
  scale_fill_discrete_sequential(palette = AlberPalettes[[1]]) +
  geom_polygon(data = PodPolygons, aes(group = Polygon), 
               colour = LineColour) +
  geom_polygon(data = PodInteriors, aes(group = Polygon), 
               fill = "white", colour = "dark grey") +
  coord_fixed()

# Adding it all together ####

PodJitter <- 0

LineColour <- "black"

VoronoiDF %>% 
  ggplot(aes(X, Y)) + #, fill = Polygon)) + 
  #geom_polygon(fill = "white", colour = LineColour, aes(group = Polygon)) +
  # geom_polygon(data = Outline, fill = NA, colour = LineColour) + 
  scale_fill_discrete_sequential(palette = AlberPalettes[[1]]) +
  geom_polygon(data = PodInteriors, aes(group = Polygon), 
               fill = "white", colour = LineColour)  +
  coord_fixed() + 
  theme_void() ->
  
  FlowerPlot

OutlineAdd <- ShadowAdd <- PointAdd <- T

if(ShadowAdd){
  
  FlowerPlot <- FlowerPlot +
    geom_polygon(data = PodPolygons, aes(group = Polygon), 
                 colour = "black", size = 1) +
    geom_polygon(data = PodInteriors, aes(group = Polygon), 
                 fill = "white", colour = NA)
  
}

if(PointAdd){
  
  FlowerPlot <- FlowerPlot +
    geom_point(data = PodLocations %>% 
                 mutate_at("X", ~.x + XShadowOffset) %>% 
                 mutate_at("Y", ~.x + YShadowOffset) %>% 
                 RandomSlice(round(NPods)),
               position = position_jitter(w = PodJitter, h = PodJitter),
               alpha = 0.6)
  
}

if(OutlineAdd){
  
  FlowerPlot <- FlowerPlot + 
    geom_polygon(data = Outline, fill = NA, colour = LineColour)
  
}

FlowerPlot

PodLocations %>% rownames_to_column("ID")







