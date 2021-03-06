
# Lotus ####

library(tidyverse); library(sf); library(spatial); library(sp); library(adehabitatHR);library(igraph)
library(ggforce); library(magrittr); library(ggregplot); library(raster); library(colorspace)

Lotus <- function(
  
  N = 1000,
  
  NPods = 35,
  
  XNoise = 0,
  YNoise = 0,
  
  # Aesthetics
  
  OutlineAdd = T,
  ShadowAdd = T,
  PointAdd = T,
  
  NetworkAdd = F,
  Density = 0.05,
  EdgeColour = "black", EdgeAlpha = 1, EdgeWidth = 1,
  Network = "Spatial",
  
  SpotJitter = 0.1,
  
  LineColour = "black",
  
  Shrink = 3,
  
  Grow = 0.2,
  
  # Shadows 
  
  XShadowOffset = - 0.1,
  YShadowOffset = -0.05,
  ShadingDirection = "Uniform",
  CameraDistance = 0.5,
  ShadowNoise = 0.015,
  
  Size = 1,
  
  RemoveOuter = "Relaxed"
  
){
  
  X <- rnorm(N, 0, Size)
  Y <- rnorm(N, 0, Size)
  
  Locations <- data.frame(X, Y)
  
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
  
  # Buffering the bounds of the lotus ####
  
  BufferWidth <- Size/1.2
  
  Hull[,c("X", "Y")] %>%
    Polygon() %>% list %>%
    Polygons(ID = '0') %>%
    list %>% SpatialPolygons %>%
    st_as_sf %>% 
    st_buffer(BufferWidth) -> Buffered
  
  # Creating subsections of the lotus ####
  
  M1 <- table(data.frame(1:NPods, 1:NPods))
  
  diag(M1) <- 0
  
  M1 %>% graph_from_adjacency_matrix() %>% layout.auto() -> PodLocations
  
  PodLocations[,1] <- PodLocations[,1] + runif(NPods, -XNoise, XNoise)
  PodLocations[,2] <- PodLocations[,2] + runif(NPods, -XNoise, XNoise)
  
  PodLocations[,1] %<>% scales::rescale(c(min(Hull[,1]), max(Hull[,1])))
  PodLocations[,2] %<>% scales::rescale(c(min(Hull[,2]), max(Hull[,2])))
  
  PodLocations %<>% data.frame %>% rename(X = X1, Y = X2)
  
  Buffered$geometry %>% unlist %>% 
    matrix(ncol = 2) %>% data.frame %>% 
    rename(X = X1, Y = X2) -> 
    Outline
  
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
    rename(X = X1, Y = X2) -> 
    VoronoiDF
  
  # Making shrunken pod shapes ####
  
  1:length(Voronoi@polygons) %>% 
    map(~Voronoi@polygons[[.x]]@Polygons[[1]] %>% 
          list %>%
          Polygons(ID = '0') %>%
          list %>% SpatialPolygons %>%
          st_as_sf %>% st_centroid %>% 
          unlist %>% as_tibble %>% t %>% as_tibble) %>% 
    bind_rows(.id = "Polygon") %>% rename(X = V1, Y= V2) ->
    Centroids
  
  # This sort-of works but need to mask the voronois slightly ####
  
  Outline %>% Polygon() %>% list %>%
    Polygons(ID = '0') %>%
    list %>% SpatialPolygons -> SpatialBuffered
  
  1:length(Voronoi@polygons) %>% 
    map(~Voronoi@polygons[[.x]]@Polygons[[1]]@coords %>% 
          Polygon() %>% list %>%
          Polygons(ID = '0') %>%
          list %>% SpatialPolygons %>% crop(SpatialBuffered, .) %>% 
          (function(a){a@polygons[[1]]@Polygons[[1]]@coords}) %>% 
          data.frame %>% rename(X = x, Y = y) %>% 
          apply(1, function(a){
            
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
        
    ) -> PodPolygonList
  
  PodPolygonList %>% bind_rows(.id = "Polygon") -> PodPolygons
  
  if(RemoveOuter == "Relaxed"){
    
    Centroids %>% 
      dplyr::select(X,Y) %>% as.matrix %>% 
      chull -> OuterWhich
    
    Outline <- PodLocations %>% 
      slice(OuterWhich) %>% 
      dplyr::select(X,Y)
    
    VoronoiDF %<>% filter(!Polygon %in% OuterWhich)
    Centroids %<>% filter(!Polygon %in% OuterWhich)
    PodPolygons %<>% filter(!Polygon %in% OuterWhich)
    
  }else if(RemoveOuter == "Stringent"){
    
    Centroids %>% 
      dplyr::select(X,Y) %>% as.matrix %>% 
      chull -> OuterWhichStringent
    
    Outline <- PodLocations %>% 
      slice(OuterWhichStringent) %>% 
      dplyr::select(X,Y)
    
    VoronoiDF %<>% filter(!Polygon %in% OuterWhichStringent)
    Centroids %<>% filter(!Polygon %in% OuterWhichStringent)
    PodPolygons %<>% filter(!Polygon %in% OuterWhichStringent)
    
  }
  
  # Adding some shading ####
  
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
  
  1:length(PodPolygonList) %>% 
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
  
  if(RemoveOuter == "Relaxed"){
    
    PodInteriors %<>% 
      filter(!Polygon %in% OuterWhich)
    
    PodLocations %<>% 
      mutate_at("X", ~.x + XShadowOffset) %>% 
      mutate_at("Y", ~.x + YShadowOffset) %>% 
      slice(-OuterWhich)
    
  }else  if(RemoveOuter == "Stringent"){
    
    PodInteriors %<>% 
      filter(!Polygon %in% OuterWhichStringent) %>% 
      slice(-OuterWhichStringent)
    
    PodLocations %<>% 
      mutate_at("X", ~.x + XShadowOffset) %>% 
      mutate_at("Y", ~.x + YShadowOffset) %>% 
      slice(-OuterWhichStringent)
    
  }else if(RemoveOuter == "None"){
    
    PodLocations %<>% 
      mutate_at("X", ~.x + XShadowOffset) %>% 
      mutate_at("Y", ~.x + YShadowOffset)
    
  }
  
  if(NetworkAdd){
    
    if(Network == "Random"){
      
      Network <- erdos.renyi.game(NPods, Density)
      
      Network %>% get.edgelist() %>% as.data.frame() %>% 
        rownames_to_column("Group") %>% 
        gather("Whom", "Polygon", -Group) %>% 
        mutate_at("Polygon", as.character) %>% 
        dplyr::select(-Whom) %>% 
        left_join(Centroids, by = c("Polygon")) -> 
        
        EdgeDF
      
    }else if(Network == "Spatial"){
      
      Centroids %>% dplyr::select(X, Y) %>% as.matrix %>% 
        dist %>% as.matrix %>% reshape2::melt() %>% 
        as.data.frame %>% 
        mutate_at("value", ~scales::rescale(.x, c(0, Density))) %>% 
        mutate(Connection = rbinom(n(), 1, value)) %>% 
        filter(as.logical(Connection)) %>% 
        dplyr::select(From = Var1, To = Var2) %>% 
        rownames_to_column("Group") %>% 
        gather("Whom", "Polygon", -Group) %>% 
        mutate_at("Polygon", as.character) %>% 
        dplyr::select(-Whom) %>% 
        left_join(Centroids, by = c("Polygon")) -> 
        
        EdgeDF
      
    }
    
  }
  
  # Adding it all together ####
  
  VoronoiDF %>% 
    ggplot(aes(X, Y))  +
    coord_fixed() + 
    theme_void() ->
    FlowerPlot
  
  if(NetworkAdd){
    
    FlowerPlot <- FlowerPlot + 
      geom_path(data = EdgeDF, 
                aes(group = Group), 
                colour = EdgeColour, 
                alpha = EdgeAlpha,
                size = EdgeWidth)
    
  }
  
  FlowerPlot + 
    scale_fill_discrete_sequential(palette = AlberPalettes[[1]]) +
    geom_polygon(data = PodInteriors, aes(group = Polygon), 
                 fill = "white", colour = LineColour) ->
    
    FlowerPlot
  
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
                   RandomSlice(round(NPods)),
                 position = position_jitter(w = SpotJitter, h = SpotJitter),
                 alpha = 0.6)
    
  }
  
  if(OutlineAdd){
    
    FlowerPlot <- FlowerPlot + 
      geom_polygon(data = Outline, fill = NA, colour = LineColour)
    
  }
  
  FlowerPlot %>% return
  
}

Lotus(NetworkAdd = T, Density = 0.5, 
      OutlineAdd = F, 
      EdgeWidth = 3.5, EdgeAlpha = 0.5, 
      EdgeColour = AlberColours[["Blue"]], Network = "Spatial") + 
  ggsave("Lotus1.jpeg")

Lotus(NetworkAdd = T, Density = 0.5, 
      OutlineAdd = F, 
      EdgeWidth = 3.5, EdgeAlpha = 0.5, 
      EdgeColour = AlberColours[["Pink"]], Network = "Spatial") + 
  ggsave("Lotus2.jpeg")

Lotus(OutlineAdd = T, 
      NPods = 30, Size = 1,
      XNoise = 0, YNoise = 0,
      ShadowNoise = 0.02,
      SpotJitter = 0.1,
      Grow = 0.2)## + ggsave("Lotus1.jpeg")

Lotus(OutlineAdd = F, 
      NPod = 20, Size = 1,
      XNoise = 0, YNoise = 0,
      Grow = 0.2) + ggsave("Lotus1.jpeg")

Lotus(OutlineAdd = F, 
      NPod = 50, Size = 2,
      XNoise = 0, YNoise = 0,
      SpotJitter = 0.01,
      XShadowOffset = -0.05,
      YShadowOffset = -0.05,
      
      Grow = 0.1) #+ ggsave("Lotus1.jpeg")

erdos.renyi.game(NPods, 0.05) %>% plot
