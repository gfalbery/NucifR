
# Manual Lotus ####

library(tidyverse); library(magrittr); library(igraph); library(ggregplot); library(cowplot)

library(INLA); library(ggregplot); library(tidyverse); library(GGally); library(patchwork)
library(cowplot); library(gganimate); library(colorspace); library(RColorBrewer); library(MCMCglmm)
library(ggrepel); library(fs); library(ggraph)

dir_create("Figures")

theme_set(theme_cowplot() + 
            theme(strip.background = element_rect(fill = "white")))

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])

AlberColours[length(AlberColours)+1:2] <- 
  
  RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]

AlberColours <- append(AlberColours, list(Pink = "#FD6396", Blue = "#3C78D8")) %>% unlist

# Importing ####

ManualLocations <- read.delim("LotusLocations.txt", header = F) %>% 
  rename(X = V1, Y = V2) %>% 
  mutate_at("Y", ~-.x) %>% 
  mutate_at(c("X", "Y"), ~c(scale(.x)))

ManualLocations %>% plot

ManualLocations %<>% rownames_to_column("ID")

ManualLocations %>% ggplot(aes(X, Y, label = ID)) +
  geom_text() + coord_fixed() + theme_void()

# Sexual Network ####

2:4 %>% 
  #sample(40, replace = T) %>% 
  rep(10) %>% 
  map(function(a){
    
    Begin <- sample(1:20, 1)
    
    Chain <- Begin:(Begin + a)
    
    DF <- Chain[1:(a-1)] %>% cbind(Chain[2:a]) %>% as.data.frame
    
    colnames(DF) <- c("From", "To")
    
    DF[DF>20] <- DF[DF>20] - 20
    
    return(DF)
    
  }) -> Chains

Chains %>% bind_rows -> LinkDF

LinkDF %>% unique %>% RandomSlice(20) %>% as.matrix %>% 
  
  graph_from_edgelist(directed = F) -> SexualNetwork

SexualNetwork %>% plot(layout = as.matrix(ManualLocations[,2:3]))

# Aerosol Network ####

2:4 %>% 
  #sample(40, replace = T) %>% 
  rep(10) %>% 
  map(function(a){
    
    Begin <- sample(1:20, 1)
    
    # Chain <- Begin:(Begin + a)
    
    Chain <- sample(1:20, a)
    
    DF <- Chain[1:(a-1)] %>% cbind(Chain[2:a]) %>% as.data.frame
    
    colnames(DF) <- c("From", "To")
    
    DF[DF>20] <- DF[DF>20] - 20
    
    return(DF)
    
  }) -> Chains

Chains %>% bind_rows -> LinkDF

LinkDF %>% unique %>% as.matrix %>% 
  
  graph_from_edgelist(directed = F) -> AerosolNetwork

AerosolNetwork %>% plot(layout = as.matrix(ManualLocations[,2:3]))

# Making the network bigger ####

NInd <- 40

# Sexual Network ####

2:4 %>% 
  #sample(40, replace = T) %>% 
  rep(10) %>% 
  map(function(a){
    
    Begin <- sample(1:NInd, 1)
    
    Chain <- Begin:(Begin + a)
    
    DF <- Chain[1:(a-1)] %>% cbind(Chain[2:a]) %>% as.data.frame
    
    colnames(DF) <- c("From", "To")
    
    DF[DF > NInd] <- DF[DF > NInd] - NInd
    
    return(DF)
    
  }) -> Chains

Chains %>% bind_rows -> LinkDF

LinkDF %>% unique %>% RandomSlice(20) %>% as.matrix %>% 
  
  graph_from_edgelist(directed = F) -> SexualNetwork2

LinkDF %>% unique %>% RandomSlice(10) %>% as.matrix %>% 
  
  graph_from_edgelist(directed = F) -> SexualNetwork

SexualNetwork %>% plot #(layout = as.matrix(ManualLocations[,2:3]))

# Aerosol Network ####

2:4 %>% 
  #sample(40, replace = T) %>% 
  rep(20) %>% 
  map(function(a){
    
    Begin <- sample(1:NInd, 1)
    
    # Chain <- Begin:(Begin + a)
    
    Chain <- sample(1:NInd, a)
    
    DF <- Chain[1:(a-1)] %>% cbind(Chain[2:a]) %>% as.data.frame
    
    colnames(DF) <- c("From", "To")
    
    DF[DF>NInd] <- DF[DF>NInd] - NInd
    
    return(DF)
    
  }) -> Chains

Chains %>% bind_rows -> LinkDF

LinkDF %>% unique %>% as.matrix %>% 
  
  graph_from_edgelist(directed = F) -> AerosolNetwork2

AerosolNetwork2 %>% plot #(layout = as.matrix(ManualLocations[,2:3]))


# Plotting ####

Layout <- layout.sphere(AerosolNetwork2)

AerosolNetwork2 %>% plot(layout = Layout)
SexualNetwork2 %>% plot(layout = Layout)

SexualNetwork %>% ggraph(mode = "undirected", 
                         layout = ManualLocations[,c("X", "Y")] %>% as.matrix) +
  # geom_edge_link(width = 2, alpha = 0.4) +
  geom_edge_diagonal(width = 2, alpha = 0.4, colour = AlberColours[["Blue"]]) + 
  geom_node_point(size = 10, fill = "white", colour = "black") + 
  # geom_node_point(size = 8, fill = "white", colour = "white") + 
  coord_fixed() -> Figure1

AerosolNetwork %>% ggraph(mode = "undirected", 
                          layout = ManualLocations[,c("X", "Y")] %>% as.matrix) +
  # geom_edge_link(width = 2, alpha = 0.4) +
  geom_edge_diagonal(width = 2, alpha = 0.4, colour = AlberColours[["Pink"]]) + 
  geom_node_point(size = 10, fill = "white", colour = "black") + 
  # geom_node_point(size = 8, fill = "white", colour = "white") + 
  coord_fixed() -> Figure2

SexualNetwork2 %>% ggraph(mode = "undirected", 
                          layout = Layout) +
  # geom_edge_link(width = 2, alpha = 0.4) +
  geom_edge_diagonal(width = 2, alpha = 0.4, colour = AlberColours[["Blue"]]) + 
  geom_node_point(size = 10, fill = "white", colour = "black") + 
  # geom_node_point(size = 8, fill = "white", colour = "white") + 
  coord_fixed() -> Figure1Denser

AerosolNetwork2 %>% ggraph(mode = "undirected", 
                           layout = Layout) +
  # geom_edge_link(width = 2, alpha = 0.4) +
  geom_edge_diagonal(width = 2, alpha = 0.4, colour = AlberColours[["Pink"]]) + 
  geom_node_point(size = 10, fill = "white", colour = "black") + 
  # geom_node_point(size = 8, fill = "white", colour = "white") + 
  coord_fixed() -> Figure2Denser

(Figure1Denser |
    Figure2Denser)  +
  ggsave("Figures/LotusPresentation2.jpeg", 
         units = "mm", height = 150, width = 250,
         dpi = 600)

(Figure1|Figure1Denser)/
  (Figure2|Figure2Denser)  +
  ggsave("Figures/LotusTetrad.jpeg", 
         units = "mm", height = 250, width = 250,
         dpi = 600)

BlankPlot <- Figure1 + 
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white", colour ="white")

list(Figure2, Figure2Denser,
     BlankPlot, BlankPlot,
     Figure1, Figure1Denser) %>% 
  plot_grid(plotlist = ., ncol = 2, nrow = 3, rel_heights = c(1, 0.25, 1)) %>% 
  save_plot(filename = "Figures/CowplotLotusTetrad.jpeg", 
            ncol = 2, nrow = 3)
