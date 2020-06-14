
# Manual Lotus ####

library(tidyverse); library(magrittr); library(igraph)

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

LinkDF %>% unique %>% RandomSlice(10) %>% as.matrix %>% 
  
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


# Trying a ggraph ####



