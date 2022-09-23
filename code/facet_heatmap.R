### Create world heatmap, rural vs. urban 1x2 grid
facet_heatmap <- function(data, ind, yr) {
  
  # Create filtered datasets for each location
  data_urban <- select_ind_year_loc(data, ind, yr, "URBAN")
  data_rural <- select_ind_year_loc(data, ind, yr, "RURAL")
  
  # Joined filtered data with world map
  map_urban <- left_join(map_world, data_urban, by = 'country')
  map_rural <- left_join(map_world, data_rural, by = 'country')
  
  # Bind all data together
  map <- rbind(map_urban, map_rural)
  
  # Create data frame ... with something I'll later facet by ## OBS! Why does this work?
  map <- data.frame(map, Facet = rep(c("map_urban","map_rural"),
                                          times=c(nrow(map_urban),nrow(map_rural))))
  
  # Text for plot title
  title <- map$seriesdescription %>% na.omit() %>% unique()
  
  # Plot graph
  ggplot(map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = value)) +
    
    # Labels and legend
    labs(title = "Population with access to electricity, by rural/urban",
         subtitle = paste('Year', yr),
         fill = '  %',
         x = NULL, y = NULL) +
    
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = 'lightgrey',
                        breaks = seq(0, 100, 20)) +
    
    # Theme adjustments
    theme(plot.title = element_text(size = 18, face = 'bold'),
          plot.subtitle = element_text(size = 16, hjust = 0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.key.size = unit(25, 'pt'),
          panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(10,10,0,23),"mm")) + # top, right, bottom, left
    
    # Faceting
    facet_wrap(.~Facet)
  
}

facet_heatmap(dat, "7.1.1", 2000)
