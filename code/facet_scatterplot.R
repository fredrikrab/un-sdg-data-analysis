### Create scatterplot, rural vs. urban 1x2 grid
facet_scatterplot <- function(data, ind, start, end, x_max = end) {
 
  # Create function that filters data used for plotting
  filter_data <- function(data, ind, start, end, loc) {
    
    dat %>%
      
      # Filter for indicator, given year interval and location
      filter(indicator == ind, year %in% c(start:end), location == loc) %>%
      
      # Add column for nr. of people covered (in millions) for all countries
      group_by(year) %>%
      mutate(total_people_million = sum(nr_people/10^6, na.rm = TRUE))
    
  }
  
  # Create filtered datasets for each location
  rural <- filter_data(data, ind, start, end, "RURAL")
  urban <- filter_data(data, ind, start, end, "URBAN")
  
  # Joined filtered data with world map
  plot <- rbind(rural, urban)
  
  # Create data frame ... with something I'll later facet by ## OBS! Why does this work?
  plot <- data.frame(plot, Facet = rep(c("rural","urban"),
                                     times=c(nrow(rural),nrow(urban))))
  
  # Specify limits for plot
  x_min <- start
  #x_max <- end   note: x_max = end is default value of the function (can be overridden)
  y_min <- 3400
  y_max <- 6250
  
  # Plot graph
  plot %>%
    ggplot(aes(year, total_people_million)) +
    geom_point(size = 2) +
    geom_line(size = 0.1) +
    
    # Set limits
    scale_x_continuous("Year", breaks = seq(2000, 2015, by = 5), limits = c(x_min, x_max)) +
    scale_y_continuous("Million people", breaks = seq(3500, 6500, by = 500), limits = c(3400, 6750)) +
    
    # Labels
    labs(x = "Year") +
    
    # Second axis
    
    
    # Theme adjustments
    theme(panel.background = element_rect(linetype = 5),
          panel.border = element_rect(size = 1, colour = 'gray50', fill = NA),
          plot.margin = unit(c(0,34,10,6),"mm"),  # top, right, bottom, left
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12)) +
    
    # Faceting
    facet_grid(~Facet)
  
}

facet_scatterplot(dat, "7.1.1", 2000, 2016)
