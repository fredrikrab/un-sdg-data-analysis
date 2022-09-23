### Create GIF
gif_grid <- function(data, ind, start, end, d = 0.5, filename = "animation", w = 1280, h = 720) {
  
  # Accumulator variable for the GIF frame
  frame <- start
  
  for (i in start:end) {
    
    # Set PNG file specifications
    png(paste('./figs/', filename, '_', frame, '.png', sep = ''), width = w, height = h)
    
    # Create plots
    heatmap <- facet_heatmap(data, ind, frame)
    scatterplot <- facet_scatterplot(data, ind, start, frame, end)
    
    # Print arranged plots
    grid.arrange(heatmap, scatterplot, nrow = 2, heights = c(7/10, 3/10))
    
    # Close PNG file
    dev.off()
    
    # Move on to next frame in GIF
    frame <- frame + 1
  }
  
  # Set GIF filename
  gif_filename <- paste('./figs/', filename, '.gif', sep = '')
  
  # Create list of PNG filenames
  filenames <- as.character(seq(start, end))
  filenames <- paste('./figs/', filename, '_', filenames, '.png', sep= '')
  
  # Create GIF from PNG images
  gifski(filenames, delay = d, width = w, height = h,
         gif_file = gif_filename)
  
  # Delete PNG images
  file.remove(filenames)
  
  # Open GIF with default local application
  utils::browseURL(gif_filename)
  
}

gif_grid(dat, "7.1.1", 2000, 2016, 0.5, "electricity_access_urban_vs_rural", 1440, 720)
