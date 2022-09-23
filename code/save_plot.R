# Save plot with filename given as argument
save_last_plot <- function(filename, w_mm = 400, h_mm = 200) {
  
  ggsave(filename = paste(filename, '.png', set = ''),
         plot = last_plot(),
         path = './figs/',
         device = 'png',
         width = w_mm, height = h_mm, units = 'mm')
  
}
