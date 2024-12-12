#Code to save a plot as a png file:

save_plot_png <- function(plot_object, 
                          filename, 
                          size, 
                          res, 
                          scaling) {
  # Create a png file with specified parameters.
  
  agg_png(filename, width = size, 
          height = size, 
          units = "cm", 
          res = res, 
          scaling = scaling)
  print(plot_object)
  dev.off()
}
