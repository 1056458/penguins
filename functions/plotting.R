
#-----------------------------------------------------------------------------------------------------------------

# Plot regression of body mass as a function of flipper length 

plot_regression <- function(penguin_cleaner){
  penguin_cleaner %>%
    ggplot(aes(x = flipper_length_mm, 
               y = body_mass_g,
               colour = species)) +
    geom_point(alpha = 0.8) + 
    geom_smooth(method = "lm", colour = "#0d0887") +
    scale_color_manual(values = c("#f0f921", "#ed7953","#9c179e")) +
    labs(title = expression(paste(underline("Linear regression of body mass as a function of flipper length"))),
         x = "Flipper Length (mm)",
         y = "Body Mass (g)", 
         colour = expression(paste(underline("Species of Penguin")))) +
    theme_light() + 
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", hjust = -0.1))
}

#-----------------------------------------------------------------------------------------------------------------

# Save figure as a png 

save_lm_plot_png <- function(penguin_cleaner, 
                             filename, size, res, scaling){
  agg_png(filename, 
          width   =  size*1.25, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  mass_length_lm <- plot_regression(penguin_cleaner)
  print(mass_length_lm)
  dev.off()
}

#-----------------------------------------------------------------------------------------------------------------

# Save figure as a vector

save_lm_plot_svg <- function(penguin_cleaner, 
                             filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, 
          width   = size_inches*1.25, 
          height  = size_inches, 
          scaling = scaling)
  mass_length_lm <- plot_regression(penguin_cleaner)
  print(mass_length_lm)
  dev.off()
}

#-----------------------------------------------------------------------------------------------------------------