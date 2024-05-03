## plot theme
theme_plot <- function() {
  
  theme_bw() +
    theme(
      ## legend
      legend.position = "top",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      
      ## axes
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      
      ## facets
      strip.background=element_blank(),
      strip.text = element_text(face = "bold", size = 8),
      panel.border = element_blank(),
      axis.line = element_line(color = 'black')
      )
}