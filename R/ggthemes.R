library(ggplot2)

theme_science = theme(panel.grid.major = element_line(size =0.5, color ="grey"),
                      axis.line = element_line(size =0.7, color ="black"),
                      legend.position = c(0.85, 0.7),
                      text =element_text(size =14))

theme_presentation<- function(base_size = 28, base_family = "") {
  # Starts with theme_bw, drops some lines and increase font size
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.text.x = element_text(size = 18),
      strip.text.y = element_text(size = 18),
      axis.text.x = element_text(size=28),
      axis.text.y = element_text(size=28,hjust=1),
      axis.title.x= element_text(size=42),
      axis.title.y= element_text(size=42,angle=90),
      #legend.position = "none", 
      panel.border =element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.margin = unit(1.0, "lines"), 
      plot.title =element_text(size=28), 
      plot.margin = unit(c(1,  1, 1, 1), "lines"),
      legend.title=element_text(size=28),
      legend.text=element_text(size=28),
      legend.key.size = unit(c(1, 1), "lines")
    )
}

theme_presentation_dark<- function(base_size = 28, base_family = "") {
  # Starts with theme_grey, removes some lines, flips bw
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 18,colour="white"),
      strip.text.y = element_text(size = 18,colour="white"),
      axis.text.x = element_text(size=28,colour="white"),
      axis.text.y = element_text(size=28,colour="white",hjust=1),
      axis.ticks =  element_line(colour = "white"), 
      axis.title.x= element_text(size=42,colour="white"),
      axis.title.y= element_text(size=42,angle=90,colour="white"),
      #legend.position = "none", 
      panel.background = element_rect(fill="black"), 
      panel.border =element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.margin = unit(1.0, "lines"), 
      plot.background = element_rect(fill="black"), 
      plot.title =element_text(size=28,colour="white"), 
      plot.margin = unit(c(1,  1, 1, 1), "lines"),
      axis.line = element_line(colour = "white"),
      legend.background=element_rect(fill='black'),
      legend.title=element_text(size=28,colour="white"),
      legend.text=element_text(size=28,colour="white"),
      legend.key = element_rect( fill = 'black'),
      legend.key.size = unit(c(1, 1), "lines")
    )
}
