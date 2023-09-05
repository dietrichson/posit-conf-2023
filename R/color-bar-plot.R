library(ggplot2)
library(dplyr)
library(tidyr)
color_bar_plot <- function(analysis){
  
  
  my_analysis <<- analysis
  
  my_analysis |> 
    select(R,G,B) |> 
    pivot_longer(c(R,B,G)) |> 
    ggplot(aes(name,value,fill = name))+
    geom_col()+
    scale_fill_manual(values = c(R="red",B="blue", G="green"))+
    coord_flip()+
    theme(legend.position = "none")
  
  # barplot(c(my_analysis$R,my_analysis$G, my_analysis$B),
  #         col = c("Red","Green","Blue"))
}

# image_read(my_files[5]) |> 
#   image_crop(geometry_area(300,300,100,50)) |> 
#   color_analysis() |> 
#   color_bar_plot()
