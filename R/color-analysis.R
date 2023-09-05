#' Analyze Color in an Image
#' 
#' Analyzes the colors of an image and returns overall mean along with means for the quadrants.
#' Reduces the color space to RBG.
#'
#' @param img either a "magick-image or character-vector (filename) 
#' @importFrom purrr map_df
#' @importFrom magick image_read
#' @importFrom stringr str_sub
#' @return a data.frame with variables
#' @export 

library(purrr)
library(stringr)

color_analysis <- function(img){
  
  # NOTE: Use magick to reduce to a 4x4 ? This way we get the quadrants
  
  
  
  .color_analysis <- function(img){
    .x <- img
    tmp <- as.raster(.x) %>% as.character()
    tmp <- str_sub(tmp,1,7) %>% 
      col2rgb()
    
    cols <- image_info(.x)$width # Length of q-line
    rows <- image_info(.x)$height #height of q-lines
    
    
    M <- matrix(rows,cols,data=1:(rows*cols))
    
    mid_row <- ceiling(rows / 2)
    mid_col <- ceiling(cols / 2)
    
    # q1 <- M[1:mid_row, 1:mid_col]
    # q2 <- M[1:mid_row, (mid_col + 1):cols]
    # q3 <- M[(mid_row + 1):rows, 1:mid_col]
    # q4 <- M[(mid_row + 1):rows, (mid_col + 1):cols]
    # 
    
    # Slices
    l <- length(M)
    q1=M[1:l/4]
    q2=M[(l/4):(l/2)]
    q3=M[l/2:(l*3/4)]
    q4=M[(l*3/4):l]
    
    th = c(q1,q2)
    lh = c(q2,q4)
    data.frame(
      R   = mean(tmp[1,]),
      G   = mean(tmp[2,]),
      B   = mean(tmp[3,]),
      R_S   = sd(tmp[1,]),
      G_S   = sd(tmp[2,]),
      B_S   = sd(tmp[3,]),
      
      
      Q1R = mean(tmp[1,q1]), 
      Q1G = mean(tmp[2,q1]), 
      Q1B = mean(tmp[3,q1]), 
      
      Q2R = mean(tmp[1,q2]), 
      Q2G = mean(tmp[2,q2]), 
      Q2B = mean(tmp[3,q2]),
      
      Q3R = mean(tmp[1,q3]), 
      Q3G = mean(tmp[2,q3]), 
      Q3B = mean(tmp[3,q3]), 
      
      Q4R = mean(tmp[1,q4]), 
      Q4G = mean(tmp[2,q4]), 
      Q4B = mean(tmp[3,q4]) 
      
    ) # Data frame
  }
  if("magick-image" %in% class(img)){
    return(.color_analysis(img))
  }
  purrr::map_df(c(img),function(.x){
    if(is.character(.x)){
      .x <- image_read(.x)
    }
    .color_analysis(.x)
  })
}
