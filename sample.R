library(magick)
library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
#import image----------------
#please upload image with equal width and height or it will automatically pick the center square
img1 <- image_read("https://www.shutterstock.com/blog/wp-content/uploads/sites/5/2014/11/img185.jpg")
img1_info <- image_info(img1)
#crop image
#boost saturation of image ----------------
img_modify <- image_modulate(img1, brightness = 100, saturation = 175, hue = 100)
#sharpen
img_modify <- image_contrast(img_modify,sharpen = 1)
#blur
img_modify <- image_blur(img_modify,radius = 1, sigma = 1)
#choose color template
map_pallate <- image_read("www/palette/10.png")
#map color
img_modify <- image_map(img_modify,map = map_pallate)

#pattern 2 from edge ------------------------
width_height_ratio <- img1_info$width/img1_info$height

shrink_ratio <- 0.10
shrinked_width <- round(img1_info$width*shrink_ratio)
target_size <- str_c(shrinked_width,"x",round(shrinked_width/width_height_ratio))
output_size <- str_c(img1_info$width,"x",img1_info$height)

image_scale(image_scale(img_modify,target_size),output_size)

