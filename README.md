# Make Pixel Art with R Shiny!

This app makes use of `magick` library to convert any images into pixel art. 

**Demo available at  https://yifyan-yusuzech.shinyapps.io/pixcel_art_converter_shiny/**

**Or run this app locally:**

```r
library(shiny)
library(magick)
library(dplyr)
library(stringr)
runGitHub("yusuzech/pixcel_art_converter_shiny")
```

#### App

Decide your configuration and start making pixel art.  
Besides adjusting saturation,brightness, etc. There are also more than [200 palettes](https://lospec.com/palette-list) to choose from.

<img src="github/demo_img.PNG" width="80%" >

Below is an example to convert a pineapple to pixel pineapple.

<img src="github/pineapple.jpg" width="40%" ><img src="github/pixel_pineapple.jpg" width="40%" >
