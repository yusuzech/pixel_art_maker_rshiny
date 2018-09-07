#get color palette from https://lospec.com/palette-list
library(RSelenium)
library(rvest)
library(readr)
library(purrr)
library(stringr)
library(ggplot2)
library(dplyr)
my_driver <- rsDriver(port = 4567L,browser = "chrome")

my_client <- my_driver$client

my_client$navigate("https://lospec.com/palette-list")

page_size <- 0
i <- 1
my_flag <- TRUE
while(my_flag & i <= 30){
    my_client$sendKeysToActiveElement(sendKeys = list(key="end"))
    current_size <- as.numeric(object.size(my_client$getPageSource()))
    if(current_size > page_size){
        my_flag <- TRUE
        page_size <- current_size
    } else {
        page_size <- FALSE
    }
    i <- i + 1
    Sys.sleep(1)
}


#click all download button once 
# my_elements <- my_client$findElements(using = "css",value = ".download")
# for(my_ele in my_elements){
#     my_ele$clickElement()
#     Sys.sleep(1)
# }

page_source <- read_html(my_client$getPageSource()[[1]])

palette_name <- page_source %>%
    html_nodes("h1 a") %>%
    html_text()

palette_n_color <- page_source %>%
    html_nodes(".palette-specs li:nth-child(1)") %>%
    html_text() %>%
    parse_number()

palette_popularity <- page_source %>%
    html_nodes(".download-count") %>%
    html_text() %>%
    parse_number() %>%
    desc() %>%
    rank() %>%
    as.integer()

all_palette <- page_source %>%
    html_nodes("article .palette")

all_palette_color <- map(all_palette,~html_nodes(.x,".color") %>% 
                             html_attr("style") %>% 
                             str_extract("(?<=background:).*(?=; min)") %>%
                             as.matrix() %>% 
                             t())
#function to create ggplot of palette
palette_create <- function(color_vec){
    n_color <- length(color_vec)
    data.frame(x = as.factor(1:n_color),y=rep(1,n_color)) %>% 
        ggplot(mapping = aes(x = x,y = y,fill=x)) + 
        geom_col(width=1) + 
        coord_fixed() + 
        scale_fill_manual(values = color_vec) +
        guides(fill = FALSE) + 
        theme_void() +
        theme(plot.margin=unit(c(0,0,0,0), "cm"),
              panel.spacing=unit(c(0,0,0,0), "cm"))
}

#convert ggplot to palette with png format
ggplot_to_png_palette <- function(ggplot_obj,image_name,path=""){
    ggsave(filename = "temp.png",plot = ggplot_obj)
    file_fullname <- str_c(path,image_name,".png")
    inport_img <- image_trim(image_read("temp.png"))
    img_info <- image_info(inport_img)
    imageaspect_ratio <- img_info$height/img_info$width
    export_img_height <- 600
    export_img <- image_scale(inport_img,str_c(export_img_height,"x",export_img_height*imageaspect_ratio))
    image_write(export_img,path = file_fullname)
}

palette_img <- map(all_palette_color,palette_create)

#create a data frame to store information
#check this website for showcase
infor_frame <- data.frame(index=1:length(palette_name),
                          palette_name=palette_name,
                          number_of_color=palette_n_color,
                          palette_rank=palette_popularity)

walk2(palette_img,infor_frame$index,~ggplot_to_png_palette(.x,.y,path = "www/palette/"))

info_frame <- infor_frame %>%
    mutate(img_path=str_c("www/palette/",index,".png"))
write_csv(infor_frame,"info_frame.csv")