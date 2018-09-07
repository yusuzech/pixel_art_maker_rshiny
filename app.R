library(shiny)
library(magick)
library(dplyr)
library(stringr)

info_frame <- read.csv("info_frame.csv",stringsAsFactors = F)


ui <- fluidPage(
   
   # Application title
   titlePanel("Pixel Art Converter with Shiny"),
   
   sidebarLayout(
       
       sidebarPanel(width = 4,
           #Input
           fileInput(inputId = "input_image",
                     label = "Upload Your Image",
                     placeholder = "Upload your image",
                     accept = c('image/png','image/jpeg')),
           # textInput(inputId = "image_url",
           #           label = "Or Input Your Image URL",
           #           placeholder = "Image Url"),
           #ui to control shrinkage
           sliderInput(inputId = "shrinkage",
                       label = "% Pixels",
                       min = 0,
                       max = 100,
                       step = 1,
                       post = "%",
                       value = 10),
           #display image size
           tableOutput("sizes"),
           #palette
           selectInput(inputId = "palette",
                       label = "Palette",
                       choices = info_frame$palette_name,
                       selected = "Endesga 32"
           ),
           div("Visit",
                   tags$a("https://lospec.com/palette-list",href="https://lospec.com/palette-list"),"to see details and showcases for all palettes"),
           #brightness
           sliderInput(inputId = "brightness",
                       label = "Brightness",
                        min = 0,
                        max = 200,
                        value = 100,
                        step = 1),
           #saturation
           sliderInput(inputId = "saturation",
                       label = "Saturation",
                       min = 0,
                       max = 200,
                       value = 150,
                       step = 1),
           #hue
           sliderInput(inputId = "hue",
                       label = "Hue",
                       min = 0,
                       max = 200,
                       value = 100,
                       step = 1),
           #Sharpen
           sliderInput(inputId = "sharpen",
                       label = "Sharpen",
                       min = 0,
                       max = 2,
                       value = 1.5,
                       step = 0.01),
           #blur radius
           sliderInput(inputId = "blur_radius",
                       label = "Blur Radius",
                       min = 0,
                       max = 2,
                       value = 1,
                       step = 0.01),
           #blur sigma
           sliderInput(inputId = "blur_sigma",
                       label = "Blur Sigma",
                       min = 0,
                       max = 200,
                       value = 100,
                       step = 1)
           ),
       
       mainPanel(downloadButton("download_image","Download Image"),
                 imageOutput("output_image")
                 # tags$br(),
                 # tags$h4("Current Palette"),
                 # imageOutput("my_palette"))
   )
   
))

server <- function(input, output) {
    #observe upload button or URL input box ------------
    image <- eventReactive(input$input_image,
                           image_convert(image_read(input$input_image$datapath),format = "png"))
    # image <- eventReactive(input$image_url,
    #                        image_convert(image_read(input$image_url),format = "png"))
    #get image info once uploaded
    your_image_info <- eventReactive(input$input_image,
                                image_info(image()))
    #get configuration for pixelization
    image_size <- reactive(str_c(your_image_info()$width,"x",your_image_info()$height))
    target_width <- eventReactive(input$shrinkage,
                                  round(your_image_info()$width*input$shrinkage/100))
    target_height <- eventReactive(input$shrinkage,
                                  round(your_image_info()$height*input$shrinkage/100))
    target_size <- reactive(str_c(target_width(),"x",target_height()))
    
    output$sizes <- renderTable({
        req(input$input_image)
        return(data.frame("Original Size"=image_size(),
                   "Converted Size"=target_size()))
        })
    
    map_palette_path <- eventReactive(input$palette,
                                      info_frame %>%
                                          dplyr::filter(palette_name == input$palette) %>%
                                          pull(img_path))
    map_palette <- reactive(image_read(map_palette_path()))
    output$my_palette <- renderImage({
        tempfile1 <- reactive(image_write(map_palette(),tempfile(fileext = "png")))
        return(list(src=tempfile1(),contentType="image/png"))
    })
    
    modified_image <- reactive(image() %>%
                                        image_modulate(brightness = input$brightness,
                                                       saturation = input$saturation,
                                                       hue = input$hue) %>%
                                        image_contrast(sharpen = input$sharpen) %>%
                                        image_blur(radius = input$blur_radius,
                                                   sigma = input$blur_sigma) %>%
                                        image_map(map = map_palette()) %>%
                                        image_scale(target_size()) %>%
                                        image_scale(image_size())
                                    )
    
    output$output_image <- renderImage({
        tmpfile <- reactive(image_write(modified_image(),tempfile(fileext = "png")))
        return(list(src=tmpfile(),contentType="image/png"))
        })
    
    output$download_image <- downloadHandler(
        filename = function(){
            return(str_c("pixel_",input$input_image$name))
            },
        content = function(file){
            return(image_write(modified_image(),file))
            },
        contentType = "image/png")
}

# Run the application 
shinyApp(ui = ui, server = server)

