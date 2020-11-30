#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load the shiny package
library(shiny)

#install.packages("magick")
library(magick)

# Define UI for application
ui <- fluidPage(
    
    # Navigation Bar at the top of the Shiny app
    navbarPage(title = span("Machine Learning Algorithms in Action", style = "color: #532d8e; font-family: Futura; font-size: 25px"), id = NULL,
               
               # *************************
               # TAB 1: Number Recognition
               # mini-project which accepts user-drawn numbers and displays the number as a text output
               tabPanel(title = "Number Recognition",
                        h4("Click on the plot to start drawing, click again to pause"),
                        sliderInput("mywidth", "width of the pencil", min = 1, max = 30, step = 1, value = 10),
                        actionButton(inputId = "reset", label = "reset", style = "background-color: #ff0000"),
                        actionButton(inputId = "send", label = "send", style = "background-color: #00ff00"),
                        plotOutput("plot", width = "500px", height = "500px",
                                   hover = hoverOpts(id = "hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),click = "click")),
               
               # *************************
               # TAB 2: Personal Medical Insurance Costs
               # mini-project which looks at the multiple factors that are considered when a person registers for Medical Insurance
               tabPanel(title = "Personal Medical Insurance Costs",
                   sidebarLayout(
                       sidebarPanel(
                           width = 3,
                           h4("Please fill out the following fields:"),
                           numericInput(inputId = "age", label = "Age, years", value = 1, min = 1, max = 101, step = 1, width = "150px"),
                           selectInput(inputId = "sex", label = "Sex, gender", choices = c("Male", "Female"), width = "150px"),
                           h5("Height:"),
                           
                           wellPanel(
                               splitLayout(
                                   numericInput(inputId = "feet", label = "ft", value = 5, min = 4, max = 8, step = 1, width = "150px"),
                                   numericInput(inputId = "inches", label = "in.", value = 11, min = 1, max = 11, step = 1, width = "150px"), cellWidths = "150px"),
                               ),
                           numericInput(inputId = "children", label = "Children", value = 0, min = 0, max = 10, step = 1, width = "150px"),
                           radioButtons(inputId = "smoker", label = "Smoker?", choices = c("No" = "no", "Yes" = "yes")),
                           selectInput(inputId = "region", label = "Region you currently reside in the US", choices = c("northeast", "southeast", "southwest", "northwest"), width = "150px"),
                           actionButton(inputId = "submit", label = "submit", style = "background-color: #DCDCDC")
                       ),
                   mainPanel() # Plots will go inside here
                   ),
                   
               ),
               
               # *************************
               # TAB 3: Diabetes
               # page which displays information about the app
               tabPanel("Diabetes",
                        h4("This project...")),
               
               # *************************
               # TAB 4: About
               # page which displays information about the app
               tabPanel("About",
                        h4("hello"))
    )
)


# Define server logic required
server <- function(input, output) {
    
    # *************************
    # TAB 1
    library(imager)
    im <- load.example('parrots') %>% grayscale
    
    vals = reactiveValues(x = NULL, y = NULL)
    draw = reactiveVal(FALSE)
    
    # Drawing on the plot
    observeEvent(input$click, handlerExpr = {
        temp <- draw(); draw(!temp)
        if(!draw()) {
            vals$x <- c(vals$x, NA)
            vals$y <- c(vals$y, NA)
        }})
    
    # Reset button -> Clear the plot
    observeEvent(input$reset, handlerExpr = {
        vals$x <- NULL; vals$y <- NULL
        #clear shown result
    })
    
    # Send button -> send the user-drawn number to the machine learning algorithm
    observeEvent(input$send, handlerExpr = {
        #vals$x <- NULL; vals$y <- NULL
        png(file="myPlot.png",
            width=500, height=500)
        
        
        #make line thicc
        plot(x = vals$x, y = vals$y, type = "l", lwd = 6)
        
        #save image
        dev.off()
        
        #reload image
        img <- image_read("myPlot.png")
        
        #crop sides
        margin <- geometry_area(x = 70, y = 70, width = 390, height = 350)
        
        #crop sides
        img <- image_crop(img, margin)
        
        #resize to 28x28
        img <- image_resize(img, "28x28!")
        
        #save image
        image_write(img, path = "myPlot.png", format = "png", quality = 75)
        
        #grayscale
        img <- image_convert(img, type = 'Grayscale')
        
        #convert to grayscale
        tiffImage <- image_convert(img, "tiff")
        
        # Access data in raw format and convert to integer (it's in HEX)
        vec <- as.vector(tiffImage[[1]])
        
        #TRYING TO CONVERT TO INTEGERS FROM 0-255
        cat(length(vec))
        cat(vec)
    })
    
    observeEvent(input$hover, {
        if (draw()) {
            vals$x <- c(vals$x, input$hover$x)
            vals$y <- c(vals$y, input$hover$y)
        }})
    output$plot = renderPlot({
        plot(x = vals$x, y = vals$y, xlim = c(0, 28), ylim = c(0, 28), ylab = "y", xlab = "x", type = "l", lwd = input$mywidth)
        #cat("X value: ", vals$x, "Y Value: ", vals$y, "\n")
    })
    
    # *************************
    # TAB 2
    # submit button -> send all the inputs to the machine learning algorithm
    observeEvent(input$submit, handlerExpr = {
        
        # calculating and rendering BMI
        output$bmi <- renderText({ c(input$feet, input$inches, input$weight)
            
            # converting BMI from Imperial units to Metric units
            heightFeetToInches <- input$feet * 12
            heightTotalInches <- heightFeetToInches + input$inches
            heightTotalMeters <- heightTotalInches * 2.54 * (1/100)
            
            weightKilograms <- input$weight * 0.453592
            
            bmiMetric <- weightKilograms / (heightTotalMeters^2)
            substring(bmiMetric, 1, 4)      # trim the amount of digits to 00.0
            })
        # these values will be sent to the machine learning algorithm
        # output$age
        # output$sex
        # output$children
        # output$smoker
        # output$region
    })
    
    }

#output$values= renderText({
#print("Test string")
#})

# Run the application 
shinyApp(ui = ui, server = server)