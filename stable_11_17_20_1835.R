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

# Define UI for application
ui <- fluidPage(
    
    # Navigation Bar at the top of the Shiny app
    navbarPage(title = span("Machine Learning Algorithms in Action", style = "color: #532d8e; font-family: Futura; font-size: 25px"), id = NULL,
               
               # tab option 1: Number Recognition
               # mini-project which accepts user-drawn numbers and displays the number as a text output
               tabPanel(title = "Number Recognition",
                        h4("Click on the plot to start drawing, click again to pause"),
                        sliderInput("mywidth", "width of the pencil", min = 1, max = 30, step = 1, value = 10),
                        actionButton(inputId = "reset", label = "reset", style = "background-color: #ff0000"),
                        actionButton(inputId = "send", label = "send", style = "background-color: #00ff00"),
                        plotOutput("plot", width = "500px", height = "500px",
                                   hover = hoverOpts(id = "hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                   click = "click")),
               # tab option 2: Diabetes
               # mini-project which looks at the 
               tabPanel(title = "Diabetes",
                        actionButton("test", "test")),
               
               # tab option 3: About
               # page which displays information about the app
               tabPanel("About", renderText(h4(("hello"))))
    )
)


# Define server logic required
server <- function(input, output) {
    
    library(imager)
    im <- load.example('parrots') %>% grayscale
    
    vals = reactiveValues(x = NULL, y = NULL)
    draw = reactiveVal(FALSE)
    
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
        
        plot(x = vals$x, y = vals$y, type = "l")
        
        dev.off()
        #add image processing + prediction here + show result + agick to trim the image, blur, and re-size.
    })
    
    observeEvent(input$hover, {
        if (draw()) {
            vals$x <- c(vals$x, input$hover$x)
            vals$y <- c(vals$y, input$hover$y)
        }})
    output$plot= renderPlot({
        plot(x = vals$x, y = vals$y, xlim = c(0, 28), ylim = c(0, 28), ylab = "y", xlab = "x", type = "l", lwd = input$mywidth)
        #cat("X value: ", vals$x, "Y Value: ", vals$y, "\n")
    })}
#output$values= renderText({
#print("Test string")
#})

# Run the application 
shinyApp(ui = ui, server = server)