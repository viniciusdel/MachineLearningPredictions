#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("Sci Comp Projects", id = NULL,
               
    tabPanel("Numbers",
                h4("Click on plot to start drawing, click again to pause"),
                sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
                actionButton("reset", "reset"),
                actionButton("send", "send"),
                plotOutput("plot", width = "500px", height = "500px",
                           hover=hoverOpts(id = "hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                           click="click")),
    tabPanel("Testing",
             actionButton("test", "test"),
    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(imager)
    im <- load.example('parrots') %>% grayscale
    
    vals = reactiveValues(x=NULL, y=NULL)
    draw = reactiveVal(FALSE)
    observeEvent(input$click, handlerExpr = {
        temp <- draw(); draw(!temp)
        if(!draw()) {
            vals$x <- c(vals$x, NA)
            vals$y <- c(vals$y, NA)
        }})
    observeEvent(input$reset, handlerExpr = {
        vals$x <- NULL; vals$y <- NULL
    })
    observeEvent(input$send, handlerExpr = {
        vals$x <- NULL; vals$y <- NULL
    })
    observeEvent(input$hover, {
        if (draw()) {
            vals$x <- c(vals$x, input$hover$x)
            vals$y <- c(vals$y, input$hover$y)
        }})
    output$plot= renderPlot({
        plot(x=vals$x, y=vals$y, xlim=c(0, 28), ylim=c(0, 28), ylab="y", xlab="x", type="l", lwd=input$mywidth)
        #cat("X value: ", vals$x, "Y Value: ", vals$y, "\n")
    })}
    #output$values= renderText({
       #print("Test string")
    #})

# Run the application 
shinyApp(ui = ui, server = server)
