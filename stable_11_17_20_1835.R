#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#TEST 12-6-20 3:10pm

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
                        
                        #include description here
                   fluidRow(
                       column(width = 4,
                           wellPanel(
                               h4("Click on the plot to start drawing, click again to pause"),
                               sliderInput(inputId = "mywidth", "width of the pencil", min = 10, max = 50, step = 1, value = 10),
                               actionButton(inputId = "reset", label = "reset", style = "background-color: #ffce8a"),
                               actionButton(inputId = "send", label = "send", style = "background-color: #adffad")
                           ),
                           
                           plotOutput("plot", width = "400px", height = "500px",
                                      hover = hoverOpts(id = "hover", delay = 100,
                                                        delayType = "throttle", clip = TRUE, nullOutside = TRUE), click = "click"),
                           
                       ),
                       
                       column(width = 6, allign = "center",
                            wellPanel(
                                h4("Your predicted number was: "),
                                h1(textOutput("my_pred"))
                            ),
                            
                            wellPanel(
                                h2("Confusion Matrix: "),
                                tableOutput("confusion")
                            )
                        )
                   )
                       
                   
                   
                ),
               
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
                           numericInput(inputId = "weight", label = "Weight, lbs", value = 155, min = 85, max = 350, step = 1, width = "150px"),
                           numericInput(inputId = "children", label = "Children", value = 0, min = 0, max = 10, step = 1, width = "150px"),
                           radioButtons(inputId = "smoker", label = "Smoker?", choices = c("No" = "no", "Yes" = "yes")),
                           selectInput(inputId = "region", label = "Region you currently reside in the US", choices = c("northeast", "southeast", "southwest", "northwest"), width = "150px"),
                           actionButton(inputId = "submit", label = "submit", style = "background-color: #DCDCDC")
                       ),
                   mainPanel(h5("BMI of individual:"),
                       textOutput(outputId = "bmi")) # Plots will go inside here
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
                        titlePanel(HTML("<strong>Applications of Machine Learning </strong><br/>COP5090: Scientific Computation and Programming<br/r><br/r> Vinicius Seixas <br/r>Hunter Stopford <br/r>Samuel de Oliveira")),
                        mainPanel(HTML("<strong>Number Recognition</strong><br/r>")))
    )
)


# Define server logic required
server <- function(input, output) {
    
    # Restore the object RF trained 
    rf <- readRDS(file = "rf_trained.rds")
    
    confusionMatrx <- rf$confusion
    
    output$confusion <- renderTable({confusionMatrx})
    
    trainmodels <- function(){
        # read in data
        library(readr)
        
        #read training data
        train_orig <- read_csv("train.csv")
        
        #read testing data
        test_orig <- read_csv("test.csv")
        
        # save the training labels
        train_orig_labels <- train_orig[, 1]
        
        #convert it to factor (for classification)
        train_orig_labels <- as.factor(train_orig_labels$label)
        
        #install.packages("randomForest")
        library(randomForest)
        numTrees <- 25
        
        # Train on entire training dataset and predict on the test

        cat("\n Traning model... \n")
        
        rf <- randomForest(train_orig[-1], train_orig_labels,
                           xtest=test_orig, ntree=numTrees, keep.forest=TRUE)
        
        # Save an object to a file
        saveRDS(rf, file = "rf_trained.rds")
        
        cat("\n Done training! \n")
    }
    
    printImg <- function(x){
        #install.packages("RSEIS")
        library(RSEIS)
        
        #pick an image from the test poll
        number <- x
        
        #flip matrix
        m = matrix(number, nrow = 28, ncol = 28, byrow = FALSE)
        im_numbers <- apply(m, 2, as.numeric)
        im_numbers <- mirror.matrix(im_numbers)
        
        #show image
        image(1:28, 1:28, im_numbers, col=gray((0:255)/255))
    }
    
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
        plot(0:27, 0:27, lwd = 0.1, cex = 0, xlab = "", ylab = "", axes = FALSE)

        #points(x = vals$x, y = vals$y, type = "l", lwd = 50)

        #lwd is set to the SliderInput size
        points(x = vals$x, y = vals$y, type = "l", lwd = input$mywidth)
    
        #save image
        dev.off()
        
        #reload image
        img <- image_read("myPlot.png")
        
        #resize to 28x28
        img <- image_resize(img, "28x28")
        
        #save image
        image_write(img, path = "myPlot.png", format = "png", quality = 100)
        
        #grayscale
        img <- image_convert(img, type = 'Grayscale')
        
        #convert to grayscale
        tiffImage <- image_convert(img, "tiff")
        
        # Access data in raw format and convert to integer (it's in RAW)
        vec <- as.vector(tiffImage[[1]])
        
        # convert o ascii
        vec <- rawToChar(vec, multiple = TRUE)
        
        library(gtools)
        
        #convert ascii to its code
        for(i in 1:784){ vec[i] <- asc(vec[i])}
        
        #need to flip these colors
        for(i in 1:784){ vec[i] <- 255 - as.numeric(vec[i])}
        
        vec[is.na(vec)] <- 255
        
        library(randomForest)        
        
        # Restore the object RF trained 
        rf <- readRDS(file = "rf_trained.rds")
        
        #make prediction
        pred <- predict(object = rf, newdata = vec, type= "response")
        
        output$my_pred <- renderText({as.numeric(pred) - 1})
        
        confusionMatrx <- rf$confusion
        
        #plot(rf$confusion)
        
        output$confusion <- renderTable({confusionMatrx})
        
        printImg(vec)
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