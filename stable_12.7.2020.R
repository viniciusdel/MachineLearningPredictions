#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# load the shiny package
library(shiny)

# install.packages("shinythemes")
# load the shinythemes package
library(shinythemes)

# install.packages("magick")
library(magick)

# Define UI for application
ui <- fluidPage(
    
    # Navigation Bar at the top of the Shiny app
    navbarPage(title = span(strong("Machine Learning Algorithms in Action"), style = "color: #532d8e; font-size: 30px"), id = NULL, theme = shinytheme("readable"),
               
               # *************************
               # TAB 1: Number Recognition
               # mini-project which accepts user-drawn numbers and displays the number as a text output
               tabPanel(title = span(strong("Number Recognition"), style = "color: #000000"),
                        "This mini-project takes the user-drawn images and passes it to a machine learning algorithm which recognizes the number and displays it for the user.",
                        fluidRow(
                            column(width = 4,
                                   wellPanel(
                                       h4("Click on the plot to start drawing, click again to pause"),
                                       sliderInput(inputId = "mywidth", "width of the pencil", min = 10, max = 50, step = 1, value = 10),
                                       actionButton(inputId = "reset", label = "reset", style = "background-color: #ffce8a"),
                                       actionButton(inputId = "send", label = "send", style = "background-color: #adffad")
                                       ),
                                   
                                   plotOutput("plot", width = "400px", height = "500px", hover = hoverOpts(id = "hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE), click = "click")
                                   ),
                            
                            column(width = 8, allign = "center",
                                   wellPanel(
                                       h2("Your predicted number was: "),
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
               tabPanel(title = span(strong("Personal Medical Insurance Costs"), style = "color: #000000"),
                        "This mini-project takes the inputs from the user and passes them to a machine learning algorithm which decides what the likely cost of medical insurance will be for them.",
                   sidebarLayout(
                       sidebarPanel(width = 3,
                           verticalLayout(
                               h4("Please fill out the following fields:"),
                               numericInput(inputId = "ageInsurance", label = "Age, years", value = 18, min = 1, max = 101, step = 1, width = "150px"),
                               selectInput(inputId = "sex", label = "Sex, gender", choices = c("Male", "Female"), width = "150px"),
                               h5("Height:"),
                               wellPanel(
                                   numericInput(inputId = "feet", label = "ft", value = 5, min = 4, max = 8, step = 1, width = "150px"),
                                   numericInput(inputId = "inches", label = "in.", value = 11, min = 1, max = 11, step = 1, width = "150px")
                                   ),
                               numericInput(inputId = "weight", label = "Weight, lbs", value = 155, min = 85, max = 350, step = 1, width = "150px"),
                               numericInput(inputId = "children", label = "Children", value = 0, min = 0, max = 10, step = 1, width = "150px"),
                               radioButtons(inputId = "smoker", label = "Smoker?", choices = c("No" = "no", "Yes" = "yes")),
                               selectInput(inputId = "region", label = "Region in the US you currently reside in", choices = c("northeast", "southeast", "southwest", "northwest"), width = "150px"),
                               actionButton(inputId = "submitInsuranceForm", label = "submit")
                               )
                           ),
                       mainPanel(width = 9,
                                 verticalLayout(
                                     wellPanel(
                                         h1("Your calculated Medical Insurance Cost is:"),
                                         h1(textOutput("insurance_pred"), style = "color: #5CB8B2", br(), hr())
                                         )
                                     )
                                 )
                       ),
                   ),
               
               
               # *************************
               # TAB 3: Diabetes
               # mini-project which looks at the multiple factors that are considered when a person is figuring out whether or not they may have Diabetes
               tabPanel(title = span(strong("Diabetes"), style = "color: #000000"),
                        "This mini-project takes the inputs from the user and passes it to a machine learning algortihm which decides whether or not the user may have Diabetes.",
                        sidebarLayout(
                            sidebarPanel(
                                width = 6,
                                h3("Disclaimer", style = "color: #E0004D"),
                                h6("This result is not intended to be a substitute for professional medical advice, diagnosis, or treatment.
                                   Always seek the advice of your physician or other qualified health provider with any questions you may have regarding a medical condition.", style = "color: #E0004D"),
                                h4("Please fill out the form below:"),
                                splitLayout(
                                    verticalLayout(
                                        numericInput(inputId = "ageDiabetes", label = "Age, years", value = 18, min = 1, max = 101, step = 1, width = "100px"),
                                        radioButtons(inputId = "gender", label = "Gender", choices = c("Male" = "Male", "Female" = "Female")),
                                        radioButtons(inputId = "polurya", label = "Do you have Polurya?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "polydipsia", label = "Do you have Polydipsia?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "weightLoss", label = "Have you experienced sudden weight loss recently?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "weakness", label = "Do you experience weakness regularly?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "polyphagia", label = "Do you have Polyphagia?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "genitalThrush", label = "Do you have genital thrush?", choices = c("No" = "No", "Yes" = "Yes"))
                                    ),
                                    verticalLayout(
                                        radioButtons(inputId = "visualBlurring", label = "Do you experience blurred vision?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "icthing", label = "Do you experience itching?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "irritability", label = "Do you experience irritability?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "delayedHealing", label = "Do you experience delayed healing?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "partialParesis", label = "Do you have partial paresis?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "muscleStiffness", label = "Do you experience muscle stiffness?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "alopecia", label = "Do you have Alopecia?", choices = c("No" = "No", "Yes" = "Yes")),
                                        radioButtons(inputId = "obesity", label = "Do you consider yourself obese?", choices = c("No" = "No", "Yes" = "Yes"))
                                    )
                                ),
                                actionButton(inputId = "submitDiabetesForm", label = "submit"),
                            ),
                            mainPanel(width = 6,
                                verticalLayout(
                                    wellPanel(
                                        h1("Diabetes Test Results:"),
                                        h1(textOutput("diabetes_pred"), style = "color: #009FDF", br(), hr())
                                    )
                                )
                            )
                        )
                ),
               
               # *************************
               # TAB 4: About
               # page which displays information about the Shiny App
               tabPanel(title = span(strong("About"), style = "color: #000000"),
                        titlePanel(HTML("<strong> Applications of Machine Learning </strong>
                                        <br/r> COP5090: Scientific Computation and Programming <br/r>
                                        <br/r> <h2> Vinicius Seixas <br/r>
                                        <h4> Machine Learning Algorithms </h4>
                                        Numeric Recognition, Personal Medical Insurance Costs, Diabetes <br/r>
                                        <h2> Hunter Stopford </strong> <br/r>
                                        <h4> UI Layout </h4>
                                        Personal Medical Insurance Costs, Diabetes <br/r>
                                        <h2> Samuel de Oliveira <br/r>
                                        <h4> Data Visualization </h4>
                                        Numeric Recognition, Data Visualization <br/r><br/r>"),
                                   ),
                        wellPanel(
                            h4("The GitHub ", a(href = "https://github.com/viniciusdel/MachineLearningPredictions.git", "repository")," used for this Final Project"),
                            h4("The ", a(href = "http://yann.lecun.com/exdb/mnist/index.html", "dataset"), " used for the Number Recogniton"),
                            h4("The ", a(href = "https://www.kaggle.com/mirichoi0218/insurance", "dataset"), " used for the Personal Medical Insurance Costs"),
                            h4("The ", a(href = "https://archive.ics.uci.edu/ml/machine-learning-databases/00529/", "dataset")," used for the Diabetes", br(), hr())
                            )
                        )
               )
    )


# Define server logic required
server <- function(input, output) {
    
    # Restore the object RF trained 
    rf <- readRDS(file = "trained_models/rf_trained.rds")
    rf2 <- readRDS(file = "trained_models/diabetes_fit.rds")
    svm <- readRDS(file = "trained_models/insurance_fit.rds")
    
    confusionMatrx <- rf$confusion
    
    output$confusion <- renderTable({confusionMatrx})
    
    printImg <- function(x){
        
        # install.packages("RSEIS")
        library(RSEIS)
        
        # pick an image from the test poll
        number <- x
        
        # flip matrix
        m = matrix(number, nrow = 28, ncol = 28, byrow = FALSE)
        im_numbers <- apply(m, 2, as.numeric)
        im_numbers <- mirror.matrix(im_numbers)
        
        # show image
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
        # clear shown result
    })
    
    # Send button -> send the user-drawn number to the machine learning algorithm
    observeEvent(input$send, handlerExpr = {
        
        png(file = "myPlot.png",
            width = 500, height = 500)
        
        # Make line thicc
        plot(0:27, 0:27, lwd = 0.1, cex = 0, xlab = "", ylab = "", axes = FALSE)

        # `lwd` is set to the SliderInput size
        points(x = vals$x, y = vals$y, type = "l", lwd = input$mywidth)
    
        # save image
        dev.off()
        
        # reload image
        img <- image_read("myPlot.png")
        
        # resize to 28x28
        img <- image_resize(img, "28x28")
        
        # save image
        image_write(img, path = "myPlot.png", format = "png", quality = 100)
        
        # grayscale
        img <- image_convert(img, type = 'Grayscale')
        
        # convert to grayscale
        tiffImage <- image_convert(img, "tiff")
        
        # Access data in raw format and convert to integer (it's in RAW)
        vec <- as.vector(tiffImage[[1]])
        
        # convert to ascii
        vec <- rawToChar(vec, multiple = TRUE)
        
        library(gtools)
        
        # convert ascii to its code
        for(i in 1:784){ vec[i] <- asc(vec[i])}
        
        # need to flip these colors
        for(i in 1:784){ vec[i] <- 255 - as.numeric(vec[i])}
        
        vec[is.na(vec)] <- 255
        
        library(randomForest)        
        
        # make prediction
        pred <- predict(object = rf, newdata = vec, type = "response")
        
        output$my_pred <- renderText({as.numeric(pred) - 1})
        
        confusionMatrx <- rf$confusion
        
        # plot(rf$confusion)
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
    })
    
    
    # *************************
    # TAB 2
    # Submit button -> send all the inputs to the machine learning algorithm
    observeEvent(input$submitInsuranceForm, handlerExpr = {
        
        # the values: ageInsurance, sex, bmiMetric, children, smoker, and region will be sent to the machine learning algorithm
        input$ageInsurance
        input$sex
        
        # converting BMI from Imperial units to Metric units
        heightFeetToInches <- input$feet * 12
        heightTotalInches <- heightFeetToInches + input$inches
        heightTotalMeters <- heightTotalInches * 2.54 * (1/100)

        weightKilograms <- input$weight * 0.453592

        bmiMetric <- weightKilograms / (heightTotalMeters^2)
        
        input$children
        input$smoker
        input$region
        
        person <- list(age = input$ageInsurance, sex = "female", bmi = bmiMetric, children = input$children, smoker = input$smoker, region = input$region)
        
        # make prediction on price
        insurance_pred <- predict(svm, newdata = person)
        
        # send output prediction to UI
        output$insurance_pred <- renderText({paste0("$", format(as.numeric(insurance_pred), digits = 7))})
    })
    
    
    # *************************
    # TAB 3
    # Submit button -> send all the inputs to the machine learning algorithm
    
    # Restore the object diabetes_fit.rds
    rfDiabetes <- readRDS(file = "trained_models/diabetes_fit.rds")
    
    observeEvent(input$submitDiabetesForm, handlerExpr = {
        
        input$ageDiabetes
        input$gender
        input$polurya
        input$polydipsia
        input$weightLoss
        input$weakness
        input$polyphagia
        input$genitalThrush
        input$visualBlurring
        input$icthing
        input$irritability
        input$delayedHealing
        input$partialParesis
        input$muscleStiffness
        input$alopecia
        input$obesity
        
        # replace the hardcoded values here with the gathered INPUTS
        person2 <- list(Age = input$ageDiabetes,
                        Gender = input$gender,
                        Polurya = input$polurya,
                        Polydipsia = input$polydipsia,
                        suddenWeightLoss = input$weightLoss,
                        Weakness = input$weakness,
                        Polyphagia = input$polyphagia,
                        GenitalThrush = input$genitalThrush, 
                        visualBlurring = input$visualBlurring,
                        Itching = input$icthing,
                        Irritability = input$irritability, 
                        DelayedHealing = input$delayedHealing,
                        PartialParesis = input$partialParesis,
                        MuscleStiffness = input$muscleStiffness,
                        Alopecia = input$alopecia,
                        Obesity = input$obesity)
        
        diabetes_pred <- predict(rf2, person2)
        
        diabetes_pred <- as.numeric(diabetes_pred)
        
        # output prediction
        diagnosis <- "Unsure"
        
        if(diabetes_pred == 1){
            
            diagnosis <- "Negative"
            
        } else if (diabetes_pred == 2){
            
            diagnosis <- "Positive"
        }
        
        # send output prediction to UI
        output$diabetes_pred <- renderText({diagnosis})
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)