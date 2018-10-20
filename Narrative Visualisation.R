library(openxlsx)
library(leaflet)
library(ggplot2)
library(shiny)

# import data from xlsx files
rainfall <- read.xlsx("monthly-rainfall.xlsx")
coordinate <- read.xlsx("coordinate.xlsx")
average <- read.xlsx("average-rainfall.xlsx")
wettest <- read.xlsx("wettest-rainfall.xlsx")
driest <- read.xlsx("driest-rainfall.xlsx")

ui <- fluidPage(
  # define the tile of this visualisation
  titlePanel("Monthly Rainfall Data of Water Storage Areas in Melbourne"),
  br(),
  sidebarLayout(
    sidebarPanel(
      # display water storage areas and collect viewers' selections 
      selectInput(inputId = "WaterStorageArea",
                  label = strong("Please choose a water storage area:"),
                  choices = unique(rainfall$WaterStorageArea)),
      # viewers can decide to show smoother choices or not
      checkboxInput(inputId = "smoother",
                    label = strong("Show smoother choices"),
                    value = FALSE),
      # display smoother choices and catch viewers' selections
      conditionalPanel(condition = "input.smoother == true",
                       sliderInput(inputId = "smooth", 
                                   label = "Please choose a smoother:",
                                   min = 1,
                                   max = 4,
                                   value = 4)),
      # show the picture
      fluidRow(
        column(8,
               h5("Picture of this water storage area:"),
               imageOutput("myImage"))),
      h5("Map: locations and brief description of water storage areas"),
      # show the map
      leafletOutput("mymap")),
    
    mainPanel(
      # show header
      h3(textOutput("caption0")),
      h4(
        # show header
        textOutput("caption1"),
        # show plots of water storage areas
        plotOutput("distPlot")),
      p(
        # show calculated values of water storage areas
        textOutput("caption2"),
        textOutput("caption3"),
        textOutput("caption4"),
        textOutput("caption5"),
        textOutput("caption6"),
        textOutput("caption7"),
        textOutput("caption8"),
        textOutput("caption9"),
        textOutput("caption10"),
        textOutput("caption11")),
      h3(textOutput("caption12")),
      
      # show plots of rainfall types
      fluidRow(
        column(width = 10,
               plotOutput("plot1", height = 260,
                          click = "plot1_click"))),
      fluidRow(
        column(width = 8,
               h4(" Average monthly rainfall of water storage areas"),
               verbatimTextOutput("click_info1"))),
      fluidRow(
        column(width = 10,
               plotOutput("plot2", height = 260,
                          click = "plot2_click"))),
      fluidRow(
        column(width = 8,
               h4(" Wettest monthly rainfall of water storage areas"),
               verbatimTextOutput("click_info2"))),
      fluidRow(
        column(width = 10,
               plotOutput("plot3", height = 260,
                          click = "plot3_click"))),
      fluidRow(
        column(width = 8,
               h4("Driest monthly rainfall of water storage areas"),
               verbatimTextOutput("click_info3")))
      )))

server <- function(input, output) {
  
  # define the algorithm to render picture of water storage areas
  output$myImage <- renderImage({
    
    if (is.null(input$WaterStorageArea))
      return(NULL)
    if (input$WaterStorageArea == "Cardinia Reservoir") {
      return(list(
        src = "images/Cardinia.jpg",
        filetype = "image/jpeg",
        alt = "Cardinia Reservoir"
      ))
    } else if (input$WaterStorageArea == "Greenvale Reservoir") {
      return(list(
        src = "images/Greenvale.jpg",
        filetype = "image/jpeg",
        alt = "Greenvale Reservoir"
      ))
    } else if (input$WaterStorageArea == "Maroondah Reservoir") {
      return(list(
        src = "images/Maroondah.jpg",
        filetype = "image/jpeg",
        alt = "Maroondah Reservoir"
      ))
    } else if (input$WaterStorageArea == "O'Shannassy Reservoir") {
      return(list(
        src = "images/O'Shannassy.jpg",
        filetype = "image/jpeg",
        alt = "O'Shannassy Reservoir"
      ))
    } else if (input$WaterStorageArea == "Silvan Reservoir") {
      return(list(
        src = "images/Silvan.jpg",
        filetype = "image/jpeg",
        alt = "Silvan Reservoir"
      ))
    } else if (input$WaterStorageArea == "Sugarloaf Reservoir") {
      return(list(
        src = "images/Sugarloaf.jpg",
        filetype = "image/jpeg",
        alt = "Sugarloaf Reservoir"
      ))
    } else if (input$WaterStorageArea == "Tarago Reservoir") {
      return(list(
        src = "images/Tarago.jpg",
        filetype = "image/jpeg",
        alt = "Tarago Reservoir"
      ))
    } else if (input$WaterStorageArea == "Thomson Reservoir") {
      return(list(
        src = "images/Thomson.jpg",
        filetype = "image/jpeg",
        alt = "Thomson Reservoir"
      ))
    } else if (input$WaterStorageArea == "Upper Yarra Reservoir") {
      return(list(
        src = "images/Upper Yarra.jpg",
        filetype = "image/jpeg",
        alt = "Upper Yarra Reservoir"
      ))
    } else if (input$WaterStorageArea == "Yan Yean Reservoir") {
      return(list(
        src = "images/Yan Yean.jpg",
        filetype = "image/jpeg",
        alt = "Yan Yean Reservoir"
      ))
    }
    }, deleteFile = FALSE)
  
  # define the algorithm to render the map
  output$mymap <- renderLeaflet({ 
    map <- leaflet(data = coordinate) %>%
      addTiles() %>%
      addMarkers(
        ~unique(longitude),
        ~unique(latitude),
        popup = ~unique(Description),
        label = ~unique(WaterStorageArea)
      )
  })
  
  # define the algorithm to render plots of water storage areas
  output$distPlot <- renderPlot({
    ggplot(rainfall[rainfall$WaterStorageArea == input$WaterStorageArea,], aes(Month, Amount)) + 
      geom_histogram(stat = "identity", aes(fill = WaterStorageArea), fill = "lightblue", colour = "black") +
      geom_smooth(aes(group = 1),
                  method = "glm",
                  color = "red",
                  formula = y~ poly(x,input$smooth)) +
      facet_grid(input$WaterStorageArea~Type)
  })
  
  # define the algorithm to render header
  formulaText0 <- reactive({
    paste("Show monthly rainfall data by water storage areas:")
  })
  output$caption0 <- renderText({formulaText0()})
  
  # define the algorithm to render header
  formulaText1 <- reactive({
    paste("Monthly rainfall data of", input$WaterStorageArea)
  })
  output$caption1 <- renderText({formulaText1()})
  
  # define the algorithm to render calculated values of water storage areas
  formulaText2 <- reactive({
    if(input$WaterStorageArea == "Cardinia Reservoir")
      paste("Average monthly rainfall: Mean amount: 82.51667, Maximum amount: 99.9, 
            Minimum amount: 54.7, Standard Deviation: 14.65804, Variance: 214.85806.",
            "Driest monthly rainfall: Mean amount: 20.31667, Maximum amount: 38.2, 
            Minimum amount: 2, Standard Deviation: 10.42, Variance: 108.57639.",
            "Wettest monthly rainfall: Mean amount: 183.25, Maximum amount: 246.4, 
            Minimum amount: 120.2, Standard Deviation: 32.97546, Variance: 1087.38083.",
            "Therefore, in Cardinia Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption2 <- renderText({formulaText2()})
  
  formulaText3 <- reactive({
    if(input$WaterStorageArea == "Greenvale Reservoir")
      paste("Average monthly rainfall: Mean amount: 50.25, Maximum amount: 63.9, 
            Minimum amount: 42.5, Standard Deviation: 6.16556, Variance: 38.01417.",
            "Driest monthly rainfall: Mean amount: 9.01667, Maximum amount: 20.2, 
            Minimum amount: 0.6, Standard Deviation: 5.57223, Variance: 31.04972.",
            "Wettest monthly rainfall: Mean amount: 132.65, Maximum amount: 179.6, 
            Minimum amount: 97.4, Standard Deviation: 22.15282, Variance: 490.7475.",
            "Therefore, in Greenvale Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption3 <- renderText({formulaText3()})
  
  formulaText4 <- reactive({
    if(input$WaterStorageArea == "Maroondah Reservoir")
      paste("Average monthly rainfall: Mean amount: 95.64167, Maximum amount: 116.8, 
            Minimum amount: 63.6, Standard Deviation: 17.27944, Variance: 298.5791.",
            "Driest monthly rainfall: Mean amount: 13.10833, Maximum amount: 35.6, 
            Minimum amount: 0, Standard Deviation: 13.41377, Variance: 179.9291.",
            "Wettest monthly rainfall: Mean amount: 256.64167, Maximum amount: 316.1, 
            Minimum amount: 224.9, Standard Deviation: 28.06669, Variance: 787.7391.",
            "Therefore, in Maroondah Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption4 <- renderText({formulaText4()})
  
  formulaText5 <- reactive({
    if(input$WaterStorageArea == "O'Shannassy Reservoir")
      paste("Average monthly rainfall: Mean amount: 113.95833, Maximum amount: 168.8, 
            Minimum amount: 62.3, Standard Deviation: 33.77147, Variance: 1140.51243.",
            "Driest monthly rainfall: Mean amount: 19.29167, Maximum amount: 49.8, 
            Minimum amount: 0.4, Standard Deviation: 16.29046, Variance: 265.3791.",
            "Wettest monthly rainfall: Mean amount: 294.31667, Maximum amount: 438.4, 
            Minimum amount: 174.2, Standard Deviation: 81.28866, Variance: 6607.84639.",
            "Therefore, in O'Shannassy Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption5 <- renderText({formulaText5()})
  
  formulaText6 <- reactive({
    if(input$WaterStorageArea == "Silvan Reservoir")
      paste("Average monthly rainfall: Mean amount: 98.50833, Maximum amount: 122.8, 
            Minimum amount: 63.4, Standard Deviation: 20.92883, Variance: 438.01576.",
            "Driest monthly rainfall: Mean amount: 14.10833, Maximum amount: 32, 
            Minimum amount: 0, Standard Deviation: 12.34338, Variance: 152.3591.",
            "Wettest monthly rainfall: Mean amount: 280.55833, Maximum amount: 330.2, 
            Minimum amount: 221.2, Standard Deviation: 37.01733, Variance: 1370.28243.",
            "Therefore, in Silvan Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption6 <- renderText({formulaText6()})
  
  formulaText7 <- reactive({
    if(input$WaterStorageArea == "Sugarloaf Reservoir")
      paste("Average monthly rainfall: Mean amount: 62.78333, Maximum amount: 74.8, 
            Minimum amount: 45.4, Standard Deviation: 9.28788, Variance: 86.26472.",
            "Driest monthly rainfall: Mean amount: 14.76667, Maximum amount: 25, 
            Minimum amount: 0, Standard Deviation: 7.36742, Variance: 54.27889.",
            "Wettest monthly rainfall: Mean amount: 141.25, Maximum amount: 195, 
            Minimum amount: 113.4, Standard Deviation: 29.58841, Variance: 875.47417.",
            "Therefore, in Sugarloaf Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption7 <- renderText({formulaText7()})
  
  formulaText8 <- reactive({
    if(input$WaterStorageArea == "Tarago Reservoir")
      paste("Average monthly rainfall: Mean amount: 79.40833, Maximum amount: 105.1, 
            Minimum amount: 54.3, Standard Deviation: 15.05625, Variance: 226.69076.",
            "Driest monthly rainfall: Mean amount: 18.21667, Maximum amount: 36, 
            Minimum amount: 3.1, Standard Deviation: 10.10238, Variance: 102.05806.",
            "Wettest monthly rainfall: Mean amount: 176.075, Maximum amount: 237.8, 
            Minimum amount: 123.8, Standard Deviation: 36.08192, Variance: 1301.90521.",
            "Therefore, in Tarago Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption8 <- renderText({formulaText8()})
  
  formulaText9 <- reactive({
    if(input$WaterStorageArea == "Thomson Reservoir")
      paste("Average monthly rainfall: Mean amount: 84.8, Maximum amount: 107.2, 
            Minimum amount: 51.7, Standard Deviation: 18.19853, Variance: 331.18667.",
            "Driest monthly rainfall: Mean amount: 17.16667, Maximum amount: 36.6, 
            Minimum amount: 3, Standard Deviation: 9.6587, Variance: 93.29056.",
            "Wettest monthly rainfall: Mean amount: 227.39167, Maximum amount: 337.8, 
            Minimum amount: 160.8, Standard Deviation: 58.14849, Variance: 3381.24743.",
            "Therefore, in Thomson Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption9 <- renderText({formulaText9()})
  
  formulaText10 <- reactive({
    if(input$WaterStorageArea == "Upper Yarra Reservoir")
      paste("Average monthly rainfall: Mean amount: 92.84167, Maximum amount: 124.6, 
            Minimum amount: 54.9, Standard Deviation: 22.53129, Variance: 507.6591.",
            "Driest monthly rainfall: Mean amount: 17.28333, Maximum amount: 45.8, 
            Minimum amount: 0.8, Standard Deviation: 12.95922, Variance: 167.94139.",
            "Wettest monthly rainfall: Mean amount: 223.95, Maximum amount: 288.5, 
            Minimum amount: 184.2, Standard Deviation: 29.11977, Variance: 847.96083.",
            "Therefore, in Upper Yarra Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption10 <- renderText({formulaText10()})
  
  formulaText11 <- reactive({
    if(input$WaterStorageArea == "Yan Yean Reservoir")
      paste("Average monthly rainfall: Mean amount: 55.36667, Maximum amount: 67.8, 
            Minimum amount: 44.7, Standard Deviation: 6.56129, Variance: 43.05056.",
            "Driest monthly rainfall: Mean amount:  5.70833, Maximum amount: 15.4, 
            Minimum amount: 0, Standard Deviation: 5.04438, Variance: 25.44576.",
            "Wettest monthly rainfall: Mean amount: 186.04167, Maximum amount: 231.9, 
            Minimum amount: 144.1, Standard Deviation: 31.32793, Variance: 981.4391.",
            "Therefore, in Yan Yean Reservoir, the wettest rainfall fluctuate most dramatically, 
            and driest rainfall fluctuate most gentlely.")
  })
  output$caption11 <- renderText({formulaText11()})

  # define the algorithm to render header
  formulaText12 <- reactive({
      paste("Show monthly rainfall data by rainfall types:")
  })
  output$caption12 <- renderText({formulaText12()})
  
  # define the algorithm to render plots of rainfall types
  output$plot1 <- renderPlot({
    ggplot(average, aes(x=Month, y=AverageRainfall, colour=WaterStorageArea)) + geom_point() + geom_line()
  })
  
  # define the algorithm to render records corresponding to points
  output$click_info1 <- renderPrint({
    nearPoints(average, input$plot1_click)
  })
  
  output$plot2 <- renderPlot({
    ggplot(wettest, aes(x=Month, y=WettestRainfall, colour=WaterStorageArea)) + geom_point() + geom_line()
  })
  
  output$click_info2 <- renderPrint({
    nearPoints(wettest, input$plot2_click)
  })
  
  output$plot3 <- renderPlot({
    ggplot(driest, aes(x=Month, y=DriestRainfall, colour=WaterStorageArea)) + geom_point() + geom_line()
  })
  
  output$click_info3 <- renderPrint({
    nearPoints(driest, input$plot3_click)
  })
}
shinyApp(ui, server)
