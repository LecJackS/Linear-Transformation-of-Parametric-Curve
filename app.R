library(shiny)
library(plotly)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Linear Transformation of Parametric Curve"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      #sliderInput(inputId = "bins",
      #            label = "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30),
      # Input: Specify the number of observations to view ----
      h5(strong("Curve c = ( a(t), b(t) )")),
      textInput("curve_1", "a(t) =", "exp(9*t)"),
      textInput("curve_2", "b(t) =", "exp(6*t)"),
      
      fluidRow(
        column(4, offset = 0, style='padding:0px;',
               numericInput("t_min", "t min:", 0, step=0.1)),
        column(4, offset = 0, style='padding:0px;',
               numericInput("t_max", "t max:", 0.4, step=0.1)),
        column(4, offset = 0, style='padding:0px;',
               numericInput("t_step", "t step:", 0.05, step=0.01)
        )),
      checkboxInput("square_plot", "Square Plot", value = FALSE, width = NULL),
      selectInput("plot_type", "Type", choices=c("lines","markers", "lines+markers")),
      
     
      
      
      
      h5(strong("Multiplicative constants C1, C2")),
      textInput("c1s", "C1 (separated by commas):", "1, -1, 2, -2"),
      textInput("c2s", "C2 (separated by commas):", "1, -1"),
      
      h5(strong("Transformation matrix")),
      fluidRow(style = "background-color:gray;",
               column(6,
                      numericInput("trans_1", "", cos(pi/4), step=0.1),
                      numericInput("trans_3", "", sin(pi/4), step=0.1),
               ),
               column(6,
                      numericInput("trans_2", "", -sin(pi/4), step=0.1),
                      numericInput("trans_4", "", cos(pi/4), step=0.1),
               ),
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      h6("Click and drag to zoom. Double click to reset."),
      
      plotlyOutput(outputId = "curvePlot"),
      
      plotlyOutput(outputId = "transformedPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  
  output$curvePlot <- renderPlotly({

    gridt <- c(seq(input$t_min, input$t_max, input$t_step), input$t_max)
    curve <- matrix(c(rep(0, length(gridt)), seq(1, length(gridt))), ncol=2)
    
    i <- 1
    for (t in gridt){
      curve[i,] <- curve_at(t)
      i = i + 1
    }

    colnames(curve) <- c("x", "y")

    c1s <- as.numeric(strsplit(input$c1s, "[,]")[[1]])
    c2s <- as.numeric(strsplit(input$c2s, "[,]")[[1]])
    
    curve.df <- data.frame(x=curve[,"x"], y=curve[,"y"])

    fig <- plot_ly(curve.df, x=~x, y=~y, name="Curve", mode = input$plot_type)
    
    if (input$square_plot){
      # Set x and y axis to have the same scale
      fig <- fig %>% layout(
        xaxis = list(
          scaleratio = 1
        ),
        yaxis = list(
          scaleanchor = "x"
        )
      )
    }
    
  
    for(c1 in c1s){
      for(c2 in c2s){
        #lines(curve %*% matrix(c(c1,0,0,c2), ncol=2))
        curve.df_var <- data.frame(x=c1 * curve[,"x"], y=c2*curve[,"y"])
        # print("DF var:")
        # print(curve.df_var)
        # fig_var <- plot_ly(curve.df_var, x=~x, y=~y, name="Curve", mode = input$plot_type)
        fig <- fig %>% add_trace(data=curve.df_var, x=~x, y=~y, name=toString(c(c1,c2)))
      }
    }
    
    fig <- fig %>% layout(legend=list(title=list(text='<b> C1, C2 </b>')))
    
    fig
    
    })
  
  output$transformedPlot <- renderPlotly({
    
    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
      
      trans <- matrix(c(c(input$trans_1, input$trans_2),
                        c(input$trans_3, input$trans_4)), ncol=2)
      
      gridt <- seq(input$t_min, input$t_max, input$t_step)
      curve <- matrix(c(seq(1, length(gridt)), gridt), ncol=2)
      
      i <- 1
      for (t in gridt){
        curve[i,] <- curve_at(t)
        i = i + 1
      }
      
      curve_trans <- t(trans %*% t(curve))
      
      colnames(curve) <- c("x", "y")
      colnames(curve_trans) <- c("x", "y")
      
      c1s <- as.numeric(strsplit(input$c1s, "[,]")[[1]])
      c2s <- as.numeric(strsplit(input$c2s, "[,]")[[1]])
      
      curve.df <- data.frame(x=curve_trans[,"x"], y=curve_trans[,"y"])
      
      fig <- plot_ly(curve.df, x=~x, y=~y, name="Curve", mode = input$plot_type)
      
      if (input$square_plot){
        # Set x and y axis to have the same scale
        fig <- fig %>% layout(
          xaxis = list(
            scaleratio = 1
          ),
          yaxis = list(
            scaleanchor = "x"
          )
        )
      }
      
      
      
      for(c1 in c1s){
        for(c2 in c2s){
          #lines(curve %*% matrix(c(c1,0,0,c2), ncol=2))
          curve_trans <- t(matrix(c(c1,0,0,c2), ncol=2) %*% t(curve))
          curve_trans <- t(trans %*% t(curve_trans))
          colnames(curve_trans) <- c("x", "y")
          curve.df_var <- data.frame(x=(curve_trans[,"x"]),
                                     y=(curve_trans[,"y"]))
          # print("DF var:")
          # print(curve.df_var)
          # fig_var <- plot_ly(curve.df_var, x=~x, y=~y, name="Curve", mode = input$plot_type)
          fig <- fig %>% add_trace(data=curve.df_var, x=~x, y=~y, name=toString(c(c1,c2)))
        }
      }
      
      fig <- fig %>% layout(legend=list(title=list(text='<b> C1, C2 </b>')))
      
      fig
      
      
      # c1s <- as.numeric(strsplit(input$c1s, "[,]")[[1]])
      # c2s <- as.numeric(strsplit(input$c2s, "[,]")[[1]])
      # 
      # m <- max(curve_trans[,1])
      # 
      # plot(curve_trans, ylim=c(-m, m), xlim=c(-m, m), type="l")
      # 
      # for(c1 in c1s){
      #   for(c2 in c2s){
      #     lines(curve %*% trans %*% matrix(c(c1,0,0,c2), ncol=2))
      #   }
      # }
    
    
    
    
    
  })
  
    
  curve_at <- function(t){
    curve_1_at_t <- gsub("t", t, input$curve_1)
    curve_2_at_t <- gsub("t", t, input$curve_2)
    
    a <- eval(parse(text=curve_1_at_t))
    b <- eval(parse(text=curve_2_at_t))
    
    curve_at_t <- matrix(c(a, b), ncol=2)
    
    return( curve_at_t )
  }
  

}


shinyApp(ui = ui, server = server)