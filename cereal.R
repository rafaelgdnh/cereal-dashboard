library(shiny)
library(DT)
library(readr)
library(dplyr)
library(plotly)
library(rsconnect)


# Read in data ---------------------------------------------------------

df <- read_csv("https://raw.githubusercontent.com/veeps/columbia_star_2023/main/workshop_2/data/cereal_clean.csv") |> select(-Manufacturer)
nutrient_dense <- c( "Carbs", "Fiber",  "Potassium","Protein", "Vitamins")

# UI section ------------------------------------------------------------


ui <- fluidPage(
  
  # Header section -------------------------------------------------------
  
  
  fluidRow(style="padding:40px; background: #03bf7b; color: #ffffff; text-align:center",
           icon("bowl-rice", "fa-2xl", lib = "font-awesome"),
           h1("Adult Cereal Selector")
  ), # end fluid row
  
  fluidRow(style="padding:40px; background: #f2f2f2",
           h3("Nutritional Density ", align="center"),
           p("Let's start by ranking cereals based on their nutritional density. Which ones have more of the 'good stuff'? ", align="center")
  ), # end fluid row
  
  # Bar chart section -----------------------------------------------------
  fluidRow(
    column(3, radioButtons(inputId = "bar_var", 
                           label = h3("Select Variable"),
                           choices= nutrient_dense,
                           selected = "Protein")),
    column(9, plotOutput(outputId = "bar_plot"))
  ),
  
  
  
  
  
  
  # Data table section ----------------------------------------------------
  
  DTOutput(outputId = "table"),
  # Scatter plot section --------------------------------------------------
  fluidPage(
    
    # Copy the line below to make a select box 
    selectInput("xaxis", label = h3("X Variable"), 
                choices = colnames(df)[2:10] |> sort(), 
                selected = 1),
    selectInput("yaxis", label = h3("Y Variable"), 
                choices = colnames(df)[2:10] |> sort(), 
                selected = 2),
    
    
    
    plotlyOutput("scatter_plot")
    
    
  ),
  
) # end UI


# Server section -------------------------------------------------------

server <- function(input, output, session) {
  df_sub <- reactive({
    df |> arrange(desc(.data[[input$bar_var]])) |> head(10)
  })
  
  # Bar chart section -------------------------------------------------------
  
  # render barchart
  output$bar_plot <- renderPlot({
    ggplot(df_sub(),
           aes(y=.data[[input$bar_var]],
               x=Name)) +
      geom_bar(stat="identity", fill = "#0add8c") +
      theme(axis.title.x=element_blank())
  })
  
  # Data table section ----------------------------------------------------
  
  # render table
  output$table <- renderDT(
    df_sub()
  )
  
  # Scatter plot section --------------------------------------------------
  
  # render scatterplot
  output$scatter_plot <- renderPlotly({ 
    plot_ly(data = df,
            x = df[[input$xaxis]], 
            y = df[[input$yaxis]],
            type= "scatter",
            mode="markers",
            hovertemplate= paste0(
              df$Name, "<br>", "Protein", ": %{x}<br>", "Sugars", ": %{y}<extra></extra>")) 
    
  })
  
  
}

shinyApp(ui, server)
