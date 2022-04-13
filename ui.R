library(shiny)

shinyUI(fluidPage(
  
  sidebarPanel(
    numericInput("plot.width", "Plot width (px)", min=200, max=2000, value=750),
    numericInput("plot.height", "Plot height (px)", min=200, max=2000, value=750),
    checkboxInput("jitter", "Jitter", value = FALSE),
    selectInput(
      "average.type",
      "Average type",
      c("Mean", "Median", "Count", "Proportion of total"),
      selected = "Mean",
      multiple = FALSE
    ),
    numericInput("dot.size", "Dot size", min=0.1, max=2, value=0.5),
    selectInput(
      "error.type",
      "Error type",
      c("Standard Deviation", "None"),
      selected = "Standard Deviation",
      multiple = FALSE
    ),
    numericInput("bar.width", "Bar width", min=0.1, max=2, value=0.25),
    textInput("xlab", "x axis label", ""),
    textInput("ylab", "y axis label", ""),
    textInput("title", "Title", ""),
    textInput("subtitle", "Subtitle", ""),
    textInput("caption", "Caption", "")
  ),
  
  mainPanel(
    uiOutput("barplot_output")
  )
  
))
