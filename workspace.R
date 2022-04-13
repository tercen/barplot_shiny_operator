library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)

# http://localhost:5402/admin/w/7132ce367ee5df28fea4032b3f011888/ds/426b00e4-b970-4042-9a53-93a10ac2da90
############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "426b00e4-b970-4042-9a53-93a10ac2da90",
                   workflowId = "7132ce367ee5df28fea4032b3f011888")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
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

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$barplot_output <- renderUI({
    values <- dataInput()
    plotOutput(
      "main.plot",
      width = input$plot.width,
      height = input$plot.height
    )
  }) 
  
  output$main.plot <- renderPlot({
    
    values <- dataInput()

    df <- values$data
    input.par <- input
    
    if(input.par$average.type == "Mean") {
      df_agg <- df %>%
        group_by_at(vars(-.y)) %>%
        summarise(mn = mean(.y, na.rm = TRUE),
                  n = n(),
                  stdv = sd(.y, na.rm = TRUE))
    }
    if(input.par$average.type == "Median") {
      df_agg <- df %>%
        group_by_at(vars(-.y)) %>%
        summarise(mn = median(.y, na.rm = TRUE),
                  n = n(),
                  stdv = sd(.y, na.rm = TRUE))
    }
    if(input.par$average.type == "Count") {
      df_agg <- df %>%
        group_by_at(vars(-.y)) %>%
        summarise(mn = n(),
                  n = n(),
                  stdv = sd(.y, na.rm = TRUE))
    }
    if(input.par$average.type == "Proportion of total") {
      df_agg <- df %>%
        group_by_at(vars(-.y)) %>%
        summarise(n = n(),
                  stdv = sd(.y, na.rm = TRUE)) %>%
        mutate(mn = n / sum(n))
    }
    
    fill.col <- NULL
    if(length(values$ctx$colors) > 0) {
      fill.col <- unlist(values$ctx$colors)
    } 
    
    if(!values$ctx$hasXAxis) {
      df_agg$.x <- ""
      df$.x <- ""
    } 
    
    theme_set(theme_light())
    
    ### Core plot with x and y axes
    plt <- ggplot(df_agg, aes_string(x = ".x", y = "mn", fill = fill.col)) +
      geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
      labs(
        title = input.par$title,
        subtitle = input.par$subtitle,
        caption = input.par$caption,
        x = input.par$xlab,
        y = input.par$ylab,
        fill = "Legend"
      )
    
    ### Add jitter
    if(input.par$jitter) {
      plt <- plt + geom_jitter(
        data = df,
        aes_string(x = ".x", y = ".y", fill = fill.col),
        size = input.par$dot.size,
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)
      )
    }
    
    ### Add SD bars
    if(input.par$error.type == "Standard Deviation") {
      plt <- plt + geom_errorbar(
        aes(ymin = mn - stdv, ymax = mn + stdv),
        width = input.par$bar.width,
        position = position_dodge(width = 0.9)
      )
    }
    
    ### Annotate with sample size
    
    ### Facets based on rows and columns
    cnames <- unlist(values$ctx$cnames)
    if(values$ctx$cnames[[1]] == "") cnames <- "."
    rnames <- unlist(values$ctx$rnames)
    if(values$ctx$rnames[[1]] == "") rnames <- "."
    
    plt <- plt + facet_grid(
      as.formula(paste(
        paste(rnames, collapse = "+"),
        "~",
        paste(cnames, collapse = "+")
      ))
    )
    
    # png(tmp, width = input.par$plot.width, height = input.par$plot.height, unit = "px")
    plot(plt)
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  values <- list()
  
  values$ctx = ctx
  
  data <- ctx %>% select(.y, .ri, .ci)
  if(ctx$hasXAxis) data$.x <- select(ctx, .x)[[".x"]]
  
  if(length(ctx$colors)) data <- data %>% dplyr::bind_cols(ctx$select(ctx$colors))
  
  rnames <- ctx$rselect() 
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <- left_join(data, rnames, by = ".ri")
  
  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <- left_join(data, cnames, by = ".ci")
  
  values$data <- data
  return(values)
}




runApp(shinyApp(ui, server))  