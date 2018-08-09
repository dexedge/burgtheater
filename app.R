# Operas in the Burgtheater
# A Shiny app
# 
# Dexter Edge
#
# Version 1.2
# Updated: 2018-08-09

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(DT)
library(dygraphs)
library(xts)
library(RColorBrewer)

#############
# Load data #
#############
load("operas.Rdata")

operas <- operas %>% select(Date, DOW, Title, Composer, Receipts, Zinz)

d_bg_composer <- operas[, -4] # For background histograms; these don't change
d_bg_title <- operas[, -3]


# Color palette for histograms
# We need 24 colors, as there are 23 opera titles,
# so take a set of 8 and sequence it x3

palette <- rep(brewer.pal(8, "Set2"), 3)

#####################
# Utility functions #
#####################
# Population standard deviation

std <- function(x) {
  x_bar <- mean(x)
  n <- length(x)
  round(sqrt(sum((x-x_bar)^2)/n), 2)
}

# Convert kreuzer to gulden and kreuzer
fl <- function(n) {
  n %/% 60
}

kr <- function(n) {
  round(n %% 60)
}

#############
# Define UI #
#############
# themes in contention: flatly, sandstone, simplex, yeti, 
ui <- fluidPage(theme = shinytheme("flatly"),
    
    ##############
    # Custom CSS #
    ##############
    # Overwrite style for column headers in datatable so they align with data
    #    (Code adapted from answer on StackExchange)
    # Set background color and style for horizontal rules
    # Adjust height of numeric input box
    # Create "small" CSS class for various text annotations
                
    tags$head(
        tags$style(
                   "body {background: #fcfcfc}
                    table.dataTable thead th {
                         padding: 8px 16px 8px 10px !important;}
                    hr {border-top: 2px solid #e6e6e6;}
                    #kreuzer {height: 30px; padding: 0 10px;
                              text-align: left;}
                    small {font-size: 12px}"
                  ) # End tags$style
    ),            
   
   ##########
   # Layout #
   ##########
   # Application title
   titlePanel("Operas in the Burgtheater, 14 Apr 1789 to 7 Mar 1791",
              windowTitle = "Burgtheater"),
   h4("Dexter Edge"),
   hr(),
   
   # Sidebar
   sidebarLayout(
     sidebarPanel(
       radioButtons("radio", label = "Select by:",
                    choices = c("Composers" = 1, "Titles" = 2),
                    inline = TRUE,
                    selected = 1),
       
       conditionalPanel(
         condition = "input.radio == 1",
         selectInput(inputId = "composer",
                     label = "Composers",
                     choices = c("All", levels(operas$Composer)),
                     selected = "All",
                     multiple = TRUE
         ) # End selectInput Composer
       ), 
       
       conditionalPanel(
         condition = "input.radio == 2",
         selectInput(inputId = "title",
                     label = "Titles",
                     choices = c("All", levels(operas$Title)),
                     selected = "All",
                     multiple = TRUE
         ) # End selectInput Title
       ),
       
       # Note that the dates below may display incorrectly as one day
       # earlier when viewing the app with R Studio's built-in browser,
       # but they will display correctly in an external browser
       conditionalPanel(
         # Do not show with dygraph tab
         condition = "input.tabselected != 6",
         dateRangeInput(inputId = "dates",
                        label = "Dates",
                        start = "1789-04-14",
                        end = "1791-03-07",
                        min = "1789-04-14",
                        max = "1791-03-07"
         ), # End dateRangeInput
       
         HTML("<p class='small'>The season 1789/90 ended on 1790-02-11
               <br>The season 1790/91 began on 1790-04-13<br><br></p>")
       ),
       
       conditionalPanel(
         # Show sliderInput only for Table and Summary
         condition = "input.tabselected < 3",
         sliderInput(inputId = "receipts",
                     label = "Receipts (kr)",
                     min = 1000,
                     max = 40000,
                     value = c(1000, 40000),
                     step = 5,
                     sep = ""
         ) # End sliderInput
       ),
       
       # Input and output for Kreuzer converter
       hr(),
       numericInput(inputId = "kreuzer",
                   label = "Convert kreuzer",
                   value = 0,
                   min = 0,
                   width = "120px"
      ), # End numericInput
         
      uiOutput(outputId = "value"),
      
      hr(),
      HTML("<p class = 'small'>All receipt values are in kreuzer (kr),<br>
            where 60 kr = 1 gulden (fl)</p>")
      
    ), # End sidebarPanel
   
   # Show tabs and output
   mainPanel(
     tabsetPanel(type = "tabs",
       tabPanel("Table", value = 1,
                dataTableOutput(outputId = "operastable"),
                HTML("<p style='small'>To sort a column, click on its name</p>")),
       tabPanel("Summary", value = 2,
                dataTableOutput(outputId = "summarytable"),
                HTML("<p style='small'>To sort a column, click on its name</p>")),
       tabPanel("Boxplots", value = 3,
                plotOutput(outputId = "operasboxplot"),
                hr(),
                includeMarkdown("burgtheater-boxplot.Rmd")),
       tabPanel("Histograms", value = 4,
                plotOutput(outputId = "operashistogram"),
                hr(),
               HTML("The gray background shows the distribution
                  of receipts for all 291 opera performances in the Burgtheater
                  in the period 14 Apr 1789 to 7 Mar 1791. Colored bins in
                  the foreground show the distributions of receipts for
                  the selected composers or titles over the selected period.")),
       tabPanel("Series", value = 6,
                dygraphOutput(outputId = "operatimeseries"),
                HTML("<br>Move the sliders to change the date range 
                     of the time series.<br>Mousing over a point
                     shows the date, receipts, and title for that point
                     at the upper right.")
                ),
       tabPanel("Help",
                includeMarkdown("burgtheater-help.Rmd")),
       tabPanel("About",
                includeMarkdown("burgtheater-about.Rmd")),
       id = "tabselected"
    ) # tabsetPanel
  ) # End mainPanel
  
  ) # End sidebarLayout
) # End fluidPage

#######################
# Define server logic #
#######################
server <- function(input, output, session) {
  
   # Select operas by composer or title to view in datatable 
   operas_selected <- reactive({
     data <- operas
     
     # If Composer is selected, show Composer drop-down, else
     # show Title drop-down
     if (input$radio == 1) {
       req(input$composer)
       if (!("All" %in% input$composer)) {
         data <- filter(data, Composer %in% input$composer)
       }
     } else {
       req(input$title)
       if (!("All" %in% input$title)) {
         data <- filter(data, Title %in% input$title)
       }
     } # End if
     
     # Show date-range selector for all tabs except Time Series
     if (input$tabselected != 6) {
     data <- filter(data, 
                    Date >= input$dates[1] & 
                      Date <= input$dates[2])
     }
     
     # Do we have input from a receipts slider?
     if (isTruthy(input$receipts)) {
       data <- filter(data,
                      Receipts >= input$receipts[1] &
                        Receipts <= input$receipts[2])
     }
     data
   }) # End reactive operas_selected
   
   # Make summary data for selected operas
   operas_summary <- reactive({
     if (input$radio == 1) {
       operas_selected() %>% 
         group_by(Composer) %>%
         summarise(Titles = n_distinct(Title),
                   Performances = n(),
                   Median = median(Receipts),
                   Mean = mean(Receipts),
                   SD = std(Receipts)
         )} else {
           operas_selected() %>% 
             group_by(Title) %>%
             summarise(By = unique(Composer),
                       Performances = n(),
                       Median = median(Receipts),
                       Mean = mean(Receipts),
                       SD = std(Receipts))}
   }) # End reactive operas_summary
   
   # Create boxplot of selected operas
   operasboxplot <- reactive({
     if (input$radio == 1) {
       ordered <- reorder(operas_selected()$Composer, 
                          operas_selected()$Receipts, FUN = median)
     } else {
       ordered <- reorder(operas_selected()$Title, 
                          operas_selected()$Receipts, FUN = median)
     }
     operas_selected() %>% 
       ggplot() +
       geom_boxplot(aes(x = reorder(ordered, Receipts, FUN = median),
                        y = Receipts), varwidth = TRUE) +
       xlab(NULL) +
       ylab("Receipts (kr)") +
       scale_y_continuous(breaks = seq(0, 40000, 5000)) +
       theme(axis.title.x = element_text(size = 14),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             panel.border = element_rect(color = "grey", fill = NA)) +
       coord_flip()
     
   }) # End reactive operasboxplot
   
# Create facetted histogram of selected operas
# Plotting background data for groups
# Based on a method at Simon Jackson's BlogR
# https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2
   
   operashistogram <- reactive({
       if (input$radio == 1) {
           operas_selected() %>%
           group_by(Composer) %>% 
           ggplot(aes(Receipts, fill = Composer)) +
             geom_histogram(data = d_bg_composer, fill = "grey",
                            alpha = 0.5) +
             geom_histogram(color = "grey3") +
             facet_wrap(~Composer) +
             scale_fill_manual(values = palette) +
             guides(fill = FALSE) +
             xlab("Receipts (kr)") +
             ylab("Count") +
             scale_x_continuous(breaks = seq(0, 40000, 5000)) +
             scale_y_continuous(breaks = seq(0, 30, 5)) +
             theme(axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   strip.text.x = element_text(size = 12))
         } else {
           operas_selected() %>%
           group_by(Title) %>% 
           ggplot(aes(Receipts, fill = Title)) +
             geom_histogram(data = d_bg_title, fill = "grey", 
                            alpha = 0.5) +
             geom_histogram(color = "grey3") +
             facet_wrap(~Title) +
             scale_fill_manual(values = palette) +
             guides(fill = FALSE) +
             xlab("Receipts (kr)") +
             ylab("Count") +
             scale_x_continuous(breaks = seq(0, 40000, 5000)) +
             theme(axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   axis.text.x = element_text(angle = 45, hjust = 1),
                   strip.text.x = element_text(size = 12))
         } 
       
   }) # End reactive operashistogram 
   
   # Make xts time series for dygraphs time-series plot
   operas_xts <- reactive({
     receipts <- xts(operas_selected()$Receipts,
                     as_date(operas_selected()$Date))
     colnames(receipts) <- "kr"
     new <- as_date("1790-02-12")
     receipts <- merge.xts(receipts, new)
     receipts
   }) # End reactive operas_xts
   
   # Create vector of titles to use in legend
   titles <- reactive({
     sapply(operas_selected()$Title, as.character, 
                    stringsAsFactors = FALSE)
   }) # End reactive titles
   
   # Build dygraph plot
   operastimeseries <- reactive({
     dygraph(operas_xts(), 
             main = "Opera Receipts, Burgtheater, 14 Apr 1789 to 7 Mar 1791",
             ylab = "Receipts (kr)") %>% 
       dySeries("kr", label = "Receipts (kr)") %>% 
       dyOptions(drawPoints=TRUE, pointSize=2) %>%
       # Format date in Legend consistently. Add padding at extremes of
       # range so that points are easier to see
       dyAxis("x", valueFormatter = "function(ms) {
                                return new Date(ms).toDateString()}",
              rangePad=5
              )  %>% 
       dyEvent(x=as.Date("1790-02-20"), label="Death of Joseph II") %>% 
       # dyHighlight(highlightCircleSize = 5) %>% 
       dyLegend(show="auto") %>% 
       dyRangeSelector() %>%
       # Code to add title to legend adapted from StackOverflow 27671576,
       # answer by timelyportfolio
       dyCallbacks(
         highlightCallback = sprintf(
           'function(e, x, pts, row) {
           var customLegend = %s
           // should get our htmlwidget
           var legendel = e.target.parentNode.parentNode
           .querySelectorAll(".dygraph-legend")[0];
           
           // should get our htmlwidget
           legendel.innerHTML = legendel.innerHTML + "<br>" + 
           "<em>" + customLegend[row] + "</em>";}',  # supply a vector
           jsonlite::toJSON(titles())
         )) 
   }) # End reactive dygraph
   
  # dyHighlight() is currently commented out above because it creates
  # an unresolved bug in the display of the title in the legend
   
  ####################
  # Output functions #
  ####################
   
  # Output datatable of selected operas
  output$operastable <- renderDataTable({
     datatable({operas_selected()},
     rownames = FALSE,
     # Options (t)able, (l)ength, (p)agination, (i)nfo summary
     # Numeric columns default to descending order on sort
     options = list(dom = 'tlpi', pageLength = 12,
                    columnDefs = list(list(orderSequence = c('desc', 'asc'),
                                           targets = 4)))
     ) # End datatable()
     
   }) # End renderDataTable 
   
  # Output summary datatable of selected operas
  output$summarytable <- renderDataTable({
     datatable({operas_summary()},
     rownames = FALSE,
     options = list(dom = 'tip', pageLength = 12,
                    columnDefs = list(list(orderSequence = c('desc', 'asc'),
                                           targets = 1:5)))
     # Always exactly two decimal places for Mean and SD
     ) %>% 
      formatRound(columns = "Median", 1) %>% 
      formatRound(columns = c("Mean", "SD"), 2)
   }) # End renderDataTable summary
  
  # Plot boxplot of selected operas
  output$operasboxplot <- renderPlot({
    operasboxplot()
   }) # End renderPlot boxplot
  
  # Plot histogram of selected operas
  output$operashistogram <- renderPlot({
    operashistogram()
   }) # End renderPlot histogram
  
  # Plot dygraph times series of selected operas
  output$operatimeseries <- renderDygraph({
    operastimeseries()
  }) # End renderDygraph
  
  # Output gulden and kreuzer
  output$value <- renderUI({
    req(input$kreuzer)
    HTML(paste0("= ", fl(input$kreuzer), 
                " fl ", kr(input$kreuzer), " kr"))
   }) # End renderUI gulden and kreuzer
  
} # End server function()

# Run the application 
shinyApp(ui = ui, server = server)
