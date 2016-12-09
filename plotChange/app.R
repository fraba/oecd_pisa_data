library(shiny)
library(ggplot2)
library(ggrepel)
library(DT)
library(htmltools)

setwd('~/public_git/oecd_pisa_data')
source('map.R')

country_selection <- 
  subset(oecd_pisa_data, !duplicated(iso3c))$iso3c
names(country_selection) <-
  subset(oecd_pisa_data, !duplicated(iso3c))$Jurisdiction

table_tag = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Year'),
      th(colspan = 3, 'Mathematics'),
      th(colspan = 3, 'Reading')
    ),
    tr(
      lapply(rep(c('Country', 'OECD', "%"), 2), th)
    )
  )
))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%} .selectize-input{text-align: center} td {padding: 1px !important; margin: 1px !important;}"),
  plotOutput("change_plot", width = "100%", height = "100%",
             dblclick = "mainplot_dblclick",
             brush = brushOpts(
               id = "mainplot_brush",
               resetOnNew = TRUE
             )),
  absolutePanel(top = 5, left = 15,
                selectizeInput('cnt',
                               NULL, 
                               country_selection),
                div(dataTableOutput('cnt_table'),
                    style = "font-size:60%; background-color: white;")
                
  ),
  absolutePanel(bottom = 40, right = 40,
                plotOutput('zoomplot', height = '250px', width = '400px')),
  absolutePanel(top = 10, right = 50,
                div(tags$h2('OECD PISA Data'),
                    tags$a("www.oecd.org/pisa/data", href = 'http://www.oecd.org/pisa/data/'),
                    style = 'text-align: center;')),
  absolutePanel(bottom = -10, left = 50,
                div(tags$p(tags$a('Design: @FrBailo', href = 'http://www.francescobailo.net/'), " | ",
                           tags$a('Code: GitHub', href = ''))))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$mainplot_dblclick, {
    brush <- input$mainplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$cnt_table <- DT::renderDataTable({
    tmp_math_df <-
      subset(oecd_pisa_data, iso3c == input$cnt & Variable == 'mathematics', 
           select = c('Year','Average', 'annual_highincome_mean', 'of_mean'))
    tmp_math_df$Year <- as.integer(tmp_math_df$Year)
    tmp_math_df$of_mean <- tmp_math_df$of_mean * 100
    names(tmp_math_df) <- c("Year",
                       names(country_selection)[country_selection == input$cnt],
                                                'OECD', '%')
    tmp_read_df <-
      subset(oecd_pisa_data, iso3c == input$cnt & Variable == 'reading',
             select = c('Year','Average', 'annual_highincome_mean', 'of_mean'))
    tmp_read_df$Year <- as.integer(tmp_read_df$Year)
    tmp_read_df$of_mean <- tmp_read_df$of_mean * 100
    names(tmp_read_df) <- c("Year",
                            names(country_selection)[country_selection == input$cnt],
                            'OECD', '%')

    tmp_df <- merge(tmp_math_df, tmp_read_df, by = 'Year', all.x = TRUE, all.y = TRUE)
    DT::datatable(tmp_df, 
                  container = table_tag, rownames = FALSE,
                  options = list(paging = FALSE, searching = FALSE, bInfo = FALSE),
                  selection = 'none') %>%
      formatRound(columns=2:7, digits=1)
  })
  
  output$change_plot <- renderPlot({
    ggplot(math_read_df, aes(x=reading, y=mathematics)) +
      geom_point(alpha = 0.3) +
      scale_y_continuous(position = "right", limits = c(.84,1.16)) +
      scale_x_continuous(limits = c(.87,1.13)) +
      geom_vline(xintercept = 1) +
      geom_hline(yintercept = 1) +
      theme_bw() +
      geom_segment(data = subset(math_read_start_end, iso3c == input$cnt), 
                   aes(x = start_reading,
                       y = start_mathematics,
                       xend = end_reading,
                       yend = end_mathematics),
                   arrow = arrow(length = unit(0.01, "npc")),
                   curvature = 0.2, 
                   alpha = .5) +
      geom_label_repel(data = subset(math_read_df, iso3c == input$cnt), 
                       aes(x=reading, y=mathematics, label = Year),
                       segment.color = 'red',
                       alpha = 0.8) +
      geom_point(data = subset(math_read_df, iso3c == input$cnt), 
                 aes(x=reading, y=mathematics, label = Year)) +
      theme(panel.border = element_blank())
    })
  
  output$zoomplot <- renderPlot({
    
    if (is.null(ranges$x)) {
      ggplot(math_read_df, aes(x=reading, y=mathematics)) +
        geom_point(alpha = 0.3) +
        scale_y_continuous(limits = c(.84,1.16)) +
        scale_x_continuous(limits = c(.87,1.13)) +
        geom_vline(xintercept = 1) +
        geom_hline(yintercept = 1) +
        geom_rect(fill="white", aes(xmin=0.95,xmax=1.05,ymin=.95,ymax=1.05), alpha = 0.3) +
        annotate("text", x = 0.998, y = 1.003, 
                 label = "Select and double-click\n the main plot\nto zoom in") +
        labs(title = 'zoom')
    } else {
      ggplot(math_read_df, aes(x=reading, y=mathematics)) +
        geom_point(alpha = 0.3) +
        lims(x = ranges$x, y = ranges$y) +
        geom_vline(xintercept = 1) +
        geom_hline(yintercept = 1) +
        # theme_bw() +
        geom_segment(data = subset(math_read_start_end, iso3c == input$cnt), 
                     aes(x = start_reading,
                         y = start_mathematics,
                         xend = end_reading,
                         yend = end_mathematics),
                     arrow = arrow(length = unit(0.04, "npc")),
                     alpha = .5) +
        geom_label_repel(data = subset(math_read_df, iso3c == input$cnt), 
                         aes(x=reading, y=mathematics, label = Year),
                         segment.color = 'red',
                         alpha = 0.8) +
        geom_point(data = subset(math_read_df, iso3c == input$cnt), 
                   aes(x=reading, y=mathematics, label = Year)) +
        labs(title = 'zoom')
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

