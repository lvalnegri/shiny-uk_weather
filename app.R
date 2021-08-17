invisible(lapply(c('data.table', 'ggplot2', 'rvest', 'shiny'), require, char = TRUE))

wp <- read_html('https://www.metoffice.gov.uk/research/climate/maps-and-data/historic-station-data')

stn <- wp %>% 
        html_elements('td:nth-child(3), td:nth-child(2), td:nth-child(1)') %>% 
        html_text2() %>% 
        matrix(ncol = 3, byrow = TRUE) %>% 
        as.data.table() %>% 
        .[, c('x_lon', 'y_lat') := tstrsplit(V2, ', ', fixed = TRUE)] %>% 
        .[, V2 := NULL] %>% 
        cbind( wp %>% html_elements('.alternate-bg a') %>% html_attr('href') ) %>% 
        setnames(c('V1', 'V2', 'V3'), c('city', 'url', 'ystart'))

stn.lst <- 1:nrow(stn)
names(stn.lst) <- stn$city

mtc.lst <- c('Mean daily maximum temperature' = 'tmax', 
                    'Mean daily minimum temperature' = 'tmin', 
                    'Total rainfall' = 'rain', 
                    'Total sunshine duration' = 'sun'
                )

ui <- ui <- fluidPage(

    titlePanel('UK Climate Change'),

    sidebarLayout(

        sidebarPanel(

            selectInput('cbo_stn', 'STATION:', stn.lst),

            radioButtons('rdo_mtc', 'METRIC:', mtc.lst),

            width = 2

        ),

        mainPanel(
           plotOutput("out_plot")
        )

    )
    
)

server <- function(input, output) {

    output$out_plot <- renderPlot({
        
        dts <- fread(stn[as.integer(input$cbo_stn), url], header = FALSE, skip = 7, na.strings = '---', select = c(1:4, 6, 7), col.names = c('yyyy', 'mm', 'tmax', 'tmin', 'rain', 'sun'))
        cols <- c('tmin', 'tmax', 'sun')
        dts <- dts[!is.na(tmax)][, (cols) := lapply(.SD, function(x) x = as.numeric(gsub('#|\\*', '', x))), .SDcols = cols]
        dts <- data.table(mm = 1:12, mmm = factor(month.abb, levels = month.abb), mmmm = factor(month.name, levels = month.name))[dts, on = 'mm']

        ggplot(dts, aes_string('yyyy', input$rdo_mtc)) +
            geom_point() + 
            geom_smooth(method = 'lm') +
            facet_wrap('mmmm', nrow = 4) +
            theme_minimal()
        })
}

shinyApp(ui = ui, server = server)


