library(BatchGetSymbols)
library(data.table)
first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers
library(tidyquant)
library(tidyverse)
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         type.return = "log",
                         do.cache = FALSE)

raw_data <- data.table(l.out$df.tickers)
df.SP500$ticker <- df.SP500$Tickers

df<-merge(x=raw_data,y=df.SP500,by="ticker",all=TRUE)
df = subset(df, select = c("ref.date","ret.closing.prices", "Company","GICS.Sector"))
df <-na.omit(df)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(ggplot2)
ui <- fluidPage(
    theme = shinythemes::shinytheme('simplex'),
    titlePanel("Stock return"),
    sidebarPanel(
        pickerInput("company", "Live search", 
                    choices = unique(df$Company),
                    options = list(
                        `live-search` = TRUE),
                    #multiple = TRUE
        ),
        dateRangeInput("dates", "Choose a date range:",
                       start = "2019-04-15",
                       end = "2020-04-10"
        )
    ),
    # MODIFY CODE BELOW: Create a tab layout for the dashboard
    tabsetPanel(
        tabPanel('Plot', plotly::plotlyOutput('shapes')),
        tabPanel("Risk measures", tableOutput("risk"))
    )
)

server <- function(input, output) {
    return_display <- reactive({ 
        df %>%
            filter(Company==input$company,
                   ref.date >= input$dates[1],
                   ref.date <= input$dates[2]
            )%>%
            group_by(Company) %>% 
            mutate(cs = cumsum(ret.closing.prices))
    })
     
    
    output$shapes <- plotly::renderPlotly({
        return_display() %>% 
            mutate(Color = ifelse(cs > 0, 'Positive', 'Negative')) %>%
            ggplot(aes(cs, ref.date, color = Color))+
            labs(title =" Cumulative return of the selected stock", x = "value", y = "date") +
            geom_col() +
            coord_flip() 
        
    })
    
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("text.html"),
            easyClose = TRUE))})
            
    output$risk <- renderTable({
        df %>%
            filter(Company==input$company,
                   ref.date >= input$dates[1],
                   ref.date <= input$dates[2],
            ) %>%
    
           summarise(
               `Empirical quantile VaR` = sprintf("%0.5f", quantile(ret.closing.prices, 0.01)),
               `Empirical quantile ES` = sprintf("%0.5f", mean(ret.closing.prices[ret.closing.prices<quantile(ret.closing.prices, 0.01)])
)
           )
    })
}

shinyApp(ui=ui, server=server)
