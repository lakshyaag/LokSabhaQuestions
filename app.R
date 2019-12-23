#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinycssloaders)
library(lubridate)
library(dplyr)
library(bbplot)
library(RColorBrewer)
library(ggwordcloud)
library(tidytext)


dir.create('~/.fonts')
file.copy("www/Helvetica.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Reading and cleaning the dataset
questions <- read.csv("Winter_LokSabha17Questions.csv") %>% 
    rename('Question Number' = "Q.NO.", "Type" = "Q.Type") %>%
    mutate(Type = (str_replace(Type, pattern = "(PDF).*", replacement = "")) %>% str_trim(), 
            Date = as.Date(Date, format = "%d.%m.%Y"),
            Ministry = str_trim(Ministry)) %>%
    filter(Date >= as.Date("2019-11-18")) %>% # Filtering the winter session questions only
    mutate(Link = ifelse(Type == "STARRED", paste0("http://164.100.24.220/loksabhaquestions/annex/172/AS", `Question Number`, '.pdf'), 
                         paste0("http://164.100.24.220/loksabhaquestions/annex/172/AU", `Question Number`, '.pdf'))) # Adding actual link

# Ministry-wise - Questions wordlcloud
ministry_list <- questions$Ministry %>% unique() %>% str_to_title() %>% sort()

ministry_wordcloud <- function(ministry, word_count){
    
    ministry_q <- questions %>% filter(Ministry == str_to_upper(ministry)) %>%
        mutate(Subject = as.character(Subject)) %>%
        select(Subject) %>%
        unnest_tokens(word, Subject) %>%
        anti_join(get_stopwords(), by = 'word') %>%
        count(word, sort = T) %>%
        head(word_count)
    
    (
        ggplot(ministry_q, aes(label = word, color = word, size = n)) + 
            geom_text_wordcloud_area(rm_outside = T, family = 'Helvetica') +
            scale_size_area(max_size = 40) +
            bbc_style() +
            labs(title = paste0("Ministry of ", ministry, " - Subject of questions asked"), 
                 subtitle = "Winter Session of 17th Lok Sabha")
        
    ) %>% finalise_plot(source_name = "Data: Lok Sabha Question Search; Created by Lakshya Agarwal", 
                        save_filepath = paste0(ministry, '.jpg'), width = 1280, height = 720)
}




# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("17th Lok Sabha - Winter Session"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            h4('This application generates a wordcloud from the subject of questions asked to a ministry.'),
            p('Select the ministry and the number of words below.'),
            
            selectizeInput('ministry',
                           'Ministry',
                           choices = ministry_list,
                           multiple = F,
                
            ),
            
            sliderInput('words',
                        'Number of words',
                        min = 10, max = 60, value = 40, step = 5)
        ),

        # Show the plot
        mainPanel(
           plotOutput("wordplot", height = "auto") %>% withSpinner(type = 8, color = "#ff9933")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    output$wordplot <- renderPlot({
        ministry_wordcloud(input$ministry, input$words)
    }, height = function() {
        session$clientData$output_wordplot_width * (9/16)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
