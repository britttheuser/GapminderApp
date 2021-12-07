library(shiny)
library(tidyverse)
library(shinythemes)
library(stringr)
library(gapminder)
#library(rnaturalearthhires)
options(shiny.autoreload = TRUE)
output_variables <- select(gapminder, c(lifeExp, pop, gdpPercap))
#reference to dynamic select menu from  https://shiny.oxshef.io/tutorials_controls-dependent-on-data.html, https://github.com/OxShef/oxshef_shiny/tree/master/tutorial-apps/controls-dependent-on-data



ui <- fluidPage(
    titlePanel("Socioeconomic data for 142 countries"),
    strong("This is an app that helps you explore the population and GDP of countries around the world"),
    br(),
    "You can choose a country and a varaible to see its changes thorughout the years",
    br(),
    br(),
    fluidPage(theme = shinytheme("darkly")),
    sidebarLayout(
        sidebarPanel(tags$b("You can input here: "),
                     hr(),
                     "First, choose a continent",
                     # uiOutput("selected_continent_UI"),
                     selectInput("selected_continent",
                                 "Selected continent:",
                                 choices = ""),
                     br(),
                     "Select a country that is a member of the continent chosen above.",
                     # uiOutput("selected_country_UI"),
                     selectInput("selected_country",
                                 "Selected country:",
                                 choices = ""),
                     hr(),
                     sliderInput("my_slider", "Select a year range", min = 1952, max = 2007, step = 10, value = c(1952, 2007), ticks = TRUE),
                     actionButton("reset", "Reset"),
                     hr(),
                     varSelectInput("my_variable", "Select the data you want to see", data = output_variables, selected = "gdpPercap", multiple = FALSE),
                     hr(),
                     radioButtons("my_select", "Select a continent you wish to see the overview of ", choices = unique(gapminder$continent)),

        ),

        mainPanel(tags$b("Your results are displayed here: "),
                  br(),
                  hr(),
                  tabsetPanel(
                      tabPanel("Plot",
                               plotOutput("my_plot")),
                      tabPanel("Table",
                               textOutput("my_text"),
                               br(),
                               tableOutput("my_table")),
                      tabPanel("Countinent Overview",
                               textOutput("my_continent_text"),
                               br(),
                               tableOutput("my_overview")),
                      tabPanel("Country Lookup",
                               textInput("user_text", label = h3("You can look up the information for a specific country: "), value = "Enter country name here"),
                               hr(),
                               tableOutput("user_table"))
                  )
        )
    )




)


server <- function(input, output, session) {

    observe({
        updateSelectInput(session,
                          "selected_continent",
                          choices = unique(gapminder$continent))
    })


    observe({
        updateSelectInput(
            session,
            "selected_country",
            choices = gapminder%>%
                filter(continent == input$selected_continent) %>%
                select(country) %>%
                .[[1]]
        )
    })


    filtered <- reactive({
        gapminder %>%
            filter(country == input$selected_country,
                   year < input$my_slider[2],
                   year > input$my_slider[1]) %>%
            select(country, year, !!input$my_variable)
    })

    filtered_rows <- reactive (filtered()%>%summarise(n = n()))


    output$my_plot<-renderPlot(
        filtered() %>%
            ggplot(aes(year, !!input$my_variable)) +
            geom_point(alpha = 0.2) +
            geom_line(color="#34BD94") +
            xlab("Year") +
            ylab(input$my_variable) +
            scale_y_log10()
            #theme_ipsum()
    )

    #reset button referenced from https://mastering-shiny.org/action-dynamic.html
    observeEvent(input$reset, {
        updateSliderInput(inputId = "my_slider", value = c(1960, 2000))
    })

    output$my_text<- renderText ({
        paste("We found ", filtered_rows()[[1]], "results for you")
    })

    output$my_table<-renderTable(
            filtered()
    )

    output$my_overview <- renderTable(
        gapminder %>%
            group_by(input$my_select) %>%
            summarise_at(c("lifeExp", "pop", "gdpPercap"), mean, na.rm = TRUE)
    )

    #https://www.statology.org/filter-rows-that-contain-string-dplyr/ reference
    #also see https://sebastiansauer.github.io/dplyr_filter/ although have not figured out how to combine regex with input$user_text
    output$user_table<-renderTable(
        gapminder %>%
            #filter(Name == input$user_text))
            filter(grepl(input$user_text, country)))


}

#output is like a list, the element can be anything

# Run the application
shinyApp(ui = ui, server = server)
