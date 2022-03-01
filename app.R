library(shiny)
library(shinythemes)
library(devtools)
library(curl)
library(jsonlite)
library(tibble)
library(dplyr)
library(plotly)
# install_github("prmac/fplmax")
library(fplmax)


# Prepare data
matched_data <- fplmax::fpl_1920_data

position_fills <- c(
  "Goalkeeper" = "#4DAF4A",
  "Defender" = "#E41A1C",
  "Midfielder" = "#377EB8",
  "Forward" = "#984EA3"
)


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("admin", "standard"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("Admin user", "Standard user")
)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  # Application title
  titlePanel(
    fluidRow(
      column(4, "FPL data explorer"),
      column(4, img(src = "Logo2016.PNG", height = "10%", width = "50%"))
    )
  ),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar
      uiOutput("sidebarpanel"),
      width = 15
    ),
    mainPanel(
      # Show a plot of the data
      plotlyOutput("fpl_plot", height = "700px", width = "800px"),
      width = 7
    )
  )
)

# Define server logic
server <- function(input, output) {

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  plot_data <- reactive({
    matched_data %>%
      dplyr::filter(
        Minutes >= input[["minutes"]],
        between(Price, input[["price"]][[1]], input[["price"]][[2]]),
        Position %in% input[["position"]]
      )
  })

  fill_palette <- reactive({
    if (input[["colour"]] == "Team") {
      fplmax::team_fills
    } else if (input[["colour"]] == "Position") {
      position_fills
    }
  })
  colour_palette <- reactive({
    if (input[["colour"]] == "Team") {
      fplmax::team_colours
    } else if (input[["colour"]] == "Position") {
      position_fills
    }
  })

  output$fpl_plot <- renderPlotly({
    req(credentials()$user_auth)
    ggplot(plot_data(), aes_string(x = input[["x"]], y = input[["y"]], text = "Name")) +
      geom_jitter(aes_string(colour = input[["colour"]], fill = input[["colour"]]),
        shape = 21, stroke = 0.3, width = 0.05, height = 0.05, size = 3
      ) +
      scale_fill_manual(values = fill_palette()) +
      scale_colour_manual(values = colour_palette()) +
      theme_classic() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        text = element_text(colour = "white"),
        axis.line = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white", angle = 45, vjust = 0.5),
        axis.text.y = element_text(colour = "white")
      )
  })

  output$sidebarpanel <- renderUI({
    req(credentials()$user_auth)

    tagList(
      column(
        width = 4,
        sliderInput("minutes", "Minimum minutes played",
          min = 0, max = max(matched_data[["Minutes"]]), value = 2000
        ),
        sliderInput("price", "Price range",
          min = min(matched_data[["Price"]]), max = max(matched_data[["Price"]]),
          value = c(min(matched_data[["Price"]]), max(matched_data[["Price"]])),
          step = 0.1
        ),
        checkboxGroupInput("position", "Position",
          choices =
            list("Goalkeeper", "Defender", "Midfielder", "Forward"),
          selected =
            c("Goalkeeper", "Defender", "Midfielder", "Forward")
        ),
        selectInput("colour", "Colour by",
          choices = list("Team", "Position"), selected = "Team"
        ),
        fluidRow(
          column(6, selectInput("x", "x-axis",
            choices = colnames(matched_data), selected = "Price"
          )),
          column(6, selectInput("y", "y-axis",
            choices = colnames(matched_data), selected = "PPG"
          ))
        )
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
