library(shiny)
library(tidyverse)

load("betas.Rdata")

betas %>%
    group_by(date) %>%
    summarise(beta = mean(beta),
              min_ci_lb = min(ci_lb),
              max_ci_ub = max(ci_ub)) -> beta_mean

betas %>%
    left_join(beta_mean %>% select(-beta)) %>%
    bind_rows(cbind(ticker = "Mean", beta_mean)) -> app_beta

dj30 %>%
    filter(ticker != "DOW",
           ticker != "V") %>%
    arrange(company) -> dj30

choice_list <- c("Mean", dj30$ticker)
names(choice_list) <- c("Mean of all firms", dj30$company)

ui <- fluidPage(
    titlePanel("250 trading day daily betas for Dow Jones 30 firms"),

    sidebarLayout(
        sidebarPanel(
            selectInput("ticker",
                        "Select firm to display",
                        choices = choice_list,
                        selected = "Mean")
        ),

        mainPanel(
           plotOutput("beta_plot")
        )
    )
)

server <- function(input, output) {
    output$beta_plot <- renderPlot({
        app_beta %>%
            filter(ticker == input$ticker) %>%
            ggplot(aes(x = date, y = beta)) +
            geom_line(aes(y=beta), colour="blue") + 
            geom_ribbon(aes(ymin=min_ci_lb, ymax=max_ci_ub), alpha=0.2) + 
            labs(y = "Beta factor of 250-day CAPM regression ending on date",
                 caption = paste(
                     "The blue lines indicates the seletced beta factor.",
                     "The gray area indicates the lowest and highest firm-level\n",
                     "confidence interval for the given date for all 28 DJ 30", 
                     "firms with full price data.")) +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
