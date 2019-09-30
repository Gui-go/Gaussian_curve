library(shiny)
ui <- fluidPage(
    titlePanel("Probabilidade e Distribuição Normal"),
    sidebarLayout(
        sidebarPanel(width = 2,
            numericInput(inputId = "mean_input", label = "Média", value = 0, step = 1),
            numericInput(inputId = "sd_input", label = "Desvio Padrão", value = 1, step = 1, min = 0.1),
            selectInput(inputId = "tipo_input", label = "Tipo de modelo", choices = c("Entre", "Maior que", "Menor que"), selected = "Entre"),
            conditionalPanel(
                condition = "input.tipo_input == 'Entre'",
                numericInput(inputId = "input_x1", label = "X1", value = -1, step = 1),
                numericInput(inputId = "input_x2", label = "X2", value = 1, step = 1)
            ),
            conditionalPanel(
                condition = "input.tipo_input == 'Maior que'",
                numericInput(inputId = "input_x1_ma", label = "X1", value = -1, step = 1)
            ),
            conditionalPanel(
                condition = "input.tipo_input == 'Menor que'",
                numericInput(inputId = "input_x1_me", label = "X1", value = 1, step = 1)
            )
        ),
        mainPanel(width = 10,
           plotOutput("Normal_plot"),
           uiOutput("Prob_func")
        )
    )
)
server <- function(input, output) {
    library(tidyverse)
    prob_reac <- reactive(
        if(input$tipo_input == 'Entre'){(pnorm(q = input$input_x2, mean = input$mean_input, sd = input$sd_input) - pnorm(q = input$input_x1, mean = input$mean_input, sd = input$sd_input))} else
        if(input$tipo_input == 'Maior que'){1-pnorm(q = input$input_x1_ma, mean = input$mean_input, sd = input$sd_input)} else
        if(input$tipo_input == 'Menor que'){pnorm(q = input$input_x1_me, mean = input$mean_input, sd = input$sd_input)}    
    )
    output$Prob_func <- renderUI({
        if(input$tipo_input == 'Entre'){h2(strong(withMathJax(paste0("$$P(", input$input_x1, " < X <", input$input_x2, ") = ", round(prob_reac(), 3), "$$"))))} else
        if(input$tipo_input == 'Maior que'){h2(strong(withMathJax(paste0("$$P( X > ", input$input_x1_ma, ") = ", round(prob_reac(), 3), "$$"))))} else
        if(input$tipo_input == 'Menor que'){h2(strong(withMathJax(paste0("$$P( X <", input$input_x1_me, ") = ", round(prob_reac(), 3), "$$"))))}
    })
    output$Normal_plot <- renderPlot({
        mean = input$mean_input; sd = input$sd_input; x0 = (mean-4*sd); x1 = (mean+4*sd);
        if(input$tipo_input == 'Entre'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x < input$input_x1 | x > input$input_x2] <- NA; return(y)}} else
        if(input$tipo_input == 'Maior que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x < input$input_x1_ma] <- NA; return(y)}} else
        if(input$tipo_input == 'Menor que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x > input$input_x1_me] <- NA; return(y)}}
        ggplot(data.frame(x = c(x0, x1)), aes(x = x)) +
            stat_function(fun = dnorm, args = list(input$mean_input, input$sd_input), size = 1, alpha = 0.8)+
            stat_function(fun=fd, geom="area", fill="blue", alpha=0.2)+
            geom_hline(yintercept = 0, size = 1.1, alpha = 0.9)+
            labs(title = "Probabilidade",
                 x = "Média",
                 #y = "",
                 caption = "Aquarela Advanced Analytics")+
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  #panel.background = element_blank()
                  axis.text.x = element_text(face = "bold", size = 12),
                  plot.caption = element_text(size = 15, face = "bold"))
    })
}
shinyApp(ui = ui, server = server)