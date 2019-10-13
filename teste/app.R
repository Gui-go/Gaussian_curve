library(shiny)
ui <- fluidPage(
    titlePanel("Probabilidade e Distribuição Normal"),
    sidebarLayout(
        column(2,
               tabsetPanel(
                   tabPanel(title = "Valores",
                            numericInput(inputId = "mean_input_v", label = "Média", value = 0, step = 1),
                            numericInput(inputId = "sd_input_v", label = "Desvio Padrão", value = 1, step = 1, min = 0.1),
                            selectInput(inputId = "tipo_input_v", label = "Tipo de modelo", choices = c("Entre", "Maior que", "Menor que"), selected = "Entre"),
                            conditionalPanel(
                                condition = "input.tipo_input_v == 'Entre'",
                                numericInput(inputId = "input_x1_v", label = "X1", value = -1, step = 1),
                                numericInput(inputId = "input_x2_v", label = "X2", value = 1, step = 1)
                            ),
                            conditionalPanel(
                                condition = "input.tipo_input_v == 'Maior que'",
                                numericInput(inputId = "input_x1_v_ma", label = "X1", value = -1, step = 1)
                            ),
                            conditionalPanel(
                                condition = "input.tipo_input_v == 'Menor que'",
                                numericInput(inputId = "input_x1_v_me", label = "X1", value = 1, step = 1)
                            )),
                   tabPanel(title = "Percentagem",
                            numericInput(inputId = "mean_input_p", label = "Médiaaaaaa", value = 0, step = 1),
                            numericInput(inputId = "sd_input_p", label = "Desvio Padrãoaaaaaaaa", value = 1, step = 1, min = 0.1),
                            selectInput(inputId = "tipo_input_p", label = "Tipo de modeloaaaaaa", choices = c("Entre", "Maior que", "Menor que"), selected = "Entre"),
                            conditionalPanel(
                                condition = "input.tipo_input_p == 'Entre'",
                                numericInput(inputId = "input_x1_p", label = "X1", value = -1, step = 1),
                                numericInput(inputId = "input_x2_p", label = "X2", value = 1, step = 1)
                            ),
                            conditionalPanel(
                                condition = "input.tipo_input_p == 'Maior que'",
                                numericInput(inputId = "input_x1_p_ma", label = "X1", value = -1, step = 1)
                            ),
                            conditionalPanel(
                                condition = "input.tipo_input_p == 'Menor que'",
                                numericInput(inputId = "input_x1_p_me", label = "X1", value = 1, step = 1)
                            ))
               )),
        mainPanel(width = 10,
                  plotOutput("Normal_plot"),
                  uiOutput("Prob_func")
        )
    )
)
server <- function(input, output) {
    library(tidyverse)
    prob_reac <- reactive(
        if(input$tipo_input_v == 'Entre'){(pnorm(q = input$input_x2_v, mean = input$mean_input_v, sd = input$sd_input_v) - pnorm(q = input$input_x1_v, mean = input$mean_input_v, sd = input$sd_input_v))} else
            if(input$tipo_input_v == 'Maior que'){1-pnorm(q = input$input_x1_v_ma, mean = input$mean_input_v, sd = input$sd_input_v)} else
                if(input$tipo_input_v == 'Menor que'){pnorm(q = input$input_x1_v_me, mean = input$mean_input_v, sd = input$sd_input_v)}    
    )
    output$Prob_func <- renderUI({
        if(input$tipo_input_v == 'Entre'){h2(strong(withMathJax(paste0("$$P(", input$input_x1_v, " < X <", input$input_x2_v, ") = ", round(prob_reac(), 3), "$$"))))} else
            if(input$tipo_input_v == 'Maior que'){h2(strong(withMathJax(paste0("$$P( X > ", input$input_x1_v_ma, ") = ", round(prob_reac(), 3), "$$"))))} else
                if(input$tipo_input_v == 'Menor que'){h2(strong(withMathJax(paste0("$$P( X <", input$input_x1_v_me, ") = ", round(prob_reac(), 3), "$$"))))}
    })
    output$Normal_plot <- renderPlot({
        mean = input$mean_input_v; sd = input$sd_input_v; x0 = (mean-4*sd); x1 = (mean+4*sd);
        if(input$tipo_input_v == 'Entre'){fd <- function(x){y <- dnorm(x, mean = input$mean_input_v, sd = input$sd_input_v); y[x < input$input_x1_v | x > input$input_x2_v] <- NA; return(y)}} else
            if(input$tipo_input_v == 'Maior que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input_v, sd = input$sd_input_v); y[x < input$input_x1_v_ma] <- NA; return(y)}} else
                if(input$tipo_input_v == 'Menor que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input_v, sd = input$sd_input_v); y[x > input$input_x1_v_me] <- NA; return(y)}}
        ggplot(data.frame(x = c(x0, x1)), aes(x = x)) +
            stat_function(fun = dnorm, args = list(input$mean_input_v, input$sd_input_v), size = 1, alpha = 0.8)+
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


