library(shiny)
ui <- fluidPage(
    titlePanel("Distribuição de probabilidade Normal"),
    sidebarLayout(
        sidebarPanel(width = 2,
            uiOutput(outputId = "ui_out")
        ),
        mainPanel(width = 10,
           plotOutput("Normal_plot"),
           uiOutput("Prob_func")
        )
    )
)


server <- function(input, output) {
    library(tidyverse)
 
    output$ui_out <- renderUI({
        fluidPage(
            fluidRow(
                selectInput(inputId = "v_p_input", label = "Tipo", choices = c("Por valores", "Por percentagem")),
                conditionalPanel(condition = "input.v_p_input == 'Por valores'",
                                 selectInput(inputId = "tipo_input", label = "Seletor", choices = c("Entre", "Maior que", "Menor que"), selected = "Entre"),
                                 numericInput(inputId = "mean_input", label = "Média", value = 0, step = 1),
                                 numericInput(inputId = "sd_input", label = "Desvio Padrão", value = 1, step = 1, min = 0.1),
                                 conditionalPanel(
                                     condition = "input.tipo_input == 'Entre'",
                                     numericInput(inputId = "input_x1", label = "a", value = -1, step = 1),
                                     numericInput(inputId = "input_x2", label = "b", value = 1, step = 1)
                                 ),
                                 conditionalPanel(
                                     condition = "input.tipo_input == 'Maior que'",
                                     numericInput(inputId = "input_x1_ma", label = "a", value = -1, step = 1)
                                 ),
                                 conditionalPanel(
                                     condition = "input.tipo_input == 'Menor que'",
                                     numericInput(inputId = "input_x1_me", label = "a", value = 1, step = 1)
                                 )),
                conditionalPanel(condition = "input.v_p_input == 'Por percentagem'",
                                 selectInput(inputId = "tipo_p_input", label = "Seletor", choices = c("Entre", "Maior que", "Menor que"), selected = "Entre"),
                                 numericInput(inputId = "mean_p_input", label = "Média", value = 0, step = 1),
                                 numericInput(inputId = "sd_p_input", label = "Desvio Padrão", value = 1, step = 1, min = 0.1),
                                 conditionalPanel(
                                     condition = "input.tipo_p_input == 'Entre'",
                                     numericInput(inputId = "input_p_x1", label = "Probabilidade", value = 0.5, min = 0, max = 1, step = 0.05)
                                 ),
                                 conditionalPanel(
                                     condition = "input.tipo_p_input == 'Maior que'",
                                     numericInput(inputId = "input_p_x1_ma", label = "Probabilidade", value = 0.5, min = 0.5, max = 1, step = 0.05)
                                 ),
                                 conditionalPanel(
                                     condition = "input.tipo_p_input == 'Menor que'",
                                     numericInput(inputId = "input_p_x1_me", label = "Probabilidade", value = 0.5, min = 0, max = 1, step = 0.05)
                                 ))
            )
        )
    })


    
    
    prob_reac <- reactive(
        
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Entre'){(pnorm(q = input$input_x2, mean = input$mean_input, sd = input$sd_input) - pnorm(q = input$input_x1, mean = input$mean_input, sd = input$sd_input))} else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Maior que'){1-pnorm(q = input$input_x1_ma, mean = input$mean_input, sd = input$sd_input)} else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Menor que'){pnorm(q = input$input_x1_me, mean = input$mean_input, sd = input$sd_input)} else

        # if(input$v_p_input == 'Por percentagem' && input$tipo_input == 'Entre' && input$input_p_x1 == 1){(tigerstats::qnormGC(region = "between", area = 0.99999, mean = input$mean_input, sd = input$sd_input, graph = F))} else
        # if(input$v_p_input == 'Por percentagem' && input$tipo_input == 'Maior que' && input$input_p_x1_ma == 1){(tigerstats::qnormGC(region = "above", area = 0.99999, mean = input$mean_input, sd = input$sd_input, graph = F))} else
        # if(input$v_p_input == 'Por percentagem' && input$tipo_input == 'Menor que' && input$input_p_x1_me == 1){(tigerstats::qnormGC(region = "below", area = 0.99999, mean = input$mean_input, sd = input$sd_input, graph = F))} else

        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Entre' && input$input_p_x1 != 1){(tigerstats::qnormGC(region = "between", area = input$input_p_x1, mean = input$mean_p_input, sd = input$sd_p_input, graph = F))} else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Maior que' && input$input_p_x1_ma != 1){(tigerstats::qnormGC(region = "above", area = input$input_p_x1_ma, mean = input$mean_p_input, sd = input$sd_p_input, graph = F))} else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Menor que' && input$input_p_x1_me != 1){(tigerstats::qnormGC(region = "below", area = input$input_p_x1_me, mean = input$mean_p_input, sd = input$sd_p_input, graph = F))} else

        # if(input$v_p_input == 'Por percentagem' && input$tipo_input == 'Entre' && (input$input_p_x1 || input$input_p_x1_ma || input$input_p_x1_me) == 1){(tigerstats::qnormGC(region = "between", area = 0.99999, mean = input$mean_input, sd = input$sd_input, graph = F))} else
        NULL
    )

    output$Prob_func <- renderUI({

        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Entre'){tt <- h2(strong(withMathJax(paste0("$$P(", input$input_x1, " < X <", input$input_x2, ") = ", round(prob_reac(), 3), "$$"))))}else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Maior que'){tt <- h2(strong(withMathJax(paste0("$$P( X > ", input$input_x1_ma, ") = ", round(prob_reac(), 3), "$$"))))}else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Menor que'){tt <- h2(strong(withMathJax(paste0("$$P( X <", input$input_x1_me, ") = ", round(prob_reac(), 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Entre' && input$input_p_x1 == 1){tt <- h2(strong(withMathJax(paste0("$$P(", -Inf, " < X <", Inf, " ) = ", round(input$input_p_x1, 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Entre' && input$input_p_x1 != 1){tt <- h2(strong(withMathJax(paste0("$$P(", round(prob_reac()[1], 3), " < X <", round(prob_reac()[2], 3), ") = ", round(input$input_p_x1, 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Maior que' && input$input_p_x1_ma == 1){tt <- h2(strong(withMathJax(paste0("$$P( X >", -Inf, " ) = ", round(input$input_p_x1_ma, 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Maior que' && input$input_p_x1_ma != 1){tt <- h2(strong(withMathJax(paste0("$$P( X >", round(prob_reac()[1], 3), ") = ", round(input$input_p_x1_ma, 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Menor que' && input$input_p_x1_me == 1){tt <- h2(strong(withMathJax(paste0("$$P( X <", Inf, " ) = ", round(input$input_p_x1_me, 3), "$$"))))}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Menor que' && input$input_p_x1_me != 1){tt <- h2(strong(withMathJax(paste0("$$P( X <", round(prob_reac()[1], 3), ") = ", round(input$input_p_x1_me, 3), "$$"))))}else
        NULL
        
    })

    
    output$Normal_plot <- renderPlot({
        #mean = input$mean_input; sd = input$sd_input; x0 = (mean-4*sd); x1 = (mean+4*sd);

        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Entre'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x < input$input_x1 | x > input$input_x2] <- NA; return(y)}} else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Maior que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x < input$input_x1_ma] <- NA; return(y)}} else
        if(input$v_p_input == 'Por valores' && input$tipo_input == 'Menor que'){fd <- function(x){y <- dnorm(x, mean = input$mean_input, sd = input$sd_input); y[x > input$input_x1_me] <- NA; return(y)}}

        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Entre'){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); y[x < prob_reac()[1] | x > prob_reac()[2]] <- NA; return(y)}} else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Maior que'){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); y[x < prob_reac()[1]] <- NA; return(y)}} else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Menor que'){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); y[x > prob_reac()[1]] <- NA; return(y)}} else

        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Entre' && input$input_p_x1 == 1){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); return(y)}}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Maior que' && input$input_p_x1_ma == 1){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); return(y)}}else
        if(input$v_p_input == 'Por percentagem' && input$tipo_p_input == 'Menor que' && input$input_p_x1_me == 1){fd <- function(x){y <- dnorm(x, mean = input$mean_p_input, sd = input$sd_p_input); return(y)}}else
        NULL

        v <- ggplot(data.frame(x = c((input$mean_input-4*input$sd_input), (input$mean_input+4*input$sd_input))), aes(x = x)) +
            stat_function(fun = dnorm, args = list(input$mean_input, input$sd_input), size = 1, alpha = 0.8)+
            stat_function(fun=fd, geom="area", fill="blue", alpha=0.2)+
            geom_hline(yintercept = 0, size = 1.1, alpha = 0.9)+
            labs(
                #title = "Probabilidade",
                 x = "",
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
        
        p <- ggplot(data.frame(x = c((input$mean_p_input-4*input$sd_p_input), (input$mean_p_input+4*input$sd_p_input))), aes(x = x)) +
            stat_function(fun = dnorm, args = list(input$mean_p_input, input$sd_p_input), size = 1, alpha = 0.8)+
            stat_function(fun=fd, geom="area", fill="blue", alpha=0.2)+
            geom_hline(yintercept = 0, size = 1.1, alpha = 0.9)+
            labs(
                #title = "Probabilidade",
                 x = "",
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
        
        if(input$v_p_input == 'Por valores'){gg <- v}else 
        if(input$v_p_input != 'Por valores'){gg <- p}else
        NULL
        
        gg
    })
    
    

}
shinyApp(ui = ui, server = server)