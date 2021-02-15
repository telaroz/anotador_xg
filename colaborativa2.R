library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(lubridate)
library(formattable)
library(png)
tabla_para_xg <- data.table::fread('tabla_para_xg.csv')

ui <- dashboardPage(
  dashboardHeader(title = 'Anotador de tiros'),
  
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Marcador de Tiros", tabName = "marcador", icon = icon("people-carry")),
                     menuItem("Resultados", tabName = "resultados", icon = icon("child"))
                   )),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "marcador",
              fluidRow(  #hr(),
                tableOutput('marcador'),
                tags$head(tags$style("#marcador{color: blue;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"
                ))
                
              ),
              
              fluidRow(column(width = 1, radioGroupButtons(
                inputId = "tirador",
                label = "Tirador",
                choices = c('Motor', 'Barça'),
                #size = 'sm',
                direction = 'vertical',
                individual = FALSE,
                status = 'danger',
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"))
              )),
              column(width = 2, radioGroupButtons(
                inputId = "portero",
                label = "Portero Motor",
                choices = c("33 - Maroz, Ivan", '28 - Viunik, Maksym'),
                # size = 'lg',
                direction = 'vertical',
                individual = FALSE,
                status = 'primary', 
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"))
              )),
              column(width = 2, radioGroupButtons(
                inputId = "portero_visita",
                label = "Portero Barça",
                choices = c("1 - Perez de Vargas, G", "36 - Møller, Kevin"),
                # size = 'lg',
                direction = 'vertical',
                individual = FALSE,
                status = 'primary', 
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"))
              ))),
              fluidRow(
                column(width = 6,
                       plotOutput("plot1", click = "plot_click"),
                       actionButton("rem_point", "Quite el último dato")),
                column(width = 6, 
                       radioGroupButtons("color", "Gol", c("Gol", "Parada", 'Fuera'), size = 'lg'),
                       radioGroupButtons(
                         inputId = "art", 
                         label = "Tipo de Tiro",
                         size = 'lg',
                         choices = c("General", "7m", "Penetración", "Extremo","Contragolpe", "Tiro Libre Directo", "Otros")
                       ),
                       materialSwitch(
                         inputId = "material_marco",
                         label = "Marco Vacio", 
                         value = FALSE,
                         status = "danger"
                       ),materialSwitch(
                         inputId = "material_pasivo",
                         label = "Pasivo", 
                         value = FALSE,
                         status = "warning"
                       )
                )),
              column(width = 3,
                     h4("Tiros realizados"),
                     tableOutput("table")),
              downloadButton("download", "Descargar Sesión", size = 'lg')),
      tabItem(tabName = "resultados",
              fluidRow(column(width = 4,
                              tableOutput("tabla_resultados")),
              column(width = 4, tableOutput('porteros1'))))
      
    ))
  
  
  
  
  
)


server <- function(input, output, session) {
  # Create a reactive data.table
  values <- reactiveValues()
  campo <- handbaloner::court()
  values$DT <- data.table(x = numeric(),
                          y = numeric(),
                          gol = character(),
                          tirador = character(),
                          portero = character(),
                          tipo_de_tiro = character(),
                          marco_vacio = logical(),
                          pasivo = logical(),
                          distancia_a_gol = numeric(),
                          larga_distancia = logical(), 
                          posicion = character(), 
                          xg = numeric(),
                          probabilidad_parada = numeric())
  
  # Create a plot
  output$plot1 = renderPlot({

    campo +
    geom_point(data = values$DT, aes(x = x, y = y), 
               color = data.table::fifelse(values$DT$gol == 'Gol', 'Green', 'Red'), size = 3)
    
    
    
  })
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click, {
    add_row <- data.table(x = input$plot_click$x,
                          y = input$plot_click$y,
                          gol = input$color,
                          tirador = input$tirador,
                          portero = data.table::fifelse(input$tirador == 'Motor', input$portero_visita, input$portero),
                          tipo_de_tiro = paste0(input$art),
                          marco_vacio = input$material_marco,
                          pasivo = input$material_pasivo)[, distancia_a_gol := data.table::fifelse(tipo_de_tiro == '7m', 7,handbaloner::distance_to_goal(c(x , y)))
                          ][, larga_distancia := distancia_a_gol >= 9][, posicion := data.table::fcase((x < 0 & y < -2) | (x > 0 & y > 2), 'left',
                                                                                                       (x < 0 & y > 2) |  (x > 0 & y < -2), 'right', 
                                                                                                       default = 'centre')
                          ][tabla_para_xg, xg := i.xg, on = c('tipo_de_tiro', 'larga_distancia', 'marco_vacio')
                          ][, probabilidad_parada := 1 - xg]
    values$DT <- rbind(values$DT, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  # Render the table
  output$table <- renderTable({
    data.table::setDT(tail(values$DT, 5))[5:1]
  })
  
  output$marcador <- renderTable({
    data.table::data.table(Motor = values$DT[tirador != 'Barça', sum(gol == 'Gol')], Barça = values$DT[tirador == 'Barça',sum(gol == 'Gol')])
  })
  
  
  # Add a download button
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      data.table::fwrite(values$DT, file)
    }
  )
  
  
  
  # Mostrar la tabla de resultados
  output$tabla_resultados <- renderTable({
    res <- data.table::copy(values$DT)
    res[, numero_tiro := 1:.N]
    res <- res[, .(numero_tiro, equipo = tirador, probabilidad_parada, resultado = gol)]
    res[, probabilidad_parada := scales::percent(probabilidad_parada)]
    res
  })
  

  
  output$porteros1 <- renderTable({
    
    
    porteros <- data.table::copy(values$DT)
    
    resumen_porteros <- porteros[,.(cantidad_tiros = .N, paradas = sum(gol == 'Parada', na.rm = TRUE), fuera = sum(gol == 'Fuera', na.rm = TRUE)), by = portero
    ][, tiros_a_marco := cantidad_tiros - fuera][, porcentaje_paradas := scales::percent(paradas/tiros_a_marco)]

    data.table::setcolorder(resumen_porteros, c('portero', 'cantidad_tiros', 'fuera', 'tiros_a_marco', 'paradas', 'porcentaje_paradas'))
    
    colnames(resumen_porteros)[1] <- 'portero        '
    resumen_porteros
  })
  
  
  
}

shinyApp(ui, server)
