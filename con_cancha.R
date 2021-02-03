library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = 'Anotador de tiros'),
  dashboardSidebar(collapsed = TRUE),
  
  dashboardBody(
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
             radioGroupButtons("color", "Gol", c("Gol", "No Gol"), size = 'lg'),
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
    downloadButton("download", "Descargar Sesión", size = 'lg'))
  
  
  
  
  
  
  
)


server <- function(input, output, session) {
  # Create a reactive data.table
  values <- reactiveValues()
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
                          posicion = character())
  
  # Create a plot
  output$plot1 = renderPlot({
      
      handbaloner::court() +
      geom_point(data = values$DT, aes(x = x, y = y), 
                 color = data.table::fifelse(values$DT$gol == 'Gol', 'Green', 'Red'), size = 3)
     
    
    
  })
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click, {
    add_row <- data.table(x = input$plot_click$x,
                          y = input$plot_click$y,
                          gol = data.table::fifelse(input$color == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador,
                          portero = data.table::fifelse(input$tirador == 'Motor', input$portero_visita, input$portero),
                          tipo_de_tiro = paste0(input$art),
                          marco_vacio = input$material_marco,
                          pasivo = input$material_pasivo)[, distancia_a_gol := data.table::fifelse(tipo_de_tiro == '7m', 7,handbaloner::distance_to_goal(c(x , y)))
                                                         ][, larga_distancia := distancia_a_gol >= 9][, posicion := data.table::fcase((x < 0 & y < -2) | (x > 0 & y > 2), 'left',
                                                                                                                                      (x < 0 & y > 2) |  (x > 0 & y < -2), 'right', 
                                                                                                                                      default = 'centre')]
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
    data.table::data.table(Motor = values$DT[tirador != 'Barça',sum(gol == 'Gol')], Barça = values$DT[tirador == 'Barça',sum(gol == 'Gol')])
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
  
  # Timer for the game
  
  timer <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  update_interval = 1 # How many seconds between timer updates?
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time passed: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(round(timer()+update_interval,2))
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$set_time, {timer(input$seconds)})
  
  
}

shinyApp(ui, server)
