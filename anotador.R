library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = 'Anotador de tiros '),
  dashboardSidebar(collapsed = TRUE),
  
  dashboardBody(
    fluidRow(  #hr(),
               actionButton('start','Start'),
               actionButton('stop','Stop'),
               # actionButton('reset','Reset'),
              # tags$hr(),
              textOutput('timeleft'),
              tags$head(tags$style("#timeleft{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
              )
              )
    
),
    
    fluidRow(column(width = 1, radioGroupButtons(
      inputId = "tirador",
      label = "Tirador Casa",
      choices = c("Rafa", "Erick", "O. Solorzano", 'Talavera', 'Freddy', 'Valery', 'Santiago', 
                  "D. Jimenez", "Jorge", "D. Villegas", 'Cristopher', 'O. Lira', 'Esteban', 'Yamal',
                  "Johann", "Julio", "Viggo", 'Bryan R.', 'D. Herrera', 'D. Meza', 'Alejandro',
                  'Justin', 'Emmanuel', 'D. Molina', 'Noe'),
      # size = 'lg',
      direction = 'vertical',
      individual = FALSE,
      status = 'primary', 
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    )),
    column(width = 1, radioGroupButtons(
      inputId = "portero",
      label = "Portero Casa",
      choices = c("Jeff", "Adrian", 'Isaac', 'Mario', 'Bryan'),
      # size = 'lg',
      direction = 'vertical',
      individual = FALSE,
      status = 'primary', 
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    )),
    column(width = 1, radioGroupButtons(
      inputId = "tirador_visita",
      label = "Tirador Visita",
      choices = c("Rafa", "Erick", "O. Solorzano", 'Talavera', 'Freddy', 'Valery', 'Santiago', 
                  "D. Jimenez", "Jorge", "D. Villegas", 'Cristopher', 'O. Lira', 'Esteban', 'Yamal',
                  "Johann", "Julio", "Viggo", 'Bryan R.', 'D. Herrera', 'D. Meza', 'Alejandro',
                  'Justin', 'Emmanuel', 'D. Molina', 'Noe'),
      # size = 'lg',
      direction = 'vertical',
      individual = FALSE,
      status = 'primary', 
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    )),
    column(width = 1, radioGroupButtons(
      inputId = "portero_visita",
      label = "Portero Visita",
      choices = c("Jeff", "Adrian", 'Isaac', 'Mario', 'Bryan'),
      # size = 'lg',
      direction = 'vertical',
      individual = FALSE,
      status = 'primary', 
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    )), 
    column(width = 6,
           plotOutput("plot1", click = "plot_click"),
           actionButton("rem_point", "Quite el último dato"),
           splitLayout(cellWidths = c("50%", "25%", '25%'), radioGroupButtons("color", "Gol", c("Gol", "No Gol"), size = 'lg'), materialSwitch(
             inputId = "material_inferioridad",
             label = "Defensa en Inferioridad", 
             value = FALSE,
             status = "primary"
           ), materialSwitch(
             inputId = "material_marco",
             label = "Marco Vacio", 
             value = FALSE,
             status = "danger"
           )),
           radioGroupButtons(
             inputId = "art", 
             label = "Tipo de Tiro",
             size = 'lg',
             choices = c("7m", "Nueve", 
                         "Seis", "Pivote", "Entrada", "Extremo", "Contraataque", "Tiro Libre Directo", "Propio Campo", "Otros")
           ), radioGroupButtons(
             inputId = "ort", 
             label = "Posicion de Tiro",
             size = 'lg',
             choices = c('Izquierda', 'Centro', 'Derecha')
           )
          ),
    column(width = 4,
           h4("Tiros realizados"),
           tableOutput("table"))),
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
                          hora = character(),
                          tiempo = character(),
                          tipo_de_tiro = character(),
                          posicion_de_tiro = character(),
                          inferioridad = logical(),
                          marco_vacio = logical())
  
  # Create a plot
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      geom_point(color = data.table::fifelse(values$DT$gol == 'Gol', 'Green', 'Red'), size = 6) +
      lims(x = c(-3, 3), y = c(-1, 2)) +
      theme(legend.position = "none") +
      # include so that colors don't change as more colors are chosen
      scale_color_discrete(drop = FALSE) +
      theme_void() +
      geom_segment(aes(x = -1.5, xend = 1.5, y = 1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = -1.5, xend = -1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) 
  })
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click, {
    add_row <- data.table(x = input$plot_click$x,
                          y = input$plot_click$y,
                          gol = ifelse(input$color == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador,
                          portero = input$portero,
                          hora = as.character(Sys.time()),
                          tiempo = timer(),
                          tipo_de_tiro =input$art,
                          posicion_de_tiro = input$ort,
                          inferioridad = input$material_inferioridad,
                          marco_vacio = input$material_marco)
    values$DT <- rbind(values$DT, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  # Render the table
  output$table <- renderTable({
    tail(values$DT, 5)
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
  
  
}

shinyApp(ui, server)
