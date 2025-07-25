library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(googlesheets4)

source('Alejandro.R')
source('Manuel.R')
#setwd('../snsp_inteligencia/CECOSABI/')
#cata <- bind_rows(
#  read_xlsx('ESTABLECIMIENTO_SALUD_202504.xlsx') %>%
#    filter(`CLAVE ESTATUS DE OPERACION` == 1,
#           `CLAVE DE LA ENTIDAD` == '26',
#           `NIVEL ATENCION` != 'NO APLICA',
#           `NOMBRE DE LA INSTITUCION` %in% c('INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO',
#                                             'INSTITUTO MEXICANO DEL SEGURO SOCIAL',
#                                             'SECRETARIA DE LA DEFENSA NACIONAL',
#                                             'SECRETARIA DE MARINA',
#                                             'SECRETARIA DE SALUD',
#                                             'SERVICIOS DE SALUD IMSS BIENESTAR',
#                                             'SERVICIOS MEDICOS ESTATALES',
#                                             'SERVICIOS MEDICOS MUNICIPALES')),
#  read_xlsx('ESTABLECIMIENTO_SALUD_202504.xlsx') %>%
#    filter(`CLAVE ESTATUS DE OPERACION` == 1,
#           `CLAVE DE LA ENTIDAD` == '26',
#           `NIVEL ATENCION` %in% c('SEGUNDO NIVEL', 'TERCER NIVEL'),
#           `NOMBRE DE LA INSTITUCION` %in% c('SERVICIOS MEDICOS PRIVADOS'))
#)

#cata %>%
#  filter(`NOMBRE DE LA INSTITUCION` != 'SERVICIOS MEDICOS PRIVADOS') %>%
#  count(`CLAVE DE LA JURISDICCION`, MUNICIPIO, `NIVEL ATENCION`, `NOMBRE DE LA INSTITUCION`, CLUES, `NOMBRE COMERCIAL`) %>%
#  select(-n) %>%
#  mutate(`NOMBRE DE LA INSTITUCION` = case_when(
#    `NOMBRE DE LA INSTITUCION` == 'INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO' ~ 'ISSSTE',
#    `NOMBRE DE LA INSTITUCION` == 'INSTITUTO MEXICANO DEL SEGURO SOCIAL' ~ 'IMSS',
#    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE LA DEFENSA NACIONAL' ~ 'DEFENSA/MARINA',
#    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE MARINA' ~ 'DEFENSA/MARINA',
#    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE SALUD' ~ 'SSA',
#    `NOMBRE DE LA INSTITUCION` == 'SERVICIOS DE SALUD IMSS BIENESTAR' ~ 'IMSS-Bienestar',
#    `NOMBRE DE LA INSTITUCION` == 'SERVICIOS MEDICOS ESTATALES' ~ 'ISSSTESON',
#    T ~ `NOMBRE DE LA INSTITUCION`
#    )) %>%
#  rename(DSB = `CLAVE DE LA JURISDICCION`, ATN = `NIVEL ATENCION`, INSTITUCION = `NOMBRE DE LA INSTITUCION`, UNIDAD = `NOMBRE COMERCIAL`) %>%
#  saveRDS('clues.rds')
catalogo <- read_rds('clues.rds')

gs4_auth(path = "zippy-acronym-328605-937daf22dbd8.json", email = "captura-2@zippy-acronym-328605.iam.gserviceaccount.com", cache = "secrets")

SHEET_ID <- '1WNVXuo_tJNkSiABswhOtUvUUC9y--ZV74bUWP5UYHow'

# Code to save new responses: ----
saveData <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append(SHEET_ID, data)
}

# Code to read all responses: ----
loadData <- function() {
  read_sheet(SHEET_ID)
}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsMandatory <- c('AREA',	'FECHA', 'DISTRITO',	'MUNICIPIO', 	'LOCALIDAD',	'MOTIVO',	'TIPO')

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"

fieldsAll <- c()


## UI ##
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'FORMULARIO DE CAPTACION DE INFORMACION', titleWidth = '100%'),
                    dashboardSidebar(disable = T,
                                     sidebarMenu()
                    ),
                    dashboardBody(shinyjs::useShinyjs(),
                                  shinyjs::inlineCSS(appCSS),
                                  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                                  div(
                                    id = "Identification",
                                    h3(tags$b('Máscara de captura para monitoreo de acciones extramuros')),
                                    fluidRow(
                                      column(12,
                                             textInput(inputId = 'nombre', label = 'Nombre de quien reporta'),
                                             selectInput(inputId = 'municipio', label = 'Municipio',
                                                         choices = c(sort(unique(catalogo$MUNICIPIO)), ''),
                                                         selected = ''),
                                             selectInput(inputId = 'dependencia', label = 'Dependencia',
                                                         choices = c(sort(unique(catalogo$INSTITUCION)), ''),
                                                         selected = ''),
                                             selectInput(inputId = 'atn',
                                                         label = labelMandatory('Nivel de atencion'),
                                                         choices = c(sort(unique(catalogo$ATN)), ''),
                                                         selected = ''),
                                             selectInput(inputId = 'nom_ins',
                                                         label = labelMandatory('Nombre del lugar'),
                                                         choices = c(sort(unique(catalogo$UNIDAD)), ''),
                                                         selected = ''),
                                             checkboxGroupInput(inputId = 'horario',
                                                                label = 'Horario',
                                                                choices = c('Matutino', 'Vespertino', 'Jornada acumulada'),
                                                                selected = 'Matutino'),
                                             numericInput(inputId = 'tel',
                                                          label = 'Telefono del lugar',
                                                          value = NA_integer_, min = 100000000, max = 9999999999
                                                          )
                                      )
                                    )
                                    ),
                                  div(
                                    id = "2do y 3er nivel",
                                    h3(tags$b('Servicios de Atencion')),
                                    fluidRow(ale, ale2, ale3, manuel, ale4
                                      )
                                  ),
                                  actionButton("submit", "Enviar", class = "btn-primary"),
                                  shinyjs::hidden(
                                    div(
                                      id = "thankyou_msg",
                                      h3("¡Gracias, tu respuesta ha sido enviada satisfactoriamente!"),
                                      actionLink("submit_another", "Enviar otra respuesta")
                                    )
                                  ),
                                  shinyjs::hidden(
                                    span(id = "submit_msg", "Enviando..."),
                                    div(id = "error",
                                        div(br(), tags$b("Error: "), span(id = "error_msg"))
                                    )
                                  )
                                  )
)


server <- function(input, output, session) {

  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )

  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data[['tmt']] <- str_c(input[["tmt"]], collapse = ', ')
    data[["FECHA"]] <- as.character(str_replace_all((format(input[["FECHA"]], "%d-%m-%Y")), pattern = '-', replacement = '/'))
    data <- c(data, #timestamp = epochTime(),
              date = format(Sys.Date(), "%d-%m-%Y"))
    data[["date"]] <- as.character(str_replace_all((data[["date"]]), pattern = '-', replacement = '/'))
    data <- t(data)
    data
  })


  #action to take when submit button is pressed
  #observeEvent(input$submit, {
  #  saveData(formData())
  #})


  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  # action to take when submit button is pressed
  #observeEvent(input$submit, {
  #  saveData(formData())
  #  shinyjs::reset("form")
  #  shinyjs::hide("form")
  #  shinyjs::show("thankyou_msg")
  #})

  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })

  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")

    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })

  output$downloadBtn <- downloadHandler(
    filename = function() {
      sprintf("mimic-google-form_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )

  observeEvent(input$DISTRITO, {
    distritos <- catalogo

    updateSelectInput(session = session,
                      inputId = 'MUNICIPIO',
                      label = 'Seleccione municipio',
                      choices = c(unique(distritos$NOM_MUN[distritos$DISTRITO == input$DISTRITO]), NULL),
                      selected = NULL)

  })

  observeEvent(input$MUNICIPIO, {
    municipios <- catalogo

    updateSelectInput(session = session,
                      inputId = 'LOCALIDAD',
                      label = 'Seleccione localidad',
                      choices = c(unique(municipios$NOM_LOC[municipios$NOM_MUN == input$MUNICIPIO]), NULL),
                      selected = NULL)

  })

}

shinyApp(ui, server, options = list(host = '0.0.0.0', port = 8000))

#shinylive::export(appdir = '../snsp_inteligencia/extramuros', destdir = '../snsp_inteligencia/extramuros/docs')
