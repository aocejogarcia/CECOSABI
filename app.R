library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(googlesheets4)

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
                                    fluidRow(
                                      column(4, wellPanel(
                                             checkboxInput(inputId = 'atn2_01',
                                                       label = 'Laboratorio de citologia, histopatologia y anatomia patologica',
                                                       value = F),
                                             numericInput(inputId = 'atn2_01m',
                                                           label = '¿Cuántos médicos hay en este servicio?',
                                                           value = NA_integer_),
                                             numericInput(inputId = 'atn2_01e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_01i',
                                                         label = '¿Cuenta con el insumo para operar el servicio?', 
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_02',
                                                           label = 'Anestesiologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_02m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_02e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_02i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_03',
                                                           label = 'Area de recuperacion postquirurgica',
                                                           value = F),
                                             numericInput(inputId = 'atn2_03m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_03e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_03i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?', 
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_04',
                                                           label = 'Area de triage',
                                                           value = F),
                                             numericInput(inputId = 'atn2_04m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_04e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_04i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?', 
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_05',
                                                           label = 'Asistencia especializada en consulta',
                                                           value = F),
                                             numericInput(inputId = 'atn2_05m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_05e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_05i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                           choices = c('Si', 'No', 'Parcial', 'NA'),
                                                           selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_06',
                                                           label = 'Atencion de enfermeria',
                                                           value = F),
                                             numericInput(inputId = 'atn2_06m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_06e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_06i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                           choices = c('Si', 'No', 'Parcial', 'NA'),
                                                           selected = 'NA'),
                                             
                                             checkboxInput(inputId = 'atn2_07',
                                                           label = 'Atencion paliativa',
                                                           value = F),
                                             numericInput(inputId = 'atn2_07m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_07e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_07i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_08',
                                                           label = 'Audiologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_08m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_08e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_08i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_09',
                                                           label = 'Banco de sangre',
                                                           value = F),
                                             numericInput(inputId = 'atn2_09m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_09e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_09i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_10',
                                                           label = 'Cirugia ambulatoria',
                                                           value = F),
                                             numericInput(inputId = 'atn2_10m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_10e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_10i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_11',
                                                           label = 'Clinica de heridas y estomas',
                                                           value = F),
                                             numericInput(inputId = 'atn2_11m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_11e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_11i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_12',
                                                           label = 'Clinica del dolor',
                                                           value = F),
                                             numericInput(inputId = 'atn2_12m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_12e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_12i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_13',
                                                           label = 'Consulta externa',
                                                           value = F),
                                             numericInput(inputId = 'atn2_13m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_13e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_13i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_14',
                                                           label = 'Dialisis y hemodialisis',
                                                           value = F),
                                             numericInput(inputId = 'atn2_14m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_14e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_14i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_15',
                                                           label = 'Electrocardiograma',
                                                           value = F),
                                             numericInput(inputId = 'atn2_15m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_15e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_15i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_16',
                                                           label = 'Electroencefalograma',
                                                           value = F),
                                             numericInput(inputId = 'atn2_16m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_16e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_16i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA')
                                      )),
                                      column(4, wellPanel(
                                             checkboxInput(inputId = 'atn2_17',
                                                           label = 'Especialidades medico-clinicas o clinico-quirurgicas',
                                                           value = F),
                                             numericInput(inputId = 'atn2_17m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_17e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_17i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_18',
                                                           label = 'Espirometria',
                                                           value = F),
                                             numericInput(inputId = 'atn2_18m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_18e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_18i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_19',
                                                           label = 'Esterilizacion',
                                                           value = F),
                                             numericInput(inputId = 'atn2_19m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_19e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_19i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_20',
                                                           label = 'Estomatologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_20m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_20e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_20i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_21',
                                                           label = 'Farmacia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_21m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_21e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_21i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_22',
                                                           label = 'Hemodinamia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_22m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_22e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_22i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_23',
                                                           label = 'Imagenologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_23m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_23e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_23i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_24',
                                                           label = 'Inhaloterapia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_24m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_24e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_24i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_25',
                                                           label = 'Laboratorio clinico',
                                                           value = F),
                                             numericInput(inputId = 'atn2_25m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_25e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_25i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_26',
                                                           label = 'Medicina preventiva',
                                                           value = F),
                                             numericInput(inputId = 'atn2_26m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_26e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_26i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_27',
                                                           label = 'Modulo de vacunacion',
                                                           value = F),
                                             numericInput(inputId = 'atn2_27m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_27e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_27i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_28',
                                                           label = 'Nutricion',
                                                           value = F),
                                             numericInput(inputId = 'atn2_28m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_28e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_28i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_29',
                                                           label = 'Oftalmologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_29m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_29e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_29i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_30',
                                                           label = 'Oncologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_30m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_30e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_30i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_31',
                                                           label = 'Otorrinolaringologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_31m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_31e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_31i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_32',
                                                           label = 'Planificacion familiar',
                                                           value = F),
                                             numericInput(inputId = 'atn2_32m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_32e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_32i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA')
                                      )),
                                      column(4, wellPanel(
                                             checkboxInput(inputId = 'atn2_33',
                                                           label = 'Quirofanos',
                                                           value = F),
                                             numericInput(inputId = 'atn2_33m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_33e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_33i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_34',
                                                           label = 'Sala de partos',
                                                           value = F),
                                             numericInput(inputId = 'atn2_34m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_34e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_34i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_35',
                                                           label = 'Salud mental y adicciones',
                                                           value = F),
                                             numericInput(inputId = 'atn2_35m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_35e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_35i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_36',
                                                           label = 'Servicio de anestesiologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_36m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_36e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_36i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_37',
                                                           label = 'Servicio de cirugia general',
                                                           value = F),
                                             numericInput(inputId = 'atn2_37m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_37e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_37i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_38',
                                                           label = 'Servicio de ginecologia y obstetricia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_38m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_38e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_38i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_39',
                                                           label = 'Servicio de medicina interna',
                                                           value = F),
                                             numericInput(inputId = 'atn2_39m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_39e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_39i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_40',
                                                           label = 'Servicio de pediatria',
                                                           value = F),
                                             numericInput(inputId = 'atn2_40m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_40e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_40i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_41',
                                                           label = 'Servicio de traumatologia y ortopedia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_41m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_41e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_41i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_42',
                                                           label = 'Servicio de urgencias medico-quirurgicas',
                                                           value = F),
                                             numericInput(inputId = 'atn2_42m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_42e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_42i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_43',
                                                           label = 'Telemedicina',
                                                           value = F),
                                             numericInput(inputId = 'atn2_43m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_43e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_43i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_44',
                                                           label = 'Terapia intensiva',
                                                           value = F),
                                             numericInput(inputId = 'atn2_44m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_44e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_44i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_45',
                                                           label = 'Tococirugia (labor, expulsion, recuperacion)',
                                                           value = F),
                                             numericInput(inputId = 'atn2_45m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_45e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_45i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_46',
                                                           label = 'Trabajo social',
                                                           value = F),
                                             numericInput(inputId = 'atn2_46m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_46e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_46i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_47',
                                                           label = 'Urgencias 24 horas',
                                                           value = F),
                                             numericInput(inputId = 'atn2_47m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_47e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_47i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA'),

                                             checkboxInput(inputId = 'atn2_48',
                                                           label = 'Urologia',
                                                           value = F),
                                             numericInput(inputId = 'atn2_48m',
                                                          label = '¿Cuántos médicos hay en este servicio?',
                                                          value = NA_integer_),
                                             numericInput(inputId = 'atn2_48e',
                                                          label = '¿Cuántos enfermeros hay en este servicio?',
                                                          value = NA_integer_),
                                             selectInput(inputId = 'atn2_48i',
                                                           label = '¿Cuenta con el insumo para operar el servicio?',
                                                         choices = c('Si', 'No', 'Parcial', 'NA'),
                                                         selected = 'NA')
                                      ))
                                  ),
                                  actionButton("submit", "Enviar", class = "btn-primary")
                                  ),
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
