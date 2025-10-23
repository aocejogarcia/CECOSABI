library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(googlesheets4)
library(stringr)
library(RMySQL)
library(DBI)

#source('Alejandro.R')
#source('Manuel.R')
source('cecosabi-alpha.R')
#source('inputs.R')
#setwd('../snsp_inteligencia/CECOSABI/')


catalogo <- read_rds('clues.rds') 
unidades <- data.frame(
  dependencia = catalogo$INSTITUCION,
  municipio = catalogo$MUNICIPIO,
  unidad = catalogo$UNIDAD
)
#gs4_auth(path = "zippy-acronym-328605-937daf22dbd8.json", email = "captura-2@zippy-acronym-328605.iam.gserviceaccount.com", cache = "secrets")

##SHEET_ID <- '1WNVXuo_tJNkSiABswhOtUvUUC9y--ZV74bUWP5UYHow'
#TABLE_NAME <- 'respuestas'

#conexión



# Code to save new responses: ----
##saveData <- function(data) {
##  data <- data %>% as.list() %>% data.frame()
##  sheet_append(SHEET_ID, data)
##}

# Code to read all responses: ----
##loadData <- function() {
##  read_sheet(SHEET_ID)
##}

#saveData <- function(data) {
#  db <- DBI::dbConnect(MySQL(), dbname = 'dgssp', host = options()$mysql$host, 
#                       port = options()$mysql$port, user = options()$mysql$user, 
#                       password = options()$mysql$password
#                       )
#  query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", TABLE_NAME, 
#                   paste(names(data), collapse = ", "), paste(data, collapse = "', '"))
#  dbGetQuery(db, query)
#  dbDisconnect(db)
#}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsMandatory <- c('nombre', 'municipio')

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"

fieldsAll <- c('nombre', 'dependencia', 'municipio', 'nom_ins', 'tel', 'horario', 'atn_01', 'atn2_01', 'atn2_01h', 'atn2_01m', 'atn2_01e', 'atn2_01con', 'atn2_01fun', 'atn2_49', 'atn2_49h', 'atn2_49t', 'atn2_49fun', 'atn2_03', 'atn2_03h',
               'atn2_03q', 'atn2_03qmi', 'atn2_03me', 'atn2_03e', 'atn2_03fun', 'atn2_04', 'atn2_04h', 'atn2_04pm', 'atn2_04fun', 'atn2_05',  'atn2_05h', 'atn2_05pc', 'atn2_05fun', 'atn2_66', 'atn2_66h', 'atn2_66me', 'atn2_66m', 'atn2_66e', 'atn2_66con', 'atn2_66fun', 'atn2_46', 'atn2_46e',  'atn2_46eq', 'atn2_46est',
               'atn2_46fun', 'atn2_48', 'atn2_48h', 'atn2_48me', 'atn2_48m', 'atn2_48ps', 'atn2_48e', 'atn2_48fun', 'atn2_50',
               'atn2_50h', 'atn2_50n', 'atn2_50fun', 'atn2_85', 'atn2_85h', 'atn2_85e', 'atn2_85rf', 'atn2_85fun', 'atn2_107',
               'pve_88',  'pve_88fu', 'pve_88nf',  'pve_89', 'pve_89fu',  'pve_89nf', 'pve_90',  'pve_90fu', 'pve_90nf',  'pve_91', 'pve_91fu',  'pve_91nf', 'pve_92', 'pve_92fu', 'pve_92nf', 'pve_93',  'pve_93fu', 'pve_93nf', 'pve_94',
               'pve_94fu',  'pve_94nf', 'atn2_02',  'atn2_02h', 'atn2_02me', 'atn2_02m', 'atn2_02e', 'atn2_02con', 'atn2_02fun', 'atn2_108',  'atn2_108h', 'atn2_108me', 'atn2_108m', 'atn2_108e', 'atn2_108c', 'atn2_108fun',
               'atn2_54',  'atn2_54h', 'atn2_54me', 'atn2_54m', 'atn2_54e', 'atn2_54con', 'atn2_54fun', 'atn2_55', 'atn2_55h',  'atn2_55me',
               'atn2_55m', 'atn2_55e', 'atn2_55con', 'atn2_55fun', 'atn2_45',  'atn2_45h', 'atn2_45me',  'atn2_45m', 'atn2_45e', 'atn2_45con', 'atn2_45fun',
               'atn2_56', 'atn2_56h', 'atn2_56me', 'atn2_56m', 'atn2_56e', 'atn2_56c', 'atn2_56fun', 'atn2_57', 'atn2_57h', 'atn2_57me', 'atn2_57m',
               'atn2_57e', 'atn2_57con', 'atn2_57fun', 'atn2_60',  'atn2_60h', 'atn2_60me', 'atn2_60m', 'atn2_60e', 'atn2_60con', 'atn2_60fun', 'atn2_61',
               'atn2_61h', 'atn2_61me', 'atn2_61m', 'atn2_61e', 'atn2_61con', 'atn2_61fun', 'atn2_63', 'atn2_63h', 'atn2_63me', 'atn2_63m', 'atn2_63e',
               'atn2_63con',  'atn2_63fun', 'atn2_89',  'atn2_89h', 'atn2_89me',  'atn2_89m', 'atn2_89e',  'atn2_89con', 'atn2_89fun', 'atn2_87',
               'atn2_87me',  'atn2_87m', 'atn2_87e',  'atn2_87con', 'atn2_87fun', 'atn2_113',  'atn2_113h', 'atn2_113me', 'atn2_113m',  'atn2_113e',  'atn2_113c', 'atn2_113fun', 'atn2_114',
               'atn2_114h', 'atn2_114me', 'atn2_114m',  'atn2_114e',  'atn2_114c',  'atn2_114fun',  'atn2_116',  'atn2_116h',  'atn2_116me', 'atn2_116m',  'atn2_116e', 'atn2_116c',
               'atn2_116fun', 'atn2_118',  'atn2_118h', 'atn2_118me', 'atn2_118m', 'atn2_118e', 'atn2_118c', 'atn2_118fun',  'atn2_119',  'atn2_119h',  'atn2_119me', 'atn2_119m', 'atn2_119e',
               'atn2_119c', 'atn2_119fun', 'atn2_120', 'atn2_120h',  'atn2_120me',  'atn2_120m', 'atn2_120e', 'atn2_120c',  'atn2_120fun',  'atn2_121',  'atn2_121h',  'atn2_121me',
               'atn2_121m',  'atn2_121e',  'atn2_121c',  'atn2_121fun',  'atn2_122',  'atn2_122h',  'atn2_122me',  'atn2_122m', 'atn2_122e',  'atn2_122c',  'atn2_122fun',  'atn2_123',
               'atn2_123h',  'atn2_123me',  'atn2_123m', 'atn2_123e',  'atn2_123c',  'atn2_123fun', 'atn2_124',  'atn2_124h', 'atn2_124me',  'atn2_124m',  'atn2_124e',  'atn2_124c',
               'atn2_124fun',  'atn2_125', 'atn2_125h', 'atn2_125me', 'atn2_125m',  'atn2_125e', 'atn2_125c',  'atn2_125fun',  'atn2_126',  'atn2_126h',  'atn2_126me',  'atn2_126m',
               'atn2_126e',  'atn2_126c',  'atn2_126fun', 'atn2_127',  'atn2_127h', 'atn2_127me',  'atn2_127m',  'atn2_127e',  'atn2_127c',  'atn2_127fun',  'atn2_128',  'atn2_128h', 'atn2_128me', 'atn2_128m', 'atn2_128e',  'atn2_128c',  'atn2_128fun',  'atn2_129',  'atn2_129h',  'atn2_129me',  'atn2_129m',  'atn2_129e', 'atn2_129c',  'atn2_129fun',
               'atn2_130',  'atn2_130h', 'atn2_130me', 'atn2_130m',  'atn2_130e',  'atn2_130c', 'atn2_130fun', 'atn2_131',  'atn2_131h',  'atn2_131me',  'atn2_131m',  'atn2_131e',
               'atn2_131c',  'atn2_131fun',  'atn2_133',  'atn2_133h', 'atn2_133me', 'atn2_133m', 'atn2_133e',  'atn2_133c', 'atn2_133fun',  'atn2_134',  'atn2_134h',  'atn2_134me',
               'atn2_134m', 'atn2_134e', 'atn2_134c', 'atn2_134fun', 'atn2_135',  'atn2_135h',  'atn2_135me',  'atn2_135m', 'atn2_135e',  'atn2_135c', 'atn2_135fun', 'atn2_137',  'atn2_137h', 'atn2_137me', 'atn2_137m', 'atn2_137e', 'atn2_137c', 'atn2_137fun',  'atn2_138',  'atn2_138h',  'atn2_138me',  'atn2_138m',  'atn2_138e',  'atn2_138c',
               'atn2_138fun', 'atn2_139',  'atn2_139h', 'atn2_139me',  'atn2_139m', 'atn2_139e', 'atn2_139c', 'atn2_139fun', 'atn2_140',  'atn2_140h', 'atn2_140me',  'atn2_140m',
               'atn2_140e', 'atn2_140c', 'atn2_140fun', 'atn2_141',  'atn2_141h', 'atn2_141me', 'atn2_141m', 'atn2_141e', 'atn2_141c', 'atn2_141fun', 'atn2_06',  'atn2_06me',
               'atn2_06m', 'atn2_06e', 'atn2_06fun', 'atn2_06h', 'atn2_06tr',  'atn2_06cam', 'atn2_06ch', 'atn2_06ais', 'atn2_07',  'atn2_07h', 'atn2_07me', 'atn2_07m', 'atn2_07e',
               'atn2_07pm',  'atn2_07fun',  'atn2_08',  'atn2_08h',  'atn2_08me',  'atn2_08m',  'atn2_08e',  'atn2_08op', 'atn2_08cam', 'atn2_08fun', 'atn2_11',  'atn2_11h',  'atn2_11me',
               'atn2_11m',  'atn2_11e',  'atn2_11op',  'atn2_11cam',  'atn2_11fun',  'atn2_09',  'atn2_09h', 'atn2_09e', 'atn2_09fun', 'atn2_10',  'atn2_10h', 'atn2_10me',  'atn2_10m',
               'atn2_10e',  'atn2_10op',  'atn2_10fun',  'atn2_10q',  'atn2_12',  'atn2_12h',  'atn2_12me',  'atn2_12m', 'atn2_12e',  'atn2_12fun', 'atn2_12op', 'atn2_12q',
               'atn2_84', 'atn2_84h', 'atn2_84me',  'atn2_84m',  'atn2_84fun',  'atn2_84e',  'atn2_53',  'atn2_53h', 'atn2_53me', 'atn2_53m', 'atn2_53fun', 'atn2_53e',  'atn2_62',  
               'atn2_62me',  'atn2_62m',  'atn2_62e',  'atn2_62fun',  'atn2_64',  'atn2_64h',  'atn2_64me', 'atn2_64m', 'atn2_64e',  'atn2_64fun', 'atn2_65',  'atn2_65h',
               'atn2_65me', 'atn2_65m', 'atn2_65e',  'atn2_65con', 'atn2_65fun', 'atn2_67',  'atn2_67h', 'atn2_67me', 'atn2_67m', 'atn2_67e', 'atn2_67con', 'atn2_67fun',
               'atn2_69',  'atn2_69h',  'atn2_69me',  'atn2_69m', 'atn2_69e', 'atn2_69fun', 'atn2_88', 'atn2_88h', 'atn2_88me',  'atn2_88m',  'atn2_88e',  'atn2_88fun',
               'atn2_47',  'atn2_47h', 'atn2_47o', 'atn2_47me', 'atn2_47m',  'atn2_47e', 'atn2_47fun', 'atn2_59',  'atn2_59h', 'atn2_59ps', 'atn2_59fun',  'atn2_51',  'atn2_51h',
               'atn2_51a',  'atn2_51fun',  'atn2_52',  'atn2_52h', 'atn2_52a',  'atn2_52fun',  'atn2_70',  'atn2_70h',  'atn2_70me', 'atn2_70m', 'atn2_70e', 'atn2_70pm',  'atn2_70fun',
               'atn2_68',  'atn2_68h',  'atn2_68me', 'atn2_68m', 'atn2_68ft', 'atn2_68e', 'atn2_68fun', 'atn2_15',  'atn2_15h',  'atn2_15pm',  'atn2_15carg',  'atn2_15ah',
               'atn2_15fun',  'atn2_15d',  'atn2_23',  'atn2_23h',  'atn2_23fun',  'atn2_23pm',  'atn2_16',  'atn2_16h',  'atn2_16pm',  'atn2_16fun',  'atn2_22', 'atn2_22h',
               'atn2_22pm',  'atn2_22i', 'atn2_22fun', 'atn2_17',  'atn2_17h', 'atn2_17pm',  'atn2_17fun', 'atn2_18',  'atn2_18h', 'atn2_18pm', 'atn2_18fun',
               'atn2_20',  'atn2_20h', 'atn2_20pm',  'atn2_20fun', 'atn2_19', 'atn2_19h', 'atn2_19pm',  'atn2_19se',  'atn2_19fun', 'atn2_21',  'atn2_21baf', 'atn2_21ban',  'atn2_21fun',
               'atn2_24', 'atn2_24pm',  'atn2_24fun', 'atn2_25',  'atn2_25m', 'atn2_25fun', 'atn2_26',  'atn2_27',  'atn2_28',  'atn2_29',  'atn2_30',  'atn2_31', 'atn2_32',  'atn2_33',
               'atn2_34',  'atn2_35',  'atn2_40',  'atn2_36',  'atn2_37',  'atn2_38',  'atn2_39',  'atn2_41',  'atn2_42',  'atn2_106',  'atn2_95',  'atn2_95sr',  'atn2_95fu',  'atn2_95nf',
               'atn2_95fun', 'atn2_97',  'atn2_97fu', 'atn2_97nnf',  'atn2_98',  'atn2_98fu', 'atn2_98nf',  'atn2_99',  'atn2_99fu', 'atn2_99nf',  'atn2_100', 'atn2_100fu',   'atn2_100nf',
               'atn2_101',  'atn2_101fu',  'atn2_101nf', 'atn2_102',  'atn2_102fu', 'atn2_102nf', 'atn2_103', 'atn2_103fu', 'atn2_103nf',  'atn2_104',  'atn2_104fu',  'atn2_104nf',
               'atn2_71', 'atn2_71h', 'atn2_71pc', 'atn2_71fu',  'atn2_71nf',  'atn2_72',  'atn2_72h',  'atn2_72pc', 'atn2_72fu',  'atn2_72nf', 'atn2_72fun', 'atn2_73', 'atn2_73h',
               'atn2_73pc', 'atn2_73fu',  'atn2_73nf', 'atn2_77',  'atn2_77h', 'atn2_77pc', 'atn2_77es',  'atn2_77fu',  'atn2_77nf',  'atn2_79', 'atn2_79h', 'atn2_79pc', 'atn2_79fu',  
               'atn2_79nf', 'atn2_83', 'atn2_83h', 'atn2_83pc', 'atn2_83fu',  'atn2_83nf', 'atn2_74',  'atn2_74h', 'atn2_74me',  'atn2_74m',  'atn2_74e',  'atn2_74fu',  'atn2_74nf',  
               'atn2_75',  'atn2_75h', 'atn2_75me', 'atn2_75m', 'atn2_75e', 'atn2_75fu',   'atn2_75nf', 'atn2_76',  'atn2_76h',  'atn2_76me',  'atn2_76m',  'atn2_76e', 'atn2_76fu',
               'atn2_76nf', 'atn2_78', 'atn2_78h', 'atn2_78me', 'atn2_78m', 'atn2_78e', 'atn2_78es', 'atn2_78fu', 'atn2_78nf',  'atn2_80', 'atn2_80h',  'atn2_80me', 'atn2_80m',
               'atn2_80e', 'atn2_80fu',  'atn2_80nf',  'atn2_81',  'atn2_81h', 'atn2_81me',  'atn2_81m',  'atn2_81e', 'atn2_81aud',  'atn2_81fu', 'atn2_81nf',  'atn2_82',  'atn2_82h',
               'atn2_82me', 'atn2_82m', 'atn2_82e', 'atn2_82ec',  'atn2_82fu', 'atn2_82nf', 'atn3_26', 'atn3_26h', 'atn3_26a', 'atn3_26fun', 'atn3_28',  'atn3_28h',  'atn3_28a', 'atn3_28fun', 'atn3_30',  'atn3_30h',
               'atn3_30a', 'atn3_30fun', 'atn3_31',  'atn3_31h', 'atn3_31a', 'atn3_31fun', 'atn3_33', 'atn3_33h', 'atn3_33a', 'atn3_33fun', 'atn3_37', 'atn3_37h', 'atn3_37a', 'atn3_37fun', 'atn3_38',
               'atn3_38h', 'atn3_38a', 'atn3_38fun', 'atn3_42', 'atn3_42h', 'atn3_42a', 'atn3_42fun', 'atn3_47', 'atn3_47h', 'atn3_47a', 'atn3_47fun', 'atn3_48', 'atn3_48h', 'atn3_48a', 'atn3_48fun',
               'atn3_50',  'atn3_50h', 'atn3_50a', 'atn3_50fun', 'atn3_52', 'atn3_52h', 'atn3_52a', 'atn3_52fun', 'atn3_54',  'atn3_54h', 'atn3_54a', 'atn3_54fun', 'atn3_55',  'atn3_55h', 'atn3_55a',
               'atn3_55fun', 'atn3_59', 'atn3_59h', 'atn3_59a', 'atn3_59fun', 'atn3_60',  'atn3_60h', 'atn3_60a', 'atn3_60fun', 'atn3_61',  'atn3_61h', 'atn3_61a', 'atn3_61fun',  'atn2_44', 'atn2_44h', 'atn2_44me', 'atn2_44m',
               'atn2_44e', 'atn2_44con', 'atn2_44fun', 'atn2_13', 'atn2_13h', 'atn2_13me', 'atn2_13m', 'atn2_13e', 'atn2_13op', 'atn2_13fun', 'atn2_13ch', 'atn2_14', 'atn2_14h', 'atn2_14me', 'atn2_14m', 'atn2_14e',
               'atn2_14op', 'atn2_14fun', 'atn2_14q',  'atn2_14qt', 'atn2_105', 'atn3_01',  'atn3_68', 'atn3_04', 'atn3_04h', 'atn3_04fun', 'atn3_05', 'atn3_05h', 'atn3_05fun', 'atn3_06',  'atn3_06h', 'atn3_06fun', 'atn3_07',
               'atn3_07h', 'atn3_07fun', 'atn3_09',  'atn3_09h', 'atn3_09fun', 'atn3_10',  'atn3_10h', 'atn3_10fun', 'atn3_11',  'atn3_11h', 'atn3_11fun', 'atn3_12', 'atn3_12h', 'atn3_12fun', 'atn3_13', 'atn3_13h', 'atn3_13fun',
               'atn3_14',  'atn3_14h', 'atn3_14fun', 'atn3_15',  'atn3_15h', 'atn3_15fun', 'atn3_16',  'atn3_16h', 'atn3_16fun', 'atn3_17',  'atn3_17h', 'atn3_17fun', 'atn3_18',  'atn3_18h', 'atn3_18fun', 'atn3_19', 'atn3_19h',
               'atn3_19fun', 'atn3_21', 'atn3_21h', 'atn3_21fun', 'atn3_22',  'atn3_22h', 'atn3_22fun', 'atn3_23',  'atn3_23h', 'atn3_23fun', 'atn3_24', 'atn3_24h', 'atn3_24fun', 'atn3_20', 'atn3_20h', 'atn3_20fun', 'atn3_20fu',
               'atn3_20nf', 'atn3_62', 'atn3_62fu', 'atn3_62nf', 'atn3_65', 'atn3_65fu', 'atn3_65nf', 'atn3_66',  'atn3_66t',  'atn3_66fu',  'atn3_66nf', 'atn3_69',  'atn3_69h', 'atn3_69fu', 'atn3_69nf', 'atn3_69fun',
               'atn3_71',  'atn3_71a',  'atn3_72', 'atn3_72h', 'atn3_72fu',  'atn3_72nf',  'atn2_96',  'atn2_96sr', 'atn2_96fu', 'atn2_96nf', 'atn2_96fun'
)


## UI ##
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = '', 
                                    titleWidth = '100%'
                                    ),
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
      .skin-red .main-header .logo {
         background-color: #a32a5f;
      }
      .skin-red .main-header .navbar {
         background-color: #a32a5f;
      }
       .skin-red .main-sidebar {
         background-color: #808080;
      }
       .content-red, .right-side {
          background-color: #FFFFFF;
      }
    '))),
                                  div(
                                    id = "Identification",
                                    h1(tags$b(img(src= 'logo_ssa.png', width='10%'),'DIRECCIÓN GENERAL DE SERVICIOS DE SALUD PÚBLICA EN SONORA'), align = 'center'),
                                    h3(tags$b('Censo de servicios e infraestructura en salud'), align = 'center'),
                                    column(12, wellPanel(
                                    p(strong('Instrucciones generales:'), 'Por favor lea cuidadosamente cada sección y complete los campos que se le solicitan con la información más actualizada y precisa posible. Seleccione solo las opciones aplicables y evite dejar reactivos sin responder.'),
                                    p(em('Nota:'), 'Toda la información registrada en este formulario es de uso exclusivo para fines técnicos de diagnóstico, planeación estratégica y mejora del acceso a servicios de salud en el estado de Sonora.'))),
                                    br(),
                                    fluidRow(
                                      column(6, wellPanel(
                                             textInput(inputId = 'nombre', label = 'Nombre de quien reporta (nombre(s)-apellido paterno-apellido materno):'),
                                             selectInput(inputId = "dependencia", label= "Institución:", choices = c(sort(unique(unidades$dependencia)), 'Sin selección'), selected = 'Sin selección'),
                                             selectInput(inputId = "municipio", label =  "Municipio:", choices = NULL)
                                             #selectInput(inputId = 'atn',
                                             #             label = labelMandatory('Nivel de atención'),
                                             #            choices = c(sort(unique(catalogo$ATN)), 'Sin seleccion'), selected = 'Sin seleccion'
                                            #             ),
                                      )),
                                      column(6, wellPanel(
                                             selectInput(inputId = "nom_ins", label =  "Nombre de la unidad:", choices = NULL),
                                             textInput(inputId = 'tel',
                                                          label = 'Teléfono del lugar:'
                                                          ),
                                             checkboxGroupInput(inputId = 'horario',
                                                                label = 'Horario en el que la unidad de salud ofrece servicios a la población:',
                                                                choices = c('Matutino', 'Vespertino', 'Jornada acumulada'),
                                                                selected = NULL)
                                      )
                                      )
                                    )
                                    ),
                                  div(
                                    id = "2do y 3er nivel",
                                    h4(strong('Nivel de atención')),
                                    h5('Indique el nivel de atención de su unidad para habilitar los reactivos correspondientes al mismo.'),
                                    niv,
                                    #fluidRow(n1
                                    #  ),
                                    cond0, cond1, cond2
                                  ),
                                  actionButton("submit", "Enviar", class = "btn-primary"),
                                  #actionButton("probar_conexion", "Probar conexión", class = "btn-primary"), #botón para probar conexión
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
    #data[['tmt']] <- str_c(input[["tmt"]], collapse = ', ')
    #data[["FECHA"]] <- as.character(str_replace_all((format(input[["FECHA"]], "%d-%m-%Y")), pattern = '-', replacement = '/'))
    #data <- c(data, #timestamp = epochTime(),
    #          date = format(Sys.Date(), "%d-%m-%Y"))
    #data[["date"]] <- as.character(str_replace_all((data[["date"]]), pattern = '-', replacement = '/'))
    #data[['horario']] <- as.character(input[['horario']])
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
  
  #boton para probar conexion
  #observeEvent(input$probar_conexion, {
  #  tryCatch({
  #    con <- dbConnect(RMySQL::MySQL(),
  #                     user = "remote",
  #                     password = "1234",
  #                     host = "10.10.101.99",
  #                     dbname = "dgssp",
  #                     port = 3306)
  #    showModal(modalDialog("Conexión exitosa"))
  #    dbDisconnect(con)
  #  }, error = function(e) {
  #    showModal(modalDialog("Error: ", e$message))
  #  })
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
      options(mysql = list(
        "host" = "5.tcp.ngrok.io", #tcp://
        "port" = 29397,
        "user" = "remote",
        "password" = "1234"
      ))
      
      db <- DBI::dbConnect(RMySQL::MySQL(), dbname = 'dgssp', host = options()$mysql$host, 
                           port = options()$mysql$port, user = options()$mysql$user, 
                           password = options()$mysql$password
      )
      
      #Se crean objetos por cada columna
      nombre <-  input$nombre
      dependencia <-  input$dependencia
      municipio <-  input$municipio
      nom_ins <-  input$nom_ins
      tel <-  input$tel
      atn_01 <-  input$atn_01
      atn2_01 <-  input$atn2_01
      atn2_01m <-  input$atn2_01m
      atn2_01e <-  input$atn2_01e
      atn2_01con <-  input$atn2_01con
      atn2_01fun <-  input$atn2_01fun
      atn2_49 <-  input$atn2_49
      atn2_49t <-  input$atn2_49t
      atn2_49fun <-  input$atn2_49fun
      atn2_03 <-  input$atn2_03
      atn2_03q <-  input$atn2_03q
      atn2_03qmi <-  input$atn2_03qmi
      atn2_03me <-  input$atn2_03me
      atn2_03e <-  input$atn2_03e
      atn2_03fun <-  input$atn2_03fun
      atn2_04 <-  input$atn2_04
      atn2_04pm <-  input$atn2_04pm
      atn2_04fun <-  input$atn2_04fun
      atn2_05 <-  input$atn2_05
      atn2_05pc <-  input$atn2_05pc
      atn2_05fun <-  input$atn2_05fun
      atn2_66 <-  input$atn2_66
      atn2_66me <-  input$atn2_66me
      atn2_66m <-  input$atn2_66m
      atn2_66e <-  input$atn2_66e
      atn2_66con <-  input$atn2_66con
      atn2_66fun <-  input$atn2_66fun
      atn2_46 <-  input$atn2_46
      atn2_46e <-  input$atn2_46e
      atn2_46eq <-  input$atn2_46eq
      atn2_46est <-  input$atn2_46est
      atn2_46fun <-  input$atn2_46fun
      atn2_48 <-  input$atn2_48
      atn2_48me <-  input$atn2_48me
      atn2_48m <-  input$atn2_48m
      atn2_48ps <-  input$atn2_48ps
      atn2_48e <-  input$atn2_48e
      atn2_48fun <-  input$atn2_48fun
      atn2_50 <-  input$atn2_50
      atn2_50n <-  input$atn2_50n
      atn2_50fun <-  input$atn2_50fun
      atn2_85 <-  input$atn2_85
      atn2_85e <-  input$atn2_85e
      atn2_85rf <-  input$atn2_85rf
      atn2_85fun <-  input$atn2_85fun
      pve_88 <-  input$pve_88
      pve_88fu <-  input$pve_88fu
      pve_88nf <-  input$pve_88nf
      pve_89 <-  input$pve_89
      pve_89fu <-  input$pve_89fu
      pve_89nf <-  input$pve_89nf
      pve_90 <-  input$pve_90
      pve_90fu <-  input$pve_90fu
      pve_90nf <-  input$pve_90nf
      pve_91 <-  input$pve_91
      pve_91fu <-  input$pve_91fu
      pve_91nf <-  input$pve_91nf
      pve_92 <-  input$pve_92
      pve_92fu <-  input$pve_92fu
      pve_92nf <-  input$pve_92nf
      pve_93 <-  input$pve_93
      pve_93fu <-  input$pve_93fu
      pve_93nf <-  input$pve_93nf
      pve_94 <-  input$pve_94
      pve_94fu <-  input$pve_94fu
      pve_94nf <-  input$pve_94nf
      atn2_02 <-  input$atn2_02
      atn2_02me <-  input$atn2_02me
      atn2_02m <-  input$atn2_02m
      atn2_02e <-  input$atn2_02e
      atn2_02con <-  input$atn2_02con
      atn2_02fun <-  input$atn2_02fun
      atn2_108 <-  input$atn2_108
      atn2_108me <-  input$atn2_108me
      atn2_108m <-  input$atn2_108m
      atn2_108e <-  input$atn2_108e
      atn2_108c <-  input$atn2_108c
      atn2_108fun <-  input$atn2_108fun
      atn2_54 <-  input$atn2_54
      atn2_54me <-  input$atn2_54me
      atn2_54m <-  input$atn2_54m
      atn2_54e <-  input$atn2_54e
      atn2_54con <-  input$atn2_54con
      atn2_54fun <-  input$atn2_54fun
      atn2_55 <-  input$atn2_55
      atn2_55me <-  input$atn2_55me
      atn2_55m <-  input$atn2_55m
      atn2_55e <-  input$atn2_55e
      atn2_55con <-  input$atn2_55con
      atn2_55fun <-  input$atn2_55fun
      atn2_45 <-  input$atn2_45
      atn2_45me <-  input$atn2_45me
      atn2_45m <-  input$atn2_45m
      atn2_45e <-  input$atn2_45e
      atn2_45con <-  input$atn2_45con
      atn2_45fun <-  input$atn2_45fun
      atn2_56 <-  input$atn2_56
      atn2_56me <-  input$atn2_56me
      atn2_56m <-  input$atn2_56m
      atn2_56e <-  input$atn2_56e
      atn2_56c <-  input$atn2_56c
      atn2_56fun <-  input$atn2_56fun
      atn2_57 <-  input$atn2_57
      atn2_57me <-  input$atn2_57me
      atn2_57m <-  input$atn2_57m
      atn2_57e <-  input$atn2_57e
      atn2_57con <-  input$atn2_57con
      atn2_57fun <-  input$atn2_57fun
      atn2_60 <-  input$atn2_60
      atn2_60me <-  input$atn2_60me
      atn2_60m <-  input$atn2_60m
      atn2_60e <-  input$atn2_60e
      atn2_60con <-  input$atn2_60con
      atn2_60fun <-  input$atn2_60fun
      atn2_61 <-  input$atn2_61
      atn2_61me <-  input$atn2_61me
      atn2_61m <-  input$atn2_61m
      atn2_61e <-  input$atn2_61e
      atn2_61con <-  input$atn2_61con
      atn2_61fun <-  input$atn2_61fun
      atn2_63 <-  input$atn2_63
      atn2_63me <-  input$atn2_63me
      atn2_63m <-  input$atn2_63m
      atn2_63e <-  input$atn2_63e
      atn2_63con <-  input$atn2_63con
      atn2_63fun <-  input$atn2_63fun
      atn2_89 <-  input$atn2_89
      atn2_89me <-  input$atn2_89me
      atn2_89m <-  input$atn2_89m
      atn2_89e <-  input$atn2_89e
      atn2_89con <-  input$atn2_89con
      atn2_89fun <-  input$atn2_89fun
      atn2_87 <-  input$atn2_87
      atn2_87me <-  input$atn2_87me
      atn2_87m <-  input$atn2_87m
      atn2_87e <-  input$atn2_87e
      atn2_87con <-  input$atn2_87con
      atn2_87fun <-  input$atn2_87fun
      atn2_113 <-  input$atn2_113
      atn2_113me <-  input$atn2_113me
      atn2_113m <-  input$atn2_113m
      atn2_113e <-  input$atn2_113e
      atn2_113c <-  input$atn2_113c
      atn2_113fun <-  input$atn2_113fun
      atn2_114 <-  input$atn2_114
      atn2_114me <-  input$atn2_114me
      atn2_114m <-  input$atn2_114m
      atn2_114e <-  input$atn2_114e
      atn2_114c <-  input$atn2_114c
      atn2_114fun <-  input$atn2_114fun
      atn2_116 <-  input$atn2_116
      atn2_116me <-  input$atn2_116me
      atn2_116m <-  input$atn2_116m
      atn2_116e <-  input$atn2_116e
      atn2_116c <-  input$atn2_116c
      atn2_116fun <-  input$atn2_116fun
      atn2_118 <-  input$atn2_118
      atn2_118me <-  input$atn2_118me
      atn2_118m <-  input$atn2_118m
      atn2_118e <-  input$atn2_118e
      atn2_118c <-  input$atn2_118c
      atn2_118fun <-  input$atn2_118fun
      atn2_119 <-  input$atn2_119
      atn2_119me <-  input$atn2_119me
      atn2_119m <-  input$atn2_119m
      atn2_119e <-  input$atn2_119e
      atn2_119c <-  input$atn2_119c
      atn2_119fun <-  input$atn2_119fun
      atn2_120 <-  input$atn2_120
      atn2_120me <-  input$atn2_120me
      atn2_120m <-  input$atn2_120m
      atn2_120e <-  input$atn2_120e
      atn2_120c <-  input$atn2_120c
      atn2_120fun <-  input$atn2_120fun
      atn2_121 <-  input$atn2_121
      atn2_121me <-  input$atn2_121me
      atn2_121m <-  input$atn2_121m
      atn2_121e <-  input$atn2_121e
      atn2_121c <-  input$atn2_121c
      atn2_121fun <-  input$atn2_121fun
      atn2_122 <-  input$atn2_122
      atn2_122me <-  input$atn2_122me
      atn2_122m <-  input$atn2_122m
      atn2_122e <-  input$atn2_122e
      atn2_122c <-  input$atn2_122c
      atn2_122fun <-  input$atn2_122fun
      atn2_123 <-  input$atn2_123
      atn2_123me <-  input$atn2_123me
      atn2_123m <-  input$atn2_123m
      atn2_123e <-  input$atn2_123e
      atn2_123c <-  input$atn2_123c
      atn2_123fun <-  input$atn2_123fun
      atn2_124 <-  input$atn2_124
      atn2_124me <-  input$atn2_124me
      atn2_124m <-  input$atn2_124m
      atn2_124e <-  input$atn2_124e
      atn2_124c <-  input$atn2_124c
      atn2_124fun <-  input$atn2_124fun
      atn2_125 <-  input$atn2_125
      atn2_125me <-  input$atn2_125me
      atn2_125m <-  input$atn2_125m
      atn2_125e <-  input$atn2_125e
      atn2_125c <-  input$atn2_125c
      atn2_125fun <-  input$atn2_125fun
      atn2_126 <-  input$atn2_126
      atn2_126me <-  input$atn2_126me
      atn2_126m <-  input$atn2_126m
      atn2_126e <-  input$atn2_126e
      atn2_126c <-  input$atn2_126c
      atn2_126fun <-  input$atn2_126fun
      atn2_127 <-  input$atn2_127
      atn2_127me <-  input$atn2_127me
      atn2_127m <-  input$atn2_127m
      atn2_127e <-  input$atn2_127e
      atn2_127c <-  input$atn2_127c
      atn2_127fun <-  input$atn2_127fun
      atn2_128 <-  input$atn2_128
      atn2_128me <-  input$atn2_128me
      atn2_128m <-  input$atn2_128m
      atn2_128e <-  input$atn2_128e
      atn2_128c <-  input$atn2_128c
      atn2_128fun <-  input$atn2_128fun
      atn2_129 <-  input$atn2_129
      atn2_129me <-  input$atn2_129me
      atn2_129m <-  input$atn2_129m
      atn2_129e <-  input$atn2_129e
      atn2_129c <-  input$atn2_129c
      atn2_129fun <-  input$atn2_129fun
      atn2_130 <-  input$atn2_130
      atn2_130me <-  input$atn2_130me
      atn2_130m <-  input$atn2_130m
      atn2_130e <-  input$atn2_130e
      atn2_130c <-  input$atn2_130c
      atn2_130fun <-  input$atn2_130fun
      atn2_131 <-  input$atn2_131
      atn2_131me <-  input$atn2_131me
      atn2_131m <-  input$atn2_131m
      atn2_131e <-  input$atn2_131e
      atn2_131c <-  input$atn2_131c
      atn2_131fun <-  input$atn2_131fun
      atn2_133 <-  input$atn2_133
      atn2_133me <-  input$atn2_133me
      atn2_133m <-  input$atn2_133m
      atn2_133e <-  input$atn2_133e
      atn2_133c <-  input$atn2_133c
      atn2_133fun <-  input$atn2_133fun
      atn2_134 <-  input$atn2_134
      atn2_134me <-  input$atn2_134me
      atn2_134m <-  input$atn2_134m
      atn2_134e <-  input$atn2_134e
      atn2_134c <-  input$atn2_134c
      atn2_134fun <-  input$atn2_134fun
      atn2_135 <-  input$atn2_135
      atn2_135me <-  input$atn2_135me
      atn2_135m <-  input$atn2_135m
      atn2_135e <-  input$atn2_135e
      atn2_135c <-  input$atn2_135c
      atn2_135fun <-  input$atn2_135fun
      atn2_137 <-  input$atn2_137
      atn2_137me <-  input$atn2_137me
      atn2_137m <-  input$atn2_137m
      atn2_137e <-  input$atn2_137e
      atn2_137c <-  input$atn2_137c
      atn2_137fun <-  input$atn2_137fun
      atn2_138 <-  input$atn2_138
      atn2_138me <-  input$atn2_138me
      atn2_138m <-  input$atn2_138m
      atn2_138e <-  input$atn2_138e
      atn2_138c <-  input$atn2_138c
      atn2_138fun <-  input$atn2_138fun
      atn2_139 <-  input$atn2_139
      atn2_139me <-  input$atn2_139me
      atn2_139m <-  input$atn2_139m
      atn2_139e <-  input$atn2_139e
      atn2_139c <-  input$atn2_139c
      atn2_139fun <-  input$atn2_139fun
      atn2_140 <-  input$atn2_140
      atn2_140me <-  input$atn2_140me
      atn2_140m <-  input$atn2_140m
      atn2_140e <-  input$atn2_140e
      atn2_140c <-  input$atn2_140c
      atn2_140fun <-  input$atn2_140fun
      atn2_141 <-  input$atn2_141
      atn2_141me <-  input$atn2_141me
      atn2_141m <-  input$atn2_141m
      atn2_141e <-  input$atn2_141e
      atn2_141c <-  input$atn2_141c
      atn2_141fun <-  input$atn2_141fun
      atn2_06 <-  input$atn2_06
      atn2_06me <-  input$atn2_06me
      atn2_06m <-  input$atn2_06m
      atn2_06e <-  input$atn2_06e
      atn2_06fun <-  input$atn2_06fun
      atn2_06tr <-  input$atn2_06tr
      atn2_06cam <-  input$atn2_06cam
      atn2_06ch <-  input$atn2_06ch
      atn2_06ais <-  input$atn2_06ais
      atn2_07 <-  input$atn2_07
      atn2_07me <-  input$atn2_07me
      atn2_07m <-  input$atn2_07m
      atn2_07e <-  input$atn2_07e
      atn2_07pm <-  input$atn2_07pm
      atn2_07fun <-  input$atn2_07fun
      atn2_08 <-  input$atn2_08
      atn2_08me <-  input$atn2_08me
      atn2_08m <-  input$atn2_08m
      atn2_08e <-  input$atn2_08e
      atn2_08op <-  input$atn2_08op
      atn2_08cam <-  input$atn2_08cam
      atn2_08fun <-  input$atn2_08fun
      atn2_11 <-  input$atn2_11
      atn2_11me <-  input$atn2_11me
      atn2_11m <-  input$atn2_11m
      atn2_11e <-  input$atn2_11e
      atn2_11op <-  input$atn2_11op
      atn2_11cam <-  input$atn2_11cam
      atn2_11fun <-  input$atn2_11fun
      atn2_09 <-  input$atn2_09
      atn2_09e <-  input$atn2_09e
      atn2_09fun <-  input$atn2_09fun
      atn2_10 <-  input$atn2_10
      atn2_10me <-  input$atn2_10me
      atn2_10m <-  input$atn2_10m
      atn2_10e <-  input$atn2_10e
      atn2_10op <-  input$atn2_10op
      atn2_10fun <-  input$atn2_10fun
      atn2_10q <-  input$atn2_10q
      atn2_12 <-  input$atn2_12
      atn2_12me <-  input$atn2_12me
      atn2_12m <-  input$atn2_12m
      atn2_12e <-  input$atn2_12e
      atn2_12fun <-  input$atn2_12fun
      atn2_12op <-  input$atn2_12op
      atn2_12q <-  input$atn2_12q
      atn2_84 <-  input$atn2_84
      atn2_84me <-  input$atn2_84me
      atn2_84m <-  input$atn2_84m
      atn2_84fun <-  input$atn2_84fun
      atn2_84e <-  input$atn2_84e
      atn2_53 <-  input$atn2_53
      atn2_53me <-  input$atn2_53me
      atn2_53m <-  input$atn2_53m
      atn2_53fun <-  input$atn2_53fun
      atn2_53e <-  input$atn2_53e
      atn2_62 <-  input$atn2_62
      atn2_62me <-  input$atn2_62me
      atn2_62m <-  input$atn2_62m
      atn2_62e <-  input$atn2_62e
      atn2_62fun <-  input$atn2_62fun
      atn2_64 <-  input$atn2_64
      atn2_64me <-  input$atn2_64me
      atn2_64m <-  input$atn2_64m
      atn2_64e <-  input$atn2_64e
      atn2_64fun <-  input$atn2_64fun
      atn2_65 <-  input$atn2_65
      atn2_65me <-  input$atn2_65me
      atn2_65m <-  input$atn2_65m
      atn2_65e <-  input$atn2_65e
      atn2_65con <-  input$atn2_65con
      atn2_65fun <-  input$atn2_65fun
      atn2_67 <-  input$atn2_67
      atn2_67me <-  input$atn2_67me
      atn2_67m <-  input$atn2_67m
      atn2_67e <-  input$atn2_67e
      atn2_67con <-  input$atn2_67con
      atn2_67fun <-  input$atn2_67fun
      atn2_69 <-  input$atn2_69
      atn2_69me <-  input$atn2_69me
      atn2_69m <-  input$atn2_69m
      atn2_69e <-  input$atn2_69e
      atn2_69fun <-  input$atn2_69fun
      atn2_88 <-  input$atn2_88
      atn2_88me <-  input$atn2_88me
      atn2_88m <-  input$atn2_88m
      atn2_88e <-  input$atn2_88e
      atn2_88fun <-  input$atn2_88fun
      atn2_47 <-  input$atn2_47
      atn2_47o <-  input$atn2_47o
      atn2_47me <-  input$atn2_47me
      atn2_47m <-  input$atn2_47m
      atn2_47e <-  input$atn2_47e
      atn2_47fun <-  input$atn2_47fun
      atn2_59 <-  input$atn2_59
      atn2_59ps <-  input$atn2_59ps
      atn2_59fun <-  input$atn2_59fun
      atn2_51 <-  input$atn2_51
      atn2_51a <-  input$atn2_51a
      atn2_51fun <-  input$atn2_51fun
      atn2_52 <-  input$atn2_52
      atn2_52a <-  input$atn2_52a
      atn2_52fun <-  input$atn2_52fun
      atn2_70 <-  input$atn2_70
      atn2_70me <-  input$atn2_70me
      atn2_70m <-  input$atn2_70m
      atn2_70e <-  input$atn2_70e
      atn2_70pm <-  input$atn2_70pm
      atn2_70fun <-  input$atn2_70fun
      atn2_68 <-  input$atn2_68
      atn2_68me <-  input$atn2_68me
      atn2_68m <-  input$atn2_68m
      atn2_68ft <-  input$atn2_68ft
      atn2_68e <-  input$atn2_68e
      atn2_68fun <-  input$atn2_68fun
      atn2_15 <-  input$atn2_15
      atn2_15pm <-  input$atn2_15pm
      atn2_15carg <-  input$atn2_15carg
      atn2_15ah <-  input$atn2_15ah
      atn2_15fun <-  input$atn2_15fun
      atn2_15d <-  input$atn2_15d
      atn2_23 <-  input$atn2_23
      atn2_23fun <-  input$atn2_23fun
      atn2_23pm <-  input$atn2_23pm
      atn2_16 <-  input$atn2_16
      atn2_16pm <-  input$atn2_16pm
      atn2_16fun <-  input$atn2_16fun
      atn2_22 <-  input$atn2_22
      atn2_22pm <-  input$atn2_22pm
      atn2_22i <-  input$atn2_22i
      atn2_22fun <-  input$atn2_22fun
      atn2_17 <-  input$atn2_17
      atn2_17pm <-  input$atn2_17pm
      atn2_17fun <-  input$atn2_17fun
      atn2_18 <-  input$atn2_18
      atn2_18pm <-  input$atn2_18pm
      atn2_18fun <-  input$atn2_18fun
      atn2_20 <-  input$atn2_20
      atn2_20pm <-  input$atn2_20pm
      atn2_20fun <-  input$atn2_20fun
      atn2_19 <-  input$atn2_19
      atn2_19pm <-  input$atn2_19pm
      atn2_19se <-  input$atn2_19se
      atn2_19fun <-  input$atn2_19fun
      atn2_21 <-  input$atn2_21
      atn2_21baf <-  input$atn2_21baf
      atn2_21ban <-  input$atn2_21ban
      atn2_21fun <-  input$atn2_21fun
      atn2_24 <-  input$atn2_24
      atn2_24pm <-  input$atn2_24pm
      atn2_24fun <-  input$atn2_24fun
      atn2_25 <-  input$atn2_25
      atn2_25m <-  input$atn2_25m
      atn2_25fun <-  input$atn2_25fun
      atn2_26 <-  input$atn2_26
      atn2_27 <-  input$atn2_27
      atn2_28 <-  input$atn2_28
      atn2_29 <-  input$atn2_29
      atn2_30 <-  input$atn2_30
      atn2_31 <-  input$atn2_31
      atn2_32 <-  input$atn2_32
      atn2_33 <-  input$atn2_33
      atn2_34 <-  input$atn2_34
      atn2_35 <-  input$atn2_35
      atn2_40 <-  input$atn2_40
      atn2_36 <-  input$atn2_36
      atn2_37 <-  input$atn2_37
      atn2_38 <-  input$atn2_38
      atn2_39 <-  input$atn2_39
      atn2_41 <-  input$atn2_41
      atn2_42 <-  input$atn2_42
      atn2_106 <-  input$atn2_106
      atn2_95 <-  input$atn2_95
      atn2_95sr <-  input$atn2_95sr
      atn2_95fu <-  input$atn2_95fu
      atn2_95nf <-  input$atn2_95nf
      atn2_95fun <-  input$atn2_95fun
      atn2_97 <-  input$atn2_97
      atn2_97fu <-  input$atn2_97fu
      atn2_97nnf <-  input$atn2_97nnf
      atn2_98 <-  input$atn2_98
      atn2_98fu <-  input$atn2_98fu
      atn2_98nf <-  input$atn2_98nf
      atn2_99 <-  input$atn2_99
      atn2_99fu <-  input$atn2_99fu
      atn2_99nf <-  input$atn2_99nf
      atn2_100 <-  input$atn2_100
      atn2_100fu <-  input$atn2_100fu
      atn2_100nf <-  input$atn2_100nf
      atn2_101 <-  input$atn2_101
      atn2_101fu <-  input$atn2_101fu
      atn2_101nf <-  input$atn2_101nf
      atn2_102 <-  input$atn2_102
      atn2_102fu <-  input$atn2_102fu
      atn2_102nf <-  input$atn2_102nf
      atn2_103 <-  input$atn2_103
      atn2_103fu <-  input$atn2_103fu
      atn2_103nf <-  input$atn2_103nf
      atn2_104 <-  input$atn2_104
      atn2_104fu <-  input$atn2_104fu
      atn2_104nf <-  input$atn2_104nf
      atn2_71 <-  input$atn2_71
      atn2_71pc <-  input$atn2_71pc
      atn2_71fu <-  input$atn2_71fu
      atn2_71nf <-  input$atn2_71nf
      atn2_72 <-  input$atn2_72
      atn2_72pc <-  input$atn2_72pc
      atn2_72fu <-  input$atn2_72fu
      atn2_72nf <-  input$atn2_72nf
      atn2_72fun <-  input$atn2_72fun
      atn2_73 <-  input$atn2_73
      atn2_73pc <-  input$atn2_73pc
      atn2_73fu <-  input$atn2_73fu
      atn2_73nf <-  input$atn2_73nf
      atn2_77 <-  input$atn2_77
      atn2_77pc <-  input$atn2_77pc
      atn2_77es <-  input$atn2_77es
      atn2_77fu <-  input$atn2_77fu
      atn2_77nf <-  input$atn2_77nf
      atn2_79 <-  input$atn2_79
      atn2_79pc <-  input$atn2_79pc
      atn2_79fu <-  input$atn2_79fu
      atn2_79nf <-  input$atn2_79nf
      atn2_83 <-  input$atn2_83
      atn2_83pc <-  input$atn2_83pc
      atn2_83fu <-  input$atn2_83fu
      atn2_83nf <-  input$atn2_83nf
      atn2_74 <-  input$atn2_74
      atn2_74me <-  input$atn2_74me
      atn2_74m <-  input$atn2_74m
      atn2_74e <-  input$atn2_74e
      atn2_74fu <-  input$atn2_74fu
      atn2_74nf <-  input$atn2_74nf
      atn2_75 <-  input$atn2_75
      atn2_75me <-  input$atn2_75me
      atn2_75m <-  input$atn2_75m
      atn2_75e <-  input$atn2_75e
      atn2_75fu <-  input$atn2_75fu
      atn2_75nf <-  input$atn2_75nf
      atn2_76 <-  input$atn2_76
      atn2_76me <-  input$atn2_76me
      atn2_76m <-  input$atn2_76m
      atn2_76e <-  input$atn2_76e
      atn2_76fu <-  input$atn2_76fu
      atn2_76nf <-  input$atn2_76nf
      atn2_78 <-  input$atn2_78
      atn2_78me <-  input$atn2_78me
      atn2_78m <-  input$atn2_78m
      atn2_78e <-  input$atn2_78e
      atn2_78es <-  input$atn2_78es
      atn2_78fu <-  input$atn2_78fu
      atn2_78nf <-  input$atn2_78nf
      atn2_80 <-  input$atn2_80
      atn2_80me <-  input$atn2_80me
      atn2_80m <-  input$atn2_80m
      atn2_80e <-  input$atn2_80e
      atn2_80fu <-  input$atn2_80fu
      atn2_80nf <-  input$atn2_80nf
      atn2_81 <-  input$atn2_81
      atn2_81me <-  input$atn2_81me
      atn2_81m <-  input$atn2_81m
      atn2_81e <-  input$atn2_81e
      atn2_81aud <-  input$atn2_81aud
      atn2_81fu <-  input$atn2_81fu
      atn2_81nf <-  input$atn2_81nf
      atn2_82 <-  input$atn2_82
      atn2_82me <-  input$atn2_82me
      atn2_82m <-  input$atn2_82m
      atn2_82e <-  input$atn2_82e
      atn2_82ec <-  input$atn2_82ec
      atn2_82fu <-  input$atn2_82fu
      atn2_82nf <-  input$atn2_82nf
      atn3_26 <-  input$atn3_26
      atn3_26a <-  input$atn3_26a
      atn3_26fun <-  input$atn3_26fun
      atn3_28 <-  input$atn3_28
      atn3_28a <-  input$atn3_28a
      atn3_28fun <-  input$atn3_28fun
      atn3_30 <-  input$atn3_30
      atn3_30a <-  input$atn3_30a
      atn3_30fun <-  input$atn3_30fun
      atn3_31 <-  input$atn3_31
      atn3_31a <-  input$atn3_31a
      atn3_31fun <-  input$atn3_31fun
      atn3_33 <-  input$atn3_33
      atn3_33a <-  input$atn3_33a
      atn3_33fun <-  input$atn3_33fun
      atn3_37 <-  input$atn3_37
      atn3_37a <-  input$atn3_37a
      atn3_37fun <-  input$atn3_37fun
      atn3_38 <-  input$atn3_38
      atn3_38a <-  input$atn3_38a
      atn3_38fun <-  input$atn3_38fun
      atn3_42 <-  input$atn3_42
      atn3_42a <-  input$atn3_42a
      atn3_42fun <-  input$atn3_42fun
      atn3_47 <-  input$atn3_47
      atn3_47a <-  input$atn3_47a
      atn3_47fun <-  input$atn3_47fun
      atn3_48 <-  input$atn3_48
      atn3_48a <-  input$atn3_48a
      atn3_48fun <-  input$atn3_48fun
      atn3_50 <-  input$atn3_50
      atn3_50a <-  input$atn3_50a
      atn3_50fun <-  input$atn3_50fun
      atn3_52 <-  input$atn3_52
      atn3_52a <-  input$atn3_52a
      atn3_52fun <-  input$atn3_52fun
      atn3_54 <-  input$atn3_54
      atn3_54a <-  input$atn3_54a
      atn3_54fun <-  input$atn3_54fun
      atn3_55 <-  input$atn3_55
      atn3_55a <-  input$atn3_55a
      atn3_55fun <-  input$atn3_55fun
      atn3_59 <-  input$atn3_59
      atn3_59a <-  input$atn3_59a
      atn3_59fun <-  input$atn3_59fun
      atn3_60 <-  input$atn3_60
      atn3_60a <-  input$atn3_60a
      atn3_60fun <-  input$atn3_60fun
      atn3_61 <-  input$atn3_61
      atn3_61a <-  input$atn3_61a
      atn3_61fun <-  input$atn3_61fun
      atn2_44 <-  input$atn2_44
      atn2_44me <-  input$atn2_44me
      atn2_44m <-  input$atn2_44m
      atn2_44e <-  input$atn2_44e
      atn2_44con <-  input$atn2_44con
      atn2_44fun <-  input$atn2_44fun
      atn2_13 <-  input$atn2_13
      atn2_13me <-  input$atn2_13me
      atn2_13m <-  input$atn2_13m
      atn2_13e <-  input$atn2_13e
      atn2_13op <-  input$atn2_13op
      atn2_13fun <-  input$atn2_13fun
      atn2_13ch <-  input$atn2_13ch
      atn2_14 <-  input$atn2_14
      atn2_14me <-  input$atn2_14me
      atn2_14m <-  input$atn2_14m
      atn2_14e <-  input$atn2_14e
      atn2_14op <-  input$atn2_14op
      atn2_14fun <-  input$atn2_14fun
      atn2_14q <-  input$atn2_14q
      atn2_14qt <-  input$atn2_14qt
      atn2_105 <-  input$atn2_105
      atn3_01 <-  input$atn3_01
      atn3_68 <-  input$atn3_68
      atn3_04 <-  input$atn3_04
      atn3_04fun <-  input$atn3_04fun
      atn3_05 <-  input$atn3_05
      atn3_05fun <-  input$atn3_05fun
      atn3_06 <-  input$atn3_06
      atn3_06fun <-  input$atn3_06fun
      atn3_07 <-  input$atn3_07
      atn3_07fun <-  input$atn3_07fun
      atn3_09 <-  input$atn3_09
      atn3_09fun <-  input$atn3_09fun
      atn3_10 <-  input$atn3_10
      atn3_10fun <-  input$atn3_10fun
      atn3_11 <-  input$atn3_11
      atn3_11fun <-  input$atn3_11fun
      atn3_12 <-  input$atn3_12
      atn3_12fun <-  input$atn3_12fun
      atn3_13 <-  input$atn3_13
      atn3_13fun <-  input$atn3_13fun
      atn3_14 <-  input$atn3_14
      atn3_14fun <-  input$atn3_14fun
      atn3_15 <-  input$atn3_15
      atn3_15fun <-  input$atn3_15fun
      atn3_16 <-  input$atn3_16
      atn3_16fun <-  input$atn3_16fun
      atn3_17 <-  input$atn3_17
      atn3_17fun <-  input$atn3_17fun
      atn3_18 <-  input$atn3_18
      atn3_18fun <-  input$atn3_18fun
      atn3_19 <-  input$atn3_19
      atn3_19fun <-  input$atn3_19fun
      atn3_21 <-  input$atn3_21
      atn3_21fun <-  input$atn3_21fun
      atn3_22 <-  input$atn3_22
      atn3_22fun <-  input$atn3_22fun
      atn3_23 <-  input$atn3_23
      atn3_23fun <-  input$atn3_23fun
      atn3_24 <-  input$atn3_24
      atn3_24fun <-  input$atn3_24fun
      atn3_20 <-  input$atn3_20
      atn3_20fun <-  input$atn3_20fun
      atn3_20fu <-  input$atn3_20fu
      atn3_20nf <-  input$atn3_20nf
      atn3_62 <-  input$atn3_62
      atn3_62fu <-  input$atn3_62fu
      atn3_62nf <-  input$atn3_62nf
      atn3_65 <-  input$atn3_65
      atn3_65fu <-  input$atn3_65fu
      atn3_65nf <-  input$atn3_65nf
      atn3_66 <-  input$atn3_66
      atn3_66t <-  input$atn3_66t
      atn3_66fu <-  input$atn3_66fu
      atn3_66nf <-  input$atn3_66nf
      atn3_69 <-  input$atn3_69
      atn3_69fu <-  input$atn3_69fu
      atn3_69nf <-  input$atn3_69nf
      atn3_69fun <-  input$atn3_69fun
      atn3_71 <-  input$atn3_71
      atn3_71a <-  input$atn3_71a
      atn3_72 <-  input$atn3_72
      atn3_72fu <-  input$atn3_72fu
      atn3_72nf <-  input$atn3_72nf
      atn2_96 <-  input$atn2_96
      atn2_96sr <-  input$atn2_96sr
      atn2_96fu <-  input$atn2_96fu
      atn2_96nf <-  input$atn2_96nf
      atn2_96fun <-  input$atn2_96fun
      horario <-  str_c(input[["horario"]], collapse = ', ')
      atn2_01h <- str_c(input[["atn2_01h"]], collapse = ', ')
      atn2_49h <- str_c(input[["atn2_49h"]], collapse = ', ')
      atn2_03h <- str_c(input[["atn2_03h"]], collapse = ', ')
      atn2_04h <- str_c(input[["atn2_04h"]], collapse = ', ')
      atn2_05h <- str_c(input[["atn2_05h"]], collapse = ', ')
      atn2_66h <- str_c(input[["atn2_66h"]], collapse = ', ')
      atn2_48h <- str_c(input[["atn2_48h"]], collapse = ', ')
      atn2_50h <- str_c(input[["atn2_50h"]], collapse = ', ')
      atn2_85h <- str_c(input[["atn2_85h"]], collapse = ', ')
      atn2_107 <- str_c(input[["atn2_107"]], collapse = ', ')
      atn2_02h <- str_c(input[["atn2_02h"]], collapse = ', ')
      atn2_108h <- str_c(input[["atn2_108h"]], collapse = ', ')
      atn2_54h <- str_c(input[["atn2_54h"]], collapse = ', ')
      atn2_55h <- str_c(input[["atn2_55h"]], collapse = ', ')
      atn2_45h <- str_c(input[["atn2_45h"]], collapse = ', ')
      atn2_56h <- str_c(input[["atn2_56h"]], collapse = ', ')
      atn2_57h <- str_c(input[["atn2_57h"]], collapse = ', ')
      atn2_60h <- str_c(input[["atn2_60h"]], collapse = ', ')
      atn2_61h <- str_c(input[["atn2_61h"]], collapse = ', ')
      atn2_63h <- str_c(input[["atn2_63h"]], collapse = ', ')
      atn2_89h <- str_c(input[["atn2_89h"]], collapse = ', ')
      atn2_113h <- str_c(input[["atn2_113h"]], collapse = ', ')
      atn2_114h <- str_c(input[["atn2_114h"]], collapse = ', ')
      atn2_116h <- str_c(input[["atn2_116h"]], collapse = ', ')
      atn2_118h <- str_c(input[["atn2_118h"]], collapse = ', ')
      atn2_119h <- str_c(input[["atn2_119h"]], collapse = ', ')
      atn2_120h <- str_c(input[["atn2_120h"]], collapse = ', ')
      atn2_121h <- str_c(input[["atn2_121h"]], collapse = ', ')
      atn2_122h <- str_c(input[["atn2_122h"]], collapse = ', ')
      atn2_123h <- str_c(input[["atn2_123h"]], collapse = ', ')
      atn2_124h <- str_c(input[["atn2_124h"]], collapse = ', ')
      atn2_125h <- str_c(input[["atn2_125h"]], collapse = ', ')
      atn2_126h <- str_c(input[["atn2_126h"]], collapse = ', ')
      atn2_127h <- str_c(input[["atn2_127h"]], collapse = ', ')
      atn2_128h <- str_c(input[["atn2_128h"]], collapse = ', ')
      atn2_129h <- str_c(input[["atn2_129h"]], collapse = ', ')
      atn2_130h <- str_c(input[["atn2_130h"]], collapse = ', ')
      atn2_131h <- str_c(input[["atn2_131h"]], collapse = ', ')
      atn2_133h <- str_c(input[["atn2_133h"]], collapse = ', ')
      atn2_134h <- str_c(input[["atn2_134h"]], collapse = ', ')
      atn2_135h <- str_c(input[["atn2_135h"]], collapse = ', ')
      atn2_137h <- str_c(input[["atn2_137h"]], collapse = ', ')
      atn2_138h <- str_c(input[["atn2_138h"]], collapse = ', ')
      atn2_139h <- str_c(input[["atn2_139h"]], collapse = ', ')
      atn2_140h <- str_c(input[["atn2_140h"]], collapse = ', ')
      atn2_141h <- str_c(input[["atn2_141h"]], collapse = ', ')
      atn2_06h <- str_c(input[["atn2_06h"]], collapse = ', ')
      atn2_07h <- str_c(input[["atn2_07h"]], collapse = ', ')
      atn2_08h <- str_c(input[["atn2_08h"]], collapse = ', ')
      atn2_11h <- str_c(input[["atn2_11h"]], collapse = ', ')
      atn2_09h <- str_c(input[["atn2_09h"]], collapse = ', ')
      atn2_10h <- str_c(input[["atn2_10h"]], collapse = ', ')
      atn2_12h <- str_c(input[["atn2_12h"]], collapse = ', ')
      atn2_84h <- str_c(input[["atn2_84h"]], collapse = ', ')
      atn2_53h <- str_c(input[["atn2_53h"]], collapse = ', ')
      atn2_64h <- str_c(input[["atn2_64h"]], collapse = ', ')
      atn2_65h <- str_c(input[["atn2_65h"]], collapse = ', ')
      atn2_67h <- str_c(input[["atn2_67h"]], collapse = ', ')
      atn2_69h <- str_c(input[["atn2_69h"]], collapse = ', ')
      atn2_88h <- str_c(input[["atn2_88h"]], collapse = ', ')
      atn2_47h <- str_c(input[["atn2_47h"]], collapse = ', ')
      atn2_59h <- str_c(input[["atn2_59h"]], collapse = ', ')
      atn2_51h <- str_c(input[["atn2_51h"]], collapse = ', ')
      atn2_52h <- str_c(input[["atn2_52h"]], collapse = ', ')
      atn2_70h <- str_c(input[["atn2_70h"]], collapse = ', ')
      atn2_68h <- str_c(input[["atn2_68h"]], collapse = ', ')
      atn2_15h <- str_c(input[["atn2_15h"]], collapse = ', ')
      atn2_23h <- str_c(input[["atn2_23h"]], collapse = ', ')
      atn2_16h <- str_c(input[["atn2_16h"]], collapse = ', ')
      atn2_22h <- str_c(input[["atn2_22h"]], collapse = ', ')
      atn2_17h <- str_c(input[["atn2_17h"]], collapse = ', ')
      atn2_18h <- str_c(input[["atn2_18h"]], collapse = ', ')
      atn2_20h <- str_c(input[["atn2_20h"]], collapse = ', ')
      atn2_19h <- str_c(input[["atn2_19h"]], collapse = ', ')
      atn2_71h <- str_c(input[["atn2_71h"]], collapse = ', ')
      atn2_72h <- str_c(input[["atn2_72h"]], collapse = ', ')
      atn2_73h <- str_c(input[["atn2_73h"]], collapse = ', ')
      atn2_77h <- str_c(input[["atn2_77h"]], collapse = ', ')
      atn2_79h <- str_c(input[["atn2_79h"]], collapse = ', ')
      atn2_83h <- str_c(input[["atn2_83h"]], collapse = ', ')
      atn2_74h <- str_c(input[["atn2_74h"]], collapse = ', ')
      atn2_75h <- str_c(input[["atn2_75h"]], collapse = ', ')
      atn2_76h <- str_c(input[["atn2_76h"]], collapse = ', ')
      atn2_78h <- str_c(input[["atn2_78h"]], collapse = ', ')
      atn2_80h <- str_c(input[["atn2_80h"]], collapse = ', ')
      atn2_81h <- str_c(input[["atn2_81h"]], collapse = ', ')
      atn2_82h <- str_c(input[["atn2_82h"]], collapse = ', ')
      atn3_26h <- str_c(input[["atn3_26h"]], collapse = ', ')
      atn3_28h <- str_c(input[["atn3_28h"]], collapse = ', ')
      atn3_30h <- str_c(input[["atn3_30h"]], collapse = ', ')
      atn3_31h <- str_c(input[["atn3_31h"]], collapse = ', ')
      atn3_33h <- str_c(input[["atn3_33h"]], collapse = ', ')
      atn3_37h <- str_c(input[["atn3_37h"]], collapse = ', ')
      atn3_38h <- str_c(input[["atn3_38h"]], collapse = ', ')
      atn3_42h <- str_c(input[["atn3_42h"]], collapse = ', ')
      atn3_47h <- str_c(input[["atn3_47h"]], collapse = ', ')
      atn3_48h <- str_c(input[["atn3_48h"]], collapse = ', ')
      atn3_50h <- str_c(input[["atn3_50h"]], collapse = ', ')
      atn3_52h <- str_c(input[["atn3_52h"]], collapse = ', ')
      atn3_54h <- str_c(input[["atn3_54h"]], collapse = ', ')
      atn3_55h <- str_c(input[["atn3_55h"]], collapse = ', ')
      atn3_59h <- str_c(input[["atn3_59h"]], collapse = ', ')
      atn3_60h <- str_c(input[["atn3_60h"]], collapse = ', ')
      atn3_61h <- str_c(input[["atn3_61h"]], collapse = ', ')
      atn2_44h <- str_c(input[["atn2_44h"]], collapse = ', ')
      atn2_13h <- str_c(input[["atn2_13h"]], collapse = ', ')
      atn2_14h <- str_c(input[["atn2_14h"]], collapse = ', ')
      atn3_04h <- str_c(input[["atn3_04h"]], collapse = ', ')
      atn3_05h <- str_c(input[["atn3_05h"]], collapse = ', ')
      atn3_06h <- str_c(input[["atn3_06h"]], collapse = ', ')
      atn3_07h <- str_c(input[["atn3_07h"]], collapse = ', ')
      atn3_09h <- str_c(input[["atn3_09h"]], collapse = ', ')
      atn3_10h <- str_c(input[["atn3_10h"]], collapse = ', ')
      atn3_11h <- str_c(input[["atn3_11h"]], collapse = ', ')
      atn3_12h <- str_c(input[["atn3_12h"]], collapse = ', ')
      atn3_13h <- str_c(input[["atn3_13h"]], collapse = ', ')
      atn3_14h <- str_c(input[["atn3_14h"]], collapse = ', ')
      atn3_15h <- str_c(input[["atn3_15h"]], collapse = ', ')
      atn3_16h <- str_c(input[["atn3_16h"]], collapse = ', ')
      atn3_17h <- str_c(input[["atn3_17h"]], collapse = ', ')
      atn3_18h <- str_c(input[["atn3_18h"]], collapse = ', ')
      atn3_19h <- str_c(input[["atn3_19h"]], collapse = ', ')
      atn3_21h <- str_c(input[["atn3_21h"]], collapse = ', ')
      atn3_22h <- str_c(input[["atn3_22h"]], collapse = ', ')
      atn3_23h <- str_c(input[["atn3_23h"]], collapse = ', ')
      atn3_24h <- str_c(input[["atn3_24h"]], collapse = ', ')
      atn3_20h <- str_c(input[["atn3_20h"]], collapse = ', ')
      atn3_69h <- str_c(input[["atn3_69h"]], collapse = ', ')
      atn3_72h <- str_c(input[["atn3_72h"]], collapse = ', ')
      
      
      datos_p1 <- data.frame(nombre = nombre,	dependencia = dependencia,	municipio = municipio,	nom_ins = nom_ins,	tel = tel,	horario = horario,	atn_01 = atn_01,	atn2_01 = atn2_01,	atn2_01h = atn2_01h,	atn2_01m = atn2_01m,	atn2_01e = atn2_01e,	atn2_01con = atn2_01con,	atn2_01fun = atn2_01fun,	atn2_49 = atn2_49,	atn2_49h = atn2_49h,	atn2_49t = atn2_49t,	atn2_49fun = atn2_49fun,	atn2_03 = atn2_03,	atn2_03h = atn2_03h,	atn2_03q = atn2_03q,	atn2_03qmi = atn2_03qmi,	atn2_03me = atn2_03me,	atn2_03e = atn2_03e,	atn2_03fun = atn2_03fun,	atn2_04 = atn2_04,	atn2_04h = atn2_04h,	atn2_04pm = atn2_04pm,	atn2_04fun = atn2_04fun,	atn2_05 = atn2_05,	atn2_05h = atn2_05h,	atn2_05pc = atn2_05pc,	atn2_05fun = atn2_05fun,	atn2_66 = atn2_66,	atn2_66h = atn2_66h,	atn2_66me = atn2_66me,	atn2_66m = atn2_66m,
                             atn2_66e = atn2_66e,	atn2_66con = atn2_66con,	atn2_66fun = atn2_66fun,	atn2_46 = atn2_46,	atn2_46e = atn2_46e,	atn2_46eq = atn2_46eq,	atn2_46est = atn2_46est,	atn2_46fun = atn2_46fun,	atn2_48 = atn2_48,	atn2_48h = atn2_48h,	atn2_48me = atn2_48me,	atn2_48m = atn2_48m,	atn2_48ps = atn2_48ps,	atn2_48e = atn2_48e,	atn2_48fun = atn2_48fun,	atn2_50 = atn2_50,	atn2_50h = atn2_50h,	atn2_50n = atn2_50n,	atn2_50fun = atn2_50fun,	atn2_85 = atn2_85,	atn2_85h = atn2_85h,	atn2_85e = atn2_85e,	atn2_85rf = atn2_85rf,	atn2_85fun = atn2_85fun,	atn2_107 = atn2_107,	atn2_02 = atn2_02,	atn2_02h = atn2_02h,	atn2_02me = atn2_02me,	atn2_02m = atn2_02m,	atn2_02e = atn2_02e,	atn2_02con = atn2_02con,	atn2_02fun = atn2_02fun,	atn2_108 = atn2_108,	atn2_108h = atn2_108h,	atn2_108me = atn2_108me,	atn2_108m = atn2_108m,	atn2_108e = atn2_108e,	atn2_108c = atn2_108c,	atn2_108fun = atn2_108fun,	atn2_54 = atn2_54,	atn2_54h = atn2_54h,	atn2_54me = atn2_54me,	atn2_54m = atn2_54m,	atn2_54e = atn2_54e,	atn2_54con = atn2_54con,
                             atn2_54fun = atn2_54fun,	atn2_55 = atn2_55,	atn2_55h = atn2_55h,	atn2_55me = atn2_55me,	atn2_55m = atn2_55m,	atn2_55e = atn2_55e,	atn2_55con = atn2_55con,	atn2_55fun = atn2_55fun,	atn2_45 = atn2_45,	atn2_45h = atn2_45h,	atn2_45me = atn2_45me,	atn2_45m = atn2_45m,	atn2_45e = atn2_45e,	atn2_45con = atn2_45con,	atn2_45fun = atn2_45fun,	atn2_56 = atn2_56,	atn2_56h = atn2_56h,	atn2_56me = atn2_56me,	atn2_56m = atn2_56m,	atn2_56e = atn2_56e,	atn2_56c = atn2_56c,	atn2_56fun = atn2_56fun,	atn2_57 = atn2_57,	atn2_57h = atn2_57h,	atn2_57me = atn2_57me,	atn2_57m = atn2_57m,	atn2_57e = atn2_57e,	atn2_57con = atn2_57con,	atn2_57fun = atn2_57fun,	atn2_60 = atn2_60,	atn2_60h = atn2_60h,	atn2_60me = atn2_60me,	atn2_60m = atn2_60m,	atn2_60e = atn2_60e,	atn2_60con = atn2_60con,	atn2_60fun = atn2_60fun,	atn2_61 = atn2_61,	atn2_61h = atn2_61h,	atn2_61me = atn2_61me,	atn2_61m = atn2_61m,	atn2_61e = atn2_61e,	atn2_61con = atn2_61con,	atn2_61fun = atn2_61fun,	atn2_63 = atn2_63,	atn2_63h = atn2_63h,	atn2_63me = atn2_63me,	atn2_63m = atn2_63m,
                             atn2_63e = atn2_63e,	atn2_63con = atn2_63con,	atn2_63fun = atn2_63fun,	atn2_89 = atn2_89,	atn2_89h = atn2_89h,	atn2_89me = atn2_89me,	atn2_89m = atn2_89m,	atn2_89e = atn2_89e,	atn2_89con = atn2_89con,	atn2_89fun = atn2_89fun,	atn2_87 = atn2_87,	atn2_87me = atn2_87me,	atn2_87m = atn2_87m,	atn2_87e = atn2_87e,	atn2_87con = atn2_87con,	atn2_87fun = atn2_87fun,	atn2_113 = atn2_113,	atn2_113h = atn2_113h,	atn2_113me = atn2_113me,	atn2_113m = atn2_113m,	atn2_113e = atn2_113e,	atn2_113c = atn2_113c,	atn2_113fun = atn2_113fun,	atn2_114 = atn2_114,	atn2_114h = atn2_114h,	atn2_114me = atn2_114me,	atn2_114m = atn2_114m,	atn2_114e = atn2_114e,	atn2_114c = atn2_114c,	atn2_114fun = atn2_114fun,	atn2_116 = atn2_116,	atn2_116h = atn2_116h,	atn2_116me = atn2_116me,	atn2_116m = atn2_116m,	atn2_116e = atn2_116e,	atn2_116c = atn2_116c,	atn2_116fun = atn2_116fun,	atn2_118 = atn2_118,	atn2_118h = atn2_118h,	atn2_118me = atn2_118me,	atn2_118m = atn2_118m,	atn2_118e = atn2_118e,	atn2_118c = atn2_118c,	atn2_118fun = atn2_118fun,	atn2_119 = atn2_119,	atn2_119h = atn2_119h,	atn2_119me = atn2_119me,	atn2_119m = atn2_119m,
                             atn2_119e = atn2_119e,	atn2_119c = atn2_119c,	atn2_119fun = atn2_119fun,	atn2_120 = atn2_120,	atn2_120h = atn2_120h,	atn2_120me = atn2_120me,	atn2_120m = atn2_120m,	atn2_120e = atn2_120e,	atn2_120c = atn2_120c,	atn2_120fun = atn2_120fun,	atn2_121 = atn2_121,	atn2_121h = atn2_121h,	atn2_121me = atn2_121me,	atn2_121m = atn2_121m,	atn2_121e = atn2_121e,	atn2_121c = atn2_121c,	atn2_121fun = atn2_121fun,	atn2_122 = atn2_122,	atn2_122h = atn2_122h,	atn2_122me = atn2_122me,	atn2_122m = atn2_122m,	atn2_122e = atn2_122e,	atn2_122c = atn2_122c,	atn2_122fun = atn2_122fun,	atn2_123 = atn2_123,	atn2_123h = atn2_123h,	atn2_123me = atn2_123me,	atn2_123m = atn2_123m,	atn2_123e = atn2_123e,	atn2_123c = atn2_123c,	atn2_123fun = atn2_123fun,	atn2_124 = atn2_124,	atn2_124h = atn2_124h,	atn2_124me = atn2_124me,	atn2_124m = atn2_124m,	atn2_124e = atn2_124e,	atn2_124c = atn2_124c,	atn2_124fun = atn2_124fun
      )
      datos_p2 <- data.frame(atn2_125 = atn2_125,	atn2_125h = atn2_125h,	atn2_125me = atn2_125me,	atn2_125m = atn2_125m,	atn2_125e = atn2_125e,	atn2_125c = atn2_125c,	atn2_125fun = atn2_125fun,	atn2_126 = atn2_126,	atn2_126h = atn2_126h,	atn2_126me = atn2_126me,	atn2_126m = atn2_126m,	atn2_126e = atn2_126e,	atn2_126c = atn2_126c,	atn2_126fun = atn2_126fun,	atn2_127 = atn2_127,	atn2_127h = atn2_127h,	atn2_127me = atn2_127me,	atn2_127m = atn2_127m,	atn2_127e = atn2_127e,	atn2_127c = atn2_127c,	atn2_127fun = atn2_127fun,	atn2_128 = atn2_128,	atn2_128h = atn2_128h,	atn2_128me = atn2_128me,	atn2_128m = atn2_128m,	atn2_128e = atn2_128e,	atn2_128c = atn2_128c,	atn2_128fun = atn2_128fun,	atn2_129 = atn2_129,	atn2_129h = atn2_129h,	atn2_129me = atn2_129me,	atn2_129m = atn2_129m,	atn2_129e = atn2_129e,	atn2_129c = atn2_129c,	atn2_129fun = atn2_129fun,	atn2_130 = atn2_130,	atn2_130h = atn2_130h,	atn2_130me = atn2_130me,	atn2_130m = atn2_130m,	atn2_130e = atn2_130e,	atn2_130c = atn2_130c,	atn2_130fun = atn2_130fun,	atn2_131 = atn2_131,	atn2_131h = atn2_131h,	atn2_131me = atn2_131me,	atn2_131m = atn2_131m,	atn2_131e = atn2_131e,	atn2_131c = atn2_131c,
                             atn2_131fun = atn2_131fun,	atn2_133 = atn2_133,	atn2_133h = atn2_133h,	atn2_133me = atn2_133me,	atn2_133m = atn2_133m,	atn2_133e = atn2_133e,	atn2_133c = atn2_133c,	atn2_133fun = atn2_133fun,	atn2_134 = atn2_134,	atn2_134h = atn2_134h,	atn2_134me = atn2_134me,	atn2_134m = atn2_134m,	atn2_134e = atn2_134e,	atn2_134c = atn2_134c,	atn2_134fun = atn2_134fun,	atn2_135 = atn2_135,	atn2_135h = atn2_135h,	atn2_135me = atn2_135me,	atn2_135m = atn2_135m,	atn2_135e = atn2_135e,	atn2_135c = atn2_135c,	atn2_135fun = atn2_135fun,	atn2_137 = atn2_137,	atn2_137h = atn2_137h,	atn2_137me = atn2_137me,	atn2_137m = atn2_137m,	atn2_137e = atn2_137e,	atn2_137c = atn2_137c,	atn2_137fun = atn2_137fun,	atn2_138 = atn2_138,	atn2_138h = atn2_138h,	atn2_138me = atn2_138me,	atn2_138m = atn2_138m,	atn2_138e = atn2_138e,	atn2_138c = atn2_138c,	atn2_138fun = atn2_138fun,	atn2_139 = atn2_139,	atn2_139h = atn2_139h,	atn2_139me = atn2_139me,	atn2_139m = atn2_139m,	atn2_139e = atn2_139e,	atn2_139c = atn2_139c,	atn2_139fun = atn2_139fun,	atn2_140 = atn2_140,	atn2_140h = atn2_140h,	atn2_140me = atn2_140me,	atn2_140m = atn2_140m,	atn2_140e = atn2_140e,	atn2_140c = atn2_140c,	atn2_140fun = atn2_140fun,	atn2_141 = atn2_141,	atn2_141h = atn2_141h,	atn2_141me = atn2_141me,	atn2_141m = atn2_141m,	atn2_141e = atn2_141e,	atn2_141c = atn2_141c,	atn2_141fun = atn2_141fun,	atn2_06 = atn2_06,	atn2_06me = atn2_06me,
                             atn2_06m = atn2_06m,	atn2_06e = atn2_06e,	atn2_06fun = atn2_06fun,	atn2_06h = atn2_06h,	atn2_06tr = atn2_06tr,	atn2_06cam = atn2_06cam,	atn2_06ch = atn2_06ch,	atn2_06ais = atn2_06ais,	atn2_07 = atn2_07,	atn2_07h = atn2_07h,	atn2_07me = atn2_07me,	atn2_07m = atn2_07m,	atn2_07e = atn2_07e,	atn2_07pm = atn2_07pm,	atn2_07fun = atn2_07fun,	atn2_08 = atn2_08,	atn2_08h = atn2_08h,	atn2_08me = atn2_08me,	atn2_08m = atn2_08m,	atn2_08e = atn2_08e,	atn2_08op = atn2_08op,	atn2_08cam = atn2_08cam,	atn2_08fun = atn2_08fun,	atn2_11 = atn2_11,	atn2_11h = atn2_11h,	atn2_11me = atn2_11me,	atn2_11m = atn2_11m,	atn2_11e = atn2_11e,	atn2_11op = atn2_11op,	atn2_11cam = atn2_11cam,	atn2_11fun = atn2_11fun,	atn2_09 = atn2_09,	atn2_09h = atn2_09h,	atn2_09e = atn2_09e,	atn2_09fun = atn2_09fun,	atn2_10 = atn2_10,	atn2_10h = atn2_10h,	atn2_10me = atn2_10me,	atn2_10m = atn2_10m,	atn2_10e = atn2_10e,	atn2_10op = atn2_10op,	atn2_10fun = atn2_10fun,	atn2_10q = atn2_10q,	atn2_12 = atn2_12,	atn2_12h = atn2_12h,	atn2_12me = atn2_12me,	atn2_12m = atn2_12m,	atn2_12e = atn2_12e,	atn2_12fun = atn2_12fun,	atn2_12op = atn2_12op,	atn2_12q = atn2_12q,	atn2_84 = atn2_84,	atn2_84h = atn2_84h,	atn2_84me = atn2_84me,	atn2_84m = atn2_84m,	atn2_84fun = atn2_84fun,	atn2_84e = atn2_84e,	atn2_53 = atn2_53,	atn2_53h = atn2_53h,	atn2_53me = atn2_53me,	atn2_53m = atn2_53m,	atn2_53fun = atn2_53fun,	atn2_53e = atn2_53e,	atn2_62 = atn2_62,	atn2_62me = atn2_62me,	atn2_62m = atn2_62m,	atn2_62e = atn2_62e,	atn2_62fun = atn2_62fun,
                             atn2_64 = atn2_64,	atn2_64h = atn2_64h,	atn2_64me = atn2_64me,atn2_64m = atn2_64m,	atn2_64e = atn2_64e,	atn2_64fun = atn2_64fun,	atn2_65 = atn2_65,	atn2_65h = atn2_65h,	atn2_65me = atn2_65me,	atn2_65m = atn2_65m,	atn2_65e = atn2_65e,	atn2_65con = atn2_65con,	atn2_65fun = atn2_65fun,	atn2_67 = atn2_67,	atn2_67h = atn2_67h,	atn2_67me = atn2_67me,	atn2_67m = atn2_67m,	atn2_67e = atn2_67e,	atn2_67con = atn2_67con,	atn2_67fun = atn2_67fun,	atn2_69 = atn2_69,	atn2_69h = atn2_69h,	atn2_69me = atn2_69me,	atn2_69m = atn2_69m,	atn2_69e = atn2_69e,	atn2_69fun = atn2_69fun,	atn2_88 = atn2_88,	atn2_88h = atn2_88h,	atn2_88me = atn2_88me,	atn2_88m = atn2_88m,	atn2_88e = atn2_88e,	atn2_88fun = atn2_88fun,	atn2_47 = atn2_47,	atn2_47h = atn2_47h,	atn2_47o = atn2_47o,	atn2_47me = atn2_47me,	atn2_47m = atn2_47m,	atn2_47e = atn2_47e,	atn2_47fun = atn2_47fun,	atn2_59 = atn2_59,	atn2_59h = atn2_59h,	atn2_59ps = atn2_59ps,	atn2_59fun = atn2_59fun,	atn2_51 = atn2_51,	atn2_51h = atn2_51h,	atn2_51a = atn2_51a,	atn2_51fun = atn2_51fun
      )
      datos_p3 <- data.frame(atn2_52 = atn2_52,	atn2_52h = atn2_52h,	atn2_52a = atn2_52a,	atn2_52fun = atn2_52fun,	atn2_70 = atn2_70,	atn2_70h = atn2_70h,	atn2_70me = atn2_70me,	atn2_70m = atn2_70m,	atn2_70e = atn2_70e,	atn2_70pm = atn2_70pm,	atn2_70fun = atn2_70fun,	atn2_68 = atn2_68,	atn2_68h = atn2_68h,	atn2_68me = atn2_68me,	atn2_68m = atn2_68m,	atn2_68ft = atn2_68ft,	atn2_68e = atn2_68e,	atn2_68fun = atn2_68fun,	atn2_15 = atn2_15,	atn2_15h = atn2_15h,	atn2_15pm = atn2_15pm,	atn2_15carg = atn2_15carg,	atn2_15ah = atn2_15ah,	atn2_15fun = atn2_15fun,	atn2_15d = atn2_15d,	atn2_23 = atn2_23,	atn2_23h = atn2_23h,	atn2_23fun = atn2_23fun,	atn2_23pm = atn2_23pm,	atn2_16 = atn2_16,	atn2_16h = atn2_16h,	atn2_16pm = atn2_16pm,	atn2_16fun = atn2_16fun,	atn2_22 = atn2_22,	atn2_22h = atn2_22h,	atn2_22pm = atn2_22pm,	atn2_22i = atn2_22i,	atn2_22fun = atn2_22fun,	atn2_17 = atn2_17,	atn2_17h = atn2_17h,	atn2_17pm = atn2_17pm,	atn2_17fun = atn2_17fun,	atn2_18 = atn2_18,	atn2_18h = atn2_18h,	atn2_18pm = atn2_18pm,	atn2_18fun = atn2_18fun,	atn2_20 = atn2_20,	atn2_20h = atn2_20h,	atn2_20pm = atn2_20pm,	atn2_20fun = atn2_20fun,	atn2_19 = atn2_19,	atn2_19h = atn2_19h,	atn2_19pm = atn2_19pm,	atn2_19se = atn2_19se,	atn2_19fun = atn2_19fun,	atn2_21 = atn2_21,	atn2_21baf = atn2_21baf,	atn2_21ban = atn2_21ban,	atn2_21fun = atn2_21fun,	atn2_24 = atn2_24,	atn2_24pm = atn2_24pm,	atn2_24fun = atn2_24fun,	atn2_25 = atn2_25,	atn2_25m = atn2_25m,	atn2_25fun = atn2_25fun,	atn2_26 = atn2_26,	atn2_27 = atn2_27,	atn2_28 = atn2_28,	atn2_29 = atn2_29,	
                             atn2_30 = atn2_30,	atn2_31 = atn2_31,	atn2_32 = atn2_32,	atn2_33 = atn2_33,	atn2_34 = atn2_34,	atn2_35 = atn2_35,	atn2_40 = atn2_40,	atn2_36 = atn2_36,	atn2_37 = atn2_37,	atn2_38 = atn2_38,	atn2_39 = atn2_39,	atn2_41 = atn2_41,	atn2_42 = atn2_42,	atn2_106 = atn2_106,	atn2_95 = atn2_95,	atn2_95sr = atn2_95sr,	atn2_95fu = atn2_95fu,	atn2_95nf = atn2_95nf,	atn2_95fun = atn2_95fun,	atn2_97 = atn2_97,	atn2_97fu = atn2_97fu,	atn2_97nnf = atn2_97nnf,	atn2_98 = atn2_98,	atn2_98fu = atn2_98fu,	atn2_98nf = atn2_98nf,	atn2_99 = atn2_99,	atn2_99fu = atn2_99fu,	atn2_99nf = atn2_99nf,	atn2_100 = atn2_100,	atn2_100fu = atn2_100fu,	atn2_100nf = atn2_100nf,	atn2_101 = atn2_101,	atn2_101fu = atn2_101fu,	atn2_101nf = atn2_101nf,	atn2_102 = atn2_102,	atn2_102fu = atn2_102fu,	atn2_102nf = atn2_102nf,	atn2_103 = atn2_103,	atn2_103fu = atn2_103fu,	atn2_103nf = atn2_103nf,	atn2_104 = atn2_104,	atn2_104fu = atn2_104fu,	atn2_104nf = atn2_104nf,	atn2_71 = atn2_71,	atn2_71h = atn2_71h,	atn2_71pc = atn2_71pc,	atn2_71fu = atn2_71fu,	atn2_71nf = atn2_71nf,	atn2_72 = atn2_72,	atn2_72h = atn2_72h,	atn2_72pc = atn2_72pc,	atn2_72fu = atn2_72fu,	atn2_72nf = atn2_72nf,	atn2_72fun = atn2_72fun,	atn2_73 = atn2_73,	atn2_73h = atn2_73h,	atn2_73pc = atn2_73pc,	atn2_73fu = atn2_73fu,	atn2_73nf = atn2_73nf,	atn2_77 = atn2_77,	atn2_77h = atn2_77h,	atn2_77pc = atn2_77pc,	atn2_77es = atn2_77es,	atn2_77fu = atn2_77fu,	atn2_77nf = atn2_77nf,	atn2_79 = atn2_79,	atn2_79h = atn2_79h,	atn2_79pc = atn2_79pc,	atn2_79fu = atn2_79fu,	atn2_79nf = atn2_79nf,	atn2_83 = atn2_83,	atn2_83h = atn2_83h,	atn2_83pc = atn2_83pc,	atn2_83fu = atn2_83fu,	atn2_83nf = atn2_83nf,	atn2_74 = atn2_74,	atn2_74h = atn2_74h,	atn2_74me = atn2_74me,	atn2_74m = atn2_74m,	atn2_74e = atn2_74e,	atn2_74fu = atn2_74fu,	atn2_74nf = atn2_74nf,	atn2_75 = atn2_75,
                             atn2_75h = atn2_75h,	atn2_75me = atn2_75me,	atn2_75m = atn2_75m,	atn2_75e = atn2_75e,	atn2_75fu = atn2_75fu,	atn2_75nf = atn2_75nf,	atn2_76 = atn2_76,	atn2_76h = atn2_76h,	atn2_76me = atn2_76me,	atn2_76m = atn2_76m,	atn2_76e = atn2_76e,	atn2_76fu = atn2_76fu,	atn2_76nf = atn2_76nf,	atn2_78 = atn2_78,	atn2_78h = atn2_78h,	atn2_78me = atn2_78me,	atn2_78m = atn2_78m,	atn2_78e = atn2_78e,	atn2_78es = atn2_78es,	atn2_78fu = atn2_78fu,	atn2_78nf = atn2_78nf,	atn2_80 = atn2_80,	atn2_80h = atn2_80h,	atn2_80me = atn2_80me,	atn2_80m = atn2_80m,	atn2_80e = atn2_80e,	atn2_80fu = atn2_80fu,	atn2_80nf = atn2_80nf,	atn2_81 = atn2_81,	atn2_81h = atn2_81h,	atn2_81me = atn2_81me,	atn2_81m = atn2_81m,	atn2_81e = atn2_81e,	atn2_81aud = atn2_81aud,	atn2_81fu = atn2_81fu,	atn2_81nf = atn2_81nf,	atn2_82 = atn2_82,	atn2_82h = atn2_82h,	atn2_82me = atn2_82me,	atn2_82m = atn2_82m,	atn2_82e = atn2_82e,	atn2_82ec = atn2_82ec,	atn2_82fu = atn2_82fu,	atn2_82nf = atn2_82nf,	atn3_26 = atn3_26,	atn3_26h = atn3_26h,	atn3_26a = atn3_26a,	atn3_26fun = atn3_26fun,	atn3_28 = atn3_28,	atn3_28h = atn3_28h,	atn3_28a = atn3_28a,	atn3_28fun = atn3_28fun,	atn3_30 = atn3_30,	atn3_30h = atn3_30h,	atn3_30a = atn3_30a,	atn3_30fun = atn3_30fun
                             
      )
      datos_p4 <- data.frame(atn3_31 = atn3_31,	atn3_31h = atn3_31h,	atn3_31a = atn3_31a,	atn3_31fun = atn3_31fun,	atn3_33 = atn3_33,	atn3_33h = atn3_33h,	atn3_33a = atn3_33a,	atn3_33fun = atn3_33fun,	atn3_37 = atn3_37,	atn3_37h = atn3_37h,	atn3_37a = atn3_37a,	atn3_37fun = atn3_37fun,	atn3_38 = atn3_38,	atn3_38h = atn3_38h,	atn3_38a = atn3_38a,	atn3_38fun = atn3_38fun,	atn3_42 = atn3_42,	atn3_42h = atn3_42h,	atn3_42a = atn3_42a,	atn3_42fun = atn3_42fun,	atn3_47 = atn3_47,	atn3_47h = atn3_47h,	atn3_47a = atn3_47a,	atn3_47fun = atn3_47fun,	atn3_48 = atn3_48,	atn3_48h = atn3_48h,	atn3_48a = atn3_48a,	atn3_48fun = atn3_48fun,	atn3_50 = atn3_50,	atn3_50h = atn3_50h,	atn3_50a = atn3_50a,	atn3_50fun = atn3_50fun,	atn3_52 = atn3_52,	atn3_52h = atn3_52h,	atn3_52a = atn3_52a,	atn3_52fun = atn3_52fun,	atn3_54 = atn3_54,	atn3_54h = atn3_54h,	atn3_54a = atn3_54a,	atn3_54fun = atn3_54fun,	atn3_55 = atn3_55,	atn3_55h = atn3_55h,	atn3_55a = atn3_55a,	atn3_55fun = atn3_55fun,	atn3_59 = atn3_59,	atn3_59h = atn3_59h,	atn3_59a = atn3_59a,	atn3_59fun = atn3_59fun,	atn3_60 = atn3_60,	atn3_60h = atn3_60h,	atn3_60a = atn3_60a,	atn3_60fun = atn3_60fun,	atn3_61 = atn3_61,	atn3_61h = atn3_61h,	atn3_61a = atn3_61a,	atn3_61fun = atn3_61fun,	atn2_44 = atn2_44,	atn2_44h = atn2_44h,	atn2_44me = atn2_44me,	atn2_44m = atn2_44m,	atn2_44e = atn2_44e,	atn2_44con = atn2_44con,	atn2_44fun = atn2_44fun,	atn2_13 = atn2_13,	atn2_13h = atn2_13h,	atn2_13me = atn2_13me,	atn2_13m = atn2_13m,	atn2_13e = atn2_13e,	atn2_13op = atn2_13op,	atn2_13fun = atn2_13fun,	atn2_13ch = atn2_13ch,
                             atn2_14 = atn2_14,	atn2_14h = atn2_14h,	atn2_14me = atn2_14me,	atn2_14m = atn2_14m,	atn2_14e = atn2_14e,	atn2_14op = atn2_14op,	atn2_14fun = atn2_14fun,	atn2_14q = atn2_14q,	atn2_14qt = atn2_14qt,	atn2_105 = atn2_105,	atn3_01 = atn3_01,	atn3_68 = atn3_68,	atn3_04 = atn3_04,	atn3_04h = atn3_04h,	atn3_04fun = atn3_04fun,	atn3_05 = atn3_05,	atn3_05h = atn3_05h,	atn3_05fun = atn3_05fun,	atn3_06 = atn3_06,	atn3_06h = atn3_06h,	atn3_06fun = atn3_06fun,	atn3_07 = atn3_07,	atn3_07h = atn3_07h,	atn3_07fun = atn3_07fun,	atn3_09 = atn3_09,	atn3_09h = atn3_09h,	atn3_09fun = atn3_09fun,	atn3_10 = atn3_10,	atn3_10h = atn3_10h,	atn3_10fun = atn3_10fun,	atn3_11 = atn3_11,	atn3_11h = atn3_11h,	atn3_11fun = atn3_11fun,	atn3_12 = atn3_12,	atn3_12h = atn3_12h,	atn3_12fun = atn3_12fun,	atn3_13 = atn3_13,	atn3_13h = atn3_13h,	atn3_13fun = atn3_13fun,	atn3_14 = atn3_14,	atn3_14h = atn3_14h,	atn3_14fun = atn3_14fun,	atn3_15 = atn3_15,	atn3_15h = atn3_15h,	atn3_15fun = atn3_15fun,	atn3_16 = atn3_16,	atn3_16h = atn3_16h,	atn3_16fun = atn3_16fun,	atn3_17 = atn3_17,	atn3_17h = atn3_17h,	atn3_17fun = atn3_17fun,	atn3_18 = atn3_18,	atn3_18h = atn3_18h,	atn3_18fun = atn3_18fun,	atn3_19 = atn3_19,	atn3_19h = atn3_19h,	atn3_19fun = atn3_19fun,	atn3_21 = atn3_21,	atn3_21h = atn3_21h,	atn3_21fun = atn3_21fun,	atn3_22 = atn3_22,	atn3_22h = atn3_22h,	atn3_22fun = atn3_22fun,	atn3_23 = atn3_23,	atn3_23h = atn3_23h,	atn3_23fun = atn3_23fun,	atn3_24 = atn3_24,	atn3_24h = atn3_24h,	atn3_24fun = atn3_24fun,	atn3_20 = atn3_20,	atn3_20h = atn3_20h,	atn3_20fun = atn3_20fun,	atn3_20fu = atn3_20fu,	atn3_20nf = atn3_20nf,	atn3_62 = atn3_62,	atn3_62fu = atn3_62fu,	atn3_62nf = atn3_62nf,	atn3_65 = atn3_65,
                             atn3_65fu = atn3_65fu,	atn3_65nf = atn3_65nf,	atn3_66 = atn3_66,	atn3_66t = atn3_66t,	atn3_66fu = atn3_66fu,	atn3_66nf = atn3_66nf,	atn3_69 = atn3_69,	atn3_69h = atn3_69h,	atn3_69fu = atn3_69fu,	atn3_69nf = atn3_69nf,	atn3_69fun = atn3_69fun,	atn3_71 = atn3_71,	atn3_71a = atn3_71a,	atn3_72 = atn3_72,	atn3_72h = atn3_72h,	atn3_72fu = atn3_72fu,	atn3_72nf = atn3_72nf,	atn2_96 = atn2_96,	atn2_96sr = atn2_96sr,	atn2_96fu = atn2_96fu,	atn2_96nf = atn2_96nf,	atn2_96fun = atn2_96fun,	pve_88 = pve_88,	pve_88fu = pve_88fu,	pve_88nf = pve_88nf,	pve_89 = pve_89,	pve_89fu = pve_89fu,	pve_89nf = pve_89nf,	pve_90 = pve_90,	pve_90fu = pve_90fu,	pve_90nf = pve_90nf,	pve_91 = pve_91,	pve_91fu = pve_91fu,	pve_91nf = pve_91nf,	pve_92 = pve_92,	pve_92fu = pve_92fu,	pve_92nf = pve_92nf,	pve_93 = pve_93,	pve_93fu = pve_93fu,	pve_93nf = pve_93nf,	pve_94 = pve_94,	pve_94fu = pve_94fu,	pve_94nf = pve_94nf
      )
      #DBI::dbExecute(conn = db, statement = DBI::sqlAppendTable(name = 'respuestas', value = datos_pru, row.names = FALSE)) #el overwrite te borra todos los registros
      DBI::dbWriteTable(conn = db, name = 'respuestas_p1', value = datos_p1, row.names = FALSE, append = TRUE, overwrite = FALSE)
      DBI::dbWriteTable(conn = db, name = 'respuestas_p2', value = datos_p2, row.names = FALSE, append = TRUE, overwrite = FALSE)
      DBI::dbWriteTable(conn = db, name = 'respuestas_p3', value = datos_p3, row.names = FALSE, append = TRUE, overwrite = FALSE)
      DBI::dbWriteTable(conn = db, name = 'respuestas_p4', value = datos_p4, row.names = FALSE, append = TRUE, overwrite = FALSE)
      
      
      
      #saveData(formData())
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
      on.exit(DBI::dbDisconnect(db))
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


    
  #observeEvent(input$dependencia, {
  #  distritos <- catalogo
  #  
  #  updateSelectInput(session = session,
  #                    inputId = 'municipio',
  #                    label = 'Municipio',
  #                    choices = c(unique(distritos$MUNICIPIO[distritos$INSTITUCION == input$dependencia]), NULL),
  #                    selected = NULL)
    
 # })
  
  #observeEvent(input$municipio, {
  #  municipios <- catalogo
  #  options <-  unique(municipios$UNIDAD[municipios$MUNICIPIO == input$municipio])
  #  
  #  updateSelectInput(session = session,
  #                    inputId = 'nom_ins',
  #                    label = 'Nombre del lugar',
                      #choices = c(unique(municipios$UNIDAD[municipios$MUNICIPIO == input$municipio]), NULL),
    #                  choices = c(options, NULL),
   #                   selected = NULL)
  #  
  #})
  
  # Actualiza los municipios al cambiar la dependencia
  observeEvent(input$dependencia, {
    municipios_filtrados <- unidades[unidades$dependencia == input$dependencia, "municipio"]
    updateSelectInput(session, "municipio", choices = unique(municipios_filtrados))
  })
  
  # Actualiza los unidades al cambiar dependencia y municipio
  observeEvent(c(input$dependencia, input$municipio), {
    datos_filtrados <- unidades[unidades$dependencia == input$dependencia & unidades$municipio == input$municipio, ]
    updateSelectInput(session, "nom_ins", choices = unique(datos_filtrados$unidad))
  })
  
session$allowReconnect(TRUE)   
}



#}

shinyApp(ui, server, options = list(host = '0.0.0.0', port = 8000)
         ) 

#shinylive::export(appdir = './', destdir = './docs')
