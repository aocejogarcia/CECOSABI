#Tabla descriptiva de las caracterísicas de la infraestructura
#############################################################################################################
###############################################ACTUALIZACIÓN 08/07/2025######################################
#############################################################################################################
ale <- column(12,
              checkboxInput(inputId = 'atn2_01', 
                         label = 'Consulta externa', 
                         value = F), 
            numericInput(inputId = 'atn2_01m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_01e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            selectInput(inputId = 'atn2_01ieq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            selectInput(inputId = 'atn2_01i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'),       
                       selected = 'NA'),
            checkboxInput(inputId = 'atn2_02', 
                          label = 'Consulta externa de especialidades', 
                          value = F),   
            numericInput(inputId = 'atn2_02me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_02m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_02e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            selectInput(inputId = 'atn2_02eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_02fu', 
                       label = '¿Cuanto equipo es funcional?',
                       value = NA_integer_), 
            numericInput(inputId = 'atn2_02nf', 
                       label = '¿Cuanto equipo no es funcional?',
                       value = NA_integer_),
            checkboxInput(inputId = 'atn2_03', 
                         label = 'Trabajo social', 
                         value = F), 
           numericInput(inputId = 'atn2_03t', 
                        label = '¿Cuántos trabajadores sociales hay en este servicio?', 
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_03i', 
                       label = '¿Cuenta con el insumo para operar el servicio?)',
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),    
           checkboxInput(inputId = 'atn2_04', 
                         label = 'Laboratorio clínico', 
                         value = F), 
            numericInput(inputId = 'atn2_04q',
                       label = '¿Cuántos químicos trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_04qmi',
                       label = '¿Cuántos microscopistas trabajan en el servicio?', 
                       value = NA_integer_),                       
            numericInput(inputId = 'atn2_04me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_04e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            selectInput(inputId = 'atn2_04eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_04fu', 
                       label = '¿Cuanto equipo es funcional?',
                       value = NA_integer_), 
            numericInput(inputId = 'atn2_04nf', 
                       label = '¿Cuanto equipo no es funcional?',
                       value = NA_integer_),
          checkboxInput(inputId = 'atn2_05', 
                         label = 'Farmacia', 
                         value = F), 
           numericInput(inputId = 'atn2_05pm', 
                        label = 'Total de personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye farmacéuticos, técnicos de farmacia, supervisor de inventario, etc.)', 
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_05i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),           
            checkboxInput(inputId = 'atn2_06', 
                         label = 'Telemedicina',
                         value = F), 
            numericInput(inputId = 'atn2_06m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_06e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_06pc',
                       label = 'Total de personal capacitado que puede brindar el servicio', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_06eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_06fu', 
                       label = '¿Cuanto equipo es funcional?',
                       value = NA_integer_), 
            numericInput(inputId = 'atn2_06nf', 
                       label = '¿Cuanto equipo no es funcional?',
                       value = NA_integer_), 

            checkboxInput(inputId = 'atn2_07', 
                         label = 'Urgencias', 
                         value = F), 
            numericInput(inputId = 'atn2_07me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_07m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_07e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),       
            selectInput(inputId = 'atn2_07tr', 
                       label = '¿Cuenta con con área de triage?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'), 
            numericInput(inputId = 'atn2_07cam', 
                       label = 'En área de observación ¿Con cuántas camas censables se cuentan en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),                                                              
            numericInput(inputId = 'atn2_07ch', 
                       label = '¿Cuantas salas de reanimación/choque hay en el servicio?(independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),
            numericInput(inputId = 'atn2_07ais', 
                       label = '¿Cuantas salas aisladas hay en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),    
                                         
          checkboxInput(inputId = 'atn2_08', 
                         label = 'Servicios de sangre',
                         value = F), 
            numericInput(inputId = 'atn2_08me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_08m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_08e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_08pm',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio',
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_08eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_08fu', 
                       label = '¿Cuanto equipo es funcional?',
                       value = NA_integer_), 
            numericInput(inputId = 'atn2_08nf', 
                       label = '¿Cuanto equipo no es funcional?',
                       value = NA_integer_),                         

          checkboxInput(inputId = 'atn2_09', 
                         label = 'Área de hospitalización', 
                         value = F), 
            numericInput(inputId = 'atn2_09me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_09m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_09e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_09op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (camilleros, intendencia, auxiuliares, etc)', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_09eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_09cam', 
                       label = '¿Cuál es el total de camas  censables disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_), 

          checkboxInput(inputId = 'atn2_10', 
                         label = 'Central de Esterilización y Equipos (CEYE)', 
                         value = F), 
            numericInput(inputId = 'atn2_10m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_10e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_10op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (técnicos, radiólogos, etc.)', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_10eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

                       
          checkboxInput(inputId = 'atn2_11', 
                         label = 'Quirófano', 
                         value = F), 
            numericInput(inputId = 'atn2_11me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),                         
            numericInput(inputId = 'atn2_11m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_11e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_11op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (técnicos, radiólogos, etc.)', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_11eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_11q', 
                       label = '¿Cuál es el total quirófanos disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),  
          
          checkboxInput(inputId = 'atn2_12', 
                         label = 'Área de recuperación', 
                         value = F), 
            numericInput(inputId = 'atn2_12me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),                         
            numericInput(inputId = 'atn2_12m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_12e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_12op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_12eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_12cam', 
                       label = '¿Cuál es el total de camas  censables disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_), 

          checkboxInput(inputId = 'atn2_13', 
                         label = 'Tococirugía (Labor, expulsión y recuperación)', 
                         value = F), 
            numericInput(inputId = 'atn2_13me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),                         
            numericInput(inputId = 'atn2_13m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_13e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_13op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_13eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_13q', 
                       label = '¿Cuál es el total quirófanos disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),  

          checkboxInput(inputId = 'atn2_14', 
                         label = 'Unidad de quemados', 
                         value = F), 
            numericInput(inputId = 'atn2_14me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),                         
            numericInput(inputId = 'atn2_14m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_14e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_14op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_14eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_14ch', 
                       label = '¿Cuantas salas de reanimación/choque hay en el servicio?(independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_), 
                       
          checkboxInput(inputId = 'atn2_15', 
                         label = 'Unidad de trasplantes',  
                         value = F), 
            numericInput(inputId = 'atn2_15me',
                       label = '¿Cuántos médicos especialistas trabajan en el servicio?', 
                       value = NA_integer_),                         
            numericInput(inputId = 'atn2_15m',
                       label = '¿Cuántos médicos generales trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_15e',
                       label = '¿Cuántos enfermeros trabajan en el servicio?', 
                       value = NA_integer_),
            numericInput(inputId = 'atn2_15op',
                       label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                       value = NA_integer_),                       
            selectInput(inputId = 'atn2_15eq', 
                       label = '¿Cuenta con el equipo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
            numericInput(inputId = 'atn2_15q', 
                       label = '¿Cuál es el total quirófanos disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)',
                       value = NA_integer_),            
            numericInput(inputId = 'atn2_15qt', 
                       label = '¿Cuál es el total unidad de cuidados intensivos disponibles en el servicio? (independientemente de si actualmente se encuentran o no ocupadas)', 
                       value = NA_integer_),   

          checkboxInput(inputId = 'atn2_16',                          
                         label = 'Almacén', 
                         value = F), 
           numericInput(inputId = 'atn2_16pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye farmacéuticos, técnicos de almacén, supervisor de inventario, etc.)', 
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_16carg', 
                       label = '¿Cuenta zona de carga y descarga?', 
                       choices = c('Si', 'No', 'NA'), 
                       selected = 'NA'),
           selectInput(inputId = 'atn2_16ah', 
                       label = '¿El almacén se encuentra dentro del hospital?', 
                       choices = c('Si', 'No', 'NA'), 
                       selected = 'NA'),   
            textInput(inputId = 'atn2_16d', 
                       label = 'En qué dirección se encuentra el almacén (calle, número, colonia, municipio, estado)', 
                       value = NA_character_),                       

          checkboxInput(inputId = 'atn2_17', 
                         label = 'Conservación y mantenimiento',
                         value = F), 
           numericInput(inputId = 'atn2_17pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye ingenieros, técnicos de mantenimiento, etc.)',
                        value = NA_integer_), 

          checkboxInput(inputId = 'atn2_18', 
                         label = 'Lavandería', 
                         value = F), 
           numericInput(inputId = 'atn2_18pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye lavanderos, técnicos de lavandería, etc.)',
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_18i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

          checkboxInput(inputId = 'atn2_19', 
                         label = 'Casa de máquinas',
                         value = F), 
           numericInput(inputId = 'atn2_19pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye ingenieros mecánicos, técnicos de mantenimiento, etc.)', 
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_19i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

######################################################
################  SEGUNDA PARTE ######################
######################################################                       

            checkboxInput(inputId = 'atn2_20', 
                         label = 'Subestación eléctrica', 
                         value = F), 
           numericInput(inputId = 'atn2_20pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye ingenieros eléctricos, técnicos de mantenimiento, etc.)', 
                        value = NA_integer_), 
           selectInput(inputId = 'atn2_20i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),
           numericInput(inputId = 'atn2_20se', 
                        label = '¿Cuantas subestaciones eléctricas funcionales hay en este servicio?', 
                        value = NA_integer_), 

            checkboxInput(inputId = 'atn2_21', 
                         label = 'Gases medicinales', 
                         value = F), 
            numericInput(inputId = 'atn2_21pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye ingenieros biomédicos, técnicos de gases medicinales, etc.)', 
                        value = NA_integer_), 
            selectInput(inputId = 'atn2_21i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

            checkboxInput(inputId = 'atn2_22', 
                         label = 'Baños y vestidores', 
                         value = F), 
            numericInput(inputId = 'atn2_22baf', 
                        label = '¿Cuantos baños y vestidores funcionales hay en este servicio?', 
                        value = NA_integer_), 
            numericInput(inputId = 'atn2_22ban', 
                        label = '¿Cuantos baños y vestidores no funcionales hay en este servicio?', 
                        value = NA_integer_),                         

            checkboxInput(inputId = 'atn2_23', 
                         label = 'Intendencia', 
                         value = F), 
            numericInput(inputId = 'atn2_23pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                        value = NA_integer_), 
            selectInput(inputId = 'atn2_23i', 
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

            checkboxInput(inputId = 'atn2_24', 
                         label = 'Vigilancia y seguridad', 
                         value = F), 
            numericInput(inputId = 'atn2_24pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                        value = NA_integer_), 

#GESTIÓN DE RESIDUOS HOSPITALARIOS

            checkboxInput(inputId = 'atn2_25', 
                         label = 'Almacén de residuos corrosivos, reactivos, explosivos e infecciosos', 
                         value = F), 
            numericInput(inputId = 'atn2_25Pm', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio (Equipo de salud que incluye químicos, técnicos de laboratorio, etc.)', 
                        value = NA_integer_), 
            selectInput(inputId = 'atn2_25in', 
                       label = '¿Cuenta con la infraestructura para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA'),

#Tóxico e inflamable (CRETI)

           checkboxInput(inputId = 'atn2_26', 
                         label = 'Almacén de residuos peligrosos biológico infecciosos (RPBIs)', 
                         value = F), 
            numericInput(inputId = 'atn2_26m', 
                        label = 'Total de otro personal que integra el equipo multidisciplinario que trabaja en el servicio', 
                        value = NA_integer_), 
            selectInput(inputId = 'atn2_26i', 
                       label = '¿Cuenta con la infraestructura para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), 
                       selected = 'NA') 

                                 
)

ale2 <- column(12,
#Oficinas directivas administrativas
            checkboxInput(inputId = 'atn2_27', 
                         label = 'Gobierno', 
                         value = F),  
            checkboxInput(inputId = 'atn2_28', 
                         label = 'Recursos humanos', 
                         value = F),                                               
            checkboxInput(inputId = 'atn2_29', 
                         label = 'Calidad', 
                         value = F),                                 
            checkboxInput(inputId = 'atn2_30', 
                         label = 'Enseñanza', 
                         value = F),        
            checkboxInput(inputId = 'atn2_31', 
                         label = 'Investigación', 
                         value = F),    
            checkboxInput(inputId = 'atn2_32', 
                         label = 'Planeación y desarrollo', 
                         value = F),                                  
            checkboxInput(inputId = 'atn2_33', 
                         label = 'Recursos financieros', 
                         value = F),       
            checkboxInput(inputId = 'atn2_34', 
                         label = 'Recursos materiales', 
                         value = F),                                                           
            checkboxInput(inputId = 'atn2_35', 
                         label = 'Unidad jurídica', 
                         value = F),            
            checkboxInput(inputId = 'atn2_36', 
                         label = 'Informática', 
                         value = F),                                  
#OTRAS
            checkboxInput(inputId = 'atn2_37', 
                         label = 'Mortuorio', 
                         value = F),       
            checkboxInput(inputId = 'atn2_38', 
                         label = 'Cocina/Comedor', 
                         value = F),            
            checkboxInput(inputId = 'atn2_39', 
                         label = 'Residencias médicas', 
                         value = F),                 
            checkboxInput(inputId = 'atn2_40', 
                         label = 'Albergue comunitario', 
                         value = F),                  
            checkboxInput(inputId = 'atn2_41', 
                         label = 'Informática', 
                         value = F),   
            checkboxInput(inputId = 'atn2_42', 
                         label = 'Ingeniería biomédica', 
                         value = F),   
            checkboxInput(inputId = 'atn2_43', 
                         label = 'Dormitorios', 
                         value = F) 

              )
