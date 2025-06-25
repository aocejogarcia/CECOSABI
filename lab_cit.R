# Ejemplo de código a agregar --
#
# 

lab_cit <- checkboxInput(inputId = 'atn2_01', # Nombre de la variable en la base de datos, checkboxInput indica que la pregunta es de tipo casilla
                         label = 'Laboratorio de citologia, histopatologia y anatomia patologica', # Como se ve la pregunta en la aplicación
                         value = F), # Valor de default, en este caso significa que no está marcada la casilla
           numericInput(inputId = 'atn2_01m', # Nombre de pregunta de seguimiento, agregué el marcador "m" para designar que se refiere a personal médico, numericInput indica que solo se aceptan números en la respuesta
                        label = '¿Cuántos médicos hay en este servicio?', # Como se ve en el cuestionario
                        value = NA_integer_), # Valor por default vacio
           numericInput(inputId = 'atn2_01e', # Nombre de pregunta de seguimiento, agregué el marcador "e" para designar que se refiere a personal de enfermería
                        label = '¿Cuántos enfermeros hay en este servicio?',
                        value = NA_integer_),
           selectInput(inputId = 'atn2_01i', # # Nombre de pregunta de seguimiento, agregué el marcador "i" para designar que se refiere a insumos, selectInput nos permite seleccionar de una lista desplegable una opción
                       label = '¿Cuenta con el insumo para operar el servicio?', 
                       choices = c('Si', 'No', 'Parcial', 'NA'), # Opciones a seleccionar, solo se permite una
                       selected = 'NA') # Valor por defecto seleccionado
