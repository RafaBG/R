# Calculadora que permite tener un aproximado bastante real
# al calculo de ISR e IMSS
# Informacion actualizada al 2016
# Fuente ISR: http://www.sat.gob.mx/informacion_fiscal/tablas_indicadores/Paginas/tarifas_pp.aspx

library(shiny)
library(shinydashboard)
# library(DT)
library(dplyr)

# source("progressBox.R")

## Cargo informacion de ISR ----
# ISR <- readRDS(paste(dirname(rstudioapi::getActiveDocumentContext()$path),
#                       "/Data/ISR_Limites.Rds",sep = ""))

ISR <- readRDS(paste0("Data/ISR_Limites.Rds",sep = ""))

## Cargo informacion de IMSS ----
# IMSS <- readRDS(paste(dirname(rstudioapi::getActiveDocumentContext()$path),
#                      "/Data/IMSS_Porcentajes.Rds",sep = ""))

IMSS <- readRDS(paste0("Data/IMSS_Porcentajes.Rds",sep = ""))

# IMSS <- readRDS(paste("C:/Users/rafael.barragan/Documents/Codigos/Codes/Shiny/",
#                       "Calcus_Impuestos/Data/IMSS_Porcentajes.Rds",sep = ""))

## Listado de deducciones ----
ListaDeducciones <- c("Limite Inferior", "Excedente", "% Excedente"
                      , "Cuota Fija", "ISR Anual", "ISR Mensual"
                      , "ISR Quincenal", "Salario Neto Mensual"
                      , "Salario Neto Quincenal")
## String Meses ----
sMeses <- format(seq(as.Date("2000/1/1"), by = "month", length.out = 12), format = "%B")

## Funcion que calcula el salario ----
fISRAnual <- function(LimInf, CuotaFija, Aplicable, SalarioBrutoAnual){
  # Calculo Excedente al cual se le aplicara impuestos
  Excedente <- SalarioBrutoAnual - LimInf
  fISRAnual <- CuotaFija + Excedente*Aplicable
}

# Progress Box ----
progressBox <- function(title, value = 0, leftColor = "navy", rightColor = NULL, iconBx = "lightbulb-o"
                        , barStatus = 0, barColor = "light-blue", description){
  tags$div(class= paste0("info-box", if(!is.null(rightColor)) paste0(" bg-",rightColor)), checked=NA
           , tags$span(class = paste0("info-box-icon",if(!is.null(leftColor)) paste0(" bg-", leftColor))
                       , icon(iconBx)),
           tags$div(class="info-box-content",
                    tags$span(class ="info-box-text", title),
                    tags$span(class = "info-box-number",value),
                    tags$div(class = "progress",
                         tags$div(class = paste0("progress-bar", 
                                             if(is.null(rightColor)) paste0(" bg-", barColor))
                                             , style=paste0("width: ",barStatus,"%"))),
                    tags$span(class = "progress-description", description)
        )
      )
    }

## Copio desde Excel ----
# paste.table <- function() {
#   f <- file(description = 'clipboard', open = 'r')
#   df <- read.table(f, sep = '\t', header = TRUE)
#   close(f)
#   return(df)
# }

# ISR <- paste.table()
# saveRDS(ISR,"C:/Users/rafael.barragan/Documents/Codigos/Codes/Shiny/Calcus_Impuestos/Data/ISR_Limites.Rds")
# 
# IMSS <- paste.table()
# saveRDS(IMSS,"C:/Users/rafael.barragan/Documents/Codigos/Codes/Shiny/Calcus_Impuestos/Data/IMSS_Porcentajes.Rds")
