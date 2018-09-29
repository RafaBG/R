library(shiny)

Tamano_Barras <- 300

## SideBar ----
Sidebar <- dashboardSidebar(
## Tamaño Sidebar ----
  width = Tamano_Barras-100,
## SideBar items ----
  sidebarMenu(
    menuItem("Retencion ISR", tabName = "ISR_Calculo", icon = icon("hand-spock-o"))
    , menuItem("Retencion IMSS", tabName = "IMSS_Calculo", icon = icon("hand-spock-o"))
    , menuItem("Salario", tabName = "Salario_Calculo", icon = icon("hand-spock-o"))
    # , menuItem("Optimizador", tabName = "Optimizador_Calculo", icon = icon("exchange"))
    # , menuItem("Pruebas", tabName = "Pruebas", icon = icon("spinner"))
  )
)

## Body ----
Body <- dashboardBody(
## Body Items ----
  tabItems(
## Body Salario ----
    tabItem(tabName = "ISR_Calculo",
            ## Esta seccion solo funciona con los strings predefinidos
            ## primary, success, info, warning y se cambia de acuerdo a la necesidad
            tags$style(HTML("
                  .box.box-solid.box-info>.box-header {
                    color:#F72C00;background:#F72C00}
                  .box.box-solid.box-info{
                    border-bottom-color:#3ED4C8;
                    border-left-color:#3ED4C8;
                    border-right-color:#3ED4C8;
                    border-top-color:#3ED4C8;}")),
            h1("Ingresa salario"),
            fluidRow(width = 5
              , box(width = 5, title = "Monto de ISR", status = "primary"
                     , color = "blue", solidHeader = TRUE, background = "navy"
                     , fluidRow(width = 12, uiOutput("ISR_Anual"))
                     , fluidRow(width = 12, uiOutput("ISR_MenQuin"))
                     )
              , box(width = 6, title = "Caracteristicas", status = "primary"
                  , color = "blue", solidHeader = TRUE, background = "navy",
                selectInput(inputId = "tSalario"
                            , label = "Tipo de Salario (Quincenal-Mensual)"
                            , choices = c("Quincenal", "Mensual")
                            , selected = "Mensual"),
                numericInput(inputId = "Salario", label = "Salario del periodo", min = 1
                             , step = 500, value = 22733.23)
                  )
                )
            , fluidRow(width = 12,
              # column(width = 6,
              box(width = 5, title = "Mas detalles", status = "primary"
                  , color = "blue", solidHeader = TRUE, background = "navy"
                  , collapsible = TRUE, collapsed = TRUE
                  , fluidRow(width = 12, uiOutput("oLimiteInferior"))
                  , fluidRow(width = 12, uiOutput("oExcedente"))
                  , fluidRow(width = 12, uiOutput("oCuotaFija"))
                )
              # )
            )
            # , fluidRow(width = 12,
            #          tags$head(tags$style(HTML('.small-box {min-height: 25px;} 
            #                                    .small-box-icon {height: 25px; line-height: 25px;} 
            #                                    .small-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
            #   box(width = 8, solidHeader = TRUE, 
            #     box(width = 8, background = "blue", height = 300,
            #         "jola"#uiOutput("Prueba3")
            #         )
            #       
            #     )
            #   )
    ),
## Body IMSS ----
tabItem(tabName = "IMSS_Calculo",
        tags$style(HTML("
                  .box.box-solid.box-info>.box-header {
                    color:#F72C00;background:#F72C00}
                  .box.box-solid.box-info{
                    border-bottom-color:#3ED4C8;
                    border-left-color:#3ED4C8;
                    border-right-color:#3ED4C8;
                    border-top-color:#3ED4C8;}")),
        fluidRow(width = 6,
                   box(width = 8, status = "primary" #, collapsible = TRUE, collapsed = TRUE
                       , title = "Descripción", solidHeader = TRUE
                       , h4("Para el cálculo del Salario Diario Integrado (SDI) es necesario ingresar las 
                        distintas prestaciones que ofrece tu empresa."))
                )
        , fluidRow(width = 12
             , column(width = 5
                          , box(width = 12, title = "Retencion IMSS", solidHeader = TRUE, background = "navy"
                                , status = "primary", color = "blue"
                                , uiOutput("oIMSSRet"))
                          , box(width = 12, title = "Detalles", solidHeader = TRUE
                                , background = "navy", collapsible = TRUE, collapsed = TRUE
                                , fluidRow(width = 6
                                           , uiOutput("oSDI")
                                           , uiOutput("oEsEx")
                                           , uiOutput("oPrestaciones")
                                           , uiOutput("oPensiones")
                                           , uiOutput("oInvalidez")
                                           , uiOutput("oCesantia")
                                           , uiOutput("oIMSSMens")
                                           , uiOutput("oCesantiaB")
                                )
                          )
                 )
        , fluidRow(width = 6
                        , column(width = 6
                                 , div(class = "well well-sm"
                                       ,style = "background-color: #3c8dbc;color: white"
                                       ,h4("Ingrese prestaciones:")
                                 )
                        )
        , column(width =3
            , fluidRow(width = 12, 
              box(width = 12, solidHeader = TRUE, status = "info", background = "light-blue"#, height = 485
                  , selectInput("MesCons", label = "Selecciona mes de consulta.",
                                choices = sMeses, selected = format(Sys.Date(), format = "%B"))#format(Sys.Date(), format = "%B")
                  , numericInput(inputId = "SalMens" , label = "Salario Mensual(Bruto)"
                              , value = 22733.23, step = 500)
                  , numericInput(inputId = "Antiguedad" , label = "Antigüedad (Años)"
                                 , value = 1, step = 1)
                  , numericInput(inputId = "PrimVac" , label = "Prima Vacacional (%)"
                                 , value = 25, step = 1)
                  # , numericInput(inputId = "Aguinaldo" , label = "Aguinaldo (dias)"
                  #                , value = 15, step = 5)
                    )
                  )
                )
                  # , includeCSS("www/Box_Style.css")
        , column(width = 3
            , fluidRow(width = 12
                  , box(width = 12, solidHeader = TRUE, status = "info", background = "light-blue"
                  # , numericInput(inputId = "SMV", label = "Salario Minimo Vigente DF"
                  #                      , value = 73.04, min = 73.04, step = 2)
                  , numericInput(inputId = "Aguinaldo" , label = "Aguinaldo (dias)"
                                 , value = 15, step = 5)
                  , tags$div(class = "well well-sm", style = "background-color: #3c8dbc;"
                                  , "Vacaciones por ley:"
                                  , tags$h4(textOutput("oDiasVac") #textOutput("oDiasVac")
                                            , align = "center"))
                  , tags$div(class = "well well-sm", style = "background-color: #3c8dbc;"
                             , "Salario Minimo Vigente:"
                             , tags$h4(textOutput("oSMV")
                                       , align = "center"))
                  , checkboxInput(inputId = "CusDiasVac", label = "Modificar vacaciones", value = FALSE)
                  , conditionalPanel(
                          condition = "input.CusDiasVac == true",
                          numericInput(inputId = "DiasVacMod", label = "Dias de Vacaciones", value = 6)
                    )
                  )
                )
              )
            )
          )
        ),
## Body Optimizador ----
    tabItem(tabName = "Optimizador",
            h2("¿Cuanto dinero neto quieres ganar?"),
            tags$div(class = "info-box bg-red")),
    tabItem(tabName = "Pruebas",
        h2("¿Cuanto dinero neto quieres tener?"),
        tags$div(class = "info-box bg-red"),
        box(width = 5
            , tags$div(class="info-box bg-light-blue", checked=NA
                       , tags$span(class = "info-box-icon", icon("minus-square")),
                       tags$div(class="info-box-content",
                                tags$span(class ="info-box-text", "Likes"),
                                tags$span(class = "info-box-number","41400"),
                                tags$div(class = "progress",
                                         tags$div(class = "progress-bar", style="width: 70%")),
                                tags$span(class = "progress-description", "70% Increase in 30 Days")
                       )
                    )
                  )
            # ,<div style="clear:both">
            #   <a href="http://www.visualcapitalist.com/chart-epic-collapse-deutsche-bank/">
            #   <img src="http://2oqz471sa19h3vbwa53m33yj.wpengine.netdna-cdn.com/wp-content/uploads/2016/07/deutsche-bank-fall-chart.png" border="0" />
            #   </a>
            #   </div>
            #   <div>Courtesy of: <a href="http://www.visualcapitalist.com">Visual Capitalist</a></div>
            , tags$div(style = "clear:both"
                       , tags$a(
                         href = "visualcapitalist.com/chart-epic-collapse-deutsche-bank/",
                         tags$img(src = paste0("http://2oqz471sa19h3vbwa53m33yj.wpengine.netdna-cdn.com/wp-content/"
                                               ,"uploads/2016/07/deutsche-bank-fall-chart.png",sep = "")
                                  , width = "600px", height = "450px")
                            )
                          )
            # , progressBox(title = "Prueba", iconBx = "minus-square"
            #               , value = 50, barStatus = 50, description = "subtitulo")
        )
  )
)

# Dashboard Page ----
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Calculadora de Impuestos", titleWidth = Tamano_Barras),
                    Sidebar,
                    Body
)
