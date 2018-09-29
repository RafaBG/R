library(shiny)

server <- shinyServer(function(input, output) {

## Salario: Tipo de Salario ----
  TipoSal <- reactive({
                TipoSalario <- switch(input$tSalario,
                                    "Mensual" = 1,
                                    "Quincenal" = 2)
                
                TipoSalario})
  
## Salario: Salario Anual ----  
  SalarioBaseAnual <- reactive({
    SalBAnual <- TipoSal()*input$Salario*12
  })

## Salario: Limite Inferio ----
  LimInferior <- reactive({
    
    SalBAnual <- SalarioBaseAnual()
    
    Lim_Inf <- ISR %>%
      filter(Limite_Inferior <= SalBAnual,
             Limite_Superior >= SalBAnual) %>%
      select(Limite_Inferior)
    
    Lim_Inf
  })

## Salario: Cuota Fija ----  
  CuotaFija <- reactive({
    
    SalBAnual <- SalarioBaseAnual()
    
    Cuota_fija <- ISR %>%
      filter(Limite_Inferior <= SalBAnual,
             Limite_Superior >= SalBAnual) %>%
      select(Cuota_fija)
    
    Cuota_fija
  })
  
## Salario: Aplicable ----
  Aplicable <- reactive({
    
    SalBAnual <- SalarioBaseAnual()
    
    Aplicable <- ISR %>%
      filter(Limite_Inferior <= SalBAnual,
             Limite_Superior >= SalBAnual) %>%
      select(Aplicable)
    
    Aplicable
  })

## Salario: Excedente ----  
  Excedente <- reactive({
      Excedente <- SalarioBaseAnual() - LimInferior()
    })

## IMSS: Salario Minimo ----
  SMV <- 73.04
## IMSS: Vacaciones por Ley ----
  VPL <- reactive({
    if (input$Antiguedad <5)
      {DV <- 2*input$Antiguedad+4}
    else
      {DV <- 12 + as.integer(input$Antiguedad/5)*2}
    
    DV
  })
## IMSS: Dias del Mes ----
  DiasMes <- reactive({
      ## Obtengo el Mes y año de consulta
      DM <- which(sMeses %in% input$MesCons)
      AnoConsulta <- format(Sys.Date(),"%Y")
      ## Obtengo los dias del mes de la consulta
      Dias <- format(as.Date(paste0("01/",(DM %% 12)+1,"/", AnoConsulta),"%d/%m/%Y")-1,"%d")
    })

## IMSS: SDI ----
  SDI <- reactive({
    
    SalMaxApl <- SMV*25
    
    if(input$CusDiasVac == TRUE)
      {Vacaciones <- input$DiasVacMod}
    else
      {Vacaciones <- VPL()}
    
    FactorSinTrunc <- (((Vacaciones*input$PrimVac/100)+input$Aguinaldo)/365)+1
    FactorTruncado <- trunc(FactorSinTrunc*10000)/10000
    
    SalMensual <- input$SalMens
    DM <- as.numeric(DiasMes())
    
    SalarioXFact <- FactorTruncado * SalMensual/DM
      
    if (SalarioXFact < SalMaxApl)
      {SalDI <- SalarioXFact}
    else
      {SalDI <- SalMaxApl}
    
    SalDITruncado <- trunc(SalDI*100)/100
  })

## IMSS: Especie Excedente ----
  Espec_Exed <- reactive({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Enfermedades_Maternidad2 == "Especie_Excedente") %>%
      select(Empleado)
    ## Valido la informacion
    if(SDI() > 3*SMV)
      {Esp_Ex <- Retenciones_IMSS}
    else
      {Esp_Ex <- 0}
    
    DM <- as.numeric(DiasMes())
    
    Retencion <- Esp_Ex/100 * (SDI()-(3*SMV)) * DM
    
  })

## IMSS: Prestaciones en Dinero ----
  Prestaciones <- reactive({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Enfermedades_Maternidad2 == "Prestaciones") %>%
      select(Empleado)
    
    DM <- as.numeric(DiasMes())
    
    Retencion <- Retenciones_IMSS/100 * SDI() * DM
    
  })

## IMSS: Pensiones y Beneficiarios ----
  Pensiones <- reactive({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Enfermedades_Maternidad2 == "Pensionados_y_Beneficiarios") %>%
      select(Empleado)
    
    DM <- as.numeric(DiasMes())
    
    Retencion <- Retenciones_IMSS/100 * SDI() * DM
    
  })
  
## IMSS: Invalidez y Vida ----
  Invalidez <- reactive({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Enfermedades_Maternidad2 == "Invalidez_y_Vida") %>%
      select(Empleado)
    
    DM <- as.numeric(DiasMes())
    
    Retencion <- Retenciones_IMSS/100 * SDI() * DM
    
  })

## IMSS: Cesantia y Vejez ----
  Cesantia <- reactive({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Enfermedades_Maternidad2 == "Cesantia_y_vejez") %>%
      select(Empleado)
    
    DM <- as.numeric(DiasMes())
    
    Retencion <- Retenciones_IMSS/100 * SDI() * DM
    
  })
  
## IMSS: IMSS Mensual ----
  IMSSMens <- reactive({
    
    if (Espec_Exed() + Prestaciones() + Pensiones() + Invalidez() > 78.3) 
      {dValue <- Espec_Exed() + Prestaciones() + Pensiones() + Invalidez()}
    else
      {dValue <- 0}

  })
  
## IMSS: Cesantia y Vejez ----
  CesantiaB <- reactive({
    
    if (Cesantia() > 78.3) 
      {dValue <- Cesantia()}
    else
      {dValue <- 0}
    
  })
  
## IMSS: Retencion IMSS Mensual----
  IMSSRetencion <- reactive({
    
    if (Cesantia() > 78.3) 
      {dValue <- CesantiaB()+ IMSSMens()}
    else
      {dValue <- 0}
    
  })
  
## AAA IMSS: Retencion IMSS ----
  output$PruebaRet <- renderTable({
    
    ## Obtengo la informacion de las retenciones del IMSS
    Retenciones_IMSS <- IMSS %>%
      filter(Empleado > 0) %>%
      select(Enfermedades_Maternidad, Enfermedades_Maternidad2, Empleado)
    ## Valido la informacion
    if(SDI() > 3*SMV)
        {Esp_Ex <- Retenciones_IMSS[which("Especie_Excedente"==Retenciones_IMSS[2]),3]}
    else
        {Esp_Ex <- 0}
    
    DM <- as.numeric(DiasMes())
    
    Retenciones_Montos <- matrix(NaN, length(Retenciones_IMSS[,3]))
    
    for(i in 1:length(Retenciones_IMSS[,3]))
      {if (Retenciones_IMSS[i,2]=="Especie_Excedente")
        {Retenciones_Montos[i,1]<- Esp_Ex/100 * (SDI()-(3*SMV)) * DM}
      # {Retenciones_Montos[i,1]<- Esp_Ex}
          else
            {Retenciones_Montos[i,1] <- Retenciones_IMSS[i,3]/100 * SDI() * DM}
            # {Retenciones_Montos[i,1] <- 725.2*DM*Retenciones_IMSS[i,3]/100}#SDI()*DM*Retenciones_IMSS[i,3]}
      }
    
    df <- data.frame(as.data.frame.vector(Retenciones_IMSS[,1]),Retenciones_Montos)
    df
  })
  
## outPut ISR: ISR Anual ----
  output$ISR_Anual <- renderUI({

          ISR_Anual <- (CuotaFija()+Excedente()*Aplicable()/100)
          
          valueBox(value = prettyNum(ISR_Anual,big.mark=",",scientific=FALSE)#Aplica
                   , subtitle = "ISR Anual:"
                   , icon = icon("money")
                   , color = "navy"
                   , width = 12)
    
  })

## outPut ISR: ISR MensQuin ----
  output$ISR_MenQuin <- renderUI({
    
    ISR_Anual <- ((CuotaFija()+Excedente()*Aplicable()/100)/(TipoSal()*12))
    
    if (TipoSal()==1)
      {sLabel <- "ISR Mensual:"}
    else
      {sLabel <- "ISR Quincenal:"}
    
    valueBox(value = prettyNum(ISR_Anual,big.mark=",",scientific=FALSE)#Aplica
             , subtitle = sLabel
             , icon = icon("money")
             , color = "navy"
             , width = 12)
    
  })

## outPut ISR: Limite Inferior ----  
  output$oLimiteInferior <- renderUI({

    valueBox(value = prettyNum(LimInferior(),big.mark=",",scientific=FALSE)
             , subtitle = "Limite Inferior:"
             , icon = icon("dollar")
             , color = "navy"
             , width = 12)
  })
## outPut ISR: Cuota Fija ----  
  output$oCuotaFija <- renderUI({

    valueBox(value = prettyNum(CuotaFija(),big.mark=",",scientific=FALSE)
             , subtitle = "Cuota Fija:"
             , icon = icon("angle-double-down")
             , color = "navy"
             , width = 12)
  })
## outPut ISR: Excedente ----  
  output$oExcedente <- renderUI({
    
    valueBox(value = prettyNum(Excedente(), big.mark=",",scientific=FALSE)
             , subtitle = "Excedente:"
             , icon = icon("level-up")
             , color = "navy"
             , width = 12)
  })

## outPut IMSS: SMV ----
  output$oSMV <- renderText({SMV})

## outPut IMSS: SDI ----
  output$oSDI <- renderUI({

    valueBox(value = prettyNum(formatC(SDI(), digits =2, format = "f"),big.mark=",",scientific=FALSE)
             , subtitle = "SDI:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: Especie - Excedente ----
  output$oEsEx <- renderUI({
    
    dValue <- trunc(Espec_Exed()*100)/100
    
    valueBox(value = prettyNum(dValue, big.mark=",",scientific=FALSE)
             , subtitle = "Especie y Excedente:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })

## outPut IMSS: Prestaciones en Dinero ----
  output$oPrestaciones <- renderUI({
    
    dValue <- trunc(Prestaciones()*100)/100
    
    valueBox(value = prettyNum(dValue,big.mark=",",scientific=FALSE)
             , subtitle = "Prestaciones:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: Pensiones y Beneficiarios ----
  output$oPensiones <- renderUI({
    
    dValue <- trunc(Pensiones()*100)/100
    
    valueBox(value = prettyNum(dValue,big.mark=",",scientific=FALSE)
             , subtitle = "Pensio. y benef.:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: Invalidez y Vida ----
  output$oInvalidez <- renderUI({
    
    dValue <- trunc(Invalidez()*100)/100
    
    valueBox(value = prettyNum(dValue,big.mark=",",scientific=FALSE)
             , subtitle = "Invalidez y vida:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: Cesantia y Vejez ----
  output$oCesantia <- renderUI({
    
    dValue <- trunc(Cesantia()*100)/100
    
    valueBox(value = prettyNum(dValue, big.mark=",",scientific=FALSE)
             , subtitle = "Cesantia y vejez:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: IMSS Mensual ----
  output$oIMSSMens <- renderUI({
    
    dValue <- trunc(IMSSMens()*100)/100
    
    valueBox(value = prettyNum(dValue, big.mark=",",scientific=FALSE)
             , subtitle = "IMSS Mensual:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })
  
## outPut IMSS: Cesantia Bimestral ----
  output$oCesantiaB <- renderUI({
    
    dValue <- trunc(CesantiaB()*100)/100
    
    valueBox(value = prettyNum(dValue, big.mark=",",scientific=FALSE)
             , subtitle = "Cesantia Bimestral:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 6)
  })

## outPut IMSS: IMSS Retencion ----
  output$oIMSSRet <- renderUI({
    
    dValue <- trunc(IMSSRetencion()*100)/100
    
    valueBox(value = prettyNum(dValue, big.mark=",",scientific=FALSE)
             , subtitle = "Retencion IMSS:"
             , icon = icon("hand-spock-o")
             , color = "navy"
             , width = 12)
  })
  
## outPut IMSS: Dias del Mes ----
  output$DiasMes <- renderText({
    DiasMes()
  })

  ## Pruebas Print ----  
  output$vLimInf <- renderText({SMV})
  output$vCuotaF <- renderText({CuotaFija()})
  output$vExcedente <- renderText({Excedente()})
  output$vISR <- renderText({ISR_Anual <- (CuotaFija()+Excedente()*Aplicable()/100)})

## Dias oficiales de vacaciones ----
  output$oDiasVac <- renderText({
      VPL()
    })

  output$Prueba <- renderPrint({50})
  
  output$Prueba2 <- renderText({sprintf("Salario actual: %d", 50)})
  output$Prueba3 <- renderUI({
                      valueBox(value = 50
                          , subtitle = "Salario actual:"
                          , icon = icon("list")
                          , color = "blue"
                          , width = 12)})
  output$Prueba4 <- renderUI({
                      valueBox(value = 50
                               , subtitle = "Salario actual:"
                               , icon = icon("list")
                               , color = "blue"
                               , width = 12)})
  
  output$Salario <- renderDataTable({
    datatable(tbSalario,colnames = c("LimI", "Cuota", "Porcentaje"))
  # output$Salario2 <- renderDataTable({
  #   tbSalario
  })
  

  # 
  # ISRAnual <- fISRAnual(Variables[1], Variables[2], Variables[3], SalBAnual)
  # 
  # DeduccionesSalario <- c(Variables[1] , SalBAnual - Variables[1], Variables[3]
  #                         , Variables[2], ISRAnual, ISRAnual/12, ISRAnual/24
  #                         , (SalBAnual - ISRAnual)/12,(SalBAnual - ISRAnual)/24)
  # 
  # tbSal <- data.frame(ListaDeducciones, DeduccionesSalario)
  # tbSal
  
  # output$tbSueldo <- DT::renderDataTable(DT::datatable({
  #   SalBAnual <- input$tSalario*input$Salario*12
  # 
  # Variables <- ISR %>%
  #   filter(Limite_Inferior <= SalBAnual, 
  #          Limite_Superior >= SalBAnual) %>%
  #   select(Limite_Inferior,Cuota_fija,Aplicable)
  # 
  # ListaDeducciones <- c("Limite Inferior", "Excedente", "% Excedente"
  #                       , "Cuota Fija", "ISR Anual", "ISR Mensual"
  #                       , "ISR Quincenal", "Salario Neto Mensual"
  #                       , "Salario Neto Quincenal")
  # 
  # ISRAnual <- fISRAnual(Variables[1], Variables[2], Variables[3], SalBAnual)
  # 
  # DeduccionesSalario <- c(Variables[1] , SalBAnual - Variables[1], Variables[3]
  #                         , Variables[2], ISRAnual, ISRAnual/12, ISRAnual/24
  #                         , (SalBAnual - ISRAnual)/12,(SalBAnual - ISRAnual)/24)
  # 
  # tbSal <- data.frame(ListaDeducciones, DeduccionesSalario)
  # }))
})