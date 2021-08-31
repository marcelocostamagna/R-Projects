#install.packages("shiny")

library(shiny)
library(plotly)
library(DT)
library(tibble)

# Condiciiones Iniciales ####
tita <- 0.75
tita_d <- 0
max_t <- 40
corte_tita <- 0.05
corte_tita_d <- 0.1


# Funciones Genericas ####
zeta_d <-  function  (c, tit , t_d)  {
    z <-  -2 * sin(tit) + (- sign(c * tit + t_d))
    return(z)
}

RK_calc <-  function  ( h,c, tit , t_d)  {
    z <- h * (-2 * sin(tit) + (- sign(c * tit + t_d)))
    return(z)
}


ult_fila <- function (tib) { 
    aux <- nrow(tib) 
    return(aux)
}



# Define UI ####
ui <- fluidPage(
    
    # Application title
    titlePanel("Simulacion TP 6"),
    
    # Valores de C y h
    fluidRow(
        column(4,
               sliderInput("sl_c1",
                           "Valor de C1 ",
                           min = 1,max = 10,step = 0.05,value = 1),
               sliderInput("sl_h1",
                           "Valor de h1 ",
                           min = 0,max = 3,step = 0.01, value = 0.05)
               
        ),
        column(4,
               sliderInput("sl_c2",
                           "Valor de C2 ",
                           min = 1,max = 10,step = 0.05,value = 1),
               
               sliderInput("sl_h2",
                           "Valor de h2 ",
                           min = 0,max = 3,step = 0.01, value = 0.15)
               
               
               
        ),
        column(4,
               sliderInput("sl_c3",
                           "Valor de C3 ",
                           min = 1,max = 10,step = 0.05,value = 1),
               
               sliderInput("sl_h3",
                           "Valor de h3 ",
                           min = 0,max = 3,step = 0.01, value = 0.25)
               
        )
        
        
    ),
    
    fluidRow(
        #column(3,selectInput("metodo", "Metodo de Aproximacion",choices = c("Seleccione","Euler", "Runge-Kutta"))),
        column(1,actionButton("btn_sim_eu", "Euler")),  
        column(2,actionButton("btn_sim_rk", "Runge-Kutta")),
        column(9,verbatimTextOutput("t_establecimiento"))
    ),
    
    fluidRow(
        
        column(6,
               plotlyOutput("plot_aprox")
        ),
        column(6,
               plotlyOutput("plot_p_f")
        )
        
    ),  
    
    fluidRow(
        DT::dataTableOutput("tb_sim_dt1") 
    ),
    fluidRow(
        DT::dataTableOutput("tb_sim_dt2") 
    ),
    fluidRow(
        DT::dataTableOutput("tb_sim_dt3") 
    )
)

# Server ####
server <- function(input, output) {
    
    
    
    # Reactive botones
    tabla1 <- reactiveValues(data = NULL)
    tabla2 <- reactiveValues(data = NULL)
    tabla3 <- reactiveValues(data = NULL)
    t_estab1 <- reactiveValues(data = NULL)
    t_estab2 <- reactiveValues(data = NULL)
    t_estab3 <- reactiveValues(data = NULL)
    
    ## Boton Simular Euler ####
    observeEvent(input$btn_sim_eu, {
        
        
        
        #EULER  Tabla 1 #### 
        
        c1 <- as.double(input$sl_c1)
        h1 <- as.double( input$sl_h1)
        
        #crear tibble 
        rm(tb_sim_e1)
        n_t <- 0
        tb_sim_e1 <- tibble( Tiempo = 0 ,
                             C = c1,
                             Tita = tita,
                             Tita_Der = tita_d,
                             Zeta_Der = zeta_d(c1,tita,tita_d))
        
        
        while ( n_t < max_t) {
            
            #trae valor de la ultima fila de la tabla
            fila <- as.vector(tb_sim_e1 [ult_fila(tb_sim_e1) , ])
            
            #copia valores y calcula nueva fila
            n_t <- as.double( fila$Tiempo+h1)
            n_tita <-as.double( fila$Tita + h1 * fila$Tita_Der)
            n_tita_d <- as.double(fila$Tita_Der + h1 * fila$Zeta_Der)
            n_zeta_d <-as.double( zeta_d(c1,n_tita,n_tita_d))
            
            #agrega fila nueva a la tabla
            tb_sim_e1 <-  add_row( tb_sim_e1, Tiempo=n_t , C=c1 ,Tita=n_tita, Tita_Der=n_tita_d, Zeta_Der=n_zeta_d )
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        #Graba tiempo de establecimiento
        t_estab_e_1 <- round(n_t,3)
        
        
        #graba reactive values
        tabla1$data <- tb_sim_e1
        t_estab1$data <- t_estab_e_1
        
        
        
        #EULER  Tabla 2 #### 
        
        c2 <- as.double(input$sl_c2)
        h2 <- as.double( input$sl_h2)
        
        #crear tibble 
        rm(tb_sim_e2)
        n_t <- 0
        tb_sim_e2 <- tibble( Tiempo = 0 ,
                             C = c2,
                             Tita = tita,
                             Tita_Der = tita_d,
                             Zeta_Der = zeta_d( c2 ,tita,tita_d))
        
        
        while ( n_t < max_t) {
            
            
            #trae valor de la ultima fila de la tabla
            fila <- as.vector(tb_sim_e2[ult_fila(tb_sim_e2),])
            
            
            #copia valores y calcula nueva fila
            n_t <- as.double( fila$Tiempo + h2)
            n_tita <-as.double( fila$Tita + h2 * fila$Tita_Der)
            n_tita_d <- as.double(fila$Tita_Der + h2 * fila$Zeta_Der)
            n_zeta_d <-as.double( zeta_d(c2,n_tita,n_tita_d))
            
            #agrega fila nueva a la tabla
            tb_sim_e2 <-  add_row( tb_sim_e2, Tiempo=n_t , C=c2 ,Tita=n_tita, Tita_Der=n_tita_d, Zeta_Der=n_zeta_d )
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        
        #Graba tiempo de establecimiento
        t_estab_e_2 <- round(n_t,3)
        
        
        #graba reactive values
        tabla2$data <- tb_sim_e2
        t_estab2$data <- t_estab_e_2
        
        
        #EULER  Tabla 3 #### 
        
        
        c3 <- as.double(input$sl_c3)
        h3 <- as.double( input$sl_h3)
        
        #crear tibble 
        rm(tb_sim_e3)
        n_t <- 0
        tb_sim_e3 <- tibble( Tiempo = 0 ,
                             C = c3,
                             Tita = tita,
                             Tita_Der = tita_d,
                             Zeta_Der = zeta_d( c3 ,tita,tita_d))
        
        
        while ( n_t < max_t) {
            
            
            #trae valor de la ultima fila de la tabla
            fila <- as.vector(tb_sim_e3[ult_fila(tb_sim_e3),])
            
            
            #copia valores y calcula nueva fila
            n_t <- as.double( fila$Tiempo + h3)
            n_tita <-as.double( fila$Tita + h3 * fila$Tita_Der)
            n_tita_d <- as.double(fila$Tita_Der + h3 * fila$Zeta_Der)
            n_zeta_d <-as.double( zeta_d(c3,n_tita,n_tita_d))
            
            #agrega fila nueva a la tabla
            tb_sim_e3 <-  add_row( tb_sim_e3, Tiempo=n_t , C=c3 ,Tita=n_tita, Tita_Der=n_tita_d, Zeta_Der=n_zeta_d )
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        
        #Graba tiempo de establecimiento
        t_estab_e_3 <- round(n_t,3)
        
        #graba reactive values
        tabla3$data <- tb_sim_e3
        t_estab3$data <- t_estab_e_3
        
        
        
        
        
    }) #fin reactive Euler
    
    ## Boton Simular  Runge-Kutta ####
    observeEvent(input$btn_sim_rk, {
        
        metodo <- "Runge-Kutta"
        
        #R-K  Tabla 1 #### 
        
        ### Inicializo variables y creo el la estructura 
        c1 <- as.double(input$sl_c1)
        h1 <- as.double( input$sl_h1)
        
        rm(tb_sim_rk1)
        n_t <- 0
        Tiempo <- 0 
        C <-  c1
        Tita <-  tita
        k11 <-  h1 * tita_d
        k12 <-  h1 * (tita_d + 0.5 * k11 )
        k13 <-  h1 * (tita_d + 0.5 * k12 ) 
        k14 <-  h1 * (tita_d +  k13 ) 
        Tita_Der <-  tita_d
        k21 <-  RK_calc(h1, c1, tita , tita_d)
        k22 <-   RK_calc(h1, c1, (tita + 0.5 * k11) , (tita_d + 0.5 * k21) )
        k23 <-   RK_calc(h1, c1, (tita + 0.5 * k12) , (tita_d + 0.5 * k22) )
        k24 <-   RK_calc(h1, c1, (tita +  k13) , (tita_d + k23))
        
        #crea la tabla
        tb_sim_rk1 <- tibble( Tiempo = Tiempo ,
                              C = C,
                              Tita = Tita,
                              k11 = k11,
                              k12 = k12,
                              k13 = k13 ,
                              k14 = k14 ,
                              Tita_Der = Tita_Der,
                              k21 = k21,
                              k22 =  k22 ,
                              k23 =  k23 ,
                              k24 =  k24 )
        
        ### Calculo de Tabla 
        
        
        while ( (n_t ) < max_t) {
            
            
            #trae valor de la ultima fila de la tabla 
            fila <- as.vector(tb_sim_rk1[ult_fila(tb_sim_rk1),])
            
            #copia valores , incrementa h y  calcula nueva fila
            n_t <-    as.double(fila$Tiempo + h1)
            
            
            n_tita <- round(  as.double( fila$Tita + (fila$k11+ 2*fila$k12 + 2*fila$k13 + fila$k14 )/6 ) , 10)
            n_tita_d <- round( as.double( fila$Tita_Der + ((fila$k21+ 2*fila$k22 + 2*fila$k23 + fila$k24 )/6) )  , 10)
            
            
            n_k11 <- round(as.double( h1 * n_tita_d ) , 10 )
            n_k12 <- round( as.double( h1 * (n_tita_d + 0.5 * n_k11 ))  ,10)
            n_k13 <- round( as.double( h1 * (n_tita_d+ 0.5 * n_k12 )) , 10)
            n_k14 <- round( as.double( h1 * (n_tita_d + n_k13 ))  ,10)
            
            
            n_k21 <-round( as.double( RK_calc(h1,c1 ,n_tita,n_tita_d )) , 10)
            n_k22 <-round( as.double( RK_calc(h1,c1 ,   (n_tita + 0.5 * n_k11)  ,  (n_tita_d + 0.5 * n_k21) )) , 10)
            n_k23 <-round( as.double( RK_calc(h1,c1 ,   (n_tita + 0.5 * n_k12)  ,  (n_tita_d + 0.5 * n_k22) )) ,10)
            n_k24 <-round( as.double( RK_calc(h1,c1 ,   (n_tita +  n_k13)  ,  (n_tita_d + n_k23) ))  ,10)
            
            
            
            tb_sim_rk1 <-  add_row( tb_sim_rk1, Tiempo=n_t , C=c1 ,Tita=n_tita, k11=n_k11, k12=n_k12, k13=n_k13, k14=n_k14, Tita_Der=n_tita_d , k21=n_k21, k22=n_k22, k23=n_k23, k24=n_k24)
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        # Graba tiempo de establecimiento
        t_estab_RK_1 <- round(n_t,3)
        
        
        #graba reactive values
        tabla1$data <- tb_sim_rk1
        t_estab1$data <- t_estab_RK_1
        
        
        #R-K  Tabla 2 #### 
        c2 <- as.double(input$sl_c2)
        h2 <- as.double( input$sl_h2)
        
        ### Inicializo variables y creo el la estructura 
        
        
        rm(tb_sim_rk2)
        n_t <- 0
        Tiempo <- 0 
        C <-  c2
        Tita <-  tita
        k11 <-  h2 * tita_d
        k12 <-  h2 * (tita_d + 0.5 * k11 )
        k13 <-  h2 * (tita_d + 0.5 * k12 ) 
        k14 <-  h2 * (tita_d +  k13 ) 
        Tita_Der <-  tita_d
        k21 <-  RK_calc(h2, c2, tita , tita_d)
        k22 <-   RK_calc(h2, c2, (tita + 0.5 * k11) , (tita_d + 0.5 * k21) )
        k23 <-   RK_calc(h2, c2, (tita + 0.5 * k12) , (tita_d + 0.5 * k22) )
        k24 <-   RK_calc(h2, c2, (tita +  k13) , (tita_d + k23))
        
        
        tb_sim_rk2 <- tibble( Tiempo = Tiempo ,
                              C = C,
                              Tita = Tita,
                              k11 = k11,
                              k12 = k12,
                              k13 = k13 ,
                              k14 = k14 ,
                              Tita_Der = Tita_Der,
                              k21 = k21,
                              k22 =  k22 ,
                              k23 =  k23 ,
                              k24 =  k24 )
        
        ### Calculo de Tabla 
        
        
        while ( (n_t ) < max_t) {
            
            
            #trae valor de la ultima fila de la tabla 
            fila <- as.vector(tb_sim_rk2[ult_fila(tb_sim_rk2),])
            
            #copia valores , incrementa h y  calcula nueva fila
            n_t <-    as.double(fila$Tiempo + h2)
            
            
            n_tita <- round(  as.double( fila$Tita + (fila$k11+ 2*fila$k12 + 2*fila$k13 + fila$k14 )/6 ) , 10)
            n_tita_d <- round( as.double( fila$Tita_Der + ((fila$k21+ 2*fila$k22 + 2*fila$k23 + fila$k24 )/6) )  , 10)
            
            
            n_k11 <- round(as.double( h2 * n_tita_d ) , 10 )
            n_k12 <- round( as.double( h2 * (n_tita_d + 0.5 * n_k11 ))  ,10)
            n_k13 <- round( as.double( h2 * (n_tita_d+ 0.5 * n_k12 )) , 10)
            n_k14 <- round( as.double( h2 * (n_tita_d + n_k13 ))  ,10)
            
            
            n_k21 <-round( as.double( RK_calc(h2,c2 ,n_tita,n_tita_d )) , 10)
            n_k22 <-round( as.double( RK_calc(h2,c2 ,   (n_tita + 0.5 * n_k11)  ,  (n_tita_d + 0.5 * n_k21) )) , 10)
            n_k23 <-round( as.double( RK_calc(h2,c2 ,   (n_tita + 0.5 * n_k12)  ,  (n_tita_d + 0.5 * n_k22) )) ,10)
            n_k24 <-round( as.double( RK_calc(h2,c2 ,   (n_tita +  n_k13)  ,  (n_tita_d + n_k23) ))  ,10)
            
            
            
            tb_sim_rk2 <-  add_row( tb_sim_rk2, Tiempo=n_t , C=c2 ,Tita=n_tita, k11=n_k11, k12=n_k12, k13=n_k13, k14=n_k14, Tita_Der=n_tita_d , k21=n_k21, k22=n_k22, k23=n_k23, k24=n_k24)
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        # Graba tiempo de establecimiento
        t_estab_RK_2 <- n_t
        
        
        #graba reactive values
        tabla2$data <- tb_sim_rk2
        t_estab2$data <- t_estab_RK_2
        
        
        #R-K  Tabla 3 #### 
        c3 <- as.double(input$sl_c3)
        h3 <- as.double( input$sl_h3)
        
        
        
        rm(tb_sim_rk3)
        n_t <- 0
        Tiempo <- 0 
        C <-  c3
        Tita <-  tita
        k11 <-  h3 * tita_d
        k12 <-  h3 * (tita_d + 0.5 * k11 )
        k13 <-  h3 * (tita_d + 0.5 * k12 ) 
        k14 <-  h3 * (tita_d +  k13 ) 
        Tita_Der <-  tita_d
        k21 <-  RK_calc(h3, c3, tita , tita_d)
        k22 <-   RK_calc(h3, c3, (tita + 0.5 * k11) , (tita_d + 0.5 * k21) )
        k23 <-   RK_calc(h3, c3, (tita + 0.5 * k12) , (tita_d + 0.5 * k22) )
        k24 <-   RK_calc(h3, c3, (tita +  k13) , (tita_d + k23))
        
        
        tb_sim_rk3 <- tibble( Tiempo = Tiempo ,
                              C = C,
                              Tita = Tita,
                              k11 = k11,
                              k12 = k12,
                              k13 = k13 ,
                              k14 = k14 ,
                              Tita_Der = Tita_Der,
                              k21 = k21,
                              k22 =  k22 ,
                              k23 =  k23 ,
                              k24 =  k24 )
        
        ### Calculo de Tabla 
        
        
        while ( (n_t ) < max_t) {
            
            
            #trae valor de la ultima fila de la tabla 
            fila <- as.vector(tb_sim_rk3[ult_fila(tb_sim_rk3),])
            
            #copia valores , incrementa h y  calcula nueva fila
            n_t <-    as.double(fila$Tiempo + h3)
            
            
            n_tita <- round(  as.double( fila$Tita + (fila$k11+ 2*fila$k12 + 2*fila$k13 + fila$k14 )/6 ) , 10)
            n_tita_d <- round( as.double( fila$Tita_Der + ((fila$k21+ 2*fila$k22 + 2*fila$k23 + fila$k24 )/6) )  , 10)
            
            
            n_k11 <- round(as.double( h3 * n_tita_d ) , 10 )
            n_k12 <- round( as.double( h3 * (n_tita_d + 0.5 * n_k11 ))  ,10)
            n_k13 <- round( as.double( h3 * (n_tita_d+ 0.5 * n_k12 )) , 10)
            n_k14 <- round( as.double( h3 * (n_tita_d + n_k13 ))  ,10)
            
            
            n_k21 <-round( as.double( RK_calc(h3,c3 ,n_tita,n_tita_d )) , 10)
            n_k22 <-round( as.double( RK_calc(h3,c3 ,   (n_tita + 0.5 * n_k11)  ,  (n_tita_d + 0.5 * n_k21) )) , 10)
            n_k23 <-round( as.double( RK_calc(h3,c3 ,   (n_tita + 0.5 * n_k12)  ,  (n_tita_d + 0.5 * n_k22) )) ,10)
            n_k24 <-round( as.double( RK_calc(h3,c3 ,   (n_tita +  n_k13)  ,  (n_tita_d + n_k23) ))  ,10)
            
            
            
            tb_sim_rk3 <-  add_row( tb_sim_rk3, Tiempo=n_t , C=c3 ,Tita=n_tita, k11=n_k11, k12=n_k12, k13=n_k13, k14=n_k14, Tita_Der=n_tita_d , k21=n_k21, k22=n_k22, k23=n_k23, k24=n_k24)
            
            #condicion de corte por equilibrio
            if(abs(n_tita)<corte_tita && abs(n_tita_d)<corte_tita_d)  break
            
            next
            
        }
        
        # Graba tiempo de establecimiento
        t_estab_RK_3 <- round(n_t,3)
        
        
        #graba reactive values
        tabla3$data <- tb_sim_rk3
        t_estab3$data <- t_estab_RK_3
        
    })
    
    
    ## CARGAR TABLAS PARA MSOTRAR ####
    output$tb_sim_dt1 <- DT::renderDataTable({DT::datatable({tabla1$data}
                                                            ,caption = 'Tabla 1'
                                                            , options = list(searching  = FALSE)
                                                            ,selection = 'none' 
                                                            
    )})
    
    output$tb_sim_dt2 <- DT::renderDataTable({DT::datatable({tabla2$data}
                                                            ,caption = 'Tabla 2'
                                                            , options = list(searching  = FALSE)
                                                            ,selection = 'none' 
                                                            
    )})
    
    output$tb_sim_dt3 <- DT::renderDataTable({DT::datatable({tabla3$data}
                                                            ,caption = 'Tabla 3'
                                                            , options = list(searching  = FALSE)
                                                            ,selection = 'none' 
                                                            
    )})
    
    
    ## Render graficos y tabla ####
    output$plot_aprox <- renderPlotly({
        if (is.null(tabla1$data)) return()
        
        ggplot(tabla1$data, aes(x=Tiempo, y= Tita)) + 
            geom_point(aes(color="Tabla 1")) +
            geom_point(data=tabla2$data, aes(color="Tabla 2")) +
            geom_point(data=tabla3$data, aes(color="Tabla 3")) +
            labs(  color="Referencia" ,title="Aproximacion ")
        
    })
    
    output$plot_p_f <- renderPlotly ({
        if (is.null(tabla1$data)) return()
        
        ggplot(tabla1$data, aes(x=Tita, y= Tita_Der)) + 
            geom_point(aes(color="Tabla 1")) +
            geom_point(data=tabla2$data, aes(color="Tabla 2")) +
            geom_point(data=tabla3$data, aes(color="Tabla 3")) +
            labs(color="Referencia" , title="Plano Fase")
        
    })
    
    
    output$t_establecimiento  <- renderText({
        
        paste(paste ("Condiciones Iniciales:","Tita=", tita,"Tita Der=", tita_d ,"Corte Tita=", corte_tita,"Corte Tita Der=", corte_tita_d ),
              paste ("Tiempos de Establecimiento", "(max =", max_t, "segundos)"  ),
              paste ("T1:", t_estab1$data, "segundos"),
              paste ("T2:",  t_estab2$data, "segundos"),
              paste ("T3:",  t_estab3$data, "segundos"),
              sep="\n"
        )
        
        
    })  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)











