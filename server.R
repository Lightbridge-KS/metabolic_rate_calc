# Metabolic rate calculator v2

library(shiny)
library(shinythemes)
library(dplyr)
library(purrr)
library(pins)

# Register   ----------------------------------------------------------------

board_register_github(repo = "Lightbridge-AI/met-calc-heroku-data", 
                      token = "fee1c77140c0c12cb7c70bd385ff066410448f37")

# Functions ---------------------------------------------------------------


get_oxycons_lph <- function(x,y){
  
  (y * 30 * 60)/(x * 1000/25)
  
}

get_stpd <- function(stpd_df, temp_c , baro){
  
  lm_fit <- lm(STPD ~ Temp_c + Baro , data = stpd_df) 
  
  
  if( (temp_c %in% c(15:32)) && (baro %in% c(750:770)) ){
    
    stpd_df %>% 
      filter(Baro == {{baro}} , Temp_c == {{temp_c}}) %>% pull(STPD)
    
  }else{
    
    newpoints <- tibble(Temp_c = temp_c, Baro = baro)
    
    predict(lm_fit, newdata = newpoints) %>% unname()
    
  }
  
}

get_bsa <- function(ht_cm, wt_kg){
  
  0.007184 * (ht_cm^0.725) * (wt_kg^0.425)
  
}

get_bmr <- function(mod_male , mod_female , sex , age){
  
  p <- switch (sex,
               "Male" = c( predict(mod_male, x = age) ),
               "Female" = c( predict(mod_female, x = age) )
  ) 
  
  p %>% .$y  
  
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Individual reactive ------------------------------------------------------------------
  
  stpd_df <- pin_reactive("stpd-df", board = "github")
  
  male_bsa_fit <- pin_reactive("male-fit", board = "github") # Male BSA model
  
  female_bsa_fit <- pin_reactive("female-fit", board = "github")
  
  oxycons_lph_atps <- reactive({
    
    req(input$x , input$y)
    get_oxycons_lph(x = input$x , y = input$y)
    
  })
  
  stpd <- reactive({
    
    req(input$temp, input$baro)
    get_stpd(stpd_df = stpd_df(), temp_c = input$temp , baro = input$baro)
    
  })
  
  oxycons_lph_stpd <- reactive({ oxycons_lph_atps() * stpd() })
  
  met_cal_p_hr <- reactive({ input$cal_eqi * oxycons_lph_stpd() })
  
  bsa <- reactive({
    
    req(input$ht, input$wt)
    get_bsa(ht_cm = input$ht , wt_kg = input$wt) 
    
  })
  
  met_cal_msq_hr <- reactive({ met_cal_p_hr() / bsa() })
  
  bmr <- reactive({
    
    req(input$age, input$sex)
    get_bmr(mod_male = male_bsa_fit(), mod_female = female_bsa_fit(), 
            sex = input$sex, age = input$age)
    
  })
  
  met_percent <- reactive({ ( met_cal_msq_hr()/bmr() )*100 })
  
  
  
  # Render rows ----------------------------------------------------------
  
  # Oxy cons (ATPS) ----------------------------------------------------------
  
  df_1 <- reactive({ 
    
    if(!isTruthy(input$x) || !isTruthy(input$y)){tibble(Parameter = c("Oxygen consumption (ATPS)"),
                                                        Value = c(NA),
                                                        Unit = c("L/hr"))
      
    }else{
      
      tibble(Parameter = c("Oxygen consumption (ATPS)"),
             Value = c( oxycons_lph_atps() ),
             Unit = c("L/hr"))
    }
  })
  
  # STPD correction factor----------------------------------------------------------
  
  df_2 <- reactive({ 
    
    if(!isTruthy(input$temp) || !isTruthy(input$baro)){tibble(Parameter = c("STPD correction factor"),
                                                              Value = c(NA),
                                                              Unit = c(""))
      
    }else{
      
      tibble(Parameter = c("STPD correction factor"),
             Value = c( stpd() ),
             Unit = c(""))
    }
  })
  
  # Oxy cons (STPD) ----------------------------------------------------------
  
  
  is.ready_oxycons_stpd <- reactive({ all(input$x, input$y, input$temp, input$baro) })
  
  
  df_3 <- reactive({ 
    
    if( !isTruthy(is.ready_oxycons_stpd()) ){tibble(Parameter = c("Oxygen consumption (STPD)"),
                                                    Value = c(NA),
                                                    Unit = c("L/hr"))
      
    }else{
      
      tibble(Parameter = c("Oxygen consumption (STPD)"),
             Value = c( oxycons_lph_stpd() ),
             Unit = c("L/hr"))
    }
  })
  
  # Cal equi of O2 ----------------------------------------------------------
  
  df_4 <- reactive({ 
    
    tibble(Parameter = c("Caloric equivalent of Oxygen"),
           Value = c( input$cal_eqi ),
           Unit = c("Cal/L"))
    
  })
  
  # Metabolic rate (cal/hr) ----------------------------------------------------------
  
  df_5 <- reactive({ 
    
    if( !isTruthy(is.ready_oxycons_stpd()) ){tibble(Parameter = c("Metabolic rate"),
                                                    Value = c(NA),
                                                    Unit = c("Cal/hr"))
      
    }else{
      
      tibble(Parameter = c("Metabolic rate"),
             Value = c( met_cal_p_hr() ),
             Unit = c("Cal/hr"))
    }
  })
  
  # BSA ----------------------------------------------------------
  
  df_6 <- reactive({ 
    
    if(!isTruthy(input$ht) || !isTruthy(input$wt)){tibble(Parameter = c("BSA (Du Bois)"),
                                                          Value = c(NA),
                                                          Unit = c("m2"))
      
    }else{
      
      tibble(Parameter = c("BSA (Du Bois)"),
             Value = c( bsa() ),
             Unit = c("m2"))
    }
  })
  
  # Metabolic rate (cal/m2/hr) ----------------------------------------------------------
  
  is.ready_met_rate_c.m2.hr <- reactive({ all(input$x, input$y, input$temp, 
                                              input$baro, input$ht, input$wt) })
  
  df_7 <- reactive({ 
    
    if(!isTruthy(is.ready_met_rate_c.m2.hr() ) ){tibble(Parameter = c("Metabolic rate"),
                                                        Value = c(NA),
                                                        Unit = c("Cal/m2/hr"))
      
    }else{
      
      tibble(Parameter = c("Metabolic rate"),
             Value = c( met_cal_msq_hr() ),
             Unit = c("Cal/m2/hr"))
    }
  })
  
  # Basal Heat production standard (cal/m2/hr) ----------------------------------------------------------
  
  df_8 <- reactive({ 
    
    if(!isTruthy(input$age) || !isTruthy(input$sex)){tibble(Parameter = c("BMR standards"),
                                                            Value = c(NA),
                                                            Unit = c("Cal/m2/hr"))
      
    }else{
      
      tibble(Parameter = c("BMR standards"),
             Value = c( bmr() ),
             Unit = c("Cal/m2/hr"))
    }
  })
  
  # Percent of Metabolic rate (cal/m2/hr) ----------------------------------------------------------
  
  is.ready_met_percent <- reactive({ all(input$x, input$y, input$temp, 
                                         input$baro, input$ht, input$wt, input$age) })
  
  df_9 <- reactive({ 
    
    if(!isTruthy(is.ready_met_percent() ) ){tibble(Parameter = c("Metabolic rate / BMR standards"),
                                                   Value = c(NA),
                                                   Unit = c("%"))
      
    }else{
      
      tibble(Parameter = c("Metabolic rate / BMR standards"),
             Value = c( met_percent() ),
             Unit = c("%"))
    }
  })
  
  # Bind rows & Render table ------------------------------------------------
  
  
  df_all <- reactive({
    
    bind_rows(df_1(), df_2(), df_3(), df_4(), df_5(), df_6(), df_7(), df_8(), df_9() ) %>% 
      modify_if(is.numeric, ~round(.x,digits = 3))
    
  })
  
  output$table <- renderDataTable({
    
    df_all() 
    
  }, options =list(lengthMenu = c(15, 20, 30), pageLength = 15) )
  
  # Download  -----------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("summary_table",".xlsx") #remove .xxx
    },
    content = function(file) {
      
      openxlsx::write.xlsx(df_all(), file)
    }
  )
  
}

