###############################################################
#### 1. LIMPIAR Y CARGAR LIBRERÍAS Y DATOS                #####
###############################################################

rm(list = ls())
library(apollo)

apollo_initialise()

apollo_control = list(
  modelName       = "MXL_teletrabajo_final",
  modelDescr      = "Modelo Mixed Logit con parámetros aleatorios",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 6
)

# Leer datos
database = read.csv("C:\\Users\\alanw\\Downloads\\UCBM\\Covid-19_survey.csv", header=TRUE, na.strings=c(""," ","NA"), sep=",", dec='.')

# Filtrar alternativas válidas
database = subset(database, database$TELE_TRA != 6)
database = subset(database, database$GEN %in% c(1,2))  # Solo masculino y femenino

# **Modificación de la variable EDAD (Menores de 36 y Mayores de 36)**
database$edad_menor_36 = ifelse(database$EDAD %in% c(1, 2), 1, 0)  # Menores de 36 (referencia)
database$edad_mayor_36 = ifelse(database$EDAD %in% c(3, 4, 5), 1, 0)  # Mayores o iguales a 36 (dummy)

# **Modificación de la variable INGRESO (Ingreso Bajo, Medio, Alto)**
database$ingreso_bajo = ifelse(database$INGRESO %in% c(1, 2), 1, 0)  # Ingreso bajo (referencia)
database$ingreso_medio = ifelse(database$INGRESO %in% c(3, 4, 5), 1, 0)  # Ingreso medio (dummy)
database$ingreso_alto = ifelse(database$INGRESO %in% c(6, 7), 1, 0)  # Ingreso alto (dummy)

# **Modificación de la variable EDU_UNI (Con estudios posteriores o no)**
database$edu_uni = ifelse(database$NIVEL_EDUC %in% c(3, 4), 1, 0)  # Con estudios universitarios

# **Modificación de la variable HHSize (menos de 6 personas vs 6 o más personas)**
database$hhsize_menor_4 = ifelse(database$HHSIZE < 4, 1, 0)  # Menos de 6 personas en el hogar (dummy)
# La categoría de referencia será 6 o más personas en el hogar, por lo que no es necesario agregarlo explícitamente.

# **Verificar que TELE_TRA_2 tenga las tres categorías correctas (1, 2, 3)**
table(database$TELE_TRA)  # Verifica la distribución de TELE_TRA

# Creación de la variable dependiente numérica (TELE_TRA_2)
database$TELE_TRA_2 = ifelse(database$TELE_TRA == 1, 1,
                             ifelse(database$TELE_TRA %in% c(2, 3), 2, 3))

# Verifica la distribución de TELE_TRA_2
table(database$TELE_TRA_2)

# Dummies sociodemográficas
database$fem = ifelse(database$GEN == 2, 1, 0)
database$santiago = ifelse(database$CIUDAD == 83, 1, 0)
database$mayores = ifelse(database$SITU_HOGAR_5 == 1, 1, 0)

# VTRA semana 1
database$VTRA_S1_BUS   <- ifelse(database$VTRA_S1_BUS %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_METRO <- ifelse(database$VTRA_S1_METRO %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_AUTO  <- ifelse(database$VTRA_S1_AUTO %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_TAXI  <- ifelse(database$VTRA_S1_TAXI %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_COL   <- ifelse(database$VTRA_S1_COL %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_BIC   <- ifelse(database$VTRA_S1_BIC %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S1_MOTO  <- ifelse(database$VTRA_S1_MOTO %in% c(5, 8.5, 12), 1, 0)

# VTRA semana 2
database$VTRA_S2_BUS   <- ifelse(database$VTRA_S2_BUS %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_METRO <- ifelse(database$VTRA_S2_METRO %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_AUTO  <- ifelse(database$VTRA_S2_AUTO %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_TAXI  <- ifelse(database$VTRA_S2_TAXI %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_COL   <- ifelse(database$VTRA_S2_COL %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_BIC   <- ifelse(database$VTRA_S2_BIC %in% c(5, 8.5, 12), 1, 0)
database$VTRA_S2_MOTO  <- ifelse(database$VTRA_S2_MOTO %in% c(5, 8.5, 12), 1, 0)

# VOTR semana 1
database$VOTR_S1_BUS   <- ifelse(database$VOTR_S1_BUS %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_METRO <- ifelse(database$VOTR_S1_METRO %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_AUTO  <- ifelse(database$VOTR_S1_AUTO %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_TAXI  <- ifelse(database$VOTR_S1_TAXI %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_COL   <- ifelse(database$VOTR_S1_COL %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_BIC   <- ifelse(database$VOTR_S1_BIC %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S1_MOTO  <- ifelse(database$VOTR_S1_MOTO %in% c(5, 8.5, 12), 1, 0)

# VOTR semana 2
database$VOTR_S2_BUS   <- ifelse(database$VOTR_S2_BUS %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_METRO <- ifelse(database$VOTR_S2_METRO %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_AUTO  <- ifelse(database$VOTR_S2_AUTO %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_TAXI  <- ifelse(database$VOTR_S2_TAXI %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_COL   <- ifelse(database$VOTR_S2_COL %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_BIC   <- ifelse(database$VOTR_S2_BIC %in% c(5, 8.5, 12), 1, 0)
database$VOTR_S2_MOTO  <- ifelse(database$VOTR_S2_MOTO %in% c(5, 8.5, 12), 1, 0)



# Agrupar “otros” modos para VTRA y VOTR, semanas 1 y 2
database$VTRA_S1_OTROS = rowSums(database[,c("VTRA_S1_TAXI","VTRA_S1_COL","VTRA_S1_BIC","VTRA_S1_MOTO")], na.rm=TRUE)
database$VTRA_S2_OTROS = rowSums(database[,c("VTRA_S2_TAXI","VTRA_S2_COL","VTRA_S2_BIC","VTRA_S2_MOTO")], na.rm=TRUE)
database$VOTR_S1_OTROS = rowSums(database[,c("VOTR_S1_TAXI","VOTR_S1_COL","VOTR_S1_BIC","VOTR_S1_MOTO")], na.rm=TRUE)
database$VOTR_S2_OTROS = rowSums(database[,c("VOTR_S2_TAXI","VOTR_S2_COL","VOTR_S2_BIC","VOTR_S2_MOTO")], na.rm=TRUE)


###############################################################
#### 2. DEFINIR PARÁMETROS DEL MODELO                    #####
###############################################################

apollo_beta = c(
  asc_TELE = 0,
  asc_HOME = 0,
  asc_NOTELE = 0,
  b_fem_HOME = 0,
  
  b_bus_TELE = 0,
  b_bus_HOME = 0,
  # b_metro_HOME = 0,
  b_metro_TELE = 0,
  
  # b_auto2 = 0,
  # b_bus2 = 0,
  b_metro2 = 0,
  b_otros2= 0,
  
  b_votr1auto_TELE = 0,
  b_votr1auto_HOME = 0,
  b_votr1bus_HOME = 0,
  b_votr1otros = 0,
  
  b_votr2bus = 0,
  b_votr2otros_TELE = 0,
  
  # b_edu_uni_TELE = 0,
  # b_edu_uni_HOME = 0,
  b_ingalto_TELE = 0,
  # b_ingmedio_TELE = 0,
  
  b_santiago = 0,
  b_hhsize_menor_4_TELE = 0,
  
  # b_edad_mayor_36_HOME = 0,  # Mayores o iguales a 36 en trabajo en casa (dummy)
  
  # Interacciones con coeficientes específicos para TELE y HOME
  # b_preo_contagio_edad_TELE = 0,  # Preocupación por contagio x Edad en TELE
  # b_preo_empleo_edad_TELE = 0,    # Preocupación por empleo x Edad en TELE
  # b_prob_teletrabajo_edad_TELE = 0,  # Probabilidad de teletrabajo x Edad en TELE
  
  b_preo_ingreso_edu_TELE = 0,    # Preocupación por ingresos x Educación en TELE
  b_preo_empleo_ingresomed_TELE = 0, # Preocupación por empleo x Ingreso en TELE
  # b_prob_teletrabajo_ingreso_TELE = 0,  # Probabilidad de teletrabajo x Ingreso en TELE
  # 
  # b_preo_salud_edu_TELE = 0,     # Preocupación por salud x Educación en TELE
  # b_preo_economia_edu_TELE = 0,  # Preocupación por economía x Educación en TELE
  b_prob_teletrabajo_edad_TELE = 0, # Probabilidad de teletrabajo x Educación en TELE
  
  # Interacciones para HOME
  # b_preo_contagio_edad_HOME = 0,  # Preocupación por contagio x Edad en HOME
  b_preo_empleo_edad_HOME = 0,    # Preocupación por empleo x Edad en HOME
  # b_prob_teletrabajo_edad_HOME = 0,  # Probabilidad de teletrabajo x Edad en HOME
  
  b_preo_ingreso_edu_HOME = 0,    # Preocupación por ingresos x Educación en HOME
  # b_preo_empleo_ingreso_HOME = 0, # Preocupación por empleo x Ingreso en HOME
  # b_prob_teletrabajo_ingreso_HOME = 0,  # Probabilidad de teletrabajo x Ingreso en HOME
  
  # b_preo_salud_edu_HOME = 0,     # Preocupación por salud x Educación en HOME
  # b_preo_economia_edu_HOME = 0,  # Preocupación por economía x Educación en HOME
  b_prob_teletrabajo_ingresomed_HOME = 0, # Probabilidad de teletrabajo x Educación en HOME
  
  # Parámetros aleatorios
  mu_auto2 = 1,
  # mu_ingalto_TELE = 0,
  # mu_hhsize_menor_4_TELE = 0,
  # mu_edad_mayor_36_HOME = 0,
  mu_bus2 = 1,
  # mu_ingalto_TELE = 0,
  
  sigma_auto2 = 0.9,
  # sigma_ingalto_TELE = 0.1
  # sigma_hhsize_menor_4_TELE = 0.1,
  # sigma_edad_mayor_36_HOME = 0.1,
  sigma_bus2 = 0.9
  # sigma_ingalto_TELE = 0.1  
)

# --- Draws ---
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c("draw1", "draw2")
)

# --- Coeficientes aleatorios ---
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_auto2"]]   = mu_auto2 + sigma_auto2 * draw1
  randcoeff[["b_bus2"]]    = mu_bus2 + sigma_bus2 * draw2

  # randcoeff[["b_edad_mayor_36_HOME"]] = mu_edad_mayor_36_HOME + sigma_edad_mayor_36_HOME * draw4
  # randcoeff[["b_ingalto_TELE"]] = mu_ingalto_TELE + sigma_ingalto_TELE * draw3
  # randcoeff[["b_hhsize_menor_4_TELE"]] = mu_hhsize_menor_4_TELE + sigma_hhsize_menor_4_TELE * draw3
  # randcoeff[["b_ingalto_TELE"]] = mu_ingalto_TELE + sigma_ingalto_TELE * draw2
  return(randcoeff)
}
apollo_fixed = c("asc_NOTELE")

###############################################################
#### 3. VALIDAR INPUTS                                     ####
###############################################################

apollo_inputs = apollo_validateInputs()

###############################################################
#### 4. DEFINIR LAS UTILIDADES Y PROBABILIDADES           ####
###############################################################

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  V = list()
  
  ###################################################################################
  V[["TELE"]] = asc_TELE +
    # VTRA semana 1
    b_bus_TELE * VTRA_S1_BUS +
    # b_auto_TELE * VTRA_S1_AUTO +
    b_metro_TELE * VTRA_S1_METRO +
    # VTRA semana 2
    b_auto2 * VTRA_S2_AUTO +
    b_bus2 * VTRA_S2_BUS +
    b_metro2 * VTRA_S2_METRO +
    b_otros2 * VTRA_S2_OTROS +
    
    # VOTR semana 1
    b_votr1auto_TELE * VOTR_S1_AUTO +
    b_votr1otros * VOTR_S1_OTROS +
    
    # VOTR semana 2
    b_votr2bus * VOTR_S2_BUS +
    b_votr2otros_TELE * VOTR_S2_OTROS +
    
    # Socioeconómicas
    # b_edu_uni_TELE * edu_uni +
    b_ingalto_TELE * ingreso_alto +
    # b_ingmedio_TELE * ingreso_medio +
    b_santiago * santiago +
    b_hhsize_menor_4_TELE * hhsize_menor_4 +
    
    # Interacciones (solo para TELE)
    # b_preo_contagio_edad_TELE * PREO_YOCONT * edad_mayor_36 +  # Preocupación por contagio x Edad en TELE
    # b_preo_empleo_edad_TELE * PREO_EMPLEO * edad_mayor_36 +    # Preocupación por empleo x Edad en TELE
    # b_prob_teletrabajo_edad_TELE * PROB_TELETR * edad_mayor_36 +  # Probabilidad de teletrabajo x Edad en TELE
    b_preo_ingreso_edu_TELE * PREO_INGRESO * edu_uni +    # Preocupación por ingresos x Educación en TELE
    b_preo_empleo_ingresomed_TELE * PREO_EMPLEO * ingreso_medio +  # Preocupación por empleo x Ingreso en TELE
    # b_prob_teletrabajo_ingreso_TELE * PROB_TELETR * ingreso_medio +  # Probabilidad de teletrabajo x Ingreso en TELE
    # b_preo_salud_edu_TELE * PREO_SALMEN * edu_uni +     # Preocupación por salud x Educación en TELE
    # b_preo_economia_edu_TELE * PREO_ECONCL * edu_uni +  # Preocupación por economía x Educación en TELE
    b_prob_teletrabajo_edad_TELE * PROB_TELETR * edad_mayor_36  # Probabilidad de teletrabajo x Educación en TELE
  
  V[["HOME"]] = asc_HOME +
    b_fem_HOME * fem +
    # VTRA semana 1
    b_bus_HOME * VTRA_S1_BUS +
    # b_metro_HOME * VTRA_S1_METRO +
    # VTRA semana 2
    b_auto2 * VTRA_S2_AUTO +
    b_bus2 * VTRA_S2_BUS +
    b_metro2 * VTRA_S2_METRO +
    b_otros2 * VTRA_S2_OTROS +
    
    # VOTR semana 1
    b_votr1auto_HOME * VOTR_S1_AUTO +
    b_votr1bus_HOME * VOTR_S1_BUS +
    b_votr1otros * VOTR_S1_OTROS +
    
    # VOTR semana 2
    b_votr2bus * VOTR_S2_BUS +
    
    # Socioeconómicas
    # b_edu_uni_HOME * edu_uni +
    b_santiago * santiago +
    # b_edad_mayor_36_HOME * edad_mayor_36 +  # Correctamente incluida solo una vez
    
    # Interacciones (solo para HOME)
    b_preo_empleo_edad_HOME * PREO_EMPLEO * edad_mayor_36 +    # Preocupación por empleo x Edad en HOME
    b_preo_ingreso_edu_HOME * PREO_INGRESO * edu_uni +    # Preocupación por ingresos x Educación en HOME
    b_prob_teletrabajo_ingresomed_HOME * PROB_TELETR * ingreso_medio  # Probabilidad de teletrabajo x Educación en HOME
  
  
  
  V[["NOTELE"]] = asc_NOTELE
  
  mnl_settings = list(
    alternatives  = c(TELE=1, HOME=2, NOTELE=3),
    avail         = 1,
    choiceVar     = TELE_TRA_2,
    utilities     = V
  )
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

###############################################################
#### 5. ESTIMACIÓN DEL MODELO                             ####
###############################################################

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

###############################################################
#### 6. OUTPUTS DEL MODELO                                ####
###############################################################

apollo_modelOutput(model)

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

str(predictions_base)

#################################################################################################################
#################################################################################################################

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

sink("OUTPUT T3-P2.txt")
apollo_modelOutput(model)           
sink()   