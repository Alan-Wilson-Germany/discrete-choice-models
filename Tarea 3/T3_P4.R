###############################################################
#### PASO 0           ####
###############################################################

rm(list=ls())
library(apollo)
apollo_initialise()

# Parámetros y controles
apollo_control = list(
  modelName       = "MNL_teletrabajo_hibrido_LV",
  modelDescr      = "Modelo MNL teletrabajo con LV integradas",
  indivID         = "ID",
  nCores          = 6,            
  outputDirectory = "output"
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
#### PARTE 1: SELECCIÓN DE INDICADORES Y VARIABLES SOCIODEM ####
###############################################################

# 3. Revisa que no hay NA's en los indicadores seleccionados (puedes filtrar casos incompletos si es necesario)
# database_limpia <- database[complete.cases(database[,all_indicadores]), ]
# 
# 
# ###
# database <- database_limpia
# ###


###############################################################
#### PASO 2 - PARTE 2: MNL HÍBRIDO CON VARIABLES LATENTES ####
###############################################################

#################################################################################
#################################################################################

apollo_beta <- c(
  asc_TELE = -0.553221,
  asc_HOME = -0.220754,
  asc_NOTELE = 0.0,
  
  b_fem_HOME = 0.767108,
  b_bus_TELE = 0.579615,
  b_bus_HOME = 0.943886,
  b_metro_TELE = 0.461798,
  
  b_auto2 = -1.863300,
  b_bus2 = -2.220338,
  b_metro2 = -0.811581,
  b_otros2 = -1.070643,
  
  b_votr1auto_TELE = 0.349647,
  b_votr1auto_HOME = 0.799076,
  b_votr1bus_HOME = 0.471752,
  b_votr1otros = 0.977162,
  b_votr2bus = -1.030412,
  b_votr2otros_TELE = -0.934194,
  
  b_ingalto_TELE = 1.238506,
  b_santiago = -0.615805,
  b_hhsize_menor_4_TELE = 0.385473,
  b_preo_ingreso_edu_TELE = 0.256420,
  b_preo_empleo_ingresomed_TELE = 0.160552,
  b_prob_teletrabajo_edad_TELE = 0.035151,
  
  b_preo_empleo_edad_HOME = 0.067677,
  b_preo_ingreso_edu_HOME = 0.157751,
  b_prob_teletrabajo_ingresomed_HOME = -0.055379,
  
  b_LV_eco_TELE = -0.386933,
  b_LV_salud_TELE = 0.105875,
  b_LV_opt_TELE = 0.096177,
  b_LV_eco_HOME = -0.182730,
  b_LV_salud_HOME = -0.023626,
  b_LV_opt_HOME = 0.009615,
  
  gamma_eco_edad = 0.047379,
  gamma_eco_fem = 0.087797,
  gamma_eco_ingmed = -0.475345,
  gamma_eco_ingalt = -0.859854,
  
  gamma_salud_edad = 0.048273,
  gamma_salud_fem = 0.252254,
  gamma_salud_ingmed = -0.128954,
  gamma_salud_ingalt = -0.285954,
  
  gamma_opt_edad = 0.186008,
  gamma_opt_fem = -0.192958,
  gamma_opt_ingmed = 0.092194,
  gamma_opt_ingalt = 0.318103,
  
  
  ##salud
  gamma_yocont = 1, gamma_famcont = 1, gamma_hospi = 1, gamma_muecl = 1,
  tau_yocont_1 = -2, tau_yocont_2 = -1, tau_yocont_3 = 1, tau_yocont_4 = 2,
  tau_famcont_1 = -2, tau_famcont_2 = -1, tau_famcont_3 = 1, tau_famcont_4 = 2,
  tau_hospi_1 = -2, tau_hospi_2 = -1, tau_hospi_3 = 1, tau_hospi_4 = 2,
  tau_muecl_1 = -2, tau_muecl_2 = -1, tau_muecl_3 = 1, tau_muecl_4 = 2,
  
  
  ##econo
  gamma_ingreso = 1, gamma_empleo = 1, gamma_deudas = 1, gamma_econcl = 1,
  tau_ingreso_1 = -2, tau_ingreso_2 = -1, tau_ingreso_3 = 1, tau_ingreso_4 = 2,
  tau_empleo_1 = -2, tau_empleo_2 = -1, tau_empleo_3 = 1, tau_empleo_4 = 2,
  tau_deudas_1 = -2, tau_deudas_2 = -1, tau_deudas_3 = 1, tau_deudas_4 = 2,
  tau_econcl_1 = -2, tau_econcl_2 = -1, tau_econcl_3 = 1, tau_econcl_4 = 2,
  
  
  ##OPT
  gamma_salpub = 1, gamma_planet = 1, gamma_teletr = 1, gamma_segsoc = 1,
  tau_salpub_1 = -2, tau_salpub_2 = -1, tau_salpub_3 = 1, tau_salpub_4 = 2,
  tau_planet_1 = -2, tau_planet_2 = -1, tau_planet_3 = 1, tau_planet_4 = 2,
  tau_teletr_1 = -2, tau_teletr_2 = -1, tau_teletr_3 = 1, tau_teletr_4 = 2,
  tau_segsoc_1 = -2, tau_segsoc_2 = -1, tau_segsoc_3 = 1, tau_segsoc_4 = 2
)

#################################################################################
#################################################################################

apollo_fixed = c("asc_NOTELE")

#################################################################################
#################################################################################

# 2. Draws para LV
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws = 100,
  interNormDraws = c("eta_eco", "eta_salud", "eta_opt")
)

# 3. Función de variables latentes
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV_ECO"]] = gamma_eco_edad*edad_mayor_36 +
    gamma_eco_fem*fem +
    gamma_eco_ingmed*ingreso_medio +
    gamma_eco_ingalt*ingreso_alto +
    eta_eco
  
  randcoeff[["LV_SALUD"]] = gamma_salud_edad*edad_mayor_36 +
    gamma_salud_fem*fem +
    gamma_salud_ingmed*ingreso_medio +
    gamma_salud_ingalt*ingreso_alto +
    eta_salud
  
  randcoeff[["LV_OPT"]] = gamma_opt_edad*edad_mayor_36 +
    gamma_opt_fem*fem +
    gamma_opt_ingmed*ingreso_medio +
    gamma_opt_ingalt*ingreso_alto +
    eta_opt
  return(randcoeff)
}

#################################################################################

#################################################################################

# 4. Validar inputs
apollo_inputs = apollo_validateInputs()

# 5. Definir las utilidades y probabilidades
# apollo_inputs$indicadores_eco = indicadores_eco
# apollo_inputs$indicadores_salud = indicadores_salud
# apollo_inputs$indicadores_opt = indicadores_opt

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Crear lista de probabilidades
  P = list()
  
  ### Definir utilidades con los mismos nombres que las alternativas del modelo
  V = list()
  V[["TELE"]] = asc_TELE +
    b_bus_TELE * VTRA_S1_BUS +
    b_metro_TELE * VTRA_S1_METRO +
    b_auto2 * VTRA_S2_AUTO +
    b_bus2 * VTRA_S2_BUS +
    b_metro2 * VTRA_S2_METRO +
    b_otros2 * VTRA_S2_OTROS +
    b_votr1auto_TELE * VOTR_S1_AUTO +
    b_votr1otros * VOTR_S1_OTROS +
    b_votr2bus * VOTR_S2_BUS +
    b_votr2otros_TELE * VOTR_S2_OTROS +
    b_ingalto_TELE * ingreso_alto +
    b_santiago * santiago +
    b_hhsize_menor_4_TELE * hhsize_menor_4 +
    b_preo_ingreso_edu_TELE * PREO_INGRESO * edu_uni +
    b_preo_empleo_ingresomed_TELE * PREO_EMPLEO * ingreso_medio +
    b_prob_teletrabajo_edad_TELE * PROB_TELETR * edad_mayor_36 +
    b_LV_eco_TELE * LV_ECO +
    b_LV_salud_TELE * LV_SALUD +
    b_LV_opt_TELE * LV_OPT
  
  V[["HOME"]] = asc_HOME +
    b_fem_HOME * fem +
    b_bus_HOME * VTRA_S1_BUS +
    b_auto2 * VTRA_S2_AUTO +
    b_bus2 * VTRA_S2_BUS +
    b_metro2 * VTRA_S2_METRO +
    b_otros2 * VTRA_S2_OTROS +
    b_votr1auto_HOME * VOTR_S1_AUTO +
    b_votr1bus_HOME * VOTR_S1_BUS +
    b_votr1otros * VOTR_S1_OTROS +
    b_votr2bus * VOTR_S2_BUS +
    b_santiago * santiago +
    b_preo_empleo_edad_HOME * PREO_EMPLEO * edad_mayor_36 +
    b_preo_ingreso_edu_HOME * PREO_INGRESO * edu_uni +
    b_prob_teletrabajo_ingresomed_HOME * PROB_TELETR * ingreso_medio +
    b_LV_eco_HOME * LV_ECO +
    b_LV_salud_HOME * LV_SALUD +
    b_LV_opt_HOME * LV_OPT
  
  V[["NOTELE"]] = asc_NOTELE
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(TELE=1, HOME=2, NOTELE=3),
    avail         = 1,
    choiceVar     = TELE_TRA_2,
    V             = V,
    componentName = "modelmnl"
  )
  
#################################################################################
#################################################################################
  
  ### Compute probabilities using MNL model
  P[["modelmnl"]] = apollo_mnl(mnl_settings, functionality)
  
  ol_settings_yocont = list(
    outcomeOrdered = PREO_YOCONT, 
    utility        = gamma_yocont * LV_SALUD, 
    tau            = list(tau_yocont_1, tau_yocont_2, tau_yocont_3, tau_yocont_4),
    componentName  = "OL_PREO_YOCONT_salud"
  )
  P[["OL_PREO_YOCONT_salud"]] = apollo_ol(ol_settings_yocont, functionality)
  
  ol_settings_famcont = list(
    outcomeOrdered = PREO_FAMCONT, 
    utility        = gamma_famcont * LV_SALUD, 
    tau            = list(tau_famcont_1, tau_famcont_2, tau_famcont_3, tau_famcont_4),
    componentName  = "OL_PREO_FAMCONT_salud"
  )
  P[["OL_PREO_FAMCONT_salud"]] = apollo_ol(ol_settings_famcont, functionality)
  
  ol_settings_hospi = list(
    outcomeOrdered = PREO_HOSPI, 
    utility        = gamma_hospi * LV_SALUD, 
    tau            = list(tau_hospi_1, tau_hospi_2, tau_hospi_3, tau_hospi_4),
    componentName  = "OL_PREO_HOSPI_salud"
  )
  P[["OL_PREO_HOSPI_salud"]] = apollo_ol(ol_settings_hospi, functionality)
  
  ol_settings_muecl = list(
    outcomeOrdered = PREO_MUECL, 
    utility        = gamma_muecl * LV_SALUD, 
    tau            = list(tau_muecl_1, tau_muecl_2, tau_muecl_3, tau_muecl_4),
    componentName  = "OL_PREO_MUECL_salud"
  )
  P[["OL_PREO_MUECL_salud"]] = apollo_ol(ol_settings_muecl, functionality)
  
  
#################################################################################
  
  
  
  # Indicador 1: PREO_INGRESO
  ol_settings_ingreso = list(
    outcomeOrdered = PREO_INGRESO, 
    utility        = gamma_ingreso * LV_ECO, 
    tau            = list(tau_ingreso_1, tau_ingreso_2, tau_ingreso_3, tau_ingreso_4),
    componentName  = "OL_PREO_INGRESO_eco"
  )
  P[["OL_PREO_INGRESO_eco"]] = apollo_ol(ol_settings_ingreso, functionality)
  
  
  # Indicador 2: PREO_EMPLEO
  ol_settings_empleo = list(
    outcomeOrdered = PREO_EMPLEO, 
    utility        = gamma_empleo * LV_ECO, 
    tau            = list(tau_empleo_1, tau_empleo_2, tau_empleo_3, tau_empleo_4),
    componentName  = "OL_PREO_EMPLEO_eco"
  )
  P[["OL_PREO_EMPLEO_eco"]] = apollo_ol(ol_settings_empleo, functionality)
  
  # Indicador 3: PREO_DEUDAS
  ol_settings_deudas = list(
    outcomeOrdered = PREO_DEUDAS, 
    utility        = gamma_deudas * LV_ECO, 
    tau            = list(tau_deudas_1, tau_deudas_2, tau_deudas_3, tau_deudas_4),
    componentName  = "OL_PREO_DEUDAS_eco"
  )
  P[["OL_PREO_DEUDAS_eco"]] = apollo_ol(ol_settings_deudas, functionality)
  
  # Indicador 4: PREO_ECONCL
  ol_settings_econcl = list(
    outcomeOrdered = PREO_ECONCL, 
    utility        = gamma_econcl * LV_ECO, 
    tau            = list(tau_econcl_1, tau_econcl_2, tau_econcl_3, tau_econcl_4),
    componentName  = "OL_PREO_ECONCL_eco"
  )
  P[["OL_PREO_ECONCL_eco"]] = apollo_ol(ol_settings_econcl, functionality)
  
  
#################################################################################
  
  
  # Indicador 1: PROB_SALPUB
  ol_settings_salpub = list(
    outcomeOrdered = PROB_SALPUB,
    utility        = gamma_salpub * LV_OPT,
    tau            = list(tau_salpub_1, tau_salpub_2, tau_salpub_3, tau_salpub_4),
    componentName  = "OL_PROB_SALPUB_opt"
  )
  P[["OL_PROB_SALPUB_opt"]] = apollo_ol(ol_settings_salpub, functionality)
  
  # Indicador 2: PROB_PLANET
  ol_settings_planet = list(
    outcomeOrdered = PROB_PLANET,
    utility        = gamma_planet * LV_OPT,
    tau            = list(tau_planet_1, tau_planet_2, tau_planet_3, tau_planet_4),
    componentName  = "OL_PROB_PLANET_opt"
  )
  P[["OL_PROB_PLANET_opt"]] = apollo_ol(ol_settings_planet, functionality)
  
  # Indicador 3: PROB_TELETR
  ol_settings_teletr = list(
    outcomeOrdered = PROB_TELETR,
    utility        = gamma_teletr * LV_OPT,
    tau            = list(tau_teletr_1, tau_teletr_2, tau_teletr_3, tau_teletr_4),
    componentName  = "OL_PROB_TELETR_opt"
  )
  P[["OL_PROB_TELETR_opt"]] = apollo_ol(ol_settings_teletr, functionality)
  
  # Indicador 4: PROB_SEGSOC
  ol_settings_segsoc = list(
    outcomeOrdered = PROB_SEGSOC,
    utility        = gamma_segsoc * LV_OPT,
    tau            = list(tau_segsoc_1, tau_segsoc_2, tau_segsoc_3, tau_segsoc_4),
    componentName  = "OL_PROB_SEGSOC_opt"
  )
  P[["OL_PROB_SEGSOC_opt"]] = apollo_ol(ol_settings_segsoc, functionality)
  

#################################################################################
  

  ### Combined model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}



###############################################################
#### PASO 2 - PARTE 3: ESTIMACIÓN Y OUTPUTS                ####
###############################################################

# Estimar el modelo
modelo_hibrido = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# Mostrar resultados por pantalla
apollo_modelOutput(modelo_hibrido)

# Guardar resultados en carpeta 'output'
apollo_saveOutput(modelo_hibrido)
