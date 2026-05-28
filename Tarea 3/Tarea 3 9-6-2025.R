###############################################################
#### 1. LIMPIAR Y CARGAR LIBRERÍAS Y DATOS                #####
###############################################################

rm(list = ls())
library(apollo)
apollo_initialise()

apollo_control = list(
  modelName       = "MNL_teletrabajo_full_factores",
  modelDescr      = "Modelo MNL teletrabajo con factores de preocupación y modos de transporte",
  indivID         = "ID", 
  outputDirectory = "output"
)

# Leer datos
database = read.csv("C:\\Users\\alanw\\Downloads\\UCBM\\Covid-19_survey.csv", header=TRUE, na.strings=c(""," ","NA"), sep=",", dec='.')

# Filtrar alternativas válidas
database = subset(database, database$TELE_TRA != 6)
database = subset(database, database$GEN %in% c(1,2))  # Solo masculino y femenino

# Variable dependiente numérica
database$TELE_TRA_2 = ifelse(database$TELE_TRA == 1, 1,
                             ifelse(database$TELE_TRA %in% c(2, 3), 2, 3))
database$TELE_TRA_2 = as.numeric(database$TELE_TRA_2)

# Dummies sociodemográficas
database$fem = ifelse(database$GEN == 2, 1, 0)
database$edu_uni = ifelse(database$NIVEL_EDUC %in% c(3,4), 1, 0)
database$ing_alto = ifelse(database$INGRESO %in% c(6,7), 1, 0)
database$ingr_medio = ifelse(database$INGRESO %in% c(3,4,5), 1, 0)
database$santiago = ifelse(database$CIUDAD == 83, 1, 0)
database$mayores = ifelse(database$SITU_HOGAR_5 == 1, 1, 0)
database$hhsize = database$HHSIZE

# Agrupar “otros” modos para VTRA y VOTR, semanas 1 y 2
database$VTRA_S1_OTROS = rowSums(database[,c("VTRA_S1_TAXI","VTRA_S1_COL","VTRA_S1_BIC","VTRA_S1_MOTO")], na.rm=TRUE)
database$VTRA_S2_OTROS = rowSums(database[,c("VTRA_S2_TAXI","VTRA_S2_COL","VTRA_S2_BIC","VTRA_S2_MOTO")], na.rm=TRUE)
database$VOTR_S1_OTROS = rowSums(database[,c("VOTR_S1_TAXI","VOTR_S1_COL","VOTR_S1_BIC","VOTR_S1_MOTO")], na.rm=TRUE)
database$VOTR_S2_OTROS = rowSums(database[,c("VOTR_S2_TAXI","VOTR_S2_COL","VOTR_S2_BIC","VOTR_S2_MOTO")], na.rm=TRUE)

# Asegúrate de que las variables de modo existen
for(v in c("VTRA_S1_AUTO","VTRA_S2_AUTO","VTRA_S1_BUS","VTRA_S2_BUS","VTRA_S1_METRO","VTRA_S2_METRO",
           "VOTR_S1_AUTO","VOTR_S2_AUTO","VOTR_S1_BUS","VOTR_S2_BUS","VOTR_S1_METRO","VOTR_S2_METRO")) {
  if(!v %in% names(database)) database[[v]] <- 0
}

# ====== NUEVO: Crea factores de preocupación según la literatura ======
# (Puedes modificar las agrupaciones según prefieras)

# 1. Preocupación salud
salud_vars = c("PREO_YOCONT","PREO_FAMCONT","PREO_HOSPI","PREO_SALMEN","PREO_MUECL","PREO_MUEMU")
database$factor_salud <- rowMeans(database[,salud_vars], na.rm=FALSE)

# 2. Preocupación económica
eco_vars = c("PREO_INGRESO","PREO_EMPLEO","PREO_DEUDAS","PREO_ECONCL","PREO_ECONMU","PREO_INSUM")
database$factor_economico <- rowMeans(database[,eco_vars], na.rm=FALSE)

# 3. Preocupación social
social_vars = c("PREO_LIB","PREO_TPUB","PREO_FAKE")
database$factor_social <- rowMeans(database[,social_vars], na.rm=FALSE)

###############################################################
#### 2. DEFINIR PARÁMETROS DEL MODELO                    #####
###############################################################

apollo_beta = c(
  asc_TELE = 0,
  asc_HOME = 0,
  asc_NOTELE = 0,
  b_fem_TELE = 0,
  b_fem_HOME = 0,
  # b_auto_TELE = 0,
  # b_auto_HOME = 0,
  # b_bus_TELE = 0,
  # b_bus_HOME = 0,
  # b_metro_TELE = 0,
  # b_metro_HOME = 0,
  # b_otros_TELE = 0,
  # b_otros_HOME = 0,
  # b_auto2_TELE = 0,
  # b_auto2_HOME = 0,
  # b_bus2_TELE = 0,
  # b_bus2_HOME = 0,
  # b_metro2_TELE = 0,
  # b_metro2_HOME = 0,
  # b_otros2_TELE = 0,
  # b_otros2_HOME = 0,
  # b_votr1auto_TELE = 0,
  # b_votr1auto_HOME = 0,
  # b_votr1bus_TELE = 0,
  # b_votr1bus_HOME = 0,
  # b_votr1metro_TELE = 0,
  # b_votr1metro_HOME = 0,
  # b_votr1otros_TELE = 0,
  # b_votr1otros_HOME = 0,
  # b_votr2auto_TELE = 0,
  # b_votr2auto_HOME = 0,
  # b_votr2bus_TELE = 0,
  # b_votr2bus_HOME = 0,
  # b_votr2metro_TELE = 0,
  # b_votr2metro_HOME = 0,
  # b_votr2otros_TELE = 0,
  # b_votr2otros_HOME = 0,
  # b_preofam_TELE = 0,
  b_eduuni_TELE = 0,
  b_eduuni_HOME = 0,
  b_ingalto_TELE = 0,
  b_ingalto_HOME = 0,
  b_ingrmedio_TELE = 0,
  b_santiago_TELE = 0,
  b_santiago_HOME = 0,
  b_mayores_TELE = 0,
  b_mayores_HOME = 0,
  b_hhsize_TELE = 0,
  # Factores de preocupación
  b_salud_TELE = 0,
  b_salud_HOME = 0,
  b_eco_TELE = 0,
  b_eco_HOME = 0,
  b_social_TELE = 0,
  b_social_HOME = 0
)

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
  V[["TELE"]] = asc_TELE +
    b_fem_TELE * fem +
    # # VTRA semana 1
    # b_auto_TELE * VTRA_S1_AUTO +
    # b_bus_TELE * VTRA_S1_BUS +
    # b_metro_TELE * VTRA_S1_METRO +
    # b_otros_TELE * VTRA_S1_OTROS +
    # # VTRA semana 2
    # b_auto2_TELE * VTRA_S2_AUTO +
    # b_bus2_TELE * VTRA_S2_BUS +
    # b_metro2_TELE * VTRA_S2_METRO +
    # b_otros2_TELE * VTRA_S2_OTROS +
    # # VOTR semana 1
    # b_votr1auto_TELE * VOTR_S1_AUTO +
    # b_votr1bus_TELE * VOTR_S1_BUS +
    # b_votr1metro_TELE * VOTR_S1_METRO +
    # b_votr1otros_TELE * VOTR_S1_OTROS +
    # # VOTR semana 2
    # b_votr2auto_TELE * VOTR_S2_AUTO +
    # b_votr2bus_TELE * VOTR_S2_BUS +
    # b_votr2metro_TELE * VOTR_S2_METRO +
    # b_votr2otros_TELE * VOTR_S2_OTROS +
    # Socioeconómicas
    # b_preofam_TELE * PREO_FAMCONT +
    b_eduuni_TELE * edu_uni +
    b_ingalto_TELE * ing_alto +
    b_ingrmedio_TELE * ingr_medio +
    b_santiago_TELE * santiago +
    b_mayores_TELE * mayores +
    b_hhsize_TELE * hhsize +
    # Factores de preocupación
    b_salud_TELE * factor_salud +
    b_eco_TELE * factor_economico +
    b_social_TELE * factor_social
  
  V[["HOME"]] = asc_HOME +
    b_fem_HOME * fem +
    # VTRA semana 1
    # b_auto_HOME * VTRA_S1_AUTO +
    # b_bus_HOME * VTRA_S1_BUS +
    # b_metro_HOME * VTRA_S1_METRO +
    # b_otros_HOME * VTRA_S1_OTROS +
    # # VTRA semana 2
    # b_auto2_HOME * VTRA_S2_AUTO +
    # b_bus2_HOME * VTRA_S2_BUS +
    # b_metro2_HOME * VTRA_S2_METRO +
    # b_otros2_HOME * VTRA_S2_OTROS +
    # # VOTR semana 1
    # b_votr1auto_HOME * VOTR_S1_AUTO +
    # b_votr1bus_HOME * VOTR_S1_BUS +
    # b_votr1metro_HOME * VOTR_S1_METRO +
    # b_votr1otros_HOME * VOTR_S1_OTROS +
    # # VOTR semana 2
    # b_votr2auto_HOME * VOTR_S2_AUTO +
    # b_votr2bus_HOME * VOTR_S2_BUS +
    # b_votr2metro_HOME * VOTR_S2_METRO +
    # b_votr2otros_HOME * VOTR_S2_OTROS +
    # Socioeconómicas
    b_eduuni_HOME * edu_uni +
    
    b_ingalto_HOME * ing_alto +
    b_santiago_HOME * santiago +
    b_mayores_HOME * mayores +
    
    # Factores de preocupación
    b_salud_HOME * factor_salud +
    b_eco_HOME * factor_economico +
    b_social_HOME * factor_social
  
  V[["NOTELE"]] = asc_NOTELE
  
  mnl_settings = list(
    alternatives  = c(TELE=1, HOME=2, NOTELE=3),
    avail         = 1,
    choiceVar     = TELE_TRA_2,
    utilities     = V
  )
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
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

print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
apollo_modelOutput(model)

print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - -")

apollo_saveOutput(model)
