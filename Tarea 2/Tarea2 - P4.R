### Limpiar memoria y cargar libreria
rm(list = ls())
library(apollo)

### Inicializar codigo
apollo_initialise()

### Configurar
apollo_control = list(
  modelName       = "MNL_RP",
  modelDescr      = "Tarea 2 UCB Parte 4",
  indivID         = "PID", 
  mixing    = TRUE, 
  nCores    = 6
)

database = read.csv("C:\\Users\\alanw\\Downloads\\UCBM\\HW02_data_champ.csv", sep=";")
database = subset(database, CHOSEN != 2)  # Excluir GOS

# Para INCOME,INCOME=1 como referencia
database$inc_2 <- ifelse(database$INCOME == 2, 1, 0)
database$inc_3 <- ifelse(database$INCOME == 3, 1, 0)

# Para DRINKER, DRINKER=1 como referencia
database$drk_2 <- ifelse(database$DRINKER == 2, 1, 0)
database$drk_3 <- ifelse(database$DRINKER == 3, 1, 0)
database$drk_4 <- ifelse(database$DRINKER == 4, 1, 0)

# GENDER: 1 = Mujer (referencia), 2 = Hombre
database$male <- ifelse(database$GENDER == 2, 1, 0)

# DENSIT: 1 = Urbano (referencia), 2 = Suburbano, 3 = Rural
database$dens_suburb <- ifelse(database$DENSIT == 2, 1, 0)
database$dens_rural  <- ifelse(database$DENSIT == 3, 1, 0)

database$freq_reg <- ifelse(database$FREQ == 2, 1, 0)

cols_to_convert <- c("PRICE_DEL", "PRICE_GOS", "PRICE_VCP", "PRICE_HM")
database[cols_to_convert] <- lapply(database[cols_to_convert], function(x) as.numeric(gsub(",", ".", x)))

apollo_beta = c(
  b_smell = 0,
  b_freq_reg = 0,
  b_inc3_del = 0,
  b_inc3_vcp = 0,
  b_drk3 = 0,
  b_male_vcp = 0,
  b_age = 0,
  
  mu_log_bottle = 2, sigma_log_bottle = 0.1,
  mu_log_inflab = 2, sigma_log_inflab = 0.1,
  mu_log_lab    = 2, sigma_log_lab    = 0.1,
  mu_log_adv    = 2, sigma_log_adv    = 0.1
)

#DE AQUI ABAJO CAMBIAR
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c("draws_bottle",
                     "draws_inflab",
                     "draws_lab",
                     "draws_adv"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_adv"]]    = exp(mu_log_adv + sigma_log_adv * draws_adv)
  randcoeff[["b_lab"]]    = exp(mu_log_lab + sigma_log_lab * draws_lab)
  randcoeff[["b_inflab"]] = exp(mu_log_inflab + sigma_log_inflab * draws_inflab)
  randcoeff[["b_bottle"]] = exp(mu_log_bottle + sigma_log_bottle * draws_bottle)
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()

  V[["DEL"]] =
    b_bottle * BOTTLE_DEL +
    b_lab    * LAB_DEL +
    b_inflab * INFLAB_DEL +
    b_smell  * SMELL_DEL +
    b_adv    * ADV_DEL +
    b_inc3_del     * inc_3 +
    b_drk3     * drk_3 
  
  V[["VCP"]] =
    b_bottle * BOTTLE_VCP +
    b_lab    * LAB_VCP +
    b_inflab * INFLAB_VCP +
    b_smell  * SMELL_VCP +
    b_adv    * ADV_VCP +
    b_freq_reg * freq_reg +
    b_inc3_vcp     * inc_3 +
    b_drk3    * drk_3 +
    b_male_vcp     * male +
    b_age     * AGE
  
  V[["HM"]] =
    b_bottle * BOTTLE_HM +
    b_lab    * LAB_HM +
    b_inflab * INFLAB_HM +
    b_smell  * SMELL_HM +
    b_adv    * ADV_HM 
  
  
  mnl_settings = list(
    alternatives = c(DEL=1, VCP=3, HM=4), 
    avail        = list(DEL=1, VCP=1, HM=1), 
    choiceVar    = CHOSEN,
    utilities    = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Para modelos con parámetros aleatorios (aunque no sean de panel):
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  # Finalizar como siempre
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
  
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

#apollo_testMixing(model, coeff = c("b_bottle", "b_inflab", "b_lab","b_adv"))

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

sink("OUTPUT T2-P4.txt")
apollo_modelOutput(model)           
sink()        
