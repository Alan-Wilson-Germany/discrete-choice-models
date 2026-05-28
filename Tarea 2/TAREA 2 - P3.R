###################################################################
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
#install.packages("apollo")
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_p2",
  modelDescr      = "Pregunta 2 - TAREA 2 - UCBM",
  indivID         = "PID", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
##database = apollo_modeChoiceData

### for data dictionary, use ?apollo_modeChoiceData

database = read.csv("C:\\Users\\alanw\\Downloads\\UCBM\\HW02_data_champ.csv", sep=";")

# Para INCOME,INCOME=1 como referencia
database$inc_2 <- ifelse(database$INCOME == 2, 1, 0)
database$inc_3 <- ifelse(database$INCOME == 3, 1, 0)

# Para DRINKER, DRINKER=1 como referencia
database$drk_2 <- ifelse(database$DRINKER == 2, 1, 0)
database$drk_3 <- ifelse(database$DRINKER == 3, 1, 0)
database$drk_4 <- ifelse(database$DRINKER == 4, 1, 0)

# DENSIT: 1 = Urbano (referencia), 2 = Suburbano, 3 = Rural
database$dens_suburb <- ifelse(database$DENSIT == 2, 1, 0)
database$dens_rural  <- ifelse(database$DENSIT == 3, 1, 0)

database$freq_reg <- ifelse(database$FREQ == 2, 1, 0)

# Solo mujeres (GENDER = 1),solo hombres (GENDER = 2)
database <- subset(database, GENDER == 2)

### Use only RP data
##database = subset(database,database$RP==1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

alternatives = c("DEL", "GOS", "VCP", "HM")


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc_del = 0,
  asc_gos = 0,
  asc_vcp = 0,
  asc_hm = 0,
  b_bottle = 0,
  b_lab = 0,
  b_smell = 0,
  b_adv = 0,
  b_freq_reg = 0,
  b_inc3_del = 0,
  b_inc3_gos = 0,
  b_inc3_vcp = 0,
  b_drk3 = 0,
  b_age = 0
)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_hm")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["DEL"]] = asc_del +
    b_bottle * BOTTLE_DEL +
    b_lab    * LAB_DEL +
    b_smell  * SMELL_DEL +
    b_adv    * ADV_DEL +
    b_inc3_del     * inc_3 +
    b_drk3     * drk_3 
  
  V[["GOS"]] = asc_gos +
    b_bottle * BOTTLE_GOS +
    b_lab    * LAB_GOS +
    b_smell  * SMELL_GOS +
    b_adv    * ADV_GOS +
    b_freq_reg * freq_reg +
    b_inc3_gos     * inc_3 +
    b_drk3     * drk_3 +
    b_age      * AGE
  
  V[["VCP"]] = asc_vcp +
    b_bottle * BOTTLE_VCP +
    b_lab    * LAB_VCP +
    b_smell  * SMELL_VCP +
    b_adv    * ADV_VCP +
    b_freq_reg * freq_reg +
    b_inc3_vcp     * inc_3 +
    b_drk3     * drk_3 +
    b_age     * AGE
  
  V[["HM"]] = asc_hm +
    b_bottle * BOTTLE_HM +
    b_lab    * LAB_HM +
    b_smell  * SMELL_HM +
    b_adv    * ADV_HM 
  
  # alternativa de referencia
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(DEL=1, GOS=2, VCP=3, HM=4), 
    avail         = list(DEL=1, GOS=1, VCP=1, HM=1), 
    choiceVar     = CHOSEN,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  ##P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

sink("OUTPUT T2-P3 - HOMBRE.txt")
apollo_modelOutput(model)           
sink()                       