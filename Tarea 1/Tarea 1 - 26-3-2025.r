# ################################################################# #
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
  modelName       = "MNL_EV",
  modelDescr      = "Modelo solo constantes de Tarea 1",
  indivID         = "HHID", 
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

database <- read.csv("C:\\Users\\alanw\\Downloads\\TAREA_MKT Grupo 13\\archivo_editado.csv", header=T, na.strings=c(""," ","NA"),sep=",", dec=',')
database$DIST  <- as.numeric(database$DIST)
database$POPDENSE <- as.numeric(database$POPDENSE)

### Use only RP data
##database = subset(database,database$RP==1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_notadopt   = 0,
              asc_adopt   = 0,
              b_bajo  = 0,
              b_medio  = 0,
              b_alto  = 0,
              b_hhsize   = 0,
              b_solpan= 0,
              b_dist = 0,
              b_density = 0,
              b_neigh = 0)
              

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_notadopt")

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
  V[["notadopt"]]  = asc_notadopt + b_bajo * Bajo + b_medio * Medio + b_alto * Alto  + b_hhsize * HHSIZE + b_solpan * SOLPAN + b_dist * DIST + b_density * POPDENSE + b_neigh * NEIGHB
  V[["adopt"]]  = asc_adopt + b_bajo * Bajo + b_medio * Medio + b_alto * Alto + b_hhsize * HHSIZE + b_solpan * SOLPAN + b_dist * DIST + b_density * POPDENSE + b_neigh * NEIGHB

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(notadopt=2, adopt=1), 
    avail         = list(notadopt=UNO, adopt=UNO), 
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








# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### RP elasticities

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
database$cost_rail = 1.01*database$cost_rail

### Rerun predictions with the new data, and save into a separate matrix
apollo_inputs=apollo_validateInputs()
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs)

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail = 1/1.01*database$cost_rail

### Compute own elasticity for rail:
log(sum(predictions_new[,6],na.rm=TRUE)/sum(predictions_base[,6],na.rm=TRUE))/log(1.01)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()



