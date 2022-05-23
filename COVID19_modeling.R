# COVID-19 modeling
#Load engine
source("functions/ModelEngine.R")

#===============================================================================
#Covid-19 initial outbreak modeling
#===============================================================================
inCTRL_model <- ModelEngine$new(model="COVID19_inCTRL")
inCTRL_model$print()

inCTRL_model$run()

inCTRL_model$plot(compact = T, filter = c("H","ICU","M"))

#Scenario 0 - no intevention
inCTRL_model_S0 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario0.RData")

inCTRL_model_S0$run()

inCTRL_model_S0$plot(compact = T, filter = c("ICU"))


#Scenario 1 - 1 month school closure and lockdown
inCTRL_model_S1 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario1.RData")

inCTRL_model_S1$run()

inCTRL_model_S1$plot(compact = T, filter = c("ICU"))

#Scenario 2 - 45 days school closure and lockdown
inCTRL_model_S2 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario2.RData")

inCTRL_model_S2$run()

inCTRL_model_S2$plot(compact = T, filter = c("ICU"))

#Scenario 3 - 2 month school closure and lockdown
inCTRL_model_S3 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario3.RData")

inCTRL_model_S3$run()

inCTRL_model_S3$plot(compact = T, filter = c("ICU"))



#===============================================================================
#COVID-19 Vacination modeling====
#===============================================================================
inCTRLVac_model <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S1_scenario.RData")
inCTRLVac_model$print()

inCTRLVac_model$run()

inCTRLVac_model$result

inCTRLVac_model$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model$plot(compact = F, filter = c("H"))

inCTRLVac_model$plot(compact = T, filter = c("ICU"))

inCTRLVac_model$plot(compact = T, filter = c("R","R_v"))

inCTRLVac_model$plot(compact = F, filter = c("R_v"))
inCTRLVac_model$plot(compact = F, filter = c("S_v"))

#S2 scenario====
inCTRLVac_model_S2 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S2_scenario.RData")
inCTRLVac_model_S2$print()

inCTRLVac_model_S2$run()

inCTRLVac_model_S2$result

inCTRLVac_model_S2$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S2$plot(compact = F, filter = c("H"))

inCTRLVac_model_S2$plot(compact = T, filter = c("ICU"))

inCTRLVac_model_S2$plot(compact = T, filter = c("R","R_v"))

inCTRLVac_model_S2$plot(compact = F, filter = c("R_v"))
inCTRLVac_model_S2$plot(compact = F, filter = c("S_v"))


#S3 scenario====
inCTRLVac_model_S3 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S3_scenario.RData")
inCTRLVac_model_S3$print()

inCTRLVac_model_S3$run()

inCTRLVac_model_S3$result

inCTRLVac_model_S3$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S3$plot(compact = F, filter = c("H"))

inCTRLVac_model_S3$plot(compact = T, filter = c("ICU"))

inCTRLVac_model_S3$plot(compact = T, filter = c("R","R_v"))

inCTRLVac_model_S3$plot(compact = F, filter = c("R_v"))
inCTRLVac_model_S3$plot(compact = F, filter = c("S_v"))


#S2 booster scenario====
inCTRLVac_model_S2 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S2booster_scenario.RData")
inCTRLVac_model_S2$print()

inCTRLVac_model_S2$run()

inCTRLVac_model_S2$result

inCTRLVac_model_S2$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S2$plot(compact = F, filter = c("H"))

inCTRLVac_model_S2$plot(compact = T, filter = c("ICU"))

inCTRLVac_model_S2$plot(compact = T, filter = c("S","S_v"))
inCTRLVac_model_S2$plot(compact = T, filter = c("R","R_v"))

inCTRLVac_model_S2$plot(compact = F, filter = c("R_v"))
inCTRLVac_model_S2$plot(compact = F, filter = c("S_v"))


#S3 booster scenario====
inCTRLVac_model_S3 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S3booster_scenario.RData")
inCTRLVac_model_S3$print()

inCTRLVac_model_S3$run()

inCTRLVac_model_S3$result

inCTRLVac_model_S3$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S3$plot(compact = F, filter = c("H"))

inCTRLVac_model_S3$plot(compact = T, filter = c("ICU"))

inCTRLVac_model_S3$plot(compact = T, filter = c("S","S_v"))
inCTRLVac_model_S3$plot(compact = T, filter = c("R","R_v"))

inCTRLVac_model_S3$plot(compact = F, filter = c("R_v"))
inCTRLVac_model_S3$plot(compact = F, filter = c("S_v"))
