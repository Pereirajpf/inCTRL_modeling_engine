# COVID-19 in-CTRL Vaccination model rules

model_rules <- list(
  comparts =  c("S","E","I","R"),
  
  variables = c("beta","sigma","gamma")
)

save(model_rules, file = "models/SEIR_heterogeneous/model_rules.RData")