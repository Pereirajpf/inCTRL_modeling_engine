# COVID-19 in-CTRL Vaccination model rules

model_rules <- list(
  comparts =  c("S","E","IA","IS","H","ICU","M","R",
               "S_v","E_v","IA_v","IS_v","H_v","ICU_v","M_v","R_v"),
  
  variables = c("ContMatrix",
                  "alfa_A",
                  "alfa_S",
                  "beta",
                  "epsilon",
                  "r_s",
                  "r_a",       
                  "theta",
                  "p",
                  "rho",
                  "pi",
                  "tau",
                  "omega",
                  "mu",        
                  "Upsilon",
                  "e",
                  "f",
                  "gamma",
                  "gamma_v")
)

save(model_rules, file = "models/COVID19_inCTRL_Vac/model_rules.RData")
