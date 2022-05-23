# COVID-19 in-CTRL model rules
folder_name <- "COVID19_inCTRL"
  
  
model_rules <- list(
  comparts =  c("S","Et","Ent","IS","IA","ISq","IAq","H","ICU","M","Robs","R"),

  variables = c("ContMatrix",
                "beta",
                "alpha_S",
                "alpha_A",
                "alpha_Sq",
                "alpha_Aq",
                "epsilon",
                "r_a",
                "r_s",
                "theta",
                "p",
                "rho",
                "pi",
                "tau",
                "mu",
                "omega",
                "q",
                "c",
                "d")
)

save(model_rules, file = paste0("models/",folder_name,"/model_rules.RData"))
