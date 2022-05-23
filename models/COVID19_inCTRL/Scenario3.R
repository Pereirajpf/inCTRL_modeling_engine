# Create default values as RData object for inCTRL model

folder_name <- "COVID19_inCTRL"

#Start and end date===
time <- as.Date("2020-02-10")

tmax <- as.Date("2022-08-31")
#save(time, file = "models/COVID19_inCTRL_Vac/start_end_dates.RData")
#write.csv(time,'models/COVID19_inCTRL_Vac/start_end_dates.csv')


#Initial values for the compartments====
comparts <- c(S   = c(436202,455843,504940,545322,550444,547680,566594,672422,784224,789733,745178,740141,676762,622912,549591,1107921),
              Et  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              Ent = c(0,0,0,0,0,0,0,0,71,0,0,0,0,0,0,0),
              IS  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              IA  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              ISq = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              IAq = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              H   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              ICU = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              M   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              Robs= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
              R   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


#Load Contact Matrix
load(paste0("models/",folder_name,"/CONTACTMATRIX.Rdata"))
#First
alpha_home   <- diag(rep(1,16))
alpha_work   <- diag(rep(1,16))
alpha_school <- diag(rep(1,16))
alpha_other  <- diag(rep(1,16))
#bks
alpha_s1 <- 0.00
alpha_hwo1 <- 1 - 0.69 
alpha_hwo2 <- 1 - 0.55
alpha_hwo3 <- 1 - 0.43
alpha_s2 <- (1-0.3)*(0.1*(1-0)+0.9*(1-0.47)) #(33% less contacts * (compliance to mask usage 90% * face mask 47% effectiveness)) 
alpha_hwo4 <- 1 - 0.58
alpha_s3 <- 0.00
alpha_hwo5 <- 1 - 0.35

#Parameters and breakpoints====
params_list <- list(timeline = c(0,
                                 35, #School closure
                                 37, #bk1
                                 90, #bk2
                                 190, #bk3
                                 218, #reopening of school
                                 266, #bk4
                                 289, #change in pi
                                 312, #school closing for Christmas holidays
                                 317, #bk5
                                 325, #change in pi
                                 #Scenarios
                                 340, #15Jan new confinement
                                 347, #schools close
                                 453, #2 months after school closure (15 March 2021)
                                 as.numeric(tmax-time)),
                    params = list(list(
                      ContMatrix = (CONTACTMATRIX$home%*%alpha_home + 
                                      CONTACTMATRIX$work%*%alpha_work +
                                      CONTACTMATRIX$school%*%alpha_school +
                                      CONTACTMATRIX$other%*%alpha_other),
                      beta = 0.068,
                      alpha_S = 1,
                      alpha_A = 0.5,
                      alpha_Sq = 1,
                      alpha_Aq = 0.5,
                      epsilon = 1/3.8,
                      r_a = 1/3.4,
                      r_s = 1/3.4,
                      theta = c(0.051,0.051,0.051,0.051,0.051,0.051,0.051,0.051,0.051,0.051,0.1011,0.1011,0.2199,0.2199,0.40,0.40),
                      p = 0.445,
                      rho = 1/9,
                      pi = 0.126,
                      tau = 0.192,
                      mu = 0.267,
                      omega = 1/20,
                      q = 0,
                      c = 1,
                      d = 0 #0.445
                    ),
                    #Clossure of schools
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%alpha_home + 
                        CONTACTMATRIX$work%*%alpha_work +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s1) +
                        CONTACTMATRIX$other%*%alpha_other
                    ),
                    #bk1
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo1) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo1) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s1) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo1)
                    ),
                    #bk2
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo2) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo2) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s1) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo2)
                    ),
                    #bk3
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo3) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo3) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s1) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo3)
                    ),
                    #reopening of schools
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo3) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo3) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s2) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo3)
                    ),
                    #bk4
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo4) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo4) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s2) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo4)
                    ),
                    #change pi
                    list(
                      pi = 0.079
                    ), 
                    #school closing for Christmas holidays
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo4) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo4) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s3) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo4)
                    ),
                    #bk5
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo5) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo5) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s3) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo5)
                    ),
                    #change pi
                    list(
                      pi = 0.112
                    ),
                    #scenario
                    # equal to bk1 but no school closure
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo1) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo1) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s3) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo1)
                    ),
                    #schools close
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo1) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo1) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s1) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo1)
                    ),
                    #reopen, like bk3 + schools open with masks
                    list(
                      ContMatrix = CONTACTMATRIX$home%*%(alpha_home*alpha_hwo3) + 
                        CONTACTMATRIX$work%*%(alpha_work*alpha_hwo3) +
                        CONTACTMATRIX$school%*%(alpha_school * alpha_s2) +
                        CONTACTMATRIX$other%*%(alpha_other*alpha_hwo3)
                    )
                    )
)
#save(params_list, file = "models/COVID19_inCTRL_Vac/parameters.RData")
#write.csv(params_list, file = "models/COVID19_inCTRL_Vac/parameters.csv")

wrapper <- "hetero_model_wrapper"

save(time, comparts, params_list, wrapper, file = paste0("models/",folder_name,"/Scenario3.RData"))

