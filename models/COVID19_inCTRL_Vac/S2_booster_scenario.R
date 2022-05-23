# Create default values as RData object for inCTRL vac model

#Scenario S2 -> S2 contact reduction
#            -> 1 year immunity loss

#Start and end date===
time <- as.Date("2021-01-24")
#tmax <- as.Date("2021-09-29")
tmax <- as.Date("2022-09-30")


#Initial values for the compartments====
comparts <- c(S   = c(386989,402879,454523,490150,471175,468934,465720,552733,672346,676536,637053,633486,586943,541284,508977,1014532),
              E   = c(  1144,  1631,  2014,  2436,  2906,  2764,  2798,  3263,  3910,  4130,  3732,  3535,  3092,  2295,  1924,   6077),
              IA  = c(   811,  1337,  1472,  1794,  2030,  2018,  1987,  2372,  2769,  2925,  2649,  2339,  2004,  1519,  1170,   3433),
              IS  = c(  1012,  1668,  1836,  2238,  2532,  2517,  2478,  2958,  3453,  3648,  3304,  2917,  2499,  1895,  1459,   4281),
              H   = c(     8,     9,     4,     5,    20,    20,    49,    59,   128,   129,   274,   272,   509,   468,  1226,   2473),
              ICU = c(     0,     0,     0,     0,     3,     2,     6,     7,    31,    32,    80,    79,   143,   132,    81,    164),
              M   = c(     1,     0,     0,     2,     0,     8,    11,    13,    30,    63,    95,   184,   361,   570,   844,   8602),
              R   = c (46237, 48319, 45091, 48697, 71778,71417,  93545,111017,101557,102270, 97991, 97329, 81211, 74749, 33910,  68359),
              #Vacinated
              S_v   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              E_v   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              IA_v  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              IS_v  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              H_v   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              ICU_v = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              M_v   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              R_v   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)


#Load Contact Matrix
load("models/COVID19_inCTRL_Vac/CONTACTMATRIX.Rdata")
alfa_home1   <- diag(rep(1,16))
alfa_work1   <- diag(rep(1,16))
alfa_school1 <- diag(rep(1,16))
alfa_other1  <- diag(rep(1,16))
ContMatrix1 = CONTACTMATRIX$home%*%alfa_home1 + 
  CONTACTMATRIX$work%*%alfa_work1 +
  CONTACTMATRIX$school%*%alfa_school1 +
  CONTACTMATRIX$other%*%alfa_other1

#Vaccination effectiveness
Pi <- c(0.40,0.80,0.85,0.85,0.95,0.95,0.95,0.95,0.95,0.95,0.99,0.99,0.99,0.99) #Final vaccination coverage
#Vacination time using real data
Vac_times <- list(start = as.numeric(as.Date(c("2021-09-01","2021-09-01", "2021-08-15","2021-08-15", "2021-08-01","2021-08-01", "2021-07-15","2021-07-15", "2021-07-01","2021-07-01", "2021-02-08","2021-02-08", "2021-02-08","2021-02-08")) - time),
                  end  = as.numeric(as.Date(c("2021-10-29","2021-10-29", "2021-10-15","2021-10-15", "2021-10-15","2021-10-15", "2021-09-29","2021-09-29", "2021-09-15","2021-09-15", "2021-08-29","2021-08-29", "2021-08-29","2021-08-29")) - time))

Ti <- c(Vac_times$end - Vac_times$start)

#Vaccination breakpoints, in days after initial time and by descending order
Vac_bk <- list(start = rev(unique(Vac_times$start)),
               end = rev(unique(Vac_times$end)))

#Function to calculate vaccination rate
Upsilon_function <- function(Pi,Ti){-((log(1-Pi))/Ti)}
#Vaccination rate by age group
Upsilon_vacinating <- c(0,0,Upsilon_function(Pi,Ti))

#COntact reduction====
#contact reduction times
l_i_time <- as.numeric(c(as.Date("2021-03-09"),as.Date("2021-07-02"),as.Date("2021-07-31"),as.Date("2021-09-30")) - time)
#Contact reduction
l_i <- c(1-0.8295, 1-0.6663, 1-0.6100, 1-0.6652,1)

#booster shot
booster_time <- list(start = as.numeric(as.Date("2021-10-14")+14-time),
                     end = as.numeric(as.Date("2021-11-14")+14-time))

#Parameters and breakpoints====
params_list <- list(timeline = c(0,
                                 Vac_bk$start[1], # 15  # Start vaccination 1
                                 l_i_time[1], # 44 #l_i2
                                 Vac_bk$start[2], # 158 # Start vaccination 2
                                 l_i_time[2],  #159 #l_i3
                                 Vac_bk$start[3], # 172 # Start vaccination 3
                                 l_i_time[3],  #188 #l_i4
                                 Vac_bk$start[4], # 189 # Start vaccination 4
                                 Vac_bk$start[5], # 203 # Start vaccination 5
                                 Vac_bk$end[1],   # 217 # End vaccination 1
                                 Vac_bk$start[6], # 220 # Start vaccination 6
                                 Vac_bk$end[2],   # 234 # End vaccination 2
                                 Vac_bk$end[3], # 248 # End vaccination 3
                                 l_i_time[4],  # 249 #l_i5
                                 Vac_bk$end[4], # 264 # End vaccination 4 and 5
                                 booster_time$start, #277 #booster shot to age groups >= 65 years old
                                 Vac_bk$end[5], # 278 # End vaccination 6
                                 booster_time$end, #308 #End booster shot to age groups >= 65 years old
                                 as.numeric(tmax-time) # 706 # End date
),

params = list(list(
  ContMatrix = ContMatrix1,
  alfa_A = 0.5,
  alfa_S = 1,
  beta = 0.1088 * l_i[1], 
  epsilon = 0.2632, 
  r_s = 0.2941, 
  r_a = 0.2941, 
  theta = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.03, 0.03, 0.15, 0.15, 0.28, 0.28), 
  p = 0.5550, 
  rho = c(0.17, 0.17, 0.12, 0.12, 0.14, 0.14, 0.12, 0.12, 0.10, 0.10, 0.09, 0.09, 0.08, 0.08, 0.07, 0.07), 
  pi = c(0.04, 0.00, 0.02, 0.00, 0.03, 0.07, 0.07, 0.18, 0.11, 0.15, 0.16, 0.13, 0.12, 0.13, 0.08, 0.03),
  tau = c(0.00, 0.00, 0.00, 0.00, 0.01, 0.00, 0.00, 0.00, 0.02, 0.03, 0.01, 0.06, 0.07, 0.08, 0.18, 0.33),
  omega = c(0.03, 0.03, 0.07, 0.07, 0.05, 0.05, 0.06, 0.06, 0.05, 0.05, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04),
  mu = c(0.10, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.11, 0.07, 0.04, 0.16, 0.14, 0.24, 0.24, 0.34, 0.53), 
  Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), 
  e = 0.7, 
  f = 0.8333, 
  gamma = 1/365,
  gamma_v= 1/365
),

#start vacination 1
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[13], Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#l_i2
list(beta = 0.1088 * l_i[2]),
#start vacination 2
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[11], Upsilon_vacinating[12], Upsilon_vacinating[13], Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#l_i3
list(beta = 0.1088 * l_i[3]),
#start vacination 3
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[09], Upsilon_vacinating[10], Upsilon_vacinating[11], Upsilon_vacinating[12], Upsilon_vacinating[13], Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#l_i4
list(beta = 0.1088 * l_i[4]),
#start vacination 4
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[7], Upsilon_vacinating[8], Upsilon_vacinating[9], Upsilon_vacinating[10], Upsilon_vacinating[11], Upsilon_vacinating[12], Upsilon_vacinating[13], Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#start vacination 5
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[5], Upsilon_vacinating[6], Upsilon_vacinating[7], Upsilon_vacinating[8], Upsilon_vacinating[9], Upsilon_vacinating[10], Upsilon_vacinating[11], Upsilon_vacinating[12], Upsilon_vacinating[13], Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#end vacination 1
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[5], Upsilon_vacinating[6], Upsilon_vacinating[7], Upsilon_vacinating[8], Upsilon_vacinating[9], Upsilon_vacinating[10], Upsilon_vacinating[11], Upsilon_vacinating[12], 0.0, 0.0, 0.0, 0.0)),
#start vacination 6
list(Upsilon = c(0.0, 0.0, Upsilon_vacinating[3], Upsilon_vacinating[4], Upsilon_vacinating[5], Upsilon_vacinating[6], Upsilon_vacinating[7], Upsilon_vacinating[8], Upsilon_vacinating[9], Upsilon_vacinating[10], Upsilon_vacinating[11], Upsilon_vacinating[12], 0.0, 0.0, 0.0, 0.0)),
#end vacination 2
list(Upsilon = c(0.0, 0.0, Upsilon_vacinating[3], Upsilon_vacinating[4], Upsilon_vacinating[5], Upsilon_vacinating[6], Upsilon_vacinating[7], Upsilon_vacinating[8], Upsilon_vacinating[9], Upsilon_vacinating[10], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
#end vacination 3
list(Upsilon = c(0.0, 0.0, Upsilon_vacinating[3], Upsilon_vacinating[4], Upsilon_vacinating[5], Upsilon_vacinating[6], Upsilon_vacinating[7], Upsilon_vacinating[8], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
#l_i5
list(beta = 0.1088 * l_i[5]),
#end vacination 4 and 5
list(Upsilon = c(0.0, 0.0, Upsilon_vacinating[3], Upsilon_vacinating[4], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
#start booster shot
list(Upsilon = c(0.0, 0.0, Upsilon_vacinating[3], Upsilon_vacinating[4], 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#end vacination 6
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Upsilon_vacinating[14], Upsilon_vacinating[15], Upsilon_vacinating[16])),
#end booster shot
list(Upsilon = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
)
)


wrapper <- "hetero_model_wrapper"

save(time, comparts, params_list, wrapper, file = "models/COVID19_inCTRL_Vac/S2booster_scenario.RData")