#Simple SEIR model

seir_model <- function(time, variables, parameters){
  with(as.list(c(variables, parameters)), {
    N <- S + E + I + R
    dS = -beta * S * (I/N)
    dE = beta * S * (I/N) - sigma * E
    dI = sigma * E - gamma * I
    dR = gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}
