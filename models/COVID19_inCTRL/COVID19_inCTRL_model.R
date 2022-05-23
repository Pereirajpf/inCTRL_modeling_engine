#COVID-19 in-CTRL model, with contact matrix

inCTRL_model <- function(time, comparts, parameters){
  with(as.list(c(comparts, parameters)), {
    
    N = S + Et + Ent + IS + IA + ISq +IAq + H + ICU + M + Robs + R
    
    lamb   = beta * ((alpha_A*(ContMatrix%*%as.matrix(IA/N))) + (alpha_Aq*(ContMatrix%*%as.matrix(IAq/N)))
                   + (alpha_S*(ContMatrix%*%as.matrix(IS/N))) + (alpha_Sq*(ContMatrix%*%as.matrix(ISq/N))))

    dS     = (-lamb)*S
    
    dEt     = ((lamb*S)*q) - (epsilon*Et)
    
    dEnt    = ((lamb*S)*(1-q)) - (epsilon*Ent)
    
    dIS    = ((1-d)*(1-p)*epsilon*Ent) + ((1-c)*(1-p)*epsilon*Et) - (r_s*IS)
    
    dIA    = (p*epsilon*Ent) + ((1-c)*p*epsilon*Et) - (r_a*IA)
    
    dISq   = (c*(1-p)*epsilon*Et) + (d*(1-p)*epsilon*Ent) - (r_s*ISq)
    
    dIAq   = (c*p*epsilon*Et) - (r_a*IAq)
    
    dH     = (r_s*theta*(IS+ISq)) - (rho*H)
    
    dICU   = (pi*rho*H) - (omega*ICU)
    
    dM = (mu*omega*ICU) + (tau*rho*H)
    
    dRobs = ((1-theta)*r_s*ISq) + (r_a*IAq) + ((1-pi-tau)*rho*H) + ((1-mu)*omega*ICU)
    
    dR = ((1-theta)*r_s*IS) + (r_a*IA)
    
    
    return(list(c(dS, dEt, dEnt, dIS, dIA, dISq, dIAq, dH, dICU, dM, dRobs, dR)))
  })
}