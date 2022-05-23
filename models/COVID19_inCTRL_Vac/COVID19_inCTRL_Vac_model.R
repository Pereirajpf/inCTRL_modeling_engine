#COVID-19 in-CTRL model, with contact matrix

inCTRL_Vac_model <- function(time, comparts, parameters){
  with(as.list(c(comparts, parameters)), {
    
    N = S+S_v+E+E_v+IA+IA_v+IS+IS_v+H+H_v+ICU+ICU_v+M+M_v+R+R_v
    
    lamb   = beta * ((alfa_A*(ContMatrix%*%as.matrix(IA/N) + (ContMatrix%*%as.matrix(IA_v/N))))
                   + (alfa_S*(ContMatrix%*%as.matrix(IS/N) + (ContMatrix%*%as.matrix(IS_v/N)))))
    
    
    dS     = (-lamb)*S - S*Upsilon + R*gamma + R_v*gamma_v + S_v*gamma_v
    
    dE     = S*lamb - E*epsilon
    
    dIA    = E*(1-p)*epsilon - IA*r_a
    
    dIS    = E*p*epsilon - IS*r_s
    
    dH     = IS*theta*r_s - H*rho
    
    dICU   = H*pi*rho - ICU*omega
    
    dM     = H*tau*rho + ICU*mu*omega
    
    dR     = ICU*(1-mu)*omega + H*(1-pi-tau)*rho + IS*(1-theta)*r_s + IA*r_a - R*Upsilon - R*gamma
    
    dS_v   = -S_v*(1-e)*lamb - S_v*gamma_v + S*Upsilon
    
    dE_v   = S_v*(1-e)*lamb - E_v*epsilon
    
    dIA_v  = E_v*(1-(1-f)*p)*epsilon - IA_v*r_a
    
    dIS_v  = E_v*(1-f)*p*epsilon - IS_v*r_s
    
    dH_v   = IS_v*theta*r_s - H_v*rho
    
    dICU_v = H_v*pi*rho - ICU_v*omega
    
    dM_v   = ICU_v*mu*omega + H_v*tau*rho
    
    dR_v   = IS_v*(1-theta)*r_s + IA_v*r_a + H_v*(1-pi-tau)*rho + ICU_v*(1-mu)*omega + R*Upsilon - R_v*gamma_v
    
    return(list(c(dS,dE,dIA,dIS,dH,dICU,dM,dR,dS_v,dE_v,dIA_v,dIS_v,dH_v,dICU_v,dM_v,dR_v)))
  })
}
