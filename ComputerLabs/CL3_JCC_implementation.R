
## Libraries
library(deBif)


## Part 1

state <- c(P = 5, D = 5)
parms <- c(r=0.2, eps=1, a=0.02, delt=0.25, Th=1, K = 20)


Rosenzweig <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dP = r*P*(1-P/K) - a*P/(1+a*Th*P)*D
    dD = eps*a*P/(1+a*Th*P)*D - delt*D
    
    return(list(c(dP, dD)))
  })
}

phaseplane(Rosenzweig, state, parms)
bifurcation(Rosenzweig, state, parms)


## part 2

state <- c(P = 0.5, D = 0.5)
parms <- c(r=2, eps=0.5, Ad=44.444, C=6.667, a=9.1463, Th=0.667, delt=0.1, K = 0.5)


Rosenzweig_pred <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dP = r*P*(1-P/K) - a*P/(1+a*Th*P)*D
    dD = eps*a*P/(1+a*Th*P)*D - delt*D - C*D*D/(1+Ad*D*D)
    
    return(list(c(dP, dD)))
  })
}

phaseplane(Rosenzweig_pred, state, parms)
bifurcation(Rosenzweig_pred, state, parms)
