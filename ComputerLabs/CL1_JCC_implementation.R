
## Libraries
library(deBif)


## Questions 1 to 3

state <- c(N = 0.1)
parms <- c(r=0.1, K=10)


Logistic <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dN = r*N*(1-N/K)
    
    return(list(c(dN)))
  })
}

phaseplane(Logistic, state, parms)
bifurcation(Logistic, state, parms)


## Questions 4 to 13

state <- c(N = 0.1)
parms <- c(A=0.5, q=20, E=0.314, f=0.474, P=0.7, r=0.1)


Logistic <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    K=q*A
    N0=f*A
    dN = r*N*(1-N/K) - E*P*N^2/(N0^2 + N^2)
    
    return(list(c(dN)))
  })
}

phaseplane(Logistic, state, parms)
bifurcation(Logistic, state, parms)
