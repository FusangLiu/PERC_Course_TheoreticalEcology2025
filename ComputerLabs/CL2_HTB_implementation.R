##computerlab 2: Vegetation catastrophes###
library(deBif)


#The main question of this computer lab is how the interplay of grazing (b) and soil water availability (W)
#affects vegetation biomass (P). 
#Therefore, we will study how the model behaves as a function of grazing (parameter b)
#and water infiltration (W0)

#1) Start with high water infiltration (W0 = 0.9). Use timeseries, isoclines, 
#and 1-parameter bifurcations over parameter b to study the model. Make sure you understand everything before moving on
#2) Do the same for low ater infiltration (W0 = 0.2). Again, make sure you understand what is happening
#3) Challenge: If you've time and want a challenge make a two-parameter bifurcation graph over parameter b and W0, do this only when you
#have done step 1 and 2! 


###Model implementation####
Vegetation <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    W_in <- R * (P + k2 * W0) / (P + k2)
    g <- W/(W + k1)
    dW <- W_in - cmax * g * P - rw * W
    dP <- gmax * g * P - d * P - b * P
    return(list(c(dW, dP)))
  })
}

state <- c(W = 0.1, P = 0.1)
parms <- c(W0 = 0.9, b = 0, gmax = 0.5, k1 = 3, d = 0.1, cmax = 0.05, R = 2, rw = 0.1, k2 = 5)

phaseplane(Vegetation, state, parms)
bifurcation(Vegetation, state, parms) 


#I set W0 and b as the first two parameters, this will put them on top of the list in de deBif program


