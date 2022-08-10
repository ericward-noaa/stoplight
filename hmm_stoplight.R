library(depmixS4)

d = readxl::read_xlsx(file.choose())

vars = d[,1]
d = as.data.frame(t(d))
names(d) = c(vars)$...1
d = d[-1,]

for(i in 1:ncol(d)) {
  d[,i] = factor(d[,1], levels = c("R","Y","G"))
}

# fit model to all 
formulas = list(PDO_decmar ~ 1,
                PDO_maysep ~ 1,
                ONI ~ 1,
                SST_ndbc ~ 1,
                Upper20_novmar~1,
                Upper20_maysep ~ 1,
                DeepTemp ~ 1,
                DeepSal ~ 1,
                Copepod_richness ~ 1,
                Copepod_biomass_N ~ 1,
                Copepod_biomass_S ~ 1,
                Biological_transition ~ 1,
                Nearshore_ichthyo ~ 1, 
                Nearoff_ichthyo ~ 1, 
                Chinook_juv ~ 1,
                Coho_juv ~ 1)

families = list(multinomial(),multinomial(),multinomial(),multinomial(),
              multinomial(),multinomial(),multinomial(),multinomial(),
              multinomial(),multinomial(),multinomial(),multinomial(),
              multinomial(),multinomial(),multinomial(),multinomial())

# fit 2 state model
best_model = NA
best_aic = 1.0e10
for(i in 1:20) {
model2 = depmix(response = formulas, data = d, nstates= 2, 
                family = families)
fm2 = fit(model2)
if(AIC(fm2) < best_aic) {
  best_aic = AIC(fm2)
  best_model = fm2
}
}
fm2 = best_model
# fit 3 state model
best_model = NA
best_aic = 1.0e10
for(i in 1:20) {
  model3 = depmix(response = formulas, data = d, nstates= 3, 
                  family = families)
  fm3 = fit(model3)
  if(AIC(fm3) < best_aic) {
    best_aic = AIC(fm3)
    best_model = fm3
  }
}
fm3 = best_model

# 3 state model >> 2 state
# AIC(fm3) AIC(fm2)

# get the estimated state
plot(posterior(fm3)$state)
