library(readr)

seeds_all <- read.csv (file = "data/seeds_all_2020.csv",
                                    sep = ";", dec = ".", header = T)

View(seeds_all)
seeds_all
str(seeds_all)

# Chi-quadrado entre dormencia e regeneration strategy
dor_reg<-table(seeds_all$Dormancy, seeds_all$Regeneration_strategy)

chisq.test(dor_reg)  ## chi2 dormencia~estrategia_regeneração
chisq.test(dor_reg)$expected

chisq.dor_reg<-chisq.test(dor_reg, simulate.p.value =T, B=10000) ##chi2 dormencia~estrategia regeneração con permutaciones
str(chisq.dor_reg)
chisq.dor_reg$expected
chisq.dor_reg$observed
summary(chisq.dor_reg)

#agora entre bud location - regeneration strategy

bud_reg<-table(seeds_all$Bud_location, seeds_all$Regeneration_strategy)

chisq.test(bud_reg)

chisq.test(bud_reg, simulate.p.value =T, B= 100000) ##chi2 clasedormencia~forma dispersion, con permutaciones







