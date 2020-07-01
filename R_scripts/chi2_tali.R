#############
# Chi-square test
###############

library(readr)

seeds_all <- read.csv (file = "data/seeds_all_2020.csv",
                                    sep = ";", dec = ".", header = T)

View(seeds_all)
seeds_all
str(seeds_all)

# Chi-quadrado entre dormencia e regeneration strategy
dor_reg<-table(seeds_all$Dormancy, seeds_all$Regeneration_strategy)
# non-dormant é 0, dormant é 1

chisq.test(dor_reg)  ## chi2 dormencia~estrategia_regeneração
chisq.test(dor_reg)$expected

chisq.dor_reg<-chisq.test(dor_reg, simulate.p.value =T, B=10000) ##chi2 dormencia~estrategia regeneração con permutaciones
str(chisq.dor_reg)
chisq.dor_reg$expected
chisq.dor_reg$observed
chisq.dor_reg$residuals #pearson's residuals. olhar as relaçoes (positivas e negativas) maiores que 1 - so essas importam.
summary(chisq.dor_reg)

#agora entre bud location - regeneration strategy
library(dplyr)
g<- filter(seeds_all,!(Bud_location == "undetermined"))

bud_reg0 <-table(g$Bud_location, g$Regeneration_strategy)
bud_reg <-bud_reg0[c(1:3), ]

chisq.test(bud_reg)

chisq.bud_reg1 <- chisq.test(bud_reg, simulate.p.value =T, B= 100000) ##chi2 clasedormencia~forma dispersion, con permutaciones

chisq.bud_reg$expected
chisq.bud_reg$observed
chisq.bud_reg1$residuals #pearson's residuals


# agora com growth forms
gf_reg<-table(seeds_all$Growth_form, seeds_all$Regeneration_strategy)

chisq.gf_reg<-chisq.test(gf_reg, simulate.p.value =T, B=10000) ##chi2 dormencia~estrategia regeneração con permutaciones
chisq.gf_reg
chisq.gf_reg$expected
chisq.gf_reg$observed
chisq.gf_reg$residuals #pearson's residuals. olhar as relaçoes (positivas e negativas) maiores que 1 - so essas importam.

