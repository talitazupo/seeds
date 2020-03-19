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

# agora...



seasb_dis<-table(fen$seasonb, fen$Dispersal_form)
chisq.test(seasb_dis, simulate.p.value =T, B=100000) ##chi2 seasonb~forma dispersion, permutaion
seasc_dis<-table(fen$seasonc, fen$Dispersal_form)
chisq.test(seasc_dis, simulate.p.value =T, B=100000) ##chi2 seasonc~forma dispersion, permutaion
seasd_dis<-table(fen$seasond, fen$Dispersal_form)
chisq.test(seasd_dis, simulate.p.value =T, B=100000) ##chi2 seasond~forma dispersion, permutaion
############################################
seasb_dor<-table(fen$seasonb, fen$Dormancy)
chisq.test(seasb_dor, simulate.p.value =T, B=100000) ##chi2 seasonb~dormencia, permutaion
seasc_dor<-table(fen$seasonc, fen$Dormancy)
chisq.test(seasc_dor, simulate.p.value =T, B=100000) ##chi2 seasonc~dormencia, permutaion
seasd_dor<-table(fen$seasond, fen$Dormancy)
chisq.test(seasd_dor, simulate.p.value =T, B=100000) ##chi2 seasond~dormencia, permutaion
############################################
seasb_dortype<-table(fen$seasonb, fen$Dormancy_type)
chisq.test(seasb_dortype, simulate.p.value =T, B=100000) ##chi2 seasonb~clase dormencia, permutaion
seasc_dortype<-table(fen$seasonc, fen$Dormancy_type)
chisq.test(seasc_dortype, simulate.p.value =T, B=100000) ##chi2 seasonc~clase dormencia, permutaion
seasd_dortype<-table(fen$seasond, fen$Dormancy_type)
chisq.test(seasd_dortype, simulate.p.value =T, B=100000) ##chi2 seasond~clase dormencia, permutaion
###########################################
dis_seasb<-table(fen$Dispersal_form, fen$seasonb)
chisq.test(dis_seasb, simulate.p.value = T, B=100000)  ## chi2 forma de dispersao~seasonb
dis_seasc<-table(fen$Dispersal_form, fen$seasonc)
chisq.test(dis_seasc, simulate.p.value = T, B=100000)
dis_seasd<-table(fen$Dispersal_form, fen$seasond)
chisq.test(dis_seasd, simulate.p.value = T, B=100000)


