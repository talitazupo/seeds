# ============ Figuras (barplots) fev-março-abril 2020 ================
# planilha e figuras atualizadas do paper Seeds - Zupo et al.
#=======================================================================

#pacotes
library(plyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

## aqui é pra fazer a figura 1 do paper das seeds! fevereiro 2020####
#lendo tables
seeds <- read.csv(file = "data/seeds.csv", sep = ";", dec = ".", header = T)
head(seeds)
class(seeds$estrategia)
class(seeds$prop_ssp)

#separando os dados

strat <- seeds[c(1:3),c(1,3)]
buds <- seeds[c(4:6),c(1,3)]
dorm <- seeds[c(7:8),c(1,3)]
types <- seeds[c(9:11),c(1,3)]
survived <- seeds[c(12:14), c(1,3)]



f1<- ggplot(data=strat, aes(x= estrategia, y=prop_ssp,  width=.4)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+

  xlab("") +
  ylab("Proportion of species") +
  scale_y_continuous(limits = c(0, 100),breaks=0:20*25) +
  theme_classic() +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8),panel.grid.major=element_blank(),   # Pra tirar os grid lines
         panel.grid.minor=element_blank(),panel.border=element_blank()) +
  theme(axis.line.x = element_line(color = 'black', size = 0.5, linetype = 'solid'), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color='black', size = 0.5, linetype= 'solid'))+
  theme(legend.position="none")


f2<- ggplot(data=buds, aes(x= estrategia, y=prop_ssp,  width=.4)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+

  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:20*25) +
  theme_classic() +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8),axis.text.y = element_blank(), panel.grid.major=element_blank(),   # Pra tirar os grid lines
         panel.grid.minor=element_blank(),panel.border=element_blank()) +
  theme(axis.line.x = element_line(color = 'black', size = 0.5, linetype = 'solid'), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color='black', size = 0.5, linetype= 'solid'))+
  theme(legend.position="none")

f3<- ggplot(data=dorm, aes(x= estrategia, y=prop_ssp,  width=.4)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+

  xlab("") +
  ylab("Proportion of species") +
  scale_y_continuous(limits = c(0, 100),breaks=0:20*25) +
  theme_classic() +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8),panel.grid.major=element_blank(),   # Pra tirar os grid lines
         panel.grid.minor=element_blank(),panel.border=element_blank()) +
  theme(axis.line.x = element_line(color = 'black', size = 0.5, linetype = 'solid'), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color='black', size = 0.5, linetype= 'solid'))+
  theme(legend.position="none")

f4<- ggplot(data=types, aes(x= estrategia, y=prop_ssp,  width=.4)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+

  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:20*25) +
  theme_classic() +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8), axis.text.y = element_blank(),panel.grid.major=element_blank(),   # Pra tirar os grid lines
         panel.grid.minor=element_blank(),panel.border=element_blank()) +
  theme(axis.line.x = element_line(color = 'black', size = 0.5, linetype = 'solid'), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color='black', size = 0.5, linetype= 'solid'))+
  theme(legend.position="none")


#salvando todos os graphs em uma figura


png("figs/figura02b.png", res = 300, width = 2400, height = 1200)
grid.arrange(f1, f2, f3, f4, ncol=2)
dev.off()

#para adicionar labels dentro do plot usando vjust and hjust:

png("figs/figura01.png", res = 300, width = 2400, height = 1200)
ggarrange(f1, f2, f3, f4,
          labels = c("a", "b", "c", "d"),vjust =  1.0, hjust = -0.5,
          ncol = 2, nrow = 2)
dev.off()

#agora usando label.x e label.y:
# que é melhor que vjust and hjust - dá pra ajustar cada label onde vc quer!

png("figs/figura03.png", res = 300, width = 2000, height = 1200)
ggarrange(f1, f2, f3, f4,
          labels = c("a", "b", "c", "d"),label.x = c(0.14, 0.09, 0.14, 0.09),
          label.y = 1, ncol = 2, nrow = 2)
dev.off()


## agora pra fazer a figura 2####
#essa figura é a antiga fig 3 e 5 juntas. stacked barplots
# pra isso tenho que fazer 2 dataframes

#primeiro dataframe
buds<-c("Aunderground", "Aunderground", "Aunderground", "Baerial", "Baerial", "Baerial", "Cbasal", "Cbasal","Cbasal")
buds1 <- as.factor(buds)

strats<-c("R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-" )
strat1 <-as.factor(strats)

values <- c('0.8', "0.16", "0.04", "0.89", "0.11", "0", "0.75","0.25", "0")
value1 <-as.numeric(values)
value1 <- value1*100
datax <- data.frame(buds1, strat1, value1)

#segundo dataframe
dorm <- c("Dormant", "Dormant", "Dormant", "Non-dormant", "Non-dormant", "Non-dormant")
dorm1 <- as.factor(dorm)

strats2<-c("R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-")
strat2 <-as.factor(strats2)

val <- c("0.56", "0.43", "0", "0.96", "0", "0.04")
val1 <- as.numeric(val)
val1 <- val1*100

datay <- data.frame(dorm1, strat2, val1)

#terceiro (growth forms, com sub-shrubs e shrubs separados)
gf <- c( "Forb", "Forb", "Forb","Graminoid", "Graminoid", "Graminoid", "Sub-shrub", "Sub-shrub","Sub-shrub", "Shrub", "Shrub", "Shrub")
gf1 <- as.factor(gf)

strats3a<-c("R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-", "R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-")
strat3 <-as.factor(strats3a)

values <-c( "1", "0", "0","0.83", "0.17","0", "1", "0", "0", "0.7", "0.26", "0.04")
val3 <- as.numeric(values)
val3 <- val3*100

dataz <-data.frame(gf1, strat3, val3)

#terceiro (growth forms, com sub-shrubs e shrubs juntos)
gf2<-c( "Forb", "Forb", "Forb","Graminoid", "Graminoid", "Graminoid", "Shrub", "Shrub", "Shrub")
gf2 <- as.factor(gf2)

straty <-c("R+PT", "R+PS", "R+P-", "R+PT", "R+PS", "R+P-","R+PT", "R+PS", "R+P-")
straty <-as.factor(straty)

valuex <- c("1", "0", "0", "0.83", "0.17","0", "0.75", "0.21", "0.04")
val4 <- as.numeric(valuex)
val4 <- val4*100

datazz <-data.frame(gf2, straty, val4)

#as figuras:
g1 <-ggplot(data=datax, aes(x=buds1, y=value1, fill = strat1,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  scale_x_discrete(limits=c("Aunderground","Baerial", "Cbasal" ),
                   labels=c("Underground","Aerial", "Basal" ))+
  xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom")
g1<- g1+labs(fill ="")

g2 <-ggplot(data=datay, aes(x=dorm1, y=val1, fill = strat2,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  xlab("") +
  ylab("") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),axis.text.y = element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom")
g2<-g2+ labs(fill ="")

g2b <-ggplot(data=datay, aes(x=dorm1, y=val1, fill = strat2,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  xlab("") +
  ylab("") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),axis.text.y = element_text(size=8),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom")
g2b<-g2b + labs(fill ="")

g3 <-ggplot(data=dataz, aes(x=gf1, y=val3, fill = strat3,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),axis.text.y = element_text(size=8),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom")
g3<-g3+ labs(fill ="")


g4 <-ggplot(data=datazz, aes(x=gf2, y=val4, fill = straty,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),axis.text.y = element_text(size=8),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom")
g4<-g4+ labs(fill ="")


#e se quiser colocar a legenda?
#https://stackoverflow.com/questions/43220862/how-can-i-move-the-legend-position-with-grid-arrange

#abaixo temos uma opçao de como pegar a legenda de qq ggplot, mas nao precisei usar nesse caso
#primeiro:
#library(gridExtra)
#get_legend<-function(myggplot){
 # tmp <- ggplot_gtable(ggplot_build(myggplot))
  #leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #legend <- tmp$grobs[[leg]]
  #return(legend)
#}

#then get legends from any of the plots
#legend <- g_legend(g2)
#

#para usar labels dentro do plot - usando label.x e label.y
library(ggpubr)

#primeira fig sem a legenda
png("figs/figura04.png", res = 300, width = 2000, height = 800)
ggarrange(g1, g2,
          labels = c("a", "b"),label.x = c(0.14, 0.09), ncol=2, nrow=1)
dev.off()

#segunda fig com a legenda!
png("figs/figura04B.png", res = 300, width = 2000, height = 800)
ggarrange(g1, g2, labels = c("a", "b"),label.x = c(0.14, 0.09),
common.legend = TRUE, legend = "bottom")
dev.off()

#adding growth forms na figura
png("figs/figura04c.png", res = 300, width = 2000, height = 1400)
ggarrange(g1, g2, g3, labels = c("a", "b", "c"),label.x = c(0.14, 0.09, 0.14),
          common.legend = TRUE, legend = "bottom")
dev.off()

png("figs/figura04d.png", res = 300, width = 2000, height = 1400)
ggarrange(g1, g2, g4, labels = c("a", "b", "c"),label.x = c(0.14, 0.09, 0.14),
          common.legend = TRUE, legend = "bottom")
dev.off()

#outro jeito de arrange:
png("figs/figura05.png", res = 300, width = 1100, height = 2000)
ggarrange(g1, g4, g2b, ncol = 1, labels = c("a", "b", "c"),label.x = c(0.14, 0.14, 0.14),common.legend = TRUE, legend = "bottom")
dev.off()
# agora a ultima figura#### a ultima figura ta em outro script: last_fig

survived
g4<- ggplot(data=survived, aes(x= estrategia, y=prop_ssp,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge(), colour="black")+

  xlab("") +
  ylab("Proportion of species") +
  scale_y_continuous(limits = c(0, 100),breaks=0:20*25) +
  theme_classic() +
  theme (axis.text = element_text(size = 8), axis.title=element_text(size=8),
         axis.text.x = element_text(size=8), axis.text.y = element_text(size=8), panel.grid.major=element_blank(),   # Pra tirar os grid lines
         panel.grid.minor=element_blank(),panel.border=element_blank()) +
  theme(axis.line.x = element_line(color = 'black', size = 0.5, linetype = 'solid'), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color='black', size = 0.5, linetype= 'solid'))+
  theme(legend.position="none")

png("figs/figura05.png", res = 300, width = 1200, height = 1000)
grid.arrange(g4)
dev.off()

#agora a ultima figura como stacked barplot####
#primeiro dataframe

exp<-c("100-1 min", "100-1 min", "100-1 min", "100-3 min", "100-3 min", "100-3 min", "200-1 min", "200-1 min","200-1 min")
exp1 <- as.factor(exp)

strats<-c("Stimulated", "Tolerated", "Decreased","Stimulated", "Tolerated", "Decreased","Stimulated", "Tolerated", "Decreased" )
strat2 <-as.factor(strats)

values <- c('0.02', "0.95", "0.02", "0.14", "0.77", "0.09", "0.12","0.34", "0.54")
value2 <-as.numeric(values)
valuex <- value2*100
data2 <- data.frame(exp1, strat2, valuex)


#a figura:

g5 <-ggplot(data=data2, aes(x=exp1, y=valuex, fill = strat2,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'))+
  scale_x_discrete(limits=c("100-1 min","100-3 min", "200-1 min" ),
                   labels=c("100 - 1 min","100 - 3 min", "200 - 1 min" ))+
    xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size=8),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="bottom", legend.text = element_text(size =9), legend.key.size = unit(0.35, "cm"))
g5<- g5+labs(fill ="")

png("figs/figura05B.png", res = 300, width = 1200, height = 1000)
ggarrange(g5,
          common.legend = TRUE, legend = "bottom")
dev.off()
