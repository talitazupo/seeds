#pacotes
library(plyr)
library(ggplot2)
library(gridExtra)

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

#fazer graficos de barra
plot(strat$estrategia, strat$prop_ssp)

f1<- ggplot(data=strat, aes(x= estrategia, y=prop_ssp,  width=.2)) + # width faz a barra ficar mais fina (ou grossa)
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


f2<- ggplot(data=buds, aes(x= estrategia, y=prop_ssp,  width=.2)) + # width faz a barra ficar mais fina (ou grossa)
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

f3<- ggplot(data=dorm, aes(x= estrategia, y=prop_ssp,  width=.2)) + # width faz a barra ficar mais fina (ou grossa)
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

f4<- ggplot(data=types, aes(x= estrategia, y=prop_ssp,  width=.2)) + # width faz a barra ficar mais fina (ou grossa)
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

#para adicionar labels dentro do plot
library(ggpubr)
png("figs/figura01.png", res = 300, width = 2400, height = 1200)
ggarrange(f1, f2, f3, f4,
          labels = c("a", "b", "c", "d"),vjust =  1.0, hjust = -0.5,
          ncol = 2, nrow = 2)
dev.off()

#usando label.x e label.y é melhor que vjust and hjust para ajustar cada label onde vc quer!

png("figs/figura03.png", res = 300, width = 2000, height = 1200)
ggarrange(f1, f2, f3, f4,
          labels = c("a", "b", "c", "d"),label.x = c(0.14, 0.09, 0.14, 0.09),
          label.y = 1, ncol = 2, nrow = 2)
dev.off()

ggarrange?


## agora pra fazer a figura 2####



