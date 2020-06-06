# ============ ULTIMA figura (barplots) junho 2020 ================
# figura 3 paper Seeds - diferentes tentativas - Zupo et al.
#=======================================================================

#pacotes
library(plyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#lendo tables
tali <- read.csv(file = "data/last.csv", sep = ";", dec = ".", header = T)
tali$prop<-tali$prop_sp*100
germ <- tali[c(1:9),]
viab <- tali[c(10:18),]

# o único jeito é fazer com germ e viab separado - 2 figuras lado a lado

f0 <- ggplot() +
  geom_bar(data=tali, aes(y = prop, x = trat, fill = fate), stat="identity",
           position='stack') +
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'), labels = c("Decreased", "Stimulated", "Unchanged"))+
  scale_x_discrete(limits=c("100-1","100-3", "200 -1" ),
                   labels=c("100 - 1 min","100 - 3 min", "200 - 1 min" ))+
  xlab("") +
  ylab("Proportion of species") +
  theme_classic() +
   facet_grid( ~ x)
  f0<- f0+labs(fill ="")

  png("figs/figura6x.png", res = 300, width = 1500, height = 800)
  ggarrange(f0,
            common.legend = TRUE, legend = "bottom")
  dev.off()

## o jeito LONGO de fazer essa figura####
###########################################

f1 <-ggplot(data=germ, aes(x=trat, y=prop_sp, fill = fate,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'), labels = c("Decreased", "Stimulated", "Unchanged"))+
  scale_x_discrete(limits=c("100-1","100-3", "200 -1" ),
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
f1<- f1+labs(fill ="")

f2 <- ggplot(data=viab, aes(x=trat, y=prop_sp, fill = fate,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#CCCCCC','#666666', '#333333'),labels = c("Decreased", "Stimulated", "Unchanged"))+
  scale_x_discrete(limits=c("100-1","100-3", "200 -1" ),
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
f2<- f2+labs(fill ="")

png("figs/figura6.png", res = 300, width = 2000, height = 800)
ggarrange(f1, f2, labels = c("a", "b"),label.x = c(0.14, 0.14),
          common.legend = TRUE, legend = "bottom")
dev.off()

#as outras tentativas, furadas, eram: 1 - grouped barplot 2- barplot (germ) + line graph (viab) e 3 - so UM stacked barplot (mas q confundia germ e viab - fail)
