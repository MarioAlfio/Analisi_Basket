#Caricamento tabella e dati necessari per i calcoli
data = read.csv("C:/Users/Mario/Desktop/Analisi Basket/12' giornata.csv", sep = ";", 
                header = TRUE, stringsAsFactors = FALSE)
colnames(data)[colnames(data)=="ï..Team"] <- "Team"
head(data)

#Indici squadra:
#Rimbalzi Offensivi giornata
TeOR = sum(data$Rimbalzi.Offensivi)
#Rimbalzi Difensivi giornata
TeDR = sum(data$Rimbalzi.Difensivi)
#Totale Rimbalzi giornata
TeTR = sum(data$Totale.Rimbalzi)
#Minuti totali di giornata
TeMP = sum(data$MP)
#Tiri Tentati giornata
TeFGA = sum(data$Tiri.tentati)
#Tiri da tre giornata
Te3P = sum (data$Tiri.da.3)
#Tiri realizzati giornata
TeFGM = sum(data$Tiri.segnati)
#Offensive Rebound Percentage
data$ORper = ((data$Rimbalzi.Offensivi)/((TeOR+TeDR)*((5*data$MP)/(TeMP))))*100
#Defensive Rebound Percentage
data$DRper = ((data$Rimbalzi.Difensivi)/((TeDR+TeOR)*((5*data$MP)/(TeMP))))*100
#Total Rebound Percentage
data$TRper = ((data$Totale.Rimbalzi)/((TeTR)*((5*data$MP)/(TeMP))))*100
#Possessi
data$Possper = ((data$Tiri.tentati)+(0.44*data$Tiri.liberi.tentati)-((1.07)*((data$Rimbalzi.Offensivi)/(data$Rimbalzi.Offensivi+data$Rimbalzi.Difensivi))*(data$Tiri.tentati-data$Tiri.segnati))+data$Palle.Perse)
#Pace
data$Pace = (data$Possper/data$MP)*40
mean(data$Pace)
Poss = sum(data$Possper)/14
#Palle rubate
data$STper = ((data$Palle.Recuperate)/((Poss)*((5*data$MP)/(TeMP))))*100
#Block Percentage individuale:
data$Blkper = ((data$Stoppate)/((TeFGA-Te3P)*((5*data$MP)/(TeMP))))*100
#Assist Percentage individuale.
data$Astper = ((data$Assist)/((TeFGM*((5*data$MP)/(TeMP)))-(data$Tiri.segnati)))*100
#newdata$Astper=round(newdata$Astper,digits = 3)
#Turnover individuale
data$TOper = ((data$Palle.Perse/data$Possper))*100
#Percentuale effettiva (ovvero Effective Field Goals Percentage) 
#che vuole dare maggior importanza al tiro da 3 punti rispetto a quello da 2 punti:
data$eFGper = ((data$Tiri.da.2+(1.5*data$Tiri.da.3))/(data$Tiri.tentati))
#Percentuale reale (ovvero la True Shooting Percentage) 
#cerca di restituire la frequenza con la quale una squadra (o un giocatore) segni:
data$TSper = ((data$Punti)/(2*(data$Tiri.tentati+(0.44*data$Tiri.liberi.tentati))))*100
#Offensive Rating[OffRtg];
data$OffRtg = ((data$Punti)/(data$Possper))
#Defensive Rating[DefRtg];
data$DefRtg = ((OppPts)/(data$Possper))
#Net Rating
data$NetRtg = (data$OffRtg-data$DefRtg)

newdata = subset(data,select = c(MP,Tiri.segnati,Tiri.tentati,Tiri.da.3,Punti.Panchina,Assist,Punti,ORper,DRper,TRper,Pace,Possper,Astper,STper,Blkper,TOper,eFGper,TSper,OffRtg))

#####################################Grafici##############################################

###Grafico correlazione### 
devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)

# Correlation matrix
corr <- round(cor(newdata), 3)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Analisi Basket", 
           ggtheme=theme_bw)

#confronto Tiri Tentati con Pace
p <- ggplot(data, aes(x=Tiri.tentati, y=Pace, color=Team )) +
  geom_point() + # Show dots
  xlab("\n Tiri tentati") +
  ylab("Pace \n") +
  xlim(60,130)+
  geom_text(
    aes(label = Team), 
    hjust = -0.15)+
   geom_hline(yintercept=mean(data$Pace), color="red")+
   theme(legend.position = "none")
p
p + theme(
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)

#confronto Tiri segnati con Rimbalzi Difensivi, 11 giornata napoli ha raccolti più rimbalzi 
#dif e totalizzato maggior numero di tiri segnati
p1 <- ggplot(data, aes(x=DRper, y=Tiri.segnati, color=Team )) +
  geom_point() + # Show dots
  xlab("\n %Rimbalzi difensivi") +
  ylab("Tiri segnati \n") +
  #xlim(60,130)+
  geom_text(
    aes(label = Team), 
    hjust = -0.15)+
  theme(legend.position = "none")
p1
p1 + theme(
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)

#squadra con maggior frequenza di tiri segnati:
p2 <- ggplot(data, aes(x=Tiri.segnati, y=TSper, color=Team )) +
  geom_point() + # Show dots
  xlab("\n Tiri segnati") +
  ylab(" Percentuale reale\n") +
  #xlim(60,130)+
  geom_text(
    aes(label = Team), 
    hjust = -0.15)+
  theme(legend.position = "none")
p2
p2 + theme(
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)

#Analisi Partite
library(dplyr)
Varese <- filter(newdata, newdata$Team == "Varese")
head(Varese)
Napoli <- filter(newdata, newdata$Team == "Napoli")
head(Napoli)
Partita1 = rbind(Napoli,Varese)
write.table(Partita1, file="PartitaNapoli.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

Venezia <- filter(newdata, newdata$Team == "Venezia")
head(Venezia)
Sassari <- filter(newdata, newdata$Team == "Sassari")
head(Sassari)
Partita1 = rbind(Venezia,Sassari)
write.table(Partita1, file="PartitaVenezia.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

Fortitudo_Bologna <- filter(newdata, newdata$Team == "Fortitudo Bologna")
head(Fortitudo_Bologna)
Trieste <- filter(newdata, newdata$Team == "Trieste")
head(Trieste)
Partita1 = rbind(Fortitudo_Bologna,Trieste)
write.table(Partita1, file="PartitaTrieste.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

Reggio_Emilia <- filter(newdata, newdata$Team == "Reggio Emilia")
head(Reggio_Emilia)
Brindisi <- filter(newdata, newdata$Team == "Brindisi")
head(Brindisi)
Partita1 = rbind(Reggio_Emilia,Brindisi)
write.table(Partita1, file="PartitaBrindisi.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

Treviso <- filter(newdata, newdata$Team == "Treviso")
head(Treviso)
Pesaro <- filter(newdata, newdata$Team == "Pesaro")
head(Pesaro)
Partita1 = rbind(Treviso,Pesaro)
write.table(Partita1, file="PartitaTreviso.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)

Trentino <- filter(newdata, newdata$Team == "Trentino")
head(Trentino)
Tortona <- filter(newdata, newdata$Team == "Tortona")
head(Tortona)
Partita1 = rbind(Trentino,Tortona)
write.table(Partita1, file="PartitaTrentino.csv", quote=F, sep=";", dec=",", na="", row.names=T, col.names=T)



