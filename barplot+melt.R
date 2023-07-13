library(reshape2)
library(tidyverse)
setwd("C:/Users/user01/Documents/Devda-Projet/Dr Ndongo/TestSIG")
mydata <- read.csv("AutresCorona.csv",sep = ",", h = T)

mycorona <- melt(mydata,
                            id.vars = "DDP",
                            measure.vars = c("HCOV","COV...SARS2","HCOV.OC43","HCOV.NL63","HCOV.229E","HCOV.HKU1","SARS2","OC43.NL63",
                                             "OC43.229E","OC43.HKUI","NL63.229E","NL63.HKU1","X229E.HKU1","OC43.SARS2",
                                             "X229E.SARS.2","NL63.SARS2","HKU1.SARS2","OC43.NL63.HKU1","OC43.NL63.229E",
                                             "OC43.HKU1.229E","NL63.HKU1.229E","OC43.NL63.SARS2","OC43.229E.SARS2",
                                             "OC43.HKUI.SARS2","NL63.229E.SARS2","NL63.HKU1.SARS2","X229E.HKU1.SARS2",
                                             "OC43.NL63.HKU1.SARS2","OC43.NL63.229E.SARS2","OC43.HKU1.229E.SARS2","NL63.HKU1.229E.SARS2"))
#mycorona$variable[mycorona$variable == "X229E"] = "229E"

mycorona1 <- filter(.data = mycorona,variable %in% c("HCOV.OC43","HCOV.NL63","HCOV.HKU1","HCOV.229E"))
#-----------------------------------------------------#

#data1$SEXE = as.factor(data1$SEXE)
mycorona1$DDP1 = as.Date(mycorona1$DDP,"%d/%m/%Y")
mycorona1$Mois = format(mycorona1$DDP1,"%Y-%m")
mycorona1$Annee = format(mycorona1$DDP1,"%Y")
order(mycorona1$Annee)
order(mycorona1$Mois)
mycorona2 <- mycorona1 %>% dplyr::rename(CoronaVirus = variable)
#________________________________ Debut _____________________________________
rotatedAxisElementText <- function(angle,position='x'){
  
  angle     = angle[1];
  
  position  = position[1]
  
  positions = list(x=0,y=90,top=180,right=270)
  
  if(!position %in% names(positions))
    
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  
  if(!is.numeric(angle))
    
    stop("'angle' must be numeric",call.=FALSE)
  
  rads  = (angle - positions[[ position ]])*pi/180
  
  hjust = 0.5*(1 - sin(rads))
  
  vjust = 0.5*(1 + cos(rads))
  
  element_text(angle=angle,vjust=vjust,hjust=hjust)
  
}
#__________________________________________ Fin__________________________________

#--------------------------------------------------#
mycorona2 <- mycorona1 %>% dplyr::rename(Type_virus = variable)

Mycolor = c("navy","deepskyblue4","dodgerblue3","darkturquoise")
p <- ggplot(mycorona2,
  aes(x = Mois, fill = Type_virus)) +
  geom_bar() +
  xlab("Years") +
  ylab("Types of Corona") +
  labs(fill = "Types of Corona") +
  theme(axis.text.x = rotatedAxisElementText(90,'x'),legend.title = element_blank()) + theme(
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_fill_manual(values = Mycolor) 
p


































