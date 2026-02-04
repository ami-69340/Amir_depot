install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

read_csv2("joueurs.csv",skip = 1)->JOU
Jou1<- JOU[-26,]
Jou2<-Jou1%>% 
  mutate(ADH=(but+pasD)/(minut/60), #ADH= Action décisives par heure
         TentH=tir/(minut/60), #TentH =tentative par heure
         PctTK=tirK/tir*100    #Pcttk = pourcentage de tirs cadrés
  )

##############Graphique adh pcttk et Tenth#################
#data
Jou2 %>% 
  drop_na %>%
  
  #mapping
  ggplot()+  
  aes(x=ADH,y=PctTK, color = TentH) +
  
  #layers=couche
  geom_point(size= 2) +
  geom_smooth(method = lm) +
  geom_count()+
  geom_text(aes(label = Action), vjust = 1.5, size = 3)+
  
  
  #scales : les axes
  scale_x_continuous()+
  scale_y_continuous()+
  scale_color_gradientn(colors = c("red", "yellow", "green"))+
  
  #Facet
  
  #option 
  ggtitle ("Graphique du nombre d' actions décisives en fonction du pourcentage de tirs cadrés. 
Groupés par nombre de tentative par heure.")+
  #Coordonnées
  
  coord_cartesian() + #ou cooord_fixed 
  
  #Thèmes
  theme_dark()


####################Graphique Adh/ FautSU##############################
Jou2 %>% 
  drop_na %>%
  
  #mapping
  ggplot()+  
  aes(x=fautSU,y=fautCO, color = ADH ) +
  
  #mapping
  geom_point(size= 2) +
  geom_smooth(method = lm) +
  geom_count()+
  geom_text(aes(label = Action), vjust = 1.5, size = 3) +
  
  #scales : les axes
  scale_x_continuous()+
  scale_y_continuous()+
  scale_color_gradientn(colors = c("red", "yellow", "green")) +

  #Options
  ggtitle("Graphique des fautes subies en focntion des fautes commises.
Groupés par le nombre d'actions décisives") +
  
  #Coordonnées
  coord_cartesian() + 
  
  #Thèmes
  theme_dark()
