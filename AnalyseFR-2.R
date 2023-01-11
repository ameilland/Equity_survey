install.packages("ggplot2")
install.packages("readxl")
install.packages("survey")
install.packages("RColorBrewer")
install.packages("questionr", dependencies=TRUE)
install.packages("gmodels")
install.packages("alluvial")
install.packages("srtingr")
install.packages("ggalluvial")
install.packages("ggpattern")


library(ggplot2)
library(readxl)
library(survey)
library(RColorBrewer)
library(questionr)
library(gmodels)
library(alluvial)
library(stringr)
library(ggalluvial)
library(ggpattern)




##Récupération des données

setwd("C:/Users/Auriane Meilland/Documents/2. Projet questionnaire Yann/Projet Questionnaire - Stage CIRED")
QuestionnaireFR <- read.csv2("results-surveyFR.csv",  encoding = "UTF-8")
QuestionnaireFR <- data.frame(QuestionnaireFR)





##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Recodage des données (ordre du questionnaire) 
##_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#réorganisation des niveaux de salaire pour affichage
QuestionnaireFR$Income <- factor(QuestionnaireFR$Income, levels= c("Moins de 20 000 euros", "De 20 001 à 35 000 euros", "De 35 001 à 50 000 euros", "De 50 001 à 75 000 euros", "Plus de 75 000 euros"))

#Age
QuestionnaireFR$Age <- as.factor(QuestionnaireFR$Age)

#Gender
QuestionnaireFR$Gender <- as.factor(QuestionnaireFR$Gender)

#Codage des régions
QuestionnaireFR$Region[QuestionnaireFR$State  =="75" | QuestionnaireFR$State  =="77"| QuestionnaireFR$State  =="78"| QuestionnaireFR$State  =="91"| QuestionnaireFR$State  =="92"| QuestionnaireFR$State  =="93"| QuestionnaireFR$State  =="94"| QuestionnaireFR$State  =="95"]  <- "Ile-de-France"
QuestionnaireFR$Region[QuestionnaireFR$State == "2" | QuestionnaireFR$State == "8" | QuestionnaireFR$State == "10"| QuestionnaireFR$State == "21"| QuestionnaireFR$State == "25"| QuestionnaireFR$State == "39"| QuestionnaireFR$State == "51"| QuestionnaireFR$State == "52"| QuestionnaireFR$State == "54"| QuestionnaireFR$State == "55"| QuestionnaireFR$State == "57"| QuestionnaireFR$State == "58"| QuestionnaireFR$State == "59"| QuestionnaireFR$State == "60"| QuestionnaireFR$State == "62"| QuestionnaireFR$State == "67"| QuestionnaireFR$State == "68"| QuestionnaireFR$State == "70"| QuestionnaireFR$State == "71"| QuestionnaireFR$State == "80"| QuestionnaireFR$State == "88"| QuestionnaireFR$State == "89"| QuestionnaireFR$State == "90"] <- "Northeast"
QuestionnaireFR$Region[QuestionnaireFR$State == "14" | QuestionnaireFR$State == "16" | QuestionnaireFR$State == "17"| QuestionnaireFR$State == "18"| QuestionnaireFR$State == "22"| QuestionnaireFR$State == "27"| QuestionnaireFR$State == "28"| QuestionnaireFR$State == "29"| QuestionnaireFR$State == "35"| QuestionnaireFR$State == "36"| QuestionnaireFR$State == "37"| QuestionnaireFR$State == "41"| QuestionnaireFR$State == "44"| QuestionnaireFR$State == "45"| QuestionnaireFR$State == "49"| QuestionnaireFR$State == "50"| QuestionnaireFR$State == "53"| QuestionnaireFR$State == "56"| QuestionnaireFR$State == "61"| QuestionnaireFR$State == "72"| QuestionnaireFR$State == "76"| QuestionnaireFR$State == "79"| QuestionnaireFR$State == "85"| QuestionnaireFR$State == "86"] <- "Northwest"
QuestionnaireFR$Region[QuestionnaireFR$State == "1" | QuestionnaireFR$State == "3" | QuestionnaireFR$State == "4"| QuestionnaireFR$State == "5"| QuestionnaireFR$State == "6"| QuestionnaireFR$State == "7"| QuestionnaireFR$State == "11"| QuestionnaireFR$State == "13"| QuestionnaireFR$State == "15"| QuestionnaireFR$State == "20"| QuestionnaireFR$State == "26"| QuestionnaireFR$State == "30"| QuestionnaireFR$State == "34"| QuestionnaireFR$State == "38"| QuestionnaireFR$State == "42"| QuestionnaireFR$State == "43"| QuestionnaireFR$State == "48"| QuestionnaireFR$State == "63"| QuestionnaireFR$State == "66"| QuestionnaireFR$State == "69"| QuestionnaireFR$State == "73"| QuestionnaireFR$State == "74"| QuestionnaireFR$State == "83"| QuestionnaireFR$State == "84"]<- "Southeast"
QuestionnaireFR$Region[QuestionnaireFR$State == "9" | QuestionnaireFR$State == "12" | QuestionnaireFR$State == "19"| QuestionnaireFR$State == "23"| QuestionnaireFR$State == "24"| QuestionnaireFR$State == "31"| QuestionnaireFR$State == "32"| QuestionnaireFR$State == "33"| QuestionnaireFR$State == "40"| QuestionnaireFR$State == "46"| QuestionnaireFR$State == "47"| QuestionnaireFR$State == "64"| QuestionnaireFR$State == "65"| QuestionnaireFR$State == "81"| QuestionnaireFR$State == "82"| QuestionnaireFR$State == "87"]<- "Southwest"
QuestionnaireFR$Region <- as.factor(QuestionnaireFR$Region)


#Changement de nom cat.agglo
QuestionnaireFR$Agglo[QuestionnaireFR$Agglo=="J'habite en agglomération parisienne"]<- "Aggl. Paris"
QuestionnaireFR$Agglo[QuestionnaireFR$Agglo=="Moins de 2 000 habitants"]<- "<2000"
QuestionnaireFR$Agglo[QuestionnaireFR$Agglo=="De 2 000 à 19 999 habitants"]<- "2000 - 19999"
QuestionnaireFR$Agglo[QuestionnaireFR$Agglo=="De 20 000 à 99 999 habitants"]<- "20000 - 99999"
QuestionnaireFR$Agglo[QuestionnaireFR$Agglo=="100 000 habitants et plus"]<- ">100000"
QuestionnaireFR$Agglo<-as.factor(QuestionnaireFR$Agglo)



#Réorganisation de la fréquence pour affichage
levels(QuestionnaireFR$Freqtalk)[levels(QuestionnaireFR$Freqtalk)==""] <- "No answer"
QuestionnaireFR$Freqtalk[QuestionnaireFR$Freqtalk=="Plusieurs fois par semaine"]<-"Several times a week"
QuestionnaireFR$Freqtalk[QuestionnaireFR$Freqtalk=="Plusieurs fois par mois"]<-"Several times a month"
QuestionnaireFR$Freqtalk[QuestionnaireFR$Freqtalk=="Plusieurs fois par an"]<-"Several times a year"
QuestionnaireFR$Freqtalk[QuestionnaireFR$Freqtalk=="Presque jamais"]<-"Almost never"
QuestionnaireFR$Freqtalk <- factor(QuestionnaireFR$Freqtalk, levels= c("No answer", "Several times a week", "Several times a month", "Several times a year", "Almost never"))
QuestionnaireFR$Freqtalk[is.na(QuestionnaireFR$Freqtalk)] <- "No answer"

#Changement de nom cause CC pour affichage
QuestionnaireFR$CauseCC[QuestionnaireFR$CauseCC=="n'est pas réel"] <- "Not a reality"
QuestionnaireFR$CauseCC[QuestionnaireFR$CauseCC=="est principalement dû à la variabilité climatique naturelle"] <- "Nature caused"
QuestionnaireFR$CauseCC[QuestionnaireFR$CauseCC=="est principalement dû aux activités humaines"] <- "Human caused"


#Changement de nom des gaz
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ001."] <- "GazCC_CO2"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ002."] <- "GazCC_CH4"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ003."] <- "GazCC_O2"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ004."] <- "GazCC_H2O"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ006."] <- "GazCC_N2O"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GazCC.SQ005."] <- "GazCC_PM"

#Recodage des gaz en connaissance du CC
QuestionnaireFR$Knowledge <- "None"
QuestionnaireFR$Knowledge[QuestionnaireFR$GazCC_CO2=="Oui"]<-"Basic"
QuestionnaireFR$Knowledge[QuestionnaireFR$GazCC_CO2=="Oui"& QuestionnaireFR$GazCC_CH4=="Oui"]<-"Intermediate"
QuestionnaireFR$Knowledge[QuestionnaireFR$GazCC_CO2=="Oui"& QuestionnaireFR$GazCC_CH4=="Oui"& QuestionnaireFR$GazCC_N2O =="Oui"]<-"Advanced"
QuestionnaireFR$Knowledge <- factor(QuestionnaireFR$Knowledge, levels= c("None", "Basic", "Intermediate", "Advanced"))


#Traduction et réorganisation des effets pour affichage
QuestionnaireFR$EffectsCC[QuestionnaireFR$EffectsCC=="Négligeables"]<-"Insignificant"
QuestionnaireFR$EffectsCC[QuestionnaireFR$EffectsCC=="Faibles"]<-"Small"
QuestionnaireFR$EffectsCC[QuestionnaireFR$EffectsCC=="Importants"]<-"Significant"
QuestionnaireFR$EffectsCC[QuestionnaireFR$EffectsCC=="Désastreux"]<-"Disastrous"
QuestionnaireFR$EffectsCC <- factor(QuestionnaireFR$EffectsCC, levels= c("Insignificant", "Small", "Significant", "Disastrous", "No answer"))
QuestionnaireFR$EffectsCC[is.na(QuestionnaireFR$EffectsCC)] <- "No answer"

#Changement de nom Région CC pour affichage
QuestionnaireFR$RegionCC[QuestionnaireFR$RegionCC=="Les Etats-Unis"] <- "USA"
QuestionnaireFR$RegionCC[QuestionnaireFR$RegionCC=="Similaires dans les deux régions"] <- "Both"
QuestionnaireFR$RegionCC[QuestionnaireFR$RegionCC=="L’Union européenne"] <- "EU"


#Changement de nom Générations
names(QuestionnaireFR)[names(QuestionnaireFR) == "GenerationCC.SQ001."] <- "Born_1960s"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GenerationCC.SQ002."] <- "Born_1990s"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GenerationCC.SQ003."] <- "Born_2020s"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GenerationCC.SQ004."] <- "Born_2050s"
names(QuestionnaireFR)[names(QuestionnaireFR) == "GenerationCC.SQ005."] <- "None"

#Recodage première génération touchée
QuestionnaireFR$FirstGen[QuestionnaireFR$Born_1960s=="Oui"]<-"1960s"
QuestionnaireFR$FirstGen[QuestionnaireFR$Born_1960s=="Non"& QuestionnaireFR$Born_1990s=="Oui"]<-"1990s"
QuestionnaireFR$FirstGen[QuestionnaireFR$Born_1960s=="Non"& QuestionnaireFR$Born_1990s=="Non"& QuestionnaireFR$Born_2020s =="Oui"]<-"2020s"
QuestionnaireFR$FirstGen[QuestionnaireFR$Born_1960s=="Non"& QuestionnaireFR$Born_1990s=="Non"& QuestionnaireFR$Born_2020s =="Non"& QuestionnaireFR$Born_2050s=="Oui"]<-"2050s"
QuestionnaireFR$FirstGen[QuestionnaireFR$None=="Oui"]<-"None of the above"




#Changement de nom VisionCoop pour affichage
QuestionnaireFR$VisioncoopCC[QuestionnaireFR$VisioncoopCC=="Les principes de justice climatique ne devraient jamais passer avant les intérêts nationaux, même si cela peut entraver l’action contre le changement climatique"]<-"National Scale"
QuestionnaireFR$VisioncoopCC[QuestionnaireFR$VisioncoopCC=="Les principes de justice climatique devraient être déterminés au niveau international, même si cela peut empêcher les pays de défendre les intérêts de leurs populations"]<- "International Scale"
QuestionnaireFR$VisioncoopCC<-as.factor(QuestionnaireFR$VisioncoopCC)


#Changement de nom CBDR pour affichage
QuestionnaireFR$CBDR[QuestionnaireFR$CBDR=="Les pays ont des responsabilités communes mais différenciées. Mais s'il fallait choisir l'un ou l'autre, je considèrerais qu'ils ont plutôt des responsabilités communes, c'est à dire que tous les pays, sans exception, devraient agir pour lutter contre le changement climatique."]<-"Common"
QuestionnaireFR$CBDR[QuestionnaireFR$CBDR=="Les pays ont des responsabilités communes mais différenciées. Mais s'il fallait choisir l'un ou l'autre, je considèrerais qu'ils ont plutôt des responsabilités différenciées, c'est à dire que certains pays doivent faire nettement plus d'efforts que d'autres pour lutter contre le changement climatique."]<-"Differentiated"
QuestionnaireFR$CBDR[QuestionnaireFR$CBDR=="Ni l'un ni l'autre"]<-"None of the above"



#réorganisation des étiquettes "rather agree"
QuestionnaireFR$Gdfr <- factor(QuestionnaireFR$Gdfr, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))




#réorganisation et changement de nom des principes
names(QuestionnaireFR)[names(QuestionnaireFR) == "Principles.SQ001."] <- "Principles_past_em"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Principles.SQ002."] <- "Principles_epc"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Principles.SQ003."] <- "Principles_gdfr"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Principles.SQ004."] <- "Principles_poor"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Principles.SQ005."] <- "Principles_cost"
QuestionnaireFR$Principles_past_em <- factor(QuestionnaireFR$Principles_past_em, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Principles_epc <- factor(QuestionnaireFR$Principles_epc, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Principles_gdfr <- factor(QuestionnaireFR$Principles_gdfr, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Principles_poor <- factor(QuestionnaireFR$Principles_poor, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Principles_cost <- factor(QuestionnaireFR$Principles_cost, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))


#Tolerance of people
QuestionnaireFR$Tolerance<-"Not particularly tolerant"
QuestionnaireFR$Tolerance[(QuestionnaireFR$Principles_epc=="Tout à fait d'accord" | QuestionnaireFR$Principles_epc=="Plutôt d'accord") & (QuestionnaireFR$Principles_cost=="Tout à fait d'accord" | QuestionnaireFR$Principles_cost=="Plutôt d'accord")&(QuestionnaireFR$Principles_gdfr=="Tout à fait d'accord" | QuestionnaireFR$Principles_gdfr=="Plutôt d'accord")&(QuestionnaireFR$Principles_poor=="Tout à fait d'accord" | QuestionnaireFR$Principles_poor=="Plutôt d'accord")&(QuestionnaireFR$Principles_past_em=="Tout à fait d'accord" | QuestionnaireFR$Principles_past_em=="Plutôt d'accord")] <- "Tolerant"
QuestionnaireFR$Tolerance[(QuestionnaireFR$Principles_epc=="Pas du tout d'accord" | QuestionnaireFR$Principles_epc=="Plutôt pas d'accord") & (QuestionnaireFR$Principles_cost=="Pas du tout d'accord" | QuestionnaireFR$Principles_cost=="Plutôt pas d'accord")&(QuestionnaireFR$Principles_gdfr=="Pas du tout d'accord" | QuestionnaireFR$Principles_gdfr=="Plutôt pas d'accord")&(QuestionnaireFR$Principles_poor=="Pas du tout d'accord" | QuestionnaireFR$Principles_poor=="Plutôt pas d'accord")&(QuestionnaireFR$Principles_past_em=="Pas du tout d'accord" | QuestionnaireFR$Principles_past_em=="Plutôt pas d'accord")] <- "Intolerant"

#réorganisation et changement de nom des statements
names(QuestionnaireFR)[names(QuestionnaireFR) == "Statements.SQ001."] <- "Statements_forests"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Statements.SQ002."] <- "Statements_trigger"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Statements.SQ003."] <- "Statements_funding"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Statements.SQ004."] <- "Statements_tech"
QuestionnaireFR$Statements_forests <- factor(QuestionnaireFR$Statements_forests, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Statements_trigger <- factor(QuestionnaireFR$Statements_trigger, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Statements_funding <- factor(QuestionnaireFR$Statements_funding, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Statements_tech <- factor(QuestionnaireFR$Statements_tech, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))




#Changement de nom de la question empreinte C vs émissions territoriales
QuestionnaireFR$Accountability[QuestionnaireFR$Accountability=="Les pays devraient être tenus responsables uniquement des émissions de carbone ayant lieu sur leur territoire"] <- "Territory"
QuestionnaireFR$Accountability[QuestionnaireFR$Accountability=="Les pays devraient être tenus responsables des émissions de carbone associées à tous les produits qu’ils consomment, y compris les émissions ayant lieu à l’étranger des produits qu’ils consomment"] <- "Footprint"


#Réorganisation et changement de nom de l'historique
QuestionnaireFR$History[QuestionnaireFR$History=="Les pays devraient être tenus responsables uniquement de leurs émissions actuelles"] <- "Current emissions"
QuestionnaireFR$History[QuestionnaireFR$History=="Les pays devraient être tenus responsables de leurs émissions passées depuis 1990 (année à partir de laquelle il était scientifiquement établi que le changement climatique est réel et causé par les activités humaines)"] <- "Since 1990"
QuestionnaireFR$History[QuestionnaireFR$History=="Les pays doivent être tenus responsables de leurs émissions passées depuis 1850 (année à partir de laquelle les sociétés ont commencé à émettre de grandes quantités de carbone dans l’atmosphère)"] <- "Since 1850"
QuestionnaireFR$History <- factor(QuestionnaireFR$History, levels= c("Current emissions", "Since 1990", "Since 1850"))



#Recodage des tests
QuestionnaireFR$CaseComprehension[QuestionnaireFR$Test.SQ001.=="Faux" & QuestionnaireFR$Test.SQ002.=="Vrai" & QuestionnaireFR$Test.SQ003.=="Faux" & QuestionnaireFR$Test.SQ004.=="Vrai"]<- "Perfect Comprehension"
QuestionnaireFR$CaseComprehension[QuestionnaireFR$Test.SQ001.=="Faux" & QuestionnaireFR$Test.SQ002.=="Vrai" & QuestionnaireFR$Test.SQ003.=="Vrai" & QuestionnaireFR$Test.SQ004.=="Vrai"]<- "Misread climate change"
QuestionnaireFR$CaseComprehension[QuestionnaireFR$Test.SQ001.=="Vrai" | QuestionnaireFR$Test.SQ002.=="Faux" | QuestionnaireFR$Test.SQ004.=="Faux"]<- "One or several errors"
QuestionnaireFR$CaseComprehension <- as.factor(QuestionnaireFR$CaseComprehension)




#Réorganisation des Cas Parsticuliers
QuestionnaireFR$Case1.SQ001. <- factor(QuestionnaireFR$Case1.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case1.SQ002. <- factor(QuestionnaireFR$Case1.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case2.SQ001. <- factor(QuestionnaireFR$Case2.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case2.SQ002. <- factor(QuestionnaireFR$Case2.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case3.SQ001. <- factor(QuestionnaireFR$Case3.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case3.SQ002. <- factor(QuestionnaireFR$Case3.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case4.SQ001. <- factor(QuestionnaireFR$Case4.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case4.SQ002. <- factor(QuestionnaireFR$Case4.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case5.SQ001. <- factor(QuestionnaireFR$Case5.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case5.SQ002. <- factor(QuestionnaireFR$Case5.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case6.SQ001. <- factor(QuestionnaireFR$Case6.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Case6.SQ002. <- factor(QuestionnaireFR$Case6.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))


#Réorganisation et renommage coordination
names(QuestionnaireFR)[names(QuestionnaireFR) == "Coordination.SQ001."] <- "Coord_China"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Coordination.SQ002."] <- "Coord_India"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Coordination.SQ003."] <- "Coord_EU"
names(QuestionnaireFR)[names(QuestionnaireFR) == "Coordination.SQ004."] <- "Coord_USA"
QuestionnaireFR$Coord_China <- factor(QuestionnaireFR$Coord_China, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Coord_India <- factor(QuestionnaireFR$Coord_India, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Coord_EU <- factor(QuestionnaireFR$Coord_EU, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Coord_USA <- factor(QuestionnaireFR$Coord_USA, levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))


#Judgement Paths
QuestionnaireFR$Path[QuestionnaireFR$Coord_China=="Tout à fait d'accord"]<-"a"
QuestionnaireFR$Path[QuestionnaireFR$Coord_China=="Plutôt d'accord"]<-"b"
QuestionnaireFR$Path[QuestionnaireFR$Coord_China=="Plutôt pas d'accord"]<-"c"
QuestionnaireFR$Path[QuestionnaireFR$Coord_China=="Pas du tout d'accord"]<-"d"

QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Tout à fait d'accord"]<- paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Tout à fait d'accord"], "a")
QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Plutôt d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Plutôt d'accord"], "b")
QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Plutôt pas d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Plutôt pas d'accord"], "c")
QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Pas du tout d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_India=="Pas du tout d'accord"], "d")

QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Tout à fait d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Tout à fait d'accord"], "a")
QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Plutôt d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Plutôt d'accord"], "b")
QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Plutôt pas d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Plutôt pas d'accord"], "c")
QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Pas du tout d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_EU=="Pas du tout d'accord"], "d")

QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Tout à fait d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Tout à fait d'accord"], "a")
QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Plutôt d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Plutôt d'accord"], "b")
QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Plutôt pas d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Plutôt pas d'accord"], "c")
QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Pas du tout d'accord"]<-paste(QuestionnaireFR$Path[QuestionnaireFR$Coord_USA=="Pas du tout d'accord"], "d")


#Paths categorization
QuestionnaireFR$Pathcat<-"Agree with two"
QuestionnaireFR$Pathcat[str_count(QuestionnaireFR$Path, "c")==0 & str_count(QuestionnaireFR$Path, "d")==0] <- "Agree with all"
QuestionnaireFR$Pathcat[str_count(QuestionnaireFR$Path, "a")==0 & str_count(QuestionnaireFR$Path, "b")==0] <- "Agree with none"
QuestionnaireFR$Pathcat[(str_count(QuestionnaireFR$Path, "a")==1 & str_count(QuestionnaireFR$Path, "b")==0) | (str_count(QuestionnaireFR$Path, "a")==0 & str_count(QuestionnaireFR$Path, "b")==1)] <- "Agree with one"
QuestionnaireFR$Pathcat[(str_count(QuestionnaireFR$Path, "c")==1 & str_count(QuestionnaireFR$Path, "d")==0) | (str_count(QuestionnaireFR$Path, "c")==0 & str_count(QuestionnaireFR$Path, "d")==1)] <- "Agree with three"


#réorganisation reward
QuestionnaireFR$Reward.SQ001. <- factor(QuestionnaireFR$Reward.SQ001., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))
QuestionnaireFR$Reward.SQ002. <- factor(QuestionnaireFR$Reward.SQ002., levels= c("Tout à fait d'accord", "Plutôt d'accord", "Plutôt pas d'accord", "Pas du tout d'accord"))


#réorganisation chances
QuestionnaireFR$Chances <- factor(QuestionnaireFR$Chances, levels= c("moins de 1%", "entre 1 et 10%", "entre 11 et 49%", "entre 50 et 90%","entre 91 et 99%","plus de 99%"))


#Changed category
QuestionnaireFR$Changecat<-"other"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Tout à fait d'accord" & QuestionnaireFR$Reward.SQ001.=="Tout à fait d'accord"]<-"1"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ001.=="Plutôt d'accord"]<-"2"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Tout à fait d'accord" & QuestionnaireFR$Reward.SQ001.=="Plutôt d'accord"]<-"3"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ001.=="Tout à fait d'accord"]<-"4"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Plutôt pas d'accord" & QuestionnaireFR$Reward.SQ001.=="Plutôt pas d'accord"]<-"5"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Plutôt pas d'accord" & QuestionnaireFR$Reward.SQ001.=="Plutôt d'accord"]<-"6"
QuestionnaireFR$Changecat[QuestionnaireFR$Coord_EU=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ001.=="Plutôt pas d'accord"]<-"7"


QuestionnaireFR$Changecat2<-"other"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Tout à fait d'accord" & QuestionnaireFR$Reward.SQ002.=="Tout à fait d'accord"]<-"1"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ002.=="Plutôt d'accord"]<-"2"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Tout à fait d'accord" & QuestionnaireFR$Reward.SQ002.=="Plutôt d'accord"]<-"3"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ002.=="Tout à fait d'accord"]<-"4"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Plutôt pas d'accord" & QuestionnaireFR$Reward.SQ002.=="Plutôt pas d'accord"]<-"5"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Plutôt pas d'accord" & QuestionnaireFR$Reward.SQ002.=="Plutôt d'accord"]<-"6"
QuestionnaireFR$Changecat2[QuestionnaireFR$Coord_USA=="Plutôt d'accord" & QuestionnaireFR$Reward.SQ002.=="Plutôt pas d'accord"]<-"7"

#Coordination 
QuestionnaireFR$Coord<-"Changed their mind"
QuestionnaireFR$Coord[QuestionnaireFR$Coord_USA==QuestionnaireFR$Reward.SQ002. & QuestionnaireFR$Coord_EU==QuestionnaireFR$Reward.SQ001.] <- "Same opinion"


#CSP
QuestionnaireFR$CSP[QuestionnaireFR$CSP =="Agriculteur"| QuestionnaireFR$CSP =="Artisan/Commerçant"| QuestionnaireFR$CSP =="Chef d'entreprise (10 employés ou plus)"| QuestionnaireFR$CSP =="Cadre supérieur, profession libérale (médecin, dentiste, avocat etc)"| QuestionnaireFR$CSP =="Cadre, enseignant, profession intermédiaire"]<-"CSP+"
QuestionnaireFR$CSP[QuestionnaireFR$CSP=="Employé (bureau, commerces, services, santé...)"| QuestionnaireFR$CSP =="Ouvrier"]<-"CSP-"
QuestionnaireFR$CSP[QuestionnaireFR$CSP=="Autre (sans emploi, homme ou femme au foyer...)"| QuestionnaireFR$CSP =="Etudiant"| QuestionnaireFR$CSP =="Retraité"]<-"Inactive"




##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Spécification du design (défaut)
##__________________________________________________________________________________________________________________________________________________________________________________________________

design_FR  <-svydesign(ids=~1, data=QuestionnaireFR)



##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Stats de base
##__________________________________________________________________________________________________________________________________________________________________________________________________


## Données répondant
##_______________________



#Genre
gender <-svymean(~Gender, design=design_FR)

#Age
age <-svymean(~Age, design=design_FR)

#Income
income <-svymean(~Income, design=design_FR)

#State et region
state <-svymean(~State, design=design_FR)
region <-svymean(~Region, design=design_FR)

#Agglo
agglo <-svymean(~Agglo, design=design_FR)


### Check for representativeness
check_income <- data.frame(summary(QuestionnaireFR$Income))
check_income$rpz <- c(240,280,230,150,100)
chisq.test(check_income)


check_gender <- data.frame(summary(QuestionnaireFR$Gender))
check_gender$rpz <- c(500,500)
chisq.test(check_gender)


check_age <- data.frame(summary(QuestionnaireFR$Age))
check_age$rpz <- c(110,160,170,170,160,220)
chisq.test(check_age)


check_region <- data.frame(summary(QuestionnaireFR$Region))
check_region$rpz <- c(190,230,230,240,110)
chisq.test(check_region)


check_agglo <- data.frame(summary(QuestionnaireFR$Agglo))
check_agglo$rpz <- c(217,309,172,139,157)
chisq.test(check_agglo)





## Avis et convictions
##_______________________



#Freqtalk
freqtalk <- data.frame(svymean(~Freqtalk, design=design_FR))
freqtalk2 <- data.frame(confint(svymean(~Freqtalk, design=design_FR), level=0.95))
freqtalk$name <-factor (rownames(freqtalk), levels= c("FreqtalkNo answer", "FreqtalkSeveral times a week", "FreqtalkSeveral times a month", "FreqtalkSeveral times a year", "FreqtalkAlmost never"))
graph_freqtalk <- ggplot(freqtalk,aes(x=name,y=mean, fill=name))
graph_freqtalk + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=freqtalk2$X2.5.. , ymax=freqtalk2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Blues")+ theme(legend.position = "none") + scale_x_discrete(labels=c("FreqtalkNo answer"="No answer","FreqtalkSeveral times a week"="Several times \na week","FreqtalkSeveral times a month"=" Several times \na month","FreqtalkSeveral times a year"=" Several times \na year","FreqtalkAlmost never"="Almost Never")) + coord_cartesian(ylim=c(0,0.42))



#Cause CC
causeCC <- data.frame(svymean(~CauseCC, design=design_FR))
causeCC2 <- data.frame(confint(svymean(~CauseCC, design=design_FR)))
causeCC$name <-factor (rownames(causeCC), levels= c("CauseCC", "CauseCCHuman caused", "CauseCCNature caused", "CauseCCNot a reality"))
graph_causeCC <- ggplot(causeCC,aes(x=name,y=mean, fill=name))
graph_causeCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=causeCC2$X2.5.., ymax=causeCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CauseCC"="No answer", "CauseCCHuman caused"="Human activity", "CauseCCNature caused"="Natural variability", "CauseCCNot a reality"= "Is not a reality")) + coord_cartesian(ylim=c(0,0.8))


#Gaz CC
gazcc_CO2 <- data.frame(svymean(~GazCC_CO2, design=design_FR))
gazcc_CO2$ci<-data.frame(confint(svymean(~GazCC_CO2, design=design_FR)))$X2.5..
gazcc_CO2$cs<-data.frame(confint(svymean(~GazCC_CO2, design=design_FR)))$X97.5..
gazcc_CH4 <-data.frame(svymean(~GazCC_CH4, design=design_FR))
gazcc_CH4$ci<-data.frame(confint(svymean(~GazCC_CH4, design=design_FR)))$X2.5..
gazcc_CH4$cs<-data.frame(confint(svymean(~GazCC_CH4, design=design_FR)))$X97.5..
gazcc_O2 <-data.frame(svymean(~GazCC_O2, design=design_FR))
gazcc_O2$ci<-data.frame(confint(svymean(~GazCC_O2, design=design_FR)))$X2.5..
gazcc_O2$cs<-data.frame(confint(svymean(~GazCC_O2, design=design_FR)))$X97.5..
gazcc_H2O <-data.frame(svymean(~GazCC_H2O, design=design_FR))
gazcc_H2O$ci<-data.frame(confint(svymean(~GazCC_H2O, design=design_FR)))$X2.5..
gazcc_H2O$cs<-data.frame(confint(svymean(~GazCC_H2O, design=design_FR)))$X97.5..
gazcc_N2O <-data.frame(svymean(~GazCC_N2O, design=design_FR))
gazcc_N2O$ci<-data.frame(confint(svymean(~GazCC_N2O, design=design_FR)))$X2.5..
gazcc_N2O$cs<-data.frame(confint(svymean(~GazCC_N2O, design=design_FR)))$X97.5..
gazcc_PM <-data.frame(svymean(~GazCC_PM, design=design_FR))
gazcc_PM$ci<-data.frame(confint(svymean(~GazCC_PM, design=design_FR)))$X2.5..
gazcc_PM$cs<-data.frame(confint(svymean(~GazCC_PM, design=design_FR)))$X97.5..

gazcc_CO2$gaz <- c("CO2")
gazcc_CH4$gaz <- c("CH4")
gazcc_O2$gaz <- c("O2")
gazcc_H2O$gaz <- c("H2O")
gazcc_N2O$gaz <-c("N2O")
gazcc_PM$gaz <-c("PM")
gazcc <- rbind.data.frame(gazcc_CO2,gazcc_CH4,gazcc_O2,gazcc_H2O,gazcc_N2O,gazcc_PM)
gazcc$answer<- c("No", "Yes")
gazcc2<- subset(gazcc, gazcc$answer=="Yes")


graph_gazcc  <- ggplot(gazcc2,aes(x=gaz,y=mean, fill=gaz))
graph_gazcc + geom_bar(position=position_dodge(),stat="identity",color="black")+ labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2") + theme(legend.position = "none")+ coord_cartesian(ylim=c(0,1))


#Knowledge CC
knowledgeCC <- data.frame(svymean(~Knowledge, design=design_FR))
knowledgeCC2<- data.frame(confint(svymean(~Knowledge, design=design_FR)))
knowledgeCC$name <-factor (rownames(knowledgeCC), levels= c("KnowledgeNone", "KnowledgeBasic", "KnowledgeIntermediate", "KnowledgeAdvanced"))
graph_knowledgeCC <- ggplot(knowledgeCC,aes(x=name,y=mean, fill=name))
graph_knowledgeCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=knowledgeCC2$X2.5.., ymax=knowledgeCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("KnowledgeNone"= "None", "KnowledgeBasic"= "Basic", "KnowledgeIntermediate"= "Intermediate", "KnowledgeAdvanced"="Advanced")) + coord_cartesian(ylim=c(0,0.4))



#Effects CC
effectsCC <-data.frame(svymean(~EffectsCC, design=design_FR))
effectsCC2<-data.frame(confint(svymean(~EffectsCC, design=design_FR)))
effectsCC$name <-factor (rownames(effectsCC), levels= c("EffectsCCInsignificant", "EffectsCCSmall", "EffectsCCSignificant", "EffectsCCDisastrous", "EffectsCCNo answer"))
graph_effectsCC <- ggplot(effectsCC,aes(x=name,y=mean, fill=name))
graph_effectsCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=effectsCC2$X2.5.., ymax=effectsCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("EffectsCCInsignificant"="Insignificant", "EffectsCCSmall"="Small", "EffectsCCSignificant"="Significant", "EffectsCCDisastrous"="Disastrous", "EffectsCCNo answer"="No answer")) + coord_cartesian(ylim=c(0,0.6))


#Effects CC by age 
CrossTable(QuestionnaireFR$EffectsCC,QuestionnaireFR$Age, chisq=TRUE)
#p=0.013

effects_age <-svyby(~EffectsCC,by=~Age,design=design_FR,FUN=svymean,vartype=c('ci'))
graph_effect_age <- ggplot(effects_age,aes(x=effects_age$Age,y=effects_age$EffectsCCDisastrous, fill=effects_age$Age))
graph_effect_age + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who think climate change will be disastrous (FR)", x="Age") + geom_errorbar(aes(ymin=effects_age$ci_l.EffectsCCDisastrous, ymax=effects_age$ci_u.EffectsCCDisastrous), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + scale_x_discrete(labels=c("18-24 ans "="18-24", "25-34 ans"="25-34", "35-44 ans"="35-44", "45-54 ans"="45-54", "55-64 ans"="55-64", "Plus de 65 ans"="65+")) + coord_cartesian(ylim=c(0,0.7))

graph_effect_age2 <- ggplot(effects_age,aes(x=effects_age$Age,y=effects_age$EffectsCCSmall, fill=effects_age$Age))
graph_effect_age2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who think climate change will be small (FR)", x="Age") + geom_errorbar(aes(ymin=effects_age$ci_l.EffectsCCSmall, ymax=effects_age$ci_u.EffectsCCSmall), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + scale_x_discrete(labels=c("18-24 ans "="18-24", "25-34 ans"="25-34", "35-44 ans"="35-44", "45-54 ans"="45-54", "55-64 ans"="55-64", "Plus de 65 ans"="65+")) + coord_cartesian(ylim=c(0,0.7))



#RegionCC
regionCC <-data.frame(svymean(~RegionCC, design=design_FR))
regionCC2<-data.frame(confint(svymean(~RegionCC, design=design_FR)))
regionCC$name <-factor (rownames(regionCC), levels= c("RegionCCBoth", "RegionCCEU", "RegionCCUSA"))
graph_regionCC <- ggplot(regionCC,aes(x=name,y=mean, fill=name))
graph_regionCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=regionCC2$X2.5.., ymax=regionCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("RegionCCBoth"="Similar consequences in both regions", "RegionCCEU"="The European Union", "RegionCCUSA"="The USA")) + coord_cartesian(ylim=c(0,0.8))


#Generation CC

generationCC <- data.frame(svymean(~FirstGen, design=design_FR))
generationCC2 <- data.frame(confint(svymean(~FirstGen, design=design_FR)))
generationCC$name <-factor (rownames(generationCC), levels= c("FirstGen1960s", "FirstGen1990s", "FirstGen2020s", "FirstGen2050s", "FirstGenNone of the above"))
graph_generationCC <- ggplot(generationCC,aes(x=name,y=mean, fill=name))
graph_generationCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=generationCC2$X2.5.., ymax=generationCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("FirstGen1960s"="1960s", "FirstGen1990s"="1990s", "FirstGen2020s"="2020s", "FirstGen2050s"="2050s", "FirstGenNone of the above"="None")) + coord_cartesian(ylim=c(0,0.4))


#generation by age
CrossTable(QuestionnaireFR$FirstGen,QuestionnaireFR$Age, chisq=TRUE)
#p=0.22


#VisioncoopCC
visionCC <-data.frame(svymean(~VisioncoopCC, design=design_FR))
visionCC2 <- data.frame(confint(svymean(~VisioncoopCC, design=design_FR)))
visionCC$name <-factor (rownames(visionCC), levels= c("VisioncoopCC", "VisioncoopCCInternational Scale", "VisioncoopCCNational Scale"))
graph_visionCC <- ggplot(visionCC,aes(x=name,y=mean, fill=name))
graph_visionCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=visionCC2$X2.5.., ymax=visionCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("VisioncoopCC"="No answer", "VisioncoopCCInternational Scale"="Cosmopolitan", "VisioncoopCCNational Scale"="Internationalist")) + coord_cartesian(ylim=c(0,0.8))


#Determinants vision coop
CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Age, chisq=TRUE)

CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Income, chisq=TRUE)

CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Gender, chisq=TRUE)

CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$CSP, chisq=TRUE)

CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Region, chisq=TRUE)

CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Freqtalk, chisq=TRUE)
#p dans les 10-7
vision_frq <-svyby(~VisioncoopCC,by=~Freqtalk,design=design_FR,FUN=svymean,vartype=c('ci'))
graph_vision_frq <- ggplot(vision_frq,aes(x=vision_frq$Freqtalk,y=vision_frq$`VisioncoopCCInternational Scale`, fill=vision_frq$Freqtalk))
graph_vision_frq + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who favour an international vision (FR)", x="Frequency with wich respondents talk about CC") + geom_errorbar(aes(ymin=vision_frq$`ci_l.VisioncoopCCInternational Scale`, ymax=vision_frq$`ci_u.VisioncoopCCInternational Scale`), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + coord_cartesian(ylim=c(0,0.99))


CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-22
vision_effects <-svyby(~VisioncoopCC,by=~EffectsCC,design=design_FR,FUN=svymean,vartype=c('ci'))
graph_vision_effects <- ggplot(vision_effects,aes(x=vision_effects$EffectsCC,y=vision_effects$`VisioncoopCCInternational Scale`, fill=vision_effects$EffectsCC))
graph_vision_effects + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who favour an international vision (FR)", x="Perception of the gravity of climate change") + geom_errorbar(aes(ymin=vision_effects$`ci_l.VisioncoopCCInternational Scale`, ymax=vision_effects$`ci_u.VisioncoopCCInternational Scale`), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Reds") + coord_cartesian(ylim=c(0,0.99))



## Principes
##_______________________

#CBDR
cbdr <-data.frame(svymean(~CBDR, design=design_FR))
cbdr2<-data.frame(confint(svymean(~CBDR, design=design_FR)))
cbdr$name <-factor (rownames(cbdr), levels= c("CBDRCommon", "CBDRDifferentiated", "CBDRNone of the above"))
graph_cbdr <- ggplot(cbdr,aes(x=name,y=mean, fill=name))
graph_cbdr + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=cbdr2$X2.5.., ymax=cbdr2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CBDRCommon"="Common", "CBDRDifferentiated"="Differentiated", "CBDRNone of the above"="None of the two")) + coord_cartesian(ylim=c(0,0.6))

#Determinants CBDR
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Age, chisq=TRUE)

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Income, chisq=TRUE)

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Gender, chisq=TRUE)

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$CSP, chisq=TRUE)
#P=0.016
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Region, chisq=TRUE)

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Freqtalk, chisq=TRUE)
#p dans les 10-13

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-15

CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$VisioncoopCC, chisq=TRUE)
#P dans les 10-8


#Grandfathering
gdfr <-data.frame(svymean(~Gdfr, design=design_FR))
gdfr2 <- data.frame(confint(svymean(~Gdfr, design=design_FR)))
gdfr$name <-factor (rownames(gdfr), levels= c("GdfrTout à fait d'accord", "GdfrPlutôt d'accord", "GdfrPlutôt pas d'accord", "GdfrPas du tout d'accord"))
graph_gdfr <- ggplot(gdfr,aes(x=name,y=mean, fill=name))
graph_gdfr + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=gdfr2$X2.5.., ymax=gdfr2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("GdfrTout à fait d'accord"="I strongly agree", "GdfrPlutôt d'accord"="I rather agree", "GdfrPlutôt pas d'accord"="I rather disagree", "GdfrPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))

#Determinants Grandfathering
CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Age, chisq=TRUE)
#p dans les 10-10

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Income, chisq=TRUE)

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Gender, chisq=TRUE)

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$CSP, chisq=TRUE)
#P dans les 10-4

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Region, chisq=TRUE)

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Freqtalk, chisq=TRUE)
#p dans les 10-3

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-11

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$VisioncoopCC, chisq=TRUE)
#P dans les 10-5

CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$CBDR, chisq=TRUE)
#P dans les 10-4



#Principle 1 : past emissions
pcp1 <-data.frame(svymean(~Principles_past_em, design=design_FR))
pcp12 <-data.frame(confint(svymean(~Principles_past_em, design=design_FR)))
pcp1$name <-factor (rownames(pcp1), levels= c("Principles_past_emTout à fait d'accord", "Principles_past_emPlutôt d'accord", "Principles_past_emPlutôt pas d'accord", "Principles_past_emPas du tout d'accord"))
graph_pcp1 <- ggplot(pcp1,aes(x=name,y=mean, fill=name))
graph_pcp1 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=pcp12$X2.5.., ymax=pcp12$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_past_emTout à fait d'accord"="I strongly agree", "Principles_past_emPlutôt d'accord"="I rather agree", "Principles_past_emPlutôt pas d'accord"="I rather disagree", "Principles_past_emPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Principle 2 : epc
pcp2 <-data.frame(svymean(~Principles_epc, design=design_FR))
pcp22 <-data.frame(confint(svymean(~Principles_epc, design=design_FR)))
pcp2$name <-factor (rownames(pcp2), levels= c("Principles_epcTout à fait d'accord", "Principles_epcPlutôt d'accord", "Principles_epcPlutôt pas d'accord", "Principles_epcPas du tout d'accord"))
graph_pcp2 <- ggplot(pcp2,aes(x=name,y=mean, fill=name))
graph_pcp2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=pcp22$X2.5.., ymax=pcp22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_epcTout à fait d'accord"="I strongly agree", "Principles_epcPlutôt d'accord"="I rather agree", "Principles_epcPlutôt pas d'accord"="I rather disagree", "Principles_epcPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Principle 3 : grandfathering
pcp3 <-data.frame(svymean(~Principles_gdfr, design=design_FR))
pcp32 <-data.frame(confint(svymean(~Principles_gdfr, design=design_FR)))
pcp3$name <-factor (rownames(pcp3), levels= c("Principles_gdfrTout à fait d'accord", "Principles_gdfrPlutôt d'accord", "Principles_gdfrPlutôt pas d'accord", "Principles_gdfrPas du tout d'accord"))
graph_pcp3 <- ggplot(pcp3,aes(x=name,y=mean, fill=name))
graph_pcp3 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=pcp32$X2.5.., ymax=pcp32$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_gdfrTout à fait d'accord"="I strongly agree", "Principles_gdfrPlutôt d'accord"="I rather agree", "Principles_gdfrPlutôt pas d'accord"="I rather disagree", "Principles_gdfrPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Principle 4 : poor countries
pcp4 <-data.frame(svymean(~Principles_poor, design=design_FR))
pcp42 <-data.frame(confint(svymean(~Principles_poor, design=design_FR)))
pcp4$name <-factor (rownames(pcp4), levels= c("Principles_poorTout à fait d'accord", "Principles_poorPlutôt d'accord", "Principles_poorPlutôt pas d'accord", "Principles_poorPas du tout d'accord"))
graph_pcp4 <- ggplot(pcp4,aes(x=name,y=mean, fill=name))
graph_pcp4 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=pcp42$X2.5.., ymax=pcp42$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_poorTout à fait d'accord"="I strongly agree", "Principles_poorPlutôt d'accord"="I rather agree", "Principles_poorPlutôt pas d'accord"="I rather disagree", "Principles_poorPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Principle 5 : cost efficiency
pcp5 <-data.frame(svymean(~Principles_cost, design=design_FR))
pcp52 <-data.frame(confint(svymean(~Principles_cost, design=design_FR)))
pcp5$name <-factor (rownames(pcp5), levels= c("Principles_costTout à fait d'accord", "Principles_costPlutôt d'accord", "Principles_costPlutôt pas d'accord", "Principles_costPas du tout d'accord"))
graph_pcp5 <- ggplot(pcp5,aes(x=name,y=mean, fill=name))
graph_pcp5 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=pcp52$X2.5.., ymax=pcp52$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_costTout à fait d'accord"="I strongly agree", "Principles_costPlutôt d'accord"="I rather agree", "Principles_costPlutôt pas d'accord"="I rather disagree", "Principles_costPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Ranking principles
rkpcp_pastem <- data.frame(svymean(~RkPriciple.SQ001., design=design_FR))
rkpcp_pastem$ci<-data.frame(confint(svymean(~RkPriciple.SQ001., design=design_FR)))$X2.5..
rkpcp_pastem$cs<-data.frame(confint(svymean(~RkPriciple.SQ001., design=design_FR)))$X97.5..
rkpcp_epc <-data.frame(svymean(~RkPriciple.SQ002., design=design_FR))
rkpcp_epc$ci<-data.frame(confint(svymean(~RkPriciple.SQ002., design=design_FR)))$X2.5..
rkpcp_epc$cs<-data.frame(confint(svymean(~RkPriciple.SQ002., design=design_FR)))$X97.5..
rkpcp_gdfr <-data.frame(svymean(~RkPriciple.SQ003., design=design_FR))
rkpcp_gdfr$ci<-data.frame(confint(svymean(~RkPriciple.SQ003., design=design_FR)))$X2.5..
rkpcp_gdfr$cs<-data.frame(confint(svymean(~RkPriciple.SQ003., design=design_FR)))$X97.5..
rkpcp_poor<-data.frame(svymean(~RkPriciple.SQ004., design=design_FR))
rkpcp_poor$ci<-data.frame(confint(svymean(~RkPriciple.SQ004., design=design_FR)))$X2.5..
rkpcp_poor$cs<-data.frame(confint(svymean(~RkPriciple.SQ004., design=design_FR)))$X97.5..
rkpcp_cost <-data.frame(svymean(~RkPriciple.SQ005., design=design_FR))
rkpcp_cost$ci<-data.frame(confint(svymean(~RkPriciple.SQ005., design=design_FR)))$X2.5..
rkpcp_cost$cs<-data.frame(confint(svymean(~RkPriciple.SQ005., design=design_FR)))$X97.5..
rkpcp_pastem$principle <- c("Past emissions")
rkpcp_epc$principle <- c("Convergence per capita")
rkpcp_gdfr$principle <- c("Grandfathering")
rkpcp_poor$principle<- c("Poor countries")
rkpcp_cost$principle <- c("Cost efficiency")

rkpcp <- rbind.data.frame(rkpcp_cost,rkpcp_poor,rkpcp_gdfr,rkpcp_epc,rkpcp_pastem)
rkpcp$answer<- c("No", "Yes")
rkpcp2<- subset(rkpcp, rkpcp$answer=="Yes")

graph_rkpcp  <- ggplot(rkpcp2,aes(x=principle,y=mean, fill=principle))
graph_rkpcp + geom_bar(position=position_dodge(),stat="identity",color="black")+ labs(y="Proportion of the French population", x="Principles")  + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2") + theme(legend.position = "none")+ coord_cartesian(ylim=c(0,0.83))


#Tolerance
tolerance<-data.frame(svymean(~Tolerance, design=design_FR))

#Statement 1
statement1 <-data.frame(svymean(~Statements_forests, design=design_FR))
statement12 <-data.frame(confint(svymean(~Statements_forests, design=design_FR)))
statement1$name <-factor (rownames(statement1), levels= c("Statements_forestsTout à fait d'accord", "Statements_forestsPlutôt d'accord", "Statements_forestsPlutôt pas d'accord", "Statements_forestsPas du tout d'accord"))
graph_statement1 <- ggplot(statement1,aes(x=name,y=mean, fill=name))
graph_statement1 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=statement12$X2.5.., ymax=statement12$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_forestsTout à fait d'accord"="I strongly agree", "Statements_forestsPlutôt d'accord"="I rather agree", "Statements_forestsPlutôt pas d'accord"="I rather disagree", "Statements_forestsPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Statement 2
statement2 <-data.frame(svymean(~Statements_trigger, design=design_FR))
statement22 <-data.frame(confint(svymean(~Statements_trigger, design=design_FR)))
statement2$name <-factor (rownames(statement2), levels= c("Statements_triggerTout à fait d'accord", "Statements_triggerPlutôt d'accord", "Statements_triggerPlutôt pas d'accord", "Statements_triggerPas du tout d'accord"))
graph_statement2 <- ggplot(statement2,aes(x=name,y=mean, fill=name))
graph_statement2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=statement22$X2.5.., ymax=statement22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_triggerTout à fait d'accord"="I strongly agree", "Statements_triggerPlutôt d'accord"="I rather agree", "Statements_triggerPlutôt pas d'accord"="I rather disagree", "Statements_triggerPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Statement 3
statement3 <-data.frame(svymean(~Statements_funding, design=design_FR))
statement32 <-data.frame(confint(svymean(~Statements_funding, design=design_FR)))
statement3$name <-factor (rownames(statement3), levels= c("Statements_fundingTout à fait d'accord", "Statements_fundingPlutôt d'accord", "Statements_fundingPlutôt pas d'accord", "Statements_fundingPas du tout d'accord"))
graph_statement3 <- ggplot(statement3,aes(x=name,y=mean, fill=name))
graph_statement3 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=statement32$X2.5.., ymax=statement32$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_fundingTout à fait d'accord"="I strongly agree", "Statements_fundingPlutôt d'accord"="I rather agree", "Statements_fundingPlutôt pas d'accord"="I rather disagree", "Statements_fundingPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Statement 4
statement4 <-data.frame(svymean(~Statements_tech, design=design_FR))
statement42 <-data.frame(confint(svymean(~Statements_tech, design=design_FR)))
statement4$name <-factor (rownames(statement4), levels= c("Statements_techTout à fait d'accord", "Statements_techPlutôt d'accord", "Statements_techPlutôt pas d'accord", "Statements_techPas du tout d'accord"))
graph_statement4 <- ggplot(statement4,aes(x=name,y=mean, fill=name))
graph_statement4 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=statement42$X2.5.., ymax=statement42$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_techTout à fait d'accord"="I strongly agree", "Statements_techPlutôt d'accord"="I rather agree", "Statements_techPlutôt pas d'accord"="I rather disagree", "Statements_techPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Accountability
accountability <-data.frame(svymean(~Accountability, design=design_FR))
accountability2 <-data.frame(confint(svymean(~Accountability, design=design_FR)))
accountability$name <-factor (rownames(accountability), levels= c("AccountabilityFootprint", "AccountabilityTerritory"))
graph_accountability <- ggplot(accountability,aes(x=name,y=mean, fill=name))
graph_accountability + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=accountability2$X2.5.., ymax=accountability2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("AccountabilityFootprint"="Footprint", "AccountabilityTerritory"="Territorial emissions")) + coord_cartesian(ylim=c(0,0.8))




#History
history <-data.frame(svymean(~History, design=design_FR))
history2 <-data.frame(confint(svymean(~History, design=design_FR)))
history$name <-factor (rownames(history), levels= c("HistoryCurrent emissions", "HistorySince 1990", "HistorySince 1850" ))
graph_history <- ggplot(history,aes(x=name,y=mean, fill=name))
graph_history + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=history2$X2.5.., ymax=history2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("HistoryCurrent emissions"="Current emissions", "HistorySince 1990"="Since 1990", "HistorySince 1850"="Since 1850")) + coord_cartesian(ylim=c(0,0.5))






## Cas particuliers
##_______________________




#Test past emissions
Test1 <-svymean(~Test.SQ001., design=design_FR)

#Test future emissions
Test2 <-svymean(~Test.SQ002., design=design_FR)

#Test jauge
Test3 <-svymean(~Test.SQ003., design=design_FR)

#Test pop/PIB
Test4 <-svymean(~Test.SQ004., design=design_FR)


#Test global
Resulttest <-data.frame(svymean(~CaseComprehension, design=design_FR))
Resulttest2 <-data.frame(confint(svymean(~CaseComprehension, design=design_FR)))
Resulttest$name <-factor (rownames(Resulttest), levels= c("CaseComprehensionMisread climate change", "CaseComprehensionOne or several errors", "CaseComprehensionPerfect Comprehension"))
graph_resulttest <- ggplot(Resulttest,aes(x=name,y=mean, fill=name))
graph_resulttest + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=Resulttest2$X2.5.., ymax=Resulttest2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CaseComprehensionMisread climate change"="Misread climate change", "CaseComprehensionOne or several errors"="One or several errors", "CaseComprehensionPerfect Comprehension"="Perfect comprehension")) + coord_cartesian(ylim=c(0,0.5))



#Case 1 : A has higher future emissions, damaging CC


Case1A <-svymean(~Case1.SQ001., design=design_FR)
Case1A_right <-t(svyby(~Case1.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case1B <-svymean(~Case1.SQ002., design=design_FR)
Case1B_right <-t(svyby(~Case1.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))



#Case 2 : A has higher future emissions and pop, damaging CC


Case2A <-svymean(~Case2.SQ001., design=design_FR)
Case2A_right <-t(svyby(~Case2.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case2B <-svymean(~Case2.SQ002., design=design_FR)
Case2B_right <-t(svyby(~Case2.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


#Case 3 : A has higher future emissions and smaller income, damaging CC


Case3A <-svymean(~Case3.SQ001., design=design_FR)
Case3A_right <-t(svyby(~Case3.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case3B <-svymean(~Case3.SQ002., design=design_FR)
Case3B_right <-t(svyby(~Case3.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))



#Case 4 : A has higher future emissions and smaller past emissions, damaging CC


Case4A <-svymean(~Case4.SQ001., design=design_FR)
Case4A_right <-t(svyby(~Case4.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case4B <-svymean(~Case4.SQ002., design=design_FR)
Case4B_right <-t(svyby(~Case4.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


#Case 5 : Tout change, mÃÂÃÂªmes émissions, damaging CC


Case5A <-svymean(~Case5.SQ001., design=design_FR)
Case5A_right <-t(svyby(~Case5.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case5B <-svymean(~Case5.SQ002., design=design_FR)
Case5B_right <-t(svyby(~Case5.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


#Case 6 : Tout change, controlled CC


Case6A <-svymean(~Case6.SQ001., design=design_FR)
Case6A_right <-t(svyby(~Case6.SQ001.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


Case6B <-svymean(~Case6.SQ002., design=design_FR)
Case6B_right <-t(svyby(~Case6.SQ002.,by=~CaseComprehension,design=design_FR,FUN=svymean,vartype=c('ci')))


#Graph on all cases
CasesFR <- read.csv2("Fig6.csv",  encoding = "UTF-8")
CasesFR <- data.frame(CasesFR)
CasesFR$Opinion<-factor(CasesFR$Opinion, levels = c("Strongly agree","Rather agree", "Strongly disagree", "Rather disagree"))
CasesFR$Significative<-factor(CasesFR$Significative)
CasesFR$Case<-factor(CasesFR$Case, levels = c("5","4", "3","2", "1", "0"))

ggplot(subset(CasesFR,Country.case=="A"), aes(x = Case)) +
  geom_col(data = subset(CasesFR, (Country.case=="A" & (Opinion == "Strongly agree"| Opinion == "Rather agree"))), 
           aes(y = Freq, fill = Opinion)) +
  geom_col(data = subset(CasesFR, (Country.case=="A" & (Opinion == "Strongly disagree"| Opinion == "Rather disagree"))), 
           aes(y = -Freq, fill = Opinion)) + 
  scale_fill_manual(values=c("#92C5DE","#F4A582", "#0571B0", "#CA0020")) +
  labs(y = "Frequence of opinions on country A", x = "Case")+
  coord_flip() 

ggplot(subset(CasesFR,Country.case=="B"), aes(x = Case)) +
  geom_col(data = subset(CasesFR, (Country.case=="B" & (Opinion == "Strongly agree"| Opinion == "Rather agree"))), 
           aes(y = Freq, fill = Opinion)) +
  geom_col(data = subset(CasesFR, (Country.case=="B" & (Opinion == "Strongly disagree"| Opinion == "Rather disagree"))), 
           aes(y = -Freq, fill = Opinion)) + 
  scale_fill_manual(values=c("#92C5DE","#F4A582", "#0571B0", "#CA0020")) +
  labs(y = "Frequence of opinions on country B", x = "Case")+
  coord_flip() 


## Coordination et cas réel
##_______________________________


#Coordination


China <-data.frame(svymean(~Coord_China, design=design_FR))
China2<-data.frame(confint(svymean(~Coord_China, design=design_FR)))
China$name <-factor (rownames(China), levels= c("Coord_ChinaTout à fait d'accord", "Coord_ChinaPlutôt d'accord", "Coord_ChinaPlutôt pas d'accord", "Coord_ChinaPas du tout d'accord"))
graph_china <- ggplot(China,aes(x=name,y=mean, fill=name))
graph_china + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=China2$X2.5.. , ymax=China2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_ChinaTout à fait d'accord"="I strongly agree", "Coord_ChinaPlutôt d'accord"="I partially agree", "Coord_ChinaPlutôt pas d'accord"="I partially disagree", "Coord_ChinaPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.63))


India <-data.frame(svymean(~Coord_India, design=design_FR))
India2 <- data.frame(confint(svymean(~Coord_India, design=design_FR)))
India$name <-factor (rownames(India), levels= c("Coord_IndiaTout à fait d'accord", "Coord_IndiaPlutôt d'accord", "Coord_IndiaPlutôt pas d'accord", "Coord_IndiaPas du tout d'accord"))
graph_India <- ggplot(India,aes(x=name,y=mean, fill=name))
graph_India + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=India2$X2.5.. , ymax=India2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_IndiaTout à fait d'accord"="I strongly agree", "Coord_IndiaPlutôt d'accord"="I partially agree", "Coord_IndiaPlutôt pas d'accord"="I partially disagree", "Coord_IndiaPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.61))


EU<-data.frame(svymean(~Coord_EU, design=design_FR))
EUci<-data.frame(confint(svymean(~Coord_EU, design=design_FR)))
EU$name <-factor (rownames(EU), levels= c("Coord_EUTout à fait d'accord", "Coord_EUPlutôt d'accord", "Coord_EUPlutôt pas d'accord", "Coord_EUPas du tout d'accord"))
graph_EU <- ggplot(EU,aes(x=name,y=mean, fill=name))
graph_EU + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=EUci$X2.5.. , ymax=EUci$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_EUTout à fait d'accord"="I strongly agree", "Coord_EUPlutôt d'accord"="I partially agree", "Coord_EUPlutôt pas d'accord"="I partially disagree", "Coord_EUPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.61))



USA<-data.frame(svymean(~Coord_USA, design=design_FR))
USAci<-data.frame(confint(svymean(~Coord_USA, design=design_FR)))
USA$name <-factor (rownames(USA), levels= c("Coord_USATout à fait d'accord", "Coord_USAPlutôt d'accord", "Coord_USAPlutôt pas d'accord", "Coord_USAPas du tout d'accord"))
graph_USA <- ggplot(USA,aes(x=name,y=mean, fill=name))
graph_USA + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=USAci$X2.5.. , ymax=USAci$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_USATout à fait d'accord"="I strongly agree", "Coord_USAPlutôt d'accord"="I partially agree", "Coord_USAPlutôt pas d'accord"="I partially disagree", "Coord_USAPas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.61))



#Paths
paths <- data.frame(svymean(~Path, design=design_FR))
paths$name <-factor (rownames(paths))
paths$ci<- data.frame(confint(svymean(~Path, design=design_FR)))$X2.5..
paths$cs<- data.frame(confint(svymean(~Path, design=design_FR)))$X97.5..
paths2<-subset(paths,as.numeric(paths$ci)>0.02)
graph_paths <- ggplot(paths2,aes(x=name,y=mean, fill="blue"))
graph_paths + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Most represented judgement paths") + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_x_discrete(labels=c("A","I","C","F","G","J","H","K","L")) + coord_cartesian(ylim=c(0,0.1))

#Path categories
pathscat <- data.frame(svymean(~Pathcat, design=design_FR))


#Reward

EU2<-data.frame(svymean(~Reward.SQ001., design=design_FR))
EU22<-data.frame(confint(svymean(~Reward.SQ001., design=design_FR)))
EU2$name <-factor (rownames(EU2), levels= c("Reward.SQ001.Tout à fait d'accord", "Reward.SQ001.Plutôt d'accord", "Reward.SQ001.Plutôt pas d'accord", "Reward.SQ001.Pas du tout d'accord"))
graph_EU2 <- ggplot(EU2,aes(x=name,y=mean, fill=name))
graph_EU2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=EU22$X2.5.. , ymax=EU22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Reward.SQ001.Tout à fait d'accord"="I strongly agree", "Reward.SQ001.Plutôt d'accord"="I partially agree", "Reward.SQ001.Plutôt pas d'accord"="I partially disagree", "Reward.SQ001.Pas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.61))


USA2<-data.frame(svymean(~Reward.SQ002., design=design_FR))
USA22<-data.frame(confint(svymean(~Reward.SQ002., design=design_FR)))
USA2$name <-factor (rownames(USA2), levels= c("Reward.SQ002.Tout à fait d'accord", "Reward.SQ002.Plutôt d'accord", "Reward.SQ002.Plutôt pas d'accord", "Reward.SQ002.Pas du tout d'accord"))
graph_USA2 <- ggplot(USA2,aes(x=name,y=mean, fill=name))
graph_USA2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=USA22$X2.5.. , ymax=USA22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Reward.SQ002.Tout à fait d'accord"="I strongly agree", "Reward.SQ002.Plutôt d'accord"="I partially agree", "Reward.SQ002.Plutôt pas d'accord"="I partially disagree", "Reward.SQ002.Pas du tout d'accord"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.61))


coord<-data.frame(svymean(~Coord, design=design_FR))
coord2<-data.frame(confint(svymean(~Coord, design=design_FR)))
coord$name <-factor (rownames(coord))
graph_coord <- ggplot(coord,aes(x=name,y=mean, fill=name))
graph_coord + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=coord2$X2.5.. , ymax=coord2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CoordChanged their mind"="Changed their mind", "CoordSame opinion"="Same opinion"))+ coord_cartesian(ylim=c(0,0.6))


#alluvial charts
list <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
Coordination_Question<- data.frame(list)
Coordination_Question$Opinion_EU<-factor(c("Strongly agree","Strongly agree","Strongly agree","Strongly agree","partially agree","partially agree","partially agree","partially agree","partially disagree","partially disagree","partially disagree","partially disagree","strongly disagree","strongly disagree","strongly disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_Question$Coordination_EU<-factor(c("Strongly agree","partially agree","partially disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_Question$freq<- c(0.125,0.073,0.006,0.006,0.073,0.446,0.062,0.004,0.007,0.063,0.080,0.008,0.001,0.010,0.008,0.029)
alluvial(Coordination_Question[,2:3], freq=Coordination_Question$freq, col = ifelse(Coordination_Question$Opinion_EU == Coordination_Question$Coordination_EU, "grey", "orange"), border=ifelse(Coordination_Question$Opinion_EU == Coordination_Question$Coordination_EU, "grey", "orange"))

ggplot(Coordination_Question, aes(y = freq, axis1 = Opinion_EU, axis2 = Coordination_EU)) +
  geom_alluvium(aes(fill = ifelse(Opinion_EU == Coordination_EU, "stayed", "changed")), aes.bind=TRUE, width = 1/12) +
  geom_stratum(width = 1/4, fill = c(brewer.pal("RdBu", n=4),brewer.pal("RdBu", n=4)) , color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("Individual judgement", "Coordination exercise"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("black", "grey"))+
  theme_minimal() +
  theme(legend.position = "none")



Coordination_QuestionUS<- data.frame(list)
Coordination_QuestionUS$Opinion_US<-factor(c("Strongly agree","Strongly agree","Strongly agree","Strongly agree","partially agree","partially agree","partially agree","partially agree","partially disagree","partially disagree","partially disagree","partially disagree","strongly disagree","strongly disagree","strongly disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_QuestionUS$Coordination_US<-factor(c("Strongly agree","partially agree","partially disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_QuestionUS$freq<- c(0.047,0.028,0.010,0.002,0.037,0.209,0.074,0.009,0.008,0.091,0.225,0.044,0.006,0.026,0.070,0.115)
alluvial(Coordination_QuestionUS[,2:3], freq=Coordination_QuestionUS$freq, col = ifelse(Coordination_QuestionUS$Opinion_US == Coordination_QuestionUS$Coordination_US, "grey", "cadetblue3"), border=ifelse(Coordination_QuestionUS$Opinion_US == Coordination_QuestionUS$Coordination_US, "grey", "cadetblue3"))

ggplot(Coordination_QuestionUS, aes(y = freq, axis1 = Opinion_US, axis2 = Coordination_US)) +
  geom_alluvium(aes(fill = ifelse(Opinion_US == Coordination_US, "stayed", "changed")), aes.bind=TRUE, width = 1/12) +
  geom_stratum(width = 1/4, fill = c(brewer.pal("RdBu", n=4),brewer.pal("RdBu", n=4)) , color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("Individual judgement", "Coordination exercise"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("black", "grey"))+
  theme_minimal() +
  theme(legend.position = "none")



#Chances

chances<-data.frame(svymean(~Chances, design=design_FR))
chances2<-data.frame(confint(svymean(~Chances, design=design_FR)))
chances$name <-factor (rownames(chances), levels= c("Chancesmoins de 1%","Chancesentre 1 et 10%","Chancesentre 11 et 49%","Chancesentre 50 et 90%", "Chancesentre 91 et 99%","Chancesplus de 99%"))
graph_chances <- ggplot(chances,aes(x=name,y=mean, fill=name))
graph_chances+ geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the French population", x="Answer") + geom_errorbar(aes(ymin=chances2$X2.5.. , ymax=chances2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("Chancesmoins de 1%"="less than 1%","Chancesentre 1 et 10%"="1 to 10%","Chancesentre 11 et 49%"="11 to 49%","Chancesentre 50 et 90%"="50 to 90%", "Chancesentre 91 et 99%"="90 to 99%","Chancesplus de 99%"="more than 99%"))+ coord_cartesian(ylim=c(0,0.4))




##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Questions de recherche
##_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


#Consistency checks 
##________________________________


#Les personnes en faveur du grandfathering sont elles en faveur de son principe ?
grandfathering <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Principles_gdfr, chisq=TRUE)
#10-10, people who strongly agree with the proposition are overrepresented among people who strongly agree with the principle and underrepresented among people who rather disagree. People who rather disagree with the proposition are underrepresented among people who strongly agree with the principle but overrepresented among people who rather agree. People who strongly disagree with the proposition are overrepresented among people who strongly agree with the principle.



#Emissions historiques : y a-t-il un lien entre les réponses ÃÂ  la question History et le cas particulier
history1 <- CrossTable(QuestionnaireFR$Case4.SQ001.,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P dans les 10-20, people who strongly agree that country A is taking its fair share in case 4 are overrepresented among people who strongly agree with the principle about past emissions, and underrepresented among people who rather agree and rather disagree. People who rather disagree with the fairness of country A in case 4 are underrepresented among people who strongly agree with the principle, and overrepresented among people who rather disagree. etc (la diagonale a de grosses contributions, surreprésentation)


#Emissions historiques : y a-t-il un lien entre les réponses ÃÂ  la question History et au principe ?
history2 <- CrossTable(QuestionnaireFR$History,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P=0.025


#Poor countries : lien entre principe et proposition ? 
poor1 <- CrossTable(QuestionnaireFR$Principles_poor,QuestionnaireFR$Statements_trigger, chisq=TRUE)
#P dans les 10-97, la diagonale a de grosses contributions (surreprésentation), soFR-représentation des rather disagree dans les strongly agree des deux cÃÂÃÂ´tés


#Poor countries : lien entre principe et cas particulier ?
poor2 <- CrossTable(QuestionnaireFR$Principles_poor,QuestionnaireFR$Case3.SQ001., chisq=TRUE)
#P dans les 10-13, la diagonale a de grosses contributions (surreprésentation), soFR-représentation des rather disagree dans les strongly agree des deux cÃÂÃÂ´tés 


##Consistency : link between pop and epc ? 
epc <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Case2.SQ001., chisq=TRUE)
#P dans les 10-7, diagonale surreprésentée + strongly agree/strongly disagree


##CBDR : link between CBDR and poor principle ?
CBDR1 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Principles_poor, chisq=TRUE)
#P=0.2

##CBDR : link between CBDR and past principle ?
CBDR2 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P=0.011

##CBDR : link between CBDR and cost principle ?
CBDR3 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Principles_cost, chisq=TRUE)
#P dans les 10-4

##CBDR : link between CBDR and poor principle ?
CBDR4 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Gdfr, chisq=TRUE)
#P dans les 10-4 

##CBDR : link between CBDR and forest principle ?
CBDR5 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Statements_forests, chisq=TRUE)
#P=0.2

##CBDR : link between CBDR and sharetech principle ?
CBDR6 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Statements_tech, chisq=TRUE)
#P=0.2

##CBDR : link between CBDR and fund principle ?
CBDR7 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Statements_funding, chisq=TRUE)
#P=0.4

##CBDR : link between CBDR and  epc principle ?
CBDR8 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Principles_epc, chisq=TRUE)
#P dans les 10-8

#Les personnes en faveur du grandfathering rejettent-elles les émissions passées ?
grandfathering2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P dans les 10-50 people in have the same opinion about grandfathering and past emissions

#Les personnes en faveur d'u grandfathering'epc rejettent-elles les émissions passées ?
epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P dans les 10-39 people in have the same opinion about grandfathering and past emissions


#Qui est en faveur d'epc ?
epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Age, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Gender, chisq=TRUE)
#p=0.04

epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Freqtalk, chisq=TRUE)
#p=0.003
epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-11
epc2 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Knowledge, chisq=TRUE)
#p dans les 10-4

#Qui est en faveur de dgfr ?
epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Age, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Gender, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Freqtalk, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-3
epc2 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Knowledge, chisq=TRUE)


#Qui est en faveur de dgfr ?
epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Age, chisq=TRUE)
#10-10
epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Gender, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Freqtalk, chisq=TRUE)
#0.01
epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$EffectsCC, chisq=TRUE)
#p dans les 10-11
epc2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Knowledge, chisq=TRUE)
#p dans les 10-5

#Effet d'apprentissage 
##________________________________



apprentissage1<-CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Orderpcp, chisq=TRUE)

apprentissage2 <- CrossTable(QuestionnaireFR$Gdfr,QuestionnaireFR$Orderpcp,chisq=TRUE)
#P=0.01, rather disagree overrepresented for people for which principles were 4rth, underrepresented for 3rd

apprentissage3 <- CrossTable(QuestionnaireFR$Principles_past_em,QuestionnaireFR$Orderpcp,chisq=TRUE)
#P dans les 10-4 Overrep of strongly disagree among people who got 3rd

apprentissage4 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$Orderpcp,chisq=TRUE)
#P=0.01, overrep of strongly agree and strongly disagree among people who got 3rd

apprentissage6 <- CrossTable(QuestionnaireFR$Principles_cost,QuestionnaireFR$Orderpcp,chisq=TRUE)
#P dans les 10-7 overrep of strongly agree and strongly disagree among people who got 3rd, underrep among people who got 4rth

apprentissage7 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$Orderpcp,chisq=TRUE)
#P=0.02 underrep of rather disagree among people who got 3rd

apprentissage8 <- CrossTable(QuestionnaireFR$Principles_poor,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage9 <- CrossTable(QuestionnaireFR$Statements_forests,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage10 <- CrossTable(QuestionnaireFR$Statements_trigger,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage11 <- CrossTable(QuestionnaireFR$Statements_funding,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage12 <- CrossTable(QuestionnaireFR$Statements_tech,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage13 <- CrossTable(QuestionnaireFR$Accountability,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage14 <- CrossTable(QuestionnaireFR$History,QuestionnaireFR$Orderpcp,chisq=TRUE)


#Apprentissage dans l'autre sens

apprentissage15 <- CrossTable(QuestionnaireFR$Case1.SQ001.,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage16 <- CrossTable(QuestionnaireFR$Case1.SQ002.,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage17 <- CrossTable(QuestionnaireFR$Case2.SQ001.,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage18 <- CrossTable(QuestionnaireFR$Case2.SQ002.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage19 <- CrossTable(QuestionnaireFR$Case3.SQ001.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage20 <- CrossTable(QuestionnaireFR$Case3.SQ002.,QuestionnaireFR$Orderpcp,chisq=TRUE)


apprentissage21 <- CrossTable(QuestionnaireFR$Case4.SQ001.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage22 <- CrossTable(QuestionnaireFR$Case4.SQ002.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage23 <- CrossTable(QuestionnaireFR$Case5.SQ001.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage24 <- CrossTable(QuestionnaireFR$Case5.SQ002.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage25 <- CrossTable(QuestionnaireFR$Case6.SQ001.,QuestionnaireFR$Orderpcp, chisq=TRUE)


apprentissage26 <- CrossTable(QuestionnaireFR$Case6.SQ002.,QuestionnaireFR$Orderpcp, chisq=TRUE)










history3 <- CrossTable(QuestionnaireFR$Knowledge,QuestionnaireFR$Principles_past_em, chisq=TRUE)

history4 <- CrossTable(QuestionnaireFR$CauseCC,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#0.02

history5 <- CrossTable(QuestionnaireFR$Freqtalk,QuestionnaireFR$Principles_past_em, chisq=TRUE)

history6 <- CrossTable(QuestionnaireFR$EffectsCC,QuestionnaireFR$Principles_past_em, chisq=TRUE)
#10-4

history7 <- CrossTable(QuestionnaireFR$FirstGen,QuestionnaireFR$Principles_past_em, chisq=TRUE)

history8 <- CrossTable(QuestionnaireFR$VisioncoopCC,QuestionnaireFR$Principles_past_em, chisq=TRUE)







###CBDR who favors it
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Age, chisq=TRUE)
#nada
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$EffectsCC, chisq=TRUE)
#common : people who think CC will be disastrous
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Knowledge, chisq=TRUE)
#common :  advanced
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$Freqtalk, chisq=TRUE)
#Several times a week
CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$VisioncoopCC, chisq=TRUE)
#International scale 





##Poor countries : link between poor and tolerance towards abroad efforts ? 
##_________________________________________________________________________________________________________________
poor4 <- CrossTable(QuestionnaireFR$Statements_funding,QuestionnaireFR$Principles_poor, chisq=TRUE)
#P dans les 10-75, la diagonale est surreprésentée





##Do people who think CC is disastrous think differently in the principles ?
##_______________________________________________________________________________________________________________

conseq1 <- CrossTable(QuestionnaireFR$Principles_poor,QuestionnaireFR$EffectsCC, chisq=TRUE)

conseq2 <- CrossTable(QuestionnaireFR$Principles_past_em,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-5. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair for countries that emitted less to make less efforts (whereas people who think CC will be significant are underrepresented). People who think climate change will be insignificant are also overrepresented among people who strongly agree with the principle, and underrepresented among people who rather disagree. 

conseq3 <- CrossTable(QuestionnaireFR$Principles_epc,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-16. People who think climate change will be disastrous are overrepresented among people who strongly agree that a per capita convergence would be fair, underrepresented in the rest. People who think it will be small are overrepresented among people who rather disagree

conseq4 <- CrossTable(QuestionnaireFR$Principles_gdfr,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-9. People who think climate change will be disastrous are overrepresented among people who strongly agree that an operational grandfathering would be fair, underrepresented in the rest. People who think it will be insignificant are overrepresented among people who disagree, people who think it will be small are underrepresented among people who strongly agree, overrepresented among people who rather agree

conseq5 <- CrossTable(QuestionnaireFR$Principles_cost,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-5. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries in which it is more costly to reduce reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree

conseq6 <- CrossTable(QuestionnaireFR$Statements_forests,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-3. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries with forests reduce less (People who think it will be significant are underrepresented). People who think it will be small are overrepresented among people who rather agree, underrrepresented among people who rather disagree

conseq7 <- CrossTable(QuestionnaireFR$Statements_trigger,QuestionnaireFR$EffectsCC, chisq=TRUE)

conseq8 <- CrossTable(QuestionnaireFR$Statements_funding,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-3. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries funding abroad projects reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree, underrrepresented among people who rather disagree

conseq9 <- CrossTable(QuestionnaireFR$Statements_tech,QuestionnaireFR$EffectsCC, chisq=TRUE)

conseq10 <- CrossTable(QuestionnaireFR$CBDR,QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-16. People who think CC will be small/insignificant are overrepresented among people who responded none of the above.


##Do people see a link between funding and footprint ?
##____________________________________________________________________________________________________________________

funding <- CrossTable(QuestionnaireFR$Statements_funding,QuestionnaireFR$Accountability, chisq=TRUE)
#P=dans les 10-4. People who think countries should be held responsible for their footprint are underrepresented among people who strongly agree with the fairness of the funding statement, underrepresented among people who rather agree. (The opposite for people who think countries are to be held responsible for their territorial emissions)



##Do people change their minds ? 
##___________________________________________________________________________________________________________________
coordination1<- CrossTable(QuestionnaireFR$Coord_EU,QuestionnaireFR$Reward.SQ001., chisq=TRUE)
#P dans les 10-159, diagonale surreprésentée

coordination2<- CrossTable(QuestionnaireFR$Coord_USA,QuestionnaireFR$Reward.SQ002., chisq=TRUE)
#P dans les 10-132, idem diagonale





##Coordination
##__________________________________________________________________________________________________________________________________
coordination3<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$Chances, chisq=TRUE)

coordination4<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$Age, chisq=TRUE)

coordination5<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$Gender, chisq=TRUE)

coordination6<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$VisioncoopCC, chisq=TRUE)

coordination7<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$CBDR, chisq=TRUE)

coordination8<- CrossTable(QuestionnaireFR$Coord,QuestionnaireFR$Gdfr, chisq=TRUE)



#Are there generational differences ? 
##______________________________________________________________________________________________________________________

generation1 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Freqtalk, chisq=TRUE)
#P dans les 10-8. 18-24 are overrepresented among people who talk about CC several times a month (underrepresented among people who talk more rarely), 25-34 among people who talk about it several times a week, 45-54 among people who talk about it several times a month, 65+ are overrepresented among people who almost never talk about it


generation2 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$CauseCC, chisq=TRUE)
#P dans les 10-3. 18-24 and 25-34 are underrepresented among people who think CC is nature caused, 65+ are overrepresented among people who think CC is nature caused


generation3 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$EffectsCC, chisq=TRUE)
#P dans les 10-3, 65+ are overrep among people who think the effects will be small, underrep for disastrous


generation4 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$RegionCC, chisq=TRUE)
#P dans les 10-4 18-24 and 25-34 are overrepresented among people who think the EU will be more affected.


generationbis <- CrossTable(QuestionnaireUS$Age, QuestionnaireUS$FirstGen, chisq=TRUE)

generation5 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$VisioncoopCC, chisq=TRUE)

generation6 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$CBDR, chisq=TRUE)

generation7 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Gdfr, chisq=TRUE)
#P dans les 10-10 25-34 and 35-44 are overrepresented among people who strongly agree and rather agree with grandfathering (55-64 and 65+ underrepresented), 25-34 are underrepresented among people who strongly disagree (55-64 and 65+ are overrepresented)


generation8 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P dans les 10-4 25-34 are overrepresented among people who agree that past emissions should be accounted for, 65+ overrepresented among people who rather disagree


generation9 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Principles_epc, chisq=TRUE)


generation10 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Principles_gdfr, chisq=TRUE)


generation11 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Principles_poor, chisq=TRUE)
#P dans les 10-6 25-34 are overrepresented among people who strongly agree that poor countries should reduce less, 65+ overrepresented among people who strongly disagree.


generation12 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Principles_cost, chisq=TRUE)
#P dans les 10-11 25-34 are overrepresented among people who strongly agree that countries in which it is more costly to reduce should reduce less (65+ underrepresented). 65+ are overrepresented among people who strongly disagree


generation13 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Statements_forests, chisq=TRUE)
#P dans les 10-9 25-34 are overrepresented among people who strongly agree that countries with forests should reduce less (45-54 underrepresented). 55-64 are overrepresented among people who rather disagree


generation14 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Statements_funding, chisq=TRUE)
#P dans les 10-9 25-34 are overrepresented among people who strongly agree that countries funding abroad projects should reduce less (55-64 and 65+ underrepresented)


generation15 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Statements_tech, chisq=TRUE)
#P dans les 10-10 25-34 are overrepresented among people who strongly agree that countries sharing their technologies should reduce less, 35-44 among people who rather agree (45+ underrepresented)


generation16 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Statements_trigger, chisq=TRUE)
#P dans les 10-14 25-34 are overrepresented among people who strongly agree and rather agree that poor countries could wait before reducing (45+ underrepresented). 55-64 are overrepresented among people who rather disagree and 65+ among people who strongly disagree


generation17 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Accountability, chisq=TRUE)
#P dans les 10-4 18-24 are overrepresented among people who think countries should be held responsible for their footprint, underrepresented among people who think territorial emissions (opposite for 55-64)


generation18 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$History, chisq=TRUE)
#P dans les 10-5 18-24 are underrepresented among people who think only current emissions should count, 55+ overrepresented


generation19 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$CaseComprehension, chisq=TRUE)
#P dans les 10-3 25-34 overrepresented among people who made one or several errors(55-64 underrepresented)


generation20 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case1.SQ001., chisq=TRUE)
#P dans les 10-9 25-34 overrepresented among people who strongly agree (55-64 underrepresented). 55+ overrepresented among people who rather disagree and strongly disagree

generation21 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case1.SQ002., chisq=TRUE)

generation22 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case2.SQ001., chisq=TRUE)
#P dans les 10-10 25-34 overrepresented among people who agree or strongly agree, 35-44 among people who strongly agree, 55-64 among people who rather disagree

generation23 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case2.SQ002., chisq=TRUE)

generation24 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case3.SQ001., chisq=TRUE)
#P dans les 10-6 25-34 and 35-44 overrepresented among people who strongly agree, 55+ underrepresented

generation25 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case3.SQ002., chisq=TRUE)

generation26 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case4.SQ001., chisq=TRUE)
#P dans les 10-7 25-34 and 35-44 overrepresented among people who strongly agree, 55+ underrepresented

generation27 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case4.SQ002., chisq=TRUE)

generation28 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case5.SQ001., chisq=TRUE)
#P dans les 10-5 25-34 and 35-44 overrepresented among people who strongly agree, 45-54 underrepresented (overrepresented among people who rather disagree)

generation29 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case5.SQ002., chisq=TRUE)
#P dans les 10-5 18-24 overrepresented among people who strongly agree, 65+ among people who strongly disagree

generation30 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case6.SQ001., chisq=TRUE)
#P dans les 10-4 25-34 and 35-44 overrepresented among people who strongly agree (55+ underrepresented)

generation31 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Case6.SQ002., chisq=TRUE)


generation32 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Coord_China, chisq=TRUE)
#P dans les 10-6 25-34 overrepresented among people who agree and rather agree, 35-44 among people who rather agree

generation33 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Coord_India, chisq=TRUE)

generation34 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Coord_EU, chisq=TRUE)

generation35 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Coord_USA, chisq=TRUE)
#P dans les 10-4 25-34 and 35-44 overrepresented among people who strongly agree


generation36 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Reward.SQ001., chisq=TRUE)

generation37 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Reward.SQ002., chisq=TRUE)
#P dans les 10-5 25-34 overrepresented among people who strongly agree, 45-54 underrepresented.

generation38 <- CrossTable (QuestionnaireFR$Age, QuestionnaireFR$Chances, chisq=TRUE)




#Are there differences across genders ? 
##______________________________________________________________________________________________________________________

gender1 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Freqtalk, chisq=TRUE)

gender2 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$CauseCC, chisq=TRUE)

gender3 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$EffectsCC, chisq=TRUE)

gender4 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$RegionCC, chisq=TRUE)
#P dans les 10-2 Men are overrepresented among people who think the EU will be more impacted, women underrepresented


gender5 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$VisioncoopCC, chisq=TRUE)

gender6 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$CBDR, chisq=TRUE)

gender7 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Gdfr, chisq=TRUE)

gender8 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Principles_past_em, chisq=TRUE)
#P dans les 10-4 Men underrepresented among people who strongly agree past emissions should be accounted for underrepresented among people who rather disagree (opposite for women)


gender9 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Principles_epc, chisq=TRUE)
#P=0,02 Men are underrepresented among people who rather disagree with epc (opposite for women)


gender10 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Principles_gdfr, chisq=TRUE)

gender11 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Principles_poor, chisq=TRUE)
#P dans les 10-3 Men are overrepresented among people who strongly agree that poor countries should do less, underrepresented among people who rather disagree (opposite for women)


gender12 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Principles_cost, chisq=TRUE)
#P=0.015 Men are overrepresented among people who strongly agree and strongly disagree that countries where it is more costly should do less, underrepresented among people who rather disagree (opposite for women)


gender13 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Statements_forests, chisq=TRUE)
#P=0.01 Men are underrepresented among people who rather disagree that countries with forests should do less (opposite for women)


gender14 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Statements_funding, chisq=TRUE)
#P dans les 10-4 Men are overrepresented among people who strongly agree and strongly disagree that countries funding abroad projects should do less, underrepresented among people who rather disagree (opposite for women)


gender15 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Statements_tech, chisq=TRUE)
#P=0.01 Men are overrepresented among people who strongly agree that countries sharing their tech should do less, underrepresented among people who rather disagree (opposite for women)


gender16 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Statements_trigger, chisq=TRUE)
#P dans les 10-3 Men are overrepresented among people who srtongly agree that poor countries could wait (opposite for women)


gender17 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Accountability, chisq=TRUE)


gender18 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$History, chisq=TRUE)


gender19 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$CaseComprehension, chisq=TRUE)


gender20 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case1.SQ001., chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree (opposite for women)

gender21 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case1.SQ002., chisq=TRUE)

gender22 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case2.SQ001., chisq=TRUE)

gender23 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case2.SQ002., chisq=TRUE)

gender24 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case3.SQ001., chisq=TRUE)

gender25 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case3.SQ002., chisq=TRUE)

gender26 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case4.SQ001., chisq=TRUE)
#P=0.02 Men overrepresented among people who strongly agree, underrepresented among people who rather agree (opposite for women)

gender27 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case4.SQ002., chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)

gender28 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case5.SQ001., chisq=TRUE)

gender29 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case5.SQ002., chisq=TRUE)
#P=0.006 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)

gender30 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case6.SQ001., chisq=TRUE)

gender31 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Case6.SQ002., chisq=TRUE)

gender32 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Coord_China, chisq=TRUE)

gender33 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Coord_India, chisq=TRUE)

gender34 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Coord_EU, chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)


gender35 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Coord_USA, chisq=TRUE)

gender36 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Reward.SQ001., chisq=TRUE)

gender37 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Reward.SQ002., chisq=TRUE)

gender38 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$Chances, chisq=TRUE)


#Are there differences across SPC ? 
##______________________________________________________________________________________________________________________

CSP1 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Freqtalk, chisq=TRUE)
#P dans les 10-3, inactive people are underrepresented among people who talk about CC several times a week, CSP+ overrepresented


CSP2 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$CauseCC, chisq=TRUE)

CSP3 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$EffectsCC, chisq=TRUE)

CSP4 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$RegionCC, chisq=TRUE)
#P dans les 10-4 CSP- overrepresented among people who think the USA will be more affected, CSP+ overrepresented among people who think the EU


CSP5 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$VisioncoopCC, chisq=TRUE)

CSP6 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$CBDR, chisq=TRUE)

CSP7 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Gdfr, chisq=TRUE)
#P dans les 10-3 CSP- overrepresented among people who strongly agree, Inactive underrepresented

CSP8 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Principles_past_em, chisq=TRUE)

CSP9 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Principles_epc, chisq=TRUE)

CSP10 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Principles_gdfr, chisq=TRUE)

CSP11 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Principles_poor, chisq=TRUE)

CSP12 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Principles_cost, chisq=TRUE)

CSP13 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Statements_forests, chisq=TRUE)

CSP14 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Statements_funding, chisq=TRUE)

CSP15 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Statements_tech, chisq=TRUE)
#P=0.02 CSP- overrepresented among people who strongly agree, Inactive underrepresented


CSP16 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Statements_trigger, chisq=TRUE)

CSP17 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Accountability, chisq=TRUE)

CSP18 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$History, chisq=TRUE)

CSP19 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$CaseComprehension, chisq=TRUE)

CSP20 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case1.SQ001., chisq=TRUE)

CSP21 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case1.SQ002., chisq=TRUE)

CSP22 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case2.SQ001., chisq=TRUE)

CSP23 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case2.SQ002., chisq=TRUE)

CSP24 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case3.SQ001., chisq=TRUE)
#P=0.027  CSP- overrepresented among people who strongly agree, Inactive underrepresented


CSP25 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case3.SQ002., chisq=TRUE)

CSP26 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case4.SQ001., chisq=TRUE)


CSP27 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case4.SQ002., chisq=TRUE)

CSP28 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case5.SQ001., chisq=TRUE)

CSP29 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case5.SQ002., chisq=TRUE)

CSP30 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case6.SQ001., chisq=TRUE)

CSP31 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Case6.SQ002., chisq=TRUE)

CSP32 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Coord_China, chisq=TRUE)
#P dans les 10-3 CSP+ underrepresented in the partially disagree, Inactive underrepresented in the partially agree

CSP33 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Coord_India, chisq=TRUE)

CSP34 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Coord_EU, chisq=TRUE)

CSP35 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Coord_USA, chisq=TRUE)

CSP36 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Reward.SQ001., chisq=TRUE)

CSP37 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Reward.SQ002., chisq=TRUE)

CSP38 <- CrossTable (QuestionnaireFR$CSP, QuestionnaireFR$Chances, chisq=TRUE)





#Are there educational differences ? 
##______________________________________________________________________________________________________________________

Educ1 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Freqtalk, chisq=TRUE)
#P dans les 10-4


Educ2 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$CauseCC, chisq=TRUE)

Educ3 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$EffectsCC, chisq=TRUE)

Educ4 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$RegionCC, chisq=TRUE)

Educ5 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$VisioncoopCC, chisq=TRUE)

Educ6 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$CBDR, chisq=TRUE)
#P dans les 10-3


Educ7 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Gdfr, chisq=TRUE)
#P=0.03


Educ8 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Principles_past_em, chisq=TRUE)

Educ9 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Principles_epc, chisq=TRUE)

Educ10 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Principles_gdfr, chisq=TRUE)
#P=0.03


Educ11 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Principles_poor, chisq=TRUE)
#P=0.029


Educ12 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Principles_cost, chisq=TRUE)

Educ13 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Statements_forests, chisq=TRUE)

Educ14 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Statements_funding, chisq=TRUE)

Educ15 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Statements_tech, chisq=TRUE)
#P=0.048


Educ16 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Statements_trigger, chisq=TRUE)
#P=0.017


Educ17 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Accountability, chisq=TRUE)

Educ18 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$History, chisq=TRUE)

Educ19 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$CaseComprehension, chisq=TRUE)
#P dans les 10-3


Educ20 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case1.SQ001., chisq=TRUE)
#P=0.025


Educ21 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case1.SQ002., chisq=TRUE)

Educ22 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case2.SQ001., chisq=TRUE)
#P=0.03


Educ23 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case2.SQ002., chisq=TRUE)

Educ24 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case3.SQ001., chisq=TRUE)

Educ25 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case3.SQ002., chisq=TRUE)

Educ26 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case4.SQ001., chisq=TRUE)
#P dans les 10-3


Educ27 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case4.SQ002., chisq=TRUE)
#P dans les 10-3


Educ28 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case5.SQ001., chisq=TRUE)
#P=0.025


Educ29 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case5.SQ002., chisq=TRUE)
#P=0.04


Educ30 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case6.SQ001., chisq=TRUE)

Educ31 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Case6.SQ002., chisq=TRUE)
#P dans les 10-5


Educ32 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Coord_China, chisq=TRUE)

Educ33 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Coord_India, chisq=TRUE)

Educ34 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Coord_EU, chisq=TRUE)
#P=0.02


Educ35 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Coord_USA, chisq=TRUE)

Educ36 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Reward.SQ001., chisq=TRUE)

Educ37 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Reward.SQ002., chisq=TRUE)

Educ38 <- CrossTable (QuestionnaireFR$Educ, QuestionnaireFR$Chances, chisq=TRUE)
#P=0.018



#Coordination
change1 <- CrossTable (QuestionnaireFR$Changecat, QuestionnaireFR$Gender, chisq=TRUE)


change2 <- CrossTable (QuestionnaireFR$Changecat, QuestionnaireFR$Income, chisq=TRUE)


change3 <- CrossTable (QuestionnaireFR$Changecat, QuestionnaireFR$Chances, chisq=TRUE)


change4 <- CrossTable (QuestionnaireFR$Changecat, QuestionnaireFR$CBDR, chisq=TRUE)


change4 <- CrossTable (QuestionnaireFR$Changecat, QuestionnaireFR$EffectsCC, chisq=TRUE)

#Are there differences when removing climatoskeptics ? 
##______________________________________________________________________________________________________________________

cause1 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Freqtalk, chisq=TRUE)
#people who think CC is human caused are underrepresented among people who never talk about CC

cause2 <- CrossTable (QuestionnaireFR$Gender, QuestionnaireFR$CauseCC, chisq=TRUE)
cause3 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$EffectsCC, chisq=TRUE)
#"human caused" is overrepresented among people who think CC will be disastrous

cause4 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$RegionCC, chisq=TRUE)
#'human caused" overrepresented among people who think both will be touched

CauseCC5 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$VisioncoopCC, chisq=TRUE)
#"human caused" underrepresented among people with an internationalist vision

CauseCC6 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$CBDR, chisq=TRUE)
#"human caused" underrepresented among none of the above

CauseCC7 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Gdfr, chisq=TRUE)
#"human caused" overrepresented among people who strongly disagree

CauseCC8 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Principles_past_em, chisq=TRUE)
#"not a reality" overrepresented among people who strongly agree

CauseCC9 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Principles_epc, chisq=TRUE)
#"human caused" underrepresented among people who strongly disagree

CauseCC10 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Principles_gdfr, chisq=TRUE)
#not a reality overrepresented among people who strongly disagree

CauseCC11 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Principles_poor, chisq=TRUE)
CauseCC12 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Principles_cost, chisq=TRUE)
#not a reality overrepresented among people who strongly agree

CauseCC13 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Statements_forests, chisq=TRUE)
#human caused overrepresented among people who disagree

CauseCC14 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Statements_funding, chisq=TRUE)
#not a reality overrepresented among people who strongly disagree

CauseCC15 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Statements_tech, chisq=TRUE)
#human caused overrepresented among people who disagree

CauseCC16 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Statements_trigger, chisq=TRUE)
#not a reality overrepresented among people who strongly agree


CauseCC17 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Accountability, chisq=TRUE)
#human caused overrepresented among people who favour the footprint

CauseCC18 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$History, chisq=TRUE)
#human caused underrepresented among people in favour of current emissions

CauseCC19 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$CaseComprehension, chisq=TRUE)
#human caused overrepresented among perfect comprehension

CauseCC20 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case1.SQ001., chisq=TRUE)

CauseCC21 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case1.SQ002., chisq=TRUE)

CauseCC22 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case2.SQ001., chisq=TRUE)

CauseCC23 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case2.SQ002., chisq=TRUE)

CauseCC24 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case3.SQ001., chisq=TRUE)

CauseCC25 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case3.SQ002., chisq=TRUE)

CauseCC26 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case4.SQ001., chisq=TRUE)

CauseCC27 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case4.SQ002., chisq=TRUE)

CauseCC28 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case5.SQ001., chisq=TRUE)

CauseCC29 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case5.SQ002., chisq=TRUE)

CauseCC30 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case6.SQ001., chisq=TRUE)

CauseCC31 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Case6.SQ002., chisq=TRUE)

CauseCC32 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Coord_China, chisq=TRUE)

CauseCC33 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Coord_India, chisq=TRUE)

CauseCC34 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Coord_EU, chisq=TRUE)

CauseCC35 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Coord_USA, chisq=TRUE)

CauseCC36 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Reward.SQ001., chisq=TRUE)

CauseCC37 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Reward.SQ002., chisq=TRUE)

CauseCC38 <- CrossTable (QuestionnaireFR$CauseCC, QuestionnaireFR$Chances, chisq=TRUE)




