install.packages("ggplot2")
install.packages("readxl")
install.packages("survey")
install.packages("RColorBrewer")
install.packages("questionr", dependencies=TRUE)
install.packages("gmodels")
install.packages("alluvial")
install.packages('stringr')
install.packages("ggalluvial")


library(ggplot2)
library(readxl)
library(survey)
library(RColorBrewer)
library(questionr)
library(gmodels)
library(alluvial)
library(stringr)
library(ggalluvial)






##Getting the data

setwd("C:/Users/Auriane Meilland/Documents/2. Projet questionnaire Yann/Projet Questionnaire - Stage CIRED")
QuestionnaireUS <- read.csv2("results-surveyUS.csv", encoding = "UTF-8")
QuestionnaireUS <- data.frame(QuestionnaireUS)





##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Recoding data (in the order of the survey)
##_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#Reorganizing the income levels (for display purposes), transforming variables into factors
QuestionnaireUS$Income <- factor(QuestionnaireUS$Income, levels= c("Less than $20,000", "$20,000 to $39,999", "$40,000 to $74,999", "More than $75,000"))
QuestionnaireUS$Gender<- as.factor(QuestionnaireUS$Gender)
QuestionnaireUS$Age<-as.factor(QuestionnaireUS$Age)


#Recoding States into larger regions for quota purposes
QuestionnaireUS$Region[QuestionnaireUS$State  =="Illinois - IL" | QuestionnaireUS$State  =="Indiana - IN"| QuestionnaireUS$State  =="Iowa - IA"| QuestionnaireUS$State  =="Kansas - KS"| QuestionnaireUS$State  =="Michigan - MI"| QuestionnaireUS$State  =="Minnesota - MN"| QuestionnaireUS$State  =="Missouri - MO"| QuestionnaireUS$State  =="Nebraska - NE"| QuestionnaireUS$State  =="North Dakota - ND"| QuestionnaireUS$State  =="Ohio - OH"| QuestionnaireUS$State  =="South Dakota - SD"| QuestionnaireUS$State  =="Wisconsin - WI"]  <- "Midwest"
QuestionnaireUS$Region[QuestionnaireUS$State == "Alabama - AL" | QuestionnaireUS$State == "Arkansas - AR" | QuestionnaireUS$State == "Washington - DC"| QuestionnaireUS$State == "Delaware - DE"| QuestionnaireUS$State == "Florida - FL"| QuestionnaireUS$State == "Georgia - GA"| QuestionnaireUS$State == "Kentucky - KY"| QuestionnaireUS$State == "Louisiana - LA"| QuestionnaireUS$State == "Maryland - MD"| QuestionnaireUS$State == "Mississipi - MS"| QuestionnaireUS$State == "North Carolina - NC"| QuestionnaireUS$State == "South Carolina - SC"| QuestionnaireUS$State == "Oklahoma - OK"| QuestionnaireUS$State == "Tennessee - TN"| QuestionnaireUS$State == "Texas - TX"| QuestionnaireUS$State == "Virginia - VA"| QuestionnaireUS$State == "West Virginia - WV"] <- "South"
QuestionnaireUS$Region[QuestionnaireUS$State == "Connecticut - CT" | QuestionnaireUS$State == "Maine - ME" | QuestionnaireUS$State == "Massaschusetts - MA"| QuestionnaireUS$State == "New Hampshire - NH"| QuestionnaireUS$State == "New Jersey - NJ"| QuestionnaireUS$State == "New York - NY"| QuestionnaireUS$State == "Pennsylvania - PA"| QuestionnaireUS$State == "Rhode Island - RI"| QuestionnaireUS$State == "Vermont - VT"] <- "Northeast/Northwest"
QuestionnaireUS$Region[QuestionnaireUS$State == "Alaska - AK" | QuestionnaireUS$State == "Arizona - AZ" | QuestionnaireUS$State == "California - CA"| QuestionnaireUS$State == "Colorado - CO"| QuestionnaireUS$State == "Hawaii - HI"| QuestionnaireUS$State == "Idaho - ID"| QuestionnaireUS$State == "Montana - MT"| QuestionnaireUS$State == "Nevada - NV"| QuestionnaireUS$State == "New Mexico - NM"| QuestionnaireUS$State == "Oregon - OR"| QuestionnaireUS$State == "Utah - UT"| QuestionnaireUS$State == "Washington - WA"| QuestionnaireUS$State == "Wyoming - WY"]<- "West"
QuestionnaireUS$Region <- as.factor(QuestionnaireUS$Region)


#Renaming agglomeration categories
QuestionnaireUS$Agglo[QuestionnaireUS$Agglo=="In a rural area or a small town"]<-"Rural/small"
QuestionnaireUS$Agglo[QuestionnaireUS$Agglo=="In a suburban area on the outskirts of a big city, or in a medium-sized city"]<-"Suburb/medium"
QuestionnaireUS$Agglo[QuestionnaireUS$Agglo=="In an urban area of a big city"]<-"Urban/big"
QuestionnaireUS$Agglo<-as.factor(QuestionnaireUS$Agglo)


#Reorganising the frequency at which respondents talkd about clmate change
levels(QuestionnaireUS$Freqtalk)[levels(QuestionnaireUS$Freqtalk)==""] <- "No answer"
QuestionnaireUS$Freqtalk <- factor(QuestionnaireUS$Freqtalk, levels= c("No answer", "Several times a week", "Several times a month", "Several times a year", "Almost never"))
QuestionnaireUS$Freqtalk[is.na(QuestionnaireUS$Freqtalk)] <- "No answer"


#Renaming the perceived cause of climate change
QuestionnaireUS$CauseCC[QuestionnaireUS$CauseCC=="is not a reality"] <- "Not a reality"
QuestionnaireUS$CauseCC[QuestionnaireUS$CauseCC=="is mainly due to natural climate variability"] <- "Nature caused"
QuestionnaireUS$CauseCC[QuestionnaireUS$CauseCC=="is mainly due to human activity"] <- "Human caused"


#Renaming gases causing climate change
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ001."] <- "GazCC_CO2"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ002."] <- "GazCC_CH4"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ003."] <- "GazCC_O2"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ004."] <- "GazCC_H2O"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ006."] <- "GazCC_N2O"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GazCC.SQ005."] <- "GazCC_PM"


#Recoding gases into a level of knowlege about climate change
QuestionnaireUS$Knowledge <- "None"
QuestionnaireUS$Knowledge[QuestionnaireUS$GazCC_CO2=="Yes"]<-"Basic"
QuestionnaireUS$Knowledge[QuestionnaireUS$GazCC_CO2=="Yes"& QuestionnaireUS$GazCC_CH4=="Yes"]<-"Intermediate"
QuestionnaireUS$Knowledge[QuestionnaireUS$GazCC_CO2=="Yes"& QuestionnaireUS$GazCC_CH4=="Yes"& QuestionnaireUS$GazCC_N2O =="Yes"]<-"Advanced"
QuestionnaireUS$Knowledge <- factor(QuestionnaireUS$Knowledge, levels= c("None", "Basic", "Intermediate", "Advanced"))


#Reorganizing the perceived effects (for display purposes)
QuestionnaireUS$EffectsCC <- factor(QuestionnaireUS$EffectsCC, levels= c("Insignificant", "Small", "Significant", "Disastrous"))


#Renaming the region most touched by climate change
QuestionnaireUS$RegionCC[QuestionnaireUS$RegionCC=="The USA"] <- "USA"
QuestionnaireUS$RegionCC[QuestionnaireUS$RegionCC=="Similar consequences in both regions"] <- "Both"
QuestionnaireUS$RegionCC[QuestionnaireUS$RegionCC=="The European Union"] <- "EU"


#Recoding affected generations
names(QuestionnaireUS)[names(QuestionnaireUS) == "GenerationCC.SQ001."] <- "Born_1960s"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GenerationCC.SQ002."] <- "Born_1990s"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GenerationCC.SQ003."] <- "Born_2020s"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GenerationCC.SQ004."] <- "Born_2050s"
names(QuestionnaireUS)[names(QuestionnaireUS) == "GenerationCC.SQ005."] <- "None"


#Recoding touched generations into the first affected generation
QuestionnaireUS$FirstGen[QuestionnaireUS$Born_1960s=="Yes"]<-"1960s"
QuestionnaireUS$FirstGen[QuestionnaireUS$Born_1960s=="No"& QuestionnaireUS$Born_1990s=="Yes"]<-"1990s"
QuestionnaireUS$FirstGen[QuestionnaireUS$Born_1960s=="No"& QuestionnaireUS$Born_1990s=="No"& QuestionnaireUS$Born_2020s =="Yes"]<-"2020s"
QuestionnaireUS$FirstGen[QuestionnaireUS$Born_1960s=="No"& QuestionnaireUS$Born_1990s=="No"& QuestionnaireUS$Born_2020s =="No"& QuestionnaireUS$Born_2050s=="Yes"]<-"2050s"
QuestionnaireUS$FirstGen[QuestionnaireUS$None=="Yes"]<-"None of the above"


#Renaming people's (national or international) vision about climate change (for display purposes)
QuestionnaireUS$VisioncoopCC[QuestionnaireUS$VisioncoopCC=="Principles of climate justice should never prevail over countries’ own national interests, even if it may impede the fight against climate change"]<-"National Scale"
QuestionnaireUS$VisioncoopCC[QuestionnaireUS$VisioncoopCC=="Principles of climate justice should be settled at the international level, even if that may prevent countries from pursuing the interest of their own people"]<- "International Scale"
QuestionnaireUS$VisioncoopCC<- as.factor(QuestionnaireUS$VisioncoopCC)

#Renaming people's preferences for CBDR (for display purposes)
QuestionnaireUS$CBDR[QuestionnaireUS$CBDR=="Countries share common but differentiated responsibilities. However, if I had to choose, I would consider that countries mostly hold common responsibilities, which means that all countries, without exception, should act to limit climate change."]<-"Common"
QuestionnaireUS$CBDR[QuestionnaireUS$CBDR=="Countries share common but differentiated responsibilities. However, if I had to choose, I would consider that countries mostly hold differentiated responsibilities, which means that some should make more efforts than others to combat climate change."]<-"Differentiated"


#Reorganizing people's opinions about grandfathering
QuestionnaireUS$Gdfr <- factor(QuestionnaireUS$Gdfr, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))


#Reorganizing and changing the judgements about principles
names(QuestionnaireUS)[names(QuestionnaireUS) == "Principles.SQ001."] <- "Principles_past_em"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Principles.SQ002."] <- "Principles_epc"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Principles.SQ003."] <- "Principles_gdfr"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Principles.SQ004."] <- "Principles_poor"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Principles.SQ005."] <- "Principles_cost"
QuestionnaireUS$Principles_past_em <- factor(QuestionnaireUS$Principles_past_em, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Principles_epc <- factor(QuestionnaireUS$Principles_epc, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Principles_gdfr <- factor(QuestionnaireUS$Principles_gdfr, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Principles_poor <- factor(QuestionnaireUS$Principles_poor, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Principles_cost <- factor(QuestionnaireUS$Principles_cost, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))


#Recoding people in to a "high tolerance" or "protest" to principles
QuestionnaireUS$Tolerance<-"Not particularly tolerant"
QuestionnaireUS$Tolerance[(QuestionnaireUS$Principles_epc=="I strongly agree" | QuestionnaireUS$Principles_epc=="I rather agree") & (QuestionnaireUS$Principles_cost=="I strongly agree" | QuestionnaireUS$Principles_cost=="I rather agree")&(QuestionnaireUS$Principles_gdfr=="I strongly agree" | QuestionnaireUS$Principles_gdfr=="I rather agree")&(QuestionnaireUS$Principles_poor=="I strongly agree" | QuestionnaireUS$Principles_poor=="I rather agree")&(QuestionnaireUS$Principles_past_em=="I strongly agree" | QuestionnaireUS$Principles_past_em=="I rather agree")] <- "Tolerant"
QuestionnaireUS$Tolerance[(QuestionnaireUS$Principles_epc=="I strongly disagree" | QuestionnaireUS$Principles_epc=="I rather disagree") & (QuestionnaireUS$Principles_cost=="I strongly disagree" | QuestionnaireUS$Principles_cost=="I rather disagree")&(QuestionnaireUS$Principles_gdfr=="I strongly disagree" | QuestionnaireUS$Principles_gdfr=="I rather disagree")&(QuestionnaireUS$Principles_poor=="I strongly disagree" | QuestionnaireUS$Principles_poor=="I rather disagree")&(QuestionnaireUS$Principles_past_em=="I strongly disagree" | QuestionnaireUS$Principles_past_em=="I rather disagree")] <- "Intolerant"


#Reorganizing and changing the judgements about statements
names(QuestionnaireUS)[names(QuestionnaireUS) == "Statements.SQ001."] <- "Statements_forests"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Statements.SQ002."] <- "Statements_trigger"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Statements.SQ003."] <- "Statements_funding"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Statements.SQ004."] <- "Statements_tech"
QuestionnaireUS$Statements_forests <- factor(QuestionnaireUS$Statements_forests, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Statements_trigger <- factor(QuestionnaireUS$Statements_trigger, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Statements_funding <- factor(QuestionnaireUS$Statements_funding, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Statements_tech <- factor(QuestionnaireUS$Statements_tech, levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))


#Changement de nom de la question empreinte C vs émissions territoriales
QuestionnaireUS$Accountability[QuestionnaireUS$Accountability=="Countries should only be held accountable for the carbon emissions that occur on their territories"] <- "Territory"
QuestionnaireUS$Accountability[QuestionnaireUS$Accountability=="Countries should be held accountable for the carbon emissions of all the products they consume, including the emissions of imported products that occur abroad"] <- "Footprint"


#Réorganisation et changement de nom de l'historique
QuestionnaireUS$History[QuestionnaireUS$History=="Countries should only be held accountable for their current emissions"] <- "Current emissions"
QuestionnaireUS$History[QuestionnaireUS$History=="Countries should be held accountable for their historical emissions since 1990, when it was scientifically established that climate change was real and caused by human activities"] <- "Since 1990"
QuestionnaireUS$History[QuestionnaireUS$History=="Countries should be held for their historical emissions since 1850, when humans started emitting large quantities of carbon in the atmosphere"] <- "Since 1850"
QuestionnaireUS$History <- factor(QuestionnaireUS$History, levels= c("Current emissions", "Since 1990", "Since 1850"))




#Recodage des tests
QuestionnaireUS$CaseComprehension[QuestionnaireUS$Test.SQ001.=="False" & QuestionnaireUS$Test.SQ002.=="True" & QuestionnaireUS$Test.SQ003.=="False" & QuestionnaireUS$Test.SQ004.=="True"]<- "Perfect Comprehension"
QuestionnaireUS$CaseComprehension[QuestionnaireUS$Test.SQ001.=="False" & QuestionnaireUS$Test.SQ002.=="True" & QuestionnaireUS$Test.SQ003.=="True" & QuestionnaireUS$Test.SQ004.=="True"]<- "Misread climate change"
QuestionnaireUS$CaseComprehension[QuestionnaireUS$Test.SQ001.=="True" | QuestionnaireUS$Test.SQ002.=="False" | QuestionnaireUS$Test.SQ004.=="False"]<- "One or several errors"
QuestionnaireUS$CaseComprehension <- as.factor(QuestionnaireUS$CaseComprehension)




#Réorganisation des Cas Particuliers
QuestionnaireUS$Case1.SQ001. <- factor(QuestionnaireUS$Case1.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case1.SQ002. <- factor(QuestionnaireUS$Case1.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case2.SQ001. <- factor(QuestionnaireUS$Case2.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case2.SQ002. <- factor(QuestionnaireUS$Case2.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case3.SQ001. <- factor(QuestionnaireUS$Case3.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case3.SQ002. <- factor(QuestionnaireUS$Case3.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case4.SQ001. <- factor(QuestionnaireUS$Case4.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case4.SQ002. <- factor(QuestionnaireUS$Case4.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case5.SQ001. <- factor(QuestionnaireUS$Case5.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case5.SQ002. <- factor(QuestionnaireUS$Case5.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case6.SQ001. <- factor(QuestionnaireUS$Case6.SQ001., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))
QuestionnaireUS$Case6.SQ002. <- factor(QuestionnaireUS$Case6.SQ002., levels= c("I strongly agree", "I rather agree", "I rather disagree", "I strongly disagree"))


#Réorganisation et changement de nom coordination
names(QuestionnaireUS)[names(QuestionnaireUS) == "Coordination.SQ001."] <- "Coord_China"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Coordination.SQ002."] <- "Coord_India"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Coordination.SQ003."] <- "Coord_EU"
names(QuestionnaireUS)[names(QuestionnaireUS) == "Coordination.SQ004."] <- "Coord_USA" 
QuestionnaireUS$Coord_China <- factor(QuestionnaireUS$Coord_China, levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))
QuestionnaireUS$Coord_India <- factor(QuestionnaireUS$Coord_India, levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))
QuestionnaireUS$Coord_EU <- factor(QuestionnaireUS$Coord_EU, levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))
QuestionnaireUS$Coord_USA <- factor(QuestionnaireUS$Coord_USA, levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))


#Judgement paths
QuestionnaireUS$Path[QuestionnaireUS$Coord_China=="I strongly agree"]<-"a"
QuestionnaireUS$Path[QuestionnaireUS$Coord_China=="I partially agree"]<-"b"
QuestionnaireUS$Path[QuestionnaireUS$Coord_China=="I partially disagree"]<-"c"
QuestionnaireUS$Path[QuestionnaireUS$Coord_China=="I strongly disagree"]<-"d"

QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I strongly agree"]<- paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I strongly agree"], "a")
QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I partially agree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I partially agree"], "b")
QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I partially disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I partially disagree"], "c")
QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I strongly disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_India=="I strongly disagree"], "d")

QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I strongly agree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I strongly agree"], "a")
QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I partially agree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I partially agree"], "b")
QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I partially disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I partially disagree"], "c")
QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I strongly disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_EU=="I strongly disagree"], "d")

QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I strongly agree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I strongly agree"], "a")
QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I partially agree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I partially agree"], "b")
QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I partially disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I partially disagree"], "c")
QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I strongly disagree"]<-paste(QuestionnaireUS$Path[QuestionnaireUS$Coord_USA=="I strongly disagree"], "d")


#Paths categorization
QuestionnaireUS$Pathcat<-"Agree with two"
QuestionnaireUS$Pathcat[str_count(QuestionnaireUS$Path, "c")==0 & str_count(QuestionnaireUS$Path, "d")==0] <- "Agree with all"
QuestionnaireUS$Pathcat[str_count(QuestionnaireUS$Path, "a")==0 & str_count(QuestionnaireUS$Path, "b")==0] <- "Agree with none"
QuestionnaireUS$Pathcat[(str_count(QuestionnaireUS$Path, "a")==1 & str_count(QuestionnaireUS$Path, "b")==0) | (str_count(QuestionnaireUS$Path, "a")==0 & str_count(QuestionnaireUS$Path, "b")==1)] <- "Agree with one"
QuestionnaireUS$Pathcat[(str_count(QuestionnaireUS$Path, "c")==1 & str_count(QuestionnaireUS$Path, "d")==0) | (str_count(QuestionnaireUS$Path, "c")==0 & str_count(QuestionnaireUS$Path, "d")==1)] <- "Agree with three"



#réorganisation reward
QuestionnaireUS$Reward.SQ001. <- factor(QuestionnaireUS$Reward.SQ001., levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))
QuestionnaireUS$Reward.SQ002. <- factor(QuestionnaireUS$Reward.SQ002., levels= c("I strongly agree", "I partially agree", "I partially disagree", "I strongly disagree"))


#réorganisation chances
QuestionnaireUS$Chances <- factor(QuestionnaireUS$Chances, levels= c("less than 1%", "1 to 10%", "11 to 49%", "50 to 90%","90 to 99%","more than 99%","other"))
QuestionnaireUS$Chances[is.na(QuestionnaireUS$Chances)] <- "other"

#Coordination 
QuestionnaireUS$Coord<-"Changed their mind"
QuestionnaireUS$Coord[QuestionnaireUS$Coord_USA==QuestionnaireUS$Reward.SQ002. & QuestionnaireUS$Coord_EU==QuestionnaireUS$Reward.SQ001.] <- "Same opinion"

#Changed category
QuestionnaireUS$Changecat<-"other"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I strongly agree" & QuestionnaireUS$Reward.SQ001.=="I strongly agree"]<-"1"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I partially agree" & QuestionnaireUS$Reward.SQ001.=="I partially agree"]<-"2"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I strongly agree" & QuestionnaireUS$Reward.SQ001.=="I partially agree"]<-"3"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I partially agree" & QuestionnaireUS$Reward.SQ001.=="I strongly agree"]<-"4"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I partially disagree" & QuestionnaireUS$Reward.SQ001.=="I partially disagree"]<-"5"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I partially disagree" & QuestionnaireUS$Reward.SQ001.=="I partially agree"]<-"6"
QuestionnaireUS$Changecat[QuestionnaireUS$Coord_EU=="I partially agree" & QuestionnaireUS$Reward.SQ001.=="I partially disagree"]<-"7"


QuestionnaireUS$Changecat2<-"other"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I strongly agree" & QuestionnaireUS$Reward.SQ002.=="I strongly agree"]<-"1"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I partially agree" & QuestionnaireUS$Reward.SQ002.=="I partially agree"]<-"2"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I strongly agree" & QuestionnaireUS$Reward.SQ002.=="I partially agree"]<-"3"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I partially agree" & QuestionnaireUS$Reward.SQ002.=="I strongly agree"]<-"4"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I partially disagree" & QuestionnaireUS$Reward.SQ002.=="I partially disagree"]<-"5"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I partially disagree" & QuestionnaireUS$Reward.SQ002.=="I partially agree"]<-"6"
QuestionnaireUS$Changecat2[QuestionnaireUS$Coord_USA=="I partially agree" & QuestionnaireUS$Reward.SQ002.=="I partially disagree"]<-"7"


#CSP
QuestionnaireUS$CSP[QuestionnaireUS$CSP =="Farmer"| QuestionnaireUS$CSP =="Craftsman/merchant"| QuestionnaireUS$CSP =="Large employer (10+ employees)"| QuestionnaireUS$CSP =="Higher grade professional, administrative and managerial occupation (engineer, doctor, consultant, lawyer etc)"| QuestionnaireUS$CSP =="Lower grade professional, administrative and managerial occupation, intermediate or teaching occupation"]<-"CSP+"
QuestionnaireUS$CSP[QuestionnaireUS$CSP=="Employee (office, service, sales and clerical occupation)"| QuestionnaireUS$CSP =="Worker"]<-"CSP-"
QuestionnaireUS$CSP[QuestionnaireUS$CSP=="Other (not employed)"| QuestionnaireUS$CSP =="Student"| QuestionnaireUS$CSP =="Retired"]<-"Inactive"





#Recodage pour logit
QuestionnaireUS$Common<-FALSE
QuestionnaireUS$Common[QuestionnaireUS$CBDR=="Common"]<-TRUE

QuestionnaireUS$Freqq<-"No answer/almost never"
QuestionnaireUS$Freqq[QuestionnaireUS$Freqtalk=="Several times a week"] <- "Several times a week"
QuestionnaireUS$Freqq[QuestionnaireUS$Freqtalk=="Several times a month"] <- "Several times a month"
QuestionnaireUS$Freqq[QuestionnaireUS$Freqtalk=="Several times a year"] <- "Several times a year"

QuestionnaireUS$International<-FALSE
QuestionnaireUS$International[QuestionnaireUS$VisioncoopCC=="International Scale"]<-TRUE


##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Spécification du design (défaut)
##__________________________________________________________________________________________________________________________________________________________________________________________________

design_US  <-svydesign(ids=~1, data=QuestionnaireUS)








##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Stats de base // affichage graphique 
##__________________________________________________________________________________________________________________________________________________________________________________________________


## Données répondant
##_______________________



#Genre
gender <-svymean(~Gender, design=design_US)

#Age
age <-svymean(~Age, design=design_US)

#Income
income <-svymean(~Income, design=design_US)

#State et region
state <-svymean(~State, design=design_US)
region <-svymean(~Region, design=design_US)

#Agglo
agglo <-svymean(~Agglo, design=design_US)


### Check for representativity
check_income <- data.frame(summary(QuestionnaireUS$Income))
check_income$rpz <- c(140, 190, 280, 390)
chisq.test(check_income)


check_gender <- data.frame(summary(QuestionnaireUS$Gender))
check_gender$rpz <- c(500,500)
chisq.test(check_gender)


check_age <- data.frame(summary(QuestionnaireUS$Age))
check_age$rpz <- c(120, 180,160,160, 170,200)
chisq.test(check_age)


check_region <- data.frame(summary(QuestionnaireUS$Region))
check_region$rpz <- c(220, 180, 370, 230)
chisq.test(check_region)


check_agglo <- data.frame(summary(QuestionnaireUS$Agglo))
check_agglo$rpz <- c(220,520,260)
chisq.test(check_agglo)


## Avis et convictions
##_______________________



#Freqtalk
freqtalk <-data.frame(svymean(~Freqtalk, design=design_US))
freqtalk2 <-data.frame(confint(svymean(~Freqtalk, design=design_US), level=0.95))
freqtalk$name <-factor (rownames(freqtalk), levels= c("FreqtalkNo answer", "FreqtalkSeveral times a week", "FreqtalkSeveral times a month", "FreqtalkSeveral times a year", "FreqtalkAlmost never"))
graph_freqtalk <- ggplot(freqtalk,aes(x=name,y=mean, fill=name))
graph_freqtalk + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=freqtalk2$X2.5.. , ymax=freqtalk2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Blues")+ theme(legend.position = "none") + scale_x_discrete(labels=c("FreqtalkNo answer"="No answer","FreqtalkSeveral times a week"="Several times \na week","FreqtalkSeveral times a month"=" Several times \na month","FreqtalkSeveral times a year"=" Several times \na year","FreqtalkAlmost never"="Almost Never")) + coord_cartesian(ylim=c(0,0.42))



#Cause CC
causeCC <- data.frame(svymean(~CauseCC, design=design_US))
causeCC2 <- data.frame(confint(svymean(~CauseCC, design=design_US), level=0.95))
causeCC$name <-factor (rownames(causeCC), levels= c("CauseCC", "CauseCCHuman caused", "CauseCCNature caused", "CauseCCNot a reality"))
graph_causeCC <- ggplot(causeCC,aes(x=name,y=mean, fill=name))
graph_causeCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=causeCC2$X2.5.., ymax=causeCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CauseCC"="No answer", "CauseCCHuman caused"="Human activity", "CauseCCNature caused"="Natural variability", "CauseCCNot a reality"= "Is not a reality")) + coord_cartesian(ylim=c(0,0.8))


#Gaz CC
gazcc_CO2 <- data.frame(svymean(~GazCC_CO2, design=design_US))
gazcc_CO2$ci<-data.frame(confint(svymean(~GazCC_CO2, design=design_US)))$X2.5..
gazcc_CO2$cs<-data.frame(confint(svymean(~GazCC_CO2, design=design_US)))$X97.5..
gazcc_CH4 <-data.frame(svymean(~GazCC_CH4, design=design_US))
gazcc_CH4$ci<-data.frame(confint(svymean(~GazCC_CH4, design=design_US)))$X2.5..
gazcc_CH4$cs<-data.frame(confint(svymean(~GazCC_CH4, design=design_US)))$X97.5..
gazcc_O2 <-data.frame(svymean(~GazCC_O2, design=design_US))
gazcc_O2$ci<-data.frame(confint(svymean(~GazCC_O2, design=design_US)))$X2.5..
gazcc_O2$cs<-data.frame(confint(svymean(~GazCC_O2, design=design_US)))$X97.5..
gazcc_H2O <-data.frame(svymean(~GazCC_H2O, design=design_US))
gazcc_H2O$ci<-data.frame(confint(svymean(~GazCC_H2O, design=design_US)))$X2.5..
gazcc_H2O$cs<-data.frame(confint(svymean(~GazCC_H2O, design=design_US)))$X97.5..
gazcc_N2O <-data.frame(svymean(~GazCC_N2O, design=design_US))
gazcc_N2O$ci<-data.frame(confint(svymean(~GazCC_N2O, design=design_US)))$X2.5..
gazcc_N2O$cs<-data.frame(confint(svymean(~GazCC_N2O, design=design_US)))$X97.5..
gazcc_PM <-data.frame(svymean(~GazCC_PM, design=design_US))
gazcc_PM$ci<-data.frame(confint(svymean(~GazCC_PM, design=design_US)))$X2.5..
gazcc_PM$cs<-data.frame(confint(svymean(~GazCC_PM, design=design_US)))$X97.5..
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
graph_gazcc + geom_bar(position=position_dodge(),stat="identity",color="black")+ labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2") + theme(legend.position = "none")+ coord_cartesian(ylim=c(0,1))


#Knowledge CC
knowledgeCC <- data.frame(svymean(~Knowledge, design=design_US))
knowledgeCC2<- data.frame(confint(svymean(~Knowledge, design=design_US)))
knowledgeCC$name <-factor (rownames(knowledgeCC), levels= c("KnowledgeNone", "KnowledgeBasic", "KnowledgeIntermediate", "KnowledgeAdvanced"))
graph_knowledgeCC <- ggplot(knowledgeCC,aes(x=name,y=mean, fill=name))
graph_knowledgeCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=knowledgeCC2$X2.5.., ymax=knowledgeCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("KnowledgeNone"= "None", "KnowledgeBasic"= "Basic", "KnowledgeIntermediate"= "Intermediate", "KnowledgeAdvanced"="Advanced")) + coord_cartesian(ylim=c(0,0.4))



#Effects CC
effectsCC <-data.frame(svymean(~EffectsCC, design=design_US))
effectsCC2 <-data.frame(confint(svymean(~EffectsCC, design=design_US)))
effectsCC$name <-factor (rownames(effectsCC), levels= c("EffectsCCInsignificant", "EffectsCCSmall", "EffectsCCSignificant", "EffectsCCDisastrous"))
graph_effectsCC <- ggplot(effectsCC,aes(x=name,y=mean, fill=name))
graph_effectsCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=effectsCC2$X2.5.., ymax=effectsCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("EffectsCCInsignificant"="Insignificant", "EffectsCCSmall"="Small", "EffectsCCSignificant"="Significant", "EffectsCCDisastrous"="Disastrous")) + coord_cartesian(ylim=c(0,0.6))

#Effects CC by age 
CrossTable(QuestionnaireUS$EffectsCC,QuestionnaireUS$Age, chisq=TRUE)
#P=0.045, 

#disastrous by age
effects_age <-svyby(~EffectsCC,by=~Age,design=design_US,FUN=svymean,vartype=c('ci'))
graph_effect_age <- ggplot(effects_age,aes(x=effects_age$Age,y=effects_age$EffectsCCDisastrous, fill=effects_age$Age))
graph_effect_age + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who think climate change will be disastrous (US)", x="Age") + geom_errorbar(aes(ymin=effects_age$ci_l.EffectsCCDisastrous, ymax=effects_age$ci_u.EffectsCCDisastrous), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + coord_cartesian(ylim=c(0,0.7))

graph_effect_age2 <- ggplot(effects_age,aes(x=effects_age$Age,y=effects_age$EffectsCCSmall, fill=effects_age$Age))
graph_effect_age2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who think climate change will be small (US)", x="Age") + geom_errorbar(aes(ymin=effects_age$ci_l.EffectsCCSmall, ymax=effects_age$ci_u.EffectsCCSmall), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + coord_cartesian(ylim=c(0,0.7))




#RegionCC
regionCC <-data.frame(svymean(~RegionCC, design=design_US))
regionCC2 <- data.frame(confint(svymean(~RegionCC, design=design_US)))
regionCC$name <-factor (rownames(regionCC), levels= c("RegionCCBoth", "RegionCCEU", "RegionCCUSA"))
graph_regionCC <- ggplot(regionCC,aes(x=name,y=mean, fill=name))
graph_regionCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=regionCC2$X2.5.., ymax=regionCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("RegionCCBoth"="Similar consequences in both regions", "RegionCCEU"="The European Union", "RegionCCUSA"="The USA")) + coord_cartesian(ylim=c(0,0.8))




#Generation CC
generationCC <- data.frame(svymean(~FirstGen, design=design_US))
generationCC2 <- data.frame(confint(svymean(~FirstGen, design=design_US)))
generationCC$name <-factor (rownames(generationCC), levels= c("FirstGen1960s", "FirstGen1990s", "FirstGen2020s", "FirstGen2050s", "FirstGenNone of the above"))
graph_generationCC <- ggplot(generationCC,aes(x=name,y=mean, fill=name))
graph_generationCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=generationCC2$X2.5.., ymax=generationCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("FirstGen1960s"="1960s", "FirstGen1990s"="1990s", "FirstGen2020s"="2020s", "FirstGen2050s"="2050s", "FirstGenNone of the above"="None")) + coord_cartesian(ylim=c(0,0.4))

#generation by age
CrossTable(QuestionnaireUS$FirstGen,QuestionnaireUS$Age, chisq=TRUE)
#P=0.054


#VisioncoopCC
visionCC <-data.frame(svymean(~VisioncoopCC, design=design_US))
visionCC2 <-data.frame(confint(svymean(~VisioncoopCC, design=design_US)))
visionCC$name <-factor (rownames(visionCC), levels= c("VisioncoopCC", "VisioncoopCCInternational Scale", "VisioncoopCCNational Scale"))
graph_visionCC <- ggplot(visionCC,aes(x=name,y=mean, fill=name))
graph_visionCC + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=visionCC2$X2.5.., ymax=visionCC2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("VisioncoopCC"="No answer", "VisioncoopCCInternational Scale"="Cosmopolitan", "VisioncoopCCNational Scale"="Internationalist")) + coord_cartesian(ylim=c(0,0.8))

#Determinants vision coop
CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Age, chisq=TRUE)

CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Income, chisq=TRUE)

CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Gender, chisq=TRUE)

CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$CSP, chisq=TRUE)

CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Region, chisq=TRUE)

CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p dans les 10-6
vision_frq <-svyby(~VisioncoopCC,by=~Freqtalk,design=design_US,FUN=svymean,vartype=c('ci'))
graph_vision_frq <- ggplot(vision_frq,aes(x=vision_frq$Freqtalk,y=vision_frq$`VisioncoopCCInternational Scale`, fill=vision_frq$Freqtalk))
graph_vision_frq + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who favour an international vision (US)", x="Frequency with wich respondents talk about CC") + geom_errorbar(aes(ymin=vision_frq$`ci_l.VisioncoopCCInternational Scale`, ymax=vision_frq$`ci_u.VisioncoopCCInternational Scale`), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Greens") + coord_cartesian(ylim=c(0,0.99))


CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-33
vision_effects <-svyby(~VisioncoopCC,by=~EffectsCC,design=design_US,FUN=svymean,vartype=c('ci'))
graph_vision_effects <- ggplot(vision_effects,aes(x=vision_effects$EffectsCC,y=vision_effects$`VisioncoopCCInternational Scale`, fill=vision_effects$EffectsCC))
graph_vision_effects + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion who favour an international vision (US)", x="Perception of the gravity of climate change") + geom_errorbar(aes(ymin=vision_effects$`ci_l.VisioncoopCCInternational Scale`, ymax=vision_effects$`ci_u.VisioncoopCCInternational Scale`), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_fill_brewer(palette="Reds") + coord_cartesian(ylim=c(0,0.99))



## Principes
##_______________________



#CBDR
cbdr <-data.frame(svymean(~CBDR, design=design_US))
cbdr2 <-data.frame(confint(svymean(~CBDR, design=design_US)))
cbdr$name <-factor (rownames(cbdr), levels= c("CBDRCommon", "CBDRDifferentiated", "CBDRNone of the above"))
graph_cbdr <- ggplot(cbdr,aes(x=name,y=mean, fill=name))
graph_cbdr + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=cbdr2$X2.5.., ymax=cbdr2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CBDRCommon"="Common", "CBDRDifferentiated"="Differentiated", "CBDRNone of the above"="None of the two")) + coord_cartesian(ylim=c(0,0.6))


#Determinants CBDR
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Age, chisq=TRUE)
#p dans les 10-3

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Income, chisq=TRUE)

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Gender, chisq=TRUE)

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$CSP, chisq=TRUE)

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Region, chisq=TRUE)

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p dans les 10-16

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-40

CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$VisioncoopCC, chisq=TRUE)
#P dans les 10-26

reg <- glm(Common ~ Age + Gender + Region + Income + Knowledge + Agglo + Freqq + EffectsCC + International, 
           data = QuestionnaireUS, family = binomial(logit))
reg2 <- step(reg)
tbl <- exp(cbind(coef(reg2), confint(reg2)))

#Grandfathering
gdfr <- data.frame(svymean(~Gdfr, design=design_US))
gdfr2 <- data.frame(confint(svymean(~Gdfr, design=design_US)))
gdfr$name <-factor (rownames(gdfr), levels= c("GdfrI strongly agree", "GdfrI rather agree", "GdfrI rather disagree", "GdfrI strongly disagree"))
graph_gdfr <- ggplot(gdfr,aes(x=name,y=mean, fill=name))
graph_gdfr + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=gdfr2$X2.5.., ymax=gdfr2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("GdfrI strongly agree"="I strongly agree", "GdfrI rather agree"="I rather agree", "GdfrI rather disagree"="I rather disagree", "GdfrI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))

#Determinants Grandfathering
CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Age, chisq=TRUE)
#p dans les 10-20

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Income, chisq=TRUE)
#P=0.017

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Gender, chisq=TRUE)
#P dans les 10-3

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$CSP, chisq=TRUE)
#P dans les 10-3

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Region, chisq=TRUE)

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p dans les 10-32

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-44

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$VisioncoopCC, chisq=TRUE)
#P dans les 10-16

CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$CBDR, chisq=TRUE)
#P dans les 10-16


#Principle 1 : past emissions
pcp1 <-data.frame(svymean(~Principles_past_em, design=design_US))
pcp12 <-data.frame(confint(svymean(~Principles_past_em, design=design_US)))
pcp1$name <-factor (rownames(pcp1), levels= c("Principles_past_emI strongly agree", "Principles_past_emI rather agree", "Principles_past_emI rather disagree", "Principles_past_emI strongly disagree"))
graph_pcp1 <- ggplot(pcp1,aes(x=name,y=mean, fill=name))
graph_pcp1 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=pcp12$X2.5.., ymax=pcp12$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_past_emI strongly agree"="I strongly agree", "Principles_past_emI rather agree"="I rather agree", "Principles_past_emI rather disagree"="I rather disagree", "Principles_past_emI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Principle 2 : epc
pcp2 <-data.frame(svymean(~Principles_epc, design=design_US))
pcp22 <-data.frame(confint(svymean(~Principles_epc, design=design_US)))
pcp2$name <-factor (rownames(pcp2), levels= c("Principles_epcI strongly agree", "Principles_epcI rather agree", "Principles_epcI rather disagree", "Principles_epcI strongly disagree"))
graph_pcp2 <- ggplot(pcp2,aes(x=name,y=mean, fill=name))
graph_pcp2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=pcp22$X2.5.., ymax=pcp22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_epcI strongly agree"="I strongly agree", "Principles_epcI rather agree"="I rather agree", "Principles_epcI rather disagree"="I rather disagree", "Principles_epcI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Principle 3 : grandfathering
pcp3 <-data.frame(svymean(~Principles_gdfr, design=design_US))
pcp32 <-data.frame(confint(svymean(~Principles_gdfr, design=design_US)))
pcp3$name <-factor (rownames(pcp3), levels= c("Principles_gdfrI strongly agree", "Principles_gdfrI rather agree", "Principles_gdfrI rather disagree", "Principles_gdfrI strongly disagree"))
graph_pcp3 <- ggplot(pcp3,aes(x=name,y=mean, fill=name))
graph_pcp3 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=pcp32$X2.5.., ymax=pcp32$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_gdfrI strongly agree"="I strongly agree", "Principles_gdfrI rather agree"="I rather agree", "Principles_gdfrI rather disagree"="I rather disagree", "Principles_gdfrI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Principle 4 : poor countries
pcp4 <- data.frame(svymean(~Principles_poor, design=design_US))
pcp42 <- data.frame(confint(svymean(~Principles_poor, design=design_US)))
pcp4$name <-factor (rownames(pcp4), levels= c("Principles_poorI strongly agree", "Principles_poorI rather agree", "Principles_poorI rather disagree", "Principles_poorI strongly disagree"))
graph_pcp4 <- ggplot(pcp4,aes(x=name,y=mean, fill=name))
graph_pcp4 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=pcp42$X2.5.., ymax=pcp42$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_poorI strongly agree"="I strongly agree", "Principles_poorI rather agree"="I rather agree", "Principles_poorI rather disagree"="I rather disagree", "Principles_poorI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Principle 5 : cost efficiency
pcp5 <-data.frame(svymean(~Principles_cost, design=design_US))
pcp52 <-data.frame(confint(svymean(~Principles_cost, design=design_US)))
pcp5$name <-factor (rownames(pcp5), levels= c("Principles_costI strongly agree", "Principles_costI rather agree", "Principles_costI rather disagree", "Principles_costI strongly disagree"))
graph_pcp5 <- ggplot(pcp5,aes(x=name,y=mean, fill=name))
graph_pcp5 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=pcp52$X2.5.., ymax=pcp52$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Principles_costI strongly agree"="I strongly agree", "Principles_costI rather agree"="I rather agree", "Principles_costI rather disagree"="I rather disagree", "Principles_costI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Ranking principles
rkpcp_pastem <- data.frame(svymean(~RkPrinciple.SQ001., design=design_US))
rkpcp_pastem$ci<-data.frame(confint(svymean(~RkPrinciple.SQ001., design=design_US)))$X2.5..
rkpcp_pastem$cs<-data.frame(confint(svymean(~RkPrinciple.SQ001., design=design_US)))$X97.5..
rkpcp_epc <-data.frame(svymean(~RkPrinciple.SQ002., design=design_US))
rkpcp_epc$ci<-data.frame(confint(svymean(~RkPrinciple.SQ002., design=design_US)))$X2.5..
rkpcp_epc$cs<-data.frame(confint(svymean(~RkPrinciple.SQ002., design=design_US)))$X97.5..
rkpcp_gdfr <-data.frame(svymean(~RkPrinciple.SQ003., design=design_US))
rkpcp_gdfr$ci<-data.frame(confint(svymean(~RkPrinciple.SQ003., design=design_US)))$X2.5..
rkpcp_gdfr$cs<-data.frame(confint(svymean(~RkPrinciple.SQ003., design=design_US)))$X97.5..
rkpcp_poor<-data.frame(svymean(~RkPrinciple.SQ004., design=design_US))
rkpcp_poor$ci<-data.frame(confint(svymean(~RkPrinciple.SQ004., design=design_US)))$X2.5..
rkpcp_poor$cs<-data.frame(confint(svymean(~RkPrinciple.SQ004., design=design_US)))$X97.5..
rkpcp_cost <-data.frame(svymean(~RkPrinciple.SQ005., design=design_US))
rkpcp_cost$ci<-data.frame(confint(svymean(~RkPrinciple.SQ005., design=design_US)))$X2.5..
rkpcp_cost$cs<-data.frame(confint(svymean(~RkPrinciple.SQ005., design=design_US)))$X97.5..
rkpcp_pastem$principle <- c("Past emissions")
rkpcp_epc$principle <- c("Convergence per capita")
rkpcp_gdfr$principle <- c("Grandfathering")
rkpcp_poor$principle<- c("Poor countries")
rkpcp_cost$principle <- c("Cost efficiency")

rkpcp <- rbind.data.frame(rkpcp_cost,rkpcp_poor,rkpcp_gdfr,rkpcp_epc,rkpcp_pastem)
rkpcp$answer<- c("No", "Yes")
rkpcp2<- subset(rkpcp, rkpcp$answer=="Yes")

graph_rkpcp  <- ggplot(rkpcp2,aes(x=principle,y=mean, fill=principle))
graph_rkpcp + geom_bar(position=position_dodge(),stat="identity",color="black")+ labs(y="Proportion of the US population", x="Principles") + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2") + theme(legend.position = "none")+ coord_cartesian(ylim=c(0,0.83))


#Tolerance
tolerance<-data.frame(svymean(~Tolerance, design=design_US))

#Statement 1
statement1 <-data.frame(svymean(~Statements_forests, design=design_US))
statement12 <-data.frame(confint(svymean(~Statements_forests, design=design_US)))
statement1$name <-factor (rownames(statement1), levels= c("Statements_forestsI strongly agree", "Statements_forestsI rather agree", "Statements_forestsI rather disagree", "Statements_forestsI strongly disagree"))
graph_statement1 <- ggplot(statement1,aes(x=name,y=mean, fill=name))
graph_statement1 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=statement12$X2.5.., ymax=statement12$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_forestsI strongly agree"="I strongly agree", "Statements_forestsI rather agree"="I rather agree", "Statements_forestsI rather disagree"="I rather disagree", "Statements_forestsI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Statement 2
statement2 <-data.frame(svymean(~Statements_trigger, design=design_US))
statement22 <-data.frame(confint(svymean(~Statements_trigger, design=design_US)))
statement2$name <-factor (rownames(statement2), levels= c("Statements_triggerI strongly agree", "Statements_triggerI rather agree", "Statements_triggerI rather disagree", "Statements_triggerI strongly disagree"))
graph_statement2 <- ggplot(statement2,aes(x=name,y=mean, fill=name))
graph_statement2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=statement22$X2.5.., ymax=statement22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_triggerI strongly agree"="I strongly agree", "Statements_triggerI rather agree"="I rather agree", "Statements_triggerI rather disagree"="I rather disagree", "Statements_triggerI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Statement 3
statement3 <-data.frame(svymean(~Statements_funding, design=design_US))
statement32 <-data.frame(confint(svymean(~Statements_funding, design=design_US)))
statement3$name <-factor (rownames(statement3), levels= c("Statements_fundingI strongly agree", "Statements_fundingI rather agree", "Statements_fundingI rather disagree", "Statements_fundingI strongly disagree"))
graph_statement3 <- ggplot(statement3,aes(x=name,y=mean, fill=name))
graph_statement3 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=statement32$X2.5.., ymax=statement32$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_fundingI strongly agree"="I strongly agree", "Statements_fundingI rather agree"="I rather agree", "Statements_fundingI rather disagree"="I rather disagree", "Statements_fundingI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



#Statement 4
statement4 <- data.frame(svymean(~Statements_tech, design=design_US))
statement42 <- data.frame(confint(svymean(~Statements_tech, design=design_US)))
statement4$name <-factor (rownames(statement4), levels= c("Statements_techI strongly agree", "Statements_techI rather agree", "Statements_techI rather disagree", "Statements_techI strongly disagree"))
graph_statement4 <- ggplot(statement4,aes(x=name,y=mean, fill=name))
graph_statement4 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=statement42$X2.5.., ymax=statement42$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Statements_techI strongly agree"="I strongly agree", "Statements_techI rather agree"="I rather agree", "Statements_techI rather disagree"="I rather disagree", "Statements_techI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Accountability
accountability <- data.frame(svymean(~Accountability, design=design_US)) 
accountability2 <- data.frame(confint(svymean(~Accountability, design=design_US)))
accountability$name <-factor (rownames(accountability), levels= c("AccountabilityFootprint", "AccountabilityTerritory"))
graph_accountability <- ggplot(accountability,aes(x=name,y=mean, fill=name))
graph_accountability + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=accountability2$X2.5.., ymax=accountability2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("AccountabilityFootprint"="Footprint", "AccountabilityTerritory"="Territorial emissions")) + coord_cartesian(ylim=c(0,0.8))




#History
history <-data.frame(svymean(~History, design=design_US))
history2 <-data.frame(confint(svymean(~History, design=design_US)))
history$name <-factor (rownames(history), levels= c("HistoryCurrent emissions", "HistorySince 1990", "HistorySince 1850" ))
graph_history <- ggplot(history,aes(x=name,y=mean, fill=name))
graph_history + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=history2$X2.5.., ymax=history2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("HistoryCurrent emissions"="Current emissions", "HistorySince 1990"="Since 1990", "HistorySince 1850"="Since 1850")) + coord_cartesian(ylim=c(0,0.5))






## Cas particuliers
##_______________________




#Test past emissions
Test1 <-svymean(~Test.SQ001., design=design_US)

#Test future emissions
Test2 <-svymean(~Test.SQ002., design=design_US)

#Test jauge
Test3 <-svymean(~Test.SQ003., design=design_US)

#Test pop/PIB
Test4 <-svymean(~Test.SQ004., design=design_US)

#Test global
Resulttest <-data.frame(svymean(~CaseComprehension, design=design_US))
Resulttest2 <-data.frame(confint(svymean(~CaseComprehension, design=design_US)))
Resulttest$name <-factor (rownames(Resulttest), levels= c("CaseComprehensionMisread climate change", "CaseComprehensionOne or several errors", "CaseComprehensionPerfect Comprehension"))
graph_resulttest <- ggplot(Resulttest,aes(x=name,y=mean, fill=name))
graph_resulttest + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=Resulttest2$X2.5.., ymax=Resulttest2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CaseComprehensionMisread climate change"="Misread climate change", "CaseComprehensionOne or several errors"="One or several errors", "CaseComprehensionPerfect Comprehension"="Perfect comprehension")) + coord_cartesian(ylim=c(0,0.5))




#Case 1 : A has higher future emissions, damaging CC


Case1A <-svymean(~Case1.SQ001., design=design_US)
Case1A_right <-t(svyby(~Case1.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case1B <-svymean(~Case1.SQ002., design=design_US)
Case1B_right <-t(svyby(~Case1.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))



#Case 2 : A has higher future emissions and pop, damaging CC


Case2A <-svymean(~Case2.SQ001., design=design_US)
Case2A_right <-t(svyby(~Case2.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case2B <-svymean(~Case2.SQ002., design=design_US)
Case2B_right <-t(svyby(~Case2.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


#Case 3 : A has higher future emissions and smaller income, damaging CC


Case3A <-svymean(~Case3.SQ001., design=design_US)
Case3A_right <-t(svyby(~Case3.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case3B <-svymean(~Case3.SQ002., design=design_US)
Case3B_right <-t(svyby(~Case3.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))



#Case 4 : A has higher future emissions and smaller past emissions, damaging CC


Case4A <-svymean(~Case4.SQ001., design=design_US)
Case4A_right <-t(svyby(~Case4.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case4B <-svymean(~Case4.SQ002., design=design_US)
Case4B_right <-t(svyby(~Case4.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


#Case 5 : Tout change, mÃªmes Ã©missions, damaging CC


Case5A <-svymean(~Case5.SQ001., design=design_US)
Case5A_right <-t(svyby(~Case5.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case5B <-svymean(~Case5.SQ002., design=design_US)
Case5B_right <-t(svyby(~Case5.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


#Case 6 : Tout change, controlled CC


Case6A <-svymean(~Case6.SQ001., design=design_US)
Case6A_right <-t(svyby(~Case6.SQ001.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


Case6B <-svymean(~Case6.SQ002., design=design_US)
Case6B_right <-t(svyby(~Case6.SQ002.,by=~CaseComprehension,design=design_US,FUN=svymean,vartype=c('ci')))


#cases plotting

CasesUS <- read.csv2("Fig6US.csv",  encoding = "UTF-8")
CasesUS <- data.frame(CasesUS)
CasesUS$Opinion<-factor(CasesUS$Opinion, levels = c("Strongly agree","Rather agree", "Strongly disagree", "Rather disagree"))
CasesUS$Significative<-factor(CasesUS$Significative)
CasesUS$Case<-factor(CasesUS$Case, levels = c("5","4", "3","2", "1", "0"))

ggplot(subset(CasesUS,Country.case=="A"), aes(x = Case)) +
  geom_col(data = subset(CasesUS, (Country.case=="A" & (Opinion == "Strongly agree"| Opinion == "Rather agree"))), 
           aes(y = Freq, fill = Opinion)) +
  geom_col(data = subset(CasesUS, (Country.case=="A" & (Opinion == "Strongly disagree"| Opinion == "Rather disagree"))), 
           aes(y = -Freq, fill = Opinion)) + 
  scale_fill_manual(values=c("#92C5DE","#F4A582", "#0571B0", "#CA0020")) +
  labs(y = "Frequence of opinions on country A", x = "Case")+
  coord_flip() 

ggplot(subset(CasesUS,Country.case=="B"), aes(x = Case)) +
  geom_col(data = subset(CasesUS, (Country.case=="B" & (Opinion == "Strongly agree"| Opinion == "Rather agree"))), 
           aes(y = Freq, fill = Opinion)) +
  geom_col(data = subset(CasesUS, (Country.case=="B" & (Opinion == "Strongly disagree"| Opinion == "Rather disagree"))), 
           aes(y = -Freq, fill = Opinion)) + 
  scale_fill_manual(values=c("#92C5DE","#F4A582", "#0571B0", "#CA0020")) +
  labs(y = "Frequence of opinions on country B", x = "Case")+
  coord_flip() 

## Coordination et cas rÃ©el
##_______________________________


#Coordination


China <-data.frame(svymean(~Coord_China, design=design_US))
China2 <-data.frame(confint(svymean(~Coord_China, design=design_US)))
China$name <-factor (rownames(China), levels= c("Coord_ChinaI strongly agree", "Coord_ChinaI partially agree", "Coord_ChinaI partially disagree", "Coord_ChinaI strongly disagree"))
graph_china <- ggplot(China,aes(x=name,y=mean, fill=name))
graph_china + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=China2$X2.5.., ymax=China2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_ChinaI strongly agree"="I strongly agree", "Coord_ChinaI partially agree"="I partially agree", "Coord_ChinaI partially disagree"="I partially disagree", "Coord_ChinaI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



India <-data.frame(svymean(~Coord_India, design=design_US))
India2<-data.frame(confint(svymean(~Coord_India, design=design_US)))
India$name <-factor (rownames(India), levels= c("Coord_IndiaI strongly agree", "Coord_IndiaI partially agree", "Coord_IndiaI partially disagree", "Coord_IndiaI strongly disagree"))
graph_India <- ggplot(India,aes(x=name,y=mean, fill=name))
graph_India + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=India2$X2.5.., ymax=India2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_IndiaI strongly agree"="I strongly agree", "Coord_IndiaI partially agree"="I partially agree", "Coord_IndiaI partially disagree"="I partially disagree", "Coord_IndiaI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



EU<-data.frame(svymean(~Coord_EU, design=design_US))
EUci<-data.frame(confint(svymean(~Coord_EU, design=design_US)))
EU$name <-factor (rownames(EU), levels= c("Coord_EUI strongly agree", "Coord_EUI partially agree", "Coord_EUI partially disagree", "Coord_EUI strongly disagree"))
graph_EU <- ggplot(EU,aes(x=name,y=mean, fill=name))
graph_EU + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=EUci$X2.5.., ymax=EUci$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_EUI strongly agree"="I strongly agree", "Coord_EUI partially agree"="I partially agree", "Coord_EUI partially disagree"="I partially disagree", "Coord_EUI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


USA<-data.frame(svymean(~Coord_USA, design=design_US))
USAci<- data.frame(confint(svymean(~Coord_USA, design=design_US)))
USA$name <-factor (rownames(USA), levels= c("Coord_USAI strongly agree", "Coord_USAI partially agree", "Coord_USAI partially disagree", "Coord_USAI strongly disagree"))
graph_USA <- ggplot(USA,aes(x=name,y=mean, fill=name))
graph_USA + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=USAci$X2.5.., ymax=USAci$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Coord_USAI strongly agree"="I strongly agree", "Coord_USAI partially agree"="I partially agree", "Coord_USAI partially disagree"="I partially disagree", "Coord_USAI strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


#Paths
paths <- data.frame(svymean(~Path, design=design_US))
paths$ci<-data.frame(confint(svymean(~Path, design=design_US)))$X2.5..
paths$cs<-data.frame(confint(svymean(~Path, design=design_US)))$X97.5..
paths2<-subset(paths,as.numeric(paths$ci)>0.02)
paths2$name <-factor (rownames(paths2))
graph_paths <- ggplot(paths2,aes(x=name,y=mean, fill='blue'))
graph_paths + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Most represented judgement paths") + geom_errorbar(aes(ymin=ci, ymax=cs), width=.2, position=position_dodge(.9)) + theme_classic() + theme(legend.position = "none") + scale_x_discrete(labels=c("A","B","C","D","E","F","G","H")) + coord_cartesian(ylim=c(0,0.1))


#Path categories
pathscat <- data.frame(svymean(~Pathcat, design=design_US))

#Reward

EU2<-data.frame(svymean(~Reward.SQ001., design=design_US))
EU22<-data.frame(confint(svymean(~Reward.SQ001., design=design_US)))
EU2$name <-factor (rownames(EU2), levels= c("Reward.SQ001.I strongly agree", "Reward.SQ001.I partially agree", "Reward.SQ001.I partially disagree", "Reward.SQ001.I strongly disagree"))
graph_EU2 <- ggplot(EU2,aes(x=name,y=mean, fill=name))
graph_EU2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=EU22$X2.5.., ymax=EU22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Reward.SQ001.I strongly agree"="I strongly agree", "Reward.SQ001.I partially agree"="I partially agree", "Reward.SQ001.I partially disagree"="I partially disagree", "Reward.SQ001.I strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))


USA2<-data.frame(svymean(~Reward.SQ002., design=design_US))
USA22<-data.frame(confint(svymean(~Reward.SQ002., design=design_US)))
USA2$name <-factor (rownames(USA2), levels= c("Reward.SQ002.I strongly agree", "Reward.SQ002.I partially agree", "Reward.SQ002.I partially disagree", "Reward.SQ002.I strongly disagree"))
graph_USA2 <- ggplot(USA2,aes(x=name,y=mean, fill=name))
graph_USA2 + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=USA22$X2.5.., ymax=USA22$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="RdBu", direction=-1)+ theme(legend.position = "none") + scale_x_discrete(labels=c("Reward.SQ002.I strongly agree"="I strongly agree", "Reward.SQ002.I partially agree"="I partially agree", "Reward.SQ002.I partially disagree"="I partially disagree", "Reward.SQ002.I strongly disagree"="I strongly disagree")) + coord_cartesian(ylim=c(0,0.6))



coord<-data.frame(svymean(~Coord, design=design_US))
coord2<-data.frame(confint(svymean(~Coord, design=design_US)))
coord$name <-factor (rownames(coord))
graph_coord <- ggplot(coord,aes(x=name,y=mean, fill=name))
graph_coord + geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=coord2$X2.5.., ymax=coord2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none") + scale_x_discrete(labels=c("CoordChanged their mind"="Changed their mind", "CoordSame opinion"="Same opinion"))+ coord_cartesian(ylim=c(0,0.6))


#alluvial charts of the coordination
list <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
Coordination_Question<- data.frame(list)
Coordination_Question$Opinion_EU<-factor(c("Strongly agree","Strongly agree","Strongly agree","Strongly agree","partially agree","partially agree","partially agree","partially agree","partially disagree","partially disagree","partially disagree","partially disagree","strongly disagree","strongly disagree","strongly disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_Question$Coordination_EU<-factor(c("Strongly agree","partially agree","partially disagree","strongly disagree"), levels=c("Strongly agree","partially agree","partially disagree","strongly disagree"))
Coordination_Question$freq<- c(0.205, 0.104,0.004,0.003,0.100,0.355,0.040,0.003,0.010,0.054,0.070,0.004, 0.004,0.013, 0.014, 0.019)
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
Coordination_QuestionUS$freq<- c(0.145,0.070,0.008,0.005,0.051,0.310,0.090,0.006,0.009,0.060,0.122,0.028,0.008,0.013,0.022,0.055)
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

chances<-data.frame(svymean(~Chances, design=design_US))
chances2<-data.frame(confint(svymean(~Chances, design=design_US)))
chances$name <-factor (rownames(chances), levels= c("Chancesless than 1%","Chances1 to 10%","Chances11 to 49%","Chances50 to 90%", "Chances90 to 99%","Chancesmore than 99%","Chancesother"))
graph_chances <- ggplot(chances,aes(x=name,y=mean, fill=name))
graph_chances+ geom_bar(position=position_dodge(0.8),stat="identity",color="black") + labs(y="Proportion of the US population", x="Answer") + geom_errorbar(aes(ymin=chances2$X2.5.., ymax=chances2$X97.5..), width=.2, position=position_dodge(.9)) + theme_classic() + scale_fill_brewer(palette="Reds")+ theme(legend.position = "none") + scale_x_discrete(labels=c("Chancesless than 1%"="less than 1%","Chances1 to 10%"="1 to 10%","Chances11 to 49%"="11 to 49%","Chances50 to 90%"="50 to 90%", "Chances90 to 99%"="90 to 99%","Chancesmore than 99%"="more than 99%","Chancesother"="No answer"))+ coord_cartesian(ylim=c(0,0.4))



##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Questions de recherche
##_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________




##Internal consistency
##_________________________________


#Les personnes en faveur du grandfathering sont elles en faveur de son principe ?
grandfathering <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Principles_gdfr, chisq=TRUE)
#10-16, people who strongly agree with the proposition are overrepresented among people who strongly agree with the principle and underrepresented among people who rather disagree. People who rather disagree with the proposition are underrepresented among people who strongly agree with the principle but overrepresented among people who rather agree. People who strongly disagree with the proposition are overrepresented among people who strongly agree with the principle.


#Emissions historiques : y a-t-il un lien entre les rÃ©ponses à la question History et le cas particulier
history1 <- CrossTable(QuestionnaireUS$Case4.SQ001.,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-40, people who strongly agree that country A is taking its fair share in case 4 are overrepresented among people who strongly agree with the principle about past emissions, and underrepresented among people who rather agree and rather disagree. People who rather disagree with the fairness of country A in case 4 are underrepresentef among people who strongly agree with the principle, and overrepresented among people who rather disagree. etc (la diagonale a de grosses contributions, surreprÃ©sentation)


#Emissions historiques : y a-t-il un lien entre les rÃ©ponses à la question History et au principe ?
history2 <- CrossTable(QuestionnaireUS$History,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P=0.03


#Poor countries : lien entre principe et proposition ? 
poor1 <- CrossTable(QuestionnaireUS$Principles_poor,QuestionnaireUS$Statements_trigger, chisq=TRUE)
#P dans les 10-84, la diagonale a de grosses contributions (surreprÃ©sentation), sous-reprÃ©sentation des rather disagree dans les strongly agree des deux cÃ´tÃ©s


#Poor countries : lien entre principe et cas particulier ?
poor2 <- CrossTable(QuestionnaireUS$Principles_poor,QuestionnaireUS$Case3.SQ001., chisq=TRUE)
#P dans les 10-28, la diagonale a de grosses contributions (surreprÃ©sentation), sous-reprÃ©sentation des rather disagree dans les strongly agree des deux cÃ´tÃ©s 


##Consistency : link between pop and epc ? 
epc <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Case2.SQ001., chisq=TRUE)
#P dans les 10-12, diagronale surreprésentée + strongly agree/strongly disagree



##CBDR : link between CBDR and poor principle ?
CBDR1 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Principles_poor, chisq=TRUE)
#P dans les 10-8, people in favor of differentiated responsibilities tend to agree more and people in favor of common tend to disagree

##CBDR : link between CBDR and past principle ?
CBDR2 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-5 people in favor of differentiated responsibilities tend to agree more and people in favor of common tend to disagree

##CBDR : link between CBDR and cost principle ?
CBDR3 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Principles_cost, chisq=TRUE)
#P=0.02

##CBDR : link between CBDR and poor principle ?
CBDR4 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Gdfr, chisq=TRUE)
#P dans les 10-16 mais pas de tendance claire

##CBDR : link between CBDR and forest principle ?
CBDR5 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Statements_forests, chisq=TRUE)
#P dans les 10-4 people in favor of differentiated responsibilities tend to agree more, people in favor of common responsibilities tend to strongly agree and strongly disagree more

##CBDR : link between CBDR and sharetech principle ?
CBDR6 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Statements_tech, chisq=TRUE)
#P=0.04

##CBDR : link between CBDR and fund principle ?
CBDR7 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Statements_funding, chisq=TRUE)
#P dans les 10-5 people in favor of differentiated responsibilities tend to agree more, people in favor of common responsibilities tend to strongly agree and strongly disagree more

##CBDR : link between CBDR and  epc principle ?
CBDR8 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Principles_epc, chisq=TRUE)
#P dans les 10-37 people in favor of common responsibilities tend to agree more and disagree less


#Les personnes en faveur du grandfathering rejettent-elles les émissions passées ?
grandfathering2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-51 people in have the same opinion about grandfathering and past emissions

#Les personnes en faveur d'u grandfathering'epc rejettent-elles les émissions passées ?
epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-42 people in have the same opinion about grandfathering and past emissions


#Qui est en faveur d'epc ?
epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Age, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Gender, chisq=TRUE)
#p=0.02

epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p=10-9
epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-19
epc2 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Knowledge, chisq=TRUE)
#p dans les 10-4

#Qui est en faveur de gdfr?
epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Age, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Gender, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p=10-5
epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-10
epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Knowledge, chisq=TRUE)


epc2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Age, chisq=TRUE)
#10-20
epc2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Income, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Gender, chisq=TRUE)

epc2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Freqtalk, chisq=TRUE)
#p=10-32
epc2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$EffectsCC, chisq=TRUE)
#p dans les 10-44
epc2 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Knowledge, chisq=TRUE)


#Effet d'apprentissage 
##________________________________



apprentissage1<-CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Orderpcp, chisq=TRUE)
#p=0.034, "Common" is underrepresented among people who had principles first, overrepresented among people who had cases first. "None of the above" is overrepresented among people who had principles first, underrepresented among people who had cases first


apprentissage2 <- CrossTable(QuestionnaireUS$Gdfr,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage3 <- CrossTable(QuestionnaireUS$Principles_past_em,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage4 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage6 <- CrossTable(QuestionnaireUS$Principles_cost,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage7 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage8 <- CrossTable(QuestionnaireUS$Principles_poor,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage9 <- CrossTable(QuestionnaireUS$Statements_forests,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage10 <- CrossTable(QuestionnaireUS$Statements_trigger,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage11 <- CrossTable(QuestionnaireUS$Statements_funding,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage12 <- CrossTable(QuestionnaireUS$Statements_tech,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage13 <- CrossTable(QuestionnaireUS$Accountability,QuestionnaireUS$Orderpcp,chisq=TRUE)
#P value dans les 10-5, "Footprint" is underrepresented among people who had the principles first, overrepresented among people who had the cases first. "Territory" is overrepresented among people who had the principles first, underrepresented among people who had the cases first


apprentissage14 <- CrossTable(QuestionnaireUS$History,QuestionnaireUS$Orderpcp,chisq=TRUE)
#P=0,002, "Since 1990" is underrepresented among people who had the principles first, overrepresented among people who had the cases first. "Current emissions" is overrepresented among people who had the principles first, underrepresented among people who had the cases first



#Apprentissage dans l'autre sens

apprentissage15 <- CrossTable(QuestionnaireUS$Case1.SQ001.,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage16 <- CrossTable(QuestionnaireUS$Case1.SQ002.,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage17 <- CrossTable(QuestionnaireUS$Case2.SQ001.,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage18 <- CrossTable(QuestionnaireUS$Case2.SQ002.,QuestionnaireUS$Orderpcp, chisq=TRUE)
#P=0.01, greatest contributions for "strongly agree" and "rather agree"


apprentissage19 <- CrossTable(QuestionnaireUS$Case3.SQ001.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage20 <- CrossTable(QuestionnaireUS$Case3.SQ002.,QuestionnaireUS$Orderpcp,chisq=TRUE)


apprentissage21 <- CrossTable(QuestionnaireUS$Case4.SQ001.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage22 <- CrossTable(QuestionnaireUS$Case4.SQ002.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage23 <- CrossTable(QuestionnaireUS$Case5.SQ001.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage24 <- CrossTable(QuestionnaireUS$Case5.SQ002.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage25 <- CrossTable(QuestionnaireUS$Case6.SQ001.,QuestionnaireUS$Orderpcp, chisq=TRUE)


apprentissage26 <- CrossTable(QuestionnaireUS$Case6.SQ002.,QuestionnaireUS$Orderpcp, chisq=TRUE)














history3 <- CrossTable(QuestionnaireUS$Knowledge,QuestionnaireUS$Principles_past_em, chisq=TRUE)

history4 <- CrossTable(QuestionnaireUS$CauseCC,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#p dans les 10-5

history5 <- CrossTable(QuestionnaireUS$Freqtalk,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-14

history6 <- CrossTable(QuestionnaireUS$EffectsCC,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#10-10

history7 <- CrossTable(QuestionnaireUS$FirstGen,QuestionnaireUS$Principles_past_em, chisq=TRUE)
#10-3

history8 <- CrossTable(QuestionnaireUS$VisioncoopCC,QuestionnaireUS$Principles_past_em, chisq=TRUE)











##Poor countries : link between poor and tolerance towards abroad efforts ? 
##_________________________________________________________________________________________________________________
poor4 <- CrossTable(QuestionnaireUS$Statements_funding,QuestionnaireUS$Principles_poor, chisq=TRUE)
#P dans les 10-68, la diagonale est surreprésentée






##Do people who think CC is disastrous think differently in the principles ?
##_______________________________________________________________________________________________________________

conseq1 <- CrossTable(QuestionnaireUS$Principles_poor,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-5. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair for poor countries to make less efforts (but also people who think climate change will be insignificant, whereas people who thing climate change will be small or significant are underrepresented). People who think climate change will be insignificant are also overrepresented among people who strongly agree with the principle, and underrepresented among people who rather disagree.

conseq2 <- CrossTable(QuestionnaireUS$Principles_past_em,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-9. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair for countries that emitted less to make less efforts (but also people who think climate change will be insignificant, whereas people who thing climate change will be small or significant are underrepresented), and underrepresented among people who strongly agree. People who think climate change will be insignificant are also overrepresented among people who strongly agree with the principle, and underrepresented among people who rather disagree. 

conseq3 <- CrossTable(QuestionnaireUS$Principles_epc,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-16. People who think climate change will be disastrous are overrepresented among people who strongly agree that a per capita convergence would be fair, underrepresented in the rest. People who think it will be small or insignificant are overrepresented among people who strongly disagree

conseq4 <- CrossTable(QuestionnaireUS$Principles_gdfr,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-9. People who think climate change will be disastrous are overrepresented among people who strongly agree that an operational grandfathering would be fair, underrepresented in the rest. People who think it will be insignificant are overrepresented among people who disagree, people who think it will be small are underrepresented among people who strongly agree, overrepresented among people who rather agree

conseq5 <- CrossTable(QuestionnaireUS$Principles_cost,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-12. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries in which it is more costly to reduce reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree

conseq6 <- CrossTable(QuestionnaireUS$Statements_forests,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-7. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries with forests reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree, underrrepresented among people who rather disagree

conseq7 <- CrossTable(QuestionnaireUS$Statements_trigger,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-10. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that the poorest countries won't reduce their emissions immediately (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree, underrrepresented among people who rather disagree

conseq8 <- CrossTable(QuestionnaireUS$Statements_funding,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-8. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries funding abroad projects reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree, underrrepresented among people who rather disagree

conseq9 <- CrossTable(QuestionnaireUS$Statements_tech,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-8. People who think climate change will be disastrous are overrepresented among people who strongly disagree that it would be fair that countries sharing their technologies reduce less (People who think it will be significant are underrepresented). People who think it will be insignificant are overrepresented among people who strongly agree, underrrepresented among people who rather disagree

conseq10 <- CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$EffectsCC, chisq=TRUE)
#P dans les 10-40. People who think CC will be small/insignificant are overrepresented among people who responded none of the above.


##Do people see a link between funding and footprint ?
##____________________________________________________________________________________________________________________

funding <- CrossTable(QuestionnaireUS$Statements_funding,QuestionnaireUS$Accountability, chisq=TRUE)
#P=0,024. People who think countries should be held responsible for their footprint are overrepresented among people who strongly agree with the fairness of the funding statement, underrepresented among people who rather agree. (The opposite for people who think countries are to be held responsible for their territorial emissions)



##Do people change their minds ? 
##___________________________________________________________________________________________________________________
coordination1<- CrossTable(QuestionnaireUS$Coord_EU,QuestionnaireUS$Reward.SQ001., chisq=TRUE)
#P dans les 10-138, diagonale surreprésentée

coordination2<- CrossTable(QuestionnaireUS$Coord_USA,QuestionnaireUS$Reward.SQ002., chisq=TRUE)
#P dans les 10-159, idem diagonale


##CBDR who favors it 
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Age, chisq=TRUE)
#bof
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$EffectsCC, chisq=TRUE)
#common : people who think CC will be disastrous
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Knowledge, chisq=TRUE)
#common :  advanced
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$Freqtalk, chisq=TRUE)
#Several times a week
CrossTable(QuestionnaireUS$CBDR,QuestionnaireUS$VisioncoopCC, chisq=TRUE)
#International scale 


##Coordination
##__________________________________________________________________________________________________________________________________
coordination3<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$Chances, chisq=TRUE)

coordination4<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$Age, chisq=TRUE)

coordination5<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$Gender, chisq=TRUE)

coordination6<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$VisioncoopCC, chisq=TRUE)
##p=0.02 

coordination7<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$CBDR, chisq=TRUE)

coordination8<- CrossTable(QuestionnaireUS$Coord,QuestionnaireUS$Gdfr, chisq=TRUE)
##p=0.01

coordination10<-


#Are there generational differences ? 
##______________________________________________________________________________________________________________________

generation1 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Freqtalk, chisq=TRUE)
#P dans les 10-8. 18-24 are overrepresented among people who talk about CC several times a month (underrepresented among people who almost never talk about CC), 25-34 and 35-44 among people who talk about it several times a week, 55-64 and 65+ are underrepresented among people who talk about it several times a week, 65+ are overrepresented among people who almost never talk about it


generation2 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$CauseCC, chisq=TRUE)
#P dans les 10-3. 18-24 are underrepresented among people who think CC is nature caused, 25-34 are overrepresented among people who think it is not a reality and 55-64 underrepresented (pas très fort, peu de répondants), 65+ are overrepresented among people who think CC is nature caused


generation3 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$EffectsCC, chisq=TRUE)


generation4 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$RegionCC, chisq=TRUE)
#P dans les 10-8. 18-24 and 25-34 are overrepresented among people who think the USA will be more affected, 65+ underrepresented.  25-34 are underrepresented among people who think both will be touched similarly

generationbis <- CrossTable(QuestionnaireUS$Age, QuestionnaireUS$FirstGen, chisq=TRUE)

generation5 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$VisioncoopCC, chisq=TRUE)

generation6 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$CBDR, chisq=TRUE)
#P dans les 10-3. 65+ are overrepresented among people who neither prefer common nor differentiated responsibilities


generation7 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Gdfr, chisq=TRUE)
#P dans les 10-18 25-34 ans 35-44 are overrepresented among people who strongly agree and rather agree with grandfathering (55-64 and 65+ underrepresented), 25-34 are underrepresented among people who disagree and strongly disagree (55-64 and 65+ are overrepresented)


generation8 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-8 25-34 are overrepresented among people who strongly agree that past emissions should be accounted for, 65+ overrepresented among people who strongly disagree


generation9 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Principles_epc, chisq=TRUE)


generation10 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Principles_gdfr, chisq=TRUE)


generation11 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Principles_poor, chisq=TRUE)
#P dans les 10-6 25-34 are overrepresented among people who strongly agree that poor countries should reduce less, 65+ overrepresented among people who strongly disagree.


generation12 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Principles_cost, chisq=TRUE)
#P dans les 10-11 25-34 are overrepresented among people who strongly agree that countries in which it is more costly to reduce should reduce less (65+ underrepresented). 65+ are overrepresented among people who strongly disagree


generation13 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Statements_forests, chisq=TRUE)
#P dans les 10-9 25-34 are overrepresented among people who strongly agree that countries with forests should reduce less (45-54 underrepresented). 55-64 are overrepresented among people who rather disagree


generation14 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Statements_funding, chisq=TRUE)
#P dans les 10-9 25-34 are overrepresented among people who strongly agree that countries funding abroad projects should reduce less (55-64 and 65+ underrepresented)


generation15 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Statements_tech, chisq=TRUE)
#P dans les 10-10 25-34 are overrepresented among people who strongly agree that countries sharing their technologies should reduce less, 35-44 among people who rather agree (45+ underrepresented)


generation16 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Statements_trigger, chisq=TRUE)
#P dans les 10-14 25-34 are overrepresented among people who strongly agree and rather agree that poor countries could wait before reducing (45+ underrepresented). 55-64 are overrepresented among people who rather disagree and 65+ among people who strongly disagree


generation17 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Accountability, chisq=TRUE)
#P dans les 10-4 18-24 are overrepresented among people who think countries should be held responsible for their footprint, underrepresented among people who think territorial emissions (opposite for 55-64)


generation18 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$History, chisq=TRUE)
#P dans les 10-5 18-24 are underrepresented among people who think only current emissions should count, 55+ overrepresented


generation19 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$CaseComprehension, chisq=TRUE)
#P dans les 10-3 25-34 overrepresented among people who made one or several errors(55-64 underrepresented)


generation20 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case1.SQ001., chisq=TRUE)
#P dans les 10-9 25-34 overrepresented among people who strongly agree (55-64 underrepresented). 55+ overrepresented among people who rather disagree and strongly disagree

generation21 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case1.SQ002., chisq=TRUE)

generation22 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case2.SQ001., chisq=TRUE)
#P dans les 10-10 25-34 overrepresented among people who agree or strongly agree, 35-44 among people who strongly agree, 55-64 among people who rather disagree

generation23 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case2.SQ002., chisq=TRUE)

generation24 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case3.SQ001., chisq=TRUE)
#P dans les 10-6 25-34 and 35-44 overrepresented among people who strongly agree, 55+ underrepresented

generation25 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case3.SQ002., chisq=TRUE)

generation26 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case4.SQ001., chisq=TRUE)
#P dans les 10-7 25-34 and 35-44 overrepresented among people who strongly agree, 55+ underrepresented

generation27 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case4.SQ002., chisq=TRUE)

generation28 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case5.SQ001., chisq=TRUE)
#P dans les 10-5 25-34 and 35-44 overrepresented among people who strongly agree, 45-54 underrepresented (overrepresented among people who rather disagree)

generation29 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case5.SQ002., chisq=TRUE)
#P dans les 10-5 18-24 overrepresented among people who strongly agree, 65+ among people who strongly disagree

generation30 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case6.SQ001., chisq=TRUE)
#P dans les 10-4 25-34 and 35-44 overrepresented among people who strongly agree (55+ underrepresented)

generation31 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Case6.SQ002., chisq=TRUE)


generation32 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Coord_China, chisq=TRUE)
#P dans les 10-6 25-34 overrepresented among people who agree and rather agree, 35-44 among people who rather agree

generation33 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Coord_India, chisq=TRUE)

generation34 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Coord_EU, chisq=TRUE)

generation35 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Coord_USA, chisq=TRUE)
#P dans les 10-4 25-34 and 35-44 overrepresented among people who strongly agree


generation36 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Reward.SQ001., chisq=TRUE)

generation37 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Reward.SQ002., chisq=TRUE)
#P dans les 10-5 25-34 overrepresented among people who strongly agree, 45-54 underrepresented.

generation38 <- CrossTable (QuestionnaireUS$Age, QuestionnaireUS$Chances, chisq=TRUE)




#Are there differences across genders ? 
##______________________________________________________________________________________________________________________

gender1 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Freqtalk, chisq=TRUE)

gender2 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$CauseCC, chisq=TRUE)

gender3 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$EffectsCC, chisq=TRUE)

gender4 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$RegionCC, chisq=TRUE)
#P dans les 10-2 Men are overrepresented among people who think the EU will be more impacted, women underrepresented


gender5 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$VisioncoopCC, chisq=TRUE)

gender6 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$CBDR, chisq=TRUE)

gender7 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Gdfr, chisq=TRUE)

gender8 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Principles_past_em, chisq=TRUE)
#P dans les 10-4 Men underrepresented among people who strongly agree past emissions should be accounted for underrepresented among people who rather disagree (opposite for women)


gender9 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Principles_epc, chisq=TRUE)
#P=0,02 Men are underrepresented among people who rather disagree with epc (opposite for women)


gender10 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Principles_gdfr, chisq=TRUE)

gender11 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Principles_poor, chisq=TRUE)
#P dans les 10-3 Men are overrepresented among people who strongly agree that poor countries should do less, underrepresented among people who rather disagree (opposite for women)


gender12 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Principles_cost, chisq=TRUE)
#P=0.015 Men are overrepresented among people who strongly agree and strongly disagree that countries where it is more costly should do less, underrepresented among people who rather disagree (opposite for women)


gender13 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Statements_forests, chisq=TRUE)
#P=0.01 Men are underrepresented among people who rather disagree that countries with forests should do less (opposite for women)


gender14 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Statements_funding, chisq=TRUE)
#P dans les 10-4 Men are overrepresented among people who strongly agree and strongly disagree that countries funding abroad projects should do less, underrepresented among people who rather disagree (opposite for women)


gender15 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Statements_tech, chisq=TRUE)
#P=0.01 Men are overrepresented among people who strongly agree that countries sharing their tech should do less, underrepresented among people who rather disagree (opposite for women)


gender16 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Statements_trigger, chisq=TRUE)
#P dans les 10-3 Men are overrepresented among people who srtongly agree that poor countries could wait (opposite for women)


gender17 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Accountability, chisq=TRUE)


gender18 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$History, chisq=TRUE)


gender19 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$CaseComprehension, chisq=TRUE)


gender20 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case1.SQ001., chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree (opposite for women)

gender21 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case1.SQ002., chisq=TRUE)

gender22 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case2.SQ001., chisq=TRUE)

gender23 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case2.SQ002., chisq=TRUE)

gender24 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case3.SQ001., chisq=TRUE)

gender25 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case3.SQ002., chisq=TRUE)

gender26 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case4.SQ001., chisq=TRUE)
#P=0.02 Men overrepresented among people who strongly agree, underrepresented among people who rather agree (opposite for women)

gender27 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case4.SQ002., chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)

gender28 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case5.SQ001., chisq=TRUE)

gender29 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case5.SQ002., chisq=TRUE)
#P=0.006 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)

gender30 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case6.SQ001., chisq=TRUE)

gender31 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Case6.SQ002., chisq=TRUE)

gender32 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Coord_China, chisq=TRUE)

gender33 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Coord_India, chisq=TRUE)

gender34 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Coord_EU, chisq=TRUE)
#P=0.01 Men overrepresented among people who strongly agree, underrepresented among people who rather disagree (opposite for women)


gender35 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Coord_USA, chisq=TRUE)

gender36 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Reward.SQ001., chisq=TRUE)

gender37 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Reward.SQ002., chisq=TRUE)

gender38 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$Chances, chisq=TRUE)


#Are there differences across SPC ? 
##______________________________________________________________________________________________________________________

CSP1 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Freqtalk, chisq=TRUE)
#P dans les 10-3, inactive people are underrepresented among people who talk about CC several times a week, CSP+ overrepresented


CSP2 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$CauseCC, chisq=TRUE)

CSP3 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$EffectsCC, chisq=TRUE)

CSP4 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$RegionCC, chisq=TRUE)
#P dans les 10-4 CSP- overrepresented among people who think the USA will be more affected, CSP+ overrepresented among people who think the EU


CSP5 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$VisioncoopCC, chisq=TRUE)

CSP6 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$CBDR, chisq=TRUE)

CSP7 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Gdfr, chisq=TRUE)
#P dans les 10-3 CSP- overrepresented among people who strongly agree, Inactive underrepresented

CSP8 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Principles_past_em, chisq=TRUE)

CSP9 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Principles_epc, chisq=TRUE)

CSP10 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Principles_gdfr, chisq=TRUE)

CSP11 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Principles_poor, chisq=TRUE)

CSP12 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Principles_cost, chisq=TRUE)

CSP13 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Statements_forests, chisq=TRUE)

CSP14 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Statements_funding, chisq=TRUE)

CSP15 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Statements_tech, chisq=TRUE)
#P=0.02 CSP- overrepresented among people who strongly agree, Inactive underrepresented


CSP16 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Statements_trigger, chisq=TRUE)

CSP17 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Accountability, chisq=TRUE)

CSP18 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$History, chisq=TRUE)

CSP19 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$CaseComprehension, chisq=TRUE)

CSP20 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case1.SQ001., chisq=TRUE)

CSP21 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case1.SQ002., chisq=TRUE)

CSP22 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case2.SQ001., chisq=TRUE)

CSP23 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case2.SQ002., chisq=TRUE)

CSP24 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case3.SQ001., chisq=TRUE)
#P=0.027  CSP- overrepresented among people who strongly agree, Inactive underrepresented


CSP25 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case3.SQ002., chisq=TRUE)

CSP26 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case4.SQ001., chisq=TRUE)


CSP27 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case4.SQ002., chisq=TRUE)

CSP28 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case5.SQ001., chisq=TRUE)

CSP29 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case5.SQ002., chisq=TRUE)

CSP30 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case6.SQ001., chisq=TRUE)

CSP31 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Case6.SQ002., chisq=TRUE)

CSP32 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Coord_China, chisq=TRUE)
#P dans les 10-3 CSP+ underrepresented in the partially disagree, Inactive underrepresented in the partially agree

CSP33 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Coord_India, chisq=TRUE)

CSP34 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Coord_EU, chisq=TRUE)

CSP35 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Coord_USA, chisq=TRUE)

CSP36 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Reward.SQ001., chisq=TRUE)

CSP37 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Reward.SQ002., chisq=TRUE)

CSP38 <- CrossTable (QuestionnaireUS$CSP, QuestionnaireUS$Chances, chisq=TRUE)





#Are there educational differences ? 
##______________________________________________________________________________________________________________________

Educ1 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Freqtalk, chisq=TRUE)
#P dans les 10-4


Educ2 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$CauseCC, chisq=TRUE)

Educ3 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$EffectsCC, chisq=TRUE)

Educ4 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$RegionCC, chisq=TRUE)

Educ5 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$VisioncoopCC, chisq=TRUE)

Educ6 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$CBDR, chisq=TRUE)
#P dans les 10-3


Educ7 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Gdfr, chisq=TRUE)
#P=0.03


Educ8 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Principles_past_em, chisq=TRUE)

Educ9 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Principles_epc, chisq=TRUE)

Educ10 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Principles_gdfr, chisq=TRUE)
#P=0.03


Educ11 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Principles_poor, chisq=TRUE)
#P=0.029


Educ12 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Principles_cost, chisq=TRUE)

Educ13 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Statements_forests, chisq=TRUE)

Educ14 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Statements_funding, chisq=TRUE)

Educ15 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Statements_tech, chisq=TRUE)
#P=0.048


Educ16 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Statements_trigger, chisq=TRUE)
#P=0.017


Educ17 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Accountability, chisq=TRUE)

Educ18 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$History, chisq=TRUE)

Educ19 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$CaseComprehension, chisq=TRUE)
#P dans les 10-3


Educ20 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case1.SQ001., chisq=TRUE)
#P=0.025


Educ21 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case1.SQ002., chisq=TRUE)

Educ22 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case2.SQ001., chisq=TRUE)
#P=0.03


Educ23 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case2.SQ002., chisq=TRUE)

Educ24 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case3.SQ001., chisq=TRUE)

Educ25 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case3.SQ002., chisq=TRUE)

Educ26 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case4.SQ001., chisq=TRUE)
#P dans les 10-3


Educ27 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case4.SQ002., chisq=TRUE)
#P dans les 10-3


Educ28 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case5.SQ001., chisq=TRUE)
#P=0.025


Educ29 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case5.SQ002., chisq=TRUE)
#P=0.04


Educ30 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case6.SQ001., chisq=TRUE)

Educ31 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Case6.SQ002., chisq=TRUE)
#P dans les 10-5


Educ32 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Coord_China, chisq=TRUE)

Educ33 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Coord_India, chisq=TRUE)

Educ34 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Coord_EU, chisq=TRUE)
#P=0.02


Educ35 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Coord_USA, chisq=TRUE)

Educ36 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Reward.SQ001., chisq=TRUE)

Educ37 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Reward.SQ002., chisq=TRUE)

Educ38 <- CrossTable (QuestionnaireUS$Educ, QuestionnaireUS$Chances, chisq=TRUE)
#P=0.018



### Explaining change of categories with different factors

change1 <- CrossTable (QuestionnaireUS$Changecat, QuestionnaireUS$Gender, chisq=TRUE)


change2 <- CrossTable (QuestionnaireUS$Changecat, QuestionnaireUS$Income, chisq=TRUE)


change3 <- CrossTable (QuestionnaireUS$Changecat, QuestionnaireUS$Chances, chisq=TRUE)


change4 <- CrossTable (QuestionnaireUS$Changecat, QuestionnaireUS$CBDR, chisq=TRUE)


change4 <- CrossTable (QuestionnaireUS$Changecat, QuestionnaireUS$EffectsCC, chisq=TRUE)

#Are there differences when removing climatoskeptics ? 
##______________________________________________________________________________________________________________________

cause1 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Freqtalk, chisq=TRUE)
#people who think CC is human caused are underrepresented among people who never talk about CC

cause2 <- CrossTable (QuestionnaireUS$Gender, QuestionnaireUS$CauseCC, chisq=TRUE)
cause3 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$EffectsCC, chisq=TRUE)
#"human caused" is overrepresented among people who think CC will be disastrous

cause4 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$RegionCC, chisq=TRUE)
#'human caused" overrepresented among people who think both will be touched

CauseCC5 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$VisioncoopCC, chisq=TRUE)
#"human caused" underrepresented among people with an internationalist vision

CauseCC6 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$CBDR, chisq=TRUE)
#"human caused" underrepresented among none of the above

CauseCC7 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Gdfr, chisq=TRUE)
#"human caused" overrepresented among people who strongly disagree

CauseCC8 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Principles_past_em, chisq=TRUE)
#"not a reality" overrepresented among people who strongly agree

CauseCC9 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Principles_epc, chisq=TRUE)
#"human caused" underrepresented among people who strongly disagree

CauseCC10 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Principles_gdfr, chisq=TRUE)
#not a reality overrepresented among people who strongly disagree

CauseCC11 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Principles_poor, chisq=TRUE)
CauseCC12 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Principles_cost, chisq=TRUE)
#not a reality overrepresented among people who strongly agree

CauseCC13 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Statements_forests, chisq=TRUE)
#human caused overrepresented among people who disagree

CauseCC14 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Statements_funding, chisq=TRUE)
#not a reality overrepresented among people who strongly disagree

CauseCC15 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Statements_tech, chisq=TRUE)
#human caused overrepresented among people who disagree

CauseCC16 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Statements_trigger, chisq=TRUE)
#not a reality overrepresented among people who strongly agree


CauseCC17 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Accountability, chisq=TRUE)
#human caused overrepresented among people who favour the footprint

CauseCC18 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$History, chisq=TRUE)
#human caused underrepresented among people in favour of current emissions

CauseCC19 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$CaseComprehension, chisq=TRUE)
#human caused overrepresented among perfect comprehension

CauseCC20 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case1.SQ001., chisq=TRUE)

CauseCC21 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case1.SQ002., chisq=TRUE)

CauseCC22 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case2.SQ001., chisq=TRUE)

CauseCC23 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case2.SQ002., chisq=TRUE)

CauseCC24 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case3.SQ001., chisq=TRUE)

CauseCC25 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case3.SQ002., chisq=TRUE)

CauseCC26 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case4.SQ001., chisq=TRUE)

CauseCC27 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case4.SQ002., chisq=TRUE)

CauseCC28 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case5.SQ001., chisq=TRUE)

CauseCC29 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case5.SQ002., chisq=TRUE)

CauseCC30 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case6.SQ001., chisq=TRUE)

CauseCC31 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Case6.SQ002., chisq=TRUE)

CauseCC32 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Coord_China, chisq=TRUE)

CauseCC33 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Coord_India, chisq=TRUE)

CauseCC34 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Coord_EU, chisq=TRUE)

CauseCC35 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Coord_USA, chisq=TRUE)

CauseCC36 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Reward.SQ001., chisq=TRUE)

CauseCC37 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Reward.SQ002., chisq=TRUE)

CauseCC38 <- CrossTable (QuestionnaireUS$CauseCC, QuestionnaireUS$Chances, chisq=TRUE)



