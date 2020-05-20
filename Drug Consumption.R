####################################################################################
#The report must be written in English and uploaded as both a PDF document and an Rmd file.
# lien : https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29
################################download library##################################
library(Hmisc)
library(readr)
library(ggplot2)
library(pastecs)
library(gridExtra)
library(arules)       
library(arulesViz) 
library(dplyr)
library(tidyselect)
library(RColorBrewer)
library(corrplot)
library(RColorBrewer)
library(qgraph)
library(igraph)
library(data.table)
library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)
################################Download data########################################
drug_consumption <- read_csv("drug_consumption.data", col_names = FALSE)
#########################Replace name of variables#################################
names(drug_consumption)

drug <- drug_consumption
names(drug) <- c("ID","Age","Gender","Education","Country","Ethnicity","Nscore",
                 "Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol",
                 "Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy",
                 "Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine",
                 "Semer","VSA")
names(drug)
str(drug)
summary(drug)
#########################replace classification##################################
drug[drug=="CL0"]="no" 
drug[drug=="CL1"]="yes" 
drug[drug=="CL2"]="yes" 
drug[drug=="CL3"]="yes" 
drug[drug=="CL4"]="yes" 
drug[drug=="CL5"]="yes" 
drug[drug=="CL6"]="yes" 
###################################Gender#########################################
drug$Gender <- factor(drug$Gender,labels=c('H','F')) ; drug$Gender
summary(drug$Gender)
##################################Education#######################################
drug$Education[drug$Education==-2.43591]="Left school before 16 years" 
drug$Education[drug$Education==-1.73790]="Left school at 16 years" 
drug$Education[drug$Education==-1.43719]="Left school at 17 years" 
drug$Education[drug$Education==-1.22751]="Left school at 18 years" 
drug$Education[drug$Education==-0.61113]="Some college or university, no certificate or degree" 
drug$Education[drug$Education==-0.05921]="Professional certificate/ diploma" 
drug$Education[drug$Education==0.45468]="University degree" 
drug$Education[drug$Education==1.16365]="Masters degree" 
drug$Education[drug$Education==1.98437]="Doctorate degree" 
#################################Country##########################################
drug$Country[drug$Country==-0.09765]="Australia" 
drug$Country[drug$Country==0.24923]="Canada" 
drug$Country[drug$Country==-0.46841]="New Zealand" 
drug$Country[drug$Country==-0.28519]="Other" 
drug$Country[drug$Country==0.21128]="Republic of Ireland" 
drug$Country[drug$Country==0.96082]="UK" 
drug$Country[drug$Country==-0.57009]="USA" 
################################Ethnicity########################################
drug$Ethnicity[drug$Ethnicity==-0.50212]="Asian"
drug$Ethnicity[drug$Ethnicity==-1.10702]="Black"
drug$Ethnicity[drug$Ethnicity==1.90725]="Mixed-Black/Asian"
drug$Ethnicity[drug$Ethnicity==0.12600]="Mixed-White/Asian"
drug$Ethnicity[drug$Ethnicity==-0.22166]="Mixed-White/Black"
drug$Ethnicity[drug$Ethnicity==0.11440]="Other"
drug$Ethnicity[drug$Ethnicity==-0.31685]="White"
#################################Age##############################################
drug$Age[drug$Age==-0.95197]="18-24" 
drug$Age[drug$Age==-0.07854]="25-34"
drug$Age[drug$Age==0.49788]="35-44"
drug$Age[drug$Age==1.09449]="45-54"
drug$Age[drug$Age==1.82213]="55-64"
drug$Age[drug$Age==2.59171]="65+"
################################Data analyse######################################
#Age
table(drug$Age)
table(drug$Age)/nrow(drug) 
#Gender
table(drug$Gender)
table(drug$Gender)/nrow(drug) 
#Education
table(drug$Education)
table(drug$Education)/nrow(drug) 
#Country
table(drug$Country)
table(drug$Country)/nrow(drug) 
#Ethnicity
table(drug$Ethnicity)
table(drug$Ethnicity)/nrow(drug) 

#Graph
par(mfrow = c(2,3))
pie(table(drug$Age), main = "Age")
pie(table(drug$Gender), main = "Gender")
pie(table(drug$Education), main = "Education")
pie(table(drug$Country), main = "Country")
pie(table(drug$Ethnicity), main = "Ethnicity",cex = 0.5)

#Nscore
table(drug$Nscore)
summary(drug$Nscore)
stat.desc(drug$Nscore, norm=TRUE)

#Escore
table(drug$Escore)
summary(drug$Escore)
stat.desc(drug$Escore, norm=TRUE)

#Oscore
table(drug$Oscore)
summary(drug$Oscore)
stat.desc(drug$Oscore, norm=TRUE)

#Ascore
table(drug$Ascore)
summary(drug$Ascore)
stat.desc(drug$Ascore, norm=TRUE)

#Cscore
table(drug$Cscore)
summary(drug$Cscore)
stat.desc(drug$Cscore, norm=TRUE)

#Impulsive
table(drug$Impulsive)
summary(drug$Impulsive)
stat.desc(drug$Impulsive, norm=TRUE)

#SS
table(drug$SS)
summary(drug$SS)
stat.desc(drug$SS, norm=TRUE)
############################
#Alcohol
table(drug$Alcohol)
table(drug$Alcohol)/nrow(drug) 
#pie(table(drug$Alcohol))

#Amphet
table(drug$Amphet)
table(drug$Amphet)/nrow(drug) 

#Amyl
table(drug$Amyl)
table(drug$Amyl)/nrow(drug) 

#Benzos
table(drug$Benzos)
table(drug$Benzos)/nrow(drug) 

#caff
table(drug$Caff)
table(drug$Caff)/nrow(drug) 

#cannabis
table(drug$Cannabis)
table(drug$Cannabis)/nrow(drug) 

#choc
table(drug$Choc)
table(drug$Choc)/nrow(drug) 
#coke
table(drug$Coke)
table(drug$Coke)/nrow(drug) 
#crack
table(drug$Crack)
table(drug$Crack)/nrow(drug) 
#Ecstacy
table(drug$Ecstasy)
table(drug$Ecstasy)/nrow(drug) 
#Heroin
table(drug$Heroin)
table(drug$Heroin)/nrow(drug) 
#Ketamine
table(drug$Ketamine)
table(drug$Ketamine)/nrow(drug) 
#Legalh
table(drug$Legalh)
table(drug$Legalh)/nrow(drug) 
#LSD
table(drug$LSD)
table(drug$LSD)/nrow(drug) 
#Meth
table(drug$Meth)
table(drug$Meth)/nrow(drug) 
#Mushrooms
table(drug$Mushrooms)
table(drug$Mushrooms)/nrow(drug) 
#Nicotine
table(drug$Nicotine)
table(drug$Nicotine)/nrow(drug) 
#Semer
table(drug$Semer)
table(drug$Semer)/nrow(drug) 
#VSA
table(drug$VSA)
table(drug$VSA)/nrow(drug) 

#Graph
par(mfrow = c(2,3))
pie(table(drug$Amphet), main = "Amphet")
pie(table(drug$Amyl), main = "Amyl")
pie(table(drug$Benzos), main = "Benzos")
pie(table(drug$Caff), main = "Caff")
pie(table(drug$Cannabis), main = "Cannabis")
pie(table(drug$VSA), main = "VSA")

par(mfrow = c(2,3))
pie(table(drug$Semer), main = "Semer")
pie(table(drug$Nicotine), main = "Nicotine")
pie(table(drug$Mushrooms), main = "Mushrooms")
pie(table(drug$Meth), main = "Meth")
pie(table(drug$LSD), main = "LSD")
pie(table(drug$Legalh), main = "Legalh")

par(mfrow = c(2,3))
pie(table(drug$Ketamine), main = "Ketamine")
pie(table(drug$Heroin), main = "Heroin")
pie(table(drug$Ecstasy), main = "Ecstasy")
pie(table(drug$Crack), main = "Crack")
pie(table(drug$Coke), main = "Coke")
pie(table(drug$Caff), main = "Caff")

####################Personality Measurements
#level of education, age, gender, country of residence and ethnicity
##Education
G1<-ggplot(drug) +
  aes(x = Education, fill = Cannabis) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(title = "Education") +
  coord_flip() +
  theme_minimal()

##Age
G2<-ggplot(drug) +
  aes(x = Age, fill = Cannabis) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "Age") +
  theme_minimal()

##Gender
G3<-ggplot(drug) +
  aes(x = Gender, fill = Cannabis) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  labs(title = "Gender") +
  coord_flip() +
  theme_minimal()

##Ethnicity
G4<-ggplot(drug) +
  aes(x = Ethnicity, fill = Cannabis) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  labs(title = "Ethnicity") +
  coord_flip() +
  theme_minimal()

##Country
G5<-ggplot(drug) +
  aes(x = Country, fill = Cannabis) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(title = "Country") +
  coord_flip() +
  theme_minimal()

grid.arrange(G1, G2, G3, G4, G5, ncol=2, nrow = 3)
#NEO-FFI-R
########neuroticism 
G6<-ggplot(drug) +
  aes(x = Nscore, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Neuroticism") +
  theme_minimal()

########extraversion 
G7<-ggplot(drug) +
  aes(x = Escore, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Extraversion") +
  theme_minimal()

########openness to experience 
G8<-ggplot(drug) +
  aes(x = Oscore, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Openness to experience") +
  theme_minimal()

########Agreeableness
G9<-ggplot(drug) +
  aes(x = Ascore, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Agreeableness") +
  theme_minimal()

##########Conscientiousness
G10<-ggplot(drug) +
  aes(x = Cscore, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Conscientiousness") +
  theme_minimal()

##########impulsiveness
G11<-ggplot(drug) +
  aes(x = Impulsive, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "impulsiveness") +
  theme_minimal()

#########sensation seeing
G12<-ggplot(drug) +
  aes(x = SS, fill = Cannabis) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "sensation seeing") +
  theme_minimal()

grid.arrange(G6, G7, G8, G9, G10, G11, G12, ncol=2, nrow = 4)
##########################
drugg<- drug

drugg[drugg=="no"]="0" 
drugg[drugg=="yes"]="1" 

drugg$VSA<-as.numeric(as.character(drugg$VSA))
is.numeric(drugg$VSA)
drugg$Semer<-as.numeric(as.character(drugg$Semer))
drugg$Nicotine<-as.numeric(as.character(drugg$Nicotine))
drugg$Mushrooms<-as.numeric(as.character(drugg$Mushrooms))
drugg$Meth<-as.numeric(as.character(drugg$Meth))
drugg$LSD<-as.numeric(as.character(drugg$LSD))
drugg$Legalh<-as.numeric(as.character(drugg$Legalh))
drugg$Ketamine<-as.numeric(as.character(drugg$Ketamine))
drugg$Heroin<-as.numeric(as.character(drugg$Heroin))
drugg$Ecstasy<-as.numeric(as.character(drugg$Ecstasy))
drugg$Crack<-as.numeric(as.character(drugg$Crack))
drugg$Coke<-as.numeric(as.character(drugg$Coke))
drugg$Choc<-as.numeric(as.character(drugg$Choc))
drugg$Cannabis<-as.numeric(as.character(drugg$Cannabis))
drugg$Caff<-as.numeric(as.character(drugg$Caff))
drugg$Benzos<-as.numeric(as.character(drugg$Benzos))
drugg$Amyl<-as.numeric(as.character(drugg$Amyl))
drugg$Amphet<-as.numeric(as.character(drugg$Amphet))
drugg$Alcohol<-as.numeric(as.character(drugg$Alcohol))
##########
drugg1 <- as.matrix(drugg[14:32])

cor_2 <- rcorr(drugg1)
cor_2
############
cor(drugg1, method = c("pearson", "kendall", "spearman")) 
############
temp <- drugg %>% select(one_of("Alcohol",
                                "Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy",
                                "Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine",
                                "Semer","VSA")) %>% as.matrix()
M <- cor(temp, use = "pairwise.complete.obs")
par(mfrow = c(1,1))

corrplot(M, order = "hclust", addrect = 2, type = "lower", col = brewer.pal(n = 8, name = "RdBu"))
######
par(mfrow = c(1,2))
corrplot(M, method = "pie")
corrplot(M, order = "hclust")
#############
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 9, name = "PuOr"), bg = "darkgreen")

p_mat <- cor_2$P
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p_mat, sig.level = 0.01)
#####################
tidy_cors <- drugg1 %>% 
  correlate() %>% 
  stretch()

graph_cors <- tidy_cors %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between Drug variables")