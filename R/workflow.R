## CombiROC demo workflow

#######################
##    Tab UPLOAD    ###
#######################

# read data 
data <- read.table("data/demo_5Ags.csv", header = TRUE,sep = ";", na.strings="")
d.input <- data

# transform the data in long format for plotting purposes
library(tidyr)
library(dplyr)
data_long <- tidyr::gather(data, key = "Markers", value = "Values", -Patient.ID, -Class, na.rm = FALSE, convert = FALSE)
data_classA <- dplyr::filter(data_long, Class == "A")
data_classB <- dplyr::filter(data_long, Class == "B") 

# Class A data (DISEASE CLASS)
classA <- d.input[1:length(which(d.input$Class=="A")),]

# Class B data (CONTROL CLASS)
start <- length(which(d.input$Class=="A"))+1
end <- start+(length(which(d.input$Class=="B"))-1)
classB <- d.input[start:end,]


ns <- dim(d.input)
nclass <- unique(d.input[,2])

# the dataset contains this number of samples:
ns[1]
# the dataset containes this number of features:
ns[2]-2
# how many classes were detected in the dataset:
length(nclass)

#######################
##    Tab PLOTS     ###
#######################

### Boxplots
library(ggplot2)

ggplot(data = data_long, aes(Markers, Values)) +
  geom_boxplot(aes(color = Class)) +
  theme_classic()

ggplot(data = data_classA, aes(Markers, Values)) +
  geom_boxplot() +
  theme_classic() +
  ggtitle("Class A")

ggplot(data = data_classB, aes(Markers, Values)) +
  geom_boxplot() +
  theme_classic() +
  ggtitle("Class B")

# Boxplot statistics

summary(data_classA)
summary(data_classB)


ns
classA <- d.input[1:length(which(d.input$Class=="A")),] # già fatto prima
tempstat <- boxplot(classA, plot=F)

# remove the fisrt two columns: Patient_ID and Class
A_boxstat<-tempstat$stats[,3:ns[2]]
rownames(A_boxstat) <- c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker")
colnames(A_boxstat) <- tempstat$names[3:ns[2]]

summary(A_boxstat)

A_basicboxstat <- t(apply.dataSummary(classA)) ## non trovo questa funzione!! apply.dataSummary

# assemble the class A summary table
A_boxstat<-rbind(A_boxstat,t(A_basicboxstat[,1]),t(A_basicboxstat[,3]),t(A_basicboxstat[,4]),t(A_basicboxstat[,5]),t(A_basicboxstat[,6]),t(A_basicboxstat[,7]),t(A_basicboxstat[,8]))
rownames(A_boxstat)<-c("Lower whisker","1st quartile","Median","3rd quartile","Upper whisker","Mean","Min","Max","Range","Sd","Skewness","CV")

# repeat procedure to obtain the class B summary table

### marker plot

ggplot(data = data_long, aes(Markers, Values)) +
  geom_jitter(aes(color = Class)) +
  theme_classic()

## ....


#######################
##    Processing    ###
#######################

## .... see line 853 server.R



#######################
##  Combinatorial    ##
#######################

## .... see line 940 server.R

library(gtools)

#remove patients and class columns
dfA <- classA[,3:dim(classA)[2]]
dfB <- classB[,3:dim(classB)[2]]
dfA<-t(dfA)
dfB<-t(dfB)
n_features<-length(rownames(dfA))
k<-1:n_features
K<-2^n_features-1      #total number of combinations
nAntigen<-K
class_A<-1
class_B<-2
name_classA<-"disease"
name_classB<-"control"
nSamples_A<-length(colnames(dfA))
nSamples_B<-length(colnames(dfB))

# list of all possible combinations
listCombinationAntigens <- array(0,dim=c(K,1))
# relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
frequencyCombinationAntigens<-array(0,dim=c(K,2))

temp <- gtools::combinations(n_features,k[i],rownames(dfA))
  
  




