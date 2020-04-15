#######################################################################
## R script for CombiROC
##
## Various supporting functions for CombiROC
##
## Author: Saveria Mazzara, mazzara@ingm.org
##
##
######################################################################



WhiskerType= c("Tukey", "Spear", "Altman")
ColorType=c("lightblue","white","lightgreen","salmon","plum","orange","darkred","darkmagenta","aliceblue","forestgreen","yellow","cornflowerblue","palevioletred")
GridType=c("x and y", "x","y","none")
OrientationType=c("vertical","horizontal")
LabelOptionsType=c("x axis label","y axis label","box plot title_classA","box plot title_classB")
dataTransf=c("none","natural logarithm (log2)")
dataScaling=c("none","unit variance scaling","pareto scaling")
PanelType=c("full","reduced")
useSelectize = FALSE





dataSummary<- function(x) {                           #WhatMyTeacherWants
	res <- c(
		round(mean(x, na.rm=TRUE), digits=2),
    round(median(x, na.rm=TRUE), digits=2),
		round(min(x, na.rm=TRUE), digits=2), max(x, na.rm=TRUE),
		max(x, na.rm=TRUE)-min(x, na.rm=TRUE),                                     #range
		round(sd(x, na.rm=TRUE)),
		round(skewness(x, type=1, na.rm=TRUE), digits=2),
		round(sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE), digits=2),
		round(quantile(x,probs=c(0.25,0.75), na.rm=TRUE), digits=2))
		# round(ineq(x[!is.na(x)]),2))   Gini index
	names(res) <- c("mean","median","min","max","range","sd",
                  "skewness","CV","Q1","Q3")
	res
}

apply.dataSummary <- function(df) {
  # Select numerical variables
  numVar <- sapply(1:ncol(df),function(x){is.numeric(df[,x])})
	if (sum(numVar)==0) {
    return(NULL)
		stop("No numerical variables in the data: nothing to do...!")
	} else res <- apply(df[,numVar],2,dataSummary)

  res
}

########### Boxplot
make.boxplot <- function(df,main,xlab,ylab,scale,col) {
	# Select numerical variables
  numVar <- sapply(1:ncol(df),function(x){is.numeric(df[,x])})
  if (scale) df[,numVar] <- scale(df[,numVar])
  boxplot(as.data.frame(df[,numVar]),main=main,xlab=xlab,ylab=ylab,las=3,
          cex=0.7,col=col)
          
  }

########### Combination calcululator





#py <- plot_ly(username="mazzara", key="khyseh7ldw")







