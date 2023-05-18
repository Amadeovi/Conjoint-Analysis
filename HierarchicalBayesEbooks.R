# Welcome to Hierarchichal Bayes Ebooks program in R.
# load the library to estimate multinomial choice models. 
# This package needs to be installed first.
library(mlogit)   # perform multinomial logit fitting
library(lattice) 
library(knitr)
library(car)
library(nnet)
library(AER)  
library(ggplot2)    # nice plots
library(rmarkdown)
library(gmnl)      # generalised multinomial logit models
library(bayesm)    # perform different types of hierarchical bayes methods
library(ChoiceModelR)   # main package used during this script for estimation
library(MASS)
library(lattice)
library(Matrix)
library(readxl) 
library(R.utils)     # melting the data for the boxplots
library(reshape2)    # changing the size of data frames
library(plyr)     # computing means of the different utilities for the density est.plots
library(useful)   # get a 2-D plot for the different clusters of the k-means algorithm
library(writexl)

# Load the data set we are working with
cbc <- read.csv(url("http://www.preferencelab.com/data/Ebook_Reader.csv"))


# Clean the dataset to have it in the adequate format for the estimation:
y=c(1,2,3,0)
z=c(1,2,3,4,0)
cbc$Storage=y[match(cbc$Storage,c(4,8,16,0))]
cbc$Screen.size=y[match(cbc$Screen.size,c(5,6,7,0))]
cbc$Color=y[match(cbc$Color,c("Silver","White","Black",0))]
cbc$Price=z[match(cbc$Price,c(79,99,119,139,0))]
a<-(which(cbc$Selected==1))%%4
a[a==0]=4
b<-integer(8000)
b[seq_along(b)%%4 ==1]=a
cbc['y']=b
cbc<-cbc[!(cbc$None==1),]
ebooks<-cbc[,-c(4,5,(10:18))]

write_xlsx(ebooks,'C:/Users/amade/OneDrive/Escritorio/EbooksExampleR.xlsx')

# Once the data set is on the adequate format we can start with the computations:
# As our 4 attributes are categorical:
xcoding = c(0, 0, 0, 0)
# For specifying the features of the Markov Chain Monte Carlo method:
mcmc = list(R = 8000, use = 2000)
# Options of the choice model:
options = list(none=TRUE, save=TRUE, keep=1)
# Number of attributes and its levels
attlevels = c(3, 3, 3, 4)
# Constraints over the levels of the attributes:
constype = c(1,0,0,2)
constraints = vector("list", 4)
for (i in 1:length(attlevels)) {
  constraints[[i]] = diag(0, attlevels[i])
  if (constype[i] == 1) {
    constraints[[i]][upper.tri(constraints[[i]])] = -1
  }
  else if (constype[i] == 2) {
    constraints[[i]][upper.tri(constraints[[i]])] = 1
  }
}


# Lets initiate the Markov Chain Monte Carlo method for computing
# individual part-worth utilities.
out = choicemodelr(ebooks, xcoding, mcmc = mcmc, options = options, constraints = constraints)
# There are some interesting statistics out of this estimation
# such as out$betadraw, out$betadraw.c or out$loglik which gives us the loglikelihoods
# of the respondents given the estimators of the n-th estimation.

# For the likelihood we even created a R-file to compute the likelihood
# given some estimators, to compare the performance between the estimation
# in Python and in R.
mean(out$loglike)
# A RBetas excel has been generated with the estimates of the betas 
# for each of the individual respondents.
estbetas <- read.csv('C:/Users/amade/OneDrive/Documentos/RBetas.csv')
colnames(estbetas) = c("Id","Storage_4GB", "Storage_8GB", "Storage_16GB",
                       "screen.size_5inch", "Screen.size_6inch", "Screen.size_7inch", 
                       "Color_silver", "Color_white","Color_black",
                       "Price_79","Price_99","Price_119","Price_139","None")


# We are interested in visualizing the result using box-plots.
estbetas=data.frame(estbetas)
estbetas_long <- melt(estbetas[-1])  
estbetas_long['colors']=c(rep('Storage',600),
                          rep('Screen.size',600),rep('color',600)
                          ,rep('price',800),rep('none',200))
# Reshaping data frame
head(estbetas_long) 
x=c('4GB','8GB','16GB','5inch','6inch','7inch','silver','white','black','79','99','119','139','none')
# Making boxplot
ggplot(estbetas_long, aes(x = variable, y = value, fill=colors))+geom_boxplot()+
  scale_x_discrete(labels=x)+labs(title="Boxplots for the Individual Partworth Utilies")



# We can also obtain density function estimators
# for each of the attribute-level of the product:
#-storage:
estbetasStorage <- melt(estbetas[2:4])
muStorage <- ddply(estbetasStorage, "variable", summarise, grp.meanSt=mean(value))
ggplot(estbetasStorage, aes(x=value,color=variable,fill=variable)) + geom_density(alpha=0.4)+
  geom_vline(data=muStorage, aes(xintercept=grp.meanSt, color=variable),linetype="dashed")
#-screen size:
estbetasScreenSize <- melt(estbetas[5:7])
muScreenSize<- ddply(estbetasScreenSize, "variable", summarise, grp.meanSc=mean(value))
ggplot(estbetasScreenSize, aes(x=value,color=variable,fill=variable)) + geom_density(alpha=0.4)+
  geom_vline(data=muScreenSize, aes(xintercept=grp.meanSc, color=variable),linetype="dashed")
#-color
estbetasColor <- melt(estbetas[8:10])
muColor<- ddply(estbetasColor, "variable", summarise, grp.meanCol=mean(value))
ggplot(estbetasColor, aes(x=value,color=variable,fill=variable)) + geom_density(alpha=0.4)+
  geom_vline(data=muColor, aes(xintercept=grp.meanCol, color=variable),linetype="dashed")
#-Price
estbetasPrice <- melt(estbetas[11:14])
muPrice<- ddply(estbetasPrice, "variable", summarise, grp.meanPr=mean(value))
ggplot(estbetasPrice, aes(x=value,color=variable,fill=variable)) + geom_density(alpha=0.4)+
  geom_vline(data=muPrice, aes(xintercept=grp.meanPr, color=variable),linetype="dashed")



# Then we perform some types of clustering with the main objective
# of partition the population into different classes. Similar to what we did when
# we used the Latent Class Model, but less computationally expensive 
# and getting more accurate results:

# K-means Clustering:
#wss <- (nrow(estbetas[-1])-1)*sum(apply(estbetas[-1],2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(estbetas[-1],
#                                    centers=i)$withinss)
#wss <- dataFrame(wss)
#plot(1:15, wss,type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")

# Lets choose 3 different clusters:
#set.seed(3)
#estbetasK3 <- kmeans(x=estbetas[-1], centers=3)
#plot(estbetasK3, data=estbetas[-1])


#estbetasK3N25 <- kmeans(estbetas[-1], centers=3, nstart=25)
#estbetasK3$size

#estbetasBest <- FitKMeans(estbetas[-1], max.clusters=20, nstart=25,  seed=3)


