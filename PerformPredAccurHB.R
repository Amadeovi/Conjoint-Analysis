# This package needs to be installed first.
library(mlogit)
library(lattice) 
library(knitr)
library(car)
library(nnet)
library(AER)  
library(ggplot2)
library(rmarkdown)
library(gmnl)
library(bayesm)
library(ChoiceModelR)
library(MASS)
library(lattice)
library(Matrix)
library(readxl)
library(R.utils) 
library(reshape2)

# Load the data set we are working with:
cbc <- read.csv(url("http://www.preferencelab.com/data/Ebook_Reader.csv"))


# The main objective of this script is to train a model wiht 80% of the data
# and carry out an predictive assesment using the remaining 20%.
# The way of doing this splitting: 
# 8 first questions: train, 2 last questions: test.

train <- cbc[cbc$Set_id %in% c(1:8),]
test <- cbc[cbc$Set_id %in% c(9,10),]
# Clean the dataset to have it in the adequate format for the estimation:
y=c(1,2,3,0)
z=c(1,2,3,4,0)
train$Storage=y[match(train$Storage,c(4,8,16,0))]
train$Screen.size=y[match(train$Screen.size,c(5,6,7,0))]
train$Color=y[match(train$Color,c("Silver","White","Black",0))]
train$Price=z[match(train$Price,c(79,99,119,139,0))]
a<-(which(train$Selected==1))%%4
a[a==0]=4
b<-integer(6400)
b[seq_along(b)%%4 ==1]=a
train['y']=b
train<-train[!(train$None==1),]
train<-train[,-c(4,5,(10:18))]


# Once the training data set is on the adequate format 
# we can start with the computations:
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
out = choicemodelr(train, xcoding, mcmc = mcmc, options = options, constraints = constraints)
mean(out$loglike)


# A RBetas excel has been generated with the estimates of the betas 
# for each of the individual respondents.
estbetas <- read.csv('C:/Users/amade/OneDrive/Documentos/RBetas.csv')
colnames(estbetas) = c("Id","Storage_4GB", "Storage_8GB", "Storage_16GB",
                       "screen.size_5inch", "Screen.size_6inch", "Screen.size_7inch", 
                       "Color_silver", "Color_white","Color_black",
                       "Price_79","Price_99","Price_119","Price_139","None")

# The performance of the Hierarcical Bayes Mixed Multinomial Logit is given by:
# pred1 -> choosing the option with maximum utility
# pred2 -> using the choice model
set.seed(10)
pred1=0
pred2=0
for(i in 1:max(estbetas$Id)){
  for(j in 1:2){
    a1=sum(test[(1+(i-1)*8+(j-1)*4),c(10:18,4)]*estbetas[i,c(2:3,5:6,8:9,11:13,15)])
    a2=sum(test[(2+(i-1)*8+(j-1)*4),c(10:18,4)]*estbetas[i,c(2:3,5:6,8:9,11:13,15)])
    a3=sum(test[(3+(i-1)*8+(j-1)*4),c(10:18,4)]*estbetas[i,c(2:3,5:6,8:9,11:13,15)])
    a4=sum(test[(4+(i-1)*8+(j-1)*4),c(10:18,4)]*estbetas[i,c(2:3,5:6,8:9,11:13,15)])
    p1=exp(a1)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p2=exp(a2)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p3=exp(a3)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p4=exp(a4)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    m=which(test$Selected==1)%%4
    m[m==0]=4
    pred1=pred1+(which.is.max(c(a1,a2,a3,a4))==m[(i-1)*2+j])
    pred2=pred2+(sum(runif(1)>cumsum(c(0,p1,p2,p3)))==m[(i-1)*2+j])
  }
}
paste('The first accuracy prediction is given by',toString(pred1/400))
paste('The second accuracy prediction is given by', toString(pred2/400))



# Then we fit Multinomial Logit Models for each individual
# and we use the same predictive accuracy methods:
cbc <- read.csv(url("http://www.preferencelab.com/data/Ebook_Reader.csv"))
abc <- mlogit.data(cbc, choice="Selected", shape="long", alt.var="Alt_id", id.var = "Resp_id")



pred1=0
pred2=0
for(i in 1:max(cbc$Resp_id)){
  ml1<-mlogit(Selected ~ Storage_4GB + Storage_8GB + 
           Screen.size_5inch + Screen.size_6inch + 
           Color_black + Color_white + 
           Price_79 + Price_99 + Price_119 + 
           None | 0, abc[(1+(i-1)*10):(8+(i-1)*10),])
  for(j in 1:2){
    a1=sum(test[(1+(i-1)*8+(j-1)*4),c(10:18,4)]*coef(ml1))
    a2=sum(test[(2+(i-1)*8+(j-1)*4),c(10:18,4)]*coef(ml1))
    a3=sum(test[(3+(i-1)*8+(j-1)*4),c(10:18,4)]*coef(ml1))
    a4=sum(test[(4+(i-1)*8+(j-1)*4),c(10:18,4)]*coef(ml1))
    p1=exp(a1)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p2=exp(a2)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p3=exp(a3)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    p4=exp(a4)/(exp(a1)+exp(a2)+exp(a3)+exp(a4))
    m=which(test$Selected==1)%%4
    m[m==0]=4
    pred1=pred1+(which.is.max(c(a1,a2,a3,a4))==m[(i-1)*2+j])
    pred2=pred2+(sum(runif(1)>cumsum(c(0,p1,p2,p3)))==m[(i-1)*2+j])
  }
}






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







