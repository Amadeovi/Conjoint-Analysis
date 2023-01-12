# load the library to estimate multinomial choice models. 
# This package needs to be installed first.
library(mlogit)  # estimation of multinomial logit models.
library(lattice) 
library(knitr)
library(car)
library(nnet)
library(AER)
library(ggplot2)     # doing graphs in R.
library(rmarkdown)
library(gmnl)       # generalised multinomial logit model
library(R.utils)  # for inserting values inside a vector.
library(tibble)

# load (simulated) data about ebook readers
cbc <- read.csv(url("http://www.preferencelab.com/data/Ebook_Reader.csv"))
# N = 200 respondents, S = 4 alternatives per set, C = 10 choice sets, N * S * C = 8000 observations
cbc['chid']=(cbc['Resp_id']-1)*10+cbc['Set_id']


### convert data for mlogit ###
# Selected is the name of the dependent variable, i.e., the choice
# shape "long" means that each row represents one alternative; shape "wide" uses one row per choice set
# Alt_id identifies the variable with the alternative id
abc <- mlogit.data(cbc, choice="Selected", shape="long", alt.var="Alt_id", id.var = "Resp_id")


### Calculate models ###

############
# partworth model, use effect-coded variables, "| 0" means that
# no alternative-specific constants should be estimated. 
# When set to 1, delete None option.
ml1 <- mlogit(Selected ~ Storage_4GB + Storage_8GB + 
                Screen.size_5inch + Screen.size_6inch + 
                Color_black + Color_white + 
                Price_79 + Price_99 + Price_119 + 
                None | 0, abc)
summary(ml1)


coeffs = insert(coef(ml1),c(3,5,7,10),values=c(-(coef(ml1)["Storage_4GB"] + coef(ml1)["Storage_8GB"]),
                                              -(coef(ml1)["Screen.size_5inch"] + coef(ml1)["Screen.size_6inch"]),
                                              -(coef(ml1)["Color_black"] + coef(ml1)["Color_white"]),
                                              -(coef(ml1)["Price_79"] + coef(ml1)["Price_99"] + coef(ml1)
                                                ["Price_119"])),useNames = FALSE)
names(coeffs)=c("Storage_4GB", "Storage_8GB", "Storage_16GB",
                "screen.size_5inch", "Screen.size_6inch", "Screen.size_7inch", 
                "Color_black", "Color_white","Color_silver",
                "Price_79","Price_99","Price_119","Price_139","None")
colors=c(rep('Storage',3),rep('ScreenSize',3),rep('Color',3),rep('price',4),'none')
 
df=data.frame(coeffs,names(coeffs),colors)

limits=c("Storage_4GB", "Storage_8GB", "Storage_16GB",
         "screen.size_5inch", "Screen.size_6inch", "Screen.size_7inch", 
         "Color_black", "Color_white","Color_silver",
         "Price_79","Price_99","Price_119","Price_139","None")

ggplot(df, aes(x = names.coeffs. , y = coeffs, fill=colors))+
  geom_col(width = 0.7) + coord_flip() + scale_x_discrete(limits=limits)+
  labs(y= "Coefficients", x = "Levels")+geom_text(aes(label = round(coeffs,3)))

relimportance=c()
for(k in unique(df$colors)){
  relimportance=append(relimportance,max(df$coeffs[df$colors==k])-min(df$coeffs[df$colors==k]))
}

relimportance=relimportance/sum(relimportance)*100
m<-data.frame(relimportance,unique(df$colors))
ggplot(m, aes(x = reorder(unique.df.colors.,-relimportance) , y = relimportance))+
  geom_bar(stat="identity", fill="steelblue", width = 0.6)+labs(y= "Relative Importance", x = "Attributes")+
  geom_text(aes(label = round(relimportance,2)), size = 3, hjust = 0.5, vjust = 3)
 

########
# We can also treat Price as a continuous random variable, which allow us
# to compute the so-called willingness to pay. Moreover, replacing parameters
# leads to a more simple model, and the model fit is not significantly worse
# (likelihood ratio test)

ml2 <- mlogit(Selected~ Storage_4GB + Storage_8GB +
                Screen.size_5inch + Screen.size_6inch + 
                Color_black + Color_white + 
                Price + 
                None | 0, abc)
summary(ml2)


# likelihood ratio test -> no significant differences.
lrtest(ml2, ml1)

# We can compute the Willingness to Pay:
# It is based on the idea to analyze how
# much utility is lost (gained) when the price increases (decreases) and to relate this
# utility difference to the partworth utility of an attribute level.

WTP_black=coef(ml2)["Color_black"]/coef(ml2)["Price"]
WTP_white=coef(ml2)["Color_white"]/coef(ml2)["Price"]
WTP_silver=(-coef(ml2)["Color_white"]-coef(ml2)["Color_black"])/coef(ml2)["Price"]
unname(WTP_silver, force = FALSE)

print(paste("Consumers would in average be willing to spend",toString(WTP_silver-WTP_white),
            "??? for upgrading from a silver ebook reader to a white product"))



# We can also use Market Simulators:
# To see for example how likely it is that consumers buy an ebook with
# determined features or no ebook at all. To do so we can use the MLN function that we have used
# for estimating this model. We can also see how this probability changes when changing the price.
# Features: 4 GB storage, a 6-in. screen, in the color black.
price=c(79,99,119,139)
probs=c()
a=coef(ml2)["Storage_4GB"]+coef(ml2)["Screen.size_6inch"]+coef(ml2)["Color_black"]
a=unname(a, force=FALSE)
for(i in j){
  probs=append(probs,exp(a+coef(ml2)["Price"]*i)/(exp(a+coef(ml2)["Price"]*i)+exp(coef(ml2)["None"])))
}
df=data.frame(price,probs)
ggplot(df,aes(x=price, y=probs))+geom_point(color="red")+geom_smooth(method=lm)+
  labs(x="Price",y="Probabilty of purchase")



#########
# We can also treat Storage as a continuous Random Variable, in this case we have:
ml3 <- mlogit(Selected ~ Storage +
                Screen.size_5inch + Screen.size_6inch +
                Color_black + Color_white +
                Price +
                None | 0, abc)
summary(ml3)
# Actually the performance is not worse than the first fitted model:
lrtest(ml3, ml1)

# Having a continuous variable we can compute the incremental willingness-to-pay for storage:
b=coef(ml3)["Storage"]/coef(ml3)["Price"]
print(paste("Consumers would in average be willing to spend",toString(-b),"??? for each Gb of Storage"))



######
# Other types of simple models:
### replacing screen size with a vector model leads to a significant drop in fit ###
ml4 <- mlogit(Selected ~ Storage + Screen.size + Color_black + Color_white + Price + None | 0, abc)
summary(ml4)

lrtest(ml4, ml2)

### Testing an ideal point model for screen size ###
ml5 <- mlogit(Selected ~ Storage + 
                Screen.size + I(Screen.size**2) + 
                Color_black + Color_white + 
                Price + 
                None | 0, abc)
summary(ml5)

# same model fit because no difference in degrees of freedom
lrtest(ml5, ml2)

# what is the ideal point? 
screenPreference <- function(inch) {
  inch * coef(ml5)[2] + inch**2 * coef(ml5)[3]
}

idealPoint <- optimize(screenPreference, c(0:15), maximum = T)
# ideal point =  5.870546
plot(screenPreference, from = 3, to = 10)
abline(v = idealPoint, lty = 3)

### Adding interaction effects between screen size and color ###
ml6 <- mlogit(Selected ~ Storage + 
                Screen.size_5inch + Screen.size_6inch + 
                Color_black + Color_white + 
                Price + 
                Screen.size_5inch * Color_black + Screen.size_6inch * Color_black + 
                Screen.size_5inch * Color_white + Screen.size_6inch * Color_white +
                None| 0, abc)
summary(ml6)

# likelihood ratio test
lrtest(ml2, ml6)


#########
# We then start estimating more complex models to account for heterogeneity.


# The first of these methods is the so called: Latent Class-Model or 
# or the discrete Mixture logit model.
# Q is the name of different classes that we want to split.
# We should run this model for different values of Q, compute the AIC,BIC
# and choose the one with the lowest value for that criterion.
# In our case the best model was Q=3:
lc1<- gmnl(Selected ~ Storage_4GB + Storage_8GB +Screen.size_5inch + Screen.size_6inch + 
             Color_black + Color_white + 
             Price_79 + Price_99 + Price_119 + 
             None | 0 | 0 | 0 | 1 , data = abc, model = 'lc',Q = 3, subset = 1:8000,
           panel = TRUE, method = "NR")
Q=3 # For Q up to 4 the computational time is too much.
summary(lc1)
# We can see that the loglikelihood is greater than the previous models, but we can stil improve it.

coeffs=matrix(coef(lc1)[1:(Q*10)],nrow=Q,byrow=TRUE)
coeffs=data.frame(coeffs)
coeffs=add_column(coeffs,-coeffs[,1]-coeffs[,2], .after = "X2")
coeffs=add_column(coeffs,-coeffs[,4]-coeffs[,5], .after = "X4")
coeffs=add_column(coeffs,-coeffs[,7]-coeffs[,8], .after = "X6")
coeffs=add_column(coeffs,-coeffs[,10]-coeffs[,11]-coeffs[,12], .after = "X9")
colnames(coeffs)=limits
df=data.frame(colnames(coeffs),t(coeffs[1,]),t(coeffs[2,]),t(coeffs[3,]),colors)


ggplot(df,aes(x=colnames.coeffs., y= X1 ,fill=colors))+geom_col(width = 0.7)+coord_flip()+
  scale_x_discrete(limits=limits)+labs(title="Utilities Class 1",x="Levels",y="Coefficients")+
  geom_text(aes(label = round(X1,3)))
ggplot(df,aes(x=colnames.coeffs., y= X2 ,fill=colors))+geom_col(width = 0.7)+coord_flip()+
  scale_x_discrete(limits=limits)+labs(title="Utilities Class 2",x="Levels",y="Coefficients")+
  geom_text(aes(label = round(X2,3)))
ggplot(df,aes(x=colnames.coeffs., y= X3 ,fill=colors))+geom_col(width = 0.7)+coord_flip()+
  scale_x_discrete(limits=limits)+labs(title="Utilities Class 3",x="Levels",y="Coefficients")+
  geom_text(aes(label = round(X3,3)))


       
##########    
# We continues treating the part-worth utilies as random variables. 
# to account for the heterogeneity. To do so we  consider
# the Mixed Continuous Logit Model.


# First we use the mlogit package to carry out this task.
set.seed(123)
mixl1<- mlogit(Selected ~ Storage_4GB + Storage_8GB +Screen.size_5inch + 
               Screen.size_6inch + Color_black + Color_white + 
               Price_79 + Price_99 + Price_119 + None | 0 ,
             data = abc, R=600, panel = TRUE, halton=NULL,
             rpar = c(Storage_4GB="n", Storage_8GB="n", Screen.size_5inch="n",
                      Screen.size_6inch="n", Color_black="n", Color_white="n",
                      Price_79="n",Price_99="n",Price_119="n"))
# We can see that the loglikelihood is lesser than in the previous model
# But still the fact of imposing normality could be too restrictive
summary(mixl1)


# Then we use the gmnl package to carry out this estimation procedure.
# Moreover for this model, we also take into account possible correlation
# between the different attributes.
set.seed(123)
mixl2 <-gmnl(Selected ~ Storage_4GB + Storage_8GB +Screen.size_5inch + 
              Screen.size_6inch + Color_black + Color_white + 
              Price_79 + Price_99 + Price_119 + None| 0, data = abc,
              subset = 1:8000,model = 'mixl',R = 10,panel = TRUE,
              ranp = c(Storage_4GB = "n", Storage_8GB = "n", Screen.size_5inch = "n",
                       Screen.size_6inch = "n", Color_black = "n", Color_white="n",
                       Price_79="n",Price_99="n",Price_119="n"),
                 correlation = TRUE)

summary(mixl2)
cov.gmnl(mixl2)
se.cov.gmnl(mixl2)
se.cov.gmnl(mixl2, sd = TRUE)
# correlation matrix allow us to see the interactions between the different levels of the attributes.
cor.gmnl(mixl2)

plot(mixl2,par="Storage_4GB",effect="ce",breaks=10,col="blue")
plot(mixl2,par="Screen.size_6inch",effect="ce",breaks=10,col="blue")


# Let's treat the Price as a continuous random Variable, to see what happens:
mixl3 <-gmnl(Selected ~ Storage_4GB + Storage_8GB +Screen.size_5inch + 
               Screen.size_6inch + Color_black + Color_white + 
               Price + None| 0, data = abc,
             subset = 1:8000,model = 'mixl',R = 10, panel = TRUE,
             ranp = c(Storage_4GB = "n", Storage_8GB = "n", Screen.size_5inch = "n",
                      Screen.size_6inch = "n", Color_black = "n", Color_white="n",
                      Price="n"),correlation = TRUE)

# Notice that treating Price as a continuous random variable 
# lead us to better results than previous models.
summary(mixl3)

# Moreover we can compute willingness to Pay.
plot(mixl3,par="Storage_4GB",effect="wtp",wrt="Price")
plot(mixl3,par="Color_white",effect="wtp",wrt="Price")
plot(mixl3,par="Screen.size_6inch",effect="wtp",wrt="Price")
plot(mixl3,par="Storage_4GB",effect="ce")







