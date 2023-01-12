import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import random
import seaborn as sns
from sklearn.cluster import KMeans
from scipy.interpolate import interp1d
sns.set()
sns.set_style("darkgrid")

""" The main objective of this script is to create a market simulator.
This simulator can transform raw data into simulated market choices.
It has plenty of applications that we are going to show below.
The additional features is that we treat price as a continuous variable
and that we include confidence intervals for our estimators."""

# First we import the data estimated using the 
# ChoiceModelR package in R:
estbetas = pd.read_csv('C:/Users/amade/OneDrive/Documentos/RBetas.csv')
estbetas.columns = ['Id','4GB','8GB','16GB','5inch','6inch','7inch',
                    'black','white','silver','79','99','119','139','none']

# Below we interpolate the Price Utilities (PU) for each customer to 
# find a continuous relation between price and utilities, to be able to 
# incorporate continuous prices in our study.
price = ['79','99','119','139']

for i in range(0,200):
    
    globals()['PU'+str(i)] = interp1d(
        np.array([float(k) for k in price]),
        np.array([estbetas[j][i] for j in price]),
        kind='quadratic')





# GENERALISATION OF THE MARKET SIMULATOR:
# such that we can choose the number of competitors that we want
# with the features that we want:
print('Introducing new products/Checking price elasticities:')
print('Please introduce price as the last attribute')
n = int(input('Introduce the number of competitors: ')) 
  
for i in range(1,n+1):
    
    globals()['f'+str(i)] = input("Introduce features of competitor"+str(i)+" ").split()
    
    if globals()['f'+str(i)] != ['none']:
        
        globals()['util'+str(i)] = sum(
            [estbetas[j] for j in globals()['f'+str(i)][:-1]])+pd.Series(
            [globals()['PU'+str(l)](float(globals()['f'+str(i)][-1]))
                                         for l in range(0,200)])
        
    else:
        
        globals()['util'+str(i)] = estbetas['none']
       
        
myprod = input("Introduce features of our product: ").split()

# Below we proceed to sample which product would choose each individual.
# As we are dealing with probabilities, it is useful to repeat the
# procedure several times to obtain more robust estimators
# and confidence intervals.

iterations = int(input('How many samples do you want? '))

for i in range(1,n+2):
        
        globals()['COUNTER'+str(i)] = [];

# We treat price as discrete and then we interpolate 
# to treat it as a continuous attribute.

prices = np.array([79,99,119,139])
pricenew = np.linspace(79, 139, num=100, endpoint=True)
optprice = [];

for it in range(0,iterations):
    
    for i in range(1,n+2):
        
        globals()['counter'+str(i)] = [];
    
    price = ['79','99','119','139']
    
    for j in price:
        
        d = sum([estbetas[k] for k in myprod]) + estbetas[j]
        results = [];
        
        for l in range(0,200):
            
            results.append(random.choices([i for i in range(1,n+2)],
                 weights=tuple([np.exp(globals()['util'+str(i)][l]) 
                                for i in range(1,n+1)]+[np.exp(d[l])]),k=1))
            
        for i in range(1,n+2):
            
            globals()['counter'+str(i)].append(results.count([i])/2)
         
        for i in range(1,n+2):
            
            globals()['COUNTER'+str(i)].append(globals()['counter'+str(i)])
    
    prob = np.array([int(j)/200 for j in globals()['counter'+str(n+1)]])    
    g = interp1d(prices,prob,kind='quadratic')
    optprice.append(float(pricenew[np.where(pricenew*g(pricenew) == 
                             np.amax(pricenew*g(pricenew)))]))



# We average results for the COUNTER to have at least a plot to observe results:
prob = np.array([np.mean([globals()['COUNTER'+str(n+1)][i][j] for i in range(0,200)])
                  for j in range(0,len(globals()['COUNTER'+str(n+1)][0]))])
g = interp1d(prices,prob,kind='quadratic')

print('This is the elasticity curve for our product:'+'\n'+str(myprod)+
      '\n'+'When the competitors are:')
for i in range(1,n+1):
    print('Competitor'+str(i)+': '+str(globals()['f'+str(i)]))

pricenew = np.linspace(79, 139, num=100, endpoint=True)
plt.plot(prices, prob, 'o', pricenew, g(pricenew), '-')
plt.legend(['data', 'quadratic'], loc='best')
plt.title('Elasticity Curve for our Product')
plt.xlabel('Price (€)')
plt.ylabel('Probability of purchase')
plt.show()


# It also interesting to see how the percentage of sharing for each competitor
# varies as we change the prices.
print('This is the share of preferences curve of each of the products')

shares = np.zeros(4);
for l in range(1,n+2):
    sharespri = shares
    shares = shares + np.array([np.mean([globals()['COUNTER'+str(l)][i][j] for i in range(0,200)])
                  for j in range(0,len(globals()['COUNTER'+str(n+1)][0]))])
    
    fshares = interp1d(prices,sharespri,kind='quadratic')
    gshares = interp1d(prices,shares,kind='quadratic')
    plt.plot(pricenew,gshares(pricenew))
    x = np.arange(79, 139, 0.25)
    plt.fill_between(x,fshares(x),gshares(x))


plt.title('Share of Preferences')
plt.xlabel('Price of our product(€)')
plt.ylabel('Probability of purchase')
plt.legend(['competitor'+str(i) for i in range(1,n+1)]+['my product'], loc='best')
plt.show()



# We can also try to find the optimal price to maximize revenue.
plt.plot(pricenew, pricenew*g(pricenew))
plt.axvline(x=np.median(optprice),color='r')
plt.title('Revenue vs Price')
plt.xlabel('Price (€)')
plt.ylabel('Revenue')
plt.show()


print('The price to maximize revenue is: '+
      str(np.round(np.median(optprice),2)))

print('The Maximum Revenue is given by: ' + 
      str(np.round(np.median(optprice)*g(np.median(optprice)),2)))


print('The 95% confidence interval is: ['+
      str(np.round(np.sort(optprice)[int(iterations*0.025)],2))+','+
      str(np.round(np.sort(optprice)[int(iterations*0.975)],2))+']')


plt.hist(optprice)
plt.axvline(x=np.median(optprice),color='r')
plt.title('Histogram of the Optimal Prices for our product')





















# GENERALISATION OF WILLINGNESS TO PAY PER FEATURE.
# The following challengue is to compute the Willingness to Pay per feature.
# To do so, we build the following tool.
# We include competitors, this fact is motivated in the paper:
# Estimating WTP given competition in Conjoint Analysis.
# Moreover we allow the tool to vary more than one attribute at a time.

print('Computing Willingness to Pay per feature:')
print('Please introduce Price as the last attribute')
n = int(input('Introduce the number of competitors: ')) 
  
for i in range(1,n+1):
    
    globals()['f'+str(i)] = input("Introduce features of competitor"+str(i)+" ").split()
    
    if globals()['f'+str(i)] != ['none']:
        
        globals()['util'+str(i)] = sum(
            [estbetas[j] for j in globals()['f'+str(i)][:-1]])+pd.Series(
            [globals()['PU'+str(l)](float(globals()['f'+str(i)][-1]))
                                         for l in range(0,200)])
        
    else:
        
        globals()['util'+str(i)] = estbetas['none']


myprod = input("Introduce features of our product: ").split()
pricemyprod = int(input("Introduce the price of our product again: "))

utilmyprod = sum([estbetas[j] for j in myprod[:-1]])+pd.Series(
    [globals()['PU'+str(l)](float(myprod[-1]))for l in range(0,200)])


# Then we choose the levels feature that we want to change:
oldf = input("Introduce the features that you want to change: ").split()

for i in oldf:
    if i not in myprod:
        print("The features that you want to change are not in the product")
        quit

newf = input("Introduce the new feature of your product: ").split()
# we remove the price and the old feature:
newprod = myprod[:-1]
for i in oldf:
    newprod.remove(i)

newprod = newprod + newf


# We repeat the procedure iterations times in order to find confidence intervals
# and more robust estimators.

iterations = int(input('How many samples do you want? '))

for i in range(1,n+2):
    
    globals()['counter'+str(i)] = [];

for it in range(0,iterations):
    
    results = [];
    
    for l in range(0,200):
        
        results.append(random.choices([i for i in range(1,n+2)],
                 weights=tuple([np.exp(globals()['util'+str(i)][l]) 
                        for i in range(1,n+1)]+[np.exp(utilmyprod[l])]),k=1))
        
    for i in range(1,n+2):
        
        globals()['counter'+str(i)].append(results.count([i])/200)
    
sharingWTP = pd.DataFrame([np.mean( globals()['counter'+str(i)]) for i in range(1,n+2)])
sharingWTP.columns = ['Share of preferences']
sharingWTP.index = ['Competitor'+str(i) for i in range(1,n+1)]+['My product']
print(sharingWTP)

# Once the preference of share for the old product is computed,
# we carry out the same procedure but with the new product.
# Moreover we do it for the different prices and then we interpolate
# to treat price as a continuous variable.
    
for i in range(1,n+2):
    
    globals()['NEWCOUNTER'+str(i)] = [];    
    
    
for it in range(0,iterations):

    for i in range(1,n+2):
        
        globals()['newcounter'+str(i)] = [];    
        
    price = ['79','99','119','139']
    
    for j in price:
        
        d = sum([estbetas[k] for k in newprod]) + estbetas[j]
        
        results = [];
        
        for l in range(0,200):
            
            results.append(random.choices([i for i in range(1,n+2)],
                 weights=tuple([np.exp(globals()['util'+str(i)][l]) 
                                for i in range(1,n+1)]+[np.exp(d[l])]),k=1))
            
        for i in range(1,n+2):
            
            globals()['newcounter'+str(i)].append(results.count([i])/200)
            
    for i in range(1,n+2):
        
        globals()['NEWCOUNTER'+str(i)].append( globals()['newcounter'+str(i)])
        
        

# We then want to find the cutting price such that the share of preference
# is the same as we computed for the initial value. To do so, we have
# to make an interpolation. 

optprice = [];

for it in range(0,iterations):
    
    price = np.array([79,99,119,139])
    prob = np.array([j for j in globals()['NEWCOUNTER'+str(n+1)][it]])
    g = interp1d(price,prob,kind='quadratic')
    
    pricenew = np.linspace(79, 139, num=100, endpoint=True)
    optprice.append(float(pricenew[np.where(np.abs(g(pricenew)-globals()['counter'+str(n+1)][it])==
            min(np.abs(g(pricenew)-globals()['counter'+str(n+1)][it])))][0]))
    

    
# Moreover we plot the average values obtained previously:
price = np.array([79,99,119,139])  
prob = np.array([np.mean([globals()['NEWCOUNTER'+str(n+1)][i][j] for i in range(0,iterations)])
                  for j in range(0,len(globals()['NEWCOUNTER'+str(n+1)][0]))])
g = interp1d(prices,prob,kind='quadratic')


plt.plot(price, prob, 'o', pricenew, g(pricenew), '-')
plt.legend(['data', 'quadratic'], loc='best')
plt.axhline(y=np.median(globals()['counter'+str(n+1)]), color='r', linestyle='-')
plt.scatter(np.median(optprice),np.median(globals()['counter'+str(n+1)]),color='g')
plt.text(np.median(optprice),np.median(globals()['counter'+str(n+1)]),
         str(np.round(np.median(optprice),2)))
plt.title('Elasticity Curve')
plt.xlabel('Price (€)')
plt.ylabel('Probability of purchase')
plt.show()


WTP = np.median(optprice)-pricemyprod

text = ''
for i in range(0,len(newf)):
    text = text + newf[i] + ' '
    
WTP = np.median(optprice)-pricemyprod
print('The willingness to pay to include the features '+ text +
      ' is: '+str(np.round(WTP,2)))
print('The 95% confidence interval is: ['+
      str(np.round(np.sort(optprice)[int(iterations*0.025)]-pricemyprod,2))+
      ','+str(np.round(np.sort(optprice)[int(iterations*0.975)]-pricemyprod,2))+']')

plt.hist([optprice[i]-pricemyprod for i in range(0,len(optprice))])
plt.axvline(x=np.median(optprice)-pricemyprod,color='r')
plt.title('Histogram of the WTP for our product')








        












# GENERALISATION OF DESIGNING PRODUCT FOR MARKET SEGMENTS:
# The following tool that we build, takes into account the customer groups.
# The main objective is to identify the needs of the population
# and compute which product should we launch to satisfy the demand
# and obtain the maximum revenue out of it.


# First, we create a programme that computes the share of preferences 
# for the different clusters for the different competitor products. 
# We recall the clustering techniques used in the script: ClusteringCustomers.
# We use 3 clusters to carry out this example.

estbetas = estbetas.drop('Id',axis=1)
kmeans = KMeans(n_clusters=3, n_init=15,random_state=3)
kmeans.fit(estbetas)
yhat = kmeans.predict(estbetas)

estbetasclustered = pd.concat([estbetas,pd.DataFrame(yhat)],axis=1)
estbetasclustered.columns = ['4GB','8GB','16GB','5inch',
                             '6inch','7inch','black','white',
                             'silver','79','99','119','139',
                             'none','cluster']

print('Computing Market Shares per Costumer Clusters')
print('Please introduce Price as the last attribute')
n = int(input('Introduce the number of competitors: ')) 
  
for i in range(1,n+1):
    
    globals()['f'+str(i)] = input("Introduce features of competitor"+str(i)+" ").split()
    
    if globals()['f'+str(i)] != ['none']:
        
        globals()['util'+str(i)] = sum(
            [estbetas[j] for j in globals()['f'+str(i)][:-1]])+pd.Series(
            [globals()['PU'+str(l)](float(globals()['f'+str(i)][-1]))
                                         for l in range(0,200)])
        
    else:
        
        globals()['util'+str(i)] = estbetas['none']


myprod = input("Introduce features of our product: ").split()

utilmyprod = sum([estbetas[j] for j in myprod[:-1]])+pd.Series(
    [globals()['PU'+str(l)](float(myprod[-1]))for l in range(0,200)])


C = len(estbetasclustered.cluster.unique())
totalCOUNTER = [];
totalcounter = [];
iterations = int(input('How many samples do you want? '))

for i in range(0,C):
    
    globals()['COUNTER_cluster'+str(i)] = [];


for it in range(0,iterations):
    
    for i in range(0,C):
        
        globals()['counter_cluster'+str(i)] = [];
        
    results = [];
    totalcounter = [];
    
    for l in range(0,200):
        
        results.append(random.choices([i for i in range(1,n+2)],
                   weights=tuple([np.exp(globals()['util'+str(i)][l]) 
                   for i in range(1,n+1)]+[np.exp(utilmyprod[l])]),k=1))
        
    for i in range(1,n+2):
               
        totalcounter.append(results.count([i])/2)
        
        for j in range(0,C):
            
            globals()['counter_cluster'+str(j)].append(
            [results[k] for k in np.where(yhat==j)[0]].count([i])*100/sum(yhat==j))
            
    for i in range(0,C):
        
        globals()['COUNTER_cluster'+str(i)].append(globals()['counter_cluster'+str(i)])
        
    totalCOUNTER.append(totalcounter)

# Averaging the previous results:
for i in range(0,C):
    globals()['Median_cluster'+str(i)]=[np.median([globals()['COUNTER_cluster'+str(i)][j][k]
                    for j in range(0,200)]) for k in range(0,n+1)]

Median_total = [np.median([totalCOUNTER[j][k] for j in range(0,iterations)])
                for k in range(0,n+1)]


# Storaging them into a data frame, we have:
df = pd.DataFrame([globals()['Median_cluster'+str(i)]
                    for i in range(0,C)]+[Median_total]).T
df.columns = ['Cluster'+str(i)+', '+'n = '+str(sum(yhat==i)) 
              for i in range(0,C)]+['Total Share']
df.index = ['Competitor'+str(i) for i in range(1,n+1)]+['My product']
print(df)


# Plotting them to have better visual insights:
df.T.plot(kind='bar', stacked=True)
plt.show()


# With the previous data frame we can check in which segments
# we are weak compared to our competitors, and in which segments
# we are the leaders. Moreover, recalling the boxplots we can check
# with features are desirable for each cluster.

estbetas_long = pd.melt(estbetas)
zhat = yhat
for i in range(0,13):
    zhat = np.append(zhat,yhat)

estbetas_long['cluster'] = zhat  
my_pal = {"4GB": "lightgreen","8GB": "lightgreen", "16GB": "lightgreen",
          '5inch':'lightblue','6inch':'lightblue','7inch':'lightblue',
          'black':'r','white':'r','silver':'r',
          '79':'lightyellow','99':'lightyellow','119':'lightyellow','139':'lightyellow',
          'none':'w'}
         
sns.boxplot(x=estbetas_long["variable"], y=estbetas_long["value"],
            hue=estbetas_long['cluster'],linewidth=1).set_title('Clustering using K-Means')
plt.show()



# Imagine now that we have several possible products to launch and 
# we wonder which one should we carry on.
# We will compute how the total share changes when
# introducing each of these products. Moreover, we will compute the prices
# that maximize the total revenue, and we will compare these values. 
# It will be interesting to compare a random product with the preferred product
# of the cluster where we are weaker/ we are aiming for.

m = int(input("Introduce the number of new product that you want to test: "))

for i in range(1,m+1):
    
     globals()['mynewprod'+str(i)] = input(
         "Introduce features of new product"+str(i)+" ").split()
  

# We do not include price, since we will try to optimise it.
# Count+k+prod+i shows for newproduct i 
# the number of customers that would choose option k.


for i in range(1,m+1):
    
    for k in range(1,n+3):
        
         globals()['count'+str(k)+'prod'+str(i)]=[];
         globals()['COUNT'+str(k)+'prod'+str(i)]=[];

prices = ['79','99','119','139']

for it in range(0,iterations):
    
    for i in range(1,m+1):
        
        for j in prices:
            
            globals()['d'+str(i)] =  sum([estbetas[k] 
                    for k in globals()['mynewprod'+str(i)]]) + estbetas[j]
            
            globals()['results'+str(i)] = [];
            
            for l in range(0,200):
                
                globals()['results'+str(i)].append(
                random.choices([i for i in range(1,n+3)],
                 weights=tuple([np.exp(globals()['util'+str(p)][l] )
                                for p in range(1,n+1)]+[np.exp(utilmyprod[l])]
                               +[np.exp(globals()['d'+str(i)][l])]),k=1))
                
            for k in range(1,n+3):
                
                globals()['count'+str(k)+'prod'+str(i)].append(
                globals()['results'+str(i)].count([k])/2)
                
        for k in range(1,n+3):
            
             globals()['COUNT'+str(k)+'prod'+str(i)].append(
                 globals()['count'+str(k)+'prod'+str(i)])
             
             globals()['count'+str(k)+'prod'+str(i)]=[]
    



# Now we compute the price that maximize the revenue for each product
# and we compare them.
# Moreover we plot the results that we obtain:

for i in range(1,m+1):
    
    globals()['optprice'+str(i)] = [];
    globals()['maxrevenue'+str(i)] = [];
    price = np.array([79,99,119,139])
    
    
    for it in range(0,iterations):
        
        probmyprod = np.array([j for j in globals()['COUNT'+str(n+1)+'prod'+str(i)][it]])
        prob = np.array([j for j in globals()['COUNT'+str(n+2)+'prod'+str(i)][it]])
        
        g = interp1d(price,probmyprod,kind='quadratic')
        f = interp1d(price,prob,kind='quadratic')
        pricenew = np.linspace(79, 139, num=100, endpoint=True)
        
        globals()['optprice'+str(i)].append(pricenew[np.where(f(pricenew)*pricenew+
                                                              g(pricenew)*float(myprod[-1])
                ==np.amax(f(pricenew)*pricenew+g(pricenew)*float(myprod[-1])))])
        
        globals()['maxrevenue'+str(i)].append(
            g(globals()['optprice'+str(i)][-1])*globals()['optprice'+str(i)][-1]+
            f(globals()['optprice'+str(i)][-1])*float(myprod[-1]))
       
    
    globals()['OptMedPrice'+str(i)] = np.median(globals()['optprice'+str(i)])
    globals()['MaxMedRevenue'+str(i)] = np.median(globals()['maxrevenue'+str(i)])
    
""" 
pricenew = np.linspace(79, 139, num=100, endpoint=True)
for i in range(1,m+1):
    
    for it in range(0,100):
               
        globals()['a'+str(i)] = np.array(
        [globals()['COUNT'+str(n+1)+'prod'+str(i)][it][k] 
           for k in range(0,4)])
        
        globals()['b'+str(i)] = np.array(
        [globals()['COUNT'+str(n+2)+'prod'+str(i)][it][k] 
           for k in range(0,4)])
        
        g = interp1d(price,globals()['a'+str(i)],'quadratic')
        f = interp1d(price,globals()['b'+str(i)],'quadratic')
        
        
        plt.plot(pricenew,f(pricenew)*pricenew+g(pricenew)*float(myprod[-1]))
    
    plt.show()"""
        
  

for i in range(1,m+1):
    
    
    globals()['a'+str(i)] = np.array([np.median(
        [globals()['COUNT'+str(n+1)+'prod'+str(i)][l][k] 
          for l in range(0,200)]) for k in range(0,4)])
    
    globals()['b'+str(i)]  = np.array([np.median(
        [globals()['COUNT'+str(n+2)+'prod'+str(i)][l][k] 
          for l in range(0,200)]) for k in range(0,4)])
    
    g = interp1d(price,globals()['a'+str(i)],'quadratic')
    f = interp1d(price,globals()['b'+str(i)],'quadratic')
    
    plt.plot(price,globals()['a'+str(i)],'o',pricenew,g(pricenew),'-')
    plt.plot(price,globals()['b'+str(i)],'o',pricenew,f(pricenew),'-')
    plt.legend(['My Product','My product','New Product'+str(i),'New Product'+str(i)])
    plt.title('Elasticity Curve')
    plt.xlabel('Price (€)')
    plt.ylabel('Probability of purchase')
    plt.show()
    
    plt.plot(pricenew,f(pricenew)*pricenew+g(pricenew)*float(myprod[-1]))
    plt.title('Revenue vs Price for New Product'+str(i))
    plt.axvline(x = globals()['OptMedPrice'+str(i)], color = 'r')
    plt.xlabel('Price (€)')
    plt.ylabel('Revenue')
    plt.show()
    
    print('The price to maximize revenue for New Product'+str(i)+' is: '+
          str(np.round(globals()['OptMedPrice'+str(i)],2)))
    print('The total maximized revenue for New Product'+str(i)+' is: '+
          str(np.round(globals()['MaxMedRevenue'+str(i)],2)))
    


# Lastly for these previously obtained prices we are going to compute the
# share of preferences to see how it has varied along the segments:

for i in range(1,m+1):
    
    globals()['utilmynewprod'+str(i)] = sum(
            [estbetas[j] for j in globals()['mynewprod'+str(i)][:-1]]
            )+pd.Series(
            [globals()['PU'+str(l)](float(globals()['OptMedPrice'+str(i)]))
                                         for l in range(0,200)])


for i in range(1,m+1):
    
    globals()['totalCOUNTER'+str(i)] = [];
    
    for k in range(0,C):
        
        globals()['COUNTER_cluster'+str(k)+'mynewprod'+str(i)] = [];
    
    for it in range(0,iterations):
        
        for k in range(0,C):
            
            globals()['counter_cluster'+str(k)+'mynewprod'+str(i)] = [];
            
        results = [];
        totalcounter = []
        
        for l in range(0,200):
            
            results.append(random.choices([pp for pp in range(1,n+3)],
                   weights=tuple([np.exp(globals()['util'+str(jj)][l]) 
                   for jj in range(1,n+1)]+[np.exp(utilmyprod[l])]+
                                [np.exp(globals()['utilmynewprod'+str(i)][l])]
                                ),k=1))
        
        for kk in range(1,n+3):
            
            totalcounter.append(results.count([kk])/2)
            
            for j in range(0,C):
                
                globals()['counter_cluster'+str(j)+'mynewprod'+str(i)].append(
            [results[pp] for pp in np.where(yhat==j)[0]].count([kk])*100/sum(yhat==j))
                
        for p in range(0,C):
            
            globals()['COUNTER_cluster'+str(p)+'mynewprod'+str(i)].append(
                globals()['counter_cluster'+str(p)+'mynewprod'+str(i)])
        
        globals()['totalCOUNTER'+str(i)].append(totalcounter)





# Averaging the previous results:
for i in range(1,m+1):
    
    for j in range(0,C):
        
        globals()['Median_cluster'+str(j)+'mynewprod'+str(i)]=[
            np.median([globals()['COUNTER_cluster'+str(j)+'mynewprod'+str(i)][jj][k]
                    for jj in range(0,iterations)]) for k in range(0,n+2)]
        
        
    globals()['Median_total_mynewprod'+str(i)] = [
        np.median([globals()['totalCOUNTER'+str(i)][jj][kk] for jj 
                   in range(0,iterations)]) for kk in range(0,n+2)]
        
    
    # Let's store the results into a data frame, we have:
    
    globals()['df'+str(i)] = pd.DataFrame(
        [globals()['Median_cluster'+str(kk)+'mynewprod'+str(i)]
         for kk in range(0,C)]+[globals()['Median_total_mynewprod'+str(i)]]
        ).T
    
    globals()['df'+str(i)].columns = [
        'Cluster'+str(i)+','+'n='+str(sum(yhat == i)) 
              for i in range(0,C)]+['Total Share']
    
    globals()['df'+str(i)].index = [
        'Competitor'+str(i) for i in range(1,n+1)]+[
            'Myproduct']+['Mynewproduct'+str(i)]

    print('The allocation of the different clusters for'+
          ' the new product '+str(i)+' is: ')      
    print(globals()['df'+str(i)])
    
    
    # Plotting them to have better visual insights:
    globals()['df'+str(i)].T.plot(kind='bar', stacked=True)
    plt.show()




