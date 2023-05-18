import pandas as pd
import numpy as np
from random import seed
from random import choice
from random import uniform


""" The aim of this script is to build an Adaptive Questionnaire
    and to store respondent's answers to analyse them later
    using the so-called Conjoint Analysis. Let's change this."""



# We first include the Behavioral Questions.
# These questions might be used either to:
# - Make respondents recall their previous purchasing behaviour
#   and obtain more accurate answers.
# - Including them as covariates to enhance the utility estimation
#   and the customer clustering task.

print("We would like to learn about you and your general thoughts, \n"
      +"feelings and opinions when it comes to ebooks. \n"+
      "Please choose the alternative that better describes you. \n"
      +"Introduce 0 if you identify yourself with the upper statement. \n"
      +"Introduce 1 if you identify yourself with the upper statement.")

# This is an example of possible 9 questions:
# Brand related questions:
x1 = int(input("I think that brands differ a lot.\n"+
               "I think that all brands are more or less the same. "))

x2 = int(input("I always know exactly what brand I am going to buy"+
               "before I enter the shop.\n"+
               "I decide what brand I am going to buy when"+
               "I am stading in front of the self. "))

x3 = int(input("I always buy the brand I bought last time. \n"+
               "I switch between different brands. "))

# Price relates questions:
x4 = int(input("I compare prices vey carefully before make a choice. \n"+
               "To be honest, I compare prices only superficially. "))

x5 = int(input("I always search for special offers first. \n"+
               "Special offers are not the first thing I look out for. "))

x6 = int(input("I always know the prices of product I buy. \n"+
               "I never really know what products cost. "))


# Innovation related questions:
x7 = int(input("I am always interested in new products. \n"+
               "I prefer to stick to what I know. "))

x8 = int(input("I think that products in this category need to be improved. \n"+
               "I am completely satisfied with the products as they are. "))

x9 = int(input("I find it easy to make the right choice for me. \n"+
               "I it very difficult to make the right choice for me. "))

for i in range(1,10):
    if (globals()['x'+str(i)] != 0 and globals()['x'+str(i)] != 1):
        print('Your answer must be 0 or 1')
        quit

# Once the behavioral questions have been answered we proceed with 
# actual questionnaire.
# We first try to get a dictionary with all the Attributes
# and their different Levels. We also get a vector with the number
# of levels on each attribute.

Attrib = [str(Attrib) for Attrib in input(
    "Introduce the Attribute names used in the Questionnaire: ").split()]
Conjoint = {}
Numbers = []


for i in Attrib:
    Conjoint[i] = [str(m) for m in 
                   input("Intoduce the Levels of "+i+" Attribute: ").split()]
    Numbers.append(len(Conjoint[i]))


# Specify the different prices for each of the levels. 
# This is part of the conditional summed price strategy.
levelprices = np.asarray(input('Introduce level prices in the proper order: ')
                         .split(),dtype=np.float64)


if len(levelprices) != sum(Numbers):
    print('The number of level prices does not match with the number of levels: ')
    quit


# Specify the price basis.
pricebasis = int(input('Introduce the Price Basis for the product: '))


# Specify the possible values of the price that we will show. Remind
# that we should account for the whole range of values
m = int(input("Introduce the Number (recommended 5) of Price Levels: "))
Indexes = [0]+list(np.cumsum(Numbers))
minprice = (sum([min(levelprices[Indexes[i]:Indexes[i+1]])
                 for i in range(0,len(Indexes[:-1]))]) + pricebasis)*0.8
maxprice = (sum([max(levelprices[Indexes[i]:Indexes[i+1]])
                 for i in range(0,len(Indexes[:-1]))]) + pricebasis)*1.2
Prices = list(np.round(np.linspace(minprice,maxprice,m)))
Attrib = Attrib + ['Price']
Conjoint['Price'] = Prices


# We then try to get all the Levels in a list
Levels = [item for sublist in list(Conjoint.values()) for item in sublist]
Levels = Levels  +['None']


""" Once we have the setup we proceed to build the questionnaire"""

# In order to get reproducibility get set the seed to 1.
seed(1)
n = int(input('Introduce Number of Questions in the Questionnaire: '))
m = int(input('Introduce Number of Alternatives per choice set: '))


# Empty data frame where we will store all our resutls:
columns = ['Id','Question','Alternative'] + Levels + ['chosen']
df = pd.DataFrame()
counter = list(np.zeros(len(Levels)))

# Questions:
for f in range(1,n+1):
    
    print('Question Number '+str(f))

    
    # Different procedure, we use a for loop, we store the results
    # and we pick the option with the lowest utility range.
    
    for j in range(1,m):
        
        globals()['l'+str(j)] = [];
        
    utilities = []; diff = [];
    
    
    for it in range(0,200):
    
        for jj in range(1,10):
            
            globals()['q'+str(jj)] = [];
            globals()['util'+str(jj)] = 0;
            
        
        for i in range(0,len(Attrib)-1):
            
            # Now we have to choose how to sample depending on the 
            # number of Levels:
            
            if Numbers[i] >= (m-1):
                
                for k in range(1,m):
                    
                    globals()['a'+str(k)] = choice([j for j in 
                        range(Indexes[i]+1,Indexes[i+1]+1) if j not in 
                        [globals()['a'+str(k)] for k in range(1,k)]])
                    
                    globals()['util'+str(k)] = globals()['util'+str(k)] + counter[globals()['a'+str(k)]-1]
                                       
                    globals()['q'+str(k)].append(globals()['a'+str(k)])
            
            
            if Numbers[i] < (m-1):
                
                p = (m-1)//Numbers[i] + 1
                
                for k in range(1,p+1):
                    
                    for c in range(1,Numbers[i]+1):
                        
                        globals()['a'+str((k-1)*Numbers[i]+c)] = choice([j for j in 
                        range(Indexes[i]+1,Indexes[i+1]+1) if j not in 
                        [globals()['a'+str((k-1)*Numbers[i]+r)] for r in range(1,c)]])
                        
                        
                        globals()['util'+str((k-1)*Numbers[i]+c)] = globals()['util'+str((k-1)*Numbers[i]+c)] + counter[globals()['a'+str((k-1)*Numbers[i]+c)]-1]
                        
                        globals()['q'+str((k-1)*Numbers[i]+c)].append(globals()['a'+str((k-1)*Numbers[i]+c)])
                        
            
        
        for jj in range(1,m):
            
            globals()['p'+str(jj)] = (sum([levelprices[i-1] for i in globals()['q'+str(jj)]])+pricebasis)*uniform(0.75,1.25)
            globals()['p'+str(jj)] = min(Prices, key=lambda x:abs(x-globals()['p'+str(jj)]))
            
        for jj in range(1,m):
            
            globals()['q'+str(jj)].append(Levels.index(globals()['p'+str(jj)])+1)
            
            globals()['util'+str(jj)] = globals()['util'+str(jj)] + counter[Levels.index(globals()['p'+str(jj)])]
            
            globals()['l'+str(jj)].append(globals()['q'+str(jj)])
        
        
        utilities.append([globals()['util'+str(kk)] for kk in range(1,m)])
        diff.append(np.ptp([globals()['util'+str(kk)] for kk in range(1,m)]))
             
    # We want to find the most equilibrated question, that correspond to:
    b = diff.index(min(diff))
    
    for kkk in range(1,m):
        
        globals()['q'+str(kkk)] = globals()['l'+str(kkk)][b]
    
    
    # The last alternative is always going to be the None option:
    globals()['q'+str(m)] = [len(Levels)]
    
    for kkk in range(1,m):
        
        print('The accumulated utility for option '+str(kkk)+' is:'+str(utilities[b][kkk-1]))
        
    # Showing the different options:
    print("Which option do you prefer: ")
    
    for kkk in range(1,m):
        
        print("Option "+str(kkk)+": "+", ".join(str(Attrib[a])+" "+str(Levels[b-1]) for a,b in enumerate(globals()['q'+str(kkk)])))
    
    print('Option '+str(m)+': '+ str(Levels[globals()['q'+str(m)][0]-1]))
    
    
    # Choosing the favourite alternative:
    h = str(input('Choose one of the previous options: '))
    globals()['q'+h].append(len(Levels)+1)
    
    # Putting the values in the correct format for posterior storage
    for kkk in range(1,m+1):
        globals()['w'+str(kkk)] = [];
    
    for i in range(1,len(Levels)+2):
        
        for kkk in range(1,m+1):
            
            globals()['w'+str(kkk)].append(1 if i in  globals()['q'+str(kkk)] else 0)
    
    
    # Increase the pricelevels of the levels that were selected 
    # and decrease it in case the none option has been chosen
    if int(h) == m:
        
        levelprices = 0.9 * levelprices
        
    else:
        
        for i in globals()['q'+h][:-2]:
            
            levelprices[int(i)-1] = levelprices[int(i)-1] * 1.1


    # Set the counter to the prefered alternative
    # with the aim of getting utility balance.
    counter = [x+y for x,y in zip(counter,globals()['w'+h][:-1])]
    
    for kkk in range(1,m+1):
        
        globals()['w'+str(kkk)] = [1,f,kkk] + globals()['w'+str(kkk)]


    
    # Store all the information gathered during the question
    df = df.append(pd.DataFrame([globals()['w'+str(kkk)] for kkk in range(1,m+1)]))
    
df.columns = columns
