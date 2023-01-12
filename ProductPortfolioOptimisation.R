##############################
# Code to do Genetic Algorithm portfolio evaluation with CBC/ACBC data
#
# Author: Christopher N. Chapman
#         cchapman@google.com
#
# IMPORTANT:
#         This code is for RESEARCH PURPOSES only and has no warranty of fitness for any purpose.
#  ===>   Author's note: the code almost certainly contains both large and small ERRORS. Evaluate it thoroughly for your own purposes.
#         In fact, you probably should write your own version, using this purely for illustrative purposes :-)
#
#         Be SURE to read EVERY line of code here and update for your needs. 
#         This file MUST must be updated for your problem and executed in chunks; it cannot simply be "source"d
#
# Last update: June 11, 2013
# Version: 0.13 -- DEV VERSION IN PROGRESS
#                  *** Internal usage only
#
# Version: 0.12 -- Research code shared with market research community
#               -- Work in progress; updated in an ongoing fashion with limited version-to-version testing
#
# Copyright (c) 2013, Google, Inc.
#
# LICENSE
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# LICENSE SUMMARY (EXCERPT FROM APACHE LICENSE 2.0)
# Contributor hereby grants to You a perpetual, worldwide, non-exclusive, 
# no-charge, royalty-free, irrevocable copyright license to reproduce, prepare 
# Derivative Works of, publicly display, publicly perform, sublicense, and 
# distribute the Work and such Derivative Works in Source or Object form.
#
#
# CITATION:
# Chapman, C.N. (2010). Genetic algorithms for choice model portfolio analysis. [Computer software, version 0.12, July 2011]
# Chapman, C.N., and Alford, J.L. (2010). Product portfolio evaluation using choice modeling and genetic algorithms. Paper presented 
#     at the 2010 Sawtooth Software Conference, Newport Beach, CA. October 2010.
#
#
# UPDATE HISTORY
# 0.12  Speed enhancements to prod.select.product(), about 25% faster.
#       Added error estimation options for both attribute and product level error
# 0.11  Updated prod.select.product() error estimation procedure
# 0.10  Fixed design bug when using HB draws; did not use *same* draw for every respondent.  Now uses same draw (e.g., "draw 353") for everyone per iteration.
#       Massive speedup: Modified preference share function to use rowSums instead of "apply(...,1,sum)".  Speedup of 7x on this core function!
# 0.09  First public version, shared after Sawtooth Software Conference 2010
#
#
# TO DO
# 1. Better commenting and inline documentation!!
# 2. Packaging as a standard R package without namespace pollution
#

#####################
# CHECKLIST for using this procedure
#
# 0. Read through the entire code top to bottom, and try it using the fake data and toy examples given along the way
#
# 1. Import part worths and make sure they're all OK in R
# 2. Define your feature, price, and friendly name lists, and test them all
# 3. Modify the "price" function to handle any interactions
# 4. Modify the fitness functions to handle your pricing and any feature interactions
# 5. Set up your attribute/feature ==> part worth column maps
# 6. tweak the master "bootstrap" function as needed
# 7. TEST TEST TEST
# 8. Read through **each line of the code** one more time and make sure you've updated everything!
# 9. Run it!


#####################
## Load the standard GA library we're using
## library(ref)             ## so we can work with very large matrix of HB draws, uncomment and use this if needed
##                              in which case, simply define the part worth data frame as a big memory frame when imported; everything else should work unchanged

library(rgenoud)            ## the GA package.  install this from CRAN, and check out its options as noted in the GA calling function below



###### STEP ONE -- LOAD THE PART WORTHS
# 
# We could do something like this if we had real data available:
#     usc1.product.pws <- read.csv("usc1_utilities.csv")
#
# but instead let's generate them randomly for demo purposes:




## ---------- START OF FAKE DATA CREATION ------------------------
## ---------- not needed except for demonstration  ---------------

#############################################
# generateRNDpws -- from Rcbc package (Chapman, 2010)
# creates a zero-sum vector of partworths given a defined vector of attribute sizes
#############################################
generateRNDpws <- function(attrs)
{
  pw.vec <- NULL
  for (i in 1:length(attrs))
  {
    if (attrs[i] > 1) {                 # more than 1 feature so OK to generate
      pw.car <- rnorm(attrs[i]-1,0,1)
      pw.cdr <- -1*sum(pw.car)
      pw.vec <- c(pw.vec,pw.car,pw.cdr)
    } else if (attrs[1] < 1) {          # attribute with <1 level
      warning("Attribute level is missing (# features < 1).")
    } else {                            # exactly 1 level so PW must be 0
      pw.vec <- c(pw.vec,0)
    }
  }
  return(pw.vec)
}

# make sure we're starting fresh
usc1.product.pws <- NULL

# this is a fake product, but let's assume that
# we have 5 attributes with 3-5 levels each, as follow:
#   Attr 1: Size:         Small, Med, Large
#   Attr 2: Performance:  Low, Med, High
#   Attr 3: Design:       Sleek, Tiny, Uber, MaxMax, Minimal
#   Attr 4: Memory:       1M, 2M, 4M, 8M, 16M
#   Attr 5: Price:        19, 29, 39, 49

# define the attribute structure
#
usc1.attrs <- c(3,3,5,5,4)      #  1 integer in the vector to represent the # of levels for each attribute.  Not needed if you have real data -- only for creating the fake data here.

# generate fake PWS for 200 "respondents", as might be estimated by an HB procedure
#
for (i in 1:200)     # number of rows of fake PW data
{
  if ((i / 100) == (i %/% 100)) { cat (i,"\n") }    # show some progress
  usc1.product.pws <- rbind(usc1.product.pws,generateRNDpws(usc1.attrs))
}
usc1.product.pws <- as.data.frame(usc1.product.pws)

# add a fake "NONE" part worth at the end with values from [-2, 10]
usc1.product.pws <- cbind(usc1.product.pws,runif(nrow(usc1.product.pws), min=-2, max=10))

## ---------- END OF FAKE DATA CREATION ---------------
## ----------------------------------------------------



## ---------- use your data instead of the above -------------------
# for example:
# we could do something like this if we had real data available:
#     usc1.product.pws <- read.csv("usc1_utilities.csv")
## -----------------------------------------------------------------


# add any columns needed for prohibited pairs (e.g., so there is a dummy column to count on the prohibited side with PWs all ==0)
# for instance, suppose
#   col 3 = wireless
#   col 4 = wired
#   col 10 = battery AA
#
# if a product appears that is [4,10] == wired + battery, that is a prohibited pair.  
# Instead of rejecting that product definition entirely, we might change one of the attributes to make it more reasonable, for instance to remove the "battery" level
#     pseudo code:
#         if (..attr4.. && ..attr10..) then { attr10 --> attr51 }   # where attr51 = all 0 part worths
#     that would correctly include only the utility for attr4
# to let that happen, we need to (A) add a column of all 0 part worths; (B) wherever a prohibition is encoutered, update it to point to that column as needed
#
# example code: add an all-0 column between columns 17 and 18 in an existing part worth array:
#    tmp.pws <- cbind(usc1.product.pws[,1:17],rep(0,nrow(usc1.product.pws)),usc1.product.pws[,18:37])
#    usc1.product.pws <- tmp.pws
#
# WARNING: changing prohibited pairs in this way, especially if there are many of them, might result in search bias.
#       some alternative strategies to combat that would be:
#       1. reject all such prohibitions ... will make the search much less efficient
#       2. adjust them as noted above, but then set a flag and "tune down" their overall fitness
#       3. track generations and accept such things earlier in a GA run but reject them later
#       4. modify the levels to avoid prohibition (as above) but do so in a balanced way (e.g., sometimes 4-->51 and sometimes 10-->51 in the example above)
#       5. if you know a priori that one attribute is much more important than another, always change the less-important attribute 
#          (which won't eliminate the problem but should lessen its significance)
# Each of those strategies could be implemented in the fitness function in the places noted inline
# In any case, you must do something to deal with prohibited pairs. The "gold standard" would probably be to reject them (e.g., fitness <- 0) unless that
#    makes the search entirely unfeasible.  Second best thing to do would be to accept them (i.e., to make the level alterations to 0 part worth as suggested)
#    but scale down the fitness (e.g., if (prohib pair) fitness <- fitness * 0.75). This suggestion would let the search proceed but pull for solutions to avoid
#    the prohibition naturally.  Pairing that with balanced level reassignment seems like an emininently defensible option.


# name the columns so we can interpret them later
# these are purely for convenience and have no functional importance
#
names(usc1.product.pws) <- c(   "Size-Small", "Size-Med", "Size-Large",
                                "Perf-Low",   "Perf-Med", "Perf-High",
                                "Des-Sleek",  "Des-Tiny", "Des-Uber",  "Des-Max", "Des-Min",
                                "Mem-1",      "Mem-2",    "Mem-4",     "Mem-8",   "Mem-16",
                                "Price19",    "Price29",  "Price39",   "Price49",
                                "NONE"
)

head(usc1.product.pws)   # make sure the PW data all looks OK !


###### STEP TWO -- SET UP PRICING                     

# create increment price point partworths if needed for your product space
#
# In this example, the CBC partworths are at $10 increments, but we need $5 increments to match the actual product space
# so we can create price points at $5 increments with interpolation
#
# NOTE that interpolating price points can change the error structure (see K. Karty, ART Forum 2011, for details)
# and this may make the estimates of interpolated points appear oddly more or less desirable.
# Interpolation is therefore not recommended if you need good price estimates, but is included here as an example.
# If you use interpolation, you should run test cases with prod.select.product() to ensure that the share estimates look suitable.
#
price.19 <- usc1.product.pws[,17]
price.29 <- usc1.product.pws[,18]
price.39 <- usc1.product.pws[,19]
price.49 <- usc1.product.pws[,20]
price.24 <- 0.5*price.19 + 0.5*price.29
price.34 <- 0.5*price.29 + 0.5*price.39
price.44 <- 0.5*price.39 + 0.5*price.49

# now add all of those prices to the partworth file in order
# prices must align in equal increments because the "preference" function will pick the part worth column based on the price without a map
# if you need unequal price increments, you'll need to change the preference function to handle the (price-->column) map
#
tmp.prices <- cbind(price.19, price.24, price.29, price.34, price.39, price.44, price.49)
usc1.product.pws <- cbind(usc1.product.pws[,1:16],tmp.prices,usc1.product.pws[,21])
names(usc1.product.pws)[24] <- "NONE"     # reset the name

##
head(usc1.product.pws)   # check it all again!!  You cannot check too much.


# define the realistic pricing for each attribute/feature level
# this is required or else it will simply find the "maximal feature set" at the minimal price
#
# this list is constructed as a "list of lists" where each sub-list represents 1 attribute, and has entries for each feature level
# no entry is needed for the price attribute

product.price.list <- list(
  list(0,3,5),            # Attribute 1 with 3 levels
  list(0,3,10),           # Attr 2 with 3 levels
  list(2,5,15,3,0),       # Attr 3 with 5 levels
  list(0,1,2,5,15)        # Attr 4 with 5 levels
)

### price offsets
# allows indexing from column definitions into the corresponding price in the product.price.list above
# == column start of attribute - 1
# for instance, if Attribute 1 starts in Column 3 and Attr 2 starts in Column 7, then their "offsets" are 2 and 6, respectively
#
product.price.offset <- c(0,3,6,11)



###### STEP THREE -- SET UP FRIENDLY NAMES ... AND DO SOME MORE CHECKING 
# set up "friendly names" for feature levels to be used in the output
# one list for each attribute, with entries for each level
#
# these do NOT need to match the column names above -- they're just copied here for convenience
#
product.att.list  <- list(
  list("Size-Small", "Size-Med", "Size-Large"),               # Columns in part worths:  1,2,3
  list("Perf-Low",   "Perf-Med", "Perf-High"),                # 4,5,6
  list("Des-Sleek",  "Des-Tiny", "Des-Uber",  "Des-Max", "Des-Min"),     # 7,8,9,10,11
  list("Mem-1",      "Mem-2",    "Mem-4",     "Mem-8",   "Mem-16"),      # 12,13,14,15,16
  list("$19","$24","$29","$34","$39","$44","$49")             # 17,18,19,20,21,22,23
)

#
# tests
# products are defined as vectors specifying 1 column position for each attribute
product1 <- c(1, 4, 7, 12)   # Small + LowPerf  + Sleek + Mem-1
product2 <- c(2, 6, 9, 14)   # Med   + HighPerf + Uber  + Mem-4


### function to compute the price of a product, given the definitions above
### This function requires CUSTOMIZATION to handle interactions of attributes with regards to price
###
list.price <- function(att.list, price.list, att.offset)
{
  tmp.price <- 0
  for (i in 1:length(att.list))
  {
    tmp.price <- tmp.price + price.list[[i]][[att.list[[i]]-att.offset[i]]]
  }
  ### here is where you can correct prices according to multi-level interaction with price if necessary ...
  #     if (att.list[[3]]==10)    # if attr 3 == column 10, then fix the price accordingly ...
  #    {
  #        tmp.price <- tmp.price - price.list[[5]][[att.list[[5]]-att.offset[5]]]
  #    }
  return(tmp.price)
}
# test to make sure the pricing looks reasonable!
list.price(product1, product.price.list, product.price.offset)
list.price(product2, product.price.list, product.price.offset)


### function to return a "friendly name" of a product, given its attribute list
### should not need modification in most cases, unless you want to change names in case of a feature interaction
###
product.translate <- function(att.list,text.list,att.offset)
{
  product.text <- ""
  product.baseprice <- 19    ### MUST BE SET MANUALLY -- THIS IS ADDED TO THE SUMMED PRICE FROM THE list.price() FUNCTION
  for (i in 1:length(att.list))
  {
    if (i==1) {
      product.text <- text.list[[i]][[att.list[[i]]-att.offset[i]]]
    } else {
      product.text <- paste(product.text,text.list[[i]][[att.list[[i]]-att.offset[i]]],sep=",")
    }
  }
  product.text <- paste(product.text,list.price(att.list,product.price.list,att.offset)+product.baseprice,sep=",")  
  return(product.text)
}

# test the friendly name function
product.translate(product1,product.att.list,product.price.offset)
product.translate(product2,product.att.list,product.price.offset)



###### STEP FOUR -- SET UP KEY FUNCTIONS

### This one computes prefence among products, given a list of products.
### See "Rcbc" package for complete details and/or updates
###
### This function does *not* require modification except as updated in Rcbc
###

############################
# prod.select.product(data, prod.definitions, none.col, use.none)
#
# find the preference for each product vs. others in a list of defined products
# optionally including the "none" product
#
# inputs
#     pw.data = matrix of part-worth utility DRAWS by respondent
#               !! defined as a "bigmemory" matrix object  -- but should work OK passed as bigmemory() or matrix()
#               
#     prod.defs = list defining each product as a vector of column numbers
#     none.col = the column that holds the "none" part-worth (if applicable)
#     use.none = whether to compare preference to "none"
#              note that there are many caveats for using and interpreting "none"
#              for many portfolio problems, having a fixed list of competitors is a better idea (possibly in addition to "none")
#     tuning = multiplier for partworths to handle scale factor
#     draws = number of draws to make from HB draw matrix
#     use.error = whether to include Gumbel error in calculations (FALSE by default)
#               NOTE that this includes EV error added for both ATTRIBUTE level (added to betas) and PRODUCT level (added to product sum)
#               unless you change the default of the following switches
#               NOTE also: using error is more correct & robust, but MUCH slower, approximately 8x the time requirement in ad hoc testing
#               if you want exploratory results, or just piloting, suggest to do most bootstraps without error, and a smaller number for comparison
#     use.attr.error = whether to add attribute-level EV error (i.e., to betas before product summation) (ON by default but only if use.error = TRUE)
#     use.prod.error = whether to add product-level EV error (i.e., EV error added per product after betas are summed) (ON by default but only if use.error = TRUE)
#     style = "logit" (MNL logit model share of preference)
#             "first" (strict first choice -- suggest using this for IIA resistance)
#             "roulette" (draws a preference from roulette slice with p = logit share; should converge to logit; experimental only!)
#
# Example call:
#     For 2 products defined as:
#         Product 1 = columns 1,4,8
#         Product 2 = columns 1,5,9
#     and including the none part worth in column 11
# prod.select(my.partworth.data,list(c(1,4,8),c(1,5,9)),11,TRUE)
####
#
#                                                   change   vv  -- or actually, make sure to pass through the correct parameter here
prod.select.product <- function(pw.data, prod.defs, none.col=NA, use.none=FALSE, tuning=1.0, draws=1, n.resp=nrow(pw.data), draws.each=1, 
                                use.error=FALSE, use.attr.error=TRUE, use.prod.error=TRUE, style="logit")
{
  total.matrix <- NULL
  for (i in 1:draws) {
    
    # take a sample from respondent draws in pw.data
    draw.sample <- rep(sample(1:draws.each,1),n.resp)      # choose an HB draw to use, and make a vector of that to pull sample for each respondent
    row.sample  <- draw.sample + ((1:n.resp)-1) * draws.each     # which rows within the dataframe to sample (indexing to each respondent and the pulled draw)
    pws.use <- pw.data[row.sample,]                      # sample those from the pw.data
    
    all.sum <- NULL
    all.prematrix <- matrix(NA,nrow=nrow(pws.use),ncol=(length(prod.defs)+ifelse(use.none,1,0)))    # pre-define matrix to hold results
    all.matrix <- NULL
    
    # add attribute-level error to part worths (to be used across all products) 
    if (use.error && use.attr.error) {                                        # add attribute-level product error for every Beta estimate in the matrix
      error.mat <- -log(-log(matrix(runif(nrow(pws.use)*ncol(pws.use)),nrow=nrow(pws.use),ncol=ncol(pws.use))))
      pws.use <- pws.use + error.mat
    }
    
    # iterate over the list of products defined and save the sum of each one's part worths (beta)
    ii <- 0
    for (prod.def in prod.defs) {
      ii <- ii + 1
      product.sum <- rowSums(pws.use[,prod.def]*tuning)      
      all.prematrix[,ii] <- product.sum
    }
    
    # optionally add the value of the "none" choice
    if (use.none) {
      all.prematrix[,ii+1] <- pws.use[,none.col]*tuning
    }
    
    # generate Gumbel extreme value error at the summed PRODUCT level, and add it
    # 
    if (use.error && use.prod.error) {
      # create matrix of EV error terms with same shape as product choice matrix
      error.mat <- -log(-log(matrix(runif(nrow(all.prematrix)*ncol(all.prematrix)),nrow=nrow(all.prematrix),ncol=ncol(all.prematrix))))
      # utility = exp(B + error)
      all.prematrix <- all.prematrix + error.mat    # now have utility by product
    }
    all.matrix <- exp(all.prematrix)                  # now have e^(utility[+error]) for every product
    
    # compute the total utility of all choices per respondent
    all.sum <- rowSums(all.matrix)
    # and return the shares
    if (is.null(total.matrix)) {
      total.matrix <- (all.matrix / all.sum)
    } else {
      if ( (dim(total.matrix)[1] != dim(all.matrix)[1]) |
           (dim(total.matrix)[2] != dim(all.matrix)[2]) |
           (dim(total.matrix)[1] != length(all.sum) ) )
      {
        print("Warning: dimensions don't match in prod.select.product()")
        print(dim(total.matrix))
        print(dim(all.matrix))
        print(length(all.sum))
      }
      total.matrix <- total.matrix + (all.matrix / all.sum)
    }
  }
  tmp.ret <- total.matrix/draws
  if (style=="logit") {
    ## nothing to do -- this is the default
    ## just return the matrix of logit utilities as estimated
  } else if (style == "first") {
    ## return the 'first choice' preference as 0 or 1 calculated from the logit shares
    tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
    tmp.pref[cbind(1:nrow(tmp.ret),max.col(tmp.ret))] <- 1       ## set most-preferred option to '1'. Ties are broken randomly in max.col()
    tmp.ret <- tmp.pref
  } else if (style == "roulette") {
    ## return single preferred option but draw it from all the products according to their relative likelihood
    ## should converge to logit model estimate, but included here for experimental purposes
    tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
    tmp.which <- apply(tmp.ret,1,function(x) { sample(1:length(x),1,prob=x) } )   ## draw list of columns sampled according to probabilities in each row
    tmp.pref[cbind(1:nrow(tmp.ret),tmp.which)] <- 1       ## set the drawn option to '1'
    tmp.ret <- tmp.pref
  } else {
    warning("Undefined 'style' parameter. Use 'logit','first', or 'roulette'. Returning 'logit' shares by default.")
  }
  return(tmp.ret)
}


#### test the "prod.select()" function to see if the results are reasonable
product1 <- c(1,4,7,12)   # Small + LowPerf  + Sleek + Mem-1
product2 <- c(2,6,9,14)   # Med   + HighPerf + Uber  + Mem-4

# logit share-of-preference for product 1 vs. product 2 vs. "none"
ps.tmp<-prod.select.product(usc1.product.pws,list(product1,product2),none.col=24,use.none=TRUE,tuning=1.0)
apply(ps.tmp,2,mean)  # logit-model share of preference for Product 1, Product 2, and "NONE"

# same but with Gumbel error and bootstrap 10x
ps.tmp<-prod.select.product(usc1.product.pws,list(product1,product2),none.col=24,use.none=TRUE,tuning=1.0,  use.error=TRUE, draws=10)
apply(ps.tmp,2,mean)  # logit-model share of preference for Product 1, Product 2, and "NONE"

# OR, if you prefer first choice preference for product 1 vs. product 2 vs. "none"
ps.tmp<-prod.select.product(usc1.product.pws,list(product1,product2),none.col=24,use.none=TRUE,tuning=1.0, style="first", use.error=TRUE)
apply(ps.tmp,2,sum)  # count of how many people prefer Product 1, Product 2, and "NONE"


####
#### So far we've just got basic MNL / CBC stuff done ... now it's time for the "real" GA part ...
####


#############################
### GA Fitness function -- called through wrapper function below
###
### this is the KEY function that you must define to fit your problem and data
### the system works as follows: 
###     the GA library calls the WRAPPER function to be able to send just the candidate vector 
###     and the wrapper function then calls THIS function to do the complete fitness calculation according to your parameters and prohibitions
###
### This function MUST be inspected for CUSTOMIZATION, especially where noted inline

#                                                       change               vv                     
product.port.fitness <- function(vec.in,prod.len,price.list,pw.data,none.col=44,share.cutoff=NA,tuning=1.0, HB.draws=FALSE, n.PS.draws=product.fitness.PS.draws, n.draws=product.fitness.N.draws)
{
  # cut list into separate products
  #
  vec.list <- NULL
  prod.num <- length(vec.in)/prod.len
  for (i in 1:prod.num)
  {
    vec.list.el <- list(vec.in[((i-1)*prod.len+1):(i*prod.len)])
    #### Optional: recode some of the features if necessary to make sense.
    #### For example: suppose you have the option of a battery-powered or plug-in device.  
    ####    A second attribute for "battery life" might need to be reset to a dummy column of all 0 PWs in case of a plug-in device
    #### 
    #### READ: the long note above about different strategies to handle and recode prohibited pairs
    #### 
    #        if(vec.list.el[[1]][3]==10) { vec.list.el[[1]][5] <- 18 }                        # if some feature, then set something else to match it if needed
    #        if(vec.list.el[[1]][3]!=10 && vec.list.el[[1]][5] == 18) { vec.list.el[[1]][5] <- 19 }    # " "
    vec.list <- c(vec.list,vec.list.el)
  }
  
  # now determine the price for each product
  for (i in 1:prod.num)
  {
    ### find appropriate price column in PW data
    ### change all of these to match your space ...
    price.offset <- 17    ######## HARD-CODED must match start of price columns in PW data
    price.start <- 19     ### lowest possible price
    price.end   <- 49     ### highest possible price
    price.inc   <- 5      ### price increments between that range, in $  -- will be rounded off
    
    prod.price.raw <- price.start+list.price(vec.list[[i]], price.list, product.price.offset)
    
    if (prod.price.raw < price.start) { prod.price.raw <- price.start }
    if (prod.price.raw > price.end)   { prod.price.raw <- price.end }
    
    ##  change if necessary for your product price rounding ...
    ##                                     VVVVVVVVVVVVVVVVVVVVVVVVVV        
    prod.price.col <- price.offset+((round(prod.price.raw*2+1,-1)/2-1)-price.start)/price.inc   # round to nearest $5 increment and then to $..4 or $..9
    vec.list[[i]] <- c(vec.list[[i]],prod.price.col)    # add the price PW column to the product definition
  }
  
  ## sample what respondents would most likely purchase, numerous (==n.draws) times 
  ps.buy.all <- NULL
  for (i in 1:n.draws)
  {
    # figure prod.select share for each product
    # uses default "logit rule" share of preference.  add ",style="first" " if desired instead of logit-style estimation
    #
    if (HB.draws) {
      #                                            CUSTOMIZATION                                            vvvvvvvvv         vvv            vvvv  match your HB data!!
      prod.sel <- prod.select.product(pw.data,vec.list,none.col=none.col, use.none=TRUE,tuning=tuning,draws=n.PS.draws,n.resp=400,draws.each=1000,use.error=TRUE)
    } else {
      prod.sel <- prod.select.product(pw.data,vec.list,none.col=none.col, use.none=TRUE,tuning=tuning,draws=1,draws.each=1,use.error=FALSE)
    }
    
    #        ## fix selection for invalid attribute combinations
    #        ## set choice to "0" if they couldn't actually pick something listed ...
    #        for (i in 1:prod.num)
    #        {
    #            vec.tmp <- vec.list[[i]]
    #            if ((18 %in% vec.tmp) && (9 %in% vec.tmp || 10 %in% vec.tmp || 11 %in% vec.tmp))   # e.g., invalid photo resolution 8.0MP photos with < 2.0MP sensor
    #            {
    #                prod.sel[,i] <- rep(0,nrow(prod.sel))
    #            } else if ((17 %in% vec.tmp) && (9 %in% vec.tmp)) {                                # 
    #                prod.sel[,i] <- rep(0,nrow(prod.sel))
    #            }
    #        }
    #                
    
    # determine buy/no-buy for each respondent/product
    ps.which <- max.col(prod.sel)    # the column they are most likely to select
    ps.which <- factor(ps.which,levels=1:(prod.num+1),ordered=TRUE)
    
    # get average buy/no-buy for each product across respondents
    # sum that and return as fitness
    if (is.na(share.cutoff))        # use all products in share calculation
    {
      ps.buy.line   <- length(ps.which[ps.which < (prod.num+1)])/length(ps.which)   # the proportion who select something other than "none"
    } else {
      # use only those products excluding last X that are cutoff (final N competitive products)
      ps.buy.line   <- length(ps.which[ps.which < (prod.num-share.cutoff+1)])/length(ps.which)   # the proportion who select something other than "none"
    }
    ps.buy.all <- c(ps.buy.all,ps.buy.line)
  }
  ps.buy <- mean(ps.buy.all)
  
  ### if new highest fitness, print the solution and save it to environment
  if (ps.buy > product.port.fitness.bestvalue) {
    product.port.fitness.bestvalue <<- ps.buy
    print(paste("GA iteration:",GA.iteration))
    print(ps.buy)
    print(vec.in)
    print(table(ps.which))
    print(table(ps.which)/length(ps.which))
    for (i in 1:prod.num) {
      print(paste("product:",i))
      print(product.translate(vec.list[[i]][1:prod.len],product.att.list,product.price.offset))
    }
  }    
  return(ps.buy)        
}



### GA FITNESS FUNCTION Wrapper 
### Called by the GA routine to estimate fitness on each population member
###
### CUSTOMIZATION: If you want to evolve in the face of a fixed set of competitors, add them in here
###     otherwise, it will compete against "none" (and you MUST include a none part worth)
###     also, update the specific fitness function call to match your data, i.e., the number of attributes in your product part worths
### 

GArep.product.fit.wrapper <- function(vec.in)
{
  ## add the fixed vectors to those of interest
  ##
  #  vec.test <- c(vec.in, product.fix.vec)     ### IF COMPETITION
  ## ELSE
  vec.test <- vec.in   ### just use the ones we're evolving without a fixed set of competitors
  
  ## call the fitness function
  ## CUSTOMIZATION                                       vv                                        vvvvvvvvvvvvvvvv                           vv
  product.fit <- product.port.fitness(vec.test, prod.len=4, price.list=product.price.list, pw.data=usc1.product.pws, HB.draws=FALSE, none.col=24)
  return(product.fit)
}



###### STEP FIVE -- RUN A SIMPLE ONE-PASS TEST

# GENOME definition
# Define the columns used by each attribute to make up "1 product"
# define minimum and maximum columns (==levels) for each attribute
#
# CUSTOMIZATION: Must match the length and structure of your Attributes/Features ==> part worth data columns
#
product.domains.mat1 <- matrix( data = c(
  1,3,        # IF YOU HAVE A "BRAND" ATTRIBUTE, BE SURE TO SET ITS LEVELS TO INCLUDE ONLY YOUR BRAND(s)!  You may not want to search and optimize across other brands :-)
  4,6,
  7,11,
  12,16
)
,nrow=4,ncol=2,byrow=TRUE)
#

### environmental variables needed -- be sure to set in environment and reset each run!
### 
###
product.port.fitness.bestvalue <- 0
GA.iteration <- 0
product.fitness.PS.draws <- 1
product.fitness.N.draws <- 1


### TEST 6 PRODUCT VERSION
# set full matrix of possible products/columns to test simultaneously
# need to repeat the "one product" matrix several times for the test
# note that the GA wrapper does this Automatically -- this is for test purposes only
product.domains.mat <- rbind( product.domains.mat1,  product.domains.mat1,  product.domains.mat1,  product.domains.mat1,  product.domains.mat1,
                              product.domains.mat1
)

# fast test
product.port.fitness.bestvalue <- 0
#                                                          vv == #products * #non-price attributes
product.test <- genoud(fn=GArep.product.fit.wrapper, nvars=24, max=TRUE, pop.size=40, max.generations=10, print.level=1, wait.generations=2,
                       Domains=product.domains.mat, data.type.int=TRUE, boundary.enforcement=2, 
                       P1=100, P2=100, P3=100, P4=100, P5=0, P6=100, P7=100, P8=100, P9=0 )

# long test
product.port.fitness.bestvalue <- 0
product.test <- genoud(fn=GArep.product.fit.wrapper, nvars=24, max=TRUE, pop.size=400, max.generations=200, print.level=1, wait.generations=20,
                       Domains=product.domains.mat, data.type.int=TRUE, boundary.enforcement=2, 
                       P1=100, P2=100, P3=100, P4=100, P5=0, P6=100, P7=100, P8=100, P9=0 )




###### STEP SIX -- SET UP FOR COMPLETE RUN

### notes on "rgenoud" operators: 
###     P5: cannot use P5 (polytope) because it violates allele boundaries
###     P9: P9 (BFGS) is not relevant for integer alleles
###

### environmental variables needed -- be sure to set in environment and set to correct values before a run
### 
###
product.port.fitness.bestvalue <- 0
GA.iteration <- 0
product.fitness.PS.draws <- 1
product.fitness.N.draws <- 1


### USE COMPETITIVE PRODUCTS -- example of some products from competitor that you might evolve *against*
###     change:
### be sure to include these in the GArep.product.fit.wrapper() function if so

# product.fix.vec <- c(
#                    c(1,4,8,12,15,18,20,22),        ## competitor products you want to include
#                    c(1,4,8,10,14,17,20,22),        
#                    c(2,4,8,10,14,17,20,22),        
#                    c(1,4,6,9,14,16,19,21)          
#                )
#


### MASTER FUNCTION TO RUN THE WHOLE THING ONE TIME
### and not just evolve but collect all the data we need
###
### CUSTOMIZATION: set parameters of the function as needed -- most of the default parameters are OK and are overwritten by the wrapper below that calls this function
### other updates for prohibitions, etc are inline
##
### "prod.atts" is the total count of the number of levels in your product,
###

#                                                           vv         vv           vv
product.GA.bootstrap <- function(fit.function=NULL,prod.num=10,prod.len=4,prod.atts=23,GA.result.reps=10,GA.sampleProp=0.6,GApop.size=400, 
                                 GAmax.gen=100, GA.wait=10,tuning=1.0, HB.draws=FALSE, GA.pws.totalN=nrow(usc1.product.pws), GA.pws.drawsEach=1)
{
  ## set up product definition
  domains.mat <- NULL
  ## extend the search domain to reflect the number of products being optimized
  for (i in 1:prod.num) { domains.mat <- rbind( domains.mat,  product.domains.mat1) }
  
  GA.result.mat <- NULL
  GA.product.mat <- NULL
  GA.result.twoway <- matrix(0,prod.atts,prod.atts)
  
  #### UPDATE THE FOLLOWING TO MATCH YOUR PRICES
  #### CUSTOMIZATION
  GA.price.mat <- rbind(  c( 0, 1, 2, 3, 4, 5, 6),      # column offsets for price to match price.col
                          c(19,24,29,34,39,44,49),      # the actual prices, to be readable
                          c( 0, 0, 0, 0, 0, 0, 0)   )  # proportions found
  
  GA.totalN     <- GA.pws.totalN
  GA.sampleN    <- 0
  
  for (iii in 1:GA.result.reps)
  {
    GA.iteration <<- iii             # stored in top-level environment so we can print progress when the GA library calls fitness function
    GA.result.vec <- rep(0,prod.atts)
    # pick sample of respondents
    if (HB.draws) {
      GA.respSample <- sample(1:GA.totalN,round(GA.totalN*GA.sampleProp))
      # construct list of draws that match each sampled respondent
      GA.sample <- NULL
      GA.sampleN <- length(GA.respSample)
      for (i in 1:length(GA.respSample))
      {
        GA.sample <- c(GA.sample,((GA.respSample[i]-1)*GA.pws.drawsEach+1):(GA.respSample[i]*GA.pws.drawsEach))
      }
    } else {
      GA.sample <- sample(1:GA.totalN,round(GA.totalN*GA.sampleProp))
      GA.sampleN <- length(GA.sample)
    }
    
    usc1.product.pws.GA <<- usc1.product.pws[GA.sample,]
    
    #     fit GA to the sample
    #     you can consider changing the default Px parameters for GA if you want to use a specific evolution type; see the rgenoud documentation
    #     but don't use P5 or P9 because they may violate the integer bounds and yield out of bounds levels for an attribute
    product.port.fitness.bestvalue <<- 0
    GA.result <- genoud(fn=fit.function, nvars=prod.num*prod.len, max=TRUE, 
                        pop.size=GApop.size, max.generations=GAmax.gen, print.level=1, wait.generations=GA.wait,
                        Domains=domains.mat, data.type.int=TRUE, boundary.enforcement=2, 
                        P1=100, P2=100, P3=100, P4=100, P5=0, P6=100, P7=100, P8=100, P9=0 )
    
    # add the fixed competitor definitions to the list if needed
    #   vv
    #          vec.combined <- c(GA.result$par,product.fix.vec)
    vec.combined <- GA.result$par
    
    # break out the results
    vec.list <- NULL
    for (i in 1:(length(vec.combined)/prod.len))
    {
      #  vec.list.el <- list(GA.result$par[((i-1)*prod.len+1):(i*prod.len)])
      vec.list.el <- list(vec.combined[((i-1)*prod.len+1):(i*prod.len)])
      
      ### CUSTOMIZATION for feature interactions              
      #              if(vec.list.el[[1]][3]==10) { vec.list.el[[1]][5] <- 18 }                        # if COL 3 feature, set to the NULL partworth column
      #              if(vec.list.el[[1]][3]!=10 && vec.list.el[[1]][5] == 18) { vec.list.el[[1]][5] <- 19 }    # if *some feature combo* but no battery, set another feature appropriately
      
      vec.list <- c(vec.list,vec.list.el)
    }
    
    # determine price for each product
    for (i in 1:(length(vec.combined)/prod.len))
    {
      ### find appropriate price column in PW data
      ### CUSTOMIZATION for price range
      price.offset <- 17    ######## HARD-CODED must match start of price COLUMNs in the PW data matrix (not the price value itself)
      price.start <- 19     ### lowest possible price
      price.end   <- 49     ### highest possible price
      price.inc   <- 5      ### price increments between that range
      
      prod.price.raw <- price.start+list.price(vec.list[[i]], product.price.list, product.price.offset)
      
      if (prod.price.raw < price.start) { prod.price.raw <- price.start }
      if (prod.price.raw > price.end)   { prod.price.raw <- price.end }
      
      # CUSTOMIZATION update for your pricing if needed
      prod.price.col <- price.offset+((round(prod.price.raw*2+1,-1)/2-1)-price.start)/price.inc   # round to nearest $5 increment and then to $..4 or ..9
      vec.list[[i]] <- c(vec.list[[i]],prod.price.col)    # add the price PW column to the product definition
    }
    
    # figure prod.select share for each product
    # using HOLDOUT data
    
    ### CUSTOMIZATION to set the "None" column as needed
    if (HB.draws) {
      #   set None column and the structure of your HB draw data                      vv                                                                                 vvvv           vvvv
      prod.sel <- prod.select.product(usc1.product.pws[-GA.sample,],vec.list,none.col=24, use.none=TRUE,tuning=tuning,draws=100,n.resp=(GA.totalN-GA.sampleN),draws.each=1000,use.error=TRUE)
    } else {
      # set None column                                                               vv                                    vv
      # and bootstrap draws if you want them
      prod.sel <- prod.select.product(usc1.product.pws[-GA.sample,],vec.list,none.col=24, use.none=TRUE,tuning=tuning,draws= 1,n.resp=(GA.totalN-GA.sampleN),draws.each=1,use.error=FALSE)
    }
    
    #          ## CUSTOMIZATION fix selection for invalid attribute combinations
    #          ## for each "product", if it has invalid combination of features, simply set its preference to 0
    #          ##
    #          for (i in 1:(length(vec.combined)/prod.len))
    #          {
    #              vec.tmp <- vec.list[[i]]
    #              if ((18 %in% vec.tmp) && (9 %in% vec.tmp || 10 %in% vec.tmp || 11 %in% vec.tmp))   # invalid feature combination
    #              {
    #                  prod.sel[,i] <- rep(0,nrow(prod.sel))                                         # set its preference to 0
    #              } else if ((17 %in% vec.tmp) && (9 %in% vec.tmp)) {                                # another one ...
    #                  prod.sel[,i] <- rep(0,nrow(prod.sel))
    #              }
    #          }
    #
    # determine buy/no-buy for each respondent/product
    ps.which <- max.col(prod.sel)    # the column they are most likely to select
    ps.which <- factor(ps.which,levels=1:((length(vec.combined)/prod.len)+1),ordered=TRUE)
    
    # get average buy/no-buy for each product across respondents
    ps.buy   <- length(ps.which[ps.which < ((length(vec.combined)/prod.len)+1)])/length(ps.which)   # the proportion who select something other than "none"
    
    buy.table <- table(ps.which)/length(ps.which)
    GA.product.line <- buy.table
    GA.product.line <- c(GA.product.line,GA.result$par[1:(prod.num*prod.len)])
    print("Holdout result:")
    print(GA.product.line)
    GA.product.mat <- rbind(GA.product.mat,GA.product.line)
    
    for (ii in 1:(length(vec.combined)/prod.len))
    {
      for (j in 1:prod.atts) 
      {
        if (j %in% vec.list[[ii]][1:prod.len])
        {
          if (!is.na(buy.table[sub("L","",deparse(ii))])) {
            GA.result.vec[j] <- GA.result.vec[j] + buy.table[sub("L","",deparse(ii))]
          }
          # check twoway frequency
          for (k in 1:prod.atts)
          {
            if (k!=j & k %in% vec.list[[ii]][1:prod.len])
            {
              if (!is.na(buy.table[sub("L","",deparse(ii))])) {
                GA.result.twoway[j,k] <- GA.result.twoway[j,k] + buy.table[sub("L","",deparse(ii))]
              }
            }
          }
        }
      }
      
      ### CUSTOMIZATION -- update to the column where your price columns are
      ###                                                               vvv    
      price.index <- which(GA.price.mat[1,]==(vec.list[[ii]][prod.len+1]-31))   ## hard-coded offset
      if (!is.na(buy.table[sub("L","",deparse(ii))])) {
        GA.price.mat[3,price.index] <- GA.price.mat[3,price.index] + buy.table[sub("L","",deparse(ii))]
      }
    }
    GA.result.mat <- rbind(GA.result.mat,GA.result.vec)
    gc()             ## clean stuff up so next iteration will be in good shape
  }
  
  return(list(GA.result.mat,GA.price.mat,GA.result.twoway,GA.product.mat))
}


### TEST IT AGAIN!!  You cannot test too much ...
####
#### product.GA.bootstrap(
#           GArep.product.fit.wrapper,          == the fitness wrapper defined to parse a single result vector
#           GA.result.reps=5,                   == how many times to run the GA
#           GApop.size=40,                      == population size for each run
#           GAmax.gen=10,                       == maximal number of ganerations to evolve the GA
#           GA.wait=2,                          == how many generations to wait if the population is showning no improvement
#           tuning=0.5,                         == tuning factor for the partworths in MNL estimation (set to 1.0 if you are unsure)
#           prod.num=4,                         == the "portfolio size", i.e., the number of products to co-evolve
#           prod.len=4,                         == how many attributes per product NOT counting price
#           prod.atts=23)                       == the total number of levels in your product, including price levels

product.t4.test <- product.GA.bootstrap(GArep.product.fit.wrapper,GA.result.reps=5,GApop.size=40,GAmax.gen=10,GA.wait=2,tuning=0.5, prod.num=4, prod.len=4, prod.atts=23)



### some examples of simple analysis:

### what was the share of preference for the 4 portfolios vs. none (5) ?
### columns 1-4 show the HOLDOUT preference share for the 4 products listed in each attribute string at the end of the matrix row
product.t4.test[[4]]

# which products showed up?
#   You'll want to handle this yourself in other ways, but this shows how the results are structured, e.g., to compile a list of products and their frequency of appearance
#
for (i in 1:5)               # 5 runs ...
{
  print (product.translate(product.t4.test[[4]][i, 6:9],product.att.list,product.price.offset))    # of 4 products each time
  print (product.translate(product.t4.test[[4]][i, 10:13],product.att.list,product.price.offset))
  print (product.translate(product.t4.test[[4]][i, 14:17],product.att.list,product.price.offset))
  print (product.translate(product.t4.test[[4]][i, 18:21],product.att.list,product.price.offset))
}



##################
####
#### FINALLY !
#### Now do our real work ...
####


### Wrapper to run the GA a certain number of times
### This way you can simply change a couple of parameters to do the whole GA and save the results
###
### CUSTOMIZATION: set the parameters for the individual run to what you want
### Must at least set the "prod.len" (number of attributes) and "prod.atts" (total number of levels, i.e., alleles in the genome) to match your data
###
product.GA <- function(ga.reps, prod.num)
{
  #                                                                                                     vvv           vv         vv        vvvv                             vv           vvv
  product.GA.result <- product.GA.bootstrap(GArep.product.fit.wrapper,GA.result.reps=ga.reps,GApop.size=200,GAmax.gen=80,GA.wait=10,tuning=0.50, prod.num=prod.num, prod.len=4, prod.atts=23)
  return(product.GA.result)
}



###### STEP SEVEN -- RUN IT!

### environmental variables that must be defined -- be sure to set in environment and set to correct values before a run
### 
###
product.port.fitness.bestvalue <- 0
GA.iteration <- 0
product.fitness.PS.draws <- 1
product.fitness.N.draws <- 1

product.GA.k2 <- product.GA(20,2)   # run it 20 times with portfolio of 2 products and save the results
product.GA.k4 <- product.GA(20,4)   # 4 products ...
product.GA.k6 <- product.GA(20,6)   # 6
product.GA.k8 <- product.GA(20,8)   # etc
product.GA.k10 <- product.GA(20,10)

# and then do whatever analyses you want on the products found, feature distributions, etc.
# see the sample analysis above from a single run for a simple example

# Good luck!
#
# -- Chris
