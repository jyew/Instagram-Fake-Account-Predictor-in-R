# Instagram-Fake-Account-Predictor
 This is an implementation of building a predictive model to detect fake account on Instagram using R

In this project we could see how information on one of the most popular social media, Instagram could be used to build a predictive model that determines whether an account is a fake and fraudulent account. 

I analyzed the data distributions and relationships between the variables and used GLM and GAM techniques to build a model that has achieved an accuracy rate of 92.5% in estimating fake or real account.

## Final Model
log(𝜋/1−𝜋)= 11.9449−4.2872𝐼𝑝𝑟𝑜𝑓𝑖𝑙𝑒.𝑝𝑖𝑐=1−4.0259log(𝑋.𝑓𝑜𝑙𝑙𝑜𝑤𝑒𝑟𝑠+1)+ 𝑔1(𝑛𝑢𝑚𝑠.𝑙𝑒𝑛𝑔𝑡ℎ.𝑢𝑠𝑒𝑟𝑛𝑎𝑚𝑒)+ 𝑔2(𝑋.𝑝𝑜𝑠𝑡𝑠)+ 𝑔3(𝑋.𝑓𝑜𝑙𝑙𝑜𝑤𝑠)+ 𝑔4(𝑑𝑒𝑠𝑐𝑟𝑖𝑝𝑡𝑖𝑜𝑛.𝑙𝑒𝑛𝑔𝑡ℎ)

Where 𝜋 is the probability of a fake account and the components g1, g2, g3, g4 are estimated by smoothing splines of nums.length.username, X.posts, X.follows and description.length.
