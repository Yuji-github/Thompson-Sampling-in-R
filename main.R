# Thompson Sampling
# install.packages('rBeta2009')
# library(rBeta2009)

# import data 
dataset = read.csv('Ads_CTR_Optimisation.csv')

#  with random selections for comparing

# preparing
N = nrow(dataset) # get num of rows
d = ncol(dataset) # get num of lows

ads_selection = integer(0)
total_reward = 0 

for (n in 1:N) # must have 1 start
{
  # step 1
  ad = sample(1:10, 1) #sample takes a sample of the specified size from the elements of x using either with or without replacement.
  
  # step2
  ads_selection = append(ads_selection, ad)
  reward = dataset[n, ad]
  
  # step 3 
  total_reward = total_reward + reward
} 

# visualizing results
hist(ads_selection, 
     col = 'green'
)

# Thompson Sampling implementation
# step 1: 
nums_of_selection_1 = integer(d) # d =10
nums_of_selection_0 = integer(d)

ads_selected = integer() # store the results
all_rewards = 0

# step 2 and 3
for (n in 1:N)
{
  ad = 0
  max_random = 0 # check maximum random each round
  for (i in 1:d)
  {
    random = rbeta(n=1, shape1 = nums_of_selection_1[i]+1, shape2 = nums_of_selection_0[i]+1)
    
    if (max_random < random)
    {
      max_random = random # swap the value
      ad = i # for ads_selected
    }
  }
  
  # updating the values
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  
  if(reward == 1)
  {
    nums_of_selection_1[ad] = nums_of_selection_1[ad] +1
  }
  else
  {
    nums_of_selection_0[ad] = nums_of_selection_0[ad] +1
  }
  
  all_rewards = all_rewards + reward 
}

# visualizing results
hist(ads_selected, 
     col = 'blue'
)