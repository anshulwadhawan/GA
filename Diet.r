install.packages('GA')
library(GA) # Version 3.0.1

# Define data 
dataset=read.csv('NutritionalFacts_Fruit_Vegetables_Seafood.csv',header = TRUE)
p<-dataset$Protein
e<-dataset$Calories
food<-dataset$Food.and.Serving
p=p[2:62]
e=e[2:62]
food=food[2:62]
p=as.vector(p)
e=as.vector(e)
p=as.numeric(p)
e=as.numeric(e)
p <- c(6, 5, 8, 9, 6, 7, 3) # Proteins
e <- c(2, 3, 6, 7, 5, 9, 4) # Carbs
E <- 500 # Energy limit 
n <- length(p) # Number of food items

# Define fitness function 
knapsack <- function(x) { 
  f <- sum(x * p) 
  penalty <- sum(e) * abs(sum(x * e) - E) 
  f - penalty 
}

# Run SGA
SGA <- ga(type="binary", 
          fitness=knapsack , 
          nBits=n, 
          maxiter=100, # Maximum number of generations 
          run=200,     # Stop if the best-so-far fitness
          # hasn't improved for 'run' generations 
          popSize=100, 
          seed=101)

plot(SGA)

summary(SGA)

sol <- SGA@solution 
sol
sum(sol)     #Total items to take

for(i in 1:61){
  if(sol[i]==1){
    print(food[i],max.levels=0)
  }
}

sum(sol * p) #Total protein intake
sum(sol * e) #Total energy intake (<=E)