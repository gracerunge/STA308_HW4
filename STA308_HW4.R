#################################################
#* This script is to fulfill the 
#*    requirements of STA308 
#*    homework #4 
#*    
#* Author: Grace Runge
#* 
#* Date: November 9th, 2023 
#* 
#* Purpose: To write a function called roulette 
#*    streaks that randomly generates num_spins 
#*    of roulette outcomes and determines the 
#*    longest streak of consecutive outcomes.
#################################################

roulette_streaks <- function(num_spins=600) {
  spin <- sample(c("Black", "Red", "Green"), size=num_spins, prob=c(18/38, 18/38, 2/38), 
                 replace = TRUE)
  number=1
  streak_number = 0 
  streak_list = list()
  while(number<length(spin)){
    if(spin[number] == spin[number + 1]){
      streak_number = streak_number + 1
    } else {
      streak_list <- c(streak_list, streak_number)
      streak_number = 0 
    } 
    number = number + 1
  }
  max(unlist(streak_list))
}

#* Ensure the function is working properly 
#* by simulating 1,500 iterations with the following details:
#*    Set the random number generate seed to 314159 
#*    Provide the summary() output and a histogram 
#*    Comment about the distribution of the number of matches

set.seed(314159)
roulette_simulation_1 <- sapply(rep(600, 1500), roulette_streaks)
summary(roulette_simulation_1)
hist(roulette_simulation_1)

#* Insert comments here 
#* 

#* Suppose a casino can do 1200 spins per day as 
#* opposed to the default 600, and run the function 
#* for 1500 iterations to see how the distribution changes.

set.seed(314159)
roulette_simulation_2 <- sapply(rep(1200, 1500), roulette_streaks)
summary(roulette_simulation_2)
hist(roulette_simulation_2)

#* Insert comments here 
#* 







