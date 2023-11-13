#################################################
#* This script is to fulfill the 
#*    requirements of STA308 Fall 2023
#*    Homework #4 
#*    
#* Author: Grace Runge
#* 
#* Date: November 10th, 2023 
#* 
#* Purpose: To write a function called roulette 
#*    streaks that randomly generates num_spins 
#*    of roulette outcomes and determines the 
#*    longest streak of consecutive outcomes and 
#*    then to test it under different scenarios.
#################################################

roulette_streaks <- function(num_spins=600) {
  spin <- sample(c("Black", "Red", "Green"), size=num_spins, 
                 prob=c(18/38, 18/38, 2/38), replace = TRUE)
  number=1
  streak_number = 1 
  streak_list = list()
  while(number<length(spin)){
    if(spin[number] == spin[number + 1]){
      streak_number = streak_number + 1
    } else {
      streak_list <- c(streak_list, streak_number)
      streak_number = 1 
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

#* The summary of this situation provides a minimum length of a 
#*   streak as 5 and the maximum of 18. The mean is about 8.9
#*   and the median is 9. The mean and median are pretty close 
#*   so I expect the shape to be close to symmetrical. The histogram shows a 
#*   slightly right skewed bell curve shape. If you are playing roulette 
#*   in a casino you would be very lucky to get a maximum streak 
#*   on the higher end of this distribution, but this simulation shows it 
#*   does happen occasionally and this is perhaps why people continue 
#*   to play games like this for the small chance to win big.
#* 

#* Suppose a casino can do 1200 spins per day as 
#* opposed to the default 600, and run the function for
#* 1500 iterations to see how the distribution changes.

set.seed(314159)
roulette_simulation_2 <- sapply(rep(1200, 1500), roulette_streaks)
summary(roulette_simulation_2)
hist(roulette_simulation_2)

#* If a casino could increase the number of spins per day to 
#*   1200 (double the standard of 600) the distribution of 
#*   longest streaks is going to change at least slightly. 
#*   The minimum is now 6 and the maximum is 20. The range of streaks 
#*   increased in this new scenario. The range of streaks in this 
#*   distribution is 14 which is 1 larger than the range of 13 in 
#*   the previous simulation. The minimum streak has increased by 1 as well.
#*   The median is still 9, but the mean has now increased from 
#*   to 9.7. The more times you are able to run the simulation 
#*   in a day the more likely you are to get a higher streak if 
#*   your criteria is strictly the mean. 
#*   The histogram still shows a right skewed distribution but the main
#*   peak appears slightly less spread out than the previous situation. 
#*   The two distributions are not drastically different. 
#*   I think you would have to change the number of spins
#*   or the number of repetitions even more to see more drastic 
#*   changes to the distribution.
#*   








