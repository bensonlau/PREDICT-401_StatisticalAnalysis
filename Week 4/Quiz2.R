#Question 1
  #Assume that a procedure yields a binomial distribution with
  #a trial repeated n times. Use the binomial probability 
  #formula to find the probability of x successes given the 
  #probability p of success on a single trial. 

  #Round to three decimal places. n = 4, x = 3, p = 1/6.
  round(dbinom(x=3,size=4,prob=(1/6)),3) #[1] 0.015

#Question 2
  #Use Bayes' theorem to find the indicated probability.
  
  #5.8% of a population is infected with a certain disease. There is a test for the 
  #disease, however the test is not completely accurate. 93.9% of those who have the
  #disease test positive. However 4.1% of those who do not have the disease also test 
  #positive (false positives). A person is randomly selected and tested for the disease.
  
  #What is the probability that the person has the disease given that the test result is positive?

    d <- 0.058
    dNot <- (1 - d)

    dHaveTrue <- 0.939
    dHaveFalse <- (1 - dHaveTrue)

    dNotTrue <- 0.041                                     #False Positive
    dNotFalse <- (1 - dNotTrue)

    Q2 <- (d * dHaveTrue) / ((dNot * dNotTrue) + (d * dHaveTrue))
    round(Q2, 3) #0.585

#Question 3
  #If the random variable x has a Poisson Distribution with mean equal to 4, find the probability that
  #x = 5.  Round to 3 decimal places
  dpois(5,4)

#Question 4
  #Solve the problem.
  #Given the following sample data: 1.3, 2.2, 2.7, 3.1, 3.3, 3.7, use quantile() in R with 
  #type = 2 to find the estimated 63rd percentile.  Pick the correct answer.

  test<-c(1.3, 2.2, 2.7, 3.1, 3.3, 3.7)
  quantile(test, .63,type=2)

  # Problem:  Suppose a production process produces widgets with a weight distributed
  # as a normal variable with mean of 100 grams and standard deviation of 10 grams.

  # What is the probability of a random sample of size 25 having a mean value that is 
  # outside 100 +- 2 grams?

  pnorm(98, mean = 100, sd = 10/sqrt(25), lower.tail = TRUE)+ 
      pnorm(102, mean = 100, sd = 10/sqrt(25), lower.tail = FALSE)

  2*pnorm(-1, 0, 1, lower.tail = TRUE)

#Question 5
  #A study of the amount of time it takes a mechanic to 
  #rebuild the transmission for a 2005 Chevrolet Cavalier 
  #shows that the mean is 8.4 hours and the standard deviation
  #is 1.8 hours. If a random sample of 36 mechanics is 
  #selected, find the probability that their mean rebuild time
  #exceeds 8.7 hours. Assume the mean rebuild time has a 
  #normal distribution. (Hint, interpolate in the tables or 
  #use pnorm().)

  #http://www.harjunoxie.com/xie/Spring/Sp2012/math227/Sp12m227q9Fsol.pdf

  round(pnorm(8.7, mean = 8.4, sd = 1.8/sqrt(36), lower.tail = FALSE),3) #0.159

#Question 6
  #Find the mean for the binomial distribution with the number
  #of trials n = 676 and the probability of success p = 0.7.
  #The formula for the mean of binomial distribution is:
  #μ = n *p

  n=676
  p=0.7
  n*p #473.2

#Question 7
  #The given values are discrete (binomial outcomes). Use the continuity correction and 
  #describe the region of the normal distribution that corresponds to the indicated probability.
  #The probability of more than 44 correct answer

  #answer
  ####The area to the right of 44.5 


#Question 8
  #Solve the problem. Round to the nearest tenth unless indicated otherwise.
  #In one region, the September energy consumption levels for single-family homes are found
  #to be normally distributed with a mean of 1050 kWh and a standard deviation of 218 kWh.
  #Find P45 (45th percentile).

  round(qnorm(45,1050,218),1)

#Question 9
  #Find the indicated probability.
  #In a homicide case 4 different witnesses picked the same man from a line up. The line up
  #contained 5 men. If the identifications were made by random guesses, find the 
  #probability that all 4 witnesses would pick the same person. (Hint-there is more than one
  #way the witnesses can agree.)

  p=0.2
  p^4 #0.0016

#Question 10
  #Find the indicated probability.
  #An IRS auditor randomly selects 3 tax returns from 49 returns of which 7 contain
  #errors. What is the probability that she selects none of those containing errors? 
  #Round to four decimal places.

  b<-factorial(49) / (factorial(3) * factorial(49 - 3))
  a<-factorial(42) / (factorial(3) * factorial(42 - 3))

  round(a/b,4)
