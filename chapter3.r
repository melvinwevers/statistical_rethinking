#3.2

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#3.3
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#3.4
plot(samples)

#3.5
library(rethinking)
Options(mc.cores = parallel::detectCores())
dens(samples)

#3.6
# add posterior probs where p < 0.5
sum(posterior[p_grid <0.5])

#3.7
sum(samples < 0.5) / 1e4

#3.8
sum(samples> 0.5 & samples < 0.75) / 1e4

#3.9
quantile(samples, 0.8)

#3.10
quantile(samples, c(.1, .9))

#3.11
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size=3, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

#3.12
PI(samples, prob=0.5)

#3.13
HPDI(samples, prob=0.5)

#3.14
p_grid[which.max(posterior)]

#3.15
chainmode(samples, adj=0.01)

#3.16
mean(samples)
median(samples)


                                        #3.17
# weighted average loss
sum(posterior*abs(0.5-p_grid))

#3.18
loss <- sapply(p_grid, function(d) sum(posterior*abs(d - p_grid)))
#3.19
p_grid[which.min(loss)]

                                        #3.20
dbinom(0:2, size=2, prob=0.7)

#3.22
                                        # sample from binomial
rbinom(10, size=2, prob=0.7)

#3.23
dummy_w <- rbinom(1e5, size=2, prob=0.7)
table(dummy_w)/1e5

                                        #3.24
dummy_w <- rbinom(1e5, size=9, prob=0.7)
simplehist(dummy_w, xlab='dummy water count')

#3.25
w <- rbinom (1e4, size=9, prob=.6)
simplehist(w)

#3.26
w <- rbinom(1e4, size=9, prob=samples)
simplehist(w)
                                        #

                                        #Exercises
library(rethinking)

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#3E1
mean(samples < 0.2)

#3E2
mean(samples > 0.8)

#3E3
mean(samples > 0.2 & samples < 0.8)

#3E4
quantile(samples, 0.2)

#3E5
quantile(samples, 0.8)

#3E6
HPDI(samples, prob=.66)
# quarter of values representing water (p) provides 66% of probabilty mass
#3E7
PI(samples, prob=.66)
                                      # Not too skewed, as HPDI and PI are almost equal

#3M1

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#3M2
HPDI(samples, prob=.9)

                                        #3M3
                                        #simulate posterior predictive check
                                        # what is probabilty of observing 8 water in 15 tosses

w <- rbinom(1e4, size=15, prob=samples)
table(w)/1e4

                                        #observating 8 water is .1473



                                        #3M5
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

HPDI(samples, prob=.9)
                                        #the range is narrower
w <- rbinom(1e4, size=15, prob=samples)
table(w)/1e4
                                        #probability is higher, but peak has shifted to higher values
                                        # 70 percent of 15 is 10.5

                                        #3H1
library(rethinking)
data(homeworkch3)

boys_born = sum(birth1) + sum(birth2)
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,200)
likelihood <- dbinom(boys_born, size=200, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior, type="b",
     xlab="probability of boy", ylab="posterior probability")

p_grid[which.max(posterior)]

                                        #3H2
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

                                        #50, 89 and 97 highest probability intervals
HPDI(samples, prob=.5)

HPDI(samples, prob=.89)

HPDI(samples, prob=.97)

                                        #3H3
simulation_boys <- rbinom(1e4, size = 200, prob=samples )

dens(simulation_boys)
abline(v=111)

                                        #peak of simulation is a little bit higher than actual data, but generally the central peak is around 111.

                                        #3H4
                                        #3H3
#Calculate again? or using size 100 in simulation??
simulation_boys_first <- rbinom(1e4, size = 100, prob=samples )
dens(simulation_boys_first)
abline(v=sum(birth1))
#actually number of boys in firth birth is lower than in simulated distribution (which is based on 2 births)

                                        #3H5
                                        #Are they independent?
girl_first <- 100 - sum(birth1)
boy_after_girl <- birth2[birth1==0]
simulation3h5 <- rbinom(1e4, size=girl_first, prob=samples)

dens(simulation3h5)
abline(v=sum(boy_after_girl))
                                        #male births that followed girls much higher than distribution male and female in first birth
                                        # people selected who was born?

