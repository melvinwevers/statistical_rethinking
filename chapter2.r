

P_grid <- seq(from = 0, to=1, length.out = 200)
#prior <- rep(1,20)
#prior <- ifelse(p_grid < 0.5, 0, 1)
prior <- exp(-5*abs (p_grid - 0.5))
likelihood <- dbinom(6, 9, p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type='b',
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")

library(rethinking)


globe.qa <- map(
    alist(
        w ~ dbinom(9, p), # binomial likelihood
        p ~ dunif(0,1) # uniform prior
    ),
    data=list(w=6))

#display summary of quadratic approx.

precis(globe.qa)


#analytical cal
w <- 6
n <- 9

curve(dbeta(x, w+1, n-w+1), from=0, to =1)

#quadratic approx.
curve(dnorm(x, 0.67, 0.16), lty=2, add=TRUE)

#exercises
# 2E1
# 2 or 4
#2E2 3
p
#2E3 1 or 4
#2E4 Given the uncertainty and our imperfect knowledge, we measure a probability of water of 0.7

#2M1

compute_posterior <- function(w, n, prior, p = p_grid){
    likelihood <- dbinom(w, n, p)
    unstd.posterior <- likelihood * prior
    return(unstd.posterior / sum(unstd.posterior))
}

plot_posterior <- function(x, y){
    plot(x, y, type='b', xlab='P', ylab='Posterior Probability' )
    title <- paste(length(x), "points")
    mtext(title)
}

p_grid <- seq(from = 0, to=1, length.out = 20)
prior <- rep(1, length(p_grid))
#prior <- ifelse(p_grid < 0.5, 0, 1)

w <- 5
n <- 7

posterior <- compute_posterior(w = w, n = n, prior = prior)

plot_posterior(x = p_grid, y = posterior)

#2M3
prior <- c(.5)
likelihood <- c(.3, 1)

unstd.posterior <- prior * likelihood 
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]

# 2M4
# three cards. (1,0), (.5, .5), (0,1)
# there are two ways for card1, one for card 2, and 0 for card 3.
likelihood <- c(2, 1, 0)
prior <- rep(1, length(likelihood))
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]

# 2M5
likelihood <- c(2, 1, 0, 2)
prior <- rep(1, length(likelihood))
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] + posterior[4]

# 2M6
likelihood <- c(2, 1, 0)
prior <- rep(1, 2, 3)
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]

# 2M7
# card1=b/b
# card2=b/w
# card3=w/w

1_2-likelihood <- 2
2_1-likelihood <- 0
1_3-likelihood <- 4
3_1-likelihood <- 0
2_3-likelihood <- 2
3_2-likelihood <- 0

likelihood <- c(2, 0, 4, 0, 2, 0)
prior <- rep(1, length(likelihood))
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] + posterior[3]

#2H1
#posterior serves as prior for second round of twins

p1 = .1
p2 = .2

likelihood <-c(p1, p2)
prior <-c(1,1)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)

posterior[1] * p1 + posterior[2] * p2

posterior2 = posterior * likelihood
#posterior2 = posterior2 / sum(posterior2)
posterior2



#2H2

p1 = .1
p2 = .2

likelihood <-c(p1, p2)
prior <-c(1,1)
posterior <- prior * likelihood
posterior <- posterior/ sum(posterior)
posterior[1]

#2h3

p1 = .1
p2 = .2

likelihood2 <-c(p1 * (1-p1), p2 * (1-p2))
prior <-c(1,1)
posterior <- prior * likelihood2
posterior_single <- posterior / sum(posterior)

posterior_single[1]

#2h4
p_correctA = .8
p_correctB = .65

likelihood <- c(p_correctA, (1 - p_correctB))
prior <- c(1, 1)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
posterior[1]

#update with information from 2h3

prior2 <- posterior_single
posterior_2h4 <- prior2 * likelihood
posterior_2h4 <- posterior_2h4 / sum(posterior_2h4)
