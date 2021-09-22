//======================================================================================
// This Stan program defines a model, with a vector of values 'Arc' spanning the modeled 
// a vonMises distribution (normally distributed for angles) with mean 'muA' and 
// dispersion 'kappaA'. Where the mean is dependent on the height of sampling h (i.e., 
// breast height versus at the top of the tree).
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//---------------------------------------------------------------------------------------

// The input data is a vector 'Arc' and a integer of length 'N'.
data {
  int<lower=0> N; // length cannot be negative
  vector[N] Arc;
  int h[N];
}

// The parameters accepted by the model. Our model accepts an intercept 'a0' and an effect 
// of height 'aH' plus a dispersion parameter 'kappaA'.
parameters {
  real a0; // intercept 
  vector[2] aH; // effect of height
  real<lower=0> kappaA; // dispersion parameter 'kappaA' cannot be negative
}

// We model the output 'Arc' to be a normally distributed angle (vonMises distribution) 
// with mean 'muA' and a measure of dispersion 'kappaA'.
model{
  vector[N] muA;
  kappaA ~ exponential( 1 );
  aH ~ normal( 0 , 2 ); // should I use a uniform prior here?
  a0 ~ normal( 0 , 2 ); // should I use a uniform prior here?
  for ( i in 1:N ) {
    muA[i] = a0 + aH[h[i]];
    //muA[i] = aH[h[i]];
  }
  Arc ~ von_mises( muA , kappaA );
}
