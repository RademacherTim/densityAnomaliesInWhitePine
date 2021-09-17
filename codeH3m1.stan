data{
  vector[1000] Arc;
  int h[1000];
}
parameters{
  real a0;
  vector[2] aH;
  real<lower=0> kappaA;
}
model{
  vector[1000] muA;
  kappaA ~ exponential( 1 );
  aH ~ normal( 0 , 2 );
  a0 ~ normal( 0 , 2 );
  for ( i in 1:1000 ) {
    muA[i] = a0 + aH[h[i]];
    //muA[i] = aH[h[i]];
  }
  Arc ~ von_mises( muA , kappaA );
}
generated quantities{
  vector[1000] log_lik;
  vector[1000] muA;
  for ( i in 1:1000 ) {
    muA[i] = a0 + aH[h[i]];
    //muA[i] = aH[h[i]];
  }
  for ( i in 1:1000 ) log_lik[i] = normal_lpdf( Arc[i] | muA[i] , kappaA );
}
