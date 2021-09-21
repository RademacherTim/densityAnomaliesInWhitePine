data{
  vector[64] Arc;
  int h[64];
}
parameters{
  real a0;
  vector[2] aH;
  real<lower=0> kappaA;
}
model{
  vector[64] muA;
  kappaA ~ exponential( 1 );
  aH ~ normal( 0 , 2 );
  a0 ~ normal( 0 , 2 );
  for ( i in 1:64 ) {
    muA[i] = a0 + aH[h[i]];
    //muA[i] = aH[h[i]];
  }
  Arc ~ von_mises( muA , kappaA );
}
generated quantities{
  vector[64] log_lik;
  vector[64] muA;
  for ( i in 1:64 ) {
    muA[i] = a0 + aH[h[i]];
    //muA[i] = aH[h[i]];
  }
  for ( i in 1:64 ) log_lik[i] = normal_lpdf( Arc[i] | muA[i] , kappaA );
}
