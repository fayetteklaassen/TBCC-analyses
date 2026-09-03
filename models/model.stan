// MODEL starting with fixed parameters, but ALL the intended states included
// also keeping the force of infection fixed, but making a contact parameter that is estimated w/ random walk (?)
// and a healthcare access parameter with another random walk??? that seems too strong...
 functions{ vector nutrition_wave(real effect_size, int time_to_peak, int time_after_peak, int H) {
    int total_time = time_to_peak + time_after_peak;
    vector[H] wave = rep_vector(1.0, H);
    
    for (t in 1:total_time) {
      real progress;
      if (t <= time_to_peak) {
        progress = (t - 1.0) / (time_to_peak - 1.0);
        wave[t] = 1.0 + effect_size * (2 * progress - progress^2);
      } else {
        int time_after = t - time_to_peak;
        progress = (time_after - 1.0) / (time_after_peak - 1.0);
        wave[t] = 1.0 + effect_size * (1.0 - (2 * progress - progress^2));
      }
    }
    return wave;
  }
 }
  
data {
  real<lower=0> pop_init;
  int<lower=0> Q;
  int<lower=0> H;
  int<lower=0> S;
  int<lower=0> TB_rep[Q];
  int<lower=0,upper=1> skip[Q];
  vector[Q+H] xi; //birthrate
  vector[Q+H] mu; // mortality
  vector[S] fi_effect_size;
  int fi_time_start[S];
  int fi_time_end[S];
  vector[S] ha_effect_size;
  
  real<lower=0,upper=1> prot_reinf_lo;
  real<lower=0,upper=1> prot_reinf_hi;
  
  real<lower=0> r_fast_slow_lo; 
  real<lower=0> r_fast_slow_hi; 
  real<lower=0> r_slow_sym_lo; 
  real<lower=0> r_slow_sym_hi; 
  real<lower=0> r_fast_sym_lo; 
  real<lower=0> r_fast_sym_hi; 
  real<lower=0> r_slow_clear_lo;
  real<lower=0> r_slow_clear_hi;
  real<lower=0> r_fast_clear_lo;
  real<lower=0> r_fast_clear_hi;
  real<lower=0> r_sym_cure_lo; 
  real<lower=0> r_sym_cure_hi; 
  real<lower=0> r_sym_die_lo;
  real<lower=0> r_sym_die_hi;
  real<lower=0> r_sym_diag_lo;
  real<lower=0> r_sym_diag_hi;
  real<lower=0> r_diag_sym; 
  real<lower=0> r_diag_die;
  real<lower=0> r_diag_cure;
  real<lower=0,upper=1> r_ha;
  real<lower=0, upper=1> r_nutrition;
  real<lower=0> re_sd;

}

transformed data{
    vector[Q+H] pop;
    // vector[8] r_nutrition_wave;
    int N = H+Q;
for(i in 1:N){
  if(i == 1){
    pop[1] = pop_init;
  } else {
    pop[i] = pop[i-1] * (1.0 - mu[i] + xi[i]);
  }

}
}

parameters {
  real<lower=.1,upper=.6> init_slow;
  real<lower=0.01,upper=.2> init_fast;
  real<lower=0.001,upper=.3> init_sym;
  real<lower=.0005,upper=.15> init_diagnosed;
  
  real<lower=prot_reinf_lo,upper=prot_reinf_hi> prot_reinf;
  real<lower=r_fast_slow_lo,upper=r_fast_slow_hi> r_fast_slow;
  real<lower=r_fast_sym_lo,upper=r_fast_sym_hi> r_fast_sym;
  real<lower=r_fast_clear_lo,upper=r_fast_clear_hi> r_fast_clear;
  real<lower=r_slow_clear_lo,upper=r_slow_clear_hi> r_slow_clear;
  real<lower=r_slow_sym_lo,upper=r_slow_sym_hi> r_slow_sym;
  real<lower=r_sym_die_lo,upper=r_sym_die_hi> r_sym_die;
  real<lower=r_sym_diag_lo,upper=r_sym_diag_hi> r_sym_diag;
  real<lower=r_sym_cure_lo,upper=r_sym_cure_hi> r_sym_cure;

  simplex[3] theta;
  real force;
  real<lower=0> inv_sqrt_phi;
  vector[Q] re_contact;
  // real<lower=0> re_sd;
}

transformed parameters{
  vector[Q] new_inf;
  vector[Q] new_diagnosed;
  vector[Q] uninf;
  vector[Q] latent_slow;
  vector[Q] latent_fast;
  vector[Q] latent_clear;
  vector[Q] sym;
  vector[Q] diagnosed;
  vector[Q] recovered;
  vector[Q] susceptible;
  vector[Q] r_diagnosis;

  vector[Q] pop_check;

  real phi;
  real<lower=0,upper=1> init_ps = init_slow + init_fast + init_sym + init_diagnosed;
  real init_qs = 1.0 - init_ps;
  
  new_inf[1] = 0.0;
  r_diagnosis = rep_vector(r_sym_diag,Q);

  uninf[1] = pop_init * theta[1]*init_qs;
  latent_fast[1] = pop_init * init_fast;
  latent_slow[1] = pop_init * init_slow;
  latent_clear[1] = pop_init * theta[2]*init_qs;
  sym[1] = pop_init * init_sym;
  diagnosed[1] = pop_init * init_diagnosed;
  recovered[1] = pop_init * theta[3]*init_qs;

  new_diagnosed[1] = diagnosed[1]/2.0;
  pop_check[1] = uninf[1] + latent_fast[1] + latent_slow[1]+ latent_clear[1] + sym[1] + diagnosed[1] + recovered[1];
  
  susceptible[1] = uninf[1] + (recovered[1] + latent_clear[1] + latent_slow[1]) * prot_reinf;

  
  for(i in 2:Q){
   new_inf[i] = (sym[i-1]*susceptible[i-1] * (force+re_contact[i-1]))/pop[i-1];
  // transitions
  uninf[i] = pop[i-1]*xi[i] + uninf[i-1]*(1.0 - mu[i] - ((force+re_contact[i-1]) * sym[i-1])/pop[i-1]);

  latent_fast[i] = new_inf[i] + latent_fast[i-1] * (1.0 - mu[i] - r_fast_sym - r_fast_slow - r_fast_clear);
  
  latent_slow[i] = latent_fast[i-1] * r_fast_slow + latent_slow[i-1]*(1.0 - mu[i] - r_slow_sym - r_slow_clear -
          ((force+re_contact[i-1])*sym[i-1]*prot_reinf)/pop[i-1]);
  
  latent_clear[i] = latent_slow[i-1]*r_slow_clear + latent_fast[i-1]*r_fast_clear + 
        latent_clear[i-1]*(1.0 - mu[i] - ((force+re_contact[i-1])*sym[i-1]*prot_reinf)/pop[i-1]);
  
  sym[i] = latent_slow[i-1]*r_slow_sym + latent_fast[i-1] * r_fast_sym + diagnosed[i-1]*r_diag_sym + sym[i-1]*(1.0 - mu[i] - r_diagnosis[i] - r_sym_cure - r_sym_die);
  
  //treatment and recovery
  new_diagnosed[i] = sym[i-1] * r_diagnosis[i];
  diagnosed[i] = sym[i-1] * r_diagnosis[i] + diagnosed[i-1] * (1.0 - mu[i] - r_diag_sym - r_diag_cure - r_diag_die);
  
  recovered[i] = sym[i-1] * r_sym_cure + diagnosed[i-1] * r_diag_cure + recovered[i-1]*(1.0 - mu[i] - ((force+re_contact[i-1]) * sym[i-1]*prot_reinf)/pop[i-1]);
  susceptible[i] = uninf[i] + (recovered[i] + latent_clear[i] + latent_slow[i]) * prot_reinf;



  pop_check[i] = uninf[i] + latent_fast[i] + latent_slow[i] + latent_clear[i] + sym[i] + diagnosed[i] + recovered[i];
  }
  phi = pow(inv_sqrt_phi, -2.0);
}

model {
  inv_sqrt_phi ~ normal(0.0,1.0);
  force ~ normal(0.4,.5);

  re_contact ~ normal(0,re_sd);
  for(i in 1:Q){
    if(skip[i] == 0)
  target += neg_binomial_2_lpmf(TB_rep[i] | new_diagnosed[i], phi);
}
}

generated quantities{
  vector[Q] new_died_sym;
  vector[Q] new_died_diag;
  vector[Q] new_died;
  vector[Q] new_ltfu;
  vector[Q] new_sym;
  vector[Q] new_cured;
  // vector[Q] latent;
  
  // scenario 1 BMI, so progression (extend to array to test secondary)
  array[S] vector[H] proj_new_died_sym;
  array[S] vector[H] proj_new_died_diag;
  array[S] vector[H] proj_new_died;
  array[S] vector[H] proj_new_ltfu;
  array[S] vector[H] proj_new_sym;
  array[S] vector[H] proj_new_cured;

  array[S] vector[H] proj_new_inf;
  array[S] vector[H] proj_new_diagnosed;
  array[S] vector[H] proj_uninf;
  array[S] vector[H] proj_latent_slow;
  array[S] vector[H] proj_latent_fast;
  array[S] vector[H] proj_latent_clear;
  array[S] vector[H] proj_sym;
  array[S] vector[H] proj_diagnosed;
  array[S] vector[H] proj_recovered;
  array[S] vector[H] proj_susceptible;
  array[S] vector[H] proj_r_diagnosis;
  array[S] vector[H] proj_r_diag_sym;
  array[S] vector[H] proj_r_slow_sym;
  array[S] vector[H] proj_r_fast_sym;
  array[S] vector[H] proj_pop_check;
  
 // additional cases
 array[S] vector[H] proj_add_death;
 array[S] vector[H] proj_add_sym;
 array[S] vector[H] proj_add_death_cum;
 array[S] vector[H] proj_add_sym_cum;
 array[S] vector[H] proj_add_death_rel;
 array[S] vector[H] proj_add_sym_rel;
 array[S] vector[H] proj_add_death_cum_rel;
 array[S] vector[H] proj_add_sym_cum_rel;
// scenario 1: FOOD shock (BMI reduction). Sustained changes in the progression rates

// all

  for(s in 1:S){
  // scn1_new_inf[Q] = new_inf[Q];
  proj_r_slow_sym[s] = rep_vector(r_slow_sym, H);
  proj_r_fast_sym[s] = rep_vector(r_fast_sym, H);
  proj_r_diagnosis[s] = rep_vector(r_sym_diag, H);
  proj_r_diag_sym[s] = rep_vector(r_diag_sym, H);
  
  proj_r_slow_sym[s] = r_slow_sym * nutrition_wave(fi_effect_size[s], fi_time_start[s], fi_time_end[s], H);
  proj_r_fast_sym[s] = r_fast_sym * nutrition_wave(fi_effect_size[s], fi_time_start[s], fi_time_end[s], H);
    proj_r_diagnosis[s,2] = r_sym_diag * ha_effect_size[s];
    proj_r_diag_sym[s,2] = r_diag_sym * (1.0/ha_effect_size[s]);


    for(i in 1:H){
        proj_new_inf[s,1] = new_inf[Q];
  proj_uninf[s,1] = uninf[Q];
  proj_latent_fast[s,1] = latent_fast[Q];
  proj_latent_slow[s,1] = latent_slow[Q];
  proj_latent_clear[s,1] = latent_clear[Q];
  proj_sym[s,1] = sym[Q];
  proj_diagnosed[s,1] = diagnosed[Q];
  proj_recovered[s,1] = recovered[Q];

  proj_new_diagnosed[s,1] = new_diagnosed[Q];

      if(i ==1){
     proj_new_inf[s,i] = (sym[Q]* susceptible[Q] * force)/pop[Q+i-1];

  proj_uninf[s,i] = pop[Q+i-1]*xi[Q+i] + uninf[Q]*(1.0 - mu[Q+i] - (sym[Q] * force)/pop[Q+i-1]);
  
  proj_latent_fast[s,i] = new_inf[Q] + latent_fast[Q]*(1.0 - mu[Q+i] - proj_r_fast_sym[s,i] - r_fast_slow - r_fast_clear);

  proj_latent_slow[s, i] = latent_fast[Q] * r_fast_slow + latent_slow[Q]*(1.0-mu[Q+i] - proj_r_slow_sym[s,i] - r_slow_clear -
  (force*sym[Q]*prot_reinf)/pop[Q+i-1]);
  
  proj_latent_clear[s,i] = latent_slow[Q] * r_slow_clear + latent_fast[Q] * r_fast_clear + latent_clear[Q]*(1.0- mu[Q+i] - (force * sym[Q] * prot_reinf)/pop[Q+i-1]);
  
  proj_sym[s,i] = latent_slow[Q] * proj_r_slow_sym[s,i] + latent_fast[Q] * proj_r_fast_sym[s,i] +diagnosed[Q]*proj_r_diag_sym[s,i] +
         sym[Q] * (1.0 - mu[Q+i] - proj_r_diagnosis[s,i] - r_sym_cure - r_sym_die);
         
  proj_new_diagnosed[s,i] = sym[Q] * proj_r_diagnosis[s,i];

  proj_diagnosed[s,i] = sym[Q] * proj_r_diagnosis[s,i] + diagnosed[Q] *(1.0-mu[Q+i] - proj_r_diag_sym[s,i] - r_diag_die - r_diag_cure);

  proj_recovered[s,i] = diagnosed[Q] * r_diag_cure + sym[Q] * r_sym_cure + recovered[Q] * (1.0 - mu[Q+i] - (force * sym[Q] * prot_reinf)/pop[Q+i-1]);

  proj_susceptible[s,i] = proj_uninf[s,i] + (proj_recovered[s,i] + proj_latent_clear[s,i] + proj_latent_slow[s,i]) * prot_reinf;      
      } else {
      
      
   proj_new_inf[s,i] = (proj_sym[s,i-1]* proj_susceptible[s,i-1] * force)/pop[Q+i-1];

  proj_uninf[s,i] = pop[Q+i-1]*xi[Q+i] + proj_uninf[s,i-1]*(1.0 - mu[Q+i] - (proj_sym[s,i-1] * force)/pop[Q+i-1]);
  
  proj_latent_fast[s,i] = proj_new_inf[s,i-1] + proj_latent_fast[s,i-1]*(1.0 - mu[Q+i] - proj_r_fast_sym[s,i] - r_fast_slow - r_fast_clear);

  proj_latent_slow[s, i] = proj_latent_fast[s,i-1] * r_fast_slow + proj_latent_slow[s,i-1]*(1.0-mu[Q+i] - proj_r_slow_sym[s,i] - r_slow_clear -
  (force*proj_sym[s,i-1]*prot_reinf)/pop[Q+i-1]);
  
  proj_latent_clear[s,i] = proj_latent_slow[s,i-1] * r_slow_clear + proj_latent_fast[s,i-1] * r_fast_clear + proj_latent_clear[s,i-1]*(1.0- mu[Q+i] - (force * proj_sym[s,i-1] * prot_reinf)/pop[Q+i-1]);
  
  proj_sym[s,i] = proj_latent_slow[s,i-1] * proj_r_slow_sym[s,i] + proj_latent_fast[s,i-1] * proj_r_fast_sym[s,i] +proj_diagnosed[s,i-1]*proj_r_diag_sym[s,i] +
         proj_sym[s,i-1] * (1.0 - mu[Q+i] - proj_r_diagnosis[s,i] - r_sym_cure - r_sym_die);
         
  proj_new_diagnosed[s,i] = proj_sym[s,i-1] * proj_r_diagnosis[s,i];

  proj_diagnosed[s,i] = proj_sym[s,i-1] * proj_r_diagnosis[s,i] + proj_diagnosed[s,i-1] *(1.0-mu[Q+i] - proj_r_diag_sym[s,i] - r_diag_die - r_diag_cure);

  proj_recovered[s,i] = proj_diagnosed[s,i-1] * r_diag_cure + proj_sym[s,i-1] * r_sym_cure + proj_recovered[s,i-1] * (1.0 - mu[Q+i] - (force * proj_sym[s,i-1] * prot_reinf)/pop[Q+i-1]);

  proj_susceptible[s,i] = proj_uninf[s,i] + (proj_recovered[s,i] + proj_latent_clear[s,i] + proj_latent_slow[s,i]) * prot_reinf;


}
  proj_pop_check[s,i] = proj_uninf[s,i] + proj_latent_fast[s,i] + proj_latent_slow[s,i] + proj_latent_clear[s,i] + proj_sym[s,i] + proj_diagnosed[s,i] + proj_recovered[s,i];
}
  proj_new_died_sym[s] = proj_sym[s] * r_sym_die;
  proj_new_died_diag[s] = proj_diagnosed[s] * r_diag_die;
  proj_new_died[s] = proj_new_died_sym[s] + proj_new_died_diag[s];
  proj_new_cured[s] = proj_diagnosed[s] * r_diag_cure;
  proj_new_ltfu[s] = proj_diagnosed[s] .* proj_r_diag_sym[s];
  proj_new_sym[s] = proj_latent_fast[s] .* proj_r_fast_sym[s] + proj_latent_slow[s] .* proj_r_slow_sym[s];
 
  for(i in 1:H){
  proj_add_death[s,i] = (proj_new_died[s,i] - proj_new_died[1,i]);
  proj_add_death_rel[s,i] = (proj_add_death[s,i])/proj_new_died[1,i];
  proj_add_sym[s,i] = (proj_new_sym[s,i] - proj_new_sym[1,i]);
  proj_add_sym_rel[s,i] = (proj_add_sym[s,i])/proj_new_sym[1,i];
}
  proj_add_sym_cum[s] = cumulative_sum(proj_add_sym[s]);
  proj_add_death_cum[s] = cumulative_sum(proj_add_death[s]);
  proj_add_sym_cum_rel[s] = cumulative_sum(proj_add_sym_rel[s]);
  proj_add_death_cum_rel[s] = cumulative_sum(proj_add_death_rel[s]);
}

  // latent = latent_fast + latent_slow;
  new_died_sym = sym * r_sym_die;
  new_died_diag = diagnosed * r_diag_die;
  new_died = new_died_sym + new_died_diag;
  new_cured = diagnosed * r_diag_cure;
  new_ltfu = diagnosed * r_diag_sym;
  new_sym = latent_fast * r_fast_sym + latent_slow * r_slow_sym;
  
}



