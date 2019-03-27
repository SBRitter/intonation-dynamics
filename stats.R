library(rstudioapi)
library(brms)

current_dir = dirname(getActiveDocumentContext()$path)
setwd(current_dir)
data = read.csv("onglide_data.csv", sep=",")
df = subset(data, focus_type != "background")
df$focus_type = droplevels(df$focus_type)
df = na.omit(df)
df$focus_type = factor(df$focus_type, levels(factor(df$focus_type))[c(1, 3, 2)])

build_model = function(data) {
  
  df = data

  mix <- mixture(gaussian, gaussian)
  prior_onglide <- c(
    prior(normal(-0.5, .1), class = Intercept, dpar = mu1),
    prior(normal(0.5, .1), class = Intercept, dpar = mu2),
    prior(normal(0, .1), class = b, dpar = mu1),
    prior(normal(0, .1), class = b, dpar = mu2),
    prior(normal(0, .05), class = sd, dpar = mu1),
    prior(normal(0, .05), class = sd, dpar = mu2),
    prior(normal(0, 1), class = b, dpar = theta2)
  )
  
  # Fit the model
  # Estimate both mu1 and mu2 as well as theta2
  xmdl_onglide = brm(bf(onglide_log ~ focus_type +
                          (1+focus_type|speaker) +
                          (1|target_word),
                        theta2 ~ focus_type +
                          (1+focus_type|speaker) +
                          (1|target_word)),
                     data = df,
                     family = mix,
                     prior = prior_onglide,
                     iter = 3000,
                     inits = 0,
                     chains = 4,
                     control = list(adapt_delta = 0.9999,
                                    max_treedepth = 15,
                                    stepsize=.001)
                     )
  return(xmdl_onglide)
}

summarise_mu2 = function(psamples) {
  
  # Calculate difference between focus types for mu2, i.e. "rising" mode
  broad_mu2 = psamples$b_mu2_Intercept
  narrow_mu2 = psamples$b_mu2_Intercept + psamples$b_mu2_focus_typenarrow
  contrastive_mu2 = psamples$b_mu2_Intercept + psamples$b_mu2_focus_typecontrastive
  processed_psamples_mu2 = data.frame(broad_mu2,narrow_mu2,contrastive_mu2)
  
  # Difference between broad and narrow
  diff_br_na_mu2 = processed_psamples_mu2$narrow_mu2 - processed_psamples_mu2$broad_mu2
  
  # Difference between narrow and contrastive
  diff_na_co_mu2 = processed_psamples_mu2$contrastive_mu2 - processed_psamples_mu2$narrow_mu2
  
  # Difference between broad and contrastive
  diff_br_co_mu2 = processed_psamples_mu2$contrastive_mu2 - processed_psamples_mu2$broad_mu2
  
  # Extract the high and low boundaries of the 95% confidence interval of the 
  # estimated posterior distributions for the differences and the probability 
  # that the estimated differences are greater than zero.
  lci = round(c(coda::HPDinterval(as.mcmc(diff_br_na_mu2))[1],
          coda::HPDinterval(as.mcmc(diff_na_co_mu2))[1],
          coda::HPDinterval(as.mcmc(diff_br_co_mu2))[1]),2)
  hci = round(c(coda::HPDinterval(as.mcmc(diff_br_na_mu2))[2],
          coda::HPDinterval(as.mcmc(diff_na_co_mu2))[2],
          coda::HPDinterval(as.mcmc(diff_br_co_mu2))[2]),2)
  p_zero = round(c(mean(diff_br_na_mu2>0),
             mean(diff_na_co_mu2>0),
             mean(diff_br_co_mu2>0)),2)
  estimate = round(c(mean(diff_br_na_mu2),
               mean(diff_na_co_mu2),
               mean(diff_br_co_mu2)),2)
  diff = c("br vs. na",
           "na vs. co",
           "br vs. co")
  
  # Put everything into a summary table
  mu2_summary = data.frame(diff,estimate,lci,hci,p_zero)

  return(mu2_summary)
}

summarise_theta2 = function(psamples) {
  
  # Calculate difference between focus types for theta, i.e. mixture parameter of the model
  broad_theta2 = psamples$b_theta2_Intercept
  narrow_theta2 = psamples$b_theta2_Intercept + psamples$b_theta2_focus_typenarrow
  contrastive_theta2 = psamples$b_theta2_Intercept + psamples$b_theta2_focus_typecontrastive
  processed_psamples_theta2 = data.frame(broad_theta2,narrow_theta2,contrastive_theta2)
  
  # Difference between broad and narrow
  diff_br_na_theta2 = processed_psamples_theta2$narrow_theta2 - processed_psamples_theta2$broad_theta2
  
  # Difference between narrow and contrastive
  diff_na_co_theta2 = processed_psamples_theta2$contrastive_theta2 - processed_psamples_theta2$narrow_theta2
  
  # Difference between broad and contrastive
  diff_br_co_theta2 = processed_psamples_theta2$contrastive_theta2 - processed_psamples_theta2$broad_theta2
  
  # Extract the high and low boundaries of the 95% confidence interval of the 
  # estimated posterior distributions for the differences and the probability 
  # that the estimated differences are greater than zero.
  lci = round(c(coda::HPDinterval(as.mcmc(diff_br_na_theta2))[1],
          coda::HPDinterval(as.mcmc(diff_na_co_theta2))[1],
          coda::HPDinterval(as.mcmc(diff_br_co_theta2))[1]),2)
  hci = round(c(coda::HPDinterval(as.mcmc(diff_br_na_theta2))[2],
          coda::HPDinterval(as.mcmc(diff_na_co_theta2))[2],
          coda::HPDinterval(as.mcmc(diff_br_co_theta2))[2]),2)
  p_zero = round(c(mean(diff_br_na_theta2>0),
             mean(diff_na_co_theta2>0),
             mean(diff_br_co_theta2>0)),2)
  estimate = round(c(mean(diff_br_na_theta2),
               mean(diff_na_co_theta2),
               mean(diff_br_co_theta2)),2)
  diff = c("br vs. na",
           "na vs. co",
           "br vs. co")
  
  # Put everything into a summary table
  theta2_summary = data.frame(diff,estimate,lci,hci,p_zero)
  
  return(theta2_summary)

}

### Model for all speakers
#xmdl_onglide = build_model(df)
#save(xmdl_onglide, file = "onglide_model.RData")
load("onglide_model.RData")
pp_check(xmdl_onglide)
summary(xmdl_onglide)
psamples_all = posterior_samples(xmdl_onglide)
mu2_summary = summarise_mu2(psamples_all)
theta2_summary = summarise_theta2(psamples_all)
mu2_summary
theta2_summary

# Divide speakers in two groups (same as in the modelling script)
group = c()
for (speaker in unique(df$speaker)) {
  falls = sum(df[df$speaker == speaker, ]$onglide < 0, na.rm=T)
  all = nrow(df[df$speaker == speaker,])
  threshold = all*0.33
  if (falls > threshold) {
    group = c(group, rep("1", nrow(df[df$speaker==speaker,])))
  } else {
    group = c(group, rep("2", nrow(df[df$speaker==speaker,])))
  }
}
df$group = group
gr1 = droplevels(subset(df, group=="1"))
gr2 = droplevels(subset(df, group=="2"))

### Model for group1
#xmdl_onglide_gr1 = build_model(gr1)
#save(xmdl_onglide_gr1, file = "onglide_model_gr1.RData")
load("onglide_model_gr1.RData")
pp_check(xmdl_onglide_gr1)
summary(xmdl_onglide_gr1)
psamples_gr1 = posterior_samples(xmdl_onglide_gr1)
mu2_summary_gr1 = summarise_mu2(psamples_gr1)
theta2_summary_gr1 = summarise_theta2(psamples_gr1)
mu2_summary_gr1
theta2_summary_gr1

### Model for group2
#xmdl_onglide_gr2 = build_model(gr2)
#save(xmdl_onglide_gr2, file = "onglide_model_gr2.RData")
load("onglide_model_gr2.RData")
pp_check(xmdl_onglide_gr2)
summary(xmdl_onglide_gr2)
psamples_gr2 = posterior_samples(xmdl_onglide_gr2)
mu2_summary_gr2 = summarise_mu2(psamples_gr2)
theta2_summary_gr2 = summarise_theta2(psamples_gr2)
mu2_summary_gr2
theta2_summary_gr2

