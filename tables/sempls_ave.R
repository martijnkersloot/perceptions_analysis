# Latent variables, mean, sd, and Average Variance Extracted (AVE)

#perceptions_fit_loadings <- as.data.frame(perceptions_fit$)


perceptions_fit

perceptions_fit_ave <- as.data.frame(cbind(
  rownames(perceptions_model$inner_summary),
  perceptions_model$inner_summary[,5]
))


perceptions_model$inner_model


#AVE & R2
perceptions_model$inner_summary[,5]


#plspm.groups(perceptions_model, data_plspm$fair_knowledge, method="permutation")
