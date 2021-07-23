# Latent variables, mean, sd, and Average Variance Extracted (AVE)

perceptions_fit_ave <- as.data.frame(cbind(
  rownames(perceptions_model$inner_summary),
  perceptions_model$inner_summary[,5]
))

colnames(perceptions_fit_ave) <- c("variable", "ave")
perceptions_fit_ave$ave <- format(as.numeric(perceptions_fit_ave$ave), digits = 3)

#AVE & R2
perceptions_model$inner_summary[,5]


#plspm.groups(perceptions_model, data_plspm$fair_knowledge, method="permutation")


perceptions_fit_r2 <- as.data.frame(cbind(
  rownames(perceptions_model$inner_summary),
  format(as.numeric(perceptions_model$inner_summary[,2]), digits = 2)
))

