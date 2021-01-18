# Table
# Composite reliability measures of latent variables with more than one observed variable

# perceptions_fit_rho <- as.data.frame(dgrho(perceptions_fit))
# colnames(perceptions_fit_rho) <- c("rho", "mvs")
# perceptions_fit_rho <- subset(perceptions_fit_rho, perceptions_fit_rho$mvs > 1)
# perceptions_fit_rho$rho <- format(perceptions_fit_rho$rho, digits = 3)
# perceptions_fit_rho$variable <- structural_model_names[, 1][match(rownames(perceptions_fit_rho), rownames(structural_model_names))]
# perceptions_fit_rho <- perceptions_fit_rho[c("variable", "mvs", "rho")]
# rownames(perceptions_fit_rho) <- NULL
# 
# perceptions_fit_rho <- perceptions_fit_rho[order(perceptions_fit_rho$rho, decreasing=TRUE),]
# 
# kable(perceptions_fit_rho,
#       col.names = c("", "# of observed variables", "Dillon-Goldstein’s rho"),
#       row.names = FALSE
# ) %>%
#   kable_styling()
# 


perceptions_fit_rho <- as.data.frame(perceptions_model$unidim[,c(2, 4)])
colnames(perceptions_fit_rho) <- c("mvs", "rho")
perceptions_fit_rho <- subset(perceptions_fit_rho, perceptions_fit_rho$mvs > 1)
perceptions_fit_rho$rho <- format(perceptions_fit_rho$rho, digits = 3)
perceptions_fit_rho$variable <- perceptions_structural_model_names[, 1][match(rownames(perceptions_fit_rho), rownames(perceptions_structural_model_names))]
perceptions_fit_rho <- perceptions_fit_rho[c("variable", "mvs", "rho")]
rownames(perceptions_fit_rho) <- NULL

perceptions_fit_rho <- perceptions_fit_rho[order(perceptions_fit_rho$rho, decreasing=TRUE),]

kable(perceptions_fit_rho,
      col.names = c("", "# of observed variables", "Dillon-Goldstein’s rho"),
      row.names = FALSE
) %>%
  kable_styling()
