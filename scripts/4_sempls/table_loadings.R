# Table
# All observed variables, their latent variable, and their loadings

perceptions_fit_loadings <- as.data.frame(perceptions_model$outer_model[,c(1, 2, 4)])

# perceptions_fit_loadings$observed <- rownames(perceptions_fit_loadings)
# perceptions_fit_loadings <- melt(perceptions_fit_loadings, id=c("observed"))
perceptions_fit_loadings$observed_num <- parse_number(as.character(perceptions_fit_loadings$name))
colnames(perceptions_fit_loadings) <- c("observed", "variable", "loading", "observed_num")

perceptions_fit_loadings <- subset(perceptions_fit_loadings, perceptions_fit_loadings$loading < 1.000)
rownames(perceptions_fit_loadings) <- NULL
perceptions_fit_loadings$variable <- perceptions_structural_model_names[, 1][match(perceptions_fit_loadings$variable, rownames(perceptions_structural_model_names))]
perceptions_fit_loadings <- perceptions_fit_loadings[order(perceptions_fit_loadings$variable, perceptions_fit_loadings$observed_num),]
rownames(perceptions_fit_loadings) <- NULL

perceptions_fit_loadings$loading <- format(perceptions_fit_loadings$loading, digits = 3)
perceptions_fit_loadings$loading <- parse_number(as.character(perceptions_fit_loadings$loading))

perceptions_fit_loadings$observed <- paste(tocamel(perceptions_fit_loadings$variable), perceptions_fit_loadings$observed_num, sep="")
perceptions_fit_loadings$observed_num <- NULL


# Loadings in bold cells satisfy the prescribed threshold (>= 0.708).
perceptions_fit_loadings$loading = cell_spec(perceptions_fit_loadings$loading, format="html", bold = perceptions_fit_loadings$loading >= 0.708)

print(
  kable(perceptions_fit_loadings,
      col.names = c("Observed variable", "Latent variable", "Loading"),
      row.names = FALSE,
      escape = FALSE,
      
      # format="latex", booktabs = T
) %>%
  kable_styling()
)
