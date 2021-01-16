library(reshape)
library(scales)


perceptions_fit_effects <- perceptions_model$effects

# perceptions_fit_path_coefs <- as.data.frame(perceptions_model$path_coefs)
# 
# perceptions_fit_path_coefs$from <- rownames(perceptions_fit_path_coefs)
# perceptions_fit_path_coefs <- melt(perceptions_fit_path_coefs, id="from")
# colnames(perceptions_fit_path_coefs) <- c("to", "from", "coefficient")
# perceptions_fit_path_coefs <- perceptions_fit_path_coefs[c("from", "to", "coefficient")]
# 
# 
# perceptions_fit_path_coefs <- subset(perceptions_fit_path_coefs, perceptions_fit_path_coefs$coefficient != 0)


perceptions_fit_path_coefs <- data.frame(from=character(),
                               to=character(), 
                               coefficient=numeric(), 
                               p=numeric()) 

for (i in 1:length(perceptions_model$inner_model)) {
  to_name <- name(perceptions_model$inner_model[i])
  to <- as.data.frame(perceptions_model$inner_model[i])
  
  
  for(j in 1:nrow(to)) {
    from <- to[j,]
    from_name <- rownames(from)
    
    if(from_name != "Intercept") {
      coeff <- from[1, 1]
      p <- from[1, 4]
      
      perceptions_fit_path_coefs <- rbind(perceptions_fit_path_coefs, c(from_name, to_name, coeff, p))
    }
  }
}

colnames(perceptions_fit_path_coefs) <- c("from", "to", "coefficient", "p")

#options(scipen = 9999)

perceptions_fit_path_coefs$coefficient <- format(as.numeric(perceptions_fit_path_coefs$coefficient), digits = 3)
perceptions_fit_path_coefs$p <- as.numeric(perceptions_fit_path_coefs$p)

perceptions_fit_path_coefs$p_star <- symnum(
  perceptions_fit_path_coefs$p,
  corr = FALSE,
  na = FALSE,
  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("***", "**", "*", ".", " ")
)

perceptions_fit_path_coefs$p <- scales::pvalue(as.numeric(perceptions_fit_path_coefs$p))


#psycho::format_p(as.numeric(perceptions_fit_path_coefs$p))
