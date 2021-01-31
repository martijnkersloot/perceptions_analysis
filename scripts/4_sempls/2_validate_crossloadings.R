crossloadings <- perceptions_model$crossloadings



crossloadings$max <- colnames(crossloadings)[max.col(crossloadings[,3:19])+2]
crossloadings$correct <- crossloadings$block == crossloadings$max

# Bleek dat facilitatingconditions_4 een hogere crossloading had voor structural_assurance
# I know where to find help when making my research data FAIR

