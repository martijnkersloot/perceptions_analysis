date <- "20210129"

# --------
# 0. Data preparation
# --------
source('scripts/0_preparation/0_helpers.R')
source('scripts/0_preparation/1_load_data.R')
source('scripts/0_preparation/2_progress.R')

source('scripts/0_preparation/figure_progress.R')


# --------
# 1. Demographics
source('scripts/1_demographics/0_demographics.R')

source('scripts/1_demographics/table.R')


# --------
# 2. Data sharing
# --------
source('scripts/2_data-sharing/0_sharing_of_data.R')

source('scripts/2_data-sharing/table.R')


# --------
# 3. Research Data Management (RDM) policy
# --------
source('scripts/3_rdm-policy/0_rdm.R')

source('scripts/3_rdm-policy/table.R')

# --------
# 4. Partial Least Squares Path Modeling (PLS-PM)
# --------
source('scripts/4_sempls/0_plspm.R')
source('scripts/4_sempls/1_coeffs.R')
source('scripts/4_sempls/2_validate_crossloadings.R')

source('scripts/4_sempls/table_ave.R')
source('scripts/4_sempls/table_rho.R')
source('scripts/4_sempls/table_loadings.R')


# --------
# 5. FAIR
# --------
source('scripts/5_fair/0_fair.R')
source('scripts/5_fair/1_scores.R')

source('scripts/5_fair/figure_effort.R')


# --------
# 9. Contact for future research (emails)
# --------
source('scripts/9_future-research/0_emails.R')
