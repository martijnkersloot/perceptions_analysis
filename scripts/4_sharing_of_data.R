data_sharing <- data_raw[,2]

data_raw$share_methods_sum <-
  data_raw$`share_methods#Yes_using_a_USB_Flash_drive` +
  data_raw$`share_methods#Yes_using_email` +
  data_raw$`share_methods#Yes_as_a_standalone_data_publication_in_a_dat0` +
  data_raw$`share_methods#Yes_in_a_data_repository_of_my_organization` +
  data_raw$`share_methods#Yes_in_an_external_data_repository_eg_Figshar0` +
  data_raw$`share_methods#Yes_as_an_appendix_or_online_supplementary_in0` +
  data_raw$`share_methods#Yes_using_a_shared_network_drive` +
  data_raw$`share_methods#Yes_using_cloud_storage_eg_Dropbox_or_Google_0` +
  data_raw$`share_methods#Yes_in_another_way`

data_raw$shared_yn <- NA
data_raw$shared_yn[data_raw$`share_methods#No` == 1] <- 0
data_raw$shared_yn[data_raw$share_methods_sum > 0] <- 1

# Exclude don't know / not sure
data_raw$shared_yn[data_raw$`share_methods#Dont_knowNot_sure` == 1] <- NA

# data_raw$shared_yn[data_raw$`share_methods#Dont_knowNot_sure` == 1] <- 99

data_sharing$shared <- factor(
  data_raw$shared_yn,
  levels = c(1, 0, 99),
  labels = c("Yes",
             "No",
             "Don't know / Not sure")
)

data_sharing$method_usb_flash_drive <- factor(
  data_raw$`share_methods#Yes_using_a_USB_Flash_drive`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_email <- factor(
  data_raw$`share_methods#Yes_using_email`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_standalone <- factor(
  data_raw$`share_methods#Yes_as_a_standalone_data_publication_in_a_dat0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_data_repository_organization <- factor(
  data_raw$`share_methods#Yes_in_a_data_repository_of_my_organization`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_data_repository_external <- factor(
  data_raw$`share_methods#Yes_in_an_external_data_repository_eg_Figshar0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_appendix <- factor(
  data_raw$`share_methods#Yes_as_an_appendix_or_online_supplementary_in0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_network_drive <- factor(
  data_raw$`share_methods#Yes_using_a_shared_network_drive`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_cloud_storage <- factor(
  data_raw$`share_methods#Yes_using_cloud_storage_eg_Dropbox_or_Google_0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_other <- factor(
  data_raw$`share_methods#Yes_in_another_way`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$method_other_text <- data_raw$share_methods_other

#
# Data sharing with
#

data_sharing$with_same_project <- factor(
  data_raw$`share_whom#Researchers_on_the_same_research_project_with_wh0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$with_personally <- factor(
  data_raw$`share_whom#Researchers____not____working_on_the_same_resear0`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$with_not_personally <- factor(
  data_raw$`share_whom#Researchers____not____working_on_the_same_resear1`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$with_project_partners <- factor(
  data_raw$`share_whom#Research_project_partners_eg_funders`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$with_other <- factor(
  data_raw$`share_whom#Other`,
  levels = c(1, 0),
  labels = c("Yes",
             "No")
)

data_sharing$with_other_text <- data_raw$share_whom_other

data_sharing$method_usb_flash_drive[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_email[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_standalone[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_data_repository_organization[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_data_repository_external[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_appendix[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_network_drive[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_cloud_storage[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_other[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$method_other_text[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA


data_sharing$with_same_project[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$with_personally[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$with_not_personally[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$with_project_partners[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$with_other[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA
data_sharing$with_other_text[data_sharing$shared != "Yes" | is.na(data_sharing$shared)] <- NA


attr(data_sharing$shared, "label") <- "Shares research data"
attr(data_sharing$method_usb_flash_drive, "label") <- "Using a USB Flash drive"
attr(data_sharing$method_email, "label") <- "Using email"
attr(data_sharing$method_standalone, "label") <- "As a stand-alone data publication in a data journal"
attr(data_sharing$method_data_repository_organization, "label") <- "In a data repository of my organization"
attr(data_sharing$method_data_repository_external, "label") <- "In an external data repository"
attr(data_sharing$method_appendix, "label") <- "As appendix/supplementary information in a scientific publication"
attr(data_sharing$method_network_drive, "label") <- "Using a shared network drive"
attr(data_sharing$method_cloud_storage, "label") <- "Using cloud storage"
attr(data_sharing$method_other, "label") <- "Other"


attr(data_sharing$with_same_project, "label") <- "Researchers on the same research project with whom I collaborate"
attr(data_sharing$with_personally, "label") <- "Researchers not working on the same research project but whom I know personally"
attr(data_sharing$with_not_personally, "label") <- "Researchers not working on the same research project whom I do not know personally"
attr(data_sharing$with_project_partners, "label") <- "Research project partners"
attr(data_sharing$with_other, "label") <- "Other"

table4_shares <- descrTable( ~ shared,
                             data_sharing,
                             hide.no = "No")

table4_method <- descrTable(
  ~  method_usb_flash_drive +
    method_email +
    method_standalone +
    method_data_repository_organization +
    method_data_repository_external +
    method_appendix +
    method_network_drive +
    method_cloud_storage +
    method_other,
  data_sharing,
  hide.no = "No"
)

table4_with <- descrTable(
  ~  with_same_project +
    with_personally +
    with_not_personally +
    with_project_partners +
    with_other,
  data_sharing,
  hide.no = "No"
)


# share_methods_other
table(data_sharing$method_other_text)

# share_methods
table(data_sharing$with_other_text)

