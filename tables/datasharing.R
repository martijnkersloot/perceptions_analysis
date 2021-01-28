# Table 2: Data sharing

data_sharing_researchers <- subset(data_sharing, data_sharing$profession_group == "Researcher")

table_sharing_shares <- descrTable( ~ shared,
                                    data_sharing_researchers,
                             hide.no = "No", show.n = FALSE)

table_sharing_method <- descrTable(
  ~  method_usb_flash_drive +
    method_email +
    method_standalone +
    method_data_repository_organization +
    method_data_repository_external +
    method_appendix +
    method_network_drive +
    method_cloud_storage +
    method_other,
  data_sharing_researchers,
  hide.no = "No", show.n = FALSE
)

table_sharing_with <- descrTable(
  ~  with_same_project +
    with_personally +
    with_not_personally +
    with_project_partners +
    with_other,
  data_sharing_researchers,
  hide.no = "No", show.n = FALSE
)

export2md(rbind(
  "Shares research data" = table_sharing_shares,
  "Data sharing methods" = table_sharing_method,
  "Data sharing with" = table_sharing_with
))

export2md(rbind(
  "Shares research data" = table_sharing_shares,
  "Data sharing methods" = table_sharing_method,
  "Data sharing with" = table_sharing_with
), format="latex")



#rm(table_sharing_shares, table_sharing_method, table_sharing_with)