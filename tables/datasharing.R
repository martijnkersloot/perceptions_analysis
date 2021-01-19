# Table 2: Data sharing

table_sharing_shares <- descrTable( ~ shared,
                             data_sharing,
                             hide.no = "No")

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
  data_sharing,
  hide.no = "No"
)

table_sharing_with <- descrTable(
  ~  with_same_project +
    with_personally +
    with_not_personally +
    with_project_partners +
    with_other,
  data_sharing,
  hide.no = "No"
)

export2md(rbind(
  "Shares research data" = table_sharing_shares,
  "Data sharing methods" = table_sharing_method,
  "Data sharing with" = table_sharing_with
))


rm(table_sharing_shares, table_sharing_method, table_sharing_with)