email_outcomes <- subset(data_raw, 
                         data_raw$`email_options#I_would_like_to_be_informed_on_the_outcomes_o0`== 1,
                         select=c("Castor Record ID", "hospital_department_1", "email"))

email_other_research <- subset(data_raw, 
                               data_raw$`email_options#I_would_like_to_participate_in_other_research0` == 1,
                               select=c("Castor Record ID", "hospital_department_1", "email"))

email_other_research$domain <- str_split_fixed(email_other_research$email, "@", n = 2)[,2]

email_castor_fair <- subset(data_raw, 
                            data_raw$`email_options#I_would_like_to_be_informed_on_Castors_approa0` == 1,
                            select=c("Castor Record ID", "hospital_department_1", "email"))

email_raffle <- subset(data_raw, 
                            data_raw$`raffle#Yes_I_want_to_participate_in_the_raffle` == 1,
                            select=c("Castor Record ID", "hospital_department_1", "email"))


set.seed(2020)

winners <- sample(email_raffle$email, 10)
