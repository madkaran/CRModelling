
#----------------------------------------------------------------
# 1. Modify variables
#----------------------------------------------------------------

# Employee length
#----------------------------------------------------------------
loan_data$emp_length_int<-gsub("< 1 year",0,loan_data$emp_length)
loan_data$emp_length_int<-gsub(" years","",loan_data$emp_length_int)
loan_data$emp_length_int<-gsub(" year","",loan_data$emp_length_int)
loan_data$emp_length_int<-gsub("\\+","",loan_data$emp_length_int)

loan_data$emp_length_int <- as.numeric(loan_data$emp_length_int)

#table(loan_data$emp_length_int)

# Term Variable
#----------------------------------------------------------------
#table(loan_data$term)

loan_data$term_int<-as.numeric(gsub(" months","",loan_data$term))

#table(loan_data$term_int)

# Earliest Credit Line
#----------------------------------------------------------------
loan_data$earliest_cr_line_date <- as.Date(paste("01",loan_data$earliest_cr_line), format = "%d %b-%y")

loan_data$mths_since_earliest_cr_line<-
  12*as.period(interval(ymd(loan_data$earliest_cr_line_date),ymd("2017-12-01")))$year+
  as.period(interval(ymd(loan_data$earliest_cr_line_date),ymd("2017-12-01")))$month


#View(loan_data[,c("earliest_cr_line_date","mths_since_earliest_cr_line")])

loan_data$mths_since_earliest_cr_line <- ifelse(loan_data$mths_since_earliest_cr_line<0,
                                                max(loan_data$mths_since_earliest_cr_line,na.rm = TRUE),
                                                loan_data$mths_since_earliest_cr_line)


# Issue Date
#----------------------------------------------------------------
loan_data$issue_date<-as.Date(paste("01",loan_data$issue_d), format = "%d %b-%y")
#table(loan_data$issue_date)

loan_data$mths_since_issue_d<-
  12*as.period(interval(ymd(loan_data$issue_date),ymd("2017-12-01")))$year+
  as.period(interval(ymd(loan_data$issue_date),ymd("2017-12-01")))$month

#----------------------------------------------------------------
# 2. Pre-Processing Discrete Variables
#----------------------------------------------------------------

# Making Dummies or discrete variables
loan_data_dummies <- dummy_cols(loan_data,select_columns = c("grade","sub_grade",
                                                             "home_ownership",
                                                             "verification_status",
                                                             "loan_status",
                                                             "purpose", "addr_state",
                                                             "initial_list_status"), split = "_")

#----------------------------------------------------------------
# 3. Missing Variables
#----------------------------------------------------------------

# Summary of counts of missing variables
sapply(loan_data_dummies, function(x) sum(is.na(x)))

# Funded_Amount is Replaced with Total__revolving_Hi_Limit
# Annual Income replaced with mean value of annual income
#-------------------------------------------------------------------------------


loan_data_dummies <- loan_data_dummies %>%
  dplyr::mutate(funded_amnt=if_else(is.na(funded_amnt),total_rev_hi_lim,funded_amnt)) %>%
  dplyr::mutate(annual_inc=if_else(is.na(annual_inc),
                                   mean(annual_inc,na.rm=TRUE),
                                   annual_inc)) %>%
  dplyr::mutate(mths_since_earliest_cr_line=if_else(is.na(mths_since_earliest_cr_line),
                                                    0,mths_since_earliest_cr_line)) %>%
  dplyr::mutate(acc_now_delinq=if_else(is.na(acc_now_delinq),
                                                    0,acc_now_delinq)) %>%
  dplyr::mutate(total_acc=if_else(is.na(total_acc),
                                       0,total_acc)) %>%
  dplyr::mutate(pub_rec=if_else(is.na(pub_rec),
                                  0,pub_rec)) %>%
  dplyr::mutate(open_acc=if_else(is.na(open_acc),
                                  0,open_acc)) %>%
  dplyr::mutate(inq_last_6mths=if_else(is.na(inq_last_6mths),
                                 0,inq_last_6mths)) %>%
  dplyr::mutate(delinq_2yrs=if_else(is.na(delinq_2yrs),
                                       0,delinq_2yrs)) %>%
  dplyr::mutate(emp_length_int=if_else(is.na(emp_length_int),
                                    0,emp_length_int))

return(loan_data_dummies)
  


  




















