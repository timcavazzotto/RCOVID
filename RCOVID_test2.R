#Algorithm detector of sar-cov2 reinfection in routine testing database
#Algorithm created by Timothy Cavazzotto 
#Started 22/10/2022 final version at: 08/06/2022


#### Packages #########
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

# READ DATA
data <- read_excel("Data_validation.xlsx" , sheet = "test2" )


# sars_cov_result list - 
data$exam_result<-0
data$exam_result[data$sars_cov_result== "sars-cov2"]<-1
data$exam_result[data$sars_cov_result== "positive"]<-1


# type_exam list
data$valid_exam<-0
data$valid_exam[data$type_exam== "Celer Sansure Kit de Detecção por PCR em Tempo Real para SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "COBAS SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "COVID-19 Ag ECO Teste"]<-1
data$valid_exam[data$type_exam== "COVID-19 Ag Rapid Test Device - Panbio"]<-1
data$valid_exam[data$type_exam== "COVID-19 Real-Time PCR Kit"]<-1
data$valid_exam[data$type_exam== "COVID-19, Biologia Molecular"]<-1
data$valid_exam[data$type_exam== "DETECT SARS-CoV-2 RT-PCR"]<-1
data$valid_exam[data$type_exam== "Diagnostic Kit for Novel-Coronavirus(2019-nCoV) RNA"]<-1
data$valid_exam[data$type_exam== "Diagnóstico Molecular CORONÁVIRUS COVID-19"]<-1
data$valid_exam[data$type_exam== "ECO F COVID-19 Ag"]<-1
data$valid_exam[data$type_exam== "EURORealTime SARS-CoV-2	RT-PCR"]<-1
data$valid_exam[data$type_exam== "Família Abbott RealTime SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "Família Abbott RealTime SARS-CoV-2 EUA"]<-1
data$valid_exam[data$type_exam== "FAMÍLIA BIO GENE COVID-19 PCR"]<-1
data$valid_exam[data$type_exam== "Família cobas SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "Família Kit de Detecção por PCR em Tempo Real VIASURE SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "FAMÍLIA KIT XGEN MASTER COVID-19"]<-1
data$valid_exam[data$type_exam== "GeneFinderTM COVID-19 PLUS RealAmp Kit"]<-1
data$valid_exam[data$type_exam== "Kit MOLECULAR SARS-CoV2 (E/P1)"]<-1
data$valid_exam[data$type_exam== "Kit Molecular SARS-CoV2 (E/RP)"]<-1
data$valid_exam[data$type_exam== "KIT XGEN MASTER COVID-19"]<-1
data$valid_exam[data$type_exam== "Novel Coronavirus (2019-nCoV) Nucleic Acid Detection Kit PCR-Fluorescence Probing"]<-1
data$valid_exam[data$type_exam== "Panbio COVID-19 Ag Rapid Test"]<-1
data$valid_exam[data$type_exam== "RealStar® SARS-CoV-2 RT-PCR Kit 1.0"]<-1
data$valid_exam[data$type_exam== "SARS-COV-2 RT-PCR KIT"]<-1
data$valid_exam[data$type_exam== "SARS-CoV-2 S gene for BD Max"]<-1
data$valid_exam[data$type_exam== "TaqPath™ COVID-19 CE-IVD RT PCR Kit"]<-1
data$valid_exam[data$type_exam== "Teste Molecular (RT-PCR)"]<-1
data$valid_exam[data$type_exam== "Teste Rápido (Molecular)"]<-1
data$valid_exam[data$type_exam== "Teste Rápido Antígeno (Imunocromatografia)"]<-1
data$valid_exam[data$type_exam== "Teste Rápido Antígeno (Imunofluorescência)"]<-1
data$valid_exam[data$type_exam== "TR COVID-19 Ag - Bio-Manguinhos"]<-1
data$valid_exam[data$type_exam== "TR DPP® COVID-19 AG- Bio-Manguinhos"]<-1
data$valid_exam[data$type_exam== "TR SARS COV 2 AG - Bio-Manguinhos"]<-1
data$valid_exam[data$type_exam== "Xpert Xpress SARS-CoV-2"]<-1
data$valid_exam[data$type_exam== "COVID-19 Ag Rapid Test Device - Panbio"] <-1
data$valid_exam[data$type_exam== "COVID-19 Real-Time PCR Kit"] <-1
data$valid_exam[data$type_exam== "COVID-19, Biologia Molecular"] <-1
data$valid_exam[data$type_exam== "ECO F COVID-19 Ag"] <-1
data$valid_exam[data$type_exam== "Panbio COVID-19 Ag Rapid Test"] <-1
data$valid_exam[data$type_exam== "Teste Molecular (RT-PCR)"] <-1
data$valid_exam[data$type_exam== "Teste Rápido (Molecular)"] <-1
data$valid_exam[data$type_exam== "Teste Rápido Antígeno (Imunocromatografia)"] <-1
data$valid_exam[data$type_exam== "Teste Rápido Antígeno (Imunofluorescência)"] <-1
data$valid_exam[data$type_exam== "TR COVID-19 Ag - Bio-Manguinhos"] <-1
data$valid_exam[data$type_exam== "TR COVID-19 AG - IBMP"] <-1
data$valid_exam[data$type_exam== "TR DPP® COVID-19 AG- Bio-Manguinhos"] <-1
data$valid_exam[data$type_exam== "TR SARS COV 2 AG - Bio-Manguinhos"] <-1



# creating classes of valid cases
data$exam_result<-as.numeric(data$exam_result)
data$valid_exam<-as.numeric(data$valid_exam)
data$valid_covid_positive<-data$exam_result + data$valid_exam
data$valid_covid_positive<-ifelse(data$valid_covid_positive==2,1,0)

# classifying date type
data$date_sample<-as.Date(data$date_sample, 
                          format = "%d/%m/%Y") #change according to your database



#Creating the wave variable
data$wave<-0
data <- data %>%
  arrange(date_sample) %>%
  mutate(
    wave=ifelse(
      date_sample %within% interval(as.Date("2020-03-11"), 
                                    as.Date("2020-10-12")), 1,
      ifelse(
        date_sample %within% interval(as.Date("2020-10-17"),
                                      as.Date("2021-12-27")), 2,
        
        ifelse(
          date_sample %within% interval(as.Date("2021-12-26"),
                                        as.Date("2023-03-18")),3,wave
        ))))


#### DECORA ########

#create variable with order of exam
data <- data[order(data$doc, data$date_sample),] #order by date
data$order_sample<-1
data <- data %>%
  group_by(doc) %>%
  arrange(doc) %>%
  mutate(
    order_sample=cumsum(order_sample)
  )

#create variable with infection order for separate cases by waves
data$order_infec<-ifelse(data$wave==1,
                         data$exam_result,
                         data$valid_covid_positive)


table(data$order_infec)

#create variable with infection order
data<-data %>%
  group_by(doc) %>%
  arrange(doc, order_sample) %>%
  mutate(
    order_infec = ifelse(valid_covid_positive==1, 
                         cumsum(order_infec), order_infec),
    infect_m1 = if_else(order_infec>0, 1, 0))

table(data$order_infec)


#create difference between date of infections in days
data<-data %>%
  group_by(doc,infect_m1) %>%
  arrange(doc, order_infec) %>%
  mutate(
    diff_time_infec = as.numeric(difftime(date_sample, lag(date_sample), units = "days")),
    diff_time_infec = if_else(is.na(diff_time_infec), 0, diff_time_infec),
    diff_time_infec = if_else(infect_m1==0, 0, diff_time_infec),
    diff_time_infec1 = if_else(infect_m1==1, cumsum(diff_time_infec), 0)
  )


#dplyr form of reinfection cases
data<-data %>%
  arrange(order_infec>0) %>%
  mutate(
    reinfectec2=ifelse(order_infec>1 & diff_time_infec>90,1,0))

table(data$reinfectec2)

#create a valid reinfection order
data<-data %>%
  group_by(doc) %>%
  arrange(doc, order_infec) %>%
  mutate(
    order_infec_valid= ifelse(reinfectec2==1, 
                              cumsum(reinfectec2), 0))


#create a valid infection - primo-infection - reinfection order
# specifying prime-infection
data$reinf1<-ifelse(data$order_infec_valid==1,1,0)

data<-data %>%
  group_by(doc, infect_m1) %>%
  arrange(order_infec_valid) %>%
  mutate(
    diff_time_infec_b= lead(reinf1),
    primo_infec2= ifelse(diff_time_infec_b == 1,1,0)
  )

table(data$primo_infec2)
table(data$order_infec_valid, data$primo_infec2)
table(data$reinfectec2)

data<-data %>%
  replace_na(list(
    primo_infec2=0,
    order_infec_valid=0))

data$primo_reinf<-ifelse(data$primo_infec2==1,1,
                         ifelse(data$order_infec_valid>0,
                                data$order_infec_valid+1,0))

data$infec_primo_reinf<-data$primo_reinf
data$infec_primo_reinf<-ifelse(data$infect_m1==1 & 
                                 data$primo_reinf==0, 9, 
                               data$infec_primo_reinf)



table(data$infec_primo_reinf)
table(data$valid_result_ord_infec)



write.csv2(data, file = "Reinfec_validation_test2.csv")



# #levels of variable infec_primo_reinfec
# # 0=no infection
# # 1=prime-infection (first infection for cases with reinfections)
# # 2=reinfection 1
# # 3=reinfection 2
# # 4=reinfection 3
# # 5=reinfection 4
# # 6...
# # 9= infection excluding reinfections and prime-infections


























