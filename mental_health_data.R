
knitr::opts_chunk$set(echo = TRUE)



rm(list = ls())


library(data.table)
library(tidyverse)
library(ggplot2)

pulse18 <- fread("pulse2020_puf_18.csv")

MHdf_2 <- subset(pulse18, select = c(SCRAM, REGION, PUBHLTH, PRIVHLTH, EST_ST, MH_SVCS, MH_NOTGET, INCOME))

MHdf_2 <- na.omit(MHdf_2)

MHdf_2$income <- ifelse(MHdf_2$INCOME == "1", "< $25K",
                        ifelse(MHdf_2$INCOME == "2", "$25K - $34.9K",
                               ifelse(MHdf_2$INCOME == "3", "$35K - $49.9K",
                                      ifelse(MHdf_2$INCOME == "4", "$50K - $74.9K",
                                             ifelse(MHdf_2$INCOME == "5", "$75K - $99.9K", 
                                                    ifelse(MHdf_2$INCOME == "6", "$100K - $149.9K",
                                                           ifelse(MHdf_2$INCOME == "7", "$150K - $199.9K", ">$199.9K"))))))) 
                                                           MHdf_2$Mh_ntgt_2 <- ifelse(MHdf_2$MH_NOTGET == "1", "Services Not Recieved", "Services Recieved")
                                                           Mh_svs_2 <- ifelse(MHdf_2$PUBHLTH == "1", "Recieved services", "No Services")
                                                    
 
                                              MHdf_2$state <- ifelse(MHdf_2$EST_ST == "1","Alabama",
                                                                                  ifelse(MHdf_2$EST_ST == "2", "Alaska",  
                                                                                         ifelse(MHdf_2$EST_ST == "4", "Arizona", 
                                                                                                ifelse(MHdf_2$EST_ST == "5", "Arkansas",
                                                                                                       ifelse(MHdf_2$EST_ST == "6", "California", 
                                                                                                              ifelse(MHdf_2$EST_ST == "8", "Colorado", 
                                                                                                                     ifelse(MHdf_2$EST_ST == "9", "Connecticut", 
                                                                                                                            ifelse(MHdf_2$EST_ST == "10","Delaware",
                                                                                                                                   ifelse(MHdf_2$EST_ST == "11", "District of Columbia", 
                                                                                                                                          ifelse(MHdf_2$EST_ST == "12", "Florida",     
                                                                                                                                                 ifelse(MHdf_2$EST_ST == "13", "Georgia",
                                                                                                                                                        ifelse(MHdf_2$EST_ST == "15", "Hawaii", 
                                                                                                                                                               ifelse(MHdf_2$EST_ST == "16", "Idaho",
                                                                                                                                                                      ifelse(MHdf_2$EST_ST == "17", "Illinois", 
                                                                                                                                                                             ifelse(MHdf_2$EST_ST == "18", "Indiana",
                                                                                                                                                                                    ifelse(MHdf_2$EST_ST == "19", "Iowa", 
                                                                                                                                                                                           ifelse(MHdf_2$EST_ST == "20", "Kansas", 
                                                                                                                                                                                                  ifelse(MHdf_2$EST_ST == "21", "Kentucky", 
                                                                                                                                                                                                         ifelse(MHdf_2$EST_ST == "22", "Louisiana", 
                                                                                                                                                                                                                ifelse(MHdf_2$EST_ST == "23", "Maine", 
                                                                                                                                                                                                                       ifelse(MHdf_2$EST_ST == "24", "Maryland", 
                                                                                                                                                                                                                              ifelse(MHdf_2$EST_ST == "25", "Massachusetts", 
                                                                                                                                                                                                                                     ifelse(MHdf_2$EST_ST == "26", "Michigan", 
                                                                                                                                                                                                                                            ifelse(MHdf_2$EST_ST == "27", "Minnesota", 
                                                                                                                                                                                                                                                   ifelse(MHdf_2$EST_ST == "28", "Mississippi", 
                                                                                                                                                                                                                                                          ifelse(MHdf_2$EST_ST == "29", "Missouri", 
                                                                                                                                                                                                                                                                 ifelse(MHdf_2$EST_ST == "30", "Montana",
                                                                                                                                                                                                                                                                        ifelse(MHdf_2$EST_ST == "31", "Nebraska",
                                                                                                                                                                                                                                                                               ifelse(MHdf_2$EST_ST == "32", "Nevada", 
                                                                                                                                                                                                                                                                                      ifelse(MHdf_2$EST_ST == "33", "New Hampshire",
                                                                                                                                                                                                                                                                                             ifelse(MHdf_2$EST_ST == "34", "New Jersey",
                                                                                                                                                                                                                                                                                                    ifelse(MHdf_2$EST_ST == "35", "New Mexico",
                                                                                                                                                                                                                                                                                                           ifelse(MHdf_2$EST_ST == "36", "New York",
                                                                                                                                                                                                                                                                                                                  ifelse(MHdf_2$EST_ST == "37", "North Carolina", 
                                                                                                                                                                                                                                                                                                                         ifelse(MHdf_2$EST_ST == "38", "North Dakota", 
                                                                                                                                                                                                                                                                                                                                ifelse(MHdf_2$EST_ST == "39", "Ohio",
                                                                                                                                                                                                                                                                                                                                       ifelse(MHdf_2$EST_ST == "40", "Oklahoma",
                                                                                                                                                                                                                                                                                                                                              ifelse(MHdf_2$EST_ST == "41", "Oregon",
                                                                                                                                                                                                                                                                                                                                                     ifelse(MHdf_2$EST_ST == "42", "Pennsylvania",
                                                                                                                                                                                                                                                                                                                                                            ifelse(MHdf_2$EST_ST == "44", "Rhode Island",
                                                                                                                                                                                                                                                                                                                                                                   ifelse(MHdf_2$EST_ST == "45", "South Carolina",
                                                                                                                                                                                                                                                                                                                                                                          ifelse(MHdf_2$EST_ST == "46", "South Dakota",
                                                                                                                                                                                                                                                                                                                                                                                 ifelse(MHdf_2$EST_ST == "47", "Tennessee",
                                                                                                                                                                                                                                                                                                                                                                                        ifelse(MHdf_2$EST_ST == "48", "Texas",
                                                                                                                                                                                                                                                                                                                                                                                               ifelse(MHdf_2$EST_ST == "49", "Utah",
                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(MHdf_2$EST_ST == "50", "Vermont",
                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(MHdf_2$EST_ST == "51", "Virginia",
                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(MHdf_2$EST_ST == "53", "Washington",
                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(MHdf_2$EST_ST == "54", "West Virgina",
                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(MHdf_2$EST_ST == "55", "Wisconsin", "Wyoming"
                                                                                                                                                                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))))))))))))))
                                                           table(MHdf_2$state, useNA = "always")
                                                           
                                                           
                                                           MHdf_2$insurance <- ifelse(MHdf_2$PUBHLTH == "1" & MHdf_2$PRIVHLTH == "1", "Both", 
                                                                                      ifelse(MHdf_2$PUBHLTH == "1" & MHdf_2$PRIVHLTH == "2", "Public insurance only", 
                                                                                             ifelse(MHdf_2$PUBHLTH == "2" & MHdf_2$PRIVHLTH == "1", "Private insurance only", 
                                                                                                    ifelse(MHdf_2$PUBHLTH == "2" & MHdf_2$PRIVHLTH == "2", "No Insurance", NA))))
                                                           table(MHdf_2$insurance, useNA = "always") 
MHdf_2$region_2 <- ifelse(MHdf_2$REGION == "1", "Northeast",
            ifelse(MHdf_2$REGION == "2", "South",
             ifelse(MHdf_2$REGION == "3", "Midwest", "West")))

DF2 <- MHdf_2 %>%
  dplyr::mutate(MH_NOTGET = ifelse(MHdf_2$MH_NOTGET == "1",1,0)) %>%
  filter(MHdf_2$insurance == "Public insurance only") %>%
  group_by(state) %>%
  summarise(Total_sv_needed = n(),
            num_notrec = sum(MH_NOTGET)) %>%
  mutate(services_rec = Total_sv_needed - num_notrec, 
         Notrec_rate = num_notrec/Total_sv_needed)  
DF4 <- MHdf_2 %>%
  dplyr::mutate(MH_NOTGET = ifelse(MHdf_2$MH_NOTGET == "1",1,0)) %>%
  filter(MHdf_2$insurance == "Private insurance only") %>%
  group_by(state) %>%
  summarise(Total_sv_needed = n(),
            num_notrec = sum(MH_NOTGET)) %>%
  mutate(services_rec = Total_sv_needed - num_notrec, 
         Notrec_rate = num_notrec/Total_sv_needed) 
DF_income_Pub <- MHdf_2 %>%
  dplyr::mutate(MH_NOTGET = ifelse(MHdf_2$MH_NOTGET == "1",1,0)) %>%
  filter(MHdf_2$insurance == "Public insurance only") %>%
  group_by(income) %>%
  summarise(Total_sv_needed = n(),
            num_notrec = sum(MH_NOTGET)) %>%
  mutate(services_rec = Total_sv_needed - num_notrec, 
         Notrec_rate = num_notrec/Total_sv_needed *100)  
DF_income_Pub$Notrec_rate <- round(DF_income_Pub$Notrec_rate, digits = 2)

DF_income_Pri <- MHdf_2 %>%
  dplyr::mutate(MH_NOTGET = ifelse(MHdf_2$MH_NOTGET == "1",1,0)) %>%
  filter(MHdf_2$insurance == "Private insurance only") %>%
  group_by(income) %>%
  summarise(Total_sv_needed = n(),
            num_notrec = sum(MH_NOTGET)) %>%
  mutate(services_rec = Total_sv_needed - num_notrec, 
         Notrec_rate = num_notrec/Total_sv_needed * 100)
DF_income_Pri$Notrec_rate <- round(DF_income_Pri$Notrec_rate, digits = 2)

private.insurance <- DF4 %>% 
  plot_ly(x = ~Total_sv_needed, y = ~num_notrec, 
          type = 'scatter',
          mode = 'markers',
          color = ~state,
          colors = "Purples")
private.insurance <- private.insurance %>% layout(title = 'Mental health services requested and not recieved by state and private insurance ',
                                                  xaxis = list(title = 'Total services requested'),
                                                  yaxis = list(title = 'Number of services not recived'))

private.insurance

public.insurance <- DF2 %>% 
  plot_ly(x = ~Total_sv_needed, y = ~num_notrec, 
          type = 'scatter',
          mode = 'markers',
          color = ~state,
          colors = "Reds")
public.insurance <- public.insurance %>% layout(title = 'Mental health services requested and not recieved by state and insurance type',
                                                xaxis = list(title = 'Total services requested'),
                                                yaxis = list(title = 'Number of services not recived'))

public.insurance

income.data <- DF_income_Pri %>%
  left_join(DF_income_Pub, by = c("income"))  



income <- plot_ly(x = ~income.data$income, y = ~income.data$num_notrec.x, 
                  type = 'bar',
                  name = 'Private Insurance')
income <- income %>% add_trace( y= ~income.data$num_notrec.y, name = "Public Insurance" )
income <- income %>% layout( title = 'Mental health services requested and not recived by income and insurance type',
                             yaxis = list( title = 'Number of services requested and not received'), barmode = 'group',
                             xaxis = list (title = "Income"),
                             color = ~income,
                             colors = "Blues")
income

large_data <- fread("world_country_and_usa_states_latitude_and_longitude_values.csv")

state.lat.lon <- large_data[, c("usa_state", "usa_state_latitude", "usa_state_longitude", "usa_state_code")]
state_data <- state.lat.lon[-40,]

state_data <-rename(state_data, state = usa_state)

state_data.comp <- state_data %>%
  left_join(DF2, by = c("state"))


state_data.comp <- na.omit(state_data.comp)


state_data.comp_2 <- state_data.comp %>%  mutate(per.rate = Notrec_rate * 100)


state_data.comp_2 [round(per.rate, digits = 2)]

DF4.pri <- rename(DF4, total_sv_needed.pri = Total_sv_needed , num_notrec.pri = num_notrec , services_rec.pri = services_rec , Notrec_rate.pri = Notrec_rate )

DF2.pub <- rename(DF2, total_sv_needed.pub = Total_sv_needed , num_notrec.pub = num_notrec , services_rec.pub = services_rec , Notrec_rate.pub = Notrec_rate )

map.data <- DF4.pri %>%
  left_join(DF2.pub, by = c("state"),
            left_join(state_data.comp_2, by = c("state")))

map.data <- map.data %>%
  left_join(state_data.comp_2, by = c("state"))
map.data <- map.data %>% mutate(total.needed = total_sv_needed.pri + total_sv_needed.pub, tot.svs.notrec = num_notrec.pub + num_notrec.pri , tot.notrec.rate = tot.svs.notrec / total.needed * 100)
    
 map.data$tot.notrec.rate <-  round(map.data$tot.notrec.rate, digits = 1)
 map.data$hover <- with(map.data, paste(" total services requested", '<br>', state, '<br>', tot.svs.notrec, "total services not received", '<br>',  tot.notrec.rate, " % services not received "  ))
 
 
 l <- list(color = toRGB("white"), width=2)
 

 g <- list(
   scope = 'usa',
   projection = list(type = 'albers usa'),
   showlakes = TRUE, 
   lakecolor = toRGB('white')
 ) 
 
 fig <- plot_geo(state_data.comp, 
                 locationmode = 'USA-states')
 
 
 
 fig <- fig %>% add_trace (z = ~map.data$total.needed,    
                           text = ~map.data$hover,
                           locations = ~map.data$usa_state_code,
                           color = ~map.data$Notrec_rate,
                           colors = 'Purples' )
 
 
 
 
 fig <- fig  %>% layout (title = "Rate of total Mental Health Services Requested and Not Received by state and insurance",
                         geo =g )
 
 fig

