### My COVID Rt estimation and general covid visualizations
### Much code used from https://www.datacamp.com/community/tutorials/replicating-in-r-covid19
library(tidyverse)
library(HDInterval)
library(smoother)
library(lubridate)
library(padr)

source("functions.R")

# Plot options

## Jupyter notebooks use the repr package to create viewable representations
## of R objects (https://github.com/IRkernel/repr). I am updating the default
## plot dimensions to 12 x 6.
options(repr.plot.width = 18, repr.plot.height = 6)


# r_t_range is a vector of possible values for R_t
R_T_MAX = 12
r_t_range = seq(0, R_T_MAX, length = R_T_MAX*100 + 1)

# Gamma is 1/serial interval
# https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
GAMMA = 1/4

### Variables for Automation
filename <- paste0("./graphs/bc-covid-epi-rt-",Sys.Date(),".PNG")
date.today <- Sys.Date()
date.yesterday <- Sys.Date() -1
count.yesterday <- 522

covid.raw <- read_csv("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv")

### Clean BC Daily Case Count
covid.daily_count <- covid.raw %>% 
  mutate(Reported_Date = as_date(Reported_Date)) %>%
  group_by(Reported_Date) %>%
  summarize(n=n()) %>%
  arrange(Reported_Date) %>%
  slice(1:(n()-1)) %>%
  add_row(Reported_Date = date.yesterday, n=count.yesterday) %>%
  pad(start_val = ymd("2020-01-26"), end_val = date.yesterday) %>%
  mutate(n=replace_na(n,0)) #%>%
  #mutate(n=replace(n, which(Reported_Date==ymd("2020-12-12")), 698)) %>% 
  #mutate(n=replace(n, which(Reported_Date==ymd("2020-12-11")), 689))


### Process, and estimate daily rt  
covid.estimated_rt <- covid.daily_count %>% smooth_new_cases() %>% 
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() 

### Temp for future
### Output smoothed cases for potential graphing
# covid.smoothed <- covid.daily_count %>% smooth_new_cases()
# 
# ### Merge case counts back into estimated_rt
# covid.estimated_rt <- covid.estimated_rt %>% right_join(covid.smoothed, by="Reported_Date") %>%
#   arrange(Reported_Date)
###

###Post-processing variables for chart automation.
date.lastdata <- last(covid.daily_count$Reported_Date)
rt.interval <- paste0(format(last(covid.estimated_rt$r_t_most_likely), nsmall=2)," (95% HDI: ",
                      format(last(covid.estimated_rt$r_t_lo), nsmall=2),", ",
                      format(last(covid.estimated_rt$r_t_hi), nsmall=2),")")

### Create the output graph
covid.estimated_rt %>%
  ggplot(aes(x = Reported_Date, y = r_t_most_likely)) +
  geom_line(color = "#14243e") +
  geom_hline(yintercept = 1, linetype = 'solid', color = 'darkblue') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'gray') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-13")), linetype=6, color='darkred')+ #PH Emergency
  annotate(geom="text",x=as.Date("2020-03-13"),y=3.1,label="Public Health Emerg",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-17")), linetype=6, color='darkred')+ #Phase 1
  annotate(geom="text",x=as.Date("2020-03-17"),y=3.1,label="Phase 1 Restrictions",angle = 90,hjust=1, vjust=1, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-05-19")), linetype=6, color='darkred')+ #Phase 2
  annotate(geom="text",x=as.Date("2020-05-19"),y=3.1,label="Phase 2 Reopening",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-24")), linetype=6, color='darkred')+ #Phase 3
  annotate(geom="text",x=as.Date("2020-06-24"),y=3.1,label="Phase 3 Reopening",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-10-09")), linetype=6, color='darkred')+ #Liquor Establishments
  annotate(geom="text",x=as.Date("2020-10-09"),y=3.1,label="Clubs & Alcohol Serving Est",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-07")), linetype=6, color='darkred')+ #Regional Restrictions
  annotate(geom="text",x=as.Date("2020-11-07"),y=3.1,label="Regional Restrictions",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-19")), linetype=6, color='darkred')+ #Province Wide Restrictions
  annotate(geom="text",x=as.Date("2020-11-19"),y=3.1,label="Prov-Wide Restrictions",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_vline(xintercept = as.numeric(as.Date("2020-12-09")), linetype=6, color='green')+ #First Vaccine Approval
  annotate(geom="text",x=as.Date("2020-12-09"),y=3.1,label="First Vaccine Approved",angle = 90,hjust=1, vjust=-0.5, fontface='bold')+
  geom_ribbon(
    aes(ymin = r_t_lo, ymax = r_t_hi),
    fill = 'blue',
    alpha = 0.21
  ) +
  labs(
    title = bquote("British Columbia Real-Time Reproductive Number"~(R[t]):~.(rt.interval)), 
              x = '', y = expression(Reproductive~Number~(R[t])),
    subtitle = unique(paste0('Published ',date.today,' - Data up to ',date.lastdata)),
    caption = "Data Source: BCCDC COVID-19 Case data & Daily Updates
                Estimates in the grey shaded area are unstable and will stabilize as new data are added
                 Modelling is based on smoothed 7-day daily new case counts in order to account for reporting lag
                Values >1 indicate the number of daily new cases is growing
                Values <1 indicate the number of daily new cases is shrinking
                Information presented is as-is and for informational purposes only"
  ) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y", date_minor_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=0, hjust=0.5, size=10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  annotate("rect", xmin=date.today-7, xmax=date.today, ymin=0, ymax=Inf, alpha=.2)

ggsave(paste0(filename), width=9, height= 6.5)
    
yesterday.month <- month(date.yesterday, label=TRUE)
message <- paste0("Reproductive Number (Rt) for BC, ", yesterday.month," ", day(date.yesterday), ", ", year(date.yesterday) ,": ",rt.interval)
print(message)  
    
    
    
    ######################################## Temp Code
    
      ### color for positivity
      
#       color_code_order <- c("<0.1%", "0.1%-1%", "1%-2%", "2%-3%", "3%-5%", "5%-10%", "10%-20%", ">20%", "Unknown")
#     
#     positivity_colors <- c("<0.1%" = "#313695", 
#                            "0.1%-1%" = "#4575b4", 
#                            "1%-2%"= "#74add1", 
#                            "2%-3%" = "#abd9e9", 
#                            "3%-5%" = "#fdae61", 
#                            "5%-10%" = "#f46d43", 
#                            "10%-20%" = "#d73027", 
#                            ">20%" = "#a50026",
#                            "Unknown" = "#4d4d4d") 
#     
#   color_code = cut(positivity, breaks = c(-1, 0.1, 1, 2, 3, 5, 10, 20, Inf), 
#                    labels = c("<0.1%", "0.1%-1%", "1%-2%", "2%-3%", "3%-5%", "5%-10%", "10%-20%", ">20%")) 
#   # color_code = cut(positivity, breaks = c(-1, 0.099, 0.99, 1.99, 2.99, 4.99, 9.99, 19.99, Inf), 
#   #                  labels = c("<0.1%", "0.1%-<1%", "1%-<2%", "2%-<3%", "3%-<5%", "5%-<10%", "10%-<20%", ">=20%")),
#   color_code = str_replace_na(color_code, "Unknown") 
# arrange(location, date) %>%
#   arrange(match(color_code, color_code_order)) 
# 
# geom_line(aes(y = measure, 
#               color = as_factor(color_code), 
#               group = location), 
#           size = 2) 
