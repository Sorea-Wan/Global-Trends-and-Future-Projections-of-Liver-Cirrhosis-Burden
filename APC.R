library(data.table)
library(dplyr)
library(reshape2)
library(tibble)
library(stringr)

population <- fread("C:\\Users\\86177\\Desktop\\IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv") %>%
  dplyr::select(location_name, sex_name, year, age_name, val) %>%
  dplyr::filter(location_name == "Global",
                sex_name %in% c("Male","Female"),
                year %in% 1992:2021)
population <- population %>%
  mutate(age_name = case_when(
    
    age_name %in% c("Early Neonatal",
                    "Late Neonatal",
                    "Post Neonatal",
                    "1 to 4") ~ "<5",
    
    TRUE ~ age_name
  ))
population_both <- population %>%
  group_by(location_name,year,age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(sex_name = "Both")
age_levels <- c("<5",
                "5 to 9","10 to 14","15 to 19","20 to 24",
                "25 to 29","30 to 34","35 to 39","40 to 44",
                "45 to 49","50 to 54","55 to 59","60 to 64",
                "65 to 69","70 to 74","75 to 79","80 to 84",
                "85 to 89","90 to 94","95 plus")

population_both$age_name <- factor(population_both$age_name,
                                   levels = age_levels)

population_both <- population_both %>%
  arrange(age_name,year)
population_n <- dcast(
  population_both,
  age_name ~ year,
  value.var = "val"
)

rownames(population_n) <- population_n$age_name
population_n$age_name <- NULL
function_year5 <- function(data,start,end){
  
  year_seq <- seq(start,end,5)
  
  result <- data.frame()
  
  for(i in 1:(length(year_seq)-1)){
    
    years <- year_seq[i]:(year_seq[i+1]-1)
    
    temp <- rowSums(data[,as.character(years)])
    
    result <- cbind(result,temp)
    
  }
  
  colnames(result) <- paste0(year_seq[-length(year_seq)],
                             "-",
                             year_seq[-1]-1)
  
  return(result)
  
}

population_g <- function_year5(population_n,1992,2022)

rownames(population_g) <- rownames(population_n)
name2 <- paste0(colnames(population_g),"p")

population_g <- population_g %>%
  stats::setNames(name2)
name <- intersect(rownames(population_g),
                  rownames(disease_g))

population_g <- population_g[rownames(population_g) %in% name,]

disease_g <- disease_g[rownames(disease_g) %in% name,]
APC_input <- tibble(
  cbind(disease_g,population_g)
)
APC_input <- APC_input %>%
  dplyr::select(`1992-1996`,`1992-1996p`,
                `1997-2001`,`1997-2001p`,
                `2002-2006`,`2002-2006p`,
                `2007-2011`,`2007-2011p`,
                `2012-2016`,`2012-2016p`,
                `2017-2021`,`2017-2021p`)
library(Epi)

R <- prepare_rates(APC_input,
                   StartYear = 1992,
                   StartAge = 0,
                   Interval = 5,
                   fullname = "",
                   description = "")

M <- apc2fit(R)

plot.apc1(M)
