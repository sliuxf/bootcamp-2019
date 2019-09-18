#import data

generation <- read.csv(here::here("data/ca_energy_generation.csv"))
imports <- read.csv(here::here("data/ca_energy_imports.csv"))
generation$datetime <- as_datetime(generation$datetime)
imports$datetime <- as_datetime(imports$datetime)
str(generation)

#change datetime from character data type
library(lubridate)

class(generation$datetime)
class(imports$datetime)

head(generation$datetime)

#Reshape data
head(generation)

#Long data
library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")
head(long_gen)

long_gen[order(long_gen$datetime)[1:20], ]

#Merging data
merge_energy <- merge(generation, imports, by = "datetime")
dim(merge_energy)
#dim(generation)
#dim(imports)
head(merge_energy)

#make merged data set long
long_merged_energy <- melt(merge_energy, id.vars = "datetime", variable.name = "source", 
                           value.name = "usage")
head(long_merged_energy)
#dim(long_merged_energy)
#dim(long_gen)

#dplyr

#select function
tmp <- select(merge_energy, biogas, biomass, geothermal, solar)
names(tmp)
tmp <- select(merge_energy, -biogas, -biomass, -geothermal, -solar)
names(tmp)
#select helper functions
tmp <- select(merged_energy, contains("hydro"), starts_with("bio"))
names(tmp)

#filter
tmp <- filter(merge_energy, imports > 7000)
nrow(tmp)
head(tmp)
#multiple conditions in filter
tmp <- filter(merge_energy, imports > 7000, natural_gas < 7000)
nrow(tmp)
head(tmp)

#mutate
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)

tmp <- mutate(long_merged_energy, log_usage = log(usage), usage2 = usage^2, usage3 = usage^3)
head(tmp)

#summarize
summarize(long_merged_energy, total = sum(usage, na.rm = T))
summarize(long_merged_energy, mean_cons = mean(usage, na.rm = T))

#%>% as "then"
# take df then filter it then select these variables
# you do not need to repeat the name of the dataframe!
long_merged_energy %>% 
    filter(source == "geothermal") %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarize(mean_log_usage = mean(log_usage, na.rm = T))

merged_energy %>% 
    select(-datetime) %>% 
    mutate(total_usage = rowSums(., na.rm = T)) %>%   #use . here to avoid datetime
    summarize(total_usage = sum(total_usage, na.rm = T))

#Piping
#Using the (wide) merged CA energy data merged_energy do the following with pipes:
#Select variables that contain the word ???hydro???
#Create a new variable called total_hydro that is the sum of the retained hydro variables
#Find the mean usage for total_hydro
merge_energy %>%
    select(contains("hydro")) %>%
    mutate(., total_hydro = rowSums (., na.rm = T)) %>%
    summarize(total_hydro = mean(total_hydro, na.rm = T))


#group by versus for loop
#Find the mean by source
#Use your knowledge of dplyr to find the mean usage for small hydro, large hydro, biogas, and biomass
#Start with either the wide or long merged dataset
merge_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars = "datetime", variable.name = "source", value.name = "usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))




#data.table
library(data.table)

data_file <- here::here("data", "ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F)

generation_dt <- fread(data_file)
class(generation_df) 
class(generation_dt) 
View(generation_df)
View(generation_dt)
generation_df
generation_dt
str(generation_df)
str(generation_dt)

#i: ???on which rows??? (row filtering)
#logical operations filter rows
generation_dt[wind > 4400]
#logical operations filter rows
generation_dt[wind > 4400 & mday(datetime) == 7]

#Exercise:
#try it! select rows for which natural gas generation is less than or equal to 
#5,000 MW and large hydro generation is greater than 2,000 MW
generation_dt[natural_gas <= 5000 & large_hydro > 2000]

#bonus: select rows for which coal generation is greater than 10 MW and solar 
#generation is greater than the median value of solar generation
generation_dt[coal > 10 & solar > median(solar)]


#j: ???what to do??? (column operations)
#perform any operation in j
generation_dt[,wind + solar]

#perform any operation in j
generation_dt[,3*wind + solar*biogas/2]

#new columns may be assigned in j or a new table can be created
generation_dt[,newcol := 3*wind + solar*biogas/2]      # := in place
generation_dt[,.(newcol = 3*wind + solar*biogas/2)]    # .() export data

#columns may be deleted in j
generation_dt[,newcol := NULL]

#Exercise:
#try it! add a column called ???total_hydro??? that is the sum of the small_hydro 
#and large_hydro columns
generation_dt[, total_hydro := small_hydro+large_hydro]

#try it! find the mean of the nuclear and biogas columns
generation_dt[, .(mean(nuclear), mean(biogas))  ]

#bonus: create a new table: for the hours when solar generation is zero, 
#get the datetime and total_thermal (sum of natural gas and coal generation)
generation_dt[solar==0, .(datetime, total_thermal = (natural_gas + coal))]

#by: ???grouped by what???
#add grouping to any operation
generation_dt[,mean(nuclear), by = mday(datetime)]

#combine with everything we???ve learned about i and j
generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

#combine with everything we???ve learned about i and j
generation_dt[hour(datetime) > 19,
              .(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

#Exercise:
#try it! find the median solar generation by hour.
generation_dt[,median(solar),by=hour(datetime)]

#try it! for hours when the solar generation is greater than zero, 
#find the maximum natural gas generation by day
generation_dt[solar>0, max(natural_gas), by=mday(datetime)]


#try it! Convert this dplyr syntax into data.table syntax (remember that this 
#created the columns day, log_output, and per_output)

#long_ca_energy <- long_ca_energy %>%
#    mutate(day = as_date(datetime),
#           log_output = log(output)) %>%
#    group_by(day) %>%
#    mutate(total_daily_output = sum(output, na.rm = T)) %>% 
#    ungroup() %>% 
#    mutate(per_output = output/total_daily_output)

#all_generation_long[,day := as_date(datetime)]
#all_generation_long[,log_output := log(value)]
#all_generation_long[,per_output := value/sum(value), by = day]




