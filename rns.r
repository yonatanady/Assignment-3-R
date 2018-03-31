#install.packages("knitr")
#install.packages("plotly")
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)

#not sure if we need this part, but at the moment i put it here first
#downloading data
download.file("https://ndownloader.figshare.com/files/2292169", "data/portal_data_joined.csv")

#read_csv = tibble, diff from read.csv, it treat stringasfactor = FALSE by default, load faster
surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)

summary(surveys)

#========Exploratory Analysis===========================
#overall distribution of the species by taxa
surveys_taxa <- surveys %>%
    filter(!is.na(taxa)) %>% group_by(taxa) %>% 
    tally()

#taxa pie chart
plot_ly(surveys_taxa, labels = ~taxa, values = ~n, type = 'pie',textposition = 'outside',
        domain = list(x = c(0, 0), y = c(0, 0)), textinfo = 'label+percent',
        rotation = -108, direction = "clockwise", 
        textfont = list(color = '#000000', size = 16)) %>%
    layout(title = 'Taxa Distribution', xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                                     showticklabels = FALSE), 
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#to find out what are the species and how many of them for each taxa
surveys_species <- surveys %>%
    filter(!is.na(taxa), !is.na(genus), !is.na(species), !species_id == "") %>%
    mutate(species_name = paste(genus, species)) %>% group_by(taxa, species_name, species_id) %>% 
    tally()

#bird distribution
bird_species <- surveys_species %>% filter(taxa == "Bird") %>% 
    group_by(taxa, species_id)

#rabbit distribution
rabbit_species <- surveys_species %>% filter(taxa == "Rabbit") %>% 
    group_by(taxa, species_id) 

#reptile distribution
reptile_species <- surveys_species %>% filter(taxa == "Reptile") %>% 
    group_by(taxa, species_id) 

#because rodent is the most abundant, show them separately
#rodent distribution
rodent_species <- surveys_species %>% filter(taxa == "Rodent") %>% 
    group_by(taxa, species_id) 

#pie chart for each taxa except rodent
b <- plot_ly(bird_species, labels = ~species_name, values = ~n, type = 'pie',
             domain = list(x = c(0, 0.5), y = c(0.55, 1)), textposition = 'inside',
             textinfo = 'label+percent', textfont = list(color = '#000000', size = 12)) %>%
    layout(xaxis = list(title = "Bird", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

ra <- plot_ly(rabbit_species, labels = ~species_name, values = ~n, type = 'pie',
              domain = list(x = c(0.5, 1), y = c(0.55, 1)), textposition = 'inside',
              textinfo = 'label+percent', textfont = list(color = '#000000', size = 12)) %>%
    layout(xaxis = list(title = "Rabbit", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

re <- plot_ly(reptile_species, labels = ~species_name, values = ~n, type = 'pie',
              domain = list(x = c(0, 0.5), y = c(0, 0.45)), textposition = 'inside',
              textinfo = 'label+percent', textfont = list(color = '#000000', size = 12)) %>%
    layout(xaxis = list(title = "Reptile", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

ro <- plot_ly(rodent_species, labels = ~species_name, values = ~n, type = 'pie',
              domain = list(x = c(0.5, 1), y = c(0, 0.45)), textposition = 'inside',
              textinfo = 'label+percent') %>%
    layout(xaxis = list(title = "Rodent", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#combined multiple pie charts
subplot(b, ra, re, ro, nrows = 2, titleX = TRUE) %>% 
    layout(title = "Distribution of Species according to Taxa", 
           showlegend = FALSE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#taxa information according to years
surveys_taxa_years <- surveys %>%
    select(taxa,year) %>%
    group_by(taxa, year) %>% tally()

surveys_taxa_years$year <- as.factor(surveys_taxa_years$year)

ggplot(surveys_taxa_years, aes(x = year, y = n)) + 
    geom_bar(stat = "identity") + facet_wrap(~taxa, scales = "free_y") +
    theme_linedraw() + theme(legend.position = "right") +
    labs(title = "Taxa Information According to Year", 
         x = "Year", y = "Count (n)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))

#weight according to species
surveys_weight <- surveys %>%
    filter(!is.na(year), !is.na(weight), !species_id == "") %>%
    group_by(year, species_id) %>% 
    mutate(average_weight = mean(weight)) 

#time series plot
ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
    geom_line(aes(color = species_id)) + theme_classic() + 
    theme(legend.position = "right") +
    labs(title = "Distribution of Species Average Weight according to Year", x = "Year", 
         y = "Average Weight", color = "Species ID") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) +
    theme(axis.text.y = element_text(size = 9)) 


#average rodent hindfoot length according to species_id for each taxa
#note that the only taxon with information about hindfoot length is Rodent (see summary below), so there is only 1 graph
surveys_hfoot_id <- surveys %>%
    filter(!is.na(hindfoot_length), !taxa == "", !species_id == "") %>% 
    group_by(species_id) %>%
    mutate(average_hfoot = mean(hindfoot_length))

summary(as.factor(surveys_hfoot_id$taxa))

ggplot(surveys_hfoot_id, aes(x = species_id, y = average_hfoot)) +
    theme_classic() + theme(legend.position = "right") +
    labs(title = "Rodent Hindfoot Length", x = "Species ID", 
         y = "Hindfoot Length (cm)") + 
    stat_summary(fun.y = "mean", geom = "bar") +
    geom_text(aes(label=round(average_hfoot)), vjust=-0.3, size=3.5) +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))

#average rodent hindfoot length according to plot type
surveys_hfoot_plottype <- surveys %>%
    filter(!is.na(hindfoot_length), !plot_type == "") 

ggplot(aes(x = plot_type, y = hindfoot_length), data = surveys_hfoot_plottype) + 
    stat_summary(fun.y = "mean", geom = "bar") + theme_classic() + 
    theme(legend.position = "right") +
    labs(title = "Average Rodent Hindfoot Length by Plot Type", x = "Plot Type", 
         y = "Hindfoot Length (cm)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))


#=========Analysis based on Rodent=====================
#distribution of rodent
plot_ly(rodent_species, labels = ~species_name, values = ~n, type = 'pie',
              domain = list(x = c(0, 0), y = c(0, 0)), textposition = 'inside',
              textinfo = 'label+percent') %>%
    layout(title = "Distribution of Rodent", 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#to correlate hindfoot_length and weight of rodent
rodent_complete <- surveys %>% 
    filter(!is.na(sex), !is.na(hindfoot_length), !is.na(weight), !is.na(plot_type))
rodent_complete$year <- as.factor(rodent_complete$year)

#year capture according to rodent species_id
rodent_genus_year <- rodent_complete %>% select(genus, species_id, year) %>% 
    group_by(genus, species_id, year) %>% tally()

spp_year_count <- ggplot(rodent_genus_year, aes(x = year, y = n, fill = species_id)) + 
    geom_bar(stat = "identity") + theme_classic() + theme(legend.position = "right") +
    labs(title = "Species Captured according to Year", x = "Year", 
         y = "Count (n)", fill = "Species ID") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))

#sex information according to taxa
surveys_taxa_sex <- surveys %>%
    filter(!is.na(sex)) %>% select(taxa,sex) %>%
    group_by(taxa, sex) %>% tally()
#only rodent that had the sex are, the other taxa sex is n/a

ggplot(surveys_taxa_sex, aes(x = sex, y = n)) + 
    geom_bar(stat = "identity") +
    theme_classic() + theme(legend.position = "right") +
    labs(title = "Sex Distribution of Rodent", x = "Sex", y = "Count (n)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) +
    scale_x_discrete(labels = c("Female", "Male")) +
    theme(axis.text.y = element_text(size = 9))

#female and male distribution according to species_id
rodent_species_sex <- rodent_complete %>% select(species_id, sex) %>%
    group_by(species_id, sex) %>% tally()

spp_sex_count <- ggplot(rodent_species_sex, aes(x = species_id, y = log(n), fill = sex)) + 
    geom_bar(stat = "identity") + theme_classic() + theme(legend.position = "right") +
    labs(title = "Sex Distribution of Species", x = "Species ID", 
         y = "Count \n log(n)", fill = "Sex") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))

#combine both plot
grid.arrange(spp_year_count, spp_sex_count, ncol = 2, widths = c(5,5))

#density of weight according to sex for each plot type
ggplot(rodent_complete, aes(x = weight, fill = sex)) +
    geom_density(alpha = 0.6) + facet_wrap(~ plot_type) + theme_linedraw() + 
    theme(legend.position = "right") +
    labs(title = "Weight Density according to Sex for Each Plot Type", x = "Weight", 
         y = "Density", fill = "Sex") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) +
    theme(axis.text.y = element_text(size = 9))

#density of hindfoot_length according to sex for each plot type
ggplot(rodent_complete, aes(x = hindfoot_length, fill = sex)) +
    geom_density(alpha = 0.6) + facet_wrap(~ plot_type) +
    theme_linedraw() + theme(legend.position = "right") +
    labs(title = "Hindfoot Length Density according to Sex for Each Plot Type", 
         x = "Hindfoot Length (cm)", y = "Density", fill = "Sex") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) +
    theme(axis.text.y = element_text(size = 9))

rodent_complete <- surveys %>% 
    filter(!is.na(sex), !is.na(hindfoot_length), !is.na(weight), !is.na(plot_type))

#density of species per year for each plot type
ggplot(rodent_complete, aes(x = year, fill = species_id)) +
    geom_density(alpha = 0.6) + facet_wrap(~ plot_type) +
    theme_linedraw() + theme(legend.position = "right") +
    labs(title = "Species Density per Year for Each Plot Type", 
         x = "Year", y = "Density", fill = "Species ID") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) + 
    theme(axis.text.y = element_text(size = 9))

#to correlate hind_foot length and weight according to species_id
ggplot(rodent_complete, aes(x = weight, y = hindfoot_length)) + 
    geom_point(aes(color = species_id)) +
    geom_smooth(method = "lm", se = FALSE, color = 'maroon') +
    theme_classic() + theme(legend.position = "right") +
    labs(title = "Relationship between Hindfoot Length and Weight of Species", 
         x = "Weight (g)", y = "Hindfoot Length (cm)", color = "Species ID") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(size = 9)) +
    theme(axis.text.y = element_text(size = 9))


#========Statistical Analysis===========================

#difference between female and male weight
surveys_weight_sex <- surveys %>%
  filter(!is.na(year), !is.na(weight), !is.na(sex), !species_id == "") %>%
  group_by(species_id, sex) %>% 
  mutate(average_weight = mean(weight))

ggplot(surveys_weight_sex, aes(x = species_id, y = weight)) + 
  geom_boxplot(aes(color = sex)) + theme_classic() + theme(legend.position = "right") +
    labs(title = "Comparison Between Female \nand Male Weight", x = "Species ID", 
         y = "Average Weight (g)", color = "Sex") +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    theme(axis.text.y = element_text(size = 9))  


#t-test & anova

