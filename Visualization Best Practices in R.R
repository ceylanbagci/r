# print dataframe to inspect
who_disease

# set x aesthetic to region column
ggplot(who_disease, aes(region)) +
  geom_bar()


# filter data to AMR region. 
amr_region <- who_disease %>%
  filter(region=='AMR')

# map x to year and y to cases. 
ggplot(amr_region, aes(x=year,y=cases)) + 
  # lower alpha to 0.5 to see overlap.   
  geom_point(alpha=0.5)


# Wrangle data into form we want. 
disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  # Use a column geometry.
  geom_col() +
  # Change coordinate system to polar and set theta to 'y'.
  coord_polar(theta = 'y')


disease_counts <- who_disease %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100))

# Create an array of rounded percentages for diseases.
case_counts <- disease_counts$percent
# Name the percentage array
names(case_counts) <- disease_counts$disease

# Pass case_counts vector to the waffle function to plot
waffle(case_counts)


disease_counts <- who_disease %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100))

# Create an array of rounded percentages for diseases.
case_counts <- disease_counts$percent
# Name the percentage array
names(case_counts) <- disease_counts$disease

# Pass case_counts vector to the waffle function to plot
waffle(case_counts)


disease_counts <- who_disease %>%
  mutate(
    disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other') %>% 
      factor(levels = c('measles', 'other', 'mumps')) # change factor levels to desired ordering
  ) %>%
  group_by(disease, year) %>%
  summarise(total_cases = sum(cases)) 

# plot
ggplot(disease_counts, aes(x = year, y = total_cases, fill = disease)) +
  geom_col(position = 'fill')



disease_counts <- who_disease %>%
  # Filter to on or later than 1999
  filter(year >= 1999) %>% 
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, region) %>%    # Add region column to grouping
  summarise(total_cases = sum(cases))

# Set aesthetics so disease is the stacking variable, region is the x-axis and counts are the y
ggplot(disease_counts, aes(x = region, y = total_cases,fill=disease)) +
  # Add a column geometry with the proper position value.
  geom_col(position='fill')


who_disease %>% 
  # filter to india in 1980
  filter(country == 'India',year == 1980) %>% 
  # map x aesthetic to disease and y to cases
  ggplot(aes(x=disease, y=cases)) +
  # use geom_col to draw
  geom_col()


who_disease %>%
  # filter data to observations of greater than 1,000 cases
  filter(cases > 1000) %>%
  # map the x-axis to the region column
  ggplot(aes(x = region)) +
  # add a geom_bar call
  geom_bar()




who_subset <- who_disease %>% 
  filter(
    countryCode %in% interestingCountries,
    disease == 'measles',
    year %in% c(1992, 2002) # Modify years to 1992 and 2002
  ) %>% 
  mutate(year = paste0('cases_', year)) %>% 
  spread(year, cases)

# Reorder y axis and change the cases year to 1992
ggplot(who_subset, aes(x = log10(cases_1992), y = reorder(country, cases_1992))) +
  geom_point()


who_subset %>% 
  # calculate the log fold change between 2016 and 2006
  mutate(logFoldChange = log2(cases_2002/cases_1992)) %>% 
  # set y axis as country ordered with respect to logFoldChange
  ggplot(aes(x = logFoldChange, y = reorder(country,logFoldChange))) +
  geom_point() +
  # add a visual anchor at x = 0
  geom_vline(xintercept=0)


who_subset %>% 
  mutate(logFoldChange = log2(cases_2002/cases_1992)) %>% 
  ggplot(aes(x = logFoldChange, y = reorder(country, logFoldChange))) +
  geom_point() +
  geom_vline(xintercept = 0) +
  xlim(-6,6) +
  # add facet_grid arranged in the column direction by region and free_y scales
  facet_grid(region~.,scale= 'free_y')


amr_pertussis <- who_disease %>% 
  filter(   # filter data to our desired subset
    region == 'AMR', 
    year == 1980, 
    disease == 'pertussis'
  )
# Set x axis as country ordered with respect to cases. 
ggplot(amr_pertussis, aes(x = reorder(country,cases), y = cases)) +
  geom_col() +
  # flip axes
  coord_flip()


amr_pertussis %>% 
  # filter to countries that had > 0 cases. 
  filter(cases>0) %>%
  ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  theme(
    # get rid of the 'major' y grid lines
    panel.grid.major.y=element_blank())



amr_pertussis %>% filter(cases > 0) %>% 
  ggplot(aes(x = reorder(country, cases), y = cases)) + 
  # switch geometry to points and set point size = 2
  geom_point(size=2) + 
  # change y-axis to log10. 
  scale_y_log10() +
  # add theme_minimal()
  theme_minimal() +
  coord_flip()


# Print data to console
md_speeding

# Change filter to red cars
md_speeding %>% 
  filter(vehicle_color == 'RED') %>% 
  # switch x mapping to speed_over column
  ggplot(aes(x = speed_over)) +
  geom_histogram() +
  # give plot a title
  ggtitle('MPH over speed limit | Red cars')


ggplot(md_speeding) + 
  # Add the histogram geometry 
  geom_histogram(aes(
    # Map speed_over to x
    x=speed_over),
    # Lower alpha to 0.7
    alpha=0.7
  ) +
  # Add minimal theme
  theme_minimal()


ggplot(md_speeding) +
  geom_histogram(
    # set x and y aesthetics to hour_of_day and stat(density) respectively.
    aes(x=hour_of_day,y=stat(density))
    # make points see-through by setting alpha to 0.8
    ,alpha=0.8
  )



# Load md_speeding into ggplot
ggplot(md_speeding) +
  # add a geom_histogram with x mapped to percentage_over_limit
  geom_histogram(
    aes(x=percentage_over_limit),
    bins=40,     # set bin number to 40
    alpha=0.8)    # reduce alpha to 0.8