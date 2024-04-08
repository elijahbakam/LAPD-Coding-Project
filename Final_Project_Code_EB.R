#### Final Project (Elijah's Code & Plots) ####
#### Elijah Bakam ####
#### Sadir, Anne-Sophie, Nadia, Elijah's Group ####
#### 11/25/2023 ####

# Load in the LAPD complete data set
dat <- read.csv("~/Downloads/LAPD_data_complete.csv")
# Select the stops and dates while renaming date column
dat1 <- dat %>%
  select(RFS_TRAFFIC_VIOLATION_TYPE,
         DATE_OF_STOP) %>%
  rename(DATE = DATE_OF_STOP)

# Since we are doing one plot with both total and minor violations on it, we don't need to make a subset of the data
# Instead, I will group my data by date so that its easier to plot
# I need to make the RFS data into total_count columns or else I will have weird observation groupings
dat2 <- dat1 %>%
  mutate(
    total_violations_count = ifelse(RFS_TRAFFIC_VIOLATION_TYPE %in% c(1,2,3), 1, 0), # Tells R that these need to be turned into count data depending on whether they have a 1,2,3 value
    minor_violations_count = ifelse(RFS_TRAFFIC_VIOLATION_TYPE %in% c(2, 3), 1, 0) # 2 and 3 are coded as minor violation stops
    ) %>%
      group_by(DATE) %>%
      summarise(
        total_violations = sum(total_violations_count),
        total_minor_violations = sum(minor_violations_count)
      ) %>%
      ungroup()

########## LINE PLOT (BY MONTH) ########## 
# Now that we have turned the data into count data which is able to be plotted, we can make our plot
final_line_plot <- ggplot(dat2, aes(x = DATE)) +
  geom_line(aes(y = total_violations, color = "Total Traffic Stops"), linetype = "solid", size = 1, show.legend = TRUE) + # Here we have 2 y values because I need to show both total stops and total minor stops
  geom_line(aes(y = total_minor_violations, color = "Stops For Minor Violations"), linetype = "dashed", size = 1, show.legend = TRUE) +
  # Add a vertical line at March 2022
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "solid", color = "black", size = 1) +  
  # Add a line for the Policy Change in the legend
  geom_line(aes(x = as.Date("2022-03-01"), y = 0, color = "Policy Change"), linetype = "solid", size = 1) +
  # Add labels and title with bolded main title
   labs(
    title = "Total and Minor Traffic Stops Count (September 2021 - August 2022)",
    subtitle = "March 2022: LAPD Policy Change on Pretextual Stops",
    x = "Traffic Stop Date",
    y = "Number Of Traffic Stops"
  ) +
  # Add legend with custom labels for color & linetype
  scale_color_manual(values = c("Stops For Minor Violations" = "blue", "Total Traffic Stops" = "red"),
                     labels = c("Stops For Minor Violations", "Total Traffic Stops"),
                     name = "Stop Type") +
  # Customize month breaks and a black border
  theme(plot.title = element_text(face = "bold"),
        axis.line = element_line(size = 1, color = "black")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") + # Ensures that the x axis goes month by month across the bottom & makes it mm/yyyy
  scale_y_continuous(expand = c(0, 0))  # Set the y-axis minimum to 0
final_line_plot

# To save this plot as a PDF I can use ggsave function
download_path <- file.path(Sys.getenv("HOME"), "Downloads", "EB_TotalplusMinor_plot.pdf") # this specifies the file path of where I want my pdf to be saved
ggsave(download_path, plot = final_line_plot , width = 10, height = 6, units = "in", device = "pdf")


########## LINE PLOT (BY DAY) ########## 
# This is the same code I ran earlier but from the original dat so that I can group by individual days because I got rid of those intially to group by month
dat4 <- dat %>%
  mutate(
    total_violations_count = ifelse(RFS_TRAFFIC_VIOLATION_TYPE %in% c(1, 2, 3), 1, 0),
    minor_violations_count = ifelse(RFS_TRAFFIC_VIOLATION_TYPE %in% c(2, 3), 1, 0)
  ) %>%
  group_by(DATE_OF_STOP) %>%
  summarise(
    total_violations = sum(total_violations_count),
    total_minor_violations = sum(minor_violations_count)
  ) %>%
  ungroup()
# Ran this line of code to make sure R read the column and grouped by the full xxxx/xx/xx format
dat4$DATE_OF_STOP <- as.Date(dat4$DATE_OF_STOP, format = "%Y-%m-%d")
# Now we can plug in the same line plot but with different data so I can see the plot by day
day_line_plot <- ggplot(dat4, aes(x = DATE_OF_STOP)) +
  geom_line(aes(y = total_violations, color = "Total Traffic Stops"), linetype = "solid", size = .50, show.legend = TRUE) + # Here we have 2 y values because I need to show both total stops and total minor stops
  geom_line(aes(y = total_minor_violations, color = "Stops For Minor Violations"), linetype = "solid", size = .50, show.legend = TRUE) +
  # Add a vertical line at March 2022
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "solid", color = "black", size = 1) +  
  # Add a line for the Policy Change in the legend
  geom_line(aes(x = as.Date("2022-03-01"), y = 0, color = "Policy Change"), linetype = "solid", size = 1) +
  # Add labels and title with bolded main title
  labs(
    title = "Total and Minor Traffic Stops Count per Day (Data from LAPD)", # I tried a different title format this time to see what I like better
    subtitle ="March 2022: LAPD Policy Change on Pretextual Stops",
    x = "Traffic Stop Date",
    y = "Number Of Traffic Stops"
  ) +
  # Add legend with custom labels for color & linetype
  scale_color_manual(values = c("Stops For Minor Violations" = "blue", "Total Traffic Stops" = "red"),
                     labels = c("Stops For Minor Violations", "Total Traffic Stops"),
                     name = "Stop Type") +
  # Customize month breaks and a black border
  # Customize month breaks and a black border
  theme(plot.title = element_text(face = "bold"),
        axis.line = element_line(size = 1, color = "black")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") + # Ensures that the x axis goes month by month across the bottom & makes it mm/yyyy
  scale_y_continuous(expand = c(0, 0))  # Set the y-axis minimum to 0

# Same download code as before except we change the plot name
download_path <- file.path(Sys.getenv("HOME"), "Downloads", "EB_daylineplot_plot.pdf") # this specifies the file path of where I want my pdf to be saved
ggsave(download_path, plot = day_line_plot , width = 10, height = 6, units = "in", device = "pdf")


######### PLOT + LAPD EXPENDITURES ######### 

# Load in my spending data
spending <- read.csv("~/Downloads/Police_Spending.csv")

# Select only the columns I need. Transaction date, and amount
spending2 <- spending%>%
  select(TRANSACTION.DATE,
         DOLLAR.AMOUNT)

# Choose my timeline of 09/01/2021 - 08/31/2022
spending1 <- spending1 %>%
  mutate(TRANSACTION.DATE = as.Date(TRANSACTION.DATE, format = "%m/%d/%Y"),
         month_year = format(TRANSACTION.DATE, "%m/%Y")) %>%
  filter(TRANSACTION.DATE >= as.Date("09/01/2021", format = "%m/%d/%Y") & TRANSACTION.DATE <= as.Date("08/31/2022", format = "%m/%d/%Y"))

# Group by and make my dollar amount a count total for each day represented

spending2 <- spending1 %>%
  mutate(TRANSACTION.DATE = as.Date(TRANSACTION.DATE, format = "%m/%d/%Y"),
         month = format(TRANSACTION.DATE, "%m/%Y"))

spending2 <- spending2%>%
  group_by(month) %>%
  summarise(total_dollars_spent = sum(DOLLAR.AMOUNT)) # Now I have 12 observations that I can link up with the old dataset in a merge

spending2 <- spending2 %>%
  mutate(month = as.yearmon(month, format = "%m/%Y")) # This reordered my months to go from oldest - newest in chronological order so it'll match the stop data

spending2 <- spending2%>%
  arrange(month) # Rearranges them!



# Merge the stop data with the spending data now that they have equal #'s of rows

merged <- cbind(dat2, spending2)

merged1 <- merged %>%
  select(DATE,
         total_violations,
         total_minor_violations,
         total_dollars_spent)
merged2 <- merged1 %>%
  mutate(log_high_value_column = log(total_dollars_spent + 1)) # I want to make the total_dollars_spent number smaller so it can plot in clear sight, so I'll try logging it

# Now its time to replot but I will do a much different code for this plot
# The Dollars Spent variable is much too big to be plotted all together so I used Stack Overflow and ChatGPT help in figuring out a way to facet wrap my plot
# I had the idea of adding LAPD expenditures on a montlhy basis into comparison with the total and minor stop data that I already had
# I thought it would be interesting to see if expenditures had any relation to the amount of stops conducted over the same timeline
# LAPD may be purchasing new equipment, cameras, cars, etc. all things that are intended on improving policing and maybe their ability to enforce the law
# So I figured this would be meaningful

# Since I was essentially plotting 3 Y values, facet wrapping each of them seemed like the best way to go so that the viewer can see each in its intended "limits"
# But crucially, it is on the same timeline so you can easily see how one trend may or may not effect the other
# List to store individual plots
merged2_long <- tidyr::pivot_longer(merged2, cols = starts_with("total"), 
                                    names_to = "Y_Variable", values_to = "value")

# Plot with facet_wrap
facet_plot <- ggplot(merged2_long, aes(x = DATE, y = value, color = Y_Variable)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "dashed", color = "black", size = .5) + # made the line dashed and a bit finer so that the line trends of Y's were more visible
  labs(
    title = "Traffic Stops and LAPD Expenditures",
    subtitle = "March 2022: LAPD Policy on Pretextual Stops",
    x = "Date",
    y = "Value (# of Stops & Spent $)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line = element_line(size = 1, color = "black"),
    legend.position = "none"  # Remove the legend because the Y variables are already labelled
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
  facet_wrap(~Y_Variable, scales = "free_y", ncol = 1)

# Print the facet plot
print(facet_plot)


download_path <- file.path(Sys.getenv("HOME"), "Downloads", "EB_expenditures_plot.pdf") # this specifies the file path of where I want my pdf to be saved
ggsave(download_path, plot = facet_plot , width = 10, height = 6, units = "in", device = "pdf")


###### 7 DAY AVERAGE PLOT #####


# This is how I can mutate my existing data to add a column for both total and minor stops that shows the average # of stops for every 7 days in my timeline
meandata <- dat4 %>%
  mutate(total_7day_avg = zoo::rollapply(total_violations, width = 7, FUN = mean, fill = NA, align = "right"), # aligning by right makes sure that it takes data on the right side of each date
         minor_7day_avg = zoo::rollapply(total_minor_violations, width = 7, FUN = mean, fill = NA, align = "right")) %>%
  slice(seq(1, n(), by = 7))



# Then, once I do that I can plug the new data and variable names back into the line ggplot and see how it looks
avg_line_plot <- ggplot(meandata, aes(x = DATE_OF_STOP)) +
  geom_line(aes(y = total_7day_avg, color = "Total Traffic Stops"), linetype = "solid", size = 1, show.legend = TRUE) + # Here we have 2 y values because I need to show both total stops and total minor stops
  geom_line(aes(y = minor_7day_avg, color = "Stops For Minor Violations"), linetype = "dashed", size = 1, show.legend = TRUE) +
  # Add a vertical line at March 2022
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "solid", color = "black", size = 1) +  
  # Add a line for the Policy Change in the legend
  geom_line(aes(x = as.Date("2022-03-01"), y = 0, color = "Policy Change"), linetype = "solid", size = 1) +
  # Add labels and title with bolded main title
  labs(
    title = "Total and Minor Traffic Stops Count (7 Day Moving Avg. from September 2021 - August 2022)",
    subtitle = "March 2022: LAPD Policy Change on Pretextual Stops",
    x = "Traffic Stop Date",
    y = "Number Of Traffic Stops"
  ) +
  # Add legend with custom labels for color & linetype
  scale_color_manual(values = c("Stops For Minor Violations" = "blue", "Total Traffic Stops" = "red"),
                     labels = c("Stops For Minor Violations", "Total Traffic Stops"),
                     name = "Stop Type") +
  # Customize month breaks and a black border
  theme(plot.title = element_text(face = "bold"),
        axis.line = element_line(size = 1, color = "black")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") + # Ensures that the x axis goes month by month across the bottom & makes it mm/yyyy
  scale_y_continuous(expand = c(0, 0))  # Set the y-axis minimum to 0
avg_line_plot

download_path <- file.path(Sys.getenv("HOME"), "Downloads", "EB_7dayavg_plot.pdf") # this specifies the file path of where I want my pdf to be saved
ggsave(download_path, plot = avg_line_plot , width = 10, height = 6, units = "in", device = "pdf")



