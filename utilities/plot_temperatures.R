#!/usr/bin/Rscript --vanilla

###############################################################################
# Command Line Processing
###############################################################################
suppressPackageStartupMessages(library(argparse));

# create parser object
parser <- ArgumentParser()

# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument("-f", "--folder", type="character",
                    help="Folder to save images",
                    default = '.',
                    metavar="folder")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
args <- parser$parse_args()

#check for folder existance
if (is.null(args$folder)) {
  print("Expected one parameter: a target folder for the images, see usage data`.");
  parser$print_help();
  quit();
}

###############################################################################
# Plotting
###############################################################################

dir.create(args$folder, showWarnings = FALSE)

library(ggplot2)
library(lubridate)
library(dplyr)
library(BerginskiRMisc)
library(grid)
library(scales)

temp = read.csv('last_week.csv') %>%
  mutate(myTime = ymd_hms(Time, tz="EST") + hours(1)) %>%
  mutate(relayTime = ifelse(Relay == 1, myTime, NA)) %>%
  arrange(desc(myTime))

tempPlot = ggplot(temp,aes(x = myTime)) +
  geom_vline(aes(xintercept = relayTime, color = tempMode), size = 0.1,alpha = 0.1) +
  geom_line(aes(y=Target_temp,color = "Target"), alpha = 0.5) +
  geom_line(aes(y=Freezer_temp,color = "Freezer"), alpha = 0.9) +
  geom_line(aes(y=Outside_Temp,color = "Outside"), alpha = 0.9) +
  ylab('Temperature (\u00B0F)') +
  xlab('Time') +
  coord_cartesian(ylim = c(30,100)) +
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%d/%m/%y")) +
  scale_color_manual(values = c("Blue","Red","White","LightBlue","Green","Gray10"),
                     limits = c("Cold","Hot","Neither","Freezer","Outside","Target")) +
  labs(color = "") +
  theme_berginski() +
  theme(text = element_text(size=8))


ggsave(file.path(args$folder,'week.jpg'),tempPlot,width=4.25,height=2)
#trimImage(file.path(args$folder,'week.jpg'))

tempDay = temp %>%
  filter(myTime >= max(temp$myTime,na.rm=T) - days(1))

tempPlotDay = tempPlot %+% tempDay +
  scale_x_datetime(date_breaks = "1 hour", date_labels = '%H')

ggsave(file.path(args$folder, 'day.jpg'), tempPlotDay, width=4.25, height=2)
trimImage(file.path(args$folder, 'day.jpg'))
