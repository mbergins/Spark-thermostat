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
library(BerginskiRMisc)
library(grid)

temp = read.csv('last_week.csv') %>%
  mutate(myTime = ymd_hms(Time, tz="EST") + hours(1)) %>%
  arrange(desc(myTime))

#missing values throw an error with ggplot, replacing missing vals with 0
temp$Relay[is.na(temp$Relay)] = 0;
temp$Relay = temp$Relay * temp$Time;
temp$Relay[temp$Relay == 0] = NA;

tempPlot = ggplot(temp,aes(x=myTime)) +
  geom_vline(aes(xintercept = Relay, color=tempMode),size=0.1,alpha=0.25) +
  geom_line(aes(y=Target_temp,color="Target"),alpha=0.5) +
  geom_line(aes(y=Freezer_temp,color="Freezer"),alpha=0.9) +
  geom_line(aes(y=Outside_Temp,color="Outside"),alpha=0.9) +
  # scale_color_brewer("",type = "qual",palette = "Dark2") +
  ylab('Temperature (°F)') +
  xlab('Time') +
  theme_berginski() +
  coord_cartesian(ylim=c(30,100)) +
  scale_x_datetime() +
  # scale_x_continuous("Time (days ago)",breaks = c(0:7), expand=c(0,0)) +
  scale_color_manual(values = c("Blue","Red","White","LightBlue","Green","Gray10"),
                     limits = c("Cold","Hot","Neither","Freezer","Outside","Target")) +
  labs(color = "") +
  theme(text = element_text(size=6),
        axis.title.x=element_text(margin=margin(1.5,0,0,0)),
        axis.title.y=element_text(margin=margin(0,1.5,0,0)))


ggsave(file.path(args$folder,'week.jpg'),tempPlot,width=4.25,height=2)
trimImage(file.path(args$folder,'week.jpg'))

tempDay = temp %>%
  filter(myTime >= max(temp$myTime,na.rm=T) - days(1))

tempPlotDay = tempPlot %+% tempDay +
  scale_x_datetime(date_breaks = "1 hour", date_labels = '%H')

ggsave(file.path(args$folder,'day.jpg'),tempPlotDay,width=4.25,height=2)
trimImage(file.path(args$folder,'day.jpg'))
