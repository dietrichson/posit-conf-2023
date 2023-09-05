# Extract Frames from Video File

# Setup ####
## Select the video file
my_video_filename <- here::here("videos","Inside Out - Vancouver Film School (VFS).mp4")

## Create output folder
my_image_folder <- here::here("video-frames") # if analyzing in bulk, use a different structure
dir.create(my_image_folder) # Will throw warning if the folder already exists.

# Process ####
library(av)
# Get Technical info
av_video_info(my_video_filename) 
av_video_images(my_video_filename, destdir = my_image_folder) -> my_images
