ADS_Collab_Map
==============

Using R and Python to map out collaborations in Astrophysics Data System articles

The R script in this repository can be used to map connections from one central point to a large number of other points with lines of varying intensity, depending on the values present in the input csv.

This script requires an installation of R (http://www.r-project.org/) with the following libraries installed: maps, geosphere, ggplot2, and tcltk.

How to use this script...

__The Easy Way:__
- Create a csv with the same headers as the template provided. You will need to provide lat, long, and totalCount.
  - The first row will be used as the central point, and will not be included in the visualization directly.
- Open and run Collab_map.R. This will prompt you for a save location, a file to work from, and several parameters to determine how the map will be output.
  - Map Colors determine the color palette to be used
  - Map Area determines the area to be rendered
  - Map Type determines how the information will be visualized: with great circle lines or with dots of varying sizes.
- Once your parameters are selected, a file will be created in your save location when the script is done running.

__The Hard (but rewarding!) Way:__
You can edit the color and area options by which the script runs fairly easily, if you are comfortable mucking about in the source code. All you have to do is copy the marked sections and replace the values with ones of your preference. The color values in the pallette section are labeled for what they color, but the area option is a bit more obtuse. All that needs to be changed, however, are the xlim and ylim values, which limit the view to within those longitude and latitude ranges.

If you have any questions about the script, or want to use it and can't, shoot me an email: jaguillette (at) gmail (dot) com
