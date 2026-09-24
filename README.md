Contents

cleaned.zip - Contains all data required to run the analyses for this work. In the code, this folder is pointed to in a "data/" path. Included are:

  Collated but otherwise unprocessed data from both GPS devices used in this study;
  
  GLS data, both light and immersion, in the form of list style R objects;
  
  Radar data from AxyTrek tags, used to search for nocturnal vessel interactions in the tracks;
  
  Processed trip data, where foraging trips are identified from location data, and colony locations are removed (< 1km from the nest);
  
  Processed trip data, same as above, but all interpolated to regular 10 minute fix intervals;
  
  EMBC state, Wind speed and direction, solar angle, lunar angle and fraction, 
  number of landings and proportion of time spent wet per GPS interval all appended to trip dfs;
  
  

0_prep - Loads in some packages and objects to run the initial data cleaning;

1_data_cleaning_26 - Loads in raw logger data from GPS devices, collates, sorts into trips, interpolates !!CAN BE SKIPPED BY LOADING trip_df AND/OR trip_df_int;

2_embc_26 - Runs EMBC algorithm on track data and appends inferred states to data !!CAN BE SKIPPED BY LOADING trip_df AND/OR trip_df_int;

3_gls_26 - Loads in raw logger data from GLS devices, then used to append immersion data to GPS, and check for light spikes at night;

4_wind_26 - Appends wind data, available from Copernicus' ERA5 dataset, to location data !!CAN BE SKIPPED BY LOADING trip_df AND/OR trip_df_int;

5_nocturnal_26 - Contains all models that correlate behaviour with environment, and describe and visualise behaviour at night;

6_big_plots_26 - Collates plots from tracking data and models to provide the 3 plots included in the manuscript

