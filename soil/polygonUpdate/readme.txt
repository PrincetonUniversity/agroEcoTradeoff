Mapping of Soil Profiles to Shapefile Polygons

The shapefile is zari\landcover\Spatial Data_FD_ILUAII\zamsoils.shp
The soil profiles are from zari\soil\Zambia_soil_profiles.xlsx

Based on the shapefile, I calculated the centroid of each polygon and outputted 
the attribute table to zamsoils_centroid.csv

I also manually went through each distinct soilsymbol on the tiff file
corresponding to the shapefile, and listed the associated soiltypes in 
Zambia_soil_classify.csv 

The script profToPoly.R was my first attempt at linking the profiles to the
polygons. It uses the coordinates of the centroids and finds the nearest
profile based on coordinates in Zambia_soil_profiles. This initial attempt was
deemed unsuccessful because of the low percentage of matches in soil 
classification.

The script profToPolyByClass.R takes a more sophisticated approach:

1. Assign the appropriate soiltypes to each of the shapefile polygons using 
   Zambia_soil_classify.csv
2. Restructure the table of profiles so that each unique set of profile 
   coordinates appears on its own line with all measured horizons
3. Find an appropriate subset of candidate profiles for each polygon
	a. Try to match the classification exactly (e.g. Carbic Podzol)
	b. If no exact matches, try to match the first level class (e.g. Podzol)
	c. If no partial matches, use the entire set of profiles
4. Using the Kd-trees algorithm, find the profile within the subset of
   candidates that is nearest spatially to the centroid of each polygon. Assign
   this profile as the best match.
5. Create a new attribute table using soil ID as the common attribute for the 
   join with every polygon's best match profile assigned to the correct ID.
6. Create a corresponding .csvt file with the appropriate variable type
   designations for the fields in the attribute table. 
   
The resulting files are zari\landcover\Spatial Data_FD_ILUAII\Zambia_soil_at.csv
and zari\landcover\Spatial Data_FD_ILUAII\Zambia_soil_at.csvt. Both 
zari\landcover\Spatial Data_FD_ILUAII\zamsoils_mps.shp and
zari\landcover\Spatial Data_FD_ILUAII\zamsoils_mps_albers.shp are joined with
the new attribute table.