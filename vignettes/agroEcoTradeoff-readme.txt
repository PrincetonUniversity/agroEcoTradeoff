tradeoff_mod.R:
Calls the input_handler to read in the input data tables and proceeds through
the four modules (target, constraints, convert, and impacts).

input_handler.R:
Reads in the input data tables by calling fetch_inputs.R and modifies potential
yield and potential production current area (yield_mod_dt2) based on the yield
modifications given as an argument to tradeoff_mod.R. Calculates conversion 
probabilities for carbon, biodiversity, and cost by dividing by yield, 
normalizing, and subtracting from 1, giving a unique conversion probability 
for each pixel for each crop within each constraint. Protected area loss is
currently being used as the sole criteria to be minimized for the biodiversity
constraint.

fetch_inputs.R:
Reads in the input data tables from external/ext_data/dt. Modified so that it 
does not read in conversion probability tables. These are calculated in
input_handler.R instead.

yield_mod.R
Modifies yields by provided multipliers. Modified so that potential yields are
normalized by area/production so that area becomes a constraint that is 
minimized with the same standardization as carbon, cost, and biodiversity.

targets.R
Calculates the production target that needs to be hit for each crop. Modified
to downsize the production of each pixel in accordance with the disaggregation
of the input grids.

constraints_dt.R
Determines probability of conversion for each pixel. Conversion probability is
calculated as a weighted sum of the four constraints using weights given as 
arguments to tradeoff_mod.R. 

convert.R
Approximates an optimal combination of pixels to convert to each crop to meet
production targets. Each pixel is either assigned to one crop or no crops at 
all. Put simply, the algorithm iteratively assigns each crop the pixels with
the highest probability of conversion for that crop, sometimes "stealing"
pixels from a different crop if the pixels are determined to be more valuable
to the crop being considered. This process is performed until all production
targets are met.

impacts.R
Calculates the impacts based upon the conversions. 


Note: The conversion fraction that previously determined the percentage of 
each pixel that can be converted to each crop stil exists in the model, but
it is not currently being used in any module. We will reimplement it with the
percentage of each pixel available for cropland (once we have better landcover
data, I assume).  
