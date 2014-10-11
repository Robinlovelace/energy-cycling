# Disaggregation of time-series scenarios of modal shift
# Input: modal split, distance and demographics of trips per unit time nationally
# Outputs: change in physical activity patterns (health), car trips (%, flow) and energy use

head(ssam)
aggregate(ssam$esave, by = list(ssam$j57g), sum) # any summary statistics can be generated per zone - challenge is to allocate individuals to realistic LAs...

# A spatial microsimulation model is in order, to allocate individuals to LAs
# Variables to constrain by on individual level
# Distance travelled to work
# LC7701EW - Method of travel to work (2001 specification) by distance travelled to work from nomis used - quite course categories used...


