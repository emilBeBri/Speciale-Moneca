# reshape() is an R function that accesses “observations” in grouped dataset columns and “records” in dataset rows, in order to programmatically transform the dataset shape into “long” or “wide” format.


# Required dataframe:
data1 <- data.frame(id=c("ID.1", "ID.2", "ID.3"),
sample1=c(5.01, 79.40, 80.37),
sample2=c(5.12, 81.42, 83.12),
sample3=c(8.62, 81.29, 85.92))




####################
#                  #
#    Exercise 1    #
#                  #
####################


# Wide-to-Long:
# Using the reshape() parameter “direction=“, “varying=” columns are stacked according to the new records created by the “idvar=” column.

# Therefore, convert “data1” to long format, by stacking columns 2 through 4. The new row names are from column “id“. The new time variable is called, “TIME“. The column name of the stacked data is called “Sample“. Set a new dataframe variable called, “data2“.


data2 <- reshape(data1, direction="long", varying=2:4, idvar='id', timevar="TIME", v.names="Sample")

####################
#                  #
#    Exercise 2    #
#                  #
####################

# Long-to-Wide:
# Use direction="wide" to convert “data2” back to the shape of “data1“. Setting a new variable isn’t needed. (Note that rownames from “data2” are retained.)

reshape(data2, direction = "wide")

##          id sample1 sample2 sample3
## ID.1.1 ID.1    5.01    5.12    8.62
## ID.2.1 ID.2   79.40   81.42   81.29
## ID.3.1 ID.3   80.37   83.12   85.92


####################
#                  #
#    Exercise 3    #
#                  #
####################

# Time Variables:
# Script a reshape() operation, where “timevar=” is set to the variable within “data2” that differentiates multiple records.

reshape(data2, timevar = "TIME", idvar = "id", direction = "wide")


##          id Sample.1 Sample.2 Sample.3
## ID.1.1 ID.1     5.01     5.12     8.62
## ID.2.1 ID.2    79.40    81.42    81.29
## ID.3.1 ID.3    80.37    83.12    85.92

####################
#                  #
#    Exercise 4    #
#                  #
####################

# New Row Names:
# Script a reshape() operation, where “data2” is converted to “wide” format, and “new.row.names=” is set to unique “data2$id” names.

reshape(data2, direction = "wide", new.row.names = unique(data2$id))

##        id sample1 sample2 sample3
## ID.1 ID.1    5.01    5.12    8.62
## ID.2 ID.2   79.40   81.42   81.29
## ID.3 ID.3   80.37   83.12   85.92

####################
#                  #
#    Exercise 5    #
#                  #
####################

# Convert “data2” to wide format. Set “v.names=” to the “data2” column with observations.

reshape(data2, timevar = "TIME", idvar = "id", direction = "wide", v.names = "Sample")

##          id Sample.1 Sample.2 Sample.3
## ID.1.1 ID.1     5.01     5.12     8.62
## ID.2.1 ID.2    79.40    81.42    81.29
## ID.3.1 ID.3    80.37    83.12    85.92

####################
#                  #
#    Exercise 6    #
#                  #
####################

# Set sep = "" in order to reshape “data1” to long format.

reshape(data1, direction = "long", varying = 2:4, sep = "")

##          id time sample
## ID.1.1 ID.1    1   5.01
## ID.2.1 ID.2    1  79.40
## ID.3.1 ID.3    1  80.37
## ID.1.2 ID.1    2   5.12
## ID.2.2 ID.2    2  81.42
## ID.3.2 ID.3    2  83.12
## ID.1.3 ID.1    3   8.62
## ID.2.3 ID.2    3  81.29
## ID.3.3 ID.3    3  85.92

####################
#                  #
#    Exercise 7    #
#                  #
####################

# Reshape “data2” to “wide“. Use the “direction =” parameter. Setting a new dataframe variable isn’t required.

reshape(data2, direction = "wide")

##          id sample1 sample2 sample3
## ID.1.1 ID.1    5.01    5.12    8.62
## ID.2.1 ID.2   79.40   81.42   81.29
## ID.3.1 ID.3   80.37   83.12   85.92

####################
#                  #
#    Exercise 8    #
#                  #
####################

# Use the most basic reshape command possible, in order to reshape
“data2” to wide format.

reshape(data2)

##          id sample1 sample2 sample3
## ID.1.1 ID.1    5.01    5.12    8.62
## ID.2.1 ID.2   79.40   81.42   81.29
## ID.3.1 ID.3   80.37   83.12   85.92

####################
#                  #
#    Exercise 9    #
#                  #
####################

# Reshape “data2” to “wide“, with column names for the reshaped data of “TIME” and “Sample“.

reshape(data2, idvar = c("TIME", "Sample"), timevar = "id", direction = "wide")

##        TIME Sample
## ID.1.1    1   5.01
## ID.2.1    1  79.40
## ID.3.1    1  80.37
## ID.1.2    2   5.12
## ID.2.2    2  81.42
## ID.3.2    2  83.12
## ID.1.3    3   8.62
## ID.2.3    3  81.29
## ID.3.3    3  85.92

####################
#                  #
#    Exercise 10   #
#                  #
####################

# Reshape “data1” by varying “sample1“, “sample2“, and “sample3“.

reshape(data1, direction = "long", varying = list(c("sample1", "sample2", "sample3")))

##          id time sample1
## ID.1.1 ID.1    1    5.01
## ID.2.1 ID.2    1   79.40
## ID.3.1 ID.3    1   80.37
## ID.1.2 ID.1    2    5.12
## ID.2.2 ID.2    2   81.42
## ID.3.2 ID.3    2   83.12
## ID.1.3 ID.1    3    8.62
## ID.2.3 ID.2    3   81.29
## ID.3.3 ID.3    3   85.92