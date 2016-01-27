Getting and Cleaning Data Course Project
=================

Original data 
-----------------

Orignal data was obtained from [archive.ics.uci.edu site][DataSite] and available by [link][DataLink].

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

**Credentials:**

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.  
Smartlab - Non Linear Complex Systems Laboratory  
DITEN - Università degli Studi di Genova.  
Via Opera Pia 11A, I-16145, Genoa, Italy.  
activityrecognition@smartlab.ws  
www.smartlab.ws

### Feature Selection 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* maxInds(): index of the frequency component with largest magnitude
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

Modifications
-------------

The original data set is heavily fragmented and is hard to read. run_analisis.R script simplifies it and creates *tidy_data.txt* file with more clear data. Resulted data set has list of variables listed below. *Subject* variable represents subject's id in the experiment, *Activity* represents the activity of subject during the experiment. All other variables show the average of corresponded values in original data set, grouped by subject and activity (only variable of mean and std from original data set are used) For example, MeanTimeBodyAccX is calculated using tBodyAcc-mean()-X variable.

* Subject
* Activity
* TimeMeanBodyAccX
* TimeMeanBodyAccY
* TimeMeanBodyAccZ
* TimeStdBodyAccX
* TimeStdBodyAccY
* TimeStdBodyAccZ
* TimeMeanGravityAccX
* TimeMeanGravityAccY
* TimeMeanGravityAccZ
* TimeStdGravityAccX
* TimeStdGravityAccY
* TimeStdGravityAccZ
* TimeMeanBodyAccJerkX
* TimeMeanBodyAccJerkY
* TimeMeanBodyAccJerkZ
* TimeStdBodyAccJerkX
* TimeStdBodyAccJerkY
* TimeStdBodyAccJerkZ
* TimeMeanBodyGyroX
* TimeMeanBodyGyroY
* TimeMeanBodyGyroZ
* TimeStdBodyGyroX
* TimeStdBodyGyroY
* TimeStdBodyGyroZ
* TimeMeanBodyGyroJerkX
* TimeMeanBodyGyroJerkY
* TimeMeanBodyGyroJerkZ
* TimeStdBodyGyroJerkX
* TimeStdBodyGyroJerkY
* TimeStdBodyGyroJerkZ
* TimeMeanBodyAccMag
* TimeStdBodyAccMag
* TimeMeanGravityAccMag
* TimeStdGravityAccMag
* TimeMeanBodyAccJerkMag
* TimeStdBodyAccJerkMag
* TimeMeanBodyGyroMag
* TimeStdBodyGyroMag
* TimeMeanBodyGyroJerkMag
* TimeStdBodyGyroJerkMag
* FreqMeanBodyAccX
* FreqMeanBodyAccY
* FreqMeanBodyAccZ
* FreqStdBodyAccX
* FreqStdBodyAccY
* FreqStdBodyAccZ
* FreqMeanBodyAccJerkX
* FreqMeanBodyAccJerkY
* FreqMeanBodyAccJerkZ
* FreqStdBodyAccJerkX
* FreqStdBodyAccJerkY
* FreqStdBodyAccJerkZ
* FreqMeanBodyGyroX
* FreqMeanBodyGyroY
* FreqMeanBodyGyroZ
* FreqStdBodyGyroX
* FreqStdBodyGyroY
* FreqStdBodyGyroZ
* FreqMeanBodyAccMag
* FreqStdBodyAccMag
* FreqMeanBodyBodyAccJerkMag
* FreqStdBodyBodyAccJerkMag
* FreqMeanBodyBodyGyroMag
* FreqStdBodyBodyGyroMag
* FreqMeanBodyBodyGyroJerkMag
* FreqStdBodyBodyGyroJerkMag

[//]: #
   [DataSite]: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>
   [DataLink]: <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>