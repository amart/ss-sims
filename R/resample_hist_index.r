# R code to resample hist env index
#
# R 3.2.2 64-bit
# ZTA, 2016-02-24
#
# see DAT file in \SS sims\run\CMIP run 20160203\00


idx_len <- 89
nsamples <- 2060 - 2015 + 1


ssh_idx <- matrix(c(
1925, 1, 4, 0.226, 0.168,
1926, 1, 4, 0.991, 4.325,
1927, 1, 4, 0.533, 0.568,
1928, 1, 4, 0.169, 0.085,
1929, 1, 4, 0.411, 0.557,
1930, 1, 4, 0.608, 0.165,
1931, 1, 4, 0.616, 0.534,
1932, 1, 4, 0.195, 0.156,
1933, 1, 4, 0.587, 1.184,
1934, 1, 4, 0.231, 0.333,
1935, 1, 4, 0.683, 0.902,
1936, 1, 4, 0.167, 0.486,
1937, 1, 4, 0.286, 0.259,
1938, 1, 4, 0.480, 0.141,
1939, 1, 4, 1.053, 2.376,
1940, 1, 4, 0.844, 1.116,
1941, 1, 4, 0.473, 0.392,
1942, 1, 4, 0.265, 0.120,
1943, 1, 4, 0.261, 0.570,
1944, 1, 4, 1.588, 0.432,
1945, 1, 4, 1.371, 1.283,
1947, 1, 4, 0.496, 0.624,
1948, 1, 4, 0.045, 0.265,
1949, 1, 4, 0.085, 0.364,
1950, 1, 4, 1.109, 1.298,
1951, 1, 4, 0.477, 0.532,
1952, 1, 4, 0.622, 0.392,
1953, 1, 4, 0.560, 0.631,
1954, 1, 4, 0.380, 0.768,
1955, 1, 4, 1.053, 2.691,
1956, 1, 4, 0.323, 0.346,
1957, 1, 4, 0.489, 0.597,
1958, 1, 4, 0.223, 0.211,
1959, 1, 4, 0.518, 0.619,
1960, 1, 4, 0.618, 0.920,
1961, 1, 4, 0.878, 1.389,
1962, 1, 4, 1.706, 0.420,
1963, 1, 4, 0.579, 0.794,
1964, 1, 4, 3.202, 0.463,
1965, 1, 4, 1.606, 0.876,
1966, 1, 4, 3.631, 0.128,
1967, 1, 4, 1.793, 0.678,
1968, 1, 4, 6.536, 0.201,
1969, 1, 4, 0.733, 0.947,
1970, 1, 4, 7.512, 0.107,
1971, 1, 4, 1.393, 0.520,
1972, 1, 4, 1.160, 1.277,
1973, 1, 4, 7.471, 0.155,
1974, 1, 4, 1.524, 0.666,
1975, 1, 4, 4.937, 0.123,
1976, 1, 4, 2.235, 0.397,
1977, 1, 4, 4.810, 0.304,
1978, 1, 4, 1.537, 0.891,
1979, 1, 4, 3.857, 0.383,
1980, 1, 4, 1.749, 0.441,
1981, 1, 4, 1.669, 0.177,
1982, 1, 4, 1.407, 1.277,
1983, 1, 4, 0.687, 0.546,
1984, 1, 4, 1.171, 1.260,
1985, 1, 4, 2.664, 0.149,
1986, 1, 4, 1.968, 0.188,
1987, 1, 4, 2.497, 0.350,
1988, 1, 4, 1.548, 0.580,
1989, 1, 4, 1.878, 0.367,
1990, 1, 4, 1.571, 0.459,
1991, 1, 4, 2.803, 0.332,
1992, 1, 4, 1.180, 1.653,
1993, 1, 4, 0.256, 0.305,
1994, 1, 4, 1.730, 0.170,
1995, 1, 4, 1.628, 0.393,
1996, 1, 4, 1.047, 3.086,
1997, 1, 4, 0.478, 0.407,
1998, 1, 4, 1.159, 0.999,
1999, 1, 4, 3.255, 0.112,
2000, 1, 4, 1.546, 0.479,
2001, 1, 4, 4.364, 0.177,
2002, 1, 4, 4.116, 0.139,
2003, 1, 4, 1.222, 1.737,
2004, 1, 4, 1.736, 0.181,
2005, 1, 4, 0.637, 0.727,
2006, 1, 4, 0.912, 2.196,
2007, 1, 4, 3.237, 0.318,
2008, 1, 4, 3.530, 0.025,
2009, 1, 4, 2.181, 0.397,
2010, 1, 4, 1.110, 2.022,
2011, 1, 4, 0.902, 1.261,
2012, 1, 4, 0.738, 1.296,
2013, 1, 4, 2.791, 0.113,
2014, 1, 4, 0.833, 1.424),
byrow=TRUE,nrow=idx_len,ncol=5,
dimnames=list(NULL,c("year","seas","idx","obs","se")))




# for repeatability in generating the sets
set.seed(-99199,kind="default",normal.kind="default")


qs <- quantile(ssh_idx[,"obs"],probs=c(0.25,0.75))


seq_idx <- seq(1,idx_len,1)

lower_idx <- which(ssh_idx[,"obs"] < qs[1],arr.ind=TRUE)

upper_idx <- which(ssh_idx[,"obs"] > qs[2],arr.ind=TRUE)

middle_idx <- seq_idx[!(seq_idx %in% c(lower_idx,upper_idx))]




# generate one set of 20 for each quantile set

nvec <- 20

hist_obs <- matrix(0,nrow=nsamples,ncol=(1+3*nvec),dimnames=list(NULL,c("Year",paste("Lower_",seq(1,20),sep=""),paste("Upper_",seq(1,20),sep=""),paste("Middle_",seq(1,20),sep=""))))
hist_obs[,1] <- seq(2015,2060,1)

hist_se  <- matrix(0,nrow=nsamples,ncol=(1+3*nvec),dimnames=list(NULL,c("Year",paste("Lower_",seq(1,20),sep=""),paste("Upper_",seq(1,20),sep=""),paste("Middle_",seq(1,20),sep=""))))
hist_se[,1] <- seq(2015,2060,1)


for(i in 1:nvec)
{
    # lower
    sample_idx <- sample(lower_idx,nsamples,replace=TRUE)

    hist_obs[,(i+1)] <- ssh_idx[sample_idx,"obs"]
    hist_se[,(i+1)]  <- ssh_idx[sample_idx,"se"]

    # upper
    sample_idx <- sample(upper_idx,nsamples,replace=TRUE)

    hist_obs[,(nvec+i+1)] <- ssh_idx[sample_idx,"obs"]
    hist_se[,(nvec+i+1)]  <- ssh_idx[sample_idx,"se"]

    # middle
    sample_idx <- sample(middle_idx,nsamples,replace=TRUE)

    hist_obs[,(2*nvec+i+1)] <- ssh_idx[sample_idx,"obs"]
    hist_se[,(2*nvec+i+1)]  <- ssh_idx[sample_idx,"se"]
}


# write matrices out to CSV files

write.csv(hist_obs,"indices.csv",row.names=FALSE)
write.csv(hist_se,"indices.std_err.csv",row.names=FALSE)

