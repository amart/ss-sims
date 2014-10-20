# Creative Commons License
# Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)
# https://creativecommons.org/licenses/by-nc-sa/4.0/
# 
# Run SS simulations
# ZTA, initial version 2014-10-13
# R version 3.1.1, 32-bit
#

devtools::install_github("amart/r4ss")
library(r4ss)

library(Hmisc)
library(stringr)


# NOTE:  these variables are set for testing purposes
num_proj_years <-10
std_err <- 0.95
seas <- 1
index_fleet <- 4


source_dir  <- "F:\\Folder\\w\\dev\\SS sims\\R"

working_dir <- "F:\\Folder\\w\\dev\\SS sims\\run"


ss_exe_file <- "ss3.exe"

starter_file <- "starter.ss"
forecast_file <- "forecast.ss"
dat_file <- "2011_sablefish_data.ss"
ctl_file <- "2011_sablefish_control.ss"

idx_file <- "indices.csv"



setwd(working_dir)


index_data <- read.csv(idx_file,header=TRUE,numerals="no.loss")
dim(index_data)

nrows_data <- dim(index_data)[1]
num_indices <- (dim(index_data)[2]) - 1
index_names <- names(index_data)[-1]


if (nrows_data < 1 || num_indices < 1)
{
    print("Error with index file")
    return(1)
}


# number of projections for each index; limited to max of 99
num_proj <- min(min(99,num_proj_years),nrows_data)


source(paste(source_dir,"\\sim_functions.r",sep=""))


dat_struct <- SS_readdat(dat_file)
ctl_struct <- readLines(paste(ctl_file,".for_proj",sep=""))   # will need to update if and when SS_readctl() and SS_writectl() are completed
fc_struct  <- SS_readforecast(forecast_file,Nfleets=dat_struct$Nfleet,Nareas=dat_struct$N_areas)

# not necessary for now
# start_struct <- SS_readstarter(starter_file)



# directory for run in starting year
base_dir <- "00"
dir.create(base_dir)

copy_sim_files_into_dir(base_dir)


setwd(base_dir)

system(paste(ss_exe_file,"-nox",sep=" "),intern=FALSE,ignore.stderr=TRUE,wait=TRUE,
       input=NULL,show.output.on.console=FALSE,minimized=FALSE,invisible=FALSE)


setwd(working_dir)

# base_run <- SS_output(base_dir,forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE)

# get the forecasted catch by fleet
base_fc_file    <- file.path(base_dir,"Forecast-report.sso")
base_catch_proj <- get_forecast_catch_by_fleet(base_fc_file,dat_struct$Nfleet,dat_struct$nseas,fc_struct$Nforecastyrs)

# get the projected catch total and catch fraction
if (!is.null(base_catch_proj))
{
    base_catch_tot  <- sum(base_catch_proj)
    base_catch_frac <- base_catch_proj / sum(base_catch_proj)
} else {
    base_catch_tot  <- 0.0
    base_catch_frac <- rep(0.0,dat_struct$Nfleet)
    print("Warning:  bad forecasted catch in base run")
}



# main loop for projections
for (i in 1:num_indices)
{
    idx_name <- str_trim(index_names[i],side="both")
    index_names[i] <- idx_name

    new_dat_struct <- dat_struct
    new_ctl_struct <- ctl_struct
    new_fc_struct  <- fc_struct

    catch_year <- dat_struct$endyr
    catch_tot  <- base_catch_tot
    catch_frac <- base_catch_frac

    for (j in 1:num_proj)
    {
        setwd(working_dir)

        if (j > 1)
        {
            # save the previous directory if this is not the first run for this index
            prev_dir <- new_dir

            # get catch by fleet from previous year's Forecast-report.sso file
            fc_file    <- file.path(prev_dir,"Forecast-report.sso")
            catch_proj <- get_forecast_catch_by_fleet(fc_file,dat_struct$Nfleet,dat_struct$nseas,fc_struct$Nforecastyrs)

            if (!is.null(catch_proj))
            {
                catch_tot  <- sum(catch_proj)
                catch_frac <- catch_proj / sum(catch_proj)
            } else {
                catch_tot  <- 0.0
                catch_frac <- rep(0.0,dat_struct$Nfleet)
                print(paste("Warning:  bad forecasted catch in index ",i," run ",(j-1),sep=""))
            }
        }

        new_dir <- paste(idx_name,"\\",sprintf("%02d",j),sep="")
        dir.create(new_dir,recursive=TRUE)

        # copy files from current directory into directory for initial run
        copy_sim_files_into_dir(new_dir)


        setwd(new_dir)


        curr_year  <- catch_year
        catch_year <- catch_year + 1


        # edit the DAT file
        new_dat_struct <- sim_set_endyr(new_dat_struct,catch_year)

        # calculate fraction of total annual catch for each fleet and add to catch
        catch_vec <- catch_tot * catch_frac
        new_dat_struct <- sim_add_catch(new_dat_struct,catch_vec,seas,catch_year)

        # add index for endyr to CPUE
        idx_yr <- which(index_data$Year == new_dat_struct$endyr,arr.ind=TRUE)
        if (idx_yr > 0)
        {
            new_dat_struct <- sim_add_index_as_CPUE(new_dat_struct,catch_year,seas,index_fleet,index_data[idx_yr,(i+1)],std_err)
        }

        SS_writedat(new_dat_struct,dat_file,overwrite=TRUE,verbose=TRUE)


        # edit the forecast file
        new_fc_struct$FirstYear_for_caps_and_allocations <- catch_year + 1

        SS_writeforecast(new_fc_struct,file=forecast_file,overwrite=TRUE,verbose=TRUE)


        # edit the CTL file
        # change catch_year to catch_year+1 in bias adjustment section
        new_ctl_struct <- sedit(new_ctl_struct,as.character(catch_year),as.character(catch_year+1))

        # change curr_year to curr_year+1 in bias adjustment section
        new_ctl_struct <- sedit(new_ctl_struct,as.character(curr_year),as.character(catch_year))

        writeLines(new_ctl_struct,ctl_file)


        system(paste(ss_exe_file,"-nox",sep=" "),intern=FALSE,ignore.stderr=TRUE,wait=TRUE,
               input=NULL,show.output.on.console=FALSE,minimized=FALSE,invisible=FALSE)


        setwd(working_dir)

    }
}


setwd(working_dir)

# check output
new_run <- SS_output(new_dir,forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE)
SS_plots(new_run)

