# Run SS simulations
# ZTA, 2014-10-13
# R version 3.1.1, 32-bit

# at some point, there will be a function to parse the starter.ss file to determine the names
# of the DAT and CTL files.  for now it will input the directory, the names of the DAT and CTL files,
# the file with the environmental indices, and the number of years to project forward
#

# what is already installed?
installed.local <- library()$results[,"Package"]

# install what is missing
needed.local <- c("devtools","foreach","doParallel","Hmisc","stringr","stringi","gdata")
needed.installed <- needed.local %in% installed.local
needed.remaining <- needed.local[!needed.installed]
if (length(needed.remaining) > 0)
{
    install.packages(needed.remaining)
}

library(gdata)

devtools::install_github("amart/r4ss")
library(r4ss)


library(foreach)
library(doParallel)
library(Hmisc)
library(stringr)
library(stringi)
library(gdata)


# for repeatability in generating the rec devs
set.seed(-99199,kind="default",normal.kind="default")


# NOTE:  these variables are set for testing purposes
num_proj_years <- 46 # 2015 - 2060


index_seas <- 1
index_fleet <- 4


num_proj_fleets <- 4
proj_fleet <- c(1,2,3,8)
proj_fleet_seas <- c(1,1,1,1)
proj_fleet_name <- c("HKL","POT","TWL","NWCBO")
proj_fleet_len_comp_gender <- c(0,0,0,3)
proj_fleet_len_comp_part <- c(2,2,2,0)
proj_fleet_age_comp_gender <- c(3,3,3,3)
proj_fleet_age_comp_part <- c(2,2,2,0)




working_dir <- "E:/dev/SS sims/run"




# ~~~~~~~~~~~ variables set for specific simulations ~~~~~~~~~~~

# num of CPUs for projections running in parallel
num_cpus <- 50001


ss_exe_file <- "ss3.exe"

starter_file <- "starter.ss"
starter_calc_only_file <- paste(starter_file,".for_proj",sep="")
forecast_file <- "forecast.ss"
dat_file <- "DAT.ss"
ctl_file <- "CTL.ss"
est_dat_file <- "data.ss_new"

par_file <- "ss3.par"

idx_file <- "indices.csv"
idx_std_err_file <- "indices.std_err.csv"



setwd(working_dir)



# read in and check index file
index_data <- read.csv(idx_file,header=TRUE,numerals="no.loss")
dim(index_data)

nrows_data <- dim(index_data)[1]
num_indices <- (dim(index_data)[2]) - 1     # account for the year in the first column
index_names <- names(index_data)[-1]


if (nrows_data < 1 || num_indices < 1)
{
    cat("Error with index file\n")
    return(-1)
}


# read in and check index std err file
index_std_err <- read.csv(idx_std_err_file,header=TRUE,numerals="no.loss")
dim(index_std_err)

if (dim(index_std_err)[1] != nrows_data || (dim(index_std_err)[2] - 1) != num_indices)
{
    cat("Error with index std err file\n")
    return(-1)
}



# number of projections for each index; limited to max of 99
num_proj_years <- min(min(99,num_proj_years),nrows_data)

cat(paste("\nNumber of projection years for each index:  ",num_proj_years,"\n\n",sep=""))




paste_dir_file <- function(paste_dir,paste_filename)
{
    return(paste(paste_dir,"/",paste_filename,sep=""))
}



copy_sim_files_into_dir <- function(copy_from_dir,copy_to_dir)
{
    file.copy(paste_dir_file(copy_from_dir,ss_exe_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,starter_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,forecast_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,dat_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,ctl_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
}



copy_sim_calc_only_files_into_dir <- function(copy_from_dir,copy_to_dir)
{
    file.copy(paste_dir_file(copy_from_dir,ss_exe_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,starter_calc_only_file),paste_dir_file(copy_to_dir,starter_file),copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,forecast_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,dat_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(paste_dir_file(copy_from_dir,ctl_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
}



copy_sim_run_files_into_dir <- function(copy_from_dir,copy_to_dir)
{
    file.copy(paste_dir_file(copy_from_dir,dat_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE,overwrite=TRUE)
    file.copy(paste_dir_file(copy_from_dir,ctl_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE,overwrite=TRUE)
    file.copy(paste_dir_file(copy_from_dir,forecast_file),copy_to_dir,copy.mode=TRUE,copy.date=TRUE,overwrite=TRUE)
}







start_struct <- SS_readstarter(starter_file)
dat_struct   <- SS_readdat(dat_file)
ctl_struct   <- readLines(paste(ctl_file,".for_proj",sep=""))   # will need to update if and when SS_readctl() and SS_writectl() are completed
fc_struct    <- SS_readforecast(forecast_file,Nfleets=dat_struct$Nfleet,Nareas=dat_struct$N_areas)





# directory for run in starting year
base_dir <- "00"
dir.create(base_dir)

copy_sim_files_into_dir(working_dir,base_dir)


setwd(base_dir)

system(paste(ss_exe_file,"-nox","-cbs 1000000000",sep=" "),intern=FALSE,ignore.stderr=TRUE,wait=TRUE,
       input=NULL,show.output.on.console=FALSE,minimized=FALSE,invisible=FALSE)


setwd(working_dir)


# get the results of the base run
base_rep_struct <- SS_output(base_dir,forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE,covar=FALSE)

# get sigmaR
base_sigmaR <- base_rep_struct$parameters[base_rep_struct$parameters$Label=="SR_sigmaR",]$Value
env_idx_var_frac <- 0.35
base_var_frac <- 1.0 - env_idx_var_frac
proj_sigmaR <- base_var_frac * base_sigmaR


# get the estimated parameters from the base run
base_par_struct <- readLines(paste_dir_file(base_dir,par_file))

# get the historical rec dev vector
rec_dev_line <- 1 + grep("# recdev1:",base_par_struct)
rec_devs <- as.double(str_split(str_trim(base_par_struct[rec_dev_line])," ")[[1]])
rec_dev_hist_max <- max(rec_devs)


# get the forecasted catch by fleet
base_catch_proj <- sim_get_forecast_catch_by_fleet(base_dir,dat_struct$Nfleet,dat_struct$nseas,fc_struct$Nforecastyrs)

# get the projected catch total and catch fraction
if (!is.null(base_catch_proj))
{
    base_catch_tot  <- sum(base_catch_proj)
    base_catch_frac <- base_catch_proj / sum(base_catch_proj)
} else {
    base_catch_tot  <- 0.0
    base_catch_frac <- matrix(0.0,nrow=dat_struct$nseas,ncol=dat_struct$Nfleet)
    cat("\nWarning:  bad forecasted catch in base run\n\n")
}




# get the mean index std err, length and age sample sizes, and discard std err for the projected fleets
proj_fleet_cpue_std_err    <- rep(0.0,num_proj_fleets)
proj_fleet_len_comp_N      <- rep(0.0,num_proj_fleets)
proj_fleet_age_comp_N      <- rep(0.0,num_proj_fleets)
proj_fleet_discard_average <- rep(0.0,num_proj_fleets)
proj_fleet_discard_std_err <- rep(0.0,num_proj_fleets)

for (f in 1:num_proj_fleets)
{
    proj_fl <- proj_fleet[f]

    proj_fleet_cpue_std_err[f] <- max(0.001,mean(dat_struct$CPUE[which(dat_struct$CPUE$index == proj_fl,arr.ind=TRUE),]$se_log))

    proj_fleet_len_comp_N[f]   <- min(400,max(10,floor(mean(dat_struct$lencomp[which(dat_struct$lencomp$FltSvy == proj_fl,arr.ind=TRUE),]$Nsamp))))

    proj_fleet_age_comp_N[f]   <- min(400,max(10,floor(mean(dat_struct$agecomp[which(dat_struct$agecomp$FltSvy == proj_fl,arr.ind=TRUE),]$Nsamp))))

    if (dat_struct$N_discard_fleets > 0 && f %in% dat_struct$discard_fleet_info$Fleet)
    {
        proj_fleet_discard_average[f] <- 0.0
        proj_fleet_discard_std_err[f] <- 0.001

        fleet_discard <- subset(dat_struct$discard_data,dat_struct$discard_data$Flt == f)
        nrows_discard <- nrow(fleet_discard)

        if (nrows_discard > 0)
        {
            # get the average of the most recent 5 values
            if (nrows_discard > 5)
            {
                exclude_idx <- nrows_discard - 5
                obs_avg     <- mean(fleet_discard$Discard[-exclude_idx:-1])
                std_err_avg <- mean(fleet_discard$Std_in[-exclude_idx:-1])
            } else {
                obs_avg     <- mean(fleet_discard$Discard)
                std_err_avg <- mean(fleet_discard$Std_in)
            }

            proj_fleet_discard_average[f] <- obs_avg
            proj_fleet_discard_std_err[f] <- max(0.001,std_err_avg)

            # there are no discard obs for 2010, so add them
            dat_struct <- sim_add_discard(dat_struct,dis_year=2010,dis_seas=proj_fleet_seas[f],dis_fleet=proj_fleet[f],dis_obs=proj_fleet_discard_average[f],dis_std_err=proj_fleet_discard_std_err[f])
        }
    }
}









# ~~~~~~~~~~~ function to run projections for a specific index number ~~~~~~~~~~~
do_projections_for_index <- function(index_num=-1)
{
    library(gdata)
    library(r4ss)
    library(Hmisc)
    library(stringr)
    library(stringi)
    library(gdata)

    if (index_num >= 1 && index_num <= num_indices)
    {
        idx_name <- str_trim(index_names[index_num],side="both")
        index_names[index_num] <- idx_name

        cat(paste("\nStarting projections for index ",idx_name,"\n\n",sep=""))

        new_dat_struct <- dat_struct
        new_ctl_struct <- ctl_struct
        new_fc_struct  <- fc_struct

        catch_year <- dat_struct$endyr
        catch_tot  <- base_catch_tot
        catch_frac <- base_catch_frac

        prev_par_struct <- base_par_struct
        prev_rep_struct <- base_rep_struct

        for (j in 1:num_proj_years)
        {
            setwd(working_dir)

            if (j > 1)
            {
                # save the previous directory if this is not the first run for this index
                prev_dir <- new_dir

                # get the results from the previous run
                prev_rep_struct <- SS_output(prev_dir,forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE,covar=FALSE)

                # get the estimated parameters from the previous run
                prev_par_struct <- readLines(paste_dir_file(prev_dir,par_file))

                # get catch by fleet from previous year's Forecast-report.sso file
                catch_proj <- sim_get_forecast_catch_by_fleet(prev_dir,dat_struct$Nfleet,dat_struct$nseas,fc_struct$Nforecastyrs)

                if (!is.null(catch_proj))
                {
                    catch_tot  <- sum(catch_proj)
                    catch_frac <- catch_proj / sum(catch_proj)
                } else {
                    catch_tot  <- 0.0
                    catch_frac <- matrix(0.0,nrow=dat_struct$nseas,ncol=dat_struct$Nfleet)
                    cat(paste("\nWarning:  bad forecasted catch in index ",idx_name," run ",(j-1),"\n\n",sep=""))
                }
            }

            new_dir <- paste_dir_file(idx_name,sprintf("%02d",j))
            new_calc_only_dir <- paste_dir_file(new_dir,"calc_only")
            dir.create(new_calc_only_dir,recursive=TRUE)


            # ||||||||||||||||| THE (length comp) CALCULATION RUN |||||||||||||||||


            # copy files from the initial directory for the length comp calculation run
            copy_sim_calc_only_files_into_dir(working_dir,new_calc_only_dir)

            setwd(new_calc_only_dir)


            curr_year  <- catch_year
            catch_year <- catch_year + 1


            # &&&&&&&&&&&&&&&&& edit the DAT file &&&&&&&&&&&&&&&&&
            new_dat_struct <- sim_set_endyr(new_dat_struct,catch_year)

            # calculate fraction of total annual catch for each fleet and add to catch
            catch_mat <- catch_tot * catch_frac
            new_dat_struct <- sim_add_catch(new_dat_struct,catch_mat,catch_year)

            # add env index for endyr to CPUE
            idx_yr <- which(index_data$Year == new_dat_struct$endyr,arr.ind=TRUE)
            if (idx_yr > 0)
            {
                new_dat_struct <- sim_add_index_as_CPUE(new_dat_struct,catch_year,index_seas,index_fleet,index_data[idx_yr,(index_num+1)],index_std_err[idx_yr,(index_num+1)])
            }

            # generate "data" for the projected fleets
            for (f in 1:num_proj_fleets)
            {
                proj_fl <- proj_fleet[f]
                proj_ss <- proj_fleet_seas[f]
                proj_se <- proj_fleet_cpue_std_err[f]

                # add generated srv index for endyr to CPUE for proj_fleet
                if (is.numeric(proj_se) && !is.na(proj_se))
                {
                    cpue_struct <- sim_generate_CPUE(new_dat_struct,prev_rep_struct,catch_year,proj_ss,proj_fl,proj_se,apply_error=TRUE)
                    if (proj_se > 0.0 && cpue_struct$obs > 0.0)
                    {
                        new_dat_struct <- sim_add_CPUE(new_dat_struct,catch_year,proj_ss,proj_fl,cpue_struct$obs,proj_se)
                    }
                }

                # add average of historical values for discard for proj_fleet
                if (new_dat_struct$N_discard_fleets > 0 && f %in% new_dat_struct$discard_fleet_info$Fleet)
                {
                    new_dat_struct <- sim_add_discard(new_dat_struct,dis_year=catch_year,dis_seas=proj_fleet_seas[f],dis_fleet=proj_fleet[f],dis_obs=proj_fleet_discard_average[f],dis_std_err=proj_fleet_discard_std_err[f])
                }
            }

            # save the DAT file struct; without the length and age comp data, it is the same for the calc-only run and the main run
            save_new_dat_struct <- new_dat_struct

            # number of bins, per sex, to use for *generating* multinomial composition values with the bootstrap function
            nsamples_age <- sim_calculate_nsamples(new_dat_struct$N_agebins)
            nsamples_len <- sim_calculate_nsamples(new_dat_struct$N_lbins)

            # generate dummy length and age comp "data" for the projected fleets
            for (f in 1:num_proj_fleets)
            {
                proj_fl <- proj_fleet[f]
                proj_ss <- proj_fleet_seas[f]

                # add generated srv age comps for endyr to agecomp for proj_fleet
                agecomp_struct <- sim_generate_age_comp(new_dat_struct,prev_rep_struct,catch_year,proj_ss,proj_fl,apply_error=FALSE)
                agecomp_struct <- sim_map_pop_age_to_data_age(new_dat_struct,agecomp_struct)
                if (!is.null(agecomp_struct))
                {
                    new_dat_struct <- sim_add_age_comp(new_dat_struct,catch_year,proj_ss,proj_fl,nsamples_age,proj_fleet_age_comp_gender[f],proj_fleet_age_comp_part[f],agecomp_struct)
                }

                # add generated length comps for endyr to lencomp for proj_fleet
                lencomp_struct <- sim_generate_length_comp(new_dat_struct,prev_rep_struct,catch_year,proj_ss,proj_fl,apply_error=FALSE)
                lencomp_struct <- sim_map_pop_len_to_data_len(new_dat_struct,lencomp_struct)
                if (!is.null(lencomp_struct))
                {
                    # these values are placeholder/dummy values; they will NOT be used in model fitting
                    new_dat_struct <- sim_add_length_comp(new_dat_struct,catch_year,proj_ss,proj_fl,nsamples_len,proj_fleet_len_comp_gender[f],proj_fleet_len_comp_part[f],lencomp_struct)
                }
            }

            # ----------------- write the new DAT file -----------------
            SS_writedat(new_dat_struct,dat_file,overwrite=TRUE,verbose=TRUE)


            # &&&&&&&&&&&&&&&&& edit the forecast file &&&&&&&&&&&&&&&&&
            new_fc_struct$FirstYear_for_caps_and_allocations <- catch_year + 1

            # ----------------- write the new forecast file -----------------
            SS_writeforecast(new_fc_struct,file=forecast_file,overwrite=TRUE,verbose=TRUE)


            # &&&&&&&&&&&&&&&&& edit the CTL file &&&&&&&&&&&&&&&&&
            # change catch_year to catch_year+1 in bias adjustment section
            new_ctl_struct <- sedit(new_ctl_struct,as.character(catch_year),as.character(catch_year+1))

            # change curr_year to curr_year+1 in bias adjustment section
            new_ctl_struct <- sedit(new_ctl_struct,as.character(curr_year),as.character(catch_year))

            # ----------------- write the new CTL file -----------------
            writeLines(new_ctl_struct,ctl_file)


            # &&&&&&&&&&&&&&&&& edit the input PAR file &&&&&&&&&&&&&&&&&
            new_par_struct <- prev_par_struct

            # on which line are the main rec devs (name recdev1)?
            rec_dev_line <- 1 + grep("# recdev1:",prev_par_struct)

            # get row for current year and env idx fleet from REP index matrix
            env_idx_row <- subset(subset(prev_rep_struct$cpue,Yr==curr_year),Fleet==index_fleet)
            if (dim(env_idx_row)[1] == 1)
            {
                # get rec dev string
                rec_devs <- str_split(str_trim(prev_par_struct[rec_dev_line])," ")

                # omit the value for the current (last) year
                new_rec_dev_str <- str_c(rec_devs[[1]][1:(length(rec_devs[[1]])-1)],sep="",collapse=" ")

                # replace rec dev for current year with transformed env idx value from REP index matrix
                # the env index accounts for 35% of the total rec dev ONLY; randomly generate the remaining 65%
                calc_q <- env_idx_row$Calc_Q
                env_idx <- env_idx_row$Obs
                curr_year_env_rec_dev <- ((env_idx_var_frac * log(env_idx / calc_q)) +
                                          (base_var_frac * rnorm(1,mean=0,sd=proj_sigmaR)))
                if (curr_year_env_rec_dev > rec_dev_hist_max)
                {
                    curr_year_env_rec_dev <- rec_dev_hist_max
                }

                new_par_struct[rec_dev_line] <- paste(new_rec_dev_str,curr_year_env_rec_dev,"0.0",sep=" ")
            } else {
                # add " 0.0" to the main rec devs vector as the default
                new_par_struct[rec_dev_line] <- paste(prev_par_struct[rec_dev_line],"0.0",sep=" ")
            }

            # ----------------- write the new input PAR file -----------------
            writeLines(new_par_struct,par_file)


            # run the calculations only run
            system(paste(ss_exe_file,"-nox","-nohess","-cbs 1000000000",sep=" "),intern=FALSE,ignore.stderr=TRUE,wait=TRUE,
                   input=NULL,show.output.on.console=FALSE,minimized=FALSE,invisible=FALSE)




            # ||||||||||||||||| THE MAIN RUN |||||||||||||||||

            # (currently in new_calc_only_dir)
            # get the calculated DAT file values from the calculation only run
            calc_only_dat_struct <- SS_readdat(est_dat_file,section=2)

            # get the calculated REP file values from the calculation only run
            calc_only_rep_struct <- SS_output(".",forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE,covar=FALSE)


            setwd(working_dir)


            # copy files from the initial directory into this directory for the main run
            copy_sim_files_into_dir(working_dir,new_dir)

            # copy DAT, CTL, and forecast files from the calculation only run to this directory
            copy_sim_run_files_into_dir(new_calc_only_dir,new_dir)


            setwd(new_dir)


            # &&&&&&&&&&&&&&&&& edit the DAT file &&&&&&&&&&&&&&&&&
            # new_dat_struct <- SS_readdat(dat_file)    # the new DAT file is already in memory
            new_dat_struct <- save_new_dat_struct

            # generate "data" for the projected fleets given the calc-only run output (REP) values
            for (f in 1:num_proj_fleets)
            {
                proj_fl <- proj_fleet[f]
                proj_ss <- proj_fleet_seas[f]

                # add generated age comps for endyr to agecomp for proj_fleet
                agecomp_struct <- sim_generate_age_comp_from_expected(calc_only_dat_struct,calc_only_rep_struct,catch_year,proj_ss,proj_fl,apply_error=TRUE)
                if (!is.null(agecomp_struct))
                {
                    new_dat_struct <- sim_add_age_comp(new_dat_struct,catch_year,proj_ss,proj_fl,proj_fleet_age_comp_N[f],proj_fleet_age_comp_gender[f],proj_fleet_age_comp_part[f],agecomp_struct)
                }

                # add generated length comps for endyr to lencomp for proj_fleet
                lencomp_struct <- sim_generate_length_comp_from_expected(calc_only_dat_struct,calc_only_rep_struct,catch_year,proj_ss,proj_fl,apply_error=TRUE)
                if (!is.null(lencomp_struct))
                {
                    new_dat_struct <- sim_add_length_comp(new_dat_struct,catch_year,proj_ss,proj_fl,proj_fleet_len_comp_N[f],proj_fleet_len_comp_gender[f],proj_fleet_len_comp_part[f],lencomp_struct)
                }
            }

            # ----------------- write the new DAT file -----------------
            SS_writedat(new_dat_struct,dat_file,overwrite=TRUE,verbose=TRUE)


            # run the main run
            system(paste(ss_exe_file,"-nox","-cbs 1000000000",sep=" "),intern=FALSE,ignore.stderr=TRUE,wait=TRUE,
                   input=NULL,show.output.on.console=FALSE,minimized=FALSE,invisible=FALSE)


            setwd(working_dir)

        }

        cat(paste("\nCompleted projections for index ",idx_name,"\n\n",sep=""))
    }
}





# set up for running in parallel
detectCores()
getDoParName()
getDoParVersion()

loop_cl <- makeCluster(num_cpus)
registerDoParallel(loop_cl)

cat(paste("\nNumber of CPUs in use:  ",getDoParWorkers(),"\n\n",sep=""))


setwd(working_dir)



# ~~~~~~~~~~~ main loop for projections ~~~~~~~~~~~
foreach (i = 1:num_indices,.export=c("index_names",
                                     "index_seas",
                                     "index_fleet",
                                     "num_indices",
                                     "index_data",
                                     "index_std_err",
                                     "num_proj_years",
                                     "working_dir",
                                     "start_struct",
                                     "dat_struct",
                                     "ctl_struct",
                                     "fc_struct",
                                     "base_catch_tot",
                                     "base_catch_frac",
                                     "base_par_struct",
                                     "base_rep_struct",
                                     "num_proj_fleets",
                                     "rec_dev_hist_max",
                                     "env_idx_var_frac",
                                     "base_var_frac",
                                     "proj_sigmaR",
                                     "proj_fleet_seas",
                                     "proj_fleet",
                                     "proj_fleet_name",
                                     "proj_fleet_len_comp_gender",
                                     "proj_fleet_len_comp_part",
                                     "proj_fleet_age_comp_gender",
                                     "proj_fleet_age_comp_part",
                                     "proj_fleet_cpue_std_err",
                                     "proj_fleet_len_comp_N",
                                     "proj_fleet_age_comp_N",
                                     "proj_fleet_discard_average",
                                     "proj_fleet_discard_std_err")) %dopar% do_projections_for_index(i)



stopCluster(loop_cl)



cat(paste("\nCompleted projections\n\n",sep=""))



setwd(working_dir)

# check output
# new_run <- SS_output(new_dir,forecast=TRUE,verbose=FALSE,printstats=FALSE,hidewarn=TRUE,covar=FALSE)
# SS_plots(new_run)

