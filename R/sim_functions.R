
# Creative Commons License
# Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)
# https://creativecommons.org/licenses/by-nc-sa/4.0/
# 
# Some functions for SS simulations
# ZTA, initial version 2014-10-18
# R version 3.1.1, 32-bit
#

library(gdata)


get_forecast_catch_by_fleet <- function(forecast_report_filename,num_fleets=0,num_seasons=0,num_fc_years=0)
{
    catch_proj <- NULL

    # check if file exists - thanks, Ian
    temp_file_size <- file.info(forecast_report_filename)$size

    if(!is.na(temp_file_size) && temp_file_size > 0)
    {
        # read the forecast report file
        fc_table <- read.table(file=forecast_report_filename,col.names=1:200,fill=TRUE,quote="",colClasses="character",nrows=-1)
        num.rows.fct  <- dim(fc_table)[1]

        if (num.rows.fct > 0 && num_fleets > 0 && num_seasons > 0 && num_fc_years > 0)
        {
            # in which row are the column labels?
            label_row <- num.rows.fct - ((num_fc_years * num_seasons) + 1)

            # get the columns with "dead(B):_[N]"
            catch_cols <- which(startsWith(fc_table[label_row,],"dead(B):_") == TRUE)

            # in which row is the forecast/projected catch for the following year?
            proj_row  <- label_row + 1

            if (length(catch_cols) == num_fleets)
            {
                # get the catch vector for the next/projected year
                # NOTE:  this works for THE FIRST SEASON ONLY
                catch_proj <- as.numeric(fc_table[proj_row,catch_cols])
            }
        }
    }

    return(catch_proj)
}



copy_sim_files_into_dir <- function(copy_to_dir)
{
    file.copy(ss_exe_file,copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(starter_file,copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(forecast_file,copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(dat_file,copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
    file.copy(ctl_file,copy_to_dir,copy.mode=TRUE,copy.date=TRUE)
}



