#  File ActiGlobe/R/Demand.R
#
#  Copyright (C) 2025  C. William Yao, PhD
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Affero General Public License as
#  published by the Free Software Foundation, either version 3 of the
#  License, or any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Affero General Public License for more details.
#
#  You should have received a copy of the GNU Affero General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

Demand <- function(options, MESSAGE) {


  # Print the options for the user
  for (i in 1:length(options)) {
    cat(paste(i, ": ", options[i], "\n", sep = ""))
  }

  # Request the user to select an option
  selected_option <- readline(prompt = paste0("Please select the ",MESSAGE," by entering its number: "))

  # Check if the input is a number and within the range of options
  if (!grepl("^[0-9]+$", selected_option) | as.numeric(selected_option) > length(options) | as.numeric(selected_option) < 1) {
    cat("Invalid selection. Please try again.\n")
    return(Demand(options,MESSAGE))
  }

  if(grepl("Other",options[as.numeric(selected_option)])) options[as.numeric(selected_option)] <- readline(prompt="Please enter the path: ")



  # Return the selected option
  return(options[as.numeric(selected_option)])
}

