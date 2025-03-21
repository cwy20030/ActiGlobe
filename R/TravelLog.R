#  File ActiGlobe/R/TravelLog.R
#
#  Copyright (C) 2025  C. William Yao, PhD
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title
#' Generate the Travel Log Template Needed for Time Zone Correction
#' @import utils
#' @param Write A binary code to indicate whether to write a .csv file containing the template needed for the travel log. (default = FLASE)
#' When set to FALSE, the template will be returned as an object.
#' When set to TRUE, user must provide the target directory where they wish to store the template.
#' @param Dir The directory where the travel log template to be exported <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @return data.list
#' @keywords travel, log, template, timezone, timeshift
#' @export
#'


TravelLog = function(Write = FALSE, Dir = NULL) {

 df =  data.frame(matrix(ncol = 4, nrow = 1))

 names(df) = c("ID", "UTC_Offset", "date_Start", "date_End")

 df[1,] = c("ExampleID", "+05:00", as.character(Sys.Date()), as.character(Sys.Date() + 1))


 ## Write option
 if (Write) { ### When TRUE

   ### Check essential components
   if (is.null(Dir)) stop ("A directory must be provided in order to export the template.")

   write.csv2(df, paste0(Dir,"/TravelLog_Template.csv"),row.names = F)

 }


 if (!Write) return(df)

}
