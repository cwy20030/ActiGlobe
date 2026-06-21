#  File ActiGlobe/R/mIANA.R
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
#' @title Mini IANA Table for Internal Uses
#'
#' @description
#' A miniturized IANA table generator that only extract the first timezone
#' based on the abbreviated time zone code. This internal function intends to
#' aid developers when constructing functions.
#'
#' @importFrom utils write.csv
#'
#' @param Write A binary code to indicate whether to write a .csv file
#' containing the template needed for the travel log. (default = FLASE)
#' When set to FALSE, the template will be returned as an object.
#' When set to TRUE, user must provide the target directory where they wish to
#' store the template.
#' @param Dir The directory where the travel log template to be exported
#' <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#'
#' @return a miniturized IANA table template as a data.frame or written as a
#' CSV file. The table contains the following columns:
#' \itemize{
#' \item Timezone_IANA Character; the IANA time zone name
#' (e.g.,"America/New_York")
#' \item TZ_Code Character strings of abbreviated time zone code (e.g., "EST");
#' corresponding to the "Standard_TZ_Code" column in \code{\link{IANA}}
#' \item DST_TZ_Code Character strings of abbreviated time zone code
#' during daylight saving time (e.g., "EDT"); corresponding to the
#'  "DST_TZ_Code" column in \code{\link{IANA}}
#' \item Offset Numeric; the standard UTC offset in hours (e.g., -5 for EST)
#' \item DST_Offset Numeric; the daylight saving time UTC offset in hours
#' (e.g., -4 for EDT)
#' }
#'
#' @examples
#'
#' sIANA <- mIANA (Wirte = FALSE)
#'
#' print (sIANA)
#'
#'
#' @keywords Travel Log Template Timezone Shift
#' @noRd

mIANA <- function (Write = FALSE, Dir = NULL) {
    Timezone_IANA <- c(
        "Asia/Aden","Europe/Amsterdam","America/Argentina/Salta",
        "Africa/Lagos","Asia/Tehran","America/Belem",
        "America/Asuncion",
        "Asia/Krasnoyarsk","America/Lima","Pacific/Port_Moresby",
        "America/Vancouver","Asia/Karachi","Africa/Abidjan",
        "America/Chicago","Europe/London","Asia/Kolkata",
        "Asia/Dubai","Asia/Damascus","America/Porto_Velho",
        "America/Mexico_City","America/Caracas","America/La_Paz",
        "Africa/El_Aaiun","Africa/Khartoum","Europe/Istanbul",
        "Africa/Addis_Ababa","Australia/Adelaide","Australia/Darwin",
        "Pacific/Guam","Asia/Baku","Asia/Chita","Europe/Athens",
        "America/Hermosillo","America/Montevideo","America/New_York",
        "Europe/Astrakhan","Asia/Tokyo","Asia/Kashgar",
        "Asia/Aqtau","America/Denver","Asia/Yekaterinburg",
        "Europe/Moscow","America/Guayaquil","Australia/Perth",
        "Australia/Sydney","Asia/Sakhalin","Africa/Johannesburg",
        "America/Anchorage","Asia/Rangoon","Asia/Kuala_Lumpur",
        "Asia/Hovd","Asia/Jayapura","America/Halifax",
        "Asia/Vladivostok","Asia/Anadyr","Asia/Pyongyang",
        "America/Santiago","Asia/Tashkent","Asia/Kabul",
        "Asia/Bangkok","Asia/Irkutsk","Asia/Manila",
        "Atlantic/Azores","America/Guyana","Pacific/Apia",
        "America/Bogota","Pacific/Bougainville","America/St_Johns",
        "Asia/Ulaanbaatar","Pacific/Auckland","Asia/Ashgabat",
        "Asia/Bishkek","America/Adak","Asia/Taipei",
        "Asia/Kathmandu","Asia/Makassar","Asia/Jakarta",
        "Asia/Brunei","Asia/Tbilisi","Asia/Jerusalem",
        "America/Paramaribo","America/Whitehorse","America/Cayenne",
        "Asia/Omsk","America/Rio_Branco","Europe/Dublin",
        "Indian/Mauritius","Asia/Dili","Asia/Dushanbe",
        "Atlantic/Stanley","Pacific/Funafuti","Pacific/Guadalcanal",
        "Atlantic/South_Georgia","Pacific/Honolulu","Asia/Hong_Kong",
        "America/Godthab","Pacific/Palau","Pacific/Fiji",
        "Pacific/Efate","Indian/Maldives","Atlantic/Cape_Verde",
        "Pacific/Tongatapu","Pacific/Noumea","Pacific/Pago_Pago",
        "Pacific/Pohnpei","Pacific/Tahiti","Asia/Thimphu",
        "Pacific/Galapagos","Pacific/Rarotonga","America/Miquelon",
        "America/Noronha","Antarctica/Davis","Antarctica/Mawson",
        "Antarctica/Troll","Australia/Eucla","Australia/Lord_Howe",
        "Pacific/Easter","Etc/GMT","Etc/GMT+1","Etc/GMT+10",
        "Etc/GMT+11","Etc/GMT+12","Etc/GMT+3","Etc/GMT+4",
        "Etc/GMT+5","Etc/GMT+6","Etc/GMT+7","Etc/GMT+8",
        "Etc/GMT+9","Etc/GMT-1","Etc/GMT-10","Etc/GMT-11",
        "Etc/GMT-12","Etc/GMT-13","Etc/GMT-14","Etc/GMT-2",
        "Etc/GMT-4","Etc/GMT-5","Etc/GMT-6","Etc/GMT-7",
        "Etc/GMT-8","Etc/GMT-9","Indian/Chagos",
        "Pacific/Kwajalein","Pacific/Chatham","Pacific/Kanton",
        "Pacific/Fakaofo","Pacific/Gambier","Pacific/Kiritimati",
        "Pacific/Marquesas","Pacific/Nauru","Pacific/Niue",
        "Pacific/Norfolk"
        )

    TZ_Code <- c(
        "AST","CET","ART","WAT","IRST","BRT","PYT","NOVT","PET","PGT",
        "PST","PKT","GMT","CST","GMT","IST","GST","GMT+3","AMT","CST",
        "VET","BOT","WET","CAT","TRT","EAT","ACST","ACST","ChST","AZT",
        "YAKT","EET","MST","UYT","EST","SAMT","JST","CST","KZT","MST",
        "YEKT","MSK","ECT","AWST","AEST","SAKT","SAST","AKST","MMT",
        "SGT","HOVT","WIT","AST","VLAT","PETT","KST","CLT","UZT","AFT",
        "ICT","IRKT","PHT","AZOT","GYT","SST","COT","BST","NST","ULAT",
        "NZST","TMT","KGT","HAST","TWT","NPT","WITA","WIB","MYT","GET",
        "IST","SRT","YT","GFT","OMST","ACT","GMT","MUT","TLT","TJT",
        "FKST","GILT","SBT","GST","HAST","HKT","GMT-2","PWT","FJT",
        "VUT","MVT","CVT","TOT","NCT","SST","KOST","TAHT","BTT",
        "GALT","CKT","PM","FNT","DAVT","MAWT","GMT","ACWST","LHST",
        "EAST","UTC","GMT-1","GMT-10","GMT-11","GMT-12","GMT-3",
        "GMT-4","GMT-5","GMT-6","GMT-7","GMT-8","GMT-9","GMT+1",
        "GMT+10","GMT+11","GMT+12","GMT+13","GMT+14","GMT+2",
        "GMT+4","GMT+5","GMT+6","GMT+7","GMT+8","GMT+9","IOT",
        "MHT","CHAST","PHOT","TKT","GAMT","LINT","MART","NRT",
        "NUT","NFT"
    )



    Offset <- as.numeric (
        c(
            "3","1","-3","1","3.5","-3","-3","7","-5","10","-8","5",
            "0","-6","0","5.5","4","3","-4","-6","-4","-4","0","2",
            "3","3","9.5","9.5","10","4","9","2","-7","-3","-5","4",
            "9","6","5","-7","5","3","-5","8","10","11","2","-9",
            "6.5","8","7","9","-4","10","12","9","-4","5","4.5","7",
            "8","8","-1","-4","13","-5","11","-3.5","8",
            "12","5","6","-10","8","5.75","8","7","8","4","2","-3",
            "-7","-3","6","-5","0","4","9","5","-3","12","11","-2",
            "-10","8","-2","9","12","11","5","-1","13","11","-11",
            "11","-10","6","-6","-10","-3","-2","7","5","0","8.75",
            "10.5","-6","0","-1","-10","-11","-12","-3","-4","-5",
            "-6","-7","-8","-9","1","10","11","12","13","14","2",
            "4","5","6","7","8","9","6","12","12.75","13","13",
            "-9","14","-9.5","12","-11","11"
        )
    )


    DST_TZ_Code <- c (
        "","CEST","","","","","","","","","PDT","","",
        "CDT","BST","","","","","","","","WEST","","",
        "","ACDT","","","","","EEST","","","EDT","","",
        "","","MDT","","","","","AEDT","","","AKDT","",
        "","","","ADT","","","","CLST","","","","","",
        "AZOST","","","","","NDT","","NZDT","","","HDT",
        "","","","","","","IDT","","","","","","IST",
        "","","","","","","","","","GMT-1","","","",
        "","","","","","","","","","","PMDT","","",
        "","GMT+2","","LHDT","EASST","","","","","",
        "","","","","","","","","","","","","","",
        "","","","","","","","","CHADT","","","",
        "","","","","NFDT"
        )


    DST_Offset <-  c (
           NA,2,NA,NA,NA,NA,NA,NA,NA,NA,
           -7,NA,NA,-5,1,NA,NA,NA,NA,NA,
           NA,NA,1,NA,NA,NA,10.5,NA,NA,NA,
           NA,3,NA,NA,-4,NA,NA,NA,NA,-6,
           NA,NA,NA,NA,11,NA,NA,-8,NA,NA,
           NA,NA,-3,NA,NA,NA,-3,NA,NA,NA,
           NA,NA,0,NA,NA,NA,NA,-2.5,NA,13,
           NA,NA,-9,NA,NA,NA,NA,NA,NA,3,
           NA,NA,NA,NA,NA,1,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,-1,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,-2,
           NA,NA,NA,2,NA,11,-5,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
           NA,NA,NA,NA,13.75,NA,NA,NA,NA,
           NA,NA,NA,12)



    # Prepare Output -----------

    data <-
        data.frame (
            Timezone_IANA = Timezone_IANA,
            TZ_Code       = TZ_Code,
            DST_TZ_Code   = DST_TZ_Code,
            Offset        = Offset,
            DST_Offset    = DST_Offset
        )


    ## Write option
    if (Write) { ### When TRUE

        ### Check essential components
        if (is.null (Dir)) {
            stop ("A directory must be provided to export the template.")
        }

        write.csv (data, paste0 (Dir, "/Mini_IANA_Table.csv"),
                   row.names = FALSE)
    }


    if (!Write) {
        return (data)
    }
}
