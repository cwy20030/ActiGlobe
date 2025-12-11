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
#' A miniturized IANA table generator that only extract the first timezone based
#' on the abbreviated time zone code. An internal function to resolve the internal
#' data access issue.
#'
#' @import utils
#'
#' @param Write A binary code to indicate whether to write a .csv file containing the template needed for the travel log. (default = FLASE)
#' When set to FALSE, the template will be returned as an object.
#' When set to TRUE, user must provide the target directory where they wish to store the template.
#' @param Dir The directory where the travel log template to be exported <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#'
#' @return a miniturized IANA table template as a data.frame or written as a CSV file
#'
#' @examples
#' \dontrun{
#'
#' sIANA <- mIANA (Wirte = FALSE)
#'
#' print (sIANA)
#' }
#'
#' @keywords Travel Log Template Timezone Shift
#' @noRd

mIANA <- function (Write = FALSE, Dir = NULL) {
    Timezone_IANA <- c ("Europe/Amsterdam", "America/Vancouver", "America/Chicago", "Europe/London", "Africa/El_Aaiun", "Australia/Adelaide", "Europe/Athens", "America/New_York", "America/Denver", "Australia/Sydney", "America/Anchorage", "America/Halifax", "America/Santiago", "Atlantic/Azores", "America/St_Johns", "Pacific/Auckland", "America/Adak", "Asia/Jerusalem", "Europe/Dublin", "America/Godthab", "America/Miquelon", "Antarctica/Troll", "Australia/Lord_Howe", "Pacific/Easter", "Pacific/Chatham", "Pacific/Norfolk", "Asia/Aden", "America/Argentina/Salta", "Africa/Lagos", "Asia/Tehran", "Africa/Algiers", "America/Belem", "America/Asuncion", "Asia/Krasnoyarsk", "America/Lima", "Pacific/Port_Moresby", "Asia/Karachi", "Africa/Abidjan", "Asia/Dubai", "Asia/Damascus", "America/Porto_Velho", "America/Mexico_City", "America/Caracas", "America/La_Paz", "Africa/Khartoum", "Europe/Istanbul", "Africa/Addis_Ababa", "Australia/Darwin", "Pacific/Guam", "Asia/Baku", "Asia/Chita", "America/Hermosillo", "America/Montevideo", "Africa/Tripoli", "Europe/Astrakhan", "Asia/Tokyo", "Asia/Aqtau", "Asia/Yekaterinburg", "Europe/Moscow", "America/Guayaquil", "Australia/Perth", "Asia/Sakhalin", "Africa/Johannesburg", "Asia/Rangoon", "America/Panama", "Asia/Kuala_Lumpur", "Asia/Hovd", "Asia/Jayapura", "Asia/Vladivostok", "Asia/Anadyr", "Asia/Pyongyang", "Asia/Tashkent", "Asia/Kabul", "Asia/Bangkok", "Asia/Irkutsk", "Asia/Manila", "America/Guyana", "Pacific/Apia", "America/Bogota", "Asia/Ulaanbaatar", "Asia/Ashgabat", "Asia/Bishkek", "Australia/Brisbane", "Asia/Taipei", "Asia/Kathmandu", "Asia/Makassar", "Asia/Jakarta", "Asia/Brunei", "Asia/Tbilisi", "America/Paramaribo", "America/Whitehorse", "America/Cayenne", "Asia/Omsk", "America/Rio_Branco", "Indian/Mauritius", "Asia/Dili", "Asia/Dushanbe", "Atlantic/Stanley", "Pacific/Funafuti", "Pacific/Guadalcanal", "Pacific/Honolulu", "Asia/Hong_Kong", "Pacific/Palau", "Pacific/Fiji", "Pacific/Efate", "Indian/Maldives", "Atlantic/Cape_Verde", "Pacific/Tongatapu", "Pacific/Noumea", "Pacific/Pohnpei", "Pacific/Tahiti", "Asia/Thimphu", "Pacific/Galapagos", "Pacific/Rarotonga", "America/Punta_Arenas", "America/Noronha", "Antarctica/Davis", "Antarctica/Mawson", "Australia/Eucla", "Etc/GMT", "Etc/GMT+10", "Etc/GMT+11", "Etc/GMT+12", "Etc/GMT+2", "Etc/GMT+3", "Etc/GMT+4", "Etc/GMT+5", "Etc/GMT+6", "Etc/GMT+7", "Etc/GMT+8", "Etc/GMT+9", "Etc/GMT-1", "Etc/GMT-10", "Etc/GMT-11", "Etc/GMT-12", "Etc/GMT-13", "Etc/GMT-14", "Etc/GMT-4", "Etc/GMT-5", "Etc/GMT-6", "Etc/GMT-7", "Etc/GMT-8", "Etc/GMT-9", "Indian/Chagos", "Pacific/Kwajalein", "Pacific/Kanton", "Pacific/Fakaofo", "Pacific/Gambier", "Pacific/Kiritimati", "Pacific/Marquesas", "Pacific/Nauru", "Pacific/Niue", "Pacific/Pitcairn")


    TZ_Code <- c ("CEST", "PDT", "CDT", "BST", "WEST", "ACDT", "EEST", "EDT", "MDT", "AEDT", "AKDT", "ADT", "CLST", "AZOST", "NDT", "NZDT", "HDT", "IDT", "IST", "GMT-1", "PMDT", "GMT+2", "LHDT", "EASST", "CHADT", "NFDT", "AST", "ART", "WAT", "IRST", "CET", "BRT", "PYT", "NOVT", "PET", "PGT", "PKT", "GMT", "GST", "GMT+3", "AMT", "CST", "VET", "BOT", "CAT", "TRT", "EAT", "ACST", "ChST", "AZT", "YAKT", "MST", "UYT", "EET", "SAMT", "JST", "KZT", "YEKT", "MSK", "ECT", "AWST", "SAKT", "SAST", "MMT", "EST", "SGT", "HOVT", "WIT", "VLAT", "PETT", "KST", "UZT", "AFT", "ICT", "IRKT", "PHT", "GYT", "SST", "COT", "ULAT", "TMT", "KGT", "AEST", "TWT", "NPT", "WITA", "WIB", "MYT", "GET", "SRT", "YT", "GFT", "OMST", "ACT", "MUT", "TLT", "TJT", "FKST", "GILT", "SBT", "HAST", "HKT", "PWT", "FJT", "VUT", "MVT", "CVT", "TOT", "NCT", "KOST", "TAHT", "BTT", "GALT", "CKT", "CLT", "FNT", "DAVT", "MAWT", "ACWST", "UTC", "GMT-10", "GMT-11", "GMT-12", "GMT-2", "GMT-3", "GMT-4", "GMT-5", "GMT-6", "GMT-7", "GMT-8", "GMT-9", "GMT+1", "GMT+10", "GMT+11", "GMT+12", "GMT+13", "GMT+14", "GMT+4", "GMT+5", "GMT+6", "GMT+7", "GMT+8", "GMT+9", "IOT", "MHT", "PHOT", "TKT", "GAMT", "LINT", "MART", "NRT", "NUT", "PST")


    Offset <- as.numeric (
        c ("2", "-7", "-5", "1", "1", "10.5", "3", "-4", "-6", "11", "-8", "-3", "-3", "0", "-2.5", "13", "-9", "3", "1", "-1", "-2", "2", "11", "-5", "13.75", "12", "3", "-3", "1", "3.5", "1", "-3", "-3", "7", "-5", "10", "5", "0", "4", "3", "-4", "-6", "-4", "-4", "2", "3", "3", "9.5", "10", "4", "9", "-7", "-3", "2", "4", "9", "5", "5", "3", "-5", "8", "11", "2", "6.5", "-5", "8", "7", "9", "10", "12", "9", "5", "4.5", "7", "8", "8", "-4", "13", "-5", "8", "5", "6", "10", "8", "5.75", "8", "7", "8", "4", "-3", "-7", "-3", "6", "-5", "4", "9", "5", "-3", "12", "11", "-10", "8", "9", "12", "11", "5", "-1", "13", "11", "11", "-10", "6", "-6", "-10", "-3", "-2", "7", "5", "8.75", "0", "-10", "-11", "-12", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "1", "10", "11", "12", "13", "14", "4", "5", "6", "7", "8", "9", "6", "12", "13", "13", "-9", "14", "-9.5", "12", "-11", "-8")
    )


    Standard_Offset <- c (
        "+01:00", "-08:00", "-06:00", "+00:00", "+00:00", "+09:30", "+02:00", "-05:00", "-07:00", "+10:00", "-09:00", "-04:00", "-04:00", "-01:00", "-03:30", "+12:00", "-10:00", "+02:00", "+00:00", "-02:00", "-03:00", "+00:00", "+10:30", "-06:00", "+12:45", "+11:00", "+03:00", "-03:00", "+01:00", "+03:30", "+01:00", "-03:00", "-03:00", "+07:00", "-05:00", "+10:00", "+05:00", "+00:00", "+04:00", "+03:00", "-04:00", "-06:00", "-04:00", "-04:00", "+02:00", "+03:00", "+03:00", "+09:30", "+10:00", "+04:00", "+09:00", "-07:00", "-03:00", "+02:00", "+04:00", "+09:00", "+05:00", "+05:00", "+03:00", "-05:00", "+08:00", "+11:00", "+02:00", "+06:30", "-05:00", "+08:00", "+07:00", "+09:00", "+10:00", "+12:00", "+09:00", "+05:00", "+04:30", "+07:00", "+08:00", "+08:00", "-04:00", "+13:00", "-05:00", "+08:00", "+05:00", "+06:00", "+10:00", "+08:00", "+05:45", "+08:00", "+07:00", "+08:00", "+04:00", "-03:00", "-07:00", "-03:00", "+06:00", "-05:00", "+04:00", "+09:00", "+05:00", "-03:00", "+12:00", "+11:00", "-10:00", "+08:00", "+09:00", "+12:00", "+11:00", "+05:00", "-01:00", "+13:00", "+11:00", "+11:00", "-10:00", "+06:00", "-06:00", "-10:00", "-03:00", "-02:00", "+07:00", "+05:00", "+08:45", "+00:00", "-10:00", "-11:00", "-12:00", "-02:00", "-03:00", "-04:00", "-05:00", "-06:00", "-07:00", "-08:00", "-09:00", "+01:00", "+10:00", "+11:00", "+12:00", "+13:00", "+14:00", "+04:00", "+05:00", "+06:00", "+07:00", "+08:00", "+09:00", "+06:00", "+12:00", "+13:00", "+13:00", "-09:00", "+14:00", "-09:30", "+12:00", "-11:00", "-08:00"
    )


    # Prepare Output -----------

    df <-
        data.frame (
            Timezone_IANA = Timezone_IANA,
            TZ_Code = TZ_Code,
            Offset = Offset,
            Standard_Offset = Standard_Offset
        )


    ## Write option
    if (Write) { ### When TRUE

        ### Check essential components
        if (is.null (Dir)) stop ("A directory must be provided in order to export the template.")

        write.csv (df, paste0 (Dir, "/Mini_IANA_Table.csv"), row.names = FALSE)
    }


    if (!Write) {
        return (df)
    }
}
