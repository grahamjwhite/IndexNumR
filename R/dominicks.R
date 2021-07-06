
#' Get data from the Dominicks dataset
#'
#' @description The Dominicks Scanner data, provided by the University of Chicago Booth School of Business,
#' contains around 5 years of product-level
#' data from over 100 stores, collected from 1989-1994. The data consist
#' of a UPC file that contains information on the products, and a
#' movement file that contains the information on prices and sales. For
#' a complete description of the data, see
#' \href{https://www.chicagobooth.edu/research/kilts/datasets/dominicks}{Dominicks data website}
#' and the
#' \href{https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset/dominicks-manual-and-codebook_kiltscenter.aspx}{Dominicks data user manual}.
#' This function downloads and merges the movement and UPC files, then merges the
#' result with data detailing the dates of each of the weeks in the movement file.
#'
#' @details
#' Only two transformations are performed on the data:
#' \itemize{
#'     \item The quantity of interest is calculated as MOVE/QTY (then MOVE and QTY are dropped)
#'     \item All observations where the variable OK equals 0, or price is less than or equal to 0, are dropped
#' }
#'
#' If you have already downloaded the movement and UPC csv files for a category from
#' the website, then you can pass the file paths of those files to the function
#' and just have it combine them with the weeks dataset. The default is to download
#' the files for you from the website.
#'
#' The products available are:
#' \itemize{
#'     \item Analgesics
#'     \item Bath Soap
#'     \item Beer
#'     \item Bottled Juices
#'     \item Cereals
#'     \item Cheeses
#'     \item Cigarettes
#'     \item Cookies
#'     \item Crackers
#'     \item Canned Soup
#'     \item Dish Detergent
#'     \item Front-end-candies
#'     \item Frozen Dinners
#'     \item Frozen Entrees
#'     \item Frozen Juices
#'     \item Fabric Softeners
#'     \item Grooming Products
#'     \item Laundry Detergents
#'     \item Oatmeal
#'     \item Paper Towels
#'     \item Refrigerated Juices (not currently available)
#'     \item Soft Drinks
#'     \item Shampoos
#'     \item Snack Crackers
#'     \item Soaps
#'     \item Toothbrushes
#'     \item Canned Tuna
#'     \item Toothpastes
#'     \item Bathroom Tissues
#' }
#'
#' @param x the name of the category to retrieve, see details for list.
#' @param movementcsv the path to the movement csv file for one product category. The default is NULL,
#' which downloads the file from the website.
#' @param UPCcsv the path to the UPC csv file for one product category. The default is NULL,
#' which downloads the file from the website.
#' @export
#' @references James M. Kilts Center, University of Chicago Booth School of Business
#' @examples
#' \dontrun{
#' analgesics <- dominicksData("Analgesics")
#' }
dominicksData <- function(x, movementcsv = NULL, UPCcsv = NULL){

  dlMove <- ifelse(is.null(movementcsv), TRUE, FALSE)
  dlUPC <- ifelse(is.null(UPCcsv), TRUE, FALSE)

  if(!dlMove && !file.exists(movementcsv)){
    stop(paste("movement file", movementcsv, "does not exist."))
  }

  if(!dlUPC && !file.exists(UPCcsv)){
    stop(paste("UPC file", UPCcsv, "does not exist."))
  }

  movementBaseURL <- "https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset/movement_csv-files/"
  UPCBaseURL <- "https://www.chicagobooth.edu/-/media/enterprise/centers/kilts/datasets/dominicks-dataset/upc_csv-files/"

  categories <- c("Analgesics", "Bath Soap", "Beer", "Bottled Juices", "Cereals",
                  "Cheeses", "Cigarettes", "Cookies", "Crackers", "Canned Soup",
                  "Dish Detergent", "Front-end-candies", "Frozen Dinners", "Frozen Entrees",
                  "Frozen Juices", "Fabric Softeners", "Grooming Products", "Laundry Detergents",
                  "Oatmeal", "Paper Towels", "Refrigerated Juices", "Soft Drinks", "Shampoos",
                  "Snack Crackers", "Soaps", "Toothbrushes", "Canned Tuna", "Toothpastes",
                  "Bathroom Tissues")

  xPos <- grep(paste0("^", x, "$"), categories, ignore.case = TRUE)

  if(length(xPos) == 0){
    stop(paste("Category", x, "does not exist in the Dominicks data"))
  }

  UPCfiles <- c("upcana.csv", "upcbat.csv", "upcber.csv", "upcbjc.csv", "upccer.csv",
                "upcche.csv", "upccig.csv", "upccoo.csv", "upccra.csv", "upccso.csv",
                "upcdid.csv", "upcfec.csv", "upcfrd.csv", "upcfre.csv", "upcfrj.csv",
                "upcfsf.csv", "upcgro.csv", "upclnd.csv", "upcoat.csv", "upcptw.csv",
                "Not Available", "upcsdr.csv", "upcsha.csv", "upcsna.csv", "upcsoa.csv",
                "upctbr.csv", "upctna.csv", "upctpa.csv", "upctti.csv")

  movementFiles <- c("wana.csv", "wbat.csv", "wber.csv", "wbjc.csv", "wcer.csv", "wche.csv",
                     "wcig.csv", "wcoo.csv", "wcra.csv", "wcso.csv", "wdid.csv", "wfec.csv",
                     "wfrd.csv", "wfre.csv", "wfrj.csv", "wfsf.csv", "wgro.csv", "wlnd.csv",
                     "woat.csv", "wptw.csv", "Not Available", "wsdr.csv", "wsha.csv",
                     "wsna.csv", "wsoa.csv", "wtbr.csv", "wtna.csv", "wtpa.csv", "wtti.csv")

  if(UPCfiles[xPos] == "Not Available"){
    stop(paste("Category", x, "is a category, but the csv files are not available."))
  }

  # get files if needed
  if(dlUPC){
    UPCfilename <- UPCfiles[xPos]
    UPCcsv <- tempfile(fileext = ".csv")
    download.file(url = paste0(UPCBaseURL, UPCfilename), destfile = UPCcsv)
  }

  UPCFile <- read.csv(UPCcsv)
  if(dlUPC) unlink(UPCcsv)

  if(dlMove){
    movementFilename <- movementFiles[xPos]
    movementZip <- tempfile(fileext = ".zip")
    download.file(url = paste0(movementBaseURL,
                               ifelse(movementFilename == "wana.csv",
                                      sub(pattern = "\\.csv", replacement = "_csv.zip", movementFilename),
                                      sub(pattern = "\\.csv", replacement = ".zip", movementFilename))),
                  destfile = movementZip)
    movementcsv <- unz(movementZip, filename = movementFilename)
  }

  movementFile <- read.csv(movementcsv)
  if(dlMove) unlink(movementZip)

  # clean files and calculate required columns
  movementFile <- movementFile[movementFile$OK == 1 & movementFile$PRICE > 0,]
  movementFile$QUANTITY <- movementFile$MOVE / movementFile$QTY
  movementFile <- subset(movementFile, select = -c(MOVE, QTY, PRICE_HEX, PROFIT_HEX, OK))
  movementFile$EXPENDITURE <- movementFile$PRICE * movementFile$QUANTITY
  names(movementFile) <- tolower(names(movementFile))
  names(UPCFile) <- tolower(names(UPCFile))

  # merge with weeks and UPC file
  merged <- merge(movementFile, UPCFile, by = "upc")
  merged <- merge(merged, DominicksWeeks, by = "week")

  return(merged)

}
