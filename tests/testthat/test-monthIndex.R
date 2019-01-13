context("monthIndex test functions")

load(system.file("testdata","testData_monthIndex.RData",package = "IndexNumR"))

weekMethods <- c("majority","fourWeek","naive","wholeOnly")
dayOfWeek <- c("wed", "fri")

for(i in seq_along(weekMethods)){
  for(j in seq_along(dayOfWeek)){
    test_that("monthIndex function returns the correct values for different week treatments",{
      expect_equal(monthIndex(testData[[paste0("date_",dayOfWeek[j])]],
                              overlapWeeks = weekMethods[i]),
                   (testData[[paste0(weekMethods[i],"_",dayOfWeek[j])]]))
    })
  }
}
