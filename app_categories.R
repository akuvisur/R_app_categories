# read the application categories and generic categories files
appCats <- data.frame(package_name=character(), category=character())
appCats <- read.csv("app_categories.csv", stringsAsFactors = FALSE)
genCats <- read.csv("app_general_categories.csv", sep=",", stringsAsFactors = FALSE)

#write.csv(appCats, "app_categories.csv", row.names = FALSE)
findCategory <- function(package) {
  appC <- get('appCats', envir=.GlobalEnv)
  #appCats <- data.frame(package_name=character(), category=character(), stringsAsFactors = FALSE)
  #if (file.exists("app_categories.csv")) {
  #  appCats <- read.csv("app_categories.csv", stringsAsFactors = FALSE)
  #}
  if (package %in% appC$package_name) {
    ss <- subset(appC, package_name == package)
    print(paste("found",package))
    return(ss$category[1])
  }
  else if (!is.na(pmatch("launcher",package))) {
    appC[nrow(appC)+1,] <- c(package_name=as.character(package), category=as.character("LAUNCHER"))
    assign('appCats', appC, envir=.GlobalEnv)
    #write.csv(appCats, "app_categories.csv",row.names=FALSE)
    return("LAUNCHER")
  }
  #print(paste("Fetching from PlayStore", package))
  u <- getURL(paste("https://play.google.com/store/apps/details?id=",package, sep=""))
  r <- regexpr('class="document-subtitle category" href="/store/apps/category/', u)
  if (r == -1) {
    appC[nrow(appC)+1,] <- c(package_name=as.character(package), category=as.character("OTHER"))
    assign('appCats', appC, envir=.GlobalEnv)
    #write.csv(appCats, "app_categories.csv",row.names=FALSE)
    return("OTHER")
  }
  # regexp length 62
  s <- substr(u, r + 62, r+122)
  endpos <- regexpr('\\"', s)
  cat <- substr(s,1,endpos-1)
  appC[nrow(appC)+1,] <- c(package_name=as.character(package), category=as.character(cat))
  #appCategories <<- appCats
  #write.csv(appCats, "app_categories.csv",row.names=FALSE)
  assign('appCats', appC, envir=.GlobalEnv)
  return(cat)
}

#usage

# read and append to the categories file
appCats <- read.csv("app_categories.csv", stringsAsFactors = FALSE)
for (pkg in unique(notifications$application_package)) {
  findCategory(pkg)
}
write.csv(appCats, "app_categories.csv", row.names = FALSE)

# merge with your data, rename the application package column in your dataset to 'package_name'
names(notification_sample)[10] <- "package_name"
notification_sample <- left_join(notification_sample, appCats, by="package_name")
# rename your last column as app_category
names(notification_sample)[ncol(notification_sample)] <- "app_category"

# apply generic category if required
genCats <- read.csv("app_general_categories.csv", sep=",", stringsAsFactors = FALSE)
notification_sample <- left_join(notification_sample, genCats, by="app_category")
# rename last column
names(notification_sample)[ncol(notification_sample)] <- "generic_category"

