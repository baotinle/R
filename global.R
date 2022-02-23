library(ggplot2)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(DT)
library(forcats)
library(tidyr)
library(dygraphs)
library(xts)
library(apexcharter)
library(base)
library(lubridate)
library(correlationfunnel)
library(tidyquant)
library(tidyverse)
library(shiny)
library(hrbrthemes)
library(scales)
library(rintrojs)
library(shinyBS)
library(shinydashboardPlus)



# Load data
users <- read_excel("Report_2022-01-01_2022-01-31-3.xls", 
                    sheet = "Ds Nhân sự")

report <- read_excel("Report_2022-01-01_2022-01-31-3.xls", sheet = "Worklogs")

feature <- read_excel("Epic_Feature thang 1.2022.xlsx")

creatissue <- read_excel("jiraissue2.xlsx")

jira <- read_excel("Status.xlsx", sheet = "issuejira")
status <- read_excel("Status.xlsx", sheet = "issuestatus")
project <- read_excel("Status.xlsx", sheet = "issueproject")
priority <- read_excel("Status.xlsx", sheet = "priority")
type <- read_excel("Status.xlsx",sheet = "issuetype")
colnames(feature) <- c("order", "month", "year", "epic_feature", "rate", "project_name", "note")


#bieu do 10
bugenv11 <- read_excel("bugenv11.xlsx")
#colnames(bugenv1) <-c("project","issuetype","enviroment")
#bugenv1$enviroment <-replace_na(bugenv1$enviroment,"Not fill")
bugenv11$enviroment1[bugenv11$enviroment1 == "Deverlop"] <- "Develop"

priority$Column3[priority$Column3 == "Immediate"] <- "Ngay lập tức"
priority$Column3[priority$Column3 == "High"] <- "Cao"
priority$Column3[priority$Column3 == "Normal"] <- "Bình thường"
priority$Column3[priority$Column3 == "Urgent"] <- "khẩn cấp"
priority$Column3[priority$Column3 == "Low"] <- "Thấp"
priority$Column3[priority$Column3 == "Medium"] <- "Trung bình"


report <- report %>%
  filter(report$`Project Name` != "Thực tập sinh")
project <- project %>%
  filter(project$pname != "Thực tập sinh")

rp<- report %>%
  select(`Issue Key`, Hours, `Work date`, `Activity Name`, `Full name`, `Issue Type`, 
         `Issue Status`, `Project Key`, `Project Name`, Username)


rp$issue_key    <- rp$`Issue Key`
rp$hours        <- rp$Hours
rp$work_date    <- rp$`Work date`
rp$activity_name   <- rp$`Activity Name`
rp$project_key  <- rp$`Project Key`
rp$project_name <- rp$`Project Name`
rp$issue_type<- rp$`Issue Type`
rp$issue_status<- rp$`Issue Status`
rp$full_name<- rp$`Full name`
rp$user<- rp$Username


rp<- rp %>%
  select(issue_key, hours, work_date, activity_name, full_name, user, issue_type, 
         issue_status, project_key, project_name) %>%
  mutate(date = as.Date(work_date, "%Y-%m-%d"),
         month =  format.Date(work_date, "%Y-%m"))

rp<- rp %>%
  mutate(eff = ifelse(issue_type == "Bug" | issue_type == "Hot fix", "Inefficient time", "Efficient time"),
         eff2 = ifelse(issue_type == "Bug" | issue_type == "Hot fix", "Inefficient ticket", "Efficient ticket"),
         weekday = weekdays(date),
         mana = ifelse(issue_type == "Management" | issue_type == "Report", "Management", "Specialized"))



user<- users %>%
  select(`User Jira`, `Họ và tên`, Role) %>%
  mutate(user = `User Jira`,
         name = `Họ và tên`,
         role = Role) %>%
  select(user, name, role)

user$role[user$user == "tuan.bv"] <- "Chuyên viên"
user$role[user$user == "hang.nt1"] <- "Chuyên viên"
user$role[user$user == "tin.lb"] <- "Chuyên viên"
user <- user %>% filter(!is.na(role))

data <- inner_join(rp, user, by = "user")
#bieu do 3 -5 - 7 - 8  13
data_log <- data %>% 
  filter(!is.na(project_name)) %>%
  filter(project_name != "Pháp chế ISOFHCARE") %>%
  filter(project_name != "Y Khoa Hà Nội") %>%
  filter(project_name != "SYSTEM")%>%
  mutate(date = format.Date(date,"%d-%m-%Y"))


#data_log <- data_log %>% 
#  mutate(project = project_name) %>%
#  separate(project, c("project1", "project2"), "\\_") %>%
#  filter(project2 == "TTB" | project2 == "HIS")



#data_log$project1[data_log$project1 == "BV Đại học Y"] <- "BV Đại Học Y"

datalog1 <- report %>%
  filter(`Issue Type` =="Management")%>%
  group_by(`Issue Key`) %>%
  select(`Issue Key`,Hours,`Issue Type`,`Project Name`)

datalog11 <- report %>%
  filter(`Issue Type` !="Management")%>%
  group_by(`Issue Key`) %>%
  select(`Issue Key`,Hours,`Issue Type`,`Project Name`)

datalog12 <- datalog1 %>%
  group_by(`Issue Key`)%>%
  summarise(sh = sum(Hours))
datalog12 <- 
################# Edit data status ticket

#Lấy các trường cần thiết trong bảng jira
jira <- jira %>%
  select(id, issuestatus,issuetype, project, priority, assignee)
colnames(jira) <- c("p_id", "issue_id", "issuestype","project_id", "priority_id", "user")
jira$user[jira$user == "phuong.dinh"] <- "phuong.dv"

#Lấy các trường cần thiết trong bảng status
status <- status %>%
  select(id, pname)
colnames(status) <- c("issue_id", "status")

#Lấy các trường cần thiết trong bảng project
project <- project %>%
  select(id, pkey, pname)

#Rename tên trường dữ liệu bảng project và priority
colnames(project) <- c("project_id", "pkey", "project_name")
colnames(priority) <- c("priority_id", "priority", "define")

#bieu do 12
dta_status <- inner_join(jira, status, by = "issue_id") #Ghép nối lấy status ticket
dta_status <- merge(dta_status,type, by.x = "issuestype",by.y="id")
dta_status <- inner_join(dta_status, project, by = "project_id") #Ghép nối lấy project
dta_status <- full_join(dta_status, priority, by = "priority_id") #Ghép nối lấy priority
dta_status <- inner_join(dta_status, user, by = "user") #Ghép nối chỉ lấy dev, cv, pm

dta_status$priority <- replace_na(dta_status$priority, "Not fill")

dta_status <- dta_status %>%
  filter(project_name != "Pháp chế ISOFHCARE") %>%
  filter(project_name != "Y Khoa Hà Nội") %>%
  filter(project_name != "SYSTEM")



data1<- data_log 
# bieu do 4
qa2 <- data_log %>%
  group_by(project_name) %>%
  summarise(quantity = round(n_distinct(full_name)),
            time = round(sum(hours), 2))%>%
  arrange(desc(quantity))

dta_status1 <- dta_status %>%
  mutate(count = 1) %>%
  group_by(project_name, status) %>%
  summarise(quantity = sum(count)) %>%
  arrange(desc(quantity))

dta_status2 <- dta_status %>%
  mutate(count = 1) %>%
  group_by(pname,priority) %>%
  summarise(quantity = sum(count)) %>%
  arrange(desc(quantity))

qa5<- data_log %>%
  group_by(role) %>%
  summarise(time = round(sum(hours), 2),
            man = n_distinct(user)) %>%
  arrange(desc(time))

#bieu do 14
fe2<- feature %>%
  mutate(count = 1) %>%
  group_by(project_name) %>%
  summarise(estimate = sum(count),
            release = SUM_IFS(count, rate == 1),
            notready = estimate - release) %>%
  mutate(ratio = round((release/estimate)*100, 1)) %>%
  arrange(desc(ratio))

tin1 <- data_log %>%
  filter(issue_type == "Bug" |issue_type == "Hot fix") %>%
  select(issue_type, issue_status)

tin12 <- tin1 %>%
  mutate(count=1) %>%
  summarise(counts = sum(count))
tin13 <- tin1 %>% 
  mutate(count=1)%>%
  filter(issue_status == "DONE LIVE")%>%
  summarise(counts1 = sum(count))
fullbug <- c("Lỗi chưa khắc phục","Lỗi đã khắc phục")
donebug <- c(tin12$counts,tin13$counts1)
tin14 <- data.frame(fullbug, donebug)

creatissue1 <- creatissue %>%
  mutate(createdate = as.Date(creatissue$created, "%Y-%m-%d"),
         soludate = as.Date(creatissue$resolutiondate,"%Y-%m-%d"),
         duedate = as.Date(creatissue$duedate,"%Y-%m-%d"),
         createmonth =  format.Date(created, "%Y-%m"))%>%
  filter(createmonth == "2022-01")%>%
  select(id,issuestatus,createdate,soludate,duedate,createmonth,project,issuetype)


creatissue1 <- merge(creatissue1,project, by.x = "project",by.y = "project_id")

creatissue1 <- merge(creatissue1,status, by.x = "issuestatus",by.y = "issue_id")
### bieu do 9
creatissue131 <- creatissue1 %>%
  filter(issuetype =="10004" | issuetype=="10101")%>%
  mutate(solumonth = format.Date(soludate, "%Y-%m"),
    mana = ifelse(status == "Done" |status == "Closed" |status == "Done Live" |status == "Deployed", "yes", "no"))

creatissue1311 <- creatissue131%>%
  filter(solumonth ==  "2022-01")
creatissue1311 <- creatissue131 %>%
  filter(mana == "yes")%>%
  group_by(project_name)%>%
  mutate(count =1)%>%
  summarise(hasfixed=sum(count))
creatissue132 <- creatissue1 %>%
  filter(issuetype =="10004" | issuetype=="10101")%>%
  group_by(project_name)%>%
  mutate(count =1 )%>%
  summarise(hasmade = sum(count))
creatissue133 <- full_join(creatissue1311,creatissue132,by="project_name")
creatissue133$hasfixed <-replace_na(creatissue133$hasfixed,0)
colnames(creatissue133) <- c("project_name","Lỗi được khắc phục","Lỗi được tạo ra")

creatissue133 <- data.frame(creatissue133[1], stack(creatissue133[2:3]))
##### bieu do 15
creatissue134 <- creatissue1 %>%
  mutate(solumonth = format.Date(soludate, "%Y-%m"),
         mana = ifelse(status == "Done" |status == "Closed" |status == "Done Live" |status == "Deployed", "yes", "no"))
creatissue1341 <- creatissue134%>%
  filter(solumonth ==  "2022-01")
creatissue1341 <- creatissue134 %>%
  filter(mana == "yes")%>%
  group_by(project_name)%>%
  mutate(count =1)%>%
  summarise(hasfixed=sum(count))
creatissue1342 <- creatissue1 %>%
  group_by(project_name)%>%
  mutate(count =1 )%>%
  summarise(hasmade = sum(count))
creatissue1334 <- full_join(creatissue1341,creatissue1342,by="project_name")
creatissue1334$hasfixed <-replace_na(creatissue1334$hasfixed,0)
colnames(creatissue1334) <- c("project_name","Ticket được giải quyết","Ticket được tạo ra")
creatissue1334 <- data.frame(creatissue1334[1], stack(creatissue1334[2:3]))
#b <- c(creatissue131$hasfixed,creatissue132$hasmade)
#tin15 <- data.frame(a, b)

creatissue19 <- creatissue1 %>%
  filter(status == "Done" |status == "Closed" |status == "Done Live" |status == "Deployed" )

#mutate(mana = ifelse(status == "Done" |status == "Closed" |status == "Done Live" |status == "Deployed", "yes", "no"))
cr2 <- creatissue19 %>%
  mutate(count = 1)

#bieu do 1
cr3 <- creatissue19 %>%
  mutate(real = soludate- createdate)%>%
  mutate(estimate = duedate - createdate)

cr3 <- cr3 %>%
  mutate(mm = as.integer(real - estimate))%>%
  select(project_name, mm)

cr3 <- cr3 %>%
  mutate(mm1 = ifelse(mm <= 0 ,"Keep deadline","Miss deadline"))

cr3$mm1 <- replace_na(cr3$mm1, "Not fill")

cr3 <- cr3 %>%
  mutate(count = 1)%>%
  group_by(mm1, project_name)%>%
  summarise(counts = sum(count))


creatissue2 <- creatissue %>%
  mutate(createdate = as.Date(creatissue$created, "%Y-%m-%d"),
         soludate = as.Date(creatissue$resolutiondate,"%Y-%m-%d"),
         duedate = as.Date(creatissue$duedate,"%Y-%m-%d"))%>%
  select(createdate,soludate,duedate,project)
creatissue2 <- merge(creatissue2,project, by.x = "project",by.y = "project_id")
creatissue2 <- creatissue2%>%
  select(createdate,soludate,duedate,project_name)
creatissue2 <- creatissue2 %>%
  mutate(estimate = duedate - createdate)%>%
  mutate(real = soludate - createdate)
creatissue2 <- creatissue2 %>%
  mutate(minus = as.numeric(estimate - real),
         estimate = as.character(estimate),
         real = as.character(real))
#creatissue2$minus <- replace_na(creatissue2$minus,"not") 
#bieu do 2
creatissue23 <- creatissue2 %>%
  filter(!is.na(minus))
creatissue23 <- creatissue23 %>%
  mutate(minus = as.numeric(minus))
creatissue23 <- creatissue23 %>%
  filter(estimate >=0)
creatissue23 <- creatissue23 %>%
  mutate(abc=0)


creatissue22 <- creatissue2 %>%
  mutate(soludate = as.character(soludate),
         duedate = as.character(duedate))
creatissue22$soludate <- replace_na(creatissue22$soludate,"Không có Endate") 
creatissue22$duedate <- replace_na(creatissue22$duedate,"Không điền Duedate")
creatissue221 <- creatissue22 %>%
  mutate(count = 1) %>%
  filter(soludate == "Không có Endate") %>%
  summarise(EndateNotrecorded = sum(count))
creatissue222 <- creatissue22 %>%
  mutate(count = 1) %>%
  filter(duedate == "Không điền Duedate") %>%
  summarise(DuedateNotfill = sum(count)) 
creatissue223 <- creatissue22 %>%
  mutate(count =1 ) %>%
  filter(!is.na(minus))%>%
  summarise(fullrecord = sum(count))
fullbug1 <- c("Chưa ghi nhận ngày Done","Không điền Duedate","Có điền Duedate")
donebug1 <- c(creatissue221$EndateNotrecorded,creatissue222$DuedateNotfill,creatissue223$fullrecord)
creatissue224 <- data.frame(fullbug1, donebug1)

#bieu do 11
effprio1 <- read_excel("effprio1.xlsx")
colnames(priority) <- c("priority","pname","des")

eff15 <- inner_join(effprio1,priority, by = "priority")
colnames(eff15) <- c("priority","id","project","pname","des")
colnames(project) <- c("project","pkey","project_name")
eff15 <- inner_join(eff15,type,by="id")
eff15 <- merge(project,eff15,by.x="project",by.y ="project")
eff15 <- eff15%>%
  filter(pname.y =="Bug"|pname.y=="Hot fix")%>%
  select(pname.x,project_name)

#bieu do 6
effprio <- read_excel("effprio.xlsx")
eff1 <- inner_join(effprio,priority, by = "priority")
eff1 <- merge(eff1,type,by.x ="issuetype",by.y ="id")
eff1 <- eff1%>%
  select(pname.x,pname.y,timeworked)

eff12 <- eff1 %>%
  mutate(timeworked1 = round((timeworked/3600),2))
## bieu do 5 nho
qa16 <- data_log %>%
  group_by(issue_type) %>%
  summarise(time = round(sum(hours), 2)) %>%
  arrange(desc(time)) 
qa16s <- data_log %>%
  group_by(issue_type) %>%
  summarise(quantity = n_distinct(issue_key)) %>%
  arrange(desc(quantity)) 
qa16ss <- inner_join(qa16,qa16s,by = "issue_type")
qa16ss <- qa16ss %>%
  mutate(mean = round(time/quantity,2))%>%
  arrange(desc(mean))
