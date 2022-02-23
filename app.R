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

bugenv <- read_excel("bug-env.xls")
colnames(bugenv) <-c("Issuetype","enviroment")
bugenv$enviroment <-replace_na(bugenv$enviroment,"Not fill")

effprio <- read_excel("effprio.xlsx")
colnames(priority) <- c("priority","pname","des")

eff1 <- merge(effprio,priority, by = "priority")
eff1 <- merge(eff1,type,by.x ="issuetype",by.y ="id")
eff1 <- eff1%>%
    select(pname.x,pname.y,timeworked)
eff12 <- eff1 %>%
    mutate(timeworked1 = round((timeworked/3600),2))


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
data_log <- data %>% 
    filter(!is.na(project_name)) %>%
    filter(project_name != "Pháp chế ISOFHCARE") %>%
    filter(project_name != "Y Khoa Hà Nội") %>%
    filter(project_name != "SYSTEM")%>%
    mutate(date = format.Date(date,"%d-%m-%Y"))


dta_log <- data_log %>% 
    mutate(project = project_name) %>%
    separate(project, c("project1", "project2"), "\\_") %>%
    filter(project2 == "TTB" | project2 == "HIS")

dta_log$project1[dta_log$project1 == "BV Đại học Y"] <- "BV Đại Học Y"

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


data1<- data_log %>%
    filter(weekday != "Saturday",
           weekday != "Sunday")

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
fullbug <- c("Lỗi tạo ra","Lỗi đã khắc phục")
donebug <- c(tin12$counts,tin13$counts1)
tin14 <- data.frame(fullbug, donebug)

creatissue1 <- creatissue %>%
    mutate(createdate = as.Date(creatissue$created, "%Y-%m-%d"),
           soludate = as.Date(creatissue$resolutiondate,"%Y-%m-%d"),
           duedate = as.Date(creatissue$duedate,"%Y-%m-%d"),
           createmonth =  format.Date(created, "%Y-%m")) %>%
    filter(createmonth == "2022-01")%>%
    select(id,issuestatus,createdate,soludate,duedate,createmonth,project)


creatissue1 <- merge(creatissue1,project, by.x = "project",by.y = "project_id")

creatissue1 <- merge(creatissue1,status, by.x = "issuestatus",by.y = "issue_id")
creatissue1 <- creatissue1 %>%
    filter(status == "Done" |status == "Closed" |status == "Done Live" |status == "Deployed" )
cr2 <- creatissue1 %>%
    mutate(count = 1)


cr3 <- creatissue1 %>%
    mutate(real = soludate- createdate)%>%
    mutate(estimate =duedate - createdate)

cr3 <- cr3 %>%
    mutate(mm = as.integer(real - estimate))%>%
    select(mm,project_name)

cr3 <- cr3 %>%
    mutate(mm1 = ifelse(mm <= 0 ,"Keep deadline","Miss deadline"))

cr3$mm1 <- replace_na(cr3$mm1,"Not fill")

cr3 <- cr3 %>%
    mutate(count = 1)%>%
    group_by(mm1,project_name)%>%
    summarise(counts = sum(count))
#################################################### UI #################################################################
header <- dashboardHeader(titleWidth = 180,
                          title = paste0("🧬   ","QA    "),
                          controlbarIcon = icon("share-alt-square"),
                          tags$li(a(href = 'https://isofh.com',
                                    img(src = 'img/logo-isofh.png',
                                        title = "ISOFH.COM", height = "20px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
                          
)

sidebar <- dashboardSidebar(width = 180, collapsed = TRUE,minified = TRUE,
                            sidebarMenu(
                                menuItem(text = "Biểu đồ phân tích", tabName = "dashboard", icon = icon("dashboard"),badgeColor = "red"),
                                menuItem(text = "Dữ liệu", tabName = "widgets", icon = icon("th"))
                                )
                            )

body <- dashboardBody(
    tags$head(
        tags$link( 
            rel = "shortcut icon", 
            type = "image/png", 
            href = "https://res.cloudinary.com/dxqnb8xjb/image/upload/v1510505618/tychobra-logo-blue_d2k9vt.png"
        ),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "custom.js"),
        tags$script(src="https://cdn.jsdelivr.net/npm/lodash@4.17.15/lodash.min.js")
    ),
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    column(12,
                           box(width = 12,title = paste0("📊 📉 ","Chỉ số tổng quan"),
                               column(4,sparkBoxOutput("progressBox",height = 54)),
                               column(4,sparkBoxOutput("progressBox3",height = 54)),
                               column(4,sparkBoxOutput("progressBox2",height = 54)))),
                    column(12,
                           mainPanel(
                               shiny::selectizeInput(inputId = "project5",
                                                     label = "Lựa chọn dự án",
                                                     choices = c("Tất cả", sort(unique(cr3$project_name))),
                                                     selected = "Tất cả"),
                               solidHeader = FALSE, collapsible = TRUE),
                           box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 1: Biểu đồ thể hiện mức độ hoàn thành deadline"))),
                               apexchartOutput("p15",height = 350)
                           )),
                    column(12,
                           mainPanel(
                               shiny::selectizeInput(inputId = "project",
                                                     label = "Lựa chọn dự án",
                                                     choices = c("Tất cả", sort(unique(data1$project_name))),
                                                     selected = "Tất cả",
                                                     multiple = FALSE)),
                           box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 3: Biểu đồ kết hợp giữa tổng số giờ làm việc và tổng người tham gia theo ngày"))),#khong tinh t7,cn
                               apexchartOutput("p1",height = 350))),
                    column(12,
                           box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 4: Biểu đồ kết hợp giữa tổng số giờ làm việc và số lượng người tham gia theo dự án"))),
                               apexchartOutput("p2",height = 350)
                               #box(apexchartOutput("p1", height = 250)),
                               #box(apexchartOutput("p2", height = 250)))
                           )),
                    column(12,
                           shiny::selectizeInput(inputId = "project3",
                                                 label = "Lựa chọn dự án",
                                                 choices = c("Tất cả", sort(unique(data_log$project_name))),
                                                 selected = "Tất cả"),
                           box(width = 12,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 5: Biểu đồ thể hiện efforts theo Issue Type"))),
                               apexchartOutput("p12",height = 300))),
                    column(12,
                           mainPanel(
                               shiny::selectizeInput(inputId = "prio1",
                                                     label = "Lựa chọn Issuetype",
                                                     choices = c("Tất cả", sort(unique(eff12$pname.y))),
                                                     selected = "Tất cả"),
                               solidHeader = FALSE, collapsible = TRUE),
                           mainPanel(
                               shiny::selectizeInput(inputId = "status2",
                                                     label = "Lựa chọn Issuetype",
                                                     choices = c("Tất cả", sort(unique(dta_status2$pname))),
                                                     selected = "Tất cả"),
                               solidHeader = FALSE, collapsible = TRUE),
                           box(width = 6, solidHeader = FALSE, collapsible = TRUE, 
                               title = p(tags$p(strong("Biểu đồ 6: Biểu đồ thể hiện tổng số efforts đã làm theo mức độ ưu tiên của ticket"))),
                               apexchartOutput("p16",height = 350)),
                           box(width = 6,solidHeader = FALSE, collapsible = TRUE, 
                               title = p(tags$p(strong("Biểu đồ 6: Biểu đồ thể hiện số lượng ticket theo mức độ ưu tiên (priority)"))),
                               apexchartOutput("p4",height = 350))
                           ),
                    column(12,
                           shiny::selectizeInput(inputId = "PM1",
                                                 label = "Lựa chọn dự án",
                                                 choices = c("Tất cả", sort(unique(data_log$project_name))),
                                                 selected = "Tất cả"),
                           box(width = 6,
                               title = p(tags$p(strong("Biểu đồ 7: Biểu đồ thể hiện efforts của PM"))),
                               apexchartOutput("p6",height = 300)),
                           box(width = 6,
                               title = p(tags$p(strong("Biểu đồ 7: Biểu đồ thể hiện số lượng ticket của PM"))),
                               apexchartOutput("p7",height = 300))
                    ),
                    column(12,
                           shiny::selectizeInput(inputId = "project4",
                                                 label = "Lựa chọn dự án",
                                                 choices = c("Tất cả", sort(unique(data_log$project_name))),
                                                 selected = "Tất cả"),
                           box(width = 6,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 8: Biểu đồ thể hiện tỷ lệ các loại ticket đã triển khai"))),
                               apexchartOutput("p13",height = 350)),
                           box(width = 6, solidHeader = FALSE, collapsible = TRUE, 
                               title = p(tags$p(strong("Biểu đồ 9: Biểu đồ thể hiện tổng số lượng ‘Lỗi’ được tạo ra và số Lỗi đã khắc phục"))),
                               apexchartOutput("p14",height = 350))
                    ),
                    column(12,
                           box(width = 6, solidHeader = FALSE, collapsible = TRUE, 
                               title = p(tags$p(strong("Biểu đồ 10: : Biểu đồ thể hiện số lượng Lỗi trên các môi trường"))),
                               apexchartOutput("p18",height = 350)),
                           box(width = 6,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 11: : Biểu đồ thể hiện tỷ lệ gặp Lỗi theo mức độ nghiêm trọng"))),
                               apexchartOutput("p17",height = 350))
                    ),
                    column(12,
                           mainPanel(
                               shiny::selectizeInput(inputId = "status",
                                                     label = "Lựa chọn dự án",
                                                     choices = c("Tất cả", sort(unique(dta_status1$project_name))),
                                                     selected = "Tất cả")),
                           box(width = 12,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 12: Biểu đồ thể hiện kết quả xử lý các ticket"))),
                               apexchartOutput("p3",height = 350))
                    ),
                    column(12,
                           mainPanel(
                               shiny::selectizeInput(inputId = "project2",
                                                     label = "Lựa chọn dự án",
                                                     choices = c("Tất cả", sort(unique(data_log$project_name))),
                                                     selected = "Tất cả")),
                           box(width = 6,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 13: Biểu đồ thể hiện thời gian làm việc hiệu quả/không hiệu quả"))),
                               apexchartOutput("p10",height = 300)),
                           box(width = 6,collapsible = TRUE,
                               title = p(tags$p(strong("Biểu đồ 13: Biểu đồ thể hiện tổng số lượng ticket làm việc hiệu quả/không hiệu quả"))),
                               apexchartOutput("p11",height = 300))
                    ),
                    column(12,
                           box(width = 12,
                               title = p(tags$p(strong("Biểu đồ 14: Biểu đồ thể hiện tỷ lệ hoàn thành của Dự án"))),
                               apexchartOutput("p8",height = 350))
                           )
                #    ,
                #    column(12,
                #           box(width = 12,
                #               title = p(tags$p(strong("Biểu đồ +: Biểu đồ thể hiện tổng số lượng và thời gian làm việc theo từng loại ticket"))),
                #               apexchartOutput("p9",height = 350))
                #           )
                    
                )
                ),
    tabItem(tabName = "widgets"
                #fluidRow(
                #    column(12,
                #           box(width = 12,DTOutput('tb1')
                #    )
                #)
            #)
        )
    )
)

menu <- controlbarMenu(
    id = "p2",
    controlbarItem(
        "Tab 1",
        "Welcome to tab 1"
    ),
    controlbarItem(
        "Tab 2",
        numericInput("num", "Observations:", 200, min = 1, max = 1000, step = 100)
    )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
    skin = "red-light",
    header,
    sidebar,
    controlbar = dashboardControlbar(
        collapsed = TRUE, 
        skinSelector(),
        skin = "dark",
        menu
    ),
    options = list(sidebarExpandOnHover = TRUE),
    body,
    title = "DAC"
)

######################################## SERVER #########################################################################
server <- function(input, output) {
    output$p1<- renderApexchart({
        
        if (input$project == "Tất cả") {
            qa1 <- data1 %>%
                mutate(date = as.character(date)) %>%
                group_by(date) %>%
                summarise(quantity = n_distinct(user),
                          time = round(sum(hours), 2)) %>%
                apex(type = "column", aes(date, quantity),serie_name = "Số người", height = 400) %>%
                add_line(aes(date, time), serie_name = "Số giờ làm việc") %>%
                ax_colors(c("#781D42", "#FFB319")) %>%
                ax_stroke(curve = "smooth", width = c(0,3)) %>%
                ax_markers(size = 4) %>%
                ax_yaxis(
                    decimalsInFloat = 0,
                    title = list(text = "Số lượng người tham gia"),
                    labels = list(
                        minWidth = 40)) %>% 
                ax_yaxis2(
                    opposite = TRUE,
                    decimalsInFloat = 0,
                    title = list(text = "Số giờ làm việc"),
                    labels = list(
                        formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF") %>% 
                ax_legend(position = "bottom") 
           
            
        } else {
            qa1 <- data1 %>%
                filter(project_name == input$project) %>%
                mutate(date = as.character(date)) %>%
                group_by(date) %>%
                summarise(quantity = n_distinct(user),
                          time = round(sum(hours), 2)) %>%
                apex(type = "column", aes(date, quantity),serie_name = "Số người", height = 400) %>%
                add_line(aes(date, time), serie_name = "Số giờ làm việc") %>%
                ax_colors(c("#781D42", "#FFB319")) %>%
                ax_stroke(curve = "smooth", width = c(0,3)) %>%
                ax_markers(size = 4) %>%
                ax_yaxis(
                    decimalsInFloat = 0,
                    title = list(text = "Số lượng người tham gia"),
                    labels = list(
                        minWidth = 40)) %>% 
                ax_yaxis2(
                    opposite = TRUE,
                    decimalsInFloat = 0,
                    title = list(text = "Số giờ làm việc"),
                    labels = list(
                        formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF") %>% 
                ax_legend(position = "bottom")
             }
        
    })
    
    output$p2<- renderApexchart({
        apex(qa2, type = "column", aes(project_name, quantity), serie_name = "Số lượng người tham gia", height = 400) %>%
            add_line(aes(project_name, time), serie_name = "Số giờ làm việc") %>%
            ax_colors("#916BBF", "#FFB740") %>%
            ax_stroke(curve = "smooth", width = c(0,3)) %>%
            ax_markers(size = 4) %>%
            ax_xaxis(labels = list(rotate = -70, minHeight = 90, trim = TRUE)) %>%
            ax_yaxis(
                decimalsInFloat = 0,
                title = list(text = "Số lượng người tham gia"),
                labels = list(
                    minWidth = 40)) %>%
            ax_yaxis2(
                opposite = TRUE,
                decimalsInFloat = 0,
                title = list(text = "Số giờ làm việc"),
                labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
            ax_grid(
                show = FALSE,
                position = "front",
                borderColor = "#FFF") %>% 
            ax_legend(position = "bottom")
    })
    
    output$p3<- renderApexchart({
        
        if (input$status == "Tất cả") {
            qa1 <- dta_status %>%
                mutate(count = 1) %>%
                group_by(status) %>%
                summarise(quantity = sum(count)) %>%
                arrange(desc(quantity)) %>%
                apex(type = "column", aes(status, quantity), serie_name = "Số lượng ticket", height = 400) %>%
                ax_colors("#519259") %>%
                ax_yaxis(
                    decimalsInFloat = 0,
                    title = list(text = "Số lượng ticket"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa1
            
        } else {
            qa1 <- dta_status1 %>%
                filter(project_name == input$status) %>%
                apex(type = "column", aes(status, quantity), serie_name = "Số lượng ticket", height = 400) %>%
                ax_colors("#519259") %>%
                ax_yaxis(
                    decimalsInFloat = 0,
                    title = list(text = "Số lượng ticket"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa1 }
        
    })
    
    output$p4<- renderApexchart({
        
        if (input$status2 == "Tất cả") {
            qa1 <- dta_status %>%
                mutate(count = 1) %>%
                group_by(priority) %>%
                summarise(quantity = sum(count)) %>%
                arrange(desc(quantity)) %>%
                apex(type = "column", aes(priority, quantity), serie_name = "Số lượng ticket", height = 400) %>%
                ax_colors("#D4AC2B") %>%
                ax_yaxis(
                    decimalsInFloat = 1,
                    title = list(text = "Số lượng ticket"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa1
            
        } else {
            qa1 <- dta_status2 %>%
                filter(pname == input$status2) %>%
                apex(type = "column", aes(priority, quantity), serie_name = "Số lượng ticket", height = 400) %>%
                ax_colors("#D4AC2B") %>%
                ax_yaxis(
                    decimalsInFloat = 0,
                    title = list(text = "Số lượng ticket"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa1 }
        
    })
    output$p5 <- renderApexchart({
    apexchart(ax_opts = list(
        dataLabels = list(
            enabled = FALSE),
        series = list(
            list(
                name = "Số giờ", type = "bar", data = qa5$time),
            list(
                name = "Số người", type = "bar", data = qa5$man)
        ),
        xaxis = list(categories = qa5$role)),
        width = "900px",
        height = 350) %>%
        ax_xaxis(labels = list(rotate = -70, minHeight = 20, trim = TRUE))%>%
        ax_yaxis(decimalsInFloat = 0,
                 title = list(text = "Thời gian làm việc"),
                 min = 0,
                 max = 6000,
                 labels = list(
                     formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
        ax_yaxis2(opposite = TRUE,
                  decimalsInFloat = 0,
                  title = list(text = "Số người"),
                  max = 45) %>%
        ax_stroke(width = c(0,0)) %>%
        ax_colors("#E5890A", "#F7D08A") %>%
        ax_grid(
            show = FALSE,
            position = "front",
            borderColor = "#FFF") %>% 
        ax_legend(position = "bottom")
    })
    
    output$p6 <- renderApexchart({
    ##### **Biểu đồ 6: Biểu đồ thể hiện efforts và số lượng ticket **
        if (input$PM1 == "Tất cả") {
        qa6<- data_log %>%
            filter(role == "PM") %>%
            group_by(mana) %>%
            summarise(time = round(sum(hours), 2)) %>%
            arrange(desc(time)) %>%
            apex(type = "pie", aes(mana, time)) %>%
            ax_colors("#89B5AF", "#9AE66E") %>%
            ax_labs(
                title = "Đơn vị: Số giờ làm việc") %>%
            ax_title(
                style = list(fontSize = "13px", color = "#808080")) %>%
            ax_yaxis(labels = list(
                formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
        qa6
        }
        else{
            qa6<- data_log %>%
                filter(project_name == input$PM1)%>%
                filter(role == "PM") %>%
                group_by(mana,project_name) %>%
                summarise(time = round(sum(hours), 2)) %>%
                arrange(desc(time)) %>%
                apex(type = "pie", aes(mana, time)) %>%
                ax_colors("#89B5AF", "#9AE66E") %>%
                ax_labs(
                    title = "Đơn vị: Số giờ làm việc") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
            qa6
        }
        })
    
    output$p7 <- renderApexchart({
        if (input$PM1 == "Tất cả") {
        qa6s<- data_log %>%
            filter(role == "PM") %>%
            group_by(mana) %>%
            summarise(quantity = n_distinct(issue_key)) %>%
            arrange(desc(quantity)) %>%
            apex(type = "pie", aes(mana, quantity)) %>%
            ax_colors("#FC9918", "#9AE66E") %>%
            ax_labs(title = "Đơn vị: Số lượng ticket") %>%
            ax_title(
                style = list(fontSize = "13px", color = "#808080")) %>%
            ax_yaxis(labels = list(
                formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
        qa6s
        }
        else{
            qa6s<- data_log %>%
                filter(project_name == input$PM1)%>%
                filter(role == "PM") %>%
                group_by(mana,project_name) %>%
                summarise(quantity = n_distinct(issue_key)) %>%
                arrange(desc(quantity)) %>%
                apex(type = "pie", aes(mana, quantity)) %>%
                ax_colors("#FC9918", "#35589A") %>%
                ax_labs(title = "Đơn vị: Số lượng ticket") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
            qa6s
        }
    })
    
    output$p8 <- renderApexchart({
        apexchart(ax_opts = list(
        dataLabels = list(
            enabled = FALSE),
        series = list(
            list(
                name = "Đã hoàn thành", type = "bar", data = fe2$release),
            list(
                name = "Chưa hoàn thành", type = "bar", data = fe2$notready),
            list(
                name = "Chỉ số SPI", type = "line", data = fe2$ratio,
                text = htmlwidgets::JS("function(value) {return value + ' h';}")
            )
        ),
        xaxis = list(
            categories = fe2$project_name)),
        height = 400) %>%
        ax_xaxis(labels = list(rotateAlways = TRUE, minHeight = 80)) %>%
        ax_yaxis(decimalsInFloat = 0,
                 max = 100) %>%
        ax_colors("#000099", "#9999ff", "#ff9900") %>%
        ax_stroke(width = c(0,0,4)) %>%
        ax_markers(size = 4) %>%
        ax_chart(stacked = TRUE) %>%
        ax_grid(
            show = FALSE,
            position = "front",
            borderColor = "#FFF") %>% 
        ax_legend(position = "top")
    })
    
    output$p9 <- renderApexchart({
    qa8<- data_log %>%
        group_by(issue_type) %>% 
        summarise(quantity = n_distinct(issue_key),
                  hour = round(sum(hours)), 2) %>%
        arrange(desc(quantity)) %>%
        apex(type = "column", aes(issue_type, quantity), serie_name = "Số lượng ticket", height = 400) %>%
        add_line(aes(issue_type, hour), serie_name = "Thời gian thực hiện") %>%
        ax_colors("#660033", "#ff9900") %>%
        ax_stroke(curve = "smooth", width = c(0,3)) %>%
        ax_markers(size = 4) %>%
        ax_xaxis(labels = list(rotateAlways = TRUE, minHeight = 90)) %>%
        ax_yaxis(
            decimalsInFloat = 0,
            title = list(text = "Số lượng ticket"),
            labels = list(
                minWidth = 40)) %>% 
        ax_yaxis2(
            opposite = TRUE,
            decimalsInFloat = 0,
            title = list(text = "Số giờ làm việc"),
            labels = list(
                formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
        ax_grid(
            show = FALSE,
            position = "front",
            borderColor = "#FFF") %>% 
        ax_legend(position = "top")
    })
    
    output$p10<- renderApexchart({
        if (input$project2 == "Tất cả") {
            qa6<- data_log %>%
                group_by(eff) %>%
                summarise(time = round(sum(hours), 2)) %>%
                apex(type = "pie", aes(eff, time), height = 400) %>%
                ax_colors("#34BE82", "#105652") %>%
                ax_labs(title = "Đơn vị: giờ (h)") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
            qa6
        } else {
            qa6<- data_log %>%
                filter(project_name == input$project2) %>%
                group_by(eff) %>%
                summarise(time = round(sum(hours), 2)) %>%
                apex(type = "pie", aes(eff, time), height = 400) %>%
                ax_colors("#34BE82", "#105652") %>%
                ax_labs(title = "Đơn vị: giờ (h)") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
            qa6
        }
        
    })
    
    output$p11<- renderApexchart({
        
        
        if (input$project2 == "Tất cả") {
            qa6<- data_log %>%
                group_by(eff2) %>%
                summarise(quantity = n_distinct(issue_key)) %>%
                apex(type = "pie", aes(eff2, quantity), height = 400) %>%
                ax_colors("#34BE82", "#105652") %>%
                ax_labs(title = "Đơn vị: Số lượng ticket") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
            qa6
        } else {
            qa6<- data_log %>%
                filter(project_name == input$project2) %>%
                group_by(eff2) %>%
                summarise(quantity = n_distinct(issue_key)) %>%
                apex(type = "pie", aes(eff2, quantity), height = 400) %>%
                ax_colors("#34BE82", "#105652") %>%
                ax_labs(title = "Đơn vị: Số lượng ticket") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
            qa6
        }
    })
    
    output$p12<- renderApexchart({
        if (input$project3 == "Tất cả") {
            qa10<- data_log %>%
                group_by(issue_type) %>%
                summarise(time = round(sum(hours), 2)) %>%
                arrange(desc(time)) %>%
                apex(type = "pie", aes(issue_type, time), height = 400) %>%
                ax_labs(title = "Đơn vị: giờ (h)") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
            qa10
        } else {
            qa10<- data_log %>%
                filter(project_name == input$project3) %>%
                group_by(issue_type) %>%
                summarise(time = round(sum(hours), 2)) %>%
                arrange(desc(time)) %>%
                apex(type = "pie", aes(issue_type, time), height = 400) %>%
                ax_labs(title = "Đơn vị: giờ (h)") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' h';}")))
            qa10
        }
        
    })
    
    
    ###### Create plot 2
    output$p13<- renderApexchart({
        if (input$project4 == "Tất cả") {
            qa10s<- data_log %>%
                group_by(issue_type) %>%
                summarise(quantity = n_distinct(issue_key)) %>%
                arrange(desc(quantity)) %>%
                apex(type = "pie", aes(issue_type, quantity), height = 400) %>%
                ax_labs(title = "Đơn vị: Số lượng ticket") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
            qa10s
        } else {
            qa10s<- data_log %>%
                filter(project_name == input$project4) %>%
                group_by(issue_type) %>%
                summarise(quantity = n_distinct(issue_key)) %>%
                arrange(desc(quantity)) %>%
                apex(type = "pie", aes(issue_type, quantity), height = 400) %>%
                ax_labs(title = "Đơn vị: Số lượng ticket") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) %>%
                ax_yaxis(labels = list(
                    formatter = htmlwidgets::JS("function(value) {return value + ' ticket';}")))
            qa10s
        }
    })
    
    output$p14<- renderApexchart({
        qa12<- tin14 %>%
            apex(type = "pie", aes(tin14$fullbug,tin14$donebug)) %>%
            ax_colors("#BB6464", "#395B64") %>%
            ax_labs(
                title = "Đơn vị: ticket") %>%
            ax_title(
                style = list(fontSize = "13px", color = "#808080")) 
        qa12
    })
    
    output$p15<- renderApexchart({
        if (input$project5 == "Tất cả") {
            qa13<- cr3 %>%
                group_by(mm1)%>%
                summarise(count1 = sum(counts))%>%
                apex(type = "pie", aes(mm1,count1)) %>%
                ax_colors("#113CFC","#FF2626", "#FDA65D") %>%
                ax_labs(
                    title = "Đơn vị: phần trăm") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) 
            qa13
        }
        else {
            qa13<- cr3 %>%
                filter(project_name == input$project5)%>%
                group_by(project_name,mm1)%>%
                summarise(count2 = sum(counts))%>%
                apex(type = "pie", aes(mm1,count2)) %>%
                ax_colors("#113CFC","#FF2626", "#FDA65D") %>%
                ax_labs(
                    title = "Đơn vị: phần trăm") %>%
                ax_title(
                    style = list(fontSize = "13px", color = "#808080")) 
            qa13
        }
    })
    
    output$p16<- renderApexchart({
        if (input$prio1 == "Tất cả") {
            qa14 <- eff12 %>%
                group_by(pname.x) %>%
                summarise(time1 = sum(timeworked1)) %>%
                arrange(desc(time1)) %>%
                apex(type = "column", aes(pname.x, time1), serie_name = "Số giờ", height = 400) %>%
                ax_colors("#876445") %>%
                ax_yaxis(
                    decimalsInFloat = 1,
                    title = list(text = "Số giờ"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa14
        }
        else{
            qa14 <- eff12 %>%
                filter(pname.y ==input$prio1)%>%
                group_by(pname.x) %>%
                summarise(time1 = sum(timeworked1)) %>%
                arrange(desc(time1)) %>%
                apex(type = "column", aes(pname.x, time1), serie_name = "Số giờ", height = 400) %>%
                ax_colors("#876445") %>%
                ax_yaxis(
                    decimalsInFloat = 1,
                    title = list(text = "Số giờ"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa14
        }
        
    })
    
    output$p17<- renderApexchart({
            qa15 <- eff12 %>%
                filter(pname.y=="Bug")%>%
                group_by(pname.x) %>%
                summarise(time1 = sum(timeworked1)) %>%
                arrange(desc(time1)) %>%
                apex(type = "pie", aes(pname.x, time1), serie_name = "Số giờ", height = 400) %>%
                ax_colors("#3DB2FF","#E05D5D") %>%
                ax_yaxis(
                    decimalsInFloat = 1,
                    title = list(text = "Số giờ"),
                    labels = list(
                        formatter = format_num("~s"),
                        minWidth = 40)) %>% 
                ax_grid(
                    show = FALSE,
                    position = "front",
                    borderColor = "#FFF")
            qa15

    })
    
    output$p18<- renderApexchart({
        qa16<- bugenv %>%
            mutate(count =1)%>%
            group_by(enviroment)%>%
            summarise(counts = sum(count))%>%
            arrange(desc(counts))%>%
            apex(type = "pie", aes(enviroment,counts)) %>%
            ax_colors("#113CFC","#FF2626", "#FDA65D","#219F94","#FF6464") %>%
            ax_labs(
                title = "Đơn vị: phần trăm") %>%
            ax_title(
                style = list(fontSize = "13px", color = "#808080")) 
        qa16
        
    })
    
    output$progressBox <- renderSparkBox({
        spark_box(
            data = qa5,
            title = paste0(format(sum(qa5$time), big.mark = "")),
            subtitle = "Tổng số giờ logtime",
            color = "#FFF", background = "#7267CB",
            title_style = list(color = "#FFF", fontSize = "20px"),
            subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
        )
    })
    
    output$progressBox2 <- renderSparkBox({
        spark_box(
            data = qa5,
            title = paste0(format(sum(qa5$man), big.mark = "")),
            subtitle = "Tổng số người tham gia",
            color = "#FFF", background = "#C37B89",
            title_style = list(color = "#FFF", fontSize = "20px"),
            subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
        )
    })
    
    output$progressBox3 <- renderSparkBox({
        spark_box(
            data = cr2,
            title = paste0(format(sum(cr2$count), big.mark = "")),
            subtitle = "Tổng số ticket tạo ra",
            color = "#FFF", background = "#8E806A",
            title_style = list(color = "#FFF", fontSize = "20px"),
            subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
        )
    })
}

shinyApp(ui, server)