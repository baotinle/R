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
              #column(12,
              #       box(width = 12,title = paste0("📊 📉 ","Chỉ số tổng quan"),
              #           column(4,sparkBoxOutput("progressBox",height = 54)),
              #           column(4,sparkBoxOutput("progressBox3",height = 54)),
              #           column(4,sparkBoxOutput("progressBox2",height = 54)))),
              column(12,
                     box(width = 12,title = p(tags$p(strong("Biểu đồ 1: Biểu đồ thể hiện mức độ hoàn thành deadline"))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "project5",
                                             label = "Lựa chọn dự án",
                                             choices = c("Tất cả", sort(unique(cr3$project_name))),
                                             selected = "Tất cả"),
                       solidHeader = FALSE, collapsible = TRUE),
                     box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                         apexchartOutput("p15",height = 350)
                     ))),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 2: Biểu đồ thể hiện biên độ dao động của thời gian hoàn thành so với deadline "))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "eff13",
                                             label = "Lựa chọn dự án",
                                             choices = c("Tất cả", sort(unique(creatissue23$project_name))),
                                             selected = "Tất cả"),
                       solidHeader = FALSE, collapsible = TRUE),
                     box(width = 12, solidHeader = FALSE, collapsible = TRUE,                         
                         apexchartOutput("p19",height = 350)))),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 3: Biểu đồ kết hợp giữa Tổng số giờ làm việc và Tổng số người tham gia theo ngày"))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "project",
                                             label = "Lựa chọn dự án",
                                             choices = c("Tất cả", sort(unique(data1$project_name))),
                                             selected = "Tất cả",
                                             multiple = FALSE)),
                     box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                         apexchartOutput("p1",height = 350)))),
              column(12,
                     box(width = 12, solidHeader = FALSE, collapsible = TRUE,
                         title = p(tags$p(strong("Biểu đồ 4: Biểu đồ kết hợp giữa tổng số giờ làm việc và số lượng người tham gia theo dự án"))),
                         apexchartOutput("p2",height = 350)
                         #box(apexchartOutput("p1", height = 250)),
                         #box(apexchartOutput("p2", height = 250)))
                     )),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 5: Biểu đồ phân tích tổng số efforts làm việc trên mỗi loại Issue Type "))),
                     mainPanel(
                     shiny::selectizeInput(inputId = "project3",
                                           label = "Lựa chọn dự án",
                                           choices = c("Tất cả", sort(unique(data_log$project_name))),
                                           selected = "Tất cả")),
                     box(width = 7,collapsible = TRUE,
                         apexchartOutput("p12",height = 300)),
                     box(width = 5,collapsible = TRUE,
                         apexchartOutput("p21",height = 300)))),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 6: Biểu đồ thể hiện tổng số efforts đã làm theo mức độ ưu tiên (Priority) của ticket"))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "prio1",
                                             label = "Lựa chọn Issuetype",
                                             choices = c("Tất cả", sort(unique(eff12$pname.y))),
                                             selected = "Tất cả"),
                       solidHeader = FALSE, collapsible = TRUE),
#                     mainPanel(
#                       shiny::selectizeInput(inputId = "status2",
#                                             label = "Lựa chọn Issuetype",
#                                             choices = c("Tất cả", sort(unique(dta_status2$pname))),
#                                             selected = "Tất cả"),
#                       solidHeader = FALSE, collapsible = TRUE),
                     box(width = 12, solidHeader = FALSE, collapsible = TRUE, 
                         apexchartOutput("p16",height = 350))
#                     box(width = 6,solidHeader = FALSE, collapsible = TRUE, 
#                         title = p(tags$p(strong("Biểu đồ 6: Biểu đồ thể hiện số lượng ticket theo mức độ ưu tiên (priority)"))),
#                         apexchartOutput("p4",height = 350))
              )),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 7: Biểu đồ thể hiện tổng efforts và số ticket loại ‘Quản lý’ của PM"))),
                     shiny::selectizeInput(inputId = "PM1",
                                           label = "Lựa chọn dự án",
                                           choices = c("Tất cả", sort(unique(data_log$project_name))),
                                           selected = "Tất cả"),
                     box(width = 12,
                         box(width = 6,collapsible = TRUE,
                             apexchartOutput("p6",height = 350)),
                         box(width = 6, solidHeader = FALSE, collapsible = TRUE,
                             apexchartOutput("p7",height = 350)))
              )),
              column(12,
                     box(width = 6,collapsible = TRUE,
                         shiny::selectizeInput(inputId = "project4",
                                               label = "Lựa chọn dự án",
                                               choices = c("Tất cả", sort(unique(data_log$project_name))),
                                               selected = "Tất cả"),
                         title = p(tags$p(strong("Biểu đồ 8: Biểu đồ thể hiện tỷ lệ các loại ticket đã triển khai"))),
                         apexchartOutput("p13",height = 350)),
                     box(width = 6, solidHeader = FALSE, collapsible = TRUE,
                         shiny::selectizeInput(inputId = "bug2",
                                               label = "Lựa chọn dự án",
                                               choices = c("Tất cả", sort(unique(creatissue133$project_name))),
                                               selected = "Tất cả"),
                         title = p(tags$p(strong("Biểu đồ 9: Biểu đồ thể hiện ‘Lỗi’ được tạo ra và 'Lỗi' đã khắc phục"))),
                         apexchartOutput("p14",height = 350))
              ),
              column(12,
                     box(width = 6, solidHeader = FALSE, collapsible = TRUE,
                         shiny::selectizeInput(inputId = "bug1",
                                               label = "Lựa chọn dự án",
                                               choices = c("Tất cả", sort(unique(data_log$project_name))),
                                               selected = "Tất cả"),
                         title = p(tags$p(strong("Biểu đồ 10: Biểu đồ thể hiện số lượng Lỗi trên các môi trường"))),
                         apexchartOutput("p18",height = 350)),
                     box(width = 6,collapsible = TRUE,
                         shiny::selectizeInput(inputId = "bug3",
                                               label = "Lựa chọn dự án",
                                               choices = c("Tất cả", sort(unique(eff15$project_name))),
                                               selected = "Tất cả"),
                         title = p(tags$p(strong("Biểu đồ 11: Biểu đồ thể hiện tỷ lệ gặp Lỗi theo mức độ nghiêm trọng"))),
                         apexchartOutput("p17",height = 350))
              ),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 12: Biểu đồ thể hiện kết quả xử lý các ticket"))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "status",
                                             label = "Lựa chọn dự án",
                                             choices = c("Tất cả", sort(unique(dta_status1$project_name))),
                                             selected = "Tất cả")),
                     box(width = 12,collapsible = TRUE,
                         apexchartOutput("p3",height = 350))
              )),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 13: Biểu đồ thể hiện số lượng và thời gian làm việc hiệu quả/không hiệu quả trên các ticket"))),
                     mainPanel(
                       shiny::selectizeInput(inputId = "project2",
                                             label = "Lựa chọn dự án",
                                             choices = c("Tất cả", sort(unique(data_log$project_name))),
                                             selected = "Tất cả")),
                     box(width = 12,
                         box(width = 6,collapsible = TRUE,
                             apexchartOutput("p10",height = 350)),
                         box(width = 6, solidHeader = FALSE, collapsible = TRUE,
                             apexchartOutput("p11",height = 350)))
              )),
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 14: Biểu đồ thể hiện tỷ lệ hoàn thành của Dự án"))),
                         apexchartOutput("p8",height = 350))
              ),
              #    ,
              #    column(12,
              #           box(width = 12,
              #               title = p(tags$p(strong("Biểu đồ +: Biểu đồ thể hiện tổng số lượng và thời gian làm việc theo từng loại ticket"))),
              #               apexchartOutput("p9",height = 350))
              #           )
              column(12,
                     box(width = 12,
                         title = p(tags$p(strong("Biểu đồ 15: Biểu đồ thể hiện tỷ lệ ticket giải quyết được trong tháng"))),
                         mainPanel(
                           shiny::selectizeInput(inputId = "bug4",
                                                 label = "Lựa chọn dự án",
                                                 choices = c("Tất cả", sort(unique(creatissue1334$project_name))),
                                                 selected = "Tất cả")),
                         box(width = 12,collapsible = TRUE,
                             apexchartOutput("p20",height = 350))
                     )),
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