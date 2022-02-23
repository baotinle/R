######################################## SERVER #########################################################################
server <- function(input, output) {
  output$p1<- renderApexchart({
    
    if (input$project == "Tất cả") {
      qa1 <- data_log %>%
        mutate(date = as.character(date)) %>%
        group_by(date) %>%
        summarise(quantity = n_distinct(user),
                  manday = round(quantity * 8.5,1),
                  time = round(sum(hours), 2)) %>%
        apex(type = "column", aes(date, quantity),serie_name = "Số người", height = 400) %>%
        add_line(aes(date, time), serie_name = "Số giờ làm việc thực tế") %>%
        add_line(aes(date,manday), serie_name = "Số giờ làm việc lý tưởng")%>%
        ax_colors(c("#000080","#FFB319","#00A300" )) %>%
        ax_stroke(curve = "smooth", width = c(0,3,3)) %>%
        ax_markers(size = 4) %>%
        ax_yaxis(
          decimalsInFloat = 0,
          title = list(text = "Số lượng người tham gia"),
          labels = list(
            minWidth = 40))%>%
        ax_yaxis2(
          opposite = TRUE,
          decimalsInFloat = 0,
          title = list(text = "Số giờ làm việc"),
          labels = list(
            formatter = htmlwidgets::JS("function(value) {return value + ' h';}"))) %>%
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
    qa1  
      
    } else {
      qa1 <- data_log %>%
        filter(project_name == input$project) %>%
        mutate(date = as.character(date)) %>%
        group_by(date) %>%
        summarise(quantity = n_distinct(user),
                  manday = quantity * 8.5,
                  time = round(sum(hours), 2)) %>%
        apex(type = "column", aes(date, quantity),serie_name = "Số người", height = 400) %>%
        add_line(aes(date, time), serie_name = "Số giờ làm việc thực tế") %>%
        add_line(aes(date,manday), serie_name = "Số giờ làm việc lý tưởng")%>%
        ax_colors(c("#000080","#FFB319","#00A300" )) %>%
        ax_stroke(curve = "smooth", width = c(0,3,3)) %>%
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
    qa131 <- qa2 %>%
      mutate(manday = quantity * 8.5)%>%
      apex(type = "column", aes(project_name, quantity), serie_name = "Số lượng người tham gia", height = 400) %>%
      add_line(aes(project_name, time), serie_name = "Số giờ làm việc") %>%
      #add_line(aes(project_name,manday), serie_name = "Số giờ làm việc lý tưởng")%>%
      ax_colors("#990000", "#0099ff","#150E56") %>%
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
    qa131
  })
  
  output$p3<- renderApexchart({
    
    if (input$status == "Tất cả") {
      qa1 <- dta_status %>%
        mutate(count = 1) %>%
        group_by(status) %>%
        summarise(quantity = sum(count)) %>%
        arrange(desc(quantity)) %>%
        apex(type = "pie", aes(status, quantity), serie_name = "Số lượng ticket", height = 400) %>%
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
        apex(type = "pie", aes(status, quantity), serie_name = "Số lượng ticket", height = 400) %>%
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
        summarise(time = round(sum(hours), 2))%>%
        arrange(desc(time)) %>%
        apex(type = "pie", aes(mana, time)) %>%
        ax_colors("#3399ff", "#ff3300") %>%
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
        ax_colors("#3399ff", "#ff3300") %>%
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
        ax_colors("#3399ff", "#ff3300") %>%
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
        ax_colors("#3399ff", "#ff3300") %>%
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
          name = "Tỷ lệ hoàn thành dự án", type = "line", data = fe2$ratio,
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
        ax_colors("#008000", "#ff1a1a") %>%
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
        ax_colors("#008000", "#ff1a1a") %>%
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
        ax_colors("#008000", "#ff1a1a") %>%
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
        ax_colors("#008000", "#ff1a1a") %>%
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
        ax_colors("#FF1700","#35589A", "#FFBD35","#D885A3","#F14A16","#9AD0EC","#270082","#D67D3E","#DD4A48","#DA1212","#91C483","#9D5353","#FBCAFF","#270082","#FFC600") %>%
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
        ax_colors("#FF1700","#35589A", "#FFBD35","#D885A3","#F14A16","#9AD0EC","#270082","#D67D3E","#DD4A48","#DA1212","#91C483","#9D5353","#FBCAFF","#270082","#FFC600") %>%
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
    if (input$bug2 == "Tất cả") {
    qa12<- creatissue133 %>%
      group_by(ind)%>%
      summarise(vl = sum(values))%>%
      apex(type = "pie", aes(ind,vl)) %>%
      ax_colors("#008000","#ff1a1a") %>%
      ax_labs(
        title = "Đơn vị: ticket") %>%
      ax_title(
        style = list(fontSize = "13px", color = "#808080")) 
    qa12
    }
    else{
      qa12<- creatissue133 %>%
        filter(project_name==input$bug2)%>%
        group_by(project_name,ind)%>%
        summarise(vl = sum(values))%>%
        apex(type = "pie", aes(ind,vl)) %>%
        ax_colors("#008000","#ff1a1a") %>%
        ax_labs(
          title = "Đơn vị: ticket") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) 
      qa12
    }
  })
  
  output$p15<- renderApexchart({
    if (input$project5 == "Tất cả") {
      qa13<- cr3 %>%
        group_by(mm1)%>%
        summarise(count1 = sum(counts))%>%
        apex(type = "pie", aes(mm1 ,count1)) %>%
        ax_colors("#113CFC","#FF2626", "#C7BEA2") %>%
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
        ax_colors("#193498") %>%
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
        ax_colors("#193498") %>%
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
    if (input$bug3 == "Tất cả") {
    qa15 <- eff15 %>%
      mutate(count =1) %>%
      group_by(pname.x) %>%
      summarise(counts = sum(count))%>%
      arrange(desc(counts)) %>%
      apex(type = "pie", aes(pname.x, counts), serie_name = "Số giờ", height = 400) %>%
      ax_colors("#2D46B9", "#ff1a1a","#F14A16","#FC9918") %>%
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
    }
    else{
      qa15 <- eff15 %>%
        filter(project_name == input$bug3)%>%
        mutate(count =1) %>%
        group_by(project_name,pname.x) %>%
        summarise(counts = sum(count)) %>%
        arrange(desc(counts)) %>%
        apex(type = "pie", aes(pname.x, counts), serie_name = "Số giờ", height = 400) %>%
        ax_colors("#2D46B9", "#ff1a1a","#F14A16","#FC9918") %>%
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
    }
  })
  
  output$p18<- renderApexchart({
    if (input$bug1 == "Tất cả") {
    qa16<- bugenv11 %>%
      mutate(count =1)%>%
      group_by(enviroment1)%>%
      summarise(counts = sum(count))%>%
      arrange(desc(counts))%>%
      apex(type = "pie", aes(enviroment1,counts)) %>%
      ax_colors("#113CFC","#FF2626", "#FDA65D","#219F94","#FF6464") %>%
      ax_labs(
        title = "Đơn vị: phần trăm") %>%
      ax_title(
        style = list(fontSize = "13px", color = "#808080")) 
    qa16
    }
    else{
      qa16<- bugenv11 %>%
        filter(project_name ==input$bug1)%>%
        mutate(count =1)%>%
        group_by(project_name,enviroment1)%>%
        summarise(counts = sum(count))%>%
        arrange(desc(counts))%>%
        apex(type = "pie", aes(enviroment1,counts)) %>%
        ax_colors("#113CFC","#FF2626", "#FDA65D","#219F94","#FF6464") %>%
        ax_labs(
          title = "Đơn vị: phần trăm") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) 
      qa16
    }
  })
  
  output$p19<- renderApexchart({
    if (input$eff13 == "Tất cả") {
      qa33<- creatissue23 %>%
        group_by(createdate)%>%
        summarise(counts1 = sum(minus),
                  counts2 = sum(abc),
                  date = as.character(createdate),
                  dup = duplicated(date)) %>%
        filter(dup == FALSE) %>%
        apex(type = "spline", aes(date,counts1), serie_name = "Ngày") %>%
        add_line(aes(date,counts2),serie_name = "Keep deadline")%>%
        ax_colors(c("#BB6464","#81B214")) %>%
        ax_labs(
          title = "Đơn vị: ngày") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) %>%
        ax_stroke(curve = "smooth", width = c(3,3)) 
      qa33
    }
    else{
      qa33<- creatissue23 %>%
        filter(project_name == input$eff13) %>%
          group_by(createdate)%>%
          summarise(counts1 = sum(minus),
                    counts2 = sum(abc),
                    date = as.character(createdate),
                    dup = duplicated(date))%>%
          filter(dup == FALSE) %>%
          apex(type = "spline", aes(date,counts1), serie_name = "Ngày") %>%
          add_line(aes(date,counts2),serie_name = "Keep deadline")%>%
          ax_colors(c("#BB6464","#81B214")) %>%
          ax_labs(
            title = "Đơn vị: ngày") %>%
          ax_title(
            style = list(fontSize = "13px", color = "#808080")) %>%
          ax_stroke(curve = "smooth", width = c(3,3)) 
        qa33
    }
  })
  
  #output$p20<- renderApexchart({
  #  qa18<- creatissue224 %>%
  #    apex(type = "pie", aes(fullbug1,donebug1)) %>%
  #    ax_colors("#DED9C4", "#D0CAB2","#BB6464") %>%
  #    ax_title(
  #style = list(fontSize = "13px", color = "#808080")) 
  #  qa18
  #})
  
  output$p20<- renderApexchart({
    if (input$bug4 == "Tất cả") {
      qa34<- creatissue1334 %>%
        group_by(ind)%>%
        summarise(vl = sum(values))%>%
        apex(type = "pie", aes(ind,vl)) %>%
        ax_colors("#008000","#ff1a1a") %>%
        ax_labs(
          title = "Đơn vị: ticket") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) 
      qa34
    }
    else{
      qa34<- creatissue1334 %>%
        filter(project_name==input$bug4)%>%
        group_by(project_name,ind)%>%
        summarise(vl = sum(values))%>%
        apex(type = "pie", aes(ind,vl)) %>%
        ax_colors("#008000","#ff1a1a") %>%
        ax_labs(
          title = "Đơn vị: ticket") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) 
      qa34
    }
  })
  
  output$p21<- renderApexchart({
      qa35<- qa16ss %>%
        apex(type = "column", aes(issue_type,mean)) %>%
        ax_colors("#008000","#ff1a1a") %>%
        ax_labs(
          title = "Thời gian trung bình xử lý ticket (giờ)") %>%
        ax_title(
          style = list(fontSize = "13px", color = "#808080")) 
      qa35
  })
  
 # output$progressBox <- renderSparkBox({
#    spark_box(
 #     data = qa5,
#      title = paste0(format(sum(qa5$time), big.mark = "")),
 #     subtitle = "Tổng số giờ logtime",
#      color = "#FFF", background = "#7267CB",
#      title_style = list(color = "#FFF", fontSize = "20px"),
#      subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
 #   )
#  })
  
 # output$progressBox2 <- renderSparkBox({
#    spark_box(
 #     data = qa5,
#      title = paste0(format(sum(qa5$man), big.mark = "")),
 #     subtitle = "Tổng số người tham gia",
  #    color = "#FFF", background = "#C37B89",
  #    title_style = list(color = "#FFF", fontSize = "20px"),
   #   subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
  #  )
#  })
  
 # output$progressBox3 <- renderSparkBox({
#    spark_box(
#      data = cr2,
 #     title = paste0(format(sum(cr2$count), big.mark = "")),
#      subtitle = "Tổng số ticket tạo ra",
 #     color = "#FFF", background = "#8E806A",
  #    title_style = list(color = "#FFF", fontSize = "20px"),
  #    subtitle_style = list(color = "#FFF", fontSize = "10px", fontWeight = "bold")
   # )
  #})
}