# Define server logic
server <- function(input, output, session) {
  
  source("global.R", local = TRUE)
  
  ##############################################################################
  
  # 显示加载动画
  plan(multicore)  # 让数据加载异步执行，避免阻塞 UI
  shinyjs::show("loading-screen")  # 显示加载界面
  
  future({
    return(TRUE)  # 任务完成
  }) %>% 
    promises::then(function(result) {
      shinyjs::runjs("$('#loading-screen').fadeOut(1000);")  # 1秒淡出加载界面
    })
  
  ##############################################################################
  
  observeEvent(input$user_timezone, {
    req(input$user_timezone)  # 确保 input$user_timezone 已经获取
    
    # 服务器 UTC 时间
    utc_time <- Sys.time()
    
    # 获取用户时区
    user_tz <- input$user_timezone
    
    # 转换 UTC 时间到用户本地时间
    user_time <- format(as.POSIXct(utc_time, tz = "UTC"), tz = user_tz, usetz = TRUE)
    
    time_info <- HTML(paste0(
      "📌 <b>服务器 UTC 时间:</b><br> ", format(utc_time, "%Y-%m-%d %H:%M:%S UTC"), "<br><br>",
      "🌎 <b>你的时区:</b><br> ", user_tz, "<br><br>",
      "⏰ <b>本地时间:</b><br> ", user_time
    ))
    
    showNotification(time_info, type = "message", duration = 10)
  })
  
  # Database
  con <- db_connection()
  
  # 初始化 requests_data 和 unique_items_data
  requests_data <- reactiveVal(NULL)
  unique_items_data <- reactiveVal(NULL)
  
  # 初始化 added_items
  added_items <- reactiveVal(create_empty_inventory())
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 存储 完整 maker_list 数据
  maker_list <- reactiveVal()
  
  # 存储目前数据库中存在的makers与item_names
  makers_items_map <- reactiveVal(NULL)
  
  # 触发unique_items_data刷新
  unique_items_data_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发inventory刷新
  inventory_refresh_trigger <- reactiveVal(FALSE)
  
  # 触发order刷新
  orders_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储 barcode PDF 文件路径
  barcode_pdf_file_path <- reactiveVal(NULL)
  
  # 用于存储运单 PDF 文件路径
  label_pdf_file_path <- reactiveVal(NULL)
  
  # 初始化货架和箱子内物品（售出分页）
  shelf_items <- reactiveVal(create_empty_shelf_box())
  box_items <- reactiveVal(create_empty_shelf_box())
  
  # 员工相关
  employees_data <- reactiveVal(NULL)
  work_rates <- reactiveVal(NULL)
  clock_records <- reactiveVal(NULL)
  employee_refresh_trigger <- reactiveVal(FALSE) # 添加触发器
  
  # 创建全局环境变量用于存储缓存数据
  cache_env <- new.env()
  
  ####################################################################################################################################
  
  # 应用启动时加载数据: item_type_data
  observe({
    tryCatch({
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
    }, error = function(e) {
      item_type_data(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load item type data.", type = "error")
    })
  })
  
  # 应用启动时加载数据: maker list
  observe({
    tryCatch({
      maker_list(dbGetQuery(con, "SELECT Name AS Maker, Pinyin FROM maker_list ORDER BY Pinyin ASC"))
    }, error = function(e) {
      maker_list(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load maker list data.", type = "error")
    })
  })
  
  # 更新orders表中已有运单pdf的情况
  update_label_status_column(con)
  
  ####################################################################################################################################
  
  # 库存表
  inventory <- reactive({
    inventory_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # 商品名自动联想
  item_names <- reactive({
    req(inventory())
    unique(inventory()$ItemName)  # 提取唯一的商品名
  })
  
  ####################################################################################################################################
  
  # 物品追踪表
  unique_items_data <- reactivePoll(
    intervalMillis = poll_interval, 
    session = session,       # 绑定 Shiny session，确保只在活跃时运行
    
    # **检查是否需要更新**（返回最近更新时间）
    checkFunc = function() {
      db_time <- dbGetQuery(con, "SELECT last_updated FROM update_log WHERE table_name = 'unique_items'")[[1]]
      trigger_val <- unique_items_data_refresh_trigger()
      paste(db_time, trigger_val)
    },
    
    # **获取最新数据**
    valueFunc = function() {
      result <- dbGetQuery(con, "
      SELECT 
        unique_items.UniqueID, 
        unique_items.SKU, 
        unique_items.OrderID,
        unique_items.ProductCost,
        unique_items.DomesticShippingCost,
        unique_items.Status,
        unique_items.Defect,
        unique_items.DefectNotes,
        unique_items.IntlShippingMethod,
        unique_items.IntlTracking,
        unique_items.IntlShippingCost,
        unique_items.PurchaseTime,
        unique_items.DomesticEntryTime,
        unique_items.DomesticExitTime,
        unique_items.DomesticSoldTime,
        unique_items.UsEntryTime,
        unique_items.UsShippingTime,
        unique_items.UsRelocationTime,
        unique_items.PurchaseCheck,
        unique_items.updated_at,
        inventory.Maker,
        inventory.MajorType,
        inventory.MinorType,
        inventory.ItemName,
        inventory.ItemImagePath
      FROM 
        unique_items
      JOIN 
        inventory 
      ON 
        unique_items.SKU = inventory.SKU
      ORDER BY 
        unique_items.updated_at DESC
    ")
      
      dbWithTransaction(con, {
        # **当 `unique_items` 变更时，自动更新 `inventory`**
        dbExecute(con, "
          UPDATE inventory i
          JOIN (
            SELECT 
              SKU,
              AVG(ProductCost) AS AvgProductCost,
              AVG(DomesticShippingCost + IntlShippingCost) AS AvgShippingCost,
              SUM(Status IN ('国内入库', '国内出库', '美国入库')) AS TotalQuantity,
              SUM(Status = '国内入库') AS DomesticQuantity,
              SUM(Status = '国内出库') AS TransitQuantity,
              SUM(Status = '美国入库') AS UsQuantity,
              MAX(updated_at) AS LatestUpdateTime
            FROM unique_items
            GROUP BY SKU
          ) u ON i.SKU = u.SKU
          SET 
            i.ProductCost = ROUND(u.AvgProductCost, 2),
            i.ShippingCost = ROUND(u.AvgShippingCost, 2),
            i.Quantity = u.TotalQuantity,
            i.DomesticQuantity = u.DomesticQuantity,
            i.TransitQuantity = u.TransitQuantity,
            i.UsQuantity = u.UsQuantity,
            i.updated_at = u.LatestUpdateTime
        ")
        
        # 删除不存在的 SKU
        dbExecute(con, "
          DELETE i FROM inventory i
          LEFT JOIN unique_items u ON i.SKU = u.SKU
          WHERE u.SKU IS NULL
        ")
      })
      return(result)
    }
  )
  
  # 加载当前已有的 makers 和 item names 的对应关系
  observe({
    unique_data <- unique_items_data()  # 数据源
    makers_items <- unique_data %>%
      select(Maker, ItemName) %>%  # 选择需要的列
      distinct()                   # 确保唯一性
    
    makers_items_map(makers_items)  # 更新 reactiveVal
  })
  
  ####################################################################################################################################
  
  # 订单表
  orders <- reactive({
    orders_refresh_trigger()
    dbGetQuery(con, "SELECT * FROM orders")
  })

  ####################################################################################################################################
  
  clear_invalid_item_status_history(con)
  
  ####################################################################################################################################
  
  
  
  ############################################
  ######   unique_items_data 表的过滤   ######
  ############################################
  
  # 采购页过滤
  filtered_unique_items_data_purchase <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 根据输入进行进一步过滤
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "purchase_filter-maker",
      status_input_id = "purchase_filter-status",
      item_name_input_id = "purchase_filter-name",
      other_input_id = "purchase_filter-other"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和采购日期组合的第一条记录
    data <- data %>%
      arrange(desc(Status == "采购"), desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  # 
  # # 入库页过滤
  # filtered_unique_items_data_inbound <- reactive({
  #   req(unique_items_data())
  #   data <- unique_items_data()
  #   
  #   # 只显示本页相关状态
  #   data <- data %>%
  #     filter(Status %in% c("采购", "国内入库"))
  #   
  #   data <- filter_unique_items_data_by_inputs(
  #     data = data,
  #     input = input,
  #     maker_input_id = "inbound_filter-maker",
  #     status_input_id = "inbound_filter-status",
  #     item_name_input_id = "inbound_filter-name",
  #     other_input_id = "inbound_filter-sku",
  #     purchase_date_range_id = "inbound_filter-purchase_date_range"
  #   )
  #   
  #   # 统计 SKU, Status, Defect, 和 PurchaseTime 下的数量
  #   data <- data %>%
  #     group_by(SKU, Status, Defect, PurchaseTime) %>%
  #     mutate(ItemCount = n()) %>%  # 条件统计数量
  #     ungroup()
  #   
  #   # 去重：仅保留每个 SKU 和组合的第一条记录
  #   data <- data %>%
  #     arrange(desc(updated_at)) %>%  # 按需求排序
  #     distinct(SKU, Status, Defect, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
  #   
  #   data
  # })
  # 
  # # 出库页过滤
  # filtered_unique_items_data_outbound <- reactive({
  #   req(unique_items_data())
  #   data <- unique_items_data()
  #   
  #   # 只显示本页相关状态
  #   data <- data %>%
  #     filter(Status %in% c("国内入库", "国内出库"), Defect != "瑕疵")
  #   
  #   data <- filter_unique_items_data_by_inputs(
  #     data = data,
  #     input = input,
  #     maker_input_id = "outbound_filter-maker",
  #     status_input_id = "outbound_filter-status",
  #     item_name_input_id = "outbound_filter-name",
  #     purchase_date_range_id = "outbound_filter-purchase_date_range"
  #   )
  #   
  #   # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
  #   data <- data %>%
  #     group_by(SKU, Status, PurchaseTime) %>%
  #     mutate(ItemCount = n()) %>%  # 条件统计数量
  #     ungroup()
  #   
  #   # 去重：仅保留每个 SKU 和组合的第一条记录
  #   data <- data %>%
  #     arrange(desc(updated_at)) %>%  # 按需求排序
  #     distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
  #   
  #   data
  # })
  # 
  # # 售出-物品售出分页过滤
  # filtered_unique_items_data_sold <- reactive({
  #   req(unique_items_data())
  #   data <- unique_items_data()
  #   
  #   # 只显示本页相关状态
  #   data <- data %>%
  #     filter(Status %in% c("国内入库", "国内出库", "美国入库", "美国调货", "国内售出"), Defect != "瑕疵")
  # 
  #   data <- filter_unique_items_data_by_inputs(
  #     data = data,
  #     input = input,
  #     maker_input_id = "sold_filter-maker",
  #     status_input_id = "sold_filter-status",
  #     item_name_input_id = "sold_filter-name",
  #     purchase_date_range_id = "sold_filter-purchase_date_range"
  #   )
  #   
  #   # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
  #   data <- data %>%
  #     group_by(SKU, Status, PurchaseTime) %>%
  #     mutate(ItemCount = n()) %>%  # 条件统计数量
  #     ungroup()
  #   
  #   
  #   # 去重：仅保留每个 SKU 和组合的第一条记录
  #   data <- data %>%
  #     arrange(desc(updated_at)) %>%  # 按需求排序
  #     distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
  #   
  #   data
  # })
  # 
  # # Define debounced input for the combined search field
  # debounced_filter_combined <- debounce(
  #   reactive({ trimws(input$filter_combined) }),  # Trim whitespace from input
  #   millis = 500  # Set debounce delay to 500 milliseconds
  # )
  # 
  # filtered_orders <- reactive({
  #   req(orders())  # Ensure order data exists
  #   
  #   data <- orders()  # Get all order data
  #   
  #   # Combined filter logic with debouncing
  #   search_term <- debounced_filter_combined()
  #   if (!is.null(search_term) && length(search_term) > 0 && nzchar(search_term)) {
  #     # Filter across multiple fields using OR logic
  #     data <- data %>% filter(
  #       grepl(search_term, OrderID, ignore.case = TRUE) |
  #         grepl(search_term, UsTrackingNumber, ignore.case = TRUE) |
  #         grepl(search_term, CustomerName, ignore.case = TRUE) |
  #         grepl(search_term, CustomerNetName, ignore.case = TRUE) |
  #         grepl(search_term, OrderNotes, ignore.case = TRUE)
  #     )
  #     
  #     # Handle SKU and ItemName filtering using unique_items_data
  #     req(unique_items_data())
  #     sku_or_item_orders <- unique_items_data() %>%
  #       filter(
  #         grepl(search_term, SKU, ignore.case = TRUE) |
  #           grepl(search_term, ItemName, ignore.case = TRUE)
  #       ) %>%
  #       pull(OrderID) %>%
  #       unique()
  #     
  #     # Combine orders matching SKU or ItemName
  #     data <- data %>% filter(OrderID %in% sku_or_item_orders | 
  #                               grepl(search_term, OrderID, ignore.case = TRUE) |
  #                               grepl(search_term, UsTrackingNumber, ignore.case = TRUE) |
  #                               grepl(search_term, CustomerName, ignore.case = TRUE) |
  #                               grepl(search_term, CustomerNetName, ignore.case = TRUE) |
  #                               grepl(search_term, OrderNotes, ignore.case = TRUE))
  #   }
  #   
  #   # Filter by platform
  #   if (!is.null(input$filter_platform) && input$filter_platform != "") {
  #     data <- data %>% filter(Platform == input$filter_platform)
  #   }
  #   
  #   # Filter by order status
  #   if (!is.null(input$filter_order_status) && input$filter_order_status != "") {
  #     data <- data %>% filter(OrderStatus == input$filter_order_status)
  #   }
  #   
  #   # Filter by creation date
  #   if (!is.null(input$filter_order_date) && !is.null(input$filter_order_date[[1]]) && !is.null(input$filter_order_date[[2]])) {
  #     start_date <- input$filter_order_date[[1]]
  #     end_date <- input$filter_order_date[[2]]
  #     data <- data %>% filter(created_at >= start_date & created_at <= end_date)
  #   }
  #   
  #   # Sort by creation date in descending order
  #   data <- data %>% arrange(desc(created_at))
  #   
  #   data
  # })
  
  ###
  
  # 物品管理页过滤
  filtered_unique_items_data_manage <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "manage_filter-maker",
      status_input_id = "manage_filter-status",
      item_name_input_id = "manage_filter-name",
      other_input_id = "manage_filter-other",
      purchase_date_range_id = "manage_filter-purchase_date_range"
    )
    
    data
  })
  
  # # 瑕疵品管理页过滤
  # filtered_unique_items_data_defect <- reactive({
  #   req(unique_items_data())
  #   data <- unique_items_data()
  #   
  #   data <- filter_unique_items_data_by_inputs(
  #     data = data,
  #     input = input,
  #     maker_input_id = "defect_filter-maker",
  #     item_name_input_id = "defect_filter-name",
  #     purchase_date_range_id = "defect_filter-purchase_date_range"
  #   )
  #  
  #   # 默认过滤条件：状态为“国内入库”且 Defect 不为“未知”
  #   data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "国内入库", ]
  # 
  #   # 处理开关互斥逻辑
  #   if (isTRUE(input$show_defects_only)) {
  #     # 如果仅显示瑕疵品
  #     data <- data[data$Defect == "瑕疵", ]
  #   } else if (isTRUE(input$show_perfects_only)) {
  #     # 如果仅显示无瑕品
  #     data <- data[data$Defect == "无瑕", ]
  #   }
  # 
  #   data
  # })
  # 
  # # 国际物流筛选
  # filtered_unique_items_data_logistics <- reactive({
  #   req(unique_items_data())
  #   data <- unique_items_data()
  #   
  #   # # 只显示本页相关状态
  #   # data <- data %>%
  #   #   filter(Status %in% c("国内出库", "国内售出"), Defect != "瑕疵")
  # 
  #   data <- filter_unique_items_data_by_inputs(
  #     data = data,
  #     input = input,
  #     maker_input_id = "logistic_filter-maker",
  #     status_input_id = "logistic_filter-status",
  #     item_name_input_id = "logistic_filter-name",
  #     sold_date_range_id = "logistic_filter-sold_date_range",
  #     only_show_sold_id = "logistic_filter-only_show_sold",
  #     exit_date_range_id = "logistic_filter-exit_date_range",
  #     only_show_exit_id = "logistic_filter-only_show_exit"
  #   )
  #   
  #   shipping_method <- input$intl_shipping_method
  #   
  #   # 判断并根据物流方式筛选
  #   if (!is.null(shipping_method)) {
  #     data <- data %>% filter(IntlShippingMethod == shipping_method)
  #   }
  #   
  #   # 优先显示没有国际运单号的物品
  #   data <- data %>% arrange(desc(is.na(IntlTracking)), IntlTracking)
  # 
  #   data
  # })
  
  # 查询页过滤-库存表
  filtered_inventory <- reactive({
    req(inventory(), unique_items_data()) # 确保数据存在
    
    data <- inventory()
    
    # 如果库存为空，返回空库存表
    if (nrow(data) == 0) {
      return(create_empty_inventory())
    }
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "query_filter-maker",
      item_name_input_id = "query_filter-name",
      other_input_id = "query_filter-other",
      source_type = "inventory"
    )
    
    # 根据售罄筛选
    if (!is.null(input$query_stock_status) && input$query_stock_status != "none") {
      if (input$query_stock_status == "us") {
        data <- data %>% filter(UsQuantity == 0 & DomesticQuantity > 0)  # 美国库存为 0
      } else if (input$query_stock_status == "domestic") {
        data <- data %>% filter(DomesticQuantity == 0 & UsQuantity > 0)  # 国内库存为 0
      } else if (input$query_stock_status == "all") {
        data <- data %>% filter(Quantity == 0)  # 全库存售罄
      }
    }
    
    data <- data[order(data$updated_at, decreasing = TRUE), ]
    return(data)
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
      other_input_id = "download_sku",
      purchase_date_range_id = "download_date_range"
    )
  })
  
  ###########################################################################################################################
  
  
  # 渲染物品追踪数据表
  unique_items_table_purchase_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_purchase",
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日",
                                                           ItemCount = "数量")
                                                         ), selection = "single", data = filtered_unique_items_data_purchase)
  
  # unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
  #                                                       column_mapping <- c(common_columns, list(
  #                                                         PurchaseTime = "采购日",
  #                                                         DomesticEntryTime = "入库日",
  #                                                         Defect = "瑕疵态",
  #                                                         ItemCount = "数量")
  #                                                       ), selection = "single", data = filtered_unique_items_data_inbound)
  # 
  # unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", 
  #                                                        column_mapping <- c(common_columns, list(
  #                                                          PurchaseTime = "采购日",
  #                                                          DomesticExitTime = "出库日",
  #                                                          ItemCount = "数量")
  #                                                        ), selection = "single", data = filtered_unique_items_data_outbound)
  # 
  # unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold",
  #                                                    column_mapping <- c(common_columns, list(
  #                                                      PurchaseTime = "采购日",
  #                                                      DomesticSoldTime = "售出日",
  #                                                      ItemCount = "数量")
  #                                                    ), selection = "single", data = filtered_unique_items_data_sold)
  # 
  ####################################################################################################################################
  
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- c(common_columns, list(
                                                         DomesticShippingCost = "国内运费",
                                                         PurchaseTime = "采购日",
                                                         DomesticEntryTime = "入库日",
                                                         DomesticExitTime = "出库日",
                                                         DomesticSoldTime = "售出日",
                                                         UsEntryTime = "美入库日",
                                                         UsRelocationTime = "美调货日",
                                                         UsShippingTime = "美发货日",
                                                         OrderID = "订单号")
                                                       ), selection = "multiple", data = filtered_unique_items_data_manage,
                                                       option = modifyList(table_default_options, list(scrollY = "730px")))
  
  # unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
  #                                                      column_mapping <- c(common_columns, list(
  #                                                        PurchaseTime = "采购日",
  #                                                        DomesticEntryTime = "入库日",
  #                                                        Defect = "瑕疵态",
  #                                                        DefectNotes = "瑕疵备注")
  #                                                      ), selection = "multiple", data = filtered_unique_items_data_defect,
  #                                                      option = modifyList(table_default_options, list(scrollY = "730px", searching = FALSE)))
  # 
  # unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
  #                                                         column_mapping = c(common_columns, list(
  #                                                           IntlShippingMethod = "国际运输",
  #                                                           DomesticSoldTime = "售出日",
  #                                                           DomesticExitTime = "出库日",
  #                                                           IntlShippingCost = "国际运费",
  #                                                           IntlTracking = "国际运单"
  #                                                         )), selection = "multiple",
  #                                                         data = filtered_unique_items_data_logistics,
  #                                                         option = modifyList(table_default_options, list(scrollY = "730px", 
  #                                                                                                         searching = FALSE, 
  #                                                                                                         paging = TRUE,
  #                                                                                                         pageLength = 30,
  #                                                                                                         lengthMenu = c(30, 100, 200, 500, 1000),
  #                                                                                                         dom = 'lftip')))

  output$filtered_inventory_table_query <- renderDT({  # input$filtered_inventory_table_query_rows_selected
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      DomesticQuantity = "国内库存数",
      TransitQuantity = "在途库存数",
      UsQuantity = "美国库存数",
      ProductCost = "平均成本",
      ShippingCost = "平均运费"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )$datatable
  })
  
  unique_items_table_download_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_download",
                                                         column_mapping <- c(common_columns, list(
                                                           Defect = "瑕疵态",
                                                           PurchaseTime = "采购日",
                                                           DomesticEntryTime = "入库日",
                                                           DomesticExitTime = "出库日",
                                                           DomesticSoldTime = "售出日")
                                                         ), data = filtered_unique_items_data_download)
  
  # # 订单管理分页订单表
  # selected_order_row <- callModule(orderTableServer, "orders_table_module",
  #                                  column_mapping = list(
  #                                    OrderID = "订单号",
  #                                    OrderImagePath = "订单图",
  #                                    CustomerName = "姓名",
  #                                    CustomerNetName = "网名",
  #                                    Platform = "平台",
  #                                    TransactionAmount = "成交额",
  #                                    UsTrackingNumber = "运单号",
  #                                    LabelStatus = "运单PDF",
  #                                    OrderStatus = "状态",
  #                                    OrderNotes = "备注",
  #                                    created_at = "创建时间"
  #                                  ),
  #                                  data = filtered_orders,  # 数据源
  #                                  selection = "single" # 单选模式
  # )
  
  ####################################################################################################################################
  
  observeEvent(input$refresh_item_table, {
    added_items(dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", params = list(system_type)))
    unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())  # 触发数据刷新
    orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
    employee_refresh_trigger(!employee_refresh_trigger()) # 触发员工相关数据刷新
    refreshTransactionTable("买货卡", cache_env, transaction_table_hash, output, con)
    refreshTransactionTable("工资卡", cache_env, transaction_table_hash, output, con)
    refreshTransactionTable("美元卡", cache_env, transaction_table_hash, output, con)
    refreshTransactionTable("一般户卡", cache_env, transaction_table_hash, output, con)
  })
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 打卡分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 在 server 函数顶部添加 reactiveVal
  employees_data <- reactiveVal(NULL)
  work_rates <- reactiveVal(NULL)
  clock_records <- reactiveVal(NULL)
  
  # 定义结束工作的逻辑
  finish_work <- function(ongoing_record, employee) {
    record_id <- ongoing_record$RecordID
    clock_out_time <- Sys.time()
    hours_worked <- as.numeric(difftime(clock_out_time, ongoing_record$ClockInTime, units = "hours"))
    hourly_rate <- work_rates() %>% 
      filter(EmployeeName == employee, WorkType == ongoing_record$WorkType) %>% 
      pull(HourlyRate)
    total_pay <- round(hours_worked * hourly_rate, 2)
    
    dbExecute(
      con,
      "UPDATE clock_records SET ClockOutTime = ?, TotalPay = ? WHERE RecordID = ?",
      params = list(clock_out_time, total_pay, record_id)
    )
    
    clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
    showNotification(paste("工作结束！时长:", round(hours_worked, 2), "小时，薪酬:", total_pay, "元"), type = "message")
  }
  
  # 初始化时加载数据
  observe({
    tryCatch({
      employees_data(dbGetQuery(con, "SELECT EmployeeName FROM employees"))
      work_rates(dbGetQuery(con, "SELECT EmployeeName, WorkType, HourlyRate FROM employee_work_rates"))
      clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
    }, error = function(e) {
      showNotification(paste("初始化数据失败:", e$message), type = "error")
      employees_data(data.frame(EmployeeName = character(0)))
    })
  })
  
  # 响应刷新触发器，更新数据库
  observe({
    req(employee_refresh_trigger())
    employees_data(dbGetQuery(con, "SELECT EmployeeName FROM employees"))
    work_rates(dbGetQuery(con, "SELECT EmployeeName, WorkType, HourlyRate FROM employee_work_rates"))
    clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
  })
  
  # 动态更新员工姓名下拉菜单
  observe({
    req(employees_data())
    updateSelectInput(
      session,
      "employee_name",
      choices = employees_data()$EmployeeName,
      selected = NULL
    )
  })
  
  observe({
    req(input$employee_name, work_rates())
    
    valid_work_types <- work_rates() %>%
      filter(EmployeeName == input$employee_name, HourlyRate > 0) %>%
      pull(WorkType)
    
    if (length(valid_work_types) == 0) {
      # 如果没有符合条件的工作类型，禁用下拉菜单和打卡按钮
      updateSelectInput(session, "work_type", choices = c(""), selected = "")
      shinyjs::disable("clock_in_out_btn")
    } else {
      # 否则更新下拉菜单
      updateSelectInput(session, "work_type", choices = c("", valid_work_types), selected = "")
      shinyjs::enable("clock_in_out_btn") # 视情况启用按钮
    }
  })
  
  # 统一管理状态
  observe({
    # 如果未选择员工，重置状态
    if (is.null(input$employee_name) || input$employee_name == "") {
      updateSelectInput(session, "work_type", selected = "")
      shinyjs::disable("clock_in_out_btn")
      shinyjs::removeClass("clock_in_out_btn", "btn-success btn-danger")
      updateActionButton(session, "clock_in_out_btn", label = "打卡", icon = icon("clock"))
      return()
    }
    
    # 检查未结束记录
    ongoing_record <- clock_records() %>%
      filter(EmployeeName == input$employee_name, !is.na(ClockInTime), is.na(ClockOutTime)) %>%
      slice(1)
    
    if (nrow(ongoing_record) > 0) {
      # 有未结束记录，恢复状态
      updateSelectInput(session, "work_type", selected = ongoing_record$WorkType)
      shinyjs::enable("clock_in_out_btn")
      shinyjs::removeClass("clock_in_out_btn", "btn-success")
      shinyjs::addClass("clock_in_out_btn", "btn-danger")
      updateActionButton(session, "clock_in_out_btn", label = "工作结束", icon = icon("stop"))
    } else if (!is.null(input$work_type) && input$work_type != "") {
      # 无未结束记录，且工作类型已选择，检查薪酬
      hourly_rate <- work_rates() %>% 
        filter(EmployeeName == input$employee_name, WorkType == input$work_type) %>% 
        pull(HourlyRate)
      
      if (length(hourly_rate) > 0) {
        shinyjs::enable("clock_in_out_btn")
        shinyjs::removeClass("clock_in_out_btn", "btn-danger")
        shinyjs::addClass("clock_in_out_btn", "btn-success")
        updateActionButton(session, "clock_in_out_btn", label = "工作开始", icon = icon("play"))
      } else {
        shinyjs::disable("clock_in_out_btn")
        shinyjs::removeClass("clock_in_out_btn", "btn-success btn-danger")
        updateActionButton(session, "clock_in_out_btn", label = "打卡", icon = icon("clock"))
      }
    } else {
      # 未选择工作类型，重置按钮
      shinyjs::disable("clock_in_out_btn")
      shinyjs::removeClass("clock_in_out_btn", "btn-success btn-danger")
      updateActionButton(session, "clock_in_out_btn", label = "打卡", icon = icon("clock"))
    }
  })
  
  # 打卡按钮逻辑
  observeEvent(input$clock_in_out_btn, {
    req(input$employee_name, input$work_type)
    
    employee <- input$employee_name
    work_type <- input$work_type
    
    tryCatch({
      dbWithTransaction(con, {
        # 检查当前状态
        ongoing_record <- clock_records() %>%
          filter(EmployeeName == employee, !is.na(ClockInTime), is.na(ClockOutTime)) %>%
          slice(1)
        
        if (nrow(ongoing_record) == 0) {
          # 上班打卡
          record_id <- uuid::UUIDgenerate()
          clock_in_time <- Sys.time()
          hourly_rate <- work_rates() %>% 
            filter(EmployeeName == employee, WorkType == work_type) %>% 
            pull(HourlyRate)
          
          if (length(hourly_rate) == 0 || hourly_rate == 0) {
            showNotification("该员工此工作类型的薪酬未设置，请联系管理员！", type = "error")
            return()
          }
          
          dbExecute(
            con,
            "INSERT INTO clock_records (RecordID, EmployeeName, WorkType, ClockInTime) VALUES (?, ?, ?, ?)",
            params = list(record_id, employee, work_type, clock_in_time)
          )
          
          clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
          showNotification("工作开始！", type = "message")
        } else {
          # 下班打卡
          if (work_type == "直播") {
            # 弹出销售额输入模态框
            showModal(modalDialog(
              title = "输入总销售额",
              numericInput("sales_amount", "请输入本次直播的总销售额 ($):", value = 0, min = 0, step = 0.01, width = "100%"),
              footer = tagList(
                modalButton("取消"),
                actionButton("confirm_sales_amount", "确认", class = "btn-success")
              )
            ))
          } else {
            # 非直播直接结束工作
            finish_work(ongoing_record, employee)
          }
        }
      })
    }, error = function(e) {
      showNotification(paste("打卡失败:", e$message), type = "error")
    })
  })
  
  # 打卡结束
  observeEvent(input$confirm_sales_amount, {
    req(input$sales_amount)
    
    # 获取销售额输入值
    sales_amount <- as.numeric(input$sales_amount)
    if (is.na(sales_amount) || sales_amount < 0) {
      showNotification("请输入有效的销售额！", type = "error")
      return()
    }
    
    # 更新数据库，结束工作
    ongoing_record <- clock_records() %>%
      filter(EmployeeName == input$employee_name, !is.na(ClockInTime), is.na(ClockOutTime)) %>%
      slice(1)
    
    if (nrow(ongoing_record) == 0) {
      showNotification("未找到正在进行的工作记录！", type = "error")
      removeModal()
      return()
    }
    
    record_id <- ongoing_record$RecordID
    clock_out_time <- Sys.time()
    hours_worked <- as.numeric(difftime(clock_out_time, ongoing_record$ClockInTime, units = "hours"))
    hourly_rate <- work_rates() %>% 
      filter(EmployeeName == input$employee_name, WorkType == ongoing_record$WorkType) %>% 
      pull(HourlyRate)
    total_pay <- round(hours_worked * hourly_rate, 2)
    
    tryCatch({
      dbExecute(
        con,
        "UPDATE clock_records SET ClockOutTime = ?, TotalPay = ?, SalesAmount = ? WHERE RecordID = ?",
        params = list(clock_out_time, total_pay, sales_amount, record_id)
      )
      
      clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
      showNotification(paste("工作结束！时长:", round(hours_worked, 2), "小时，薪酬:", total_pay, "元，总销售额:", sales_amount, "美元"), type = "message")
      removeModal()
    }, error = function(e) {
      showNotification(paste("更新失败:", e$message), type = "error")
      removeModal()
    })
  })
  
  # 实时计时器
  output$timer_display <- renderUI({
    # 不依赖 input$employee_name，直接检查所有未结束记录
    ongoing_record <- clock_records() %>%
      filter(!is.na(ClockInTime), is.na(ClockOutTime)) %>%
      slice(1)
    
    if (nrow(ongoing_record) == 0) {
      return(tags$p("未开始工作", style = "font-size: 24px; color: #666; text-align: center; margin-top: 20px;"))
    }
    
    # 如果有未结束记录，且匹配当前选择员工，则显示计时
    if (!is.null(input$employee_name) && ongoing_record$EmployeeName == input$employee_name) {
      invalidateLater(1000, session)
      elapsed_time <- as.numeric(difftime(Sys.time(), ongoing_record$ClockInTime, units = "secs"))
      
      hours <- floor(elapsed_time / 3600)
      minutes <- floor((elapsed_time %% 3600) / 60)
      seconds <- floor(elapsed_time %% 60)
      
      tags$h2(
        sprintf("%02d:%02d:%02d", hours, minutes, seconds),
        style = "font-size: 48px; color: #333; text-align: center; margin-top: 20px;"
      )
    } else {
      tags$p("未开始工作", style = "font-size: 24px; color: #666; text-align: center; margin-top: 20px;")
    }
  })
  
  # 销售额输入框
  observe({
    req(input$work_type)
    
    if (input$work_type == "直播") {
      shinyjs::show("manual_sales_amount") # 显示销售额输入框
    } else {
      shinyjs::hide("manual_sales_amount") # 隐藏销售额输入框
      updateNumericInput(session, "manual_sales_amount", value = 0) # 重置销售额为 0
    }
  })
  
  # 手动补录打卡逻辑
  observeEvent(input$submit_manual_clock, {
    req(input$employee_name, input$work_type)
    
    employee <- input$employee_name
    work_type <- input$work_type
    
    clock_in <- tryCatch({
      as.character(as.POSIXct(paste(input$manual_date_in, paste(input$manual_time_in, "00", sep = ":")), 
                              format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }, error = function(e) {
      showNotification("工作开始时间格式错误，请重新选择！", type = "error")
      return(NULL)
    })
    
    if (is.null(clock_in)) return()
    
    clock_out <- tryCatch({
      if (!is.null(input$manual_date_out) && !is.null(input$manual_time_out)) {
        as.character(as.POSIXct(paste(input$manual_date_out, paste(input$manual_time_out, "00", sep = ":")), 
                                format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
      } else {
        NA
      }
    }, error = function(e) {
      showNotification("工作结束时间格式错误，请重新选择！", type = "error")
      return(NULL)
    })
    
    if (!is.na(clock_out) && as.POSIXct(clock_out) <= as.POSIXct(clock_in)) {
      showNotification("工作结束时间必须晚于开始时间！", type = "error")
      return()
    }
    
    # 获取销售额（仅直播工作类型有效）
    sales_amount <- ifelse(work_type == "直播", as.numeric(input$manual_sales_amount), NA)
    
    tryCatch({
      dbWithTransaction(con, {
        record_id <- uuid::UUIDgenerate()
        hourly_rate <- work_rates() %>% 
          filter(EmployeeName == employee, WorkType == work_type) %>% 
          pull(HourlyRate)
        
        if (length(hourly_rate) == 0 || hourly_rate == 0) {
          showNotification("该员工此工作类型的薪酬未设置，请联系管理员！", type = "error")
          return()
        }
        
        total_pay <- if (!is.na(clock_out)) {
          hours_worked <- as.numeric(difftime(as.POSIXct(clock_out), as.POSIXct(clock_in), units = "hours"))
          round(hours_worked * hourly_rate, 2)
        } else {
          NA
        }
        
        dbExecute(
          con,
          if (!is.na(clock_out)) {
            "INSERT INTO clock_records (RecordID, EmployeeName, WorkType, ClockInTime, ClockOutTime, TotalPay, SalesAmount) VALUES (?, ?, ?, ?, ?, ?, ?)"
          } else {
            "INSERT INTO clock_records (RecordID, EmployeeName, WorkType, ClockInTime, SalesAmount) VALUES (?, ?, ?, ?, ?)"
          },
          params = if (!is.na(clock_out)) {
            list(record_id, employee, work_type, clock_in, clock_out, total_pay, sales_amount)
          } else {
            list(record_id, employee, work_type, clock_in, sales_amount)
          }
        )
        
        clock_records(dbGetQuery(con, "SELECT * FROM clock_records ORDER BY CreatedAt DESC"))
        showNotification("手动补录打卡成功！", type = "message")
        
        # 重置表单
        updateDateInput(session, "manual_date_in", value = NULL)
        updateTimeInput(session, "manual_time_in", value = strptime("09:00", "%H:%M"))
        updateDateInput(session, "manual_date_out", value = NULL)
        updateTimeInput(session, "manual_time_out", value = strptime("18:00", "%H:%M"))
        updateNumericInput(session, "manual_sales_amount", value = 0)
      })
    }, error = function(e) {
      showNotification(paste("手动补录失败:", e$message), type = "error")
    })
  })
  
  # 渲染当天工作记录表格（仅显示当前选择的员工）
  output$today_work_records_table <- renderDT({
    req(clock_records(), input$employee_name)
    
    # 获取当天日期
    today <- as.Date(Sys.Date())
    
    # 过滤当天记录，仅显示当前选择的员工
    today_records <- clock_records() %>%
      filter(as.Date(ClockInTime) == today, EmployeeName == input$employee_name) %>%
      left_join(work_rates(), by = c("EmployeeName", "WorkType")) %>%
      mutate(
        ClockInTime = as.character(format(as.POSIXct(ClockInTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%Y-%m-%d %H:%M:%S")),
        ClockOutTime = ifelse(is.na(ClockOutTime), "未结束", 
                              as.character(format(as.POSIXct(ClockOutTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%Y-%m-%d %H:%M:%S"))),
        HoursWorked = ifelse(is.na(ClockOutTime) | is.na(ClockInTime), 0,
                             round(as.numeric(difftime(as.POSIXct(ClockOutTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                                       as.POSIXct(ClockInTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                                       units = "hours")), 2)),
        HourlyRate = round(ifelse(is.na(HourlyRate), 0, HourlyRate), 2),
        TotalPay = sprintf("¥%.2f", round(ifelse(is.na(TotalPay), 0, TotalPay), 2)),
        SalesAmount = sprintf("$%.2f", ifelse(is.na(SalesAmount), 0, SalesAmount)) # 格式化销售额为货币形式
      ) %>%
      select(
        "员工姓名" = EmployeeName,
        "工作类型" = WorkType,
        "上班时间" = ClockInTime,
        "下班时间" = ClockOutTime,
        "工作时长 (小时)" = HoursWorked,
        "时薪 (¥)" = HourlyRate,
        "总薪酬 (¥)" = TotalPay,
        "销售额 ($)" = SalesAmount # 新增销售额列
      )
    
    # 如果没有记录，显示提示信息
    if (nrow(today_records) == 0) {
      return(datatable(
        data.frame(Message = "今天暂无工作记录"),
        options = list(pageLength = 10, scrollX = TRUE, searching = FALSE),
        rownames = FALSE
      ))
    }
    
    # 返回数据表
    datatable(
      today_records,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE),
      rownames = FALSE
    )
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 采购分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 加载购物车初始数据
  observe({
    added_items(dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", params = list(system_type)))
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "purchase_filter",
    makers_items_map = makers_items_map
  )
  
  # 供应商模块
  supplierModuleServer(input, output, session, con, maker_list)
  
  # 物品大小类模块
  typeModuleServer("type_module", con, item_type_data)
  
  
  ### SKU冲撞检查
  
  # 合并依赖变量
  combined_inputs <- reactive({
    list(
      major_type = input[["type_module-new_major_type"]],
      new_name = input[["purchase-item_name"]],
      new_maker = input$new_maker
    )
  })
  
  # 使用 debounce 延迟触发，避免短时间多次调用
  debounced_inputs <- debounce(combined_inputs, millis = 300)
  
  observeEvent(debounced_inputs(), {
    inputs <- debounced_inputs()
    
    # 检查 SKU 的来源
    is_from_table <- !is.null(unique_items_table_purchase_selected_row()) && 
      length(unique_items_table_purchase_selected_row()) > 0
    
    # 判断是否需要清空 SKU
    if (is.null(inputs$new_maker) || inputs$new_maker == "" || 
        is.null(inputs$new_name) || inputs$new_name == "") {
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      return()
    }
    
    # Dynamically generate SKU
    sku <- generate_sku(
      item_type_data = item_type_data(),
      major_type = inputs$major_type,
      item_name = input[["purchase-item_name"]],
      maker = inputs$new_maker
    )
    
    if (is_from_table) {
      # 如果 SKU 来源于表格，直接更新输入字段
      updateTextInput(session, "new_sku", value = sku)
      # showNotification("SKU 已生成（来源于表格选择）", type = "message")
    } else {
      # 如果 SKU 不是来源于表格，检查是否冲突
      existing_sku <- inventory() %>% filter(SKU == sku)
      
      if (nrow(existing_sku) > 0) {
        # 如果 SKU 冲突，弹出模态窗口提醒用户
        showModal(modalDialog(
          title = "SKU 冲突",
          paste0("生成的 SKU '", sku, "' 已存在于库存中，请重新生成 SKU！"),
          easyClose = TRUE,
          footer = modalButton("关闭")
        ))
        
        # 清空 SKU 输入字段
        updateTextInput(session, "new_sku", value = "")
      } else {
        # 如果 SKU 不冲突，更新输入字段
        updateTextInput(session, "new_sku", value = sku)
        # showNotification("SKU 生成成功！", type = "message")
      }
    }
  })
  
  autocompleteInputServer("purchase", get_suggestions = item_names)  # 返回商品名列表
  
  output$preorder_items_memo <- renderUI({
    # 从 orders() 中筛选出 OrderStatus 为“预定”的订单
    preorder_orders <- orders() %>% filter(OrderStatus == "预定")
    
    # 初始化空的数据框，用于存储所有物品和供应商信息
    all_items <- data.frame(Item = character(0), Supplier = character(0), stringsAsFactors = FALSE)
    
    # 遍历每个订单的 OrderNotes，提取物品和供应商信息
    for (order_note in preorder_orders$OrderNotes) {
      extracted <- extract_items_and_suppliers(order_note)
      all_items <- rbind(all_items, extracted)
    }
    
    all_items <- unique(all_items[all_items$Item != "", ]) %>% arrange(Supplier, Item)
    
    # 创建完整的显示字符串
    all_items$DisplayText <- paste0(all_items$Item, "（", all_items$Supplier, "）")
    
    # 根据搜索框输入进行动态筛选
    if (!is.null(input$preorder_item_search_filter) && input$preorder_item_search_filter != "") {
      search_term <- tolower(input$preorder_item_search_filter)
      all_items <- all_items %>% 
        filter(grepl(search_term, tolower(DisplayText)))
    }
    
    # 获取当前库存商品名称
    existing_items <- unique(inventory()$ItemName)
    
    if (nrow(all_items) == 0) {
      div("没有匹配的预订单物品")
    } else {
      # 创建物品列表，判断是否存在于库存
      item_list <- lapply(seq_len(nrow(all_items)), function(i) {
        item <- all_items$Item[i]
        supplier <- all_items$Supplier[i]
        
        # 判断该物品是否在库存中
        is_existing <- item %in% existing_items
        status_label <- if (is_existing) {
          tags$span("现", class = "status-badge status-existing")
        } else {
          tags$span("新", class = "status-badge status-new")
        }
        
        # 根据物品类型设置不同的 onclick 逻辑
        if (is_existing) {
          onclick_script <- sprintf(
            "Shiny.setInputValue('selected_existing_item', '%s', {priority: 'event'}); Shiny.setInputValue('selected_existing_supplier', '%s', {priority: 'event'});",
            item, supplier
          )
        } else {
          onclick_script <- sprintf(
            "Shiny.setInputValue('selected_new_item', '%s', {priority: 'event'}); Shiny.setInputValue('selected_new_supplier', '%s', {priority: 'event'});",
            item, supplier
          )
        }
        
        # 创建可点击的物品项
        actionLink(
          inputId = paste0("preorder_item_", i), 
          label = div(
            style = "padding: 5px 0; border-bottom: 1px solid #eee; display: flex; align-items: center; cursor: pointer;",
            tags$span(all_items$DisplayText[i], style = "flex-grow: 1;"),
            status_label
          ),
          onclick = onclick_script
        )
      })
      
      # 返回 UI 组件
      do.call(tagList, item_list)
    }
  })
  
  # 监听“新”物品的点击事件，填充到 `new_maker` 和 `purchase-item_name`
  observeEvent(input$selected_new_item, {
    req(input$selected_new_item)
    
    updateTextInput(session, "purchase-item_name", value = input$selected_new_item)
    
    delay(100, {
      req(input$selected_new_supplier)  # 确保 `selected_new_supplier` 存在
      updateSelectizeInput(session, "new_maker", selected = input$selected_new_supplier)
      updateSelectizeInput(session, "type_module-new_major_type", selected = "")
    })
  })
  
  # 监听“现”物品的点击事件，填充到 `purchase_filter-name`
  observeEvent(input$selected_existing_item, {
    req(input$selected_existing_item)    
    ns <- NS("purchase_filter")
    updateSelectizeInput(session, ns("maker"), selected = input$selected_existing_supplier)
    shinyjs::delay(200, {
      updateSelectizeInput(session, ns("name"), selected = input$selected_existing_item)
    })
  })
  
  # 采购商品图片处理模块
  image_purchase <- imageModuleServer("image_purchase")
  
  output$added_items_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图",
      Maker = "供应商",
      MajorType = "大类",
      Quantity = "入库数量",
      ProductCost = "采购单价"
    )
    
    render_table_with_images(
      data = added_items(),
      column_mapping = column_mapping,
      selection = "multiple",
      image_column = "ItemImagePath",
      options = list(
        fixedHeader = TRUE,
        dom = 't',
        paging = FALSE,
        searching = FALSE
      )
    )$datatable
  }, server = FALSE)
  
  # 添加/更新物品
  observeEvent(input$add_btn, {
    # 输入验证（保持不变）
    if (is.null(input[["purchase-item_name"]]) || input[["purchase-item_name"]] == "") {
      showNotification("请填写正确商品名称！", type = "error")
      return()
    }
    if (is.null(input$new_quantity) || input$new_quantity <= 0) {
      showNotification("请填写正确商品数量！", type = "error")
      return()
    }
    if (is.null(input$new_product_cost) || input$new_product_cost < 0 || input$new_product_cost > 999) {
      showNotification("请填写正确商品单价！", type = "error")
      return()
    }
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常显示！", type = "error")
      return()
    }
    
    # 处理图片上传
    inventory_item <- dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", 
                                 params = list(input$new_sku))
    existing_inventory_path <- if (nrow(inventory_item) > 0) inventory_item$ItemImagePath[1] else NULL
    new_image_path <- process_image_upload(
      sku = input$new_sku,
      file_data = image_purchase$uploaded_file(),
      pasted_data = image_purchase$pasted_file(),
      inventory_path = existing_inventory_path
    )
    
    # 检查是否已存在记录并获取当前图片路径
    existing <- dbGetQuery(con, "SELECT ItemImagePath FROM shopping_cart WHERE SKU = ? AND SystemType = ?", 
                           params = list(input$new_sku, system_type))
    
    # 确定最终使用的图片路径
    final_image_path <- if (nrow(existing) > 0) {
      current_image_path <- existing$ItemImagePath[1]
      if (!is.na(new_image_path) && new_image_path != "") new_image_path else current_image_path
    } else {
      new_image_path
    }
    
    # 创建新记录
    new_item <- data.frame(
      SKU = input$new_sku,
      Maker = input$new_maker,
      MajorType = input[["type_module-new_major_type"]],
      MinorType = "",
      ItemName = input[["purchase-item_name"]],
      Quantity = input$new_quantity,
      ProductCost = round(input$new_product_cost, 2),
      ItemImagePath = final_image_path,
      SystemType = system_type,  # 添加 SystemType 字段
      stringsAsFactors = FALSE
    )
    
    if (nrow(existing) > 0) {
      # 更新记录
      dbExecute(con, "
        UPDATE shopping_cart 
        SET Maker = ?, MajorType = ?, MinorType = ?, ItemName = ?, 
            Quantity = ?, ProductCost = ?, ItemImagePath = ?, SystemType = ?
        WHERE SKU = ?",
                params = list(new_item$Maker, new_item$MajorType, new_item$MinorType,
                              new_item$ItemName, new_item$Quantity, new_item$ProductCost,
                              new_item$ItemImagePath, new_item$SystemType, new_item$SKU))
      showNotification(paste("SKU 已更新:", input$new_sku), type = "message")
    } else {
      # 插入新记录
      dbWriteTable(con, "shopping_cart", new_item, append = TRUE, row.names = FALSE)
      showNotification(paste("SKU 已添加:", input$new_sku), type = "message")
    }
    
    # 更新 added_items
    added_items(dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", params = list(system_type)))    
    
    # 重置表单
    image_purchase$reset()
  })
  
  # 动态更新按钮文本和图标
  output$add_update_button_ui <- renderUI({
    # 检查SKU是否存在于added_items()
    sku_input <- input$new_sku
    if (is.null(sku_input) || sku_input == "") {
      label <- "添加" # 默认显示“添加”
      icon_type <- "plus" # 默认图标为“添加”图标
    } else {
      sku_exists <- sku_input %in% added_items()$SKU
      if (sku_exists) {
        label <- "更新" # SKU已存在时显示“更新”
        icon_type <- "edit" # 图标显示为“编辑”
      } else {
        label <- "添加" # SKU不存在时显示“添加”
        icon_type <- "plus" # 图标显示为“添加”
      }
    }
    
    # 创建动态按钮
    actionButton("add_btn", label, width = "100%", 
                 icon = icon(icon_type), 
                 style = "background-color: #006400; color: white;")
  })
  
  observeEvent(input$confirm_btn, {
    tryCatch({
      if (nrow(added_items()) == 0) {
        showNotification("请先录入至少一个商品!", type = "error")
        return()
      }
      
      added_items_df <- added_items()
      dbBegin(con)
      
      # 批量插入库存记录
      inventory_success <- add_new_inventory_records_batch(con, added_items_df)
      if (isFALSE(inventory_success)) {
        dbRollback(con)
        showNotification("库存登记失败，请检查输入数据！", type = "error")
        return()
      }
      
      # 更新 UI
      inventory_refresh_trigger(!inventory_refresh_trigger())
      
      # 准备 unique_items 数据
      purchase_date <- format(as.Date(input$purchase_date), "%Y-%m-%d")
      total_shipping_cost <- input$new_shipping_cost
      if (is.null(total_shipping_cost) || total_shipping_cost <= 0) total_shipping_cost <- 0
      total_quantity <- sum(added_items_df$Quantity)
      unit_shipping_cost <- if (total_quantity > 0) total_shipping_cost / total_quantity else 0
      
      batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
        sku <- added_items_df$SKU[i]
        quantity <- added_items_df$Quantity[i]
        product_cost <- added_items_df$ProductCost[i]
        t(replicate(quantity, c(
          uuid::UUIDgenerate(),
          as.character(sku),
          as.numeric(product_cost),
          as.numeric(unit_shipping_cost),
          "采购",
          "未知",
          purchase_date
        )))
      }))
      
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
      colnames(batch_data) <- c("UniqueID", "SKU", "ProductCost", "DomesticShippingCost", 
                                "Status", "Defect", "PurchaseTime")
      
      dbWriteTable(con, "unique_items", batch_data, append = TRUE, row.names = FALSE)
      
      # 清空购物车
      dbExecute(con, "DELETE FROM shopping_cart WHERE SystemType = ?", params = list(system_type))      
      dbCommit(con)
      
      # 更新 UI
      added_items(dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", params = list(system_type)))    
      showNotification("所有采购货物已成功登记！", type = "message")
      
      # 重置输入
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_product_cost", value = 0)
      updateNumericInput(session, "new_shipping_cost", value = 0)
      updateTextInput(session, "purchase-item_name", value = "")
      image_purchase$reset()
      
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("采购登记失败:", e$message), type = "error")
    })
  })
  
  # 监听采购页选中items_table
  observeEvent(unique_items_table_purchase_selected_row(), {
    if (!is.null(unique_items_table_purchase_selected_row()) && length(unique_items_table_purchase_selected_row()) > 0) {
      selected_data <- filtered_unique_items_data_purchase()[unique_items_table_purchase_selected_row(), ]
      
      # showNotification(paste("Selected MajorType:", selected_data$MajorType))
      # showNotification(paste("Selected MinorType:", selected_data$MinorType))
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      updateTextInput(session, "purchase-item_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost) 
      updateNumericInput(session, "new_shipping_cost", value = 0)
    }
  })
  
  # 预定单物品搜索框清除
  observeEvent(input$clear_preorder_search_box, {
    updateTextInput(session, "preorder_item_search_filter", value = "")
  })
  
  # 监听采购页选中added_items_table 用来更改添加数据
  observeEvent(input$added_items_table_rows_selected, {
    selected_row <- input$added_items_table_rows_selected
    
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_data <- added_items()[last_selected, ] # 提取最后一个选择的数据
      
      # 更新侧边栏的输入字段
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "type_module-new_major_type", selected = selected_data$MajorType)
      updateTextInput(session, "purchase-item_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  # 显示总采购开销（含运费）
  output$total_cost <- renderText({
    added_items_df <- added_items()
    total_quantity <- sum(added_items_df$Quantity)
    shipping_cost <- if (is.null(input$new_shipping_cost) || input$new_shipping_cost < 0) 0 else input$new_shipping_cost
    total_cost <- sum(added_items_df$Quantity * added_items_df$ProductCost) + shipping_cost
    
    paste0(
      "本次采购总金额: ¥", format(total_cost, big.mark = ",", scientific = FALSE),
      "（包含运费: ¥", format(shipping_cost, big.mark = ",", scientific = FALSE),
      "，物品数: ", total_quantity, "件）"
    )
  })
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$delete_btn, {
    selected_row <- input$added_items_table_rows_selected
    
    # 如果没有选中行，提示用户
    if (length(selected_row) == 0) {
      showNotification("请选择要删除的记录", type = "error")
      return()
    }
    
    # 显示确认框
    showModal(
      modalDialog(
        title = HTML("<strong style='color: red;'>确认删除</strong>"),
        HTML(paste0(
          "<p>您确定要删除选中的 <strong>", length(selected_row), "</strong> 条记录吗？</p>",
          "<p><strong>注意：</strong> 此操作无法撤销！</p>"
        )),
        footer = tagList(
          modalButton("取消"),  # 关闭弹窗按钮
          actionButton("confirm_delete_selected", "确认删除", class = "btn-danger")
        ),
        easyClose = FALSE
      )
    )
  })
  
  # 确认删除采购箱物品
  observeEvent(input$confirm_delete_selected, {
    removeModal()
    
    selected_row <- input$added_items_table_rows_selected
    
    if (length(selected_row) == 0) {
      showNotification("未选中任何记录，无法删除", type = "error")
      return()
    }
    
    tryCatch({
      # 获取当前数据
      current_items <- dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", 
                                  params = list(system_type))
      
      if (nrow(current_items) == 0) {
        showNotification("采购箱为空，无记录可删除", type = "warning")
        added_items(current_items)
        return()
      }
      
      # 提取选中行的 SKU
      selected_skus <- current_items$SKU[selected_row]
      
      if (length(selected_skus) == 0) {
        showNotification("所选记录不存在，请刷新页面后重试", type = "error")
        return()
      }
      
      # 动态构造删除查询
      placeholders <- paste(rep("?", length(selected_skus)), collapse = ",")
      query <- sprintf("DELETE FROM shopping_cart WHERE SKU IN (%s) AND SystemType = ?", placeholders)
      
      # 执行删除并检查影响行数
      affected_rows <- dbExecute(con, query, params = c(selected_skus, system_type))
      
      if (affected_rows == 0) {
        showNotification("删除失败，记录可能已被其他用户修改", type = "error")
        return()
      }
      
      # 更新 added_items 并延迟刷新，确保 UI 同步
      updated_items <- dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", 
                                  params = list(system_type))
      added_items(updated_items)
      
      showNotification(sprintf("成功删除 %d 条记录", affected_rows), type = "message")
      
    }, error = function(e) {
      showNotification(paste("删除失败:", e$message), type = "error")
      
      # 出错时强制刷新 added_items
      updated_items <- dbGetQuery(con, "SELECT * FROM shopping_cart WHERE SystemType = ?", 
                                  params = list(system_type))
      added_items(updated_items)
    })
  })
  
  # 清空输入
  observeEvent(input$reset_btn, {
    tryCatch({
      # 清空输入控件
      update_maker_choices(session, "new_maker", maker_list())
      updateSelectizeInput(session, "type_module-new_major_type", selected = "")
      updateTextInput(session, "purchase-item_name", value = "")
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复单价默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      
      # 重置图片控件
      image_purchase$reset()
      
      # 通知用户
      showNotification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification("清空输入时发生错误，请重试！", type = "error")
    })
  })
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 物品管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "manage_filter",
    makers_items_map = makers_items_map
  )

  # 采购商品图片处理模块
  image_manage <- imageModuleServer("image_manage")
  
  # 处理更新图片
  observeEvent(input$update_image_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    if (length(selected_rows) != 1) {
      showNotification("请确保只选中一行！", type = "error")
      return()
    }
    
    # 从选中的行获取 SKU
    selected_item <- filtered_unique_items_data_manage()[selected_rows, ]
    selected_sku <- selected_item$SKU
    
    if (is.null(selected_sku) || selected_sku == "") {
      showNotification("无法获取所选行的 SKU，请检查！", type = "error")
      return()
    }
    
    # 检查 SKU 是否存在于库存表
    existing_inventory_items <- inventory()
    if (!selected_sku %in% existing_inventory_items$SKU) {
      showNotification("库存中无此 SKU 商品，无法更新图片！", type = "error")
      return()
    }
    
    # 获取当前 SKU 的图片路径
    existing_item <- existing_inventory_items[existing_inventory_items$SKU == selected_sku, ]
    existing_image_path <- existing_item$ItemImagePath[1]
    
    # 处理图片上传或粘贴
    updated_image_path <- process_image_upload(
      sku = selected_sku,
      file_data = image_manage$uploaded_file(),
      pasted_data = image_manage$pasted_file(),
      inventory_path = existing_image_path
    )
    
    # 检查处理结果并更新数据库
    if (!is.null(updated_image_path) && !is.na(updated_image_path)) {
      tryCatch({
        # 更新数据库中 SKU 对应的图片路径
        dbExecute(con, "UPDATE inventory 
                    SET ItemImagePath = ? 
                    WHERE SKU = ?",
                  params = list(updated_image_path, selected_sku))
        
        # 更新inventory数据需要手动触发刷新
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
        
        # 显示成功通知
        showNotification(paste0("SKU ", selected_sku, " 的图片已成功更新！"), type = "message")
      }, error = function(e) {
        # 数据库操作失败时提示错误
        showNotification("图片路径更新失败，请重试！", type = "error")
      })
    } else {
      # 未检测到有效图片数据
      showNotification("未检测到有效的图片数据，请上传或粘贴图片！", type = "error")
    }
    
    # 重置图片上传状态
    image_manage$reset()
  })
  
  # 处理更新物品信息
  observeEvent(input$update_info_btn, {
    # 获取所有选中行索引
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 验证是否有选中行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请至少选中一行进行更新！", type = "error")
      return()
    }
    
    # 获取过滤后的数据
    selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
    
    # 验证用户输入的新数据
    new_product_cost <- input$update_product_cost
    new_shipping_cost <- input$update_shipping_cost
    new_purchase_date <- input$update_purchase_date
    
    if (is.null(new_product_cost) || new_product_cost < 0) {
      showNotification("请输入有效的单价！", type = "error")
      return()
    }
    if (is.null(new_shipping_cost) || new_shipping_cost < 0) {
      showNotification("请输入有效的国内运费！", type = "error")
      return()
    }
    if (is.null(new_purchase_date) || !lubridate::is.Date(as.Date(new_purchase_date))) {
      showNotification("请输入有效的采购日期！", type = "error")
      return()
    }
    
    # 遍历选中行并更新数据库
    tryCatch({
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        
        # 更新数据库
        dbExecute(
          con,
          "UPDATE unique_items 
                 SET ProductCost = ?, DomesticShippingCost = ?, PurchaseTime = ? 
                 WHERE UniqueID = ?",
          params = list(new_product_cost, new_shipping_cost, as.Date(new_purchase_date), unique_id)
        )
      })

      # 显示成功通知
      showNotification(paste0("成功更新了 ", nrow(selected_items), " 项物品的信息！"), type = "message")
    }, error = function(e) {
      showNotification(paste("更新失败：", e$message), type = "error")
    })
  })
  
  # 点击填写物品信息
  observeEvent(unique_items_table_manage_selected_row(), {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 检查是否有选中行
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      # 获取最新点击的行索引
      latest_row <- tail(selected_rows, n = 1)
      
      # 获取过滤后的数据
      data <- filtered_unique_items_data_manage()
      
      # 确保数据框不为空且行索引有效
      if (!is.null(data) && nrow(data) >= latest_row) {
        selected_data <- data[latest_row, ]  # 提取最新点击的行数据
        
        # 更新输入框
        updateNumericInput(session, "update_product_cost", value = selected_data$ProductCost)
        updateNumericInput(session, "update_shipping_cost", value = selected_data$DomesticShippingCost)
        updateDateInput(session, "update_purchase_date", value = as.Date(selected_data$PurchaseTime))
                        
      } else {
        showNotification("选中的行无效或数据为空！", type = "error")
      }
    } else {
      showNotification("未选中任何行！", type = "warning")
    }
  })

  # 清空
  observeEvent(input$clear_info_btn, {
    # 清空单价和运费输入框
    updateNumericInput(session, "update_product_cost", value = "")
    updateNumericInput(session, "update_shipping_cost", value = "")
    updateDateInput(session, "update_purchase_date", value = Sys.Date())
    
    showNotification("商品信息已清空！", type = "message")
  })
  
  ###
  
  # 监听删除按钮点击事件，弹出确认框
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "error")
      return()
    }
    
    # 显示确认框
    showModal(deleteConfirmationModal(length(selected_rows)))
  })
  
  # 确认框内 "确认删除" 按钮逻辑
  observeEvent(input$confirm_delete_final, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("没有选中任何物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- filtered_unique_items_data_manage()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        unique_id <- selected_items$UniqueID[i]
        sku <- selected_items$SKU[i]
        status <- selected_items$Status[i]  # 获取物品状态
        
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
              DELETE FROM unique_items
              WHERE UniqueID = ?", params = list(unique_id))
        
        # 删除 item_status_history 中对应的历史状态记录
        dbExecute(con, "
              DELETE FROM item_status_history
              WHERE UniqueID = ?", params = list(unique_id))
      }
      
      dbCommit(con) # 提交事务
      
      # 通知用户成功删除
      showNotification("物品及其历史状态记录删除成功！", type = "message")
      
      # 删除物品需要手动触发更新inventory
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
    })
    
    # 关闭确认框
    removeModal()
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 账务管理分页                                               ##
  ##                                                            ##
  ################################################################
  
  is_update_mode <- reactiveVal(FALSE)  # 初始化为登记模式
  selected_TransactionID  <- reactiveVal(NULL)  # 存储选中的记录 ID
  selected_TransactionImagePath <- reactiveVal(NULL)  # 存储选中的记录图片路径
  
  # 初始化全局缓存，用于存储各账户的哈希值
  transaction_table_hash <- reactiveValues(
    salary = NULL,
    dollar = NULL,
    purchase = NULL,
    general = NULL
  )
  
  # 定义转账种类说明映射
  category_notes <- list(
    "采购" = "记录购买商品与相关运费的支出",
    "税费" = "包括会计费，公司税务等法定税款",
    "杂费" = "各种运营支出，例如包装材料费、网费等",
    "工资" = "员工薪资、劳务费、兼职费等支付",
    "债务" = "记录公司借款还款",
    "社保" = "社保、公积金等相关转账",
    "图解" = "记录购买图解的支出",
    "其他" = "其他无法归类的交易"
  )
  
  # 账务登记的种类说明
  output$transaction_category_note <- renderText({
    category_notes[[input$transaction_category]] %||% ""
  })
  
  # 资金转移的种类说明
  output$transfer_category_note <- renderText({
    category_notes[[input$transfer_category]] %||% ""
  })
  
  # 分页切换更新
  observe({
    # 确保 input$transaction_tabs 存在
    req(input$transaction_tabs)
    
    if (input$transaction_tabs == "账户余额总览") {
      updateAccountOverview(output, con)
    }
    
    account_type <- switch(
      input$transaction_tabs,
      "工资卡" = "工资卡",
      "美元卡" = "美元卡",
      "买货卡" = "买货卡",
      "一般户卡" = "一般户卡"
    )
    
    if (!is.null(account_type)) {
      refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
      resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
      resetTransactionForm(session, image_transactions)
      resetTransferForm(session, image_transfer)
    }
  })
  
  # 登记转账记录
  observeEvent(input$record_transaction, {
    req(!is.null(input$amount), input$amount > 0, !is.null(input$transaction_type))
    
    # 确定账户类型
    account_type <- switch(
      input$transaction_tabs,
      "工资卡" = "工资卡",
      "美元卡" = "美元卡",
      "买货卡" = "买货卡",
      "一般户卡" = "一般户卡",
      NULL
    )
    
    if (is.null(account_type)) {
      showNotification("请选择有效的账户类型！", type = "error")
      return()
    }
    
    # 合并用户选择的日期和时间为完整时间戳
    transaction_time <- format(as.POSIXct(input$custom_time, format = "%H:%M:%S"), "%H:%M:%S")
    transaction_date <- paste(input$custom_date, transaction_time)
    transaction_datetime <- as.POSIXct(transaction_date, format = "%Y-%m-%d %H:%M:%S")
    
    # 生成 12 位 TransactionID
    transaction_id <- generate_transaction_id(account_type, input$amount, input$remarks, transaction_datetime)
    
    # 区分“登记”和“更新”模式
    if (is_update_mode()) {
      image_path <- process_image_upload(
        sku = selected_TransactionID(),
        file_data = image_transactions$uploaded_file(),
        pasted_data = image_transactions$pasted_file(),
        inventory_path = selected_TransactionImagePath(),
      )
      
      if (is.null(image_path) || length(image_path) != 1) {
        image_path <- ""  # 设置默认值
      }
      
      tryCatch({
        dbExecute(
          con,
          "UPDATE transactions 
         SET Amount = ?, Remarks = ?, TransactionTime = ?, TransactionImagePath = ?, TransactionType = ?
         WHERE TransactionID = ?",
          params = list(
            ifelse(input$transaction_type == "in", input$amount, -input$amount),
            input$remarks,
            transaction_datetime,
            image_path,
            input$transaction_category,
            selected_TransactionID()
          )
        )
        showNotification("记录更新成功！", type = "message")
        
        update_balance(account_type, con)
        
        resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
        resetTransactionForm(session, image_transactions) # 重置输入框
        
        # 自动更新账户余额和表格
        updateAccountOverview(output, con)
        refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
      }, error = function(e) {
        showNotification(paste("更新失败：", e$message), type = "error")
      })
    } else {
      # 登记逻辑
      tryCatch({
        # 插入交易记录
        transaction_id <- generate_transaction_id(account_type, input$amount, input$remarks, transaction_datetime)
        dbExecute(
          con,
          "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
          params = list(
            transaction_id,
            account_type,
            input$transaction_category,
            ifelse(input$transaction_type == "in", input$amount, -input$amount),
            input$remarks,
            process_image_upload(
              sku = transaction_id,
              file_data = image_transactions$uploaded_file(),
              pasted_data = image_transactions$pasted_file()
            ),
            transaction_datetime
          )
        )
        showNotification("记录登记成功！", type = "message")
        
        # 检查是否为最新记录
        latest_time <- dbGetQuery(
          con,
          "SELECT MAX(TransactionTime) AS LatestTime FROM transactions WHERE AccountType = ?",
          params = list(account_type)
        )$LatestTime[1]
        
        if (!is.null(latest_time) && transaction_datetime < as.POSIXct(latest_time)) {
          # 如果插入记录不是最新的，则重新计算余额
          update_balance(account_type, con)
        }
        
        resetTransactionForm(session, image_transactions)
        
        # 自动更新账户余额和表格
        updateAccountOverview(output, con)
        refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
      }, error = function(e) {
        showNotification(paste("登记失败：", e$message), type = "error")
      })
    }
  })
  
  # 登记资金转移记录
  observeEvent(input$record_transfer, {
    req(!is.null(input$transfer_amount), input$transfer_amount > 0)
    req(!is.null(input$from_account), !is.null(input$to_account))
    
    if (input$from_account == input$to_account) {
      showNotification("转出账户和转入账户不能相同！", type = "error")
      return()
    }
    
    # 动态生成备注信息
    transfer_remarks_from <- paste0("[转出至 ", input$to_account, "] ", input$transfer_remarks)
    transfer_remarks_to <- paste0("[从 ", input$from_account, " 转入] ", input$transfer_remarks)
    
    # 处理图片上传
    transfer_image_path <- process_image_upload(
      sku = paste0(input$from_account, "_", input$to_account, "_", Sys.time()), # 用账户和时间生成唯一标识
      file_data = image_transfer$uploaded_file(),
      pasted_data = image_transfer$pasted_file()
    )
    
    if (is.null(transfer_image_path) || is.na(transfer_image_path)) {
      transfer_image_path <- NA_character_  # 如果未上传图片，空
    }
    
    tryCatch({
      # 生成 TransactionID
      transaction_id_from <- generate_transaction_id(
        account_type = input$from_account,
        amount = -input$transfer_amount,
        remarks = transfer_remarks_from,
        transaction_datetime = Sys.time()
      )
      
      transaction_id_to <- generate_transaction_id(
        account_type = input$to_account,
        amount = input$transfer_amount,
        remarks = transfer_remarks_to,
        transaction_datetime = Sys.time()
      )
      
      # 插入转出记录
      dbExecute(
        con,
        "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) 
       VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(transaction_id_from, input$from_account, input$transfer_category, -input$transfer_amount, transfer_remarks_from, transfer_image_path, Sys.time())
      )
      
      # 插入转入记录
      dbExecute(
        con,
        "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionImagePath, TransactionTime) 
       VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(transaction_id_to, input$to_account, input$transfer_category, input$transfer_amount, transfer_remarks_to, transfer_image_path, Sys.time())
      )
      
      showNotification("资金转移记录成功！", type = "message")
      
      # 自动更新账户余额
      updateAccountOverview(output, con)
      
      # 自动刷新表格
      refreshTransactionTable(input$from_account, cache_env, transaction_table_hash, output, con)
      refreshTransactionTable(input$to_account, cache_env, transaction_table_hash, output, con)
      
      # 清空表单
      resetTransferForm(session, image_transfer) # 重置输入框
      
    }, error = function(e) {
      showNotification(paste("资金转移失败：", e$message), type = "error")
    })
  })
  
  # 删除转账记录 (登记)
  observeEvent(input$delete_transaction, {
    current_tab <- input$transaction_tabs
    
    account_type <- getAccountType(input)
    
    if (is.null(account_type)) {
      showNotification("请选择有效的账户类型！", type = "error")
      return()
    }
    
    # 获取选中的行
    selected_rows <- switch(
      current_tab,
      "工资卡" = input$salary_card_table_rows_selected,
      "美元卡" = input$dollar_card_table_rows_selected,
      "买货卡" = input$purchase_card_table_rows_selected,
      "一般户卡" = input$general_card_table_rows_selected
    )
    
    if (length(selected_rows) > 0) {
      # 手动构造 LIMIT 的参数
      row_index <- selected_rows - 1
      
      # 查询选中记录的 TransactionID
      query <- sprintf(
        "SELECT TransactionID FROM transactions WHERE AccountType = '%s' ORDER BY TransactionTime DESC LIMIT %d, 1",
        account_type, row_index
      )
      record_to_delete <- dbGetQuery(con, query)
      
      if (nrow(record_to_delete) > 0) {
        tryCatch({
          # 删除选中的记录
          dbExecute(con, "DELETE FROM transactions WHERE TransactionID = ?", params = list(record_to_delete$TransactionID))
          
          # 重新计算所有balance记录
          update_balance(account_type, con)
          
          # 自动刷新账户余额总览统计
          updateAccountOverview(output, con)
          
          # 自动刷新表格
          refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
        }, error = function(e) {
          showNotification(paste("删除失败：", e$message), type = "error")
        })
      } else {
        showNotification("无法找到选中的记录！", type = "error")
      }
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
    
    resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
    resetTransactionForm(session, image_transactions) # 重置输入框
  })
  
  # 删除转账记录 (转移)
  observeEvent(input$delete_transfer, {
    current_tab <- input$transaction_tabs
    
    account_type <- getAccountType(input)
    
    if (is.null(account_type)) {
      showNotification("请选择有效的账户类型！", type = "error")
      return()
    }
    
    # 获取选中的行
    selected_rows <- switch(
      current_tab,
      "工资卡" = input$salary_card_table_rows_selected,
      "美元卡" = input$dollar_card_table_rows_selected,
      "买货卡" = input$purchase_card_table_rows_selected,
      "一般户卡" = input$general_card_table_rows_selected
    )
    
    if (length(selected_rows) > 0) {
      # 手动构造 LIMIT 的参数
      row_index <- selected_rows - 1
      
      # 查询选中记录的 TransactionID
      query <- sprintf(
        "SELECT TransactionID FROM transactions WHERE AccountType = '%s' ORDER BY TransactionTime DESC LIMIT %d, 1",
        account_type, row_index
      )
      record_to_delete <- dbGetQuery(con, query)
      
      if (nrow(record_to_delete) > 0) {
        tryCatch({
          # 删除选中的记录
          dbExecute(con, "DELETE FROM transactions WHERE TransactionID = ?", params = list(record_to_delete$TransactionID))
          
          # 重新计算所有balance记录
          update_balance(account_type, con)
          
          # 自动刷新账户余额总览统计
          updateAccountOverview(output, con)
          
          # 自动刷新表格
          refreshTransactionTable(account_type, cache_env, transaction_table_hash, output, con)
        }, error = function(e) {
          showNotification(paste("删除失败：", e$message), type = "error")
        })
      } else {
        showNotification("无法找到选中的记录！", type = "error")
      }
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
    
    resetTransferForm(session, image_transfer) # 重置输入框
  })
  
  # 转账证据图片处理模块 (登记)
  image_transactions <- imageModuleServer("image_transactions")
  
  # 转账证据图片处理模块 (转移)
  image_transfer <- imageModuleServer("image_transfer")
  
  # 重置 (登记)
  observeEvent(input$reset_form, {
    resetToCreateMode(is_update_mode, selected_TransactionID, selected_TransactionImagePath, session)
    resetTransactionForm(session, image_transactions) # 重置输入框
    showNotification("表单已重置！", type = "message")
  })
  
  # 重置 (转移)
  observeEvent(input$reset_form_transfer, {
    resetTransferForm(session, image_transfer) # 重置输入框
    showNotification("表单已重置！", type = "message")
  })
  
  ###
  
  # 汇总
  observeEvent(input$summary_daily, {
    calculate_summary("day")
  })
  observeEvent(input$summary_monthly, {
    calculate_summary("month")
  })
  observeEvent(input$summary_yearly, {
    calculate_summary("year")
  })
  
  calculate_summary <- function(period) {
    req(input$summary_date_range)
    
    start_date <- input$summary_date_range[1]
    end_date <- input$summary_date_range[2]
    
    # **获取当前选中的账户类型**
    account_type <- switch(
      input$transaction_tabs,
      "工资卡" = "工资卡",
      "美元卡" = "美元卡",
      "买货卡" = "买货卡",
      "一般户卡" = "一般户卡",
      NULL
    )
    
    if (is.null(account_type)) {
      showNotification("请选择一个账户再进行统计！", type = "error")
      return()
    }
    
    summary_data <- dbGetQuery(con, sprintf("
    SELECT DATE_FORMAT(TransactionTime, CASE 
      WHEN '%s' = 'day' THEN '%%Y-%%m-%%d'
      WHEN '%s' = 'month' THEN '%%Y-%%m'
      WHEN '%s' = 'year' THEN '%%Y'
    END) AS Period,
    SUM(CASE WHEN Amount > 0 THEN Amount ELSE 0 END) AS Income,
    SUM(CASE WHEN Amount < 0 THEN ABS(Amount) ELSE 0 END) AS Expense
    FROM transactions
    WHERE TransactionTime BETWEEN '%s' AND '%s' AND AccountType = '%s'
    GROUP BY Period
    ORDER BY Period ASC
  ", period, period, period, start_date, end_date, account_type))
    
    # **修正 Period 格式**
    summary_data$Period <- sapply(summary_data$Period, function(x) {
      if (period == "month") {
        return(paste0(substr(x, 1, 4), "年", substr(x, 6, 7), "月"))
      } else if (period == "year") {
        return(paste0(x, "年"))
      } else {
        return(x)  # 按天时，格式不变 YYYY-MM-DD
      }
    })
    
    # **修改列名，显示中文**
    colnames(summary_data) <- c("时期", "总收入（元）", "总支出（元）")
    
    # **弹出窗口显示统计结果（使用 `DT::datatable` 渲染表格）**
    showModal(modalDialog(
      title = paste0("账务统计 - ", switch(period, day="每日", month="每月", year="每年"), "（", account_type, "）"),
      DT::dataTableOutput("summary_table"),
      easyClose = TRUE,
      footer = modalButton("关闭")
    ))
    
    output$summary_table <- DT::renderDataTable({
      DT::datatable(
        summary_data, 
        options = list(
          dom = "t",   # 仅显示表格
          paging = FALSE,  # 关闭分页
          ordering = FALSE,  # 关闭排序
          columnDefs = list(list(className = "dt-center", targets = "_all"))  # 让表格内容居中
        ),
        rownames = FALSE  # 不显示行号
      )
    })
  }
  
  ####

  # 监听 工资卡 点选
  observeEvent(input$salary_card_table_rows_selected, {
    selected_row <- input$salary_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("工资卡", selected_row, cache_env, con, session)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 美元卡 点选
  observeEvent(input$dollar_card_table_rows_selected, {
    selected_row <- input$dollar_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("美元卡", input$dollar_card_table_rows_selected, cache_env, con, session)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 买货卡 点选
  observeEvent(input$purchase_card_table_rows_selected, {
    selected_row <- input$purchase_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("买货卡", input$purchase_card_table_rows_selected, cache_env, con, session)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  # 监听 一般户卡 点选
  observeEvent(input$general_card_table_rows_selected, {
    selected_row <- input$general_card_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 获取选中行的数据
      fetchData <- fetchInputFromTable("一般户卡", input$general_card_table_rows_selected, cache_env, con, session)
      selected_TransactionID(fetchData$TransactionID)
      selected_TransactionImagePath(fetchData$TransactionImagePath)
      
      # 切换按钮为“更新”
      is_update_mode(TRUE)
      updateActionButton(session, "record_transaction", label = "更新", icon = icon("edit"))
      
      showNotification("信息已加载，准备更新记录", type = "message")
    }
  })
  
  ####
  
  # 处理工资卡表格的图片点击
  handleTransactionImageClick("工资卡", "salary_card_table", 5, input, cache_env, con, session)
  
  # 处理美元卡表格的图片点击
  handleTransactionImageClick("美元卡", "dollar_card_table", 5, input, cache_env, con, session)
  
  # 处理买货卡表格的图片点击
  handleTransactionImageClick("买货卡", "purchase_card_table", 5, input, cache_env, con, session)
  
  # 处理一般户卡表格的图片点击
  handleTransactionImageClick("一般户卡", "general_card_table", 5, input, cache_env, con, session)

  
  
  ################################################################
  ##                                                            ##
  ## 查询分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听主页面和子页面的切换
  observeEvent({
    list(input$inventory_cn, input$query_tabs)  # 仅在这些输入发生变化时触发
  }, {
    if (input$inventory_cn == "查询" && input$query_tabs == "商品状态") {
      inventory_refresh_trigger(!inventory_refresh_trigger())
      showNotification("库存表已加载！", type = "message")
    }
  }, ignoreInit = TRUE)  # 忽略初始值
  
  # 动态渲染 sticky-sidebar
  output$query_dynamic_sticky_sidebar <- renderUI({
    current_tab <- input$query_tabs
    
    if (current_tab == "商品状态") {
      # 商品状态页面的 sticky-sidebar 内容
      div(
        itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
        tags$hr(),
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
        ),
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #DC3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("售罄物品", style = "color: #DC3545; font-weight: bold; margin-bottom: 15px;"),
          radioButtons(
            inputId = "query_stock_status",
            label = NULL,
            choices = c("不过滤" = "none", "美国售罄, 国内有货" = "us", "国内售罄, 美国有货" = "domestic", "全库存售罄" = "all"),
            selected = "none",
            inline = FALSE
          )
        )
      )
    } else if (current_tab == "采购开销") {
      # 采购开销页面的 sticky-sidebar 内容
      div(
        class = "card",
        style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
        tags$h4("供应商筛选", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
        selectInput(
          inputId = "purchase_check_filter_maker",
          label = NULL,
          choices = c("所有供应商" = "all"),  # 初始默认选项          
          selected = "all",
          width = "100%"
        )
      )
    } else if (current_tab == "库存总览") {
      # 库存总览页面的 sticky-sidebar 内容（空白占位符）
      div(
        p("此处留空，未来扩展...", style = "color: #888; text-align: center; margin-top: 20px;")
      )
    } else {
      # 默认占位符（以防万一）
      div(
        p("请选择一个选项卡", style = "color: #888; text-align: center; margin-top: 20px;")
      )
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "query_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听点击事件，弹出大图
  observeEvent(input$show_large_image, {
    req(input$show_large_image)  # 确保图片路径有效
    
    showModal(modalDialog(
      title = "物品图片预览",
      tags$div(
        style = "overflow: auto; max-height: 700px; text-align: center;",
        tags$img(
          src = input$show_large_image,  # 直接使用传入的图片路径
          style = "max-width: 100%; height: auto; display: inline-block; border: 1px solid #ddd; border-radius: 8px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ###
  
  # 右键点击选择商品
  query_soldout_selected_item_details <- reactiveVal()
  
  # 监听鼠标右键 selected_inventory_row，并获取用户点击的 SKU。
  observeEvent(input$selected_inventory_row, {
    req(input$selected_inventory_row)
    
    row_index <- as.numeric(input$selected_inventory_row)  # 获取用户点击的行索引
    selected_item <- filtered_inventory()[row_index, ]  # 获取选中的数据
    
    if (nrow(selected_item) > 0) {
      # 存储物品详情
      query_soldout_selected_item_details(list(
        sku = selected_item$SKU,
        name = selected_item$ItemName,
        image = ifelse(
          is.na(selected_item$ItemImagePath) || selected_item$ItemImagePath == "",
          placeholder_150px_path,
          paste0(host_url, "/images/", basename(selected_item$ItemImagePath))
        ),
        maker = selected_item$Maker,
        domestic_stock = selected_item$DomesticQuantity
      ))
      
      # 动态更新出库请求按钮
      output$query_outbound_request_btn <- renderUI({
        if (selected_item$DomesticQuantity > 0) {
          actionButton("query_outbound_request", "出库请求", class = "btn btn-success btn-sm", style = "width: 100%;")
        } else {
          NULL  # 不显示按钮
        }
      })
    }
  })
  
  # 点击采购请求
  observeEvent(input$query_purchase_request, {
    req(query_soldout_selected_item_details())
    
    details <- query_soldout_selected_item_details()
    
    showModal(modalDialog(
      title = "创建采购请求",
      
      div(
        style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
        
        # 左侧：商品图片 + 详情
        div(
          style = "flex: 0 0 40%; text-align: center;",
          tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
          div(
            tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
            tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
            tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;")
          )
        ),
        
        # 右侧：采购数量 + 备注
        div(
          style = "flex: 0 0 50%;",
          numericInput("query_purchase_qty", "采购数量", value = 1, min = 1, width = "80%"),
          textAreaInput("query_purchase_remark", "备注", "", width = "80%", height = "80px")
        )
      ),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("query_confirm_purchase", "确认采购", class = "btn-primary")
      )
    ))
  })
  
  # 确认采购
  observeEvent(input$query_confirm_purchase, {
    req(query_soldout_selected_item_details(), input$query_purchase_qty)
    
    details <- query_soldout_selected_item_details()
    request_id <- uuid::UUIDgenerate()
    
    # 数据库操作：插入采购请求
    dbExecute(con, "
    INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
    VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
              params = list(
                request_id,
                details$sku,
                details$maker,
                details$image,
                details$name,
                input$query_purchase_qty,
                format_remark(input$query_purchase_remark, system_type) 
              )
    )
    
    bind_buttons(request_id, requests_data(), input, output, session, con)
    
    showNotification("采购请求已创建", type = "message")
    removeModal()  # 关闭模态框
  })
  
  # 点击出库请求
  observeEvent(input$query_outbound_request, {
    req(query_soldout_selected_item_details())
    
    details <- query_soldout_selected_item_details()
    
    showModal(modalDialog(
      title = "创建出库请求",
      
      div(
        style = "display: flex; flex-direction: row; align-items: center; gap: 20px; margin-bottom: 15px;",
        
        # 左侧：商品图片 + 详情
        div(
          style = "flex: 0 0 40%; text-align: center;",
          tags$img(src = details$image, style = "width: 150px; height: auto; object-fit: contain; border-radius: 8px;"),
          div(
            tags$h4(details$name, style = "margin-top: 10px; color: #007BFF;"),
            tags$p(paste("SKU:", details$sku), style = "margin: 0; font-weight: bold;"),
            tags$p(paste("供应商:", details$maker), style = "margin: 0; color: #6c757d; font-size: 14px;"),
            tags$p(
              paste("国内库存:", details$domestic_stock),
              style = paste("margin: 0;", ifelse(details$domestic_stock == 0, "color: #DC3545; font-weight: bold;", "color: #28A745;"))
            )
          )
        ),
        
        # 右侧：出库数量 + 备注
        div(
          style = "flex: 0 0 50%; display: flex; flex-direction: column; gap: 10px;",
          numericInput("query_outbound_qty", "出库数量", value = 1, min = 1, max = details$domestic_stock, width = "80%"),
          textAreaInput("query_outbound_remark", "备注", "", width = "80%", height = "80px")
        )
      ),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("query_confirm_outbound", "确认出库", class = "btn-success")
      )
    ))
  })
  
  # 确认出库
  observeEvent(input$query_confirm_outbound, {
    req(query_soldout_selected_item_details(), input$query_outbound_qty)
    
    details <- query_soldout_selected_item_details()
    request_id <- uuid::UUIDgenerate()
    
    # 如果用户输入的出库数量大于国内库存，禁止提交
    if (input$query_outbound_qty > details$domestic_stock) {
      showNotification("出库数量不能大于国内库存数！", type = "error")
      return()
    }
    
    # 数据库操作：插入出库请求
    dbExecute(con, "
    INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
    VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '出库')",
              params = list(
                request_id,
                details$sku,
                details$maker,
                details$image,
                details$name,
                input$query_outbound_qty,
                format_remark(input$query_outbound_remark, system_type)
              )
    )
    
    bind_buttons(request_id, requests_data(), input, output, session, con)
    
    showNotification("出库请求已创建", type = "message")
    removeModal()  # 关闭模态框
  })
  
  ###
  
  # 根据SKU产生图表
  observe({
    sku <- trimws(as.character(input$query_sku %||% ""))
    
    if (sku == "") {
      output$query_item_info <- renderUI({ div() })
      output$inventory_status_chart <- renderPlotly({ NULL })
      output$defect_status_chart <- renderPlotly({ NULL })
      return()
    }
    
    tryCatch({
      sku_data <- inventory() %>% filter(SKU == sku)
      
      if (nrow(sku_data) == 0) {
        output$query_item_info <- renderUI({
          div(tags$p("未找到该 SKU 对应的商品信息！", style = "color: red; font-size: 16px;"))
        })
        return()
      }
      
      output$query_item_info <- renderUI({
        img_path <- ifelse(
          is.na(sku_data$ItemImagePath[1]),
          placeholder_200px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )
        
        div(
          style = "display: flex; flex-direction: column; padding: 10px;",
          
          # 上部分：图片和基本信息
          div(
            style = "display: flex; align-items: flex-start; width: 100%;",
            
            # 图片区域（带点击事件）
            div(
              style = "flex: 1; text-align: center; padding-right: 10px;",
              tags$img(
                src = img_path, height = "200px",
                style = "border: 1px solid #ddd; border-radius: 8px; cursor: pointer;",
                onclick = sprintf("Shiny.setInputValue('show_large_image', '%s', {priority: 'event'})", img_path)
              )
            ),
            
            # 右侧：商品信息
            div(
              style = "flex: 2;",
              tags$table(
                style = "width: 100%; border-collapse: collapse; line-height: 2;",
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "商品名称："), 
                  tags$td(style = "word-break: break-word;", sku_data$ItemName[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "供应商："), 
                  tags$td(style = "word-break: break-word;", sku_data$Maker[1])
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "分类："), 
                  tags$td(style = "word-break: break-word;", paste(sku_data$MajorType[1], "/", sku_data$MinorType[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均成本："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ProductCost[1]))
                ),
                tags$tr(
                  tags$td(style = "white-space: nowrap; font-weight: bold; min-width: 90px;", "平均运费："), 
                  tags$td(style = "word-break: break-word;", sprintf("¥%.2f", sku_data$ShippingCost[1]))
                )
              )
            )
          ),
          
          # 底部：库存信息
          div(
            style = "width: 100%; margin-top: 10px; text-align: center; padding-top: 5px; border-top: 1px solid #ddd;",
            tags$span(
              style = "font-weight: bold;",
              "库存数："
            ),
            tags$span(
              HTML(sprintf(
                "国内：%d &emsp;|&emsp; 在途：%d &emsp;|&emsp; 美国：%d &emsp;|&emsp; 总计：%d",
                sku_data$DomesticQuantity[1], 
                sku_data$TransitQuantity[1], 
                sku_data$UsQuantity[1], 
                sku_data$Quantity[1]
              ))
            )
          )
        )
      })
      
      # 渲染库存状态图表
      output$inventory_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          inventory_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Status) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          inventory_status_data <- merge(
            data.frame(Status = status_levels),
            inventory_status_data,
            by = "Status",
            all.x = TRUE
          )
          inventory_status_data$Count[is.na(inventory_status_data$Count)] <- 0
          
          # 按 status_levels 排序，确保颜色对应
          inventory_status_data <- inventory_status_data[match(status_levels, inventory_status_data$Status), ]
          
          if (sum(inventory_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = inventory_status_data,
              labels = ~Status,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = status_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("库存状态图表生成错误：", e$message), type = "error")
          output$inventory_status_chart <- renderPlotly({ NULL })
        })
      })
      
      # 渲染瑕疵情况图表
      output$defect_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          defect_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Defect) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          defect_levels <- c("未知", "无瑕", "瑕疵", "修复")
          defect_colors <- c("darkgray", "green", "red", "orange")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          defect_status_data <- merge(
            data.frame(Defect = defect_levels),
            defect_status_data,
            by = "Defect",
            all.x = TRUE
          )
          defect_status_data$Count[is.na(defect_status_data$Count)] <- 0
          
          # 按 defect_levels 排序，确保颜色对应
          defect_status_data <- defect_status_data[match(defect_levels, defect_status_data$Defect), ]
          
          if (sum(defect_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = defect_status_data,
              labels = ~Defect,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = defect_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = FALSE, # 隐藏图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("瑕疵情况图表生成错误：", e$message), type = "error")
          output$defect_status_chart <- renderPlotly({ NULL })
        })
      })
      
    }, error = function(e) {
      showNotification(paste("发生错误：", e$message), type = "error")
    })
  })
  
  #################################################################
  
  # 开销统计
  expense_summary_data <- reactive({
    req(input$time_range)
    data <- unique_items_data()
    
    start_date <- as.Date(input$time_range[1])
    end_date <- as.Date(input$time_range[2])
    
    time_sequence <- switch(input$precision,
                            "天" = seq.Date(from = start_date, to = end_date, by = "day"),
                            "周" = seq.Date(from = floor_date(start_date, "week"),
                                           to = floor_date(end_date, "week"), by = "week"),
                            "月" = seq.Date(from = floor_date(start_date, "month"),
                                           to = floor_date(end_date, "month"), by = "month"),
                            "年" = seq.Date(from = floor_date(start_date, "year"),
                                           to = floor_date(end_date, "year"), by = "year"))
    
    time_df <- data.frame(GroupDate = time_sequence)
    
    summarized_data <- data %>%
      filter(!is.na(PurchaseTime) & PurchaseTime >= start_date & PurchaseTime <= end_date) %>%
      mutate(
        GroupDate = case_when(
          input$precision == "天" ~ as.Date(PurchaseTime),
          input$precision == "周" ~ floor_date(as.Date(PurchaseTime), "week"),
          input$precision == "月" ~ floor_date(as.Date(PurchaseTime), "month"),
          input$precision == "年" ~ floor_date(as.Date(PurchaseTime), "year")
        )
      ) %>%
      group_by(GroupDate) %>%
      summarise(
        Cost_Domestic = round(sum(ProductCost + DomesticShippingCost, na.rm = TRUE), 2),
        ProductCost = round(sum(ProductCost, na.rm = TRUE), 2),
        DomesticShippingCost = round(sum(DomesticShippingCost, na.rm = TRUE), 2),
        IntlShippingCost = round(sum(IntlShippingCost, na.rm = TRUE), 2),
        TotalExpense = round(sum(ProductCost + DomesticShippingCost + IntlShippingCost, na.rm = TRUE), 2),
        AllPurchaseCheck = all(PurchaseCheck == 1, na.rm = TRUE), # 是否全部为1
        .groups = "drop"
      )
    
    complete_data <- time_df %>%
      left_join(summarized_data, by = "GroupDate") %>%
      replace_na(list(
        Cost_Domestic = 0,
        ProductCost = 0,
        DomesticShippingCost = 0,
        IntlShippingCost = 0,
        TotalExpense = 0,
        AllPurchaseCheck = FALSE # 默认设置为 FALSE
      ))
    
    complete_data
  })
  
  # 定义 reactiveVal 用于存储观察器状态
  is_observer_click_suspended <- reactiveVal(TRUE)
  
  # 存储选定的时间范围
  selected_range <- reactiveVal(NULL) # 存储时间范围
  
  # 开销柱状图
  output$expense_chart <- renderPlotly({
    req(expense_summary_data())
    data <- expense_summary_data()
    
    # 获取用户选择的 Y 轴变量及颜色
    y_var <- switch(input$expense_type,
                    "total" = "TotalExpense",
                    "cost" = "ProductCost",
                    "domestic_shipping" = "DomesticShippingCost",
                    "intl_shipping" = "IntlShippingCost",
                    "cost_domestic" = "Cost_Domestic")
    color <- switch(input$expense_type,
                    "total" = "#007BFF",
                    "cost" = "#4CAF50",
                    "domestic_shipping" = "#FF5733",
                    "intl_shipping" = "#FFC107",
                    "cost_domestic" = "#17A2B8")
    
    # 根据精度生成时间范围标签
    data <- data %>%
      mutate(
        GroupLabel = case_when(
          input$precision == "天" ~ format(GroupDate, "%Y-%m-%d"),
          input$precision == "周" ~ paste(
            format(floor_date(GroupDate, "week"), "%Y-%m-%d"),
            "\n至\n",
            format(ceiling_date(GroupDate, "week") - 1, "%Y-%m-%d")
          ),
          input$precision == "月" ~ format(GroupDate, "%Y-%m"),
          input$precision == "年" ~ format(GroupDate, "%Y")
        )
      )
    
    # 创建柱状图
    p <- plot_ly(
      data,
      x = ~GroupLabel,
      y = ~get(y_var),
      type = "bar",
      name = NULL,
      marker = list(color = color),
      text = ~round(get(y_var), 2),
      textposition = "outside",
      source = "expense_chart" # 确保 source 唯一
    ) %>%
      # 注册事件
      event_register("plotly_click") %>%
      event_register("plotly_selected") %>%
      # 显示圆底对勾
      add_trace(
        type = "scatter",
        mode = "markers+text", # 同时使用 markers 和 text 模式
        x = ~GroupLabel,
        y = ~get(y_var) + (max(data[[y_var]], na.rm = TRUE) * 0.15), # 在柱子顶部留出空间
        marker = list(
          size = 20, # 圆点的大小
          color = ~ifelse(AllPurchaseCheck, "#039e2a", "#D3D3D3"), # 根据状态设置深绿色或浅灰色
          line = list(width = 0) # 移除外边框
        ),
        text = ~ifelse(AllPurchaseCheck, "\u2714", "\u2714"), # 使用 Unicode 的白色勾
        textfont = list(
          size = 14, # 增大字体，增强可见度
          color = "white", # 勾的颜色为白色
          weight = "bold" # 加粗字体
        ),
        textposition = "middle center", # 勾的位置在圆点正中央
        showlegend = FALSE # 不显示图例
      ) %>%
      # 添加布局和其他设置
      layout(
        xaxis = list(
          title = "",
          tickvals = data$GroupLabel,
          tickangle = -45,
          tickfont = list(size = 12),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "开销（元）",
          tickfont = list(size = 12),
          showgrid = TRUE  # 保留网格线
        ),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        showlegend = FALSE,
        plot_bgcolor = "#F9F9F9",
        paper_bgcolor = "#FFFFFF"
      )
    
    # 激活观察器
    if (is_observer_click_suspended()) {
      observer_click$resume()
      is_observer_click_suspended(FALSE)
    }

    p
  })
  
  # 定义点击观察器，初始状态为 suspended = TRUE
  observer_click <- observeEvent(event_data("plotly_click", source = "expense_chart"), suspended = TRUE, {
    clicked_point <- event_data("plotly_click", source = "expense_chart")
    if (!is.null(clicked_point)) {
      precision <- input$precision # 当前精度（天、周、月、年）

      # 根据精度解析点击的时间点
      clicked_date <- switch(
        precision,
        "年" = as.Date(paste0(clicked_point$x, "-01-01")), # 对"年"进行特殊处理
        as.Date(clicked_point$x) # 其他情况直接转为日期
      )
      
      # 根据精度计算时间范围
      range <- switch(precision,
                      "天" = c(clicked_date, clicked_date),
                      "周" = c(floor_date(clicked_date, "week"), ceiling_date(clicked_date, "week") - 1),
                      "月" = c(floor_date(clicked_date, "month"), ceiling_date(clicked_date, "month") - 1),
                      "年" = c(floor_date(clicked_date, "year"), ceiling_date(clicked_date, "year") - 1)
      )
      
      # 调用 updateDateRangeInput 更新用户界面的时间范围选择
      updateDateRangeInput(
        session,
        inputId = "time_range",
        start = range[1],
        end = range[2]
      )
      
      selected_range(range)
      show_summary(TRUE)
      
      # 更新 selectInput 的 choices
      updateSelectInput(
        session,
        inputId = "purchase_check_filter_maker",
        choices = c("所有供应商" = "all", unique(supplier_summary()$Maker)),
        selected = "all"
      )
    }
  })

  # 筛选物品详情数据
  filtered_items <- reactive({
    req(selected_range()) # 确保时间范围存在
    range <- selected_range()
    
    # 从物品数据中筛选出时间范围内的数据
    unique_items_data() %>%
      filter(PurchaseTime >= range[1] & PurchaseTime <= range[2]) %>%
      arrange(PurchaseTime) # 按采购时间升序排列
  })
  
  # 采购物品汇总数据
  supplier_summary <- reactive({
    req(filtered_items())
    items <- filtered_items()
    
    # 第一步：按 Maker 和 ItemName 分组，计算每件物品的采购数量
    item_details <- items %>%
      group_by(Maker, ItemName, SKU, ItemImagePath, ProductCost, DomesticShippingCost) %>%
      summarise(
        Quantity = n(),  # 每件物品的采购数量
        .groups = 'drop'
      ) %>%
      mutate(
        src = ifelse(is.na(ItemImagePath), 
                     placeholder_150px_path, 
                     paste0(host_url, "/images/", basename(ItemImagePath)))
      )
    
    # 第二步：按 Maker 分组，计算汇总数据
    summary_data <- items %>%
      group_by(Maker) %>%
      summarise(
        TotalItemCost = sum(ProductCost, na.rm = TRUE),
        TotalDomesticShipping = sum(DomesticShippingCost, na.rm = TRUE),
        TotalExpense = TotalItemCost + TotalDomesticShipping,
        TotalQuantity = n(),  # 总采购数量（所有记录数）
        Items = list(
          item_details[item_details$Maker == first(Maker), 
                       c("ItemName", "SKU", "Quantity", "ProductCost", "DomesticShippingCost", "src"), drop = FALSE]
        )
      ) %>%
      ungroup()
    
    return(summary_data)
  })
  
  # 采购物品汇总 UI
  output$purchase_summary_by_maker_ui <- renderUI({
    if (!show_summary()) {
      return(NULL)  # 如果不显示，返回 NULL
    }
    
    summary_data <- supplier_summary()
    
    # 根据 purchase_check_filter_maker 筛选
    if (!is.null(input$purchase_check_filter_maker) && input$purchase_check_filter_maker != "all") {
      summary_data <- summary_data %>% filter(Maker == input$purchase_check_filter_maker)
    }
    
    cards <- lapply(1:nrow(summary_data), function(i) {
      maker <- summary_data$Maker[i]
      total_item_cost <- summary_data$TotalItemCost[i]
      total_domestic_shipping <- summary_data$TotalDomesticShipping[i]
      total_expense <- summary_data$TotalExpense[i]
      total_quantity <- summary_data$TotalQuantity[i]
      items <- summary_data$Items[[i]]
      
      item_displays <- if (nrow(items) == 0) {
        p("暂无物品详情", style = "text-align: center; color: #888;")
      } else {
        lapply(1:nrow(items), function(j) {
          item_name <- items$ItemName[j]
          sku <- items$SKU[j]
          itemcost <- items$ProductCost[j]
          shipcost <- items$DomesticShippingCost[j]
          quantity <- items$Quantity[j]
          src <- items$src[j]
          
          div(
            style = "display: inline-block; margin-right: 15px; text-align: center; width: 150px;",
            img(src = src, width = "100px", height = "100px", style = "border-radius: 5px;"),
            p(strong(item_name), style = "font-size: 14px; margin: 5px 0;"),
            p(sku, style = "font-size: 12px; color: #555;"),
            p(paste("单价(运费):", itemcost, "(", sprintf("%.2f", shipcost), ")"), style = "font-size: 12px; color: #555;"),
            p(paste("数量:", quantity), style = "font-size: 12px; color: #555;")
          )
        })
      }
      
      div(
        class = "card",
        style = "margin-top: 10px; margin-bottom: 10px; padding: 15px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1); background-color: #fff;",
        # Maker 和总开销金额靠左，其他信息靠右
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div(
            style = "display: flex; align-items: center; gap: 15px;",  # Maker 和总开销金额靠左
            h4(maker, style = "margin: 0; color: #007BFF; font-weight: bold;"),
            p(
              paste("（总开销: ￥", round(total_expense, 2), "）"),
              style = "font-size: 18px; color: #FF4500; margin: 0;"  # 突出总开销金额
            )
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-end;",  # 靠右但文字左对齐
            p(paste("采购数:", total_quantity), style = "font-size: 14px; margin: 2px 0; color: #333; text-align: left;"),
            p(paste("总成本: ￥", round(total_item_cost, 2)), style = "font-size: 14px; margin: 2px 0; color: #333; text-align: left;"),
            p(paste("总运费: ￥", round(total_domestic_shipping, 2)), style = "font-size: 14px; margin: 2px 0; color: #333; text-align: left;")
          )
        ),
        # 物品详情
        div(
          style = "overflow-x: auto; white-space: nowrap; padding: 10px 0; border-top: 1px solid #eee; border-bottom: 1px solid #eee;",
          do.call(tagList, list(item_displays))
        )
      )
    })
    
    do.call(tagList, cards)
  })
  
  # 总开销分布饼图
  output$pie_chart <- renderPlotly({
    data <- expense_summary_data()
    
    # 饼图数据：计算总开销分布
    total_product_cost <- sum(data$ProductCost, na.rm = TRUE)
    total_domestic_shipping_cost <- sum(data$DomesticShippingCost, na.rm = TRUE)
    total_intl_shipping_cost <- sum(data$IntlShippingCost, na.rm = TRUE)
    
    pie_data <- data.frame(
      Category = c("商品成本", "国内运费", "国际运费"),
      Value = c(total_product_cost, total_domestic_shipping_cost, total_intl_shipping_cost)
    )
    
    # 获取时间范围
    time_range <- paste(as.Date(input$time_range[1]), "至", as.Date(input$time_range[2]))
    
    # 绘制饼图
    plot_ly(
      pie_data,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      textinfo = "label+value",  # 显示标签和数值
      hoverinfo = "label+percent",  # 悬停显示类别和百分比
      insidetextorientation = "radial",
      marker = list(colors = c("#4CAF50", "#FF5733", "#FFC107"))
    ) %>%
      layout(
        annotations = list(
          x = 0.5, y = -0.2,  # 调整注释的位置
          text = paste("统计时间范围：", time_range),
          showarrow = FALSE,
          font = list(size = 12, color = "#666")
        ),
        showlegend = FALSE,  # 隐藏图例
        paper_bgcolor = "#F9F9F9",  # 背景颜色
        margin = list(l = 50, r = 30, t = 80, b = 50)  # 增加左右和底部边距
      )
  })
  
  show_summary <- reactiveVal(TRUE)
  
  # 重置时间范围
  observeEvent(input$reset_time_range, {
    # 重置时间范围到默认值（最近30天）
    default_start <- Sys.Date() - 30
    default_end <- Sys.Date()
    
    updateDateRangeInput(
      session,
      inputId = "time_range",
      start = default_start,
      end = default_end
    )
    # 设置为不显示
    show_summary(FALSE)
  })
  
  # 开销核对动态UI
  output$confirm_expense_check_ui <- renderUI({
    req(selected_range()) # 确保有选定的时间范围
    
    range <- selected_range() # 获取当前选定的时间范围
    
    # 判断范围是否相等
    label_text <- if (range[1] == range[2]) {
      paste0("确认开销核对（采购时间：", range[1], "）")
    } else {
      paste0("确认开销核对（采购时间：", range[1], " 至 ", range[2], "）")
    }
    
    actionButton(
      inputId = "confirm_expense_check_btn", 
      label = label_text,
      icon = icon("check-circle"), 
      class = "btn-success",
      style = "width: 100%; margin-top: 5px;"
    )
  })
  
  # 确认开销核对
  observeEvent(input$confirm_expense_check_btn, {
    req(filtered_items()) # 确保筛选出的物品数据存在
    
    # 获取筛选出的物品
    items_to_update <- filtered_items()
    
    if (nrow(items_to_update) == 0) {
      showNotification("当前筛选无物品可核对，请选择有效的柱子！", type = "error")
      return(NULL)
    }
    
    # 按采购时间分组统计
    grouped_expenses <- items_to_update %>%
      group_by(PurchaseTime) %>%
      summarise(
        TotalCost = sum(ProductCost, na.rm = TRUE),
        TotalDomesticShipping = sum(DomesticShippingCost, na.rm = TRUE)
      )
    
    # 更新数据库中的 PurchaseCheck 为 1
    tryCatch({
      dbExecute(
        con,
        "UPDATE unique_items SET PurchaseCheck = 1 WHERE UniqueID IN (?)",
        params = list(items_to_update$UniqueID)
      )
      
      showNotification(paste("成功更新", nrow(items_to_update), "条物品的开销核对状态！"), type = "message")
      
      # 将物品成本和国内运费分别登记到"一般户卡"
      grouped_expenses %>%
        rowwise() %>%
        mutate(
          # 生成物品成本的交易记录
          CostTransaction = if (TotalCost > 0) {
            remarks_cost <- paste("[采购成本已核对]", "采购日期:", PurchaseTime)
            transaction_id <- generate_transaction_id("买货卡", TotalCost, remarks_cost, PurchaseTime)
            dbExecute(
              con,
              "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?, ?, ?)",
              params = list(
                transaction_id,
                "买货卡",
                "采购",
                -TotalCost,
                remarks_cost,
                PurchaseTime
              )
            )
            list(transaction_id) # 返回记录的 ID
          } else {
            list(NULL) # 如果总成本为 0，返回 NULL
          },
          
          # 生成国内运费的交易记录
          ShippingTransaction = if (TotalDomesticShipping > 0) {
            remarks_ship <- paste("[国内运费已核对]", "采购日期:", PurchaseTime)
            transaction_id <- generate_transaction_id("买货卡", TotalDomesticShipping, remarks_ship, PurchaseTime)
            dbExecute(
              con,
              "INSERT INTO transactions (TransactionID, AccountType, TransactionType, Amount, Remarks, TransactionTime) VALUES (?, ?, ?, ?, ?)",
              params = list(
                transaction_id,
                "买货卡",
                "采购",
                -TotalDomesticShipping,
                remarks_ship,
                PurchaseTime
              )
            )
            list(transaction_id) # 返回记录的 ID
          } else {
            list(NULL) # 如果国内运费为 0，返回 NULL
          }
        )
      
      showNotification("核对后的采购开销与国内运费已登记到'买货卡（139）'！", type = "message")
      
      # 重新计算所有balance记录并刷新显示
      update_balance("买货卡", con)
      refreshTransactionTable("买货卡", cache_env, transaction_table_hash, output, con)
      
    }, error = function(e) {
      showNotification(paste0("更新失败!", e), type = "error")
    })
  })
  
  
  #################################################################
  
  # 库存总览数据统计
  overview_data <- reactive({
    process_data(unique_items_data())
  })
  
  # 输出卡片数据
  output$domestic_total_count <- renderText({ overview_data()$domestic$count })
  output$domestic_total_value <- renderText({ sprintf("¥%.2f", overview_data()$domestic$value) })
  output$domestic_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$domestic$shipping) })
  
  output$logistics_total_count <- renderText({ overview_data()$logistics$count })
  output$logistics_total_value <- renderText({ sprintf("¥%.2f", overview_data()$logistics$value) })
  output$logistics_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$logistics$shipping) })
  
  output$us_total_count <- renderText({ overview_data()$us$count })
  output$us_total_value <- renderText({ sprintf("¥%.2f", overview_data()$us$value) })
  output$us_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$us$shipping) })
  
  output$sold_total_count <- renderText({ overview_data()$sold$count })
  output$sold_total_count_with_shipping <- renderText({
    count <- overview_data()$sold$count
    us_shipping_count <- overview_data()$sold$us_shipping_count
    paste0(count, " (", us_shipping_count, ")")
  })
  output$sold_total_value <- renderText({ sprintf("¥%.2f", overview_data()$sold$value) })
  output$sold_shipping_cost <- renderText({ sprintf("¥%.2f", overview_data()$sold$shipping) })
  
  # 状态流转桑基图
  output$status_sankey <- renderSankeyNetwork({
    # 获取物品状态历史数据
    history_data <- dbGetQuery(con, "SELECT * FROM item_status_history")
    
    filtered_data <- history_data %>%
      arrange(UniqueID, change_time) %>%
      # 应用过滤规则
      group_by(UniqueID) %>%
      mutate(
        to_remove = FALSE,
        to_remove = ifelse(previous_status == "采购" & !is.na(lead(previous_status)) & lead(previous_status) != "国内入库", TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内入库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("国内出库", "国内售出"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内售出" & !is.na(lead(previous_status)) & lead(previous_status) != "美国发货", TRUE, to_remove),
        to_remove = ifelse(previous_status == "国内出库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("美国入库", "美国调货"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "美国入库" & !is.na(lead(previous_status)) & !lead(previous_status) %in% c("美国调货", "美国发货"), TRUE, to_remove),
        to_remove = ifelse(previous_status == "美国调货" & !is.na(lead(previous_status)) & lead(previous_status) != "美国发货", TRUE, to_remove)
      ) %>%
      filter(!to_remove) %>%
      select(-to_remove) %>%
      ungroup() %>%
      group_by(UniqueID, previous_status) %>%
      slice_min(previous_status_timestamp, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # 确保状态流转顺序正确
    links <- filtered_data %>%
      group_by(UniqueID) %>%
      arrange(change_time, .by_group = TRUE) %>%
      mutate(next_status = lead(previous_status)) %>%
      filter(!is.na(next_status)) %>%
      ungroup() %>%
      group_by(source = previous_status, target = next_status) %>%
      summarise(value = n(), .groups = "drop")
    
    links <- as.data.frame(links)
    
    # 定义状态颜色映射
    status_colors <- c(
      "采购" = "lightgray",
      "国内入库" = "#c7e89b",
      "国内售出" = "#9ca695",
      "国内出库" = "#46a80d",
      "美国入库" = "#6f52ff",
      "美国调货" = "#529aff",
      "美国发货" = "#faf0d4",
      "交易完毕" = "#f4c7fc"
    )
    
    # 定义节点
    nodes <- data.frame(name = unique(c(links$source, links$target)))
    
    # 映射 source 和 target 到节点索引
    links <- links %>%
      mutate(
        source = match(source, nodes$name) - 1,
        target = match(target, nodes$name) - 1
      )
    
    # 校验 links 和 nodes 是否有效
    if (nrow(links) == 0 || nrow(nodes) == 0) {
      showNotification("没有可用的状态流转数据，请检查数据源。", type = "error")
      return(NULL)
    }
    
    # 生成颜色映射 JS 代码
    color_js <- sprintf(
      "d3.scaleOrdinal().domain(%s).range(%s)",
      jsonlite::toJSON(names(status_colors), auto_unbox = TRUE),
      jsonlite::toJSON(status_colors, auto_unbox = TRUE)
    )
    
    # 渲染桑基图
    sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 14,
      nodeWidth = 20,
      nodePadding = 30,
      iterations = 5,
      sinksRight = TRUE,
      colourScale = color_js
    )
  })
  
  #################################################################
  
  # 清空sku输入框
  observeEvent(input$clear_query_sku_btn, {
    updateTextInput(session, "query_sku", value = "")
  })
  
  # 监听查询页选中inventory table (for SKU query and chart summary)
  observeEvent(input$filtered_inventory_table_query_rows_selected, {
    selected_row <- input$filtered_inventory_table_query_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      # 更新 SKU 输入框(生成库存图表用)
      updateTextInput(session, "query_sku", value = selected_data$SKU)
    }
  })
  
  # 监听用户点击图片列
  observeEvent(input$filtered_inventory_table_query_cell_clicked, {
    info <- input$filtered_inventory_table_query_cell_clicked
    
    # 检查是否点击了图片列（第三列）
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == 2) {  # 第三列在 R 中的索引是 2
        
        img_path <- as.character(filtered_inventory()[info$row, "ItemImagePath"])
        
        img_host_path <- paste0(host_url, "/images/", basename(img_path))
        
        # 弹出窗口显示大图
        showModal(modalDialog(
          title = "物品图片预览",
          tags$div(
            style = "overflow: auto; max-height: 700px; text-align: center;",
            tags$img(
              src = img_host_path,
              style = "max-width: 100%; height: auto; display: inline-block;"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 数据下载分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 动态生成供应商筛选器
  output$download_maker_ui <- renderUI({
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    
    createSearchableDropdown(
      input_id = "download_maker",
      label = NULL,
      data = makers,
      placeholder = "搜索供应商..."
    )
  })
  
  # 监听供应商选择变化并动态更新商品名称
  observe({
    req(unique_items_data())  # 确保数据存在
    
    # 获取用户选择的供应商
    selected_makers <- input$download_maker
    
    # 筛选商品名称
    if (!is.null(selected_makers) && length(selected_makers) > 0) {
      filtered_data <- unique_items_data() %>% filter(Maker %in% selected_makers)
    } else {
      filtered_data <- unique_items_data()
    }
    
    # 提取对应的商品名称，并在前面加一个空选项
    item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())
    
    # 更新商品名称选项，默认选中空选项
    updateSelectizeInput(session, "download_item_name", choices = item_names, selected = "", server = TRUE)
  })
  
  # 重置筛选逻辑
  observeEvent(input$download_reset_filters, {
    # 重置供应商筛选为全选
    makers <- unique_items_data() %>% pull(Maker) %>% unique()
    updateDropdown.shinyInput(
      session = session,
      inputId = "download_maker",
      options = lapply(makers, function(maker) list(key = maker, text = maker)), # 更新选项
      value = NULL # 重置为未选中状态
    )
    
    # 重置商品名称筛选为空选项
    updateSelectizeInput(session, "download_item_name", choices = "", selected = "", server = TRUE)
    updateDateRangeInput(session, "download_date_range", start = Sys.Date() - 365, end = Sys.Date() + 1)
  })
  
  # 下载物品汇总表为 Excel
  output$download_summary_xlsx <- downloadHandler(
    filename = function() {
      paste("物品汇总表（按采购日期）-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品汇总表")
      
      # 获取数据
      data <- filtered_unique_items_data_download()
      req(!is.null(data) && nrow(data) > 0)  # 确保数据非空
      
      data <- map_column_names(data, column_mapping = list(
        SKU = "条形码",
        ItemName = "商品名",
        ItemImagePath = "商品图",
        Maker = "供应商",
        MajorType = "大类",
        MinorType = "小类",
        ProductCost = "单价",
        DomesticShippingCost = "平摊运费",
        PurchaseTime = "采购日",
        Status = "库存态",
        Defect = "瑕疵态"
      ))
      
      # 按 SKU 计算全局库存统计
      sku_inventory_stats <- data %>%
        group_by(`条形码`) %>%
        summarize(
          总剩余库存数 = sum(`库存态` %in% c("国内入库", "国内出库", "美国入库")),
          国内库存数 = sum(`库存态` == "国内入库"),
          在途库存数 = sum(`库存态` == "国内出库"),
          美国库存数 = sum(`库存态` == "美国入库"),
          无瑕 = sum(`瑕疵态` == "无瑕"),
          瑕疵 = sum(`瑕疵态` == "瑕疵"),
          修复 = sum(`瑕疵态` == "修复"),
          .groups = "drop"
        )
      
      # 按条形码和采购日期分组，统计其他信息
      grouped_data <- data %>%
        group_by(`条形码`, `采购日`) %>%
        summarize(
          商品名 = first(`商品名`),
          商品图 = first(`商品图`),
          供应商 = first(`供应商`),
          大类 = first(`大类`),
          小类 = first(`小类`),
          批次单价 = mean(`单价`, na.rm = TRUE),
          批次平摊运费 = mean(`平摊运费`, na.rm = TRUE),
          批次采购数 = n(),  # 记录数
          .groups = "drop"
        )
      
      # 合并全局统计到分组数据
      final_data <- grouped_data %>%
        left_join(sku_inventory_stats, by = "条形码")
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品汇总表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "商品图")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品汇总表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品汇总表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品汇总表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning")
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品汇总表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品汇总表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message")
    }
  )
  
  # 下载物品明细表为 Excel
  output$download_details_xlsx <- downloadHandler(
    filename = function() {
      paste("物品明细表-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "Asia/Shanghai"), ".xlsx", sep = "")
    },
    content = function(file) {
      # 创建 Excel 文件
      wb <- createWorkbook()
      addWorksheet(wb, "物品明细表")
      
      # 获取数据
      final_data <- filtered_unique_items_data_download()
      
      n_col <- ncol(final_data)
      
      # 写入数据到 Excel
      writeData(wb, "物品明细表", final_data, startCol = 1, startRow = 1)
      
      # 图片插入的列号
      col_to_insert <- which(colnames(final_data) == "ItemImagePath")
      
      # 设置固定高度 1 inch，计算动态宽度
      image_height <- 1
      
      # 插入图片到 Excel
      for (i in seq_len(nrow(final_data))) {
        image_path <- as.character(final_data[i, col_to_insert])
        
        image_width_max <- 1
        
        if (!is.na(image_path) && file.exists(image_path)) {
          
          # 获取图片的实际宽高比
          dims <- get_image_dimensions(image_path)
          width_ratio <- dims$width / dims$height  # 宽高比
          
          row_to_insert <- i + 1  # 对应数据的行号
          
          image_width <- image_height * width_ratio  # 动态宽度（英寸）
          
          # 更新最大宽度
          image_width_max <- max(image_width_max, image_width)
          
          insertImage(
            wb = wb,
            sheet = "物品明细表",
            file = normalizePath(image_path),
            startRow = row_to_insert,
            startCol = col_to_insert,
            width = image_width,
            height = image_height,
            units = "in"
          )
          
          # 清空路径数据
          writeData(wb, "物品明细表", "", startCol = col_to_insert, startRow = i + 1)
          
          # 调整行高和列宽
          setRowHeights(wb, "物品明细表", rows = row_to_insert, heights = image_height * 78)
          
        } else {
          showNotification(paste("跳过不存在的图片:", image_path), type = "warning")
        }
      }
      
      # 最终设置列宽，保证所有图片适配最大宽度
      setColWidths(wb, "物品明细表", cols = col_to_insert, widths = image_width_max * 16)
      
      # 自动调整其他列的宽度
      setColWidths(wb, "物品明细表", cols = seq_len(n_col)[-col_to_insert], widths = "auto")
      
      # 保存 Excel 文件
      saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel 文件已成功下载", type = "message")
    }
  )
  
  #########################################################################################################################
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}
