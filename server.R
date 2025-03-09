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
  
  # Database
  con <- db_connection()
  
  # 初始化 requests_data 和 unique_items_data
  requests_data <- reactiveVal(NULL)
  unique_items_data <- reactiveVal(NULL)
  
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
      item_name_input_id = "purchase_filter-name"
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
  
  # 入库页过滤
  filtered_unique_items_data_inbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("采购", "国内入库"))
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "inbound_filter-maker",
      status_input_id = "inbound_filter-status",
      item_name_input_id = "inbound_filter-name",
      purchase_date_range_id = "inbound_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, Defect, 和 PurchaseTime 下的数量
    data <- data %>%
      group_by(SKU, Status, Defect, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, Defect, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 出库页过滤
  filtered_unique_items_data_outbound <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内入库", "国内出库"), Defect != "瑕疵")
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "outbound_filter-maker",
      status_input_id = "outbound_filter-status",
      item_name_input_id = "outbound_filter-name",
      purchase_date_range_id = "outbound_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # 售出-物品售出分页过滤
  filtered_unique_items_data_sold <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # 只显示本页相关状态
    data <- data %>%
      filter(Status %in% c("国内入库", "国内出库", "美国入库", "美国调货", "国内售出"), Defect != "瑕疵")

    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "sold_filter-maker",
      status_input_id = "sold_filter-status",
      item_name_input_id = "sold_filter-name",
      purchase_date_range_id = "sold_filter-purchase_date_range"
    )
    
    # 统计 SKU, Status, 和 PurchaseTime 下的数量（仅统计非瑕疵状态）
    data <- data %>%
      group_by(SKU, Status, PurchaseTime) %>%
      mutate(ItemCount = n()) %>%  # 条件统计数量
      ungroup()
    
    
    # 去重：仅保留每个 SKU 和组合的第一条记录
    data <- data %>%
      arrange(desc(updated_at)) %>%  # 按需求排序
      distinct(SKU, Status, PurchaseTime, .keep_all = TRUE)         # 去重，保留所有列
    
    data
  })
  
  # Define debounced input for the combined search field
  debounced_filter_combined <- debounce(
    reactive({ trimws(input$filter_combined) }),  # Trim whitespace from input
    millis = 500  # Set debounce delay to 500 milliseconds
  )
  
  filtered_orders <- reactive({
    req(orders())  # Ensure order data exists
    
    data <- orders()  # Get all order data
    
    # Combined filter logic with debouncing
    search_term <- debounced_filter_combined()
    if (!is.null(search_term) && length(search_term) > 0 && nzchar(search_term)) {
      # Filter across multiple fields using OR logic
      data <- data %>% filter(
        grepl(search_term, OrderID, ignore.case = TRUE) |
          grepl(search_term, UsTrackingNumber, ignore.case = TRUE) |
          grepl(search_term, CustomerName, ignore.case = TRUE) |
          grepl(search_term, CustomerNetName, ignore.case = TRUE) |
          grepl(search_term, OrderNotes, ignore.case = TRUE)
      )
      
      # Handle SKU and ItemName filtering using unique_items_data
      req(unique_items_data())
      sku_or_item_orders <- unique_items_data() %>%
        filter(
          grepl(search_term, SKU, ignore.case = TRUE) |
            grepl(search_term, ItemName, ignore.case = TRUE)
        ) %>%
        pull(OrderID) %>%
        unique()
      
      # Combine orders matching SKU or ItemName
      data <- data %>% filter(OrderID %in% sku_or_item_orders | 
                                grepl(search_term, OrderID, ignore.case = TRUE) |
                                grepl(search_term, UsTrackingNumber, ignore.case = TRUE) |
                                grepl(search_term, CustomerName, ignore.case = TRUE) |
                                grepl(search_term, CustomerNetName, ignore.case = TRUE) |
                                grepl(search_term, OrderNotes, ignore.case = TRUE))
    }
    
    # Filter by platform
    if (!is.null(input$filter_platform) && input$filter_platform != "") {
      data <- data %>% filter(Platform == input$filter_platform)
    }
    
    # Filter by order status
    if (!is.null(input$filter_order_status) && input$filter_order_status != "") {
      data <- data %>% filter(OrderStatus == input$filter_order_status)
    }
    
    # Filter by creation date
    if (!is.null(input$filter_order_date) && !is.null(input$filter_order_date[[1]]) && !is.null(input$filter_order_date[[2]])) {
      start_date <- input$filter_order_date[[1]]
      end_date <- input$filter_order_date[[2]]
      data <- data %>% filter(created_at >= start_date & created_at <= end_date)
    }
    
    # Sort by creation date in descending order
    data <- data %>% arrange(desc(created_at))
    
    data
  })
  
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
      purchase_date_range_id = "manage_filter-purchase_date_range"
    )
    
    data
  })
  
  # 瑕疵品管理页过滤
  filtered_unique_items_data_defect <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "defect_filter-maker",
      item_name_input_id = "defect_filter-name",
      purchase_date_range_id = "defect_filter-purchase_date_range"
    )
   
    # 默认过滤条件：状态为“国内入库”且 Defect 不为“未知”
    data <- data[!is.na(data$Defect) & data$Defect != "未知" & data$Status == "国内入库", ]

    # 处理开关互斥逻辑
    if (isTRUE(input$show_defects_only)) {
      # 如果仅显示瑕疵品
      data <- data[data$Defect == "瑕疵", ]
    } else if (isTRUE(input$show_perfects_only)) {
      # 如果仅显示无瑕品
      data <- data[data$Defect == "无瑕", ]
    }

    data
  })
  
  # 国际物流筛选
  filtered_unique_items_data_logistics <- reactive({
    req(unique_items_data())
    data <- unique_items_data()
    
    # # 只显示本页相关状态
    # data <- data %>%
    #   filter(Status %in% c("国内出库", "国内售出"), Defect != "瑕疵")

    data <- filter_unique_items_data_by_inputs(
      data = data,
      input = input,
      maker_input_id = "logistic_filter-maker",
      status_input_id = "logistic_filter-status",
      item_name_input_id = "logistic_filter-name",
      sold_date_range_id = "logistic_filter-sold_date_range",
      only_show_sold_id = "logistic_filter-only_show_sold",
      exit_date_range_id = "logistic_filter-exit_date_range",
      only_show_exit_id = "logistic_filter-only_show_exit"
    )
    
    shipping_method <- input$intl_shipping_method
    
    # 判断并根据物流方式筛选
    if (!is.null(shipping_method)) {
      data <- data %>% filter(IntlShippingMethod == shipping_method)
    }
    
    # 优先显示没有国际运单号的物品
    data <- data %>% arrange(desc(is.na(IntlTracking)), IntlTracking)

    data
  })
  
  # 查询页过滤-库存表
  filtered_inventory <- reactive({
    req(inventory(), unique_items_data()) # 确保数据存在
    
    result <- inventory()
    
    # 如果库存为空，返回空库存表
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    # 供应商筛选
    if (!is.null(input[["query_filter-maker"]]) && length(input[["query_filter-maker"]]) > 0 && any(input[["query_filter-maker"]] != "")) {
      result <- result %>% filter(Maker %in% input[["query_filter-maker"]])
    }
    
    # 商品名称模糊筛选
    if (!is.null(input[["query_filter-name"]]) && input[["query_filter-name"]] != "") {
      result <- result %>% filter(grepl(input[["query_filter-name"]], ItemName, ignore.case = TRUE))
    }
    
    # 根据售罄筛选
    if (!is.null(input$query_stock_status) && input$query_stock_status != "none") {
      if (input$query_stock_status == "us") {
        result <- result %>% filter(UsQuantity == 0 & DomesticQuantity > 0)  # 美国库存为 0
      } else if (input$query_stock_status == "domestic") {
        result <- result %>% filter(DomesticQuantity == 0 & UsQuantity > 0)  # 国内库存为 0
      } else if (input$query_stock_status == "all") {
        result <- result %>% filter(Quantity == 0)  # 全库存售罄
      }
    }
    
    result <- result[order(result$updated_at, decreasing = TRUE), ]
    return(result)
  })
  
  # 下载页过滤
  filtered_unique_items_data_download <- reactive({
    filter_unique_items_data_by_inputs(
      data = unique_items_data(),
      input = input,
      maker_input_id = "download_maker",
      item_name_input_id = "download_item_name",
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
  
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- c(common_columns, list(
                                                          PurchaseTime = "采购日",
                                                          DomesticEntryTime = "入库日",
                                                          Defect = "瑕疵态",
                                                          ItemCount = "数量")
                                                        ), selection = "single", data = filtered_unique_items_data_inbound)
  
  unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", 
                                                         column_mapping <- c(common_columns, list(
                                                           PurchaseTime = "采购日",
                                                           DomesticExitTime = "出库日",
                                                           ItemCount = "数量")
                                                         ), selection = "single", data = filtered_unique_items_data_outbound)
  
  unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold",
                                                     column_mapping <- c(common_columns, list(
                                                       PurchaseTime = "采购日",
                                                       DomesticSoldTime = "售出日",
                                                       ItemCount = "数量")
                                                     ), selection = "single", data = filtered_unique_items_data_sold)
  
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
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping <- c(common_columns, list(
                                                         PurchaseTime = "采购日",
                                                         DomesticEntryTime = "入库日",
                                                         Defect = "瑕疵态",
                                                         DefectNotes = "瑕疵备注")
                                                       ), selection = "multiple", data = filtered_unique_items_data_defect,
                                                       option = modifyList(table_default_options, list(scrollY = "730px", searching = TRUE)))
  
  unique_items_table_logistics_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_logistics",
                                                          column_mapping = c(common_columns, list(
                                                            IntlShippingMethod = "国际运输",
                                                            DomesticSoldTime = "售出日",
                                                            DomesticExitTime = "出库日",
                                                            IntlShippingCost = "国际运费",
                                                            IntlTracking = "国际运单"
                                                          )), selection = "multiple",
                                                          data = filtered_unique_items_data_logistics,
                                                          option = modifyList(table_default_options, list(scrollY = "730px", 
                                                                                                          searching = FALSE, 
                                                                                                          paging = TRUE,
                                                                                                          pageLength = 30,
                                                                                                          lengthMenu = c(30, 100, 200, 500, 1000),
                                                                                                          dom = 'lftip')))
  
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
  
  # 订单管理分页订单表
  selected_order_row <- callModule(orderTableServer, "orders_table_module",
                                   column_mapping = list(
                                     OrderID = "订单号",
                                     OrderImagePath = "订单图",
                                     CustomerName = "姓名",
                                     CustomerNetName = "网名",
                                     Platform = "平台",
                                     TransactionAmount = "成交额",
                                     UsTrackingNumber = "运单号",
                                     LabelStatus = "运单PDF",
                                     OrderStatus = "状态",
                                     OrderNotes = "备注",
                                     created_at = "创建时间"
                                   ),
                                   data = filtered_orders,  # 数据源
                                   selection = "single" # 单选模式
  )
  
  ####################################################################################################################################
  
  observeEvent(input$refresh_item_table, {
    isolate({
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())  # 触发数据刷新
      orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
      refreshTransactionTable("买货卡", cache_env, transaction_table_hash, output, con)
      refreshTransactionTable("工资卡", cache_env, transaction_table_hash, output, con)
      refreshTransactionTable("美元卡", cache_env, transaction_table_hash, output, con)
      refreshTransactionTable("一般户卡", cache_env, transaction_table_hash, output, con)
    })
  })
  
  ####################################################################################################################################
  
  
  
  ################################################################
  ##                                                            ##
  ## 协作分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 渲染初始供应商筛选器（只定义一次）
  output$supplier_filter <- renderUI({
    selectizeInput(
      inputId = "selected_supplier",
      label = NULL,
      choices = NULL,  # 初始为空，动态更新
      selected = NULL, # 初始无选择
      options = list(
        placeholder = "筛选供应商...",
        searchField = "value",
        maxOptions = 1000,
        create = TRUE,
        allowEmptyOption = TRUE
      )
    )
  })
  
  # 动态更新筛选器选项
  observe({
    # 确保 input$collaboration_tabs 存在
    req(input$collaboration_tabs)
    current_value <- input$collaboration_tabs
    
    # 映射 tab value 到 RequestType
    tab_value_to_request_type <- list(
      "purchase" = "采购",
      "arranged" = "安排",
      "completed" = "完成",
      "outbound" = "出库",
      "new_product" = "新品"
    )
    
    # 检查 current_value 是否有效，若无效则使用默认值
    request_type <- if (current_value %in% names(tab_value_to_request_type)) {
      tab_value_to_request_type[[current_value]] %||% "采购"
    } else {
      "采购"
    }
    
    req(requests_data())
    
    current_requests <- requests_data() %>% filter(RequestType == request_type)
    suppliers <- unique(current_requests$Maker)
    
    # 获取当前选择
    current_selection <- isolate(input$selected_supplier)
    
    # 更新选项，但避免不必要的重新选择
    updateSelectizeInput(
      session,
      inputId = "selected_supplier",
      choices = c("全部供应商", suppliers),
      selected = if (is.null(current_selection) || !current_selection %in% c("全部供应商", suppliers)) NULL else current_selection,
      options = list(
        placeholder = "筛选供应商...",
        create = TRUE,
        allowEmptyOption = TRUE
      )
    )
  }, priority = 10)  # 提高优先级，确保先于其他观察者执行
  
  # 重置按钮逻辑
  observeEvent(input$reset_supplier, {
    updateSelectizeInput(
      session,
      "selected_supplier",
      selected = "全部供应商"
    )
  }, priority = 0)  # 较低优先级，避免干扰选项更新
  
  # 定期检查数据库更新
  poll_requests <- reactivePoll(
    intervalMillis = 20000,
    session = session,
    checkFunc = function() {
      last_updated <- dbGetQuery(con, "SELECT MAX(UpdatedAt) AS last_updated FROM requests")$last_updated[1]
      if (is.null(last_updated)) Sys.time() else last_updated
    },
    valueFunc = function() {
      dbGetQuery(con, "SELECT * FROM requests")
    }
  )
  
  # 使用 debounce 限制轮询频率
  poll_requests_debounced <- debounce(poll_requests, millis = 20000)
  
  observeEvent(poll_requests_debounced(), {
    requests <- poll_requests_debounced()
    requests_data(requests)
    # 确保 input$selected_supplier 已定义
    req(input$selected_supplier)
    refresh_board_incremental(requests, output, input)
  }, priority = 10)
  
  # 初始化时绑定所有按钮
  observeEvent(requests_data(), {
    requests <- requests_data()
    lapply(requests$RequestID, function(request_id) {
      bind_buttons(request_id, requests_data, input, output, session, con)
    })
  }, ignoreInit = FALSE, once = TRUE)
  
  # 使用 observe 监听 requests_data() 和 input$selected_supplier
  observe({
    req(requests_data(), input$selected_supplier)
    requests <- requests_data()
    refresh_board_incremental(requests, output, input, page_size = 30)  # 设置每页大小
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_sku, {
    # 如果 SKU 搜索框有值，则清空物品名称搜索框
    if (input$search_sku != "") {
      updateTextInput(session, "search_name", value = "")  # 清空物品名称搜索框
    }
  })
  
  # SKU 和物品名输入互斥逻辑
  observeEvent(input$search_name, {
    # 如果物品名称搜索框有值，则清空 SKU 搜索框
    if (input$search_name != "") {
      updateTextInput(session, "search_sku", value = "")  # 清空 SKU 搜索框
    }
  })
  
  # SKU 和物品名称搜索预览
  observeEvent(c(input$search_sku, input$search_name), {
    # 如果两个输入框都为空，则清空预览
    if (input$search_sku == "" && input$search_name == "") {
      output$item_preview <- renderUI({ NULL })
      return()  # 结束逻辑
    }
    
    req(input$search_sku != "" | input$search_name != "")  # 确保至少一个搜索条件不为空
    
    # 获取清理后的输入值
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 使用 unique_items_data() 进行过滤和统计
    result <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      group_by(SKU, ItemName, Maker, ItemImagePath) %>%
      summarise(
        DomesticStock = sum(Status == "国内入库", na.rm = TRUE),  # 国内库存
        InTransitStock = sum(Status == "国内出库", na.rm = TRUE),  # 在途库存
        UsStock = sum(Status == "美国入库", na.rm = TRUE),  # 美国库存
        .groups = "drop"
      )
    
    # 动态更新预览界面
    if (nrow(result) > 0) {
      output$item_preview <- renderUI({
        div(
          style = "max-height: 320px; overflow-y: auto; padding: 10px; border: 1px solid #e0e0e0; border-radius: 8px; background-color: #f9f9f9;",
          lapply(1:nrow(result), function(i) {
            item <- result[i, ]
            img_path <- ifelse(
              is.na(item$ItemImagePath),
              placeholder_150px_path,  # 占位符路径
              paste0(host_url, "/images/", basename(item$ItemImagePath))  # 构建完整路径
            )
            div(
              style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #ccc;",
              tags$img(src = img_path, height = "150px", style = "display: block; margin: auto;"),
              tags$h5(item$ItemName, style = "text-align: center; margin-top: 10px;"),
              tags$h5(item$Maker, style = "text-align: center; margin-top: 10px;"),
              tags$h5(item$SKU, style = "text-align: center; margin-top: 10px;"),
              div(
                style = "text-align: center; font-size: 12px;",
                tags$span(paste("国内库存:", item$DomesticStock), style = "margin-right: 10px;"),
                tags$span(paste("在途库存:", item$InTransitStock), style = "margin-right: 10px;"),
                tags$span(paste("美国库存:", item$UsStock))
              )
            )
          })
        )
      })
    } else {
      output$item_preview <- renderUI({
        div(tags$p("未找到匹配的物品", style = "color: red; text-align: center;"))
      })
    }
  })
  
  # 库存品请求按钮
  observeEvent(input$add_request, {
    req(input$request_quantity > 0)  # 确保输入合法
    
    # 获取用户输入
    search_sku <- trimws(input$search_sku)
    search_name <- trimws(input$search_name)
    
    # 检索数据并插入到数据库
    filtered_data <- unique_items_data() %>%
      filter(
        (SKU == search_sku & search_sku != "") |  # SKU 精准匹配
          (grepl(search_name, ItemName, ignore.case = TRUE) & search_name != "")  # 名称模糊匹配
      ) %>%
      distinct(SKU, Maker, ItemName, ItemImagePath)  # 去重
    
    tryCatch({
      # 主逻辑
      if (nrow(filtered_data) == 1) {
        request_id <- uuid::UUIDgenerate()
        
        item_image_path <- ifelse(is.na(filtered_data$ItemImagePath[1]), placeholder_150px_path, filtered_data$ItemImagePath[1])
        item_description <- ifelse(is.na(filtered_data$ItemName[1]), "未知", filtered_data$ItemName[1])
        
        dbExecute(con, 
                  "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
         VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')", 
                  params = list(request_id, filtered_data$SKU, filtered_data$Maker, item_image_path, item_description, 
                                input$request_quantity, format_remark(input$request_remark, system_type)))
        
        bind_buttons(request_id, requests_data, input, output, session, con)
        
        updateTextInput(session, "search_sku", value = "")
        updateTextInput(session, "search_name", value = "")
        updateNumericInput(session, "request_quantity", value = 1)
        updateTextAreaInput(session, "request_remark", value = "")
        
        showNotification("请求已成功创建", type = "message")
      } else if (nrow(filtered_data) > 1) {
        showNotification("搜索结果不唯一，请更精确地搜索 SKU 或物品名称", type = "error")
      } else {
        showNotification("未找到匹配的物品，请检查搜索条件", type = "error")
      }
      # 手动刷新
      refresh_board_incremental(dbGetQuery(con, "SELECT * FROM requests"), output, input)
    }, error = function(e) {
      # 捕获错误并打印详细信息
      showNotification(e, type = "error")
    })
  })
  
  # 初始化图片上传模块
  image_requests <- imageModuleServer("image_requests")
  
  # 新商品采购请求按钮
  observeEvent(input$submit_custom_request, {
    # 确保必要字段已填写
    req(input$custom_quantity > 0)
    
    # 获取用户输入
    custom_description <- trimws(input$custom_description)
    custom_quantity <- input$custom_quantity
    
    # 使用图片上传模块的返回数据
    custom_image_path <- process_image_upload(
      sku = "New-Request",  # 自定义物品没有 SKU，可以设置为固定值或动态生成
      file_data = image_requests$uploaded_file(),
      pasted_data = image_requests$pasted_file()
    )
    
    # 检查图片路径是否有效
    req(!is.null(custom_image_path) && !is.na(custom_image_path))
    
    # 生成唯一 RequestID
    request_id <- uuid::UUIDgenerate()
    
    # 将数据插入到数据库
    dbExecute(con, 
              "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType) 
             VALUES (?, ?, '待定', ?, ?, ?, '待处理', ?, '新品')", 
              params = list(request_id, "New-Request", custom_image_path, custom_description, custom_quantity, format_remark(input$custom_remark, system_type)))
    
    bind_buttons(request_id, requests_data, input, output, session, con)  
    
    # 清空输入字段
    updateTextInput(session, "custom_description", value = "")
    updateNumericInput(session, "custom_quantity", value = 1)
    updateTextAreaInput(session, "custom_remark", value = "")
    image_requests$reset()
    showNotification("自定义请求已成功提交", type = "message")
    # 手动刷新
    refresh_board_incremental(dbGetQuery(con, "SELECT * FROM requests"), output, input)
  })
  
  # 点击请求图片看大图
  observeEvent(input$view_request_image, {
    showModal(modalDialog(
      title = "请求物品图片",
      div(
        style = "overflow: auto; max-height: 700px; text-align: center;",        
        tags$img(src = input$view_request_image, style = "max-width: 100%; height: auto; display: inline-block;")
      ),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # 鼠标悬停请求图片显示库存分布
  output$colab_inventory_status_chart <- renderPlotly({
    req(input$hover_sku, input$hover_sku != "New-Request")  # 直接跳过 "New-Request"
    
    tryCatch({
      data <- unique_items_data()
      
      inventory_status_data <- data %>%
        filter(SKU == isolate(input$hover_sku)) %>%
        group_by(Status) %>%
        summarise(Count = n(), .groups = "drop")
      
      if (nrow(inventory_status_data) == 0) {
        return(NULL)
      }
      
      # 确保所有状态都存在，并填充 0
      inventory_status_data <- data.frame(Status = status_levels) %>%
        left_join(inventory_status_data, by = "Status") %>%
        mutate(Count = replace_na(Count, 0))
      
      # 过滤掉数量为 0 的状态
      inventory_status_data <- inventory_status_data %>% filter(Count > 0)
      
      # 重新匹配颜色：只取 **inventory_status_data$Status** 里有的颜色
      matched_colors <- status_colors[match(inventory_status_data$Status, status_levels)]
      
      plot_ly(
        data = inventory_status_data,
        labels = ~Status,
        values = ~Count,
        type = "pie",
        textinfo = "label+value",
        marker = list(colors = matched_colors)
      ) %>%
        layout(showlegend = FALSE, margin = list(l = 5, r = 5, t = 5, b = 5))
    }, error = function(e) {
      showNotification("库存状态图表生成错误", type = "error")
      return(NULL)
    })
  })
  
  outputOptions(output, "colab_inventory_status_chart", suspendWhenHidden = FALSE)
  
  # 自动转换 RequestType
  observe({
    invalidateLater(10000, session)
    
    dbWithTransaction(con, {
      # "安排" -> "完成"
      dbExecute(con, "
      UPDATE requests r
      JOIN (
        SELECT SKU, COUNT(*) AS procure_count
        FROM unique_items
        WHERE Status = '采购'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      SET r.RequestType = '完成', r.UpdatedAt = NOW()
      WHERE r.RequestType = '安排' AND u.procure_count >= r.Quantity
    ")
      
      # "完成" -> "出库"
      dbExecute(con, "
      UPDATE requests r
      JOIN (
        SELECT SKU, COUNT(*) AS domestic_count
        FROM unique_items
        WHERE Status = '国内入库'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      SET r.RequestType = '出库', r.RequestStatus = '已完成', r.UpdatedAt = NOW()
      WHERE r.RequestType = '完成' AND u.domestic_count >= r.Quantity
    ")
      
      # "出库" -> 删除
      dbExecute(con, "
      DELETE r FROM requests r
      JOIN (
        SELECT SKU, COUNT(*) AS transit_count
        FROM unique_items
        WHERE Status = '国内出库'
        GROUP BY SKU
      ) u ON r.SKU = u.SKU
      WHERE r.RequestType = '出库' AND u.transit_count >= r.Quantity
    ")
      
      # 更新 requests_data
      requests_data(dbGetQuery(con, "SELECT * FROM requests"))
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 采购分页                                                   ##
  ##                                                            ##
  ################################################################
  
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
  
  # 采购商品添加表（临时）
  added_items <- reactiveVal(create_empty_inventory())
  
  # Render added items table
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
      options = list(fixedHeader = TRUE,  # 启用表头固定
                     dom = 't',  # 隐藏搜索框和分页等控件
                     paging = FALSE,  # 禁止分页
                     searching = FALSE  # 禁止搜索
      )
    )$datatable
  })
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    # 验证输入
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
    
    # 检查是否存在该 SKU 的库存记录
    inventory_item <- tryCatch({
      dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", params = list(input$new_sku))
    }, error = function(e) {
      showNotification("检查库存时发生错误！", type = "error")
      return(data.frame())
    })
    
    existing_inventory_path <- if (nrow(inventory_item) > 0) inventory_item$ItemImagePath[1] else NULL
    
    # 上传或粘贴图片处理
    new_image_path <- process_image_upload(
      sku = input$new_sku,
      file_data = image_purchase$uploaded_file(),
      pasted_data = image_purchase$pasted_file(),
      inventory_path = existing_inventory_path
    )
    
    # 添加或更新记录
    existing_items <- added_items()
    existing_skus <- existing_items$SKU
    if (input$new_sku %in% existing_skus) {
      sku_index <- which(existing_skus == input$new_sku)
      current_image_path <- existing_items$ItemImagePath[sku_index]
      final_image_path <- if (!is.na(new_image_path) && new_image_path != "") {
        new_image_path
      } else {
        current_image_path
      }
      
      existing_items[sku_index, "SKU"] <- input$new_sku
      existing_items[sku_index, "Maker"] <- input$new_maker
      existing_items[sku_index, "MajorType"] <- input[["type_module-new_major_type"]]
      existing_items[sku_index, "ItemName"] <- input[["purchase-item_name"]]
      existing_items[sku_index, "Quantity"] <- input$new_quantity
      existing_items[sku_index, "ProductCost"] <- round(input$new_product_cost, 2)
      existing_items[sku_index, "ItemImagePath"] <- as.character(final_image_path)
      
      added_items(existing_items)
      
      showNotification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # 添加新记录
      new_item <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = "",
        ItemName = input[["purchase-item_name"]],
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = new_image_path,
        stringsAsFactors = FALSE
      )
      added_items(bind_rows(existing_items, new_item))
      showNotification(paste("SKU 已添加:", input$new_sku, "商品名:", input[["purchase-item_name"]]), type = "message")
    }
    
    # 重置
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
  
  # Confirm button: Update database and handle images
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
      
      # 计算运费
      total_shipping_cost <- input$new_shipping_cost
      if (is.null(total_shipping_cost) || total_shipping_cost <= 0) {
        total_shipping_cost <- 0
      }
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
      
      if (is.null(batch_data) || nrow(batch_data) == 0) {
        dbRollback(con)
        showNotification("采购数据无效，请检查输入！", type = "error")
        return()
      }
      
      # 转换为数据框并命名列
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
      colnames(batch_data) <- c("UniqueID", "SKU", "ProductCost", "DomesticShippingCost", 
                                "Status", "Defect", "PurchaseTime")
      
      # 批量插入 unique_items
      tryCatch({
        dbWriteTable(con, "unique_items", batch_data, append = TRUE, row.names = FALSE)
        dbCommit(con)
        showNotification("所有采购货物已成功登记！", type = "message")
      }, error = function(e) {
        dbRollback(con)
        showNotification(paste("采购登记失败:", e$message), type = "error")
        return()
      })
      
      # 重置输入字段
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_product_cost", value = 0)
      updateNumericInput(session, "new_shipping_cost", value = 0)
      updateTextInput(session, "purchase-item_name", value = "")
      image_purchase$reset()
      added_items(create_empty_inventory())
      
    }, error = function(e) {
      dbRollback(con)  # 确保外层错误也能回滚
      showNotification(paste("采购登记发生错误:", e$message), type = "error")
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
  
  # 确认删除逻辑
  observeEvent(input$confirm_delete_selected, {
    removeModal()  # 关闭确认弹窗
    
    selected_row <- input$added_items_table_rows_selected
    
    tryCatch({
      if (length(selected_row) > 0) {
        # 执行删除逻辑
        current_items <- added_items()
        updated_items <- current_items[-selected_row, ]  # 删除选中行
        added_items(updated_items)  # 更新 reactive 值
        
        # 通知用户
        showNotification("选中的记录已成功删除", type = "message")
      } else {
        showNotification("请选择要删除的记录", type = "error")
      }
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("删除失败：", e$message), type = "error")
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
  
  
  
  ################################################################
  ##                                                            ##
  ## 入库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听标签页切换事件
  observeEvent(input$inventory_cn, {
    if (input$inventory_cn == "入库") {
      runjs("document.getElementById('inbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "inbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 创建全局变量存储 预订单的 order_id 和 unique_id
  preorder_info <- reactiveValues(order_id = NULL, item_name = NULL, unique_id = NULL)
  
  # SKU 清除
  observeEvent(input$clear_inbound_sku, {
    updateTextInput(session, "inbound_sku", value = "")
  })
  
  # 监听 SKU 输入
  observeEvent(input$inbound_sku, {
    req(input$inbound_sku)
    
    # 调用 handleSkuInput 并获取待入库数量
    pending_quantity <- handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
    
    # 如果启用自动入库功能，直接执行入库逻辑
    if (input$auto_inbound) {
      req(input$inbound_sku)
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = NULL,    
        con,                  
        input, output, session
      )
      
      if (!is.null(result)) {
        preorder_info$item_name <- result$item_name
        preorder_info$unique_id <- result$unique_id  # 存储 unique_id
        
        if (input$speak_inbound_item_name) {  # 只有勾选“念出商品名”才朗读
          speak_text(preorder_info$item_name)
        } else {
          runjs("playSuccessSound()")  # 播放成功音效
        }
        
        orders_data <- dbGetQuery(con, "SELECT OrderID, OrderImagePath, OrderNotes, created_at FROM orders WHERE OrderStatus = '预定'")
        
        # 处理预定物品数据
        orders_data <- orders_data %>%
          mutate(PreorderItems = stri_match_first_regex(OrderNotes, "【预定物品】(.*?)；")[,2]) %>%
          filter(!is.na(PreorderItems)) %>%
          mutate(ItemList = stri_split_fixed(PreorderItems, "，")) %>%
          select(OrderID, OrderImagePath, OrderNotes, created_at, ItemList) %>%
          tidyr::unnest(ItemList)
        
        # 查找完全匹配的预订单
        matched_order <- orders_data %>%
          filter(ItemList == preorder_info$item_name) %>%
          arrange(created_at) %>%
          slice_head(n = 1)
        
        if (nrow(matched_order) > 0) {
          preorder_info$order_id <- matched_order$OrderID[1]  # 存储 order_id
          order_img_path <- ifelse(
            is.na(matched_order$OrderImagePath[1]) || matched_order$OrderImagePath[1] == "",
            placeholder_300px_path,
            paste0(host_url, "/images/", basename(matched_order$OrderImagePath[1]))
          )
          order_notes <- matched_order$OrderNotes[1]
          
          # **确保 `preorder_info$item_name` 只匹配以 `，` 或 `；` 结尾的完整项**
          pattern <- paste0("(】|，)(", preorder_info$item_name, ")(，|；)")
          highlighted_notes <- gsub(pattern, paste0("\\1<mark>\\2</mark>\\3"), order_notes, perl = TRUE)
          
          # 弹出确认对话框
          showModal(modalDialog(
            title = "预订单匹配",
            div(
              # 提示导语，使用强调样式
              tags$p("该商品已被如下预订单预定，是否直接做售出操作？", style = "font-weight: bold; color: #d9534f; text-align: center;"),
              div(
                tags$img(src = order_img_path, height = "300px", style = "display: block; margin: 10px auto; border-radius: 8px;")
              ),
              div(
                tags$p(HTML(paste("<strong>订单号:</strong>", preorder_info$order_id)), style = "margin-top: 10px;"),
                tags$p(HTML(paste("<strong>备注:</strong>", highlighted_notes)), style = "white-space: pre-wrap;")
              ),
              style = "text-align: left;"
            ),
            footer = tagList(
              modalButton("取消"),
              actionButton("confirm_bind_preorder", "确认预定品售出", class = "btn btn-primary")
            ),
            easyClose = FALSE  # 防止用户误触关闭
          ))
        } 
      } else {
        runjs("playErrorSound()")  # 播放失败音效
        updateTextInput(session, "inbound_sku", value = "")
        runjs("document.getElementById('inbound_sku').focus();")
        return()
      }
      
      # 清空 SKU 输入框
      updateTextInput(session, "inbound_sku", value = "")
      runjs("document.getElementById('inbound_sku').focus();")
    } else {
      # 未启用自动入库时更新待入库数量最大值
      if (!is.null(pending_quantity) && pending_quantity > 0) {
        updateNumericInput(session, "inbound_quantity", max = pending_quantity, value = 1)
        showNotification(paste0("已更新待入库数量最大值为 ", pending_quantity, "！"), type = "message")
      } else {
        updateNumericInput(session, "inbound_quantity", max = 0, value = 0)
      }
    }
  })
  
  # 监听 预订单"确认登记" 按钮，**确保只绑定一次**
  observeEvent(input$confirm_bind_preorder, {
    req(preorder_info$order_id, preorder_info$unique_id)  # 确保数据有效
    removeModal()  # 关闭 `预订单匹配`
    
    # 获取当前订单备注
    current_notes <- dbGetQuery(con, paste0(
      "SELECT OrderNotes FROM orders WHERE OrderID = '", preorder_info$order_id, "'"
    ))$OrderNotes
    
    if (!is.null(current_notes) && nchar(current_notes) > 0) {
      # **删除 `OrderNotes` 里匹配的 `item_name`**
      updated_notes <- remove_preorder_item_note(current_notes, preorder_info$item_name)
        
      # **如果 `updated_notes` 仅剩 `"【预定物品】；"`，改为 `"【预定物品登记完毕】"`**
      updated_notes <- sub("【预定物品】；", "【预定物品登记完毕】", updated_notes, fixed = TRUE)
      
      # **更新 `OrderNotes`**
      dbExecute(con, 
                "UPDATE orders SET OrderNotes = ? WHERE OrderID = ?", 
                params = list(updated_notes, preorder_info$order_id)
      )
      
      orders_refresh_trigger(!orders_refresh_trigger())
    }
    
    # 更新该物品的 `OrderID` 并修改 `Status`
    dbExecute(con, paste0(
      "UPDATE unique_items SET OrderID = '", preorder_info$order_id, "', Status = '国内售出'
     WHERE UniqueID = '", preorder_info$unique_id, "'"
    ))
    showNotification(paste0("物品已成功登记到预定单 ", preorder_info$order_id, "！"), type = "message")
    updateTabsetPanel(session, "inventory_cn", selected = "售出") # 跳转到“发货”页面
    # **延迟执行，确保 UI 加载完成后再切换子分页**
    delay(300, {
      updateTabsetPanel(session, "sold_tabs", selected = "订单管理")
    })    
    updateTextInput(session, "filter_order_id", value = preorder_info$order_id)
  }, ignoreInit = TRUE)  # **确保 `observeEvent` 只执行一次**
  
  # 手动确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    # 从输入中获取入库数量，确保为正整数
    inbound_quantity <- as.integer(input$inbound_quantity)
    if (is.na(inbound_quantity) || inbound_quantity <= 0) {
      showNotification("入库数量必须是一个正整数！", type = "error")
      runjs("playErrorSound()")  # 播放失败音效
      return()
    }
    
    # 批量处理入库逻辑
    for (i in seq_len(inbound_quantity)) {
      result <- handleOperation(
        unique_items_data(),
        operation_name = "入库", 
        sku_field = "inbound_sku",
        output_name = "inbound_item_info",
        query_status = "采购",
        update_status_value = "国内入库",
        count_label = "待入库数", 
        count_field = "PendingQuantity", 
        refresh_trigger = unique_items_data_refresh_trigger,      
        con,                  
        input, output, session
      )
      
      # 检查是否启用了瑕疵品选项
      defective_item <- input$defective_item
      defect_notes <- trimws(input$defective_notes)
      
      if (defective_item && defect_notes != "") {
        tryCatch({
          add_defective_note(
            con = con,
            unique_id = unique_ID,
            note_content = defect_notes,
            status_label = "瑕疵",
            refresh_trigger = unique_items_data_refresh_trigger
          )
          showNotification("瑕疵品备注已成功添加！", type = "message")
        }, error = function(e) {
          showNotification(paste("添加备注时发生错误：", e$message), type = "error")
        })
      } else if (defective_item) {
        showNotification("无瑕疵品备注！", type = "warning")
      }
    }

    # 重置输入
    updateTextInput(session, "inbound_sku", value = "")
    updateNumericInput(session, "inbound_quantity", value = 1)
    
    runjs("document.getElementById('inbound_sku').focus();")
  })
  
  
  # # 监听选中行并更新 SKU: 禁用
  # observeEvent(unique_items_table_inbound_selected_row(), {
  #   selected_row <- unique_items_table_inbound_selected_row()
  #   if (length(selected_row) > 0) {
  #     # 仅处理最后一个选择的行
  #     last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
  #     selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
  #     updateTextInput(session, "inbound_sku", value = selected_sku)
  #   }
  # })
  
  # 监听选中行并显示大图与物品信息
  observeEvent(unique_items_table_inbound_selected_row(), {
    selected_row <- unique_items_table_inbound_selected_row()
    if (length(selected_row) > 0) {
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_sku <- filtered_unique_items_data_inbound()[last_selected, "SKU", drop = TRUE]
      handleSkuInput(
        sku_input = selected_sku,
        output_name = "inbound_item_info",
        count_label = "待入库数",
        count_field = "PendingQuantity",
        con = con,
        output = output,
        placeholder_path = placeholder_300px_path,
        host_url = host_url
      )    
    }
  })
  
  # 控制备注输入框显示/隐藏
  observeEvent(input$defective_item, {
    if (input$defective_item) {
      show("defective_notes_container")
    } else {
      hide("defective_notes_container")
      updateTextInput(session, "defective_notes", value = "") # 清空备注
    }
  })
  
  # 生成并下载条形码 PDF
  output$download_barcode_pdf <- downloadHandler(
    filename = function() {
      selected_row <- unique_items_table_inbound_selected_row()
      selected_item <- filtered_unique_items_data_inbound()[selected_row, ]
      sku <- selected_item$SKU
      paste0(sku, "_barcode.pdf")
    },
    content = function(file) {
      selected_row <- unique_items_table_inbound_selected_row()
      selected_item <- filtered_unique_items_data_inbound()[selected_row, ]
      sku <- selected_item$SKU

      # 使用 reactive 计算的待入库数量
      skus_to_print <- rep(sku, times = selected_item$ItemCount)
      
      tryCatch({
        temp_base <- tempfile()
        
        temp_pdf <- export_barcode_pdf(
          sku = skus_to_print,
          page_width = page_width,
          page_height = page_height,
          unit = size_unit,
          output_file = temp_base
        )
        
        # 将临时文件复制到 Shiny 的下载路径
        file.copy(temp_pdf, file, overwrite = TRUE)
        
        showNotification(
          paste("条形码 PDF 已生成并下载！共", length(skus_to_print), "个条形码"),
          type = "message"
        )
        
        # 清理临时文件
        unlink(temp_pdf)
      }, error = function(e) {
        showNotification(paste("生成或下载条形码失败：", e$message), type = "error")
        unlink(temp_pdf)  # 清理失败时的临时文件
      })
    }
  )
  
  
  
  ################################################################
  ##                                                            ##
  ## 出库分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听标签页切换事件
  observeEvent(input$inventory_cn, {
    if (input$inventory_cn == "出库") {
      runjs("document.getElementById('outbound_sku').focus();")
    }
  })
  
  # 物品表过滤模块
  itemFilterServer(
    id = "outbound_filter",
    makers_items_map = makers_items_map
  )
  
  # 监听出库 SKU 输入
  observeEvent(input$outbound_sku, {
    req(input$outbound_sku)
    handleSkuInput(
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # SKU 清除
  observeEvent(input$clear_outbound_sku, {
    updateTextInput(session, "outbound_sku", value = "")
  })
  
  # 自动出库逻辑
  observeEvent(input$outbound_sku, {
    req(input$auto_outbound)  # 仅在自动出库勾选时触发
    req(input$outbound_sku)   # 确保 SKU 输入框不为空
    
    # 调用出库处理逻辑
    result <- handleOperation(
      unique_items_data(),
      operation_name = "出库", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = NULL,     
      con,                  
      input, output, session
    )

    if (!is.null(result)) {
      if (input$speak_outbound_item_name) {  # 只有勾选“念出商品名”才朗读
        speak_text(result$item_name)
      } else {
        runjs("playSuccessSound()")  # 播放成功音效
      }
    } else {
      runjs("playErrorSound()")  # 播放失败音效
      return()
    }
    
    # 清空 SKU 输入框
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 手动确认出库逻辑
  observeEvent(input$confirm_outbound_btn, {
    handleOperation(
      unique_items_data(),
      operation_name = "出库", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = NULL,     
      con,                  
      input, output, session
    )
    
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 撤回出库逻辑
  observeEvent(input$revert_outbound_btn, {
    handleOperation(
      unique_items_data(),
      operation_name = "撤回", 
      sku_field = "outbound_sku",
      output_name = "outbound_item_info",
      query_status = "国内出库",
      update_status_value = "国内入库",
      count_label = "可出库数", 
      count_field = "AvailableForOutbound", 
      refresh_trigger = NULL, 
      clear_field = "DomesticExitTime", # 清空出库日期字段
      clear_shipping_method = TRUE, # 清空出库国际运输方式
      con,                  
      input, output, session
    )
    
    updateTextInput(session, "outbound_sku", value = "")
    runjs("document.getElementById('outbound_sku').focus();")
  })
  
  # 监听选中行并显示大图与物品信息
  observeEvent(unique_items_table_outbound_selected_row(), {
    if (!is.null(unique_items_table_outbound_selected_row()) && length(unique_items_table_outbound_selected_row()) > 0) {
      selected_sku <- filtered_unique_items_data_outbound()[unique_items_table_outbound_selected_row(), "SKU", drop = TRUE]
      handleSkuInput(
        sku_input = selected_sku,
        output_name = "outbound_item_info",
        count_label = "可出库数",
        count_field = "AvailableForOutbound",
        con = con,
        output = output,
        placeholder_path = placeholder_300px_path,
        host_url = host_url
      )
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 售出分页                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听 sold_tabs 的变化，调整 filter_tabs
  observeEvent(input$sold_tabs, {
    if (input$sold_tabs == "物品售出") {
      updateTabsetPanel(session, inputId = "filter_tabs", selected = "物品筛选")
    } else if (input$sold_tabs == "订单管理") {
      updateTabsetPanel(session, inputId = "filter_tabs", selected = "订单筛选")
    }
  })
  
  # 手动刷新订单表
  observeEvent(input$refresh_orders, {
    orders_refresh_trigger(!orders_refresh_trigger()) # 触发 orders 数据刷新
    showNotification("订单数据已刷新！", type = "message")
  })
  
  ############################ 
  #####   物品售出子页   ##### 
  ############################ 
  
  itemFilterServer(
    id = "sold_filter",
    makers_items_map = makers_items_map
  )
  
  # 响应点击物品表的行，更新货架上的物品
  observeEvent(list(unique_items_table_sold_selected_row(), input$arrow_direction), {
    selected_row <- unique_items_table_sold_selected_row()
    sort_order <- input$arrow_direction
    
    if (is.null(selected_row) || length(selected_row) == 0) {
      return()
    }
    
    tryCatch({
      # 获取选中行对应的 SKU
      selected_sku <- filtered_unique_items_data_sold()[selected_row, "SKU", drop = TRUE]
      
      if (is.null(selected_sku) || selected_sku == "") {
        showNotification("未找到有效的 SKU！", type = "error")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = selected_sku, sort_order = sort_order)
      
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        return()
      }
      
      # 从箱子中获取当前 SKU 的已选数量
      box_data <- box_items()
      box_sku_count <- sum(box_data$SKU == selected_sku)
      
      # 检查是否所有物品已移入箱子
      if (box_sku_count >= nrow(all_shelf_items)) {
        shelf_items(create_empty_shelf_box())  # 清空货架
        showNotification("该 SKU 的所有物品已移入箱子，货架已清空！", type = "message")
        return()
      }
      
      # 更新货架数据，移除已移入箱子的物品
      if (box_sku_count == 0) {
        updated_shelf_items <- all_shelf_items
      } else {
        updated_shelf_items <- all_shelf_items[-seq_len(box_sku_count), ]
      }
      
      shelf_items(updated_shelf_items)
      showNotification(paste("已加载 SKU:", selected_sku, "的货架物品！"), type = "message")

    }, error = function(e) {
      showNotification(paste("加载货架时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")  
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)  # **防止初始时触发**

  ##### 网名自动填写组件
  matching_customer <- reactive({
    req(input$customer_name)  # 确保用户输入了顾客姓名
    tryCatch({
      # 将用户输入和数据中的姓名都转换为大写
      customer_name_upper <- toupper(input$customer_name)
      result <- orders() %>%
        mutate(CustomerNameUpper = toupper(CustomerName)) %>%  # 添加大写姓名列
        filter(grepl(customer_name_upper, CustomerNameUpper))  # 模糊匹配大写姓名
      
      valid_result <- result %>%
        filter(!is.na(CustomerNetName) & CustomerNetName != "") %>%  # 过滤有效的网名
        slice_head(n = 1)  # 仅返回第一条有网名的记录
      
      # 返回第一个有效的网名或 NULL
      if (nrow(valid_result) > 0) {
        return(valid_result$CustomerNetName[1])
      } else {
        return(NULL)  # 没有匹配的网名
      }
    }, error = function(e) {
      showNotification("网名查找出错！", type = "error")
      return(NULL)
    })
  })
  
  # 缓存最近查询过的顾客姓名与网名
  cache <- reactiveVal(list())
  
  # 使用 debounce 避免频繁触发查询
  customer_name_delayed <- debounce(reactive(input$customer_name), 300)
  
  # 网名自动填写
  observeEvent(customer_name_delayed(), {
    # 如果用户清空了 customer_name，则清空 customer_netname
    if (customer_name_delayed() == "") {
      updateTextInput(session, "customer_netname", value = "")
      return()
    }
    
    req(customer_name_delayed())  # 确保用户输入不为空
    
    cache_data <- cache()
    
    # 检查缓存是否已有数据
    if (customer_name_delayed() %in% names(cache_data)) {
      netname <- cache_data[[customer_name_delayed()]]
    } else {
      # 查询数据库
      netname <- matching_customer()
      
      # 如果有结果，更新缓存
      if (!is.null(netname)) {
        cache_data[[customer_name_delayed()]] <- netname
        cache(cache_data)  # 更新缓存
      }
    }
    
    # 更新网名输入框
    updateTextInput(session, "customer_netname", value = netname %||% "")
  })
  ######
  
  #运单PDF上传模块
  observeEvent(input$shiplabel_pdf_upload, {
    req(input$shiplabel_pdf_upload)
    
    # PDF 文件路径
    pdf_path <- input$shiplabel_pdf_upload$datapath
    
    tryCatch({
      # 检查 PDF 的页数
      pdf_info <- pdftools::pdf_info(pdf_path)
      if (pdf_info$pages != 1) {
        output$upload_status_message <- renderUI({
          tags$p("仅允许上传单页运单文件，请重新上传。", style = "color: red;")
        })
        return()
      }
      
      label_info <- extract_shipping_label_info(pdf_path)

      # 提取的姓名填充到输入框
      updateTextInput(session, "customer_name", value = label_info$customer_name)
      
      # 将提取的运单号填充到输入框
      updateTextInput(session, "tracking_number", value = label_info$tracking_number)
      disable("tracking_number")
      
      # 保存文件到目标目录
      dest_file <- file.path("/var/uploads/shiplabels", paste0(label_info$tracking_number, ".pdf"))
      file.copy(pdf_path, dest_file, overwrite = TRUE)

      # 上传成功提示
      output$upload_status_message <- renderUI({
        tags$p("运单上传成功！运单信息已识别", style = "color: green;")
      })
    }, error = function(e) {
      output$upload_status_message <- renderUI({
        tags$p(paste0("文件上传失败！", e), style = "color: red;")
      })
    })
    
    # 延时清空提示信息
    later::later(function() {
      output$upload_status_message <- renderUI({
        NULL  # 清空提示信息
      })
    }, delay = 3)  # 延迟 3 秒后执行
  })
  
  # 出售订单图片处理模块
  image_sold <- imageModuleServer("image_sold")
  
  # 在输入订单号时检查订单信息并填充
  observeEvent(input$order_id, {
    req(input$order_id)  # 确保订单号不为空
    
    tryCatch({
      # **去除空格和#号**
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # **提取主订单号**
      if (grepl("@$", sanitized_order_id)) {
        main_order_id <- sub("@$", "", sanitized_order_id)  # `1234@` → `1234`
      } else if (grepl("@", sanitized_order_id)) {
        main_order_id <- sub("@.*", "", sanitized_order_id)  # `1234@1` → `1234`
      } else {
        main_order_id <- sanitized_order_id  # `1234`
      }
      
      # **查询当前输入的订单**
      existing_order <- orders() %>% filter(OrderID == sanitized_order_id)
      
      # **检查主订单是否存在**
      main_order_exists <- nrow(orders() %>% filter(OrderID == main_order_id)) > 0
      
      if (nrow(existing_order) > 0) {
        # **情况 1：当前输入的订单号（如 `1234` 或 `1234@1`）存在，更新订单**
        updateSelectInput(session, "platform", selected = existing_order$Platform[1])
        updateNumericInput(session, "transaction_amount", value = existing_order$TransactionAmount[1])
        updateTextInput(session, "customer_name", value = existing_order$CustomerName[1])
        updateTextInput(session, "tracking_number", value = existing_order$UsTrackingNumber[1])
        
        # 提取分号后的用户留言部分
        user_notes <- if (grepl("；", existing_order$OrderNotes[1])) {
          sub(".*；", "", existing_order$OrderNotes[1])
        } else {
          existing_order$OrderNotes[1] %||% ""  # 如果没有分号，保留全部内容或空字符串
        }
        
        # 更新 TextAreaInput，只填充用户留言
        updateTextAreaInput(session, "order_notes", value = user_notes)
        
        # **检查 LabelStatus**
        if (existing_order$LabelStatus[1] != "无") {
          shinyjs::disable("tracking_number")  # 禁用输入框
        } else {
          shinyjs::enable("tracking_number")  # 启用输入框
        }
        
        # **处理 `调货` 和 `预定` 订单状态**
        updateCheckboxInput(session, "is_transfer_order", value = (existing_order$OrderStatus[1] == "调货"))
        updateCheckboxInput(session, "is_preorder", value = (existing_order$OrderStatus[1] == "预定"))
        
        # **处理 `预定` 订单的供应商和物品**
        if (existing_order$OrderStatus[1] == "预定") {
          updateSelectizeInput(session, "preorder_supplier", selected = character(0))
          updateTextAreaInput(session, "preorder_item_name", value = "")
          
          if (!is.null(existing_order$OrderNotes[1]) && !is.na(existing_order$OrderNotes[1])) {
            extracted <- extract_items_and_suppliers(existing_order$OrderNotes[1])
            
            if (nrow(extracted) > 0) {
              unique_suppliers <- unique(extracted$Supplier)
              if (length(unique_suppliers) > 0) {
                updateSelectizeInput(session, "preorder_supplier", selected = unique_suppliers[1])
              }
              
              # **确保 TextArea 正确更新**
              delay(50, {
                updateTextAreaInput(session, "preorder_item_name", value = paste(c(extracted$Item), collapse = "\n"))
              })
            }
          }
        } else {
          updateSelectizeInput(session, "preorder_supplier", selected = character(0))
          updateTextAreaInput(session, "preorder_item_name", value = "")
        }
        
        # **显示“更新订单”按钮**
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "更新订单",
            icon = icon("edit"),
            class = "btn-success",
            style = "font-size: 16px; width: 100%; height: 42px;"
          )
        })
      } else if (main_order_exists) {
        # **情况 2：主订单 `1234` 存在，但 `1234@1` 不存在 → 允许创建子订单**
        showNotification("主订单已存在，正在创建子订单", type = "warning")
        
        updateTextAreaInput(session, "order_notes", value = "")
        updateTextAreaInput(session, "preorder_item_name", value = "")
        
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "登记子单",
            icon = icon("plus"),
            class = "btn-primary",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
      } else {
        # **情况 3：`1234` 和 `1234@1` 都不存在，创建新订单**
        showNotification("未找到对应订单记录，可登记新订单", type = "warning")
        
        output$register_order_button_ui <- renderUI({
          actionButton(
            "register_order_btn",
            "登记订单",
            icon = icon("plus"),
            class = "btn-primary",
            style = "font-size: 16px; min-width: 130px; height: 42px;"
          )
        })
      }
    }, error = function(e) {
      showNotification(paste("检查订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 确保调货和预定两个勾选框互斥
  observeEvent(input$is_transfer_order, {
    if (input$is_transfer_order) {
      updateCheckboxInput(session, "is_preorder", value = FALSE)
    }
  })
  
  # 确保调货和预定两个勾选框互斥
  observeEvent(input$is_preorder, {
    if (input$is_preorder) {
      updateCheckboxInput(session, "is_transfer_order", value = FALSE)
    }
  })
  
  # 控制预订单额外显示区域
  observeEvent(input$is_preorder, {
    toggle(id = "preorder_fields", condition = input$is_preorder)
  })
  
  # 动态填充供应商与商品名选择器
  observe({
    selected_supplier <- input$preorder_supplier  # 获取当前选择的供应商
    
    # 根据供应商筛选库存中的商品名称
    filtered_items <- if (!is.null(selected_supplier) && selected_supplier != "") {
      inventory() %>% filter(Maker == selected_supplier) %>% pull(ItemName) %>% unique()
    } else {
      NULL  # 供应商未选择时，不提供任何选项
    }
    
    # 更新供应商选择器，确保不会清空当前选择
    updateSelectizeInput(session, "preorder_supplier", choices = maker_list()$Maker, selected = selected_supplier)
    
    # 更新商品名称选择器
    updateSelectizeInput(session, "preorder_item_name_db", choices = c("", filtered_items), selected = NULL, server = TRUE)
    
    # 控制 `preorder_item_name_db` 的显示
    toggle(id = "preorder_item_name_db", condition = !is.null(selected_supplier) && selected_supplier != "")
  })
  
  
  # 监听用户在 preorder_item_name_db 中的选择，并更新到 preorder_item_name
  observeEvent(input$preorder_item_name_db, {
    selected_item <- input$preorder_item_name_db
    selected_supplier <- input$preorder_supplier  # 获取当前供应商
    
    if (!is.null(selected_item) && selected_item != "") {
      existing_text <- input$preorder_item_name
      existing_items <- unlist(strsplit(existing_text, "\n"))
      
      # 将新选定的物品添加到文本框
      new_text <- paste(existing_text, selected_item, sep = ifelse(existing_text == "", "", "\n"))
      updateTextAreaInput(session, "preorder_item_name", value = new_text)
      
      # 仅在 `preorder_supplier` 选中的情况下查找库存
      if (!is.null(selected_supplier) && selected_supplier != "") {
        selected_inventory <- inventory() %>% 
          filter(ItemName == selected_item, Maker == selected_supplier)
        
        if (nrow(selected_inventory) > 0) {
          img_path <- ifelse(is.na(selected_inventory$ItemImagePath) || selected_inventory$ItemImagePath == "",
                             placeholder_150px_path,
                             paste0(host_url, "/images/", basename(selected_inventory$ItemImagePath)))
          
          runjs(sprintf("$('#preorder_img').attr('src', '%s').show();", img_path))
        } else {
          runjs("$('#preorder_img').hide();")
        }
      } else {
        runjs("$('#preorder_img').hide();")
      }
    }
  })
  
  # 登记、更新订单逻辑
  observeEvent(input$register_order_btn, {
    if (is.null(input$order_id) || input$order_id == "") {
      showNotification("订单号不能为空！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    if (is.null(input$platform) || input$platform == "") {
      showNotification("电商平台不能为空，请选择一个平台！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    if (is.null(input$transaction_amount) || is.na(input$transaction_amount)) {
      showNotification("总成交额不能为空！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    # 去除空格和#号
    sanitized_order_id <- gsub("#", "", trimws(input$order_id))
    
    # 调用封装函数登记订单
    order_registered <- register_order(
      order_id = sanitized_order_id,
      customer_name = input$customer_name,
      customer_netname = input$customer_netname,
      platform = input$platform,
      transaction_amount = input$transaction_amount,
      order_notes = input$order_notes,
      tracking_number = input$tracking_number,
      image_data = image_sold,
      con = con,
      orders = orders,
      box_items = box_items,
      unique_items_data = unique_items_data,
      is_transfer_order = input$is_transfer_order,
      is_preorder = input$is_preorder,
      preorder_supplier = input$preorder_supplier,
      preorder_item_name = input$preorder_item_name
    )
    
    # 如果订单登记失败，直接退出
    if (!order_registered) {
      runjs("playErrorSound()")
      return()
    } else {
      runjs("playSuccessSound()")
    }
    
    orders_refresh_trigger(!orders_refresh_trigger())
  })
  
  # 清空订单信息按钮
  observeEvent(input$clear_order_btn, {
    delay(100, {
      selected_order_id(NULL)
      associated_items(NULL)
      
      # 重置订单填写表
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      # 清空订单关联物品表
      output$associated_items_title <- renderUI({ NULL }) # 清空标题
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
      
      shinyjs::reset("orderForm")  # 假设 orderForm 是表单的 div ID
    })
  })

  # 订单合并
  observeEvent(input$merge_order_btn, {
    tryCatch({
      # 获取用户选中的订单号
      selected_order <- filtered_orders()[selected_order_row(), ]
      selected_order_id <- selected_order$OrderID
      
      if (is.null(selected_order_id) || length(selected_order_id) != 1) {
        showNotification("请选择一个订单进行合并！", type = "error")
        return()
      }
      
      # 判断选中的订单是否包含 '@'，如果没有 '@'，则其本身就是主单
      main_order_id <- ifelse(grepl("@", selected_order_id), sub("@.*", "", selected_order_id), selected_order_id)
      
      # 获取可能的子单，包括主单本身和所有 `@` 子单
      possible_sub_orders <- orders() %>%
        filter(grepl(paste0("^", main_order_id, "(@\\d+)?$"), OrderID))
      
      # 如果只找到 **1 个** 订单，且它本身就是主单（无 `@`），则不能合并
      if (nrow(possible_sub_orders) == 1 && !grepl("@", selected_order_id)) {
        showNotification("当前订单未找到可合并的子单！", type = "error")
        return()
      }
      
      # 获取所有子单的订单状态、运单号和平台信息
      order_statuses <- unique(possible_sub_orders$OrderStatus)
      tracking_numbers <- unique(possible_sub_orders$UsTrackingNumber)
      platforms <- unique(possible_sub_orders$Platform)
      
      # 检查订单状态、运单号和平台是否满足合并条件
      if (!all(order_statuses == "备货") || length(tracking_numbers) > 1 || length(platforms) > 1) {
        showNotification("子单的订单状态必须全部为 '备货'，运单号和平台必须一致才可合并！", type = "error")
        return()
      }
      
      # 获取子单的所有物品
      sub_items <- unique_items_data() %>%
        filter(OrderID %in% possible_sub_orders$OrderID)
      
      # 处理子单物品图片路径拼接
      image_paths <- unique(sub_items$ItemImagePath[!is.na(sub_items$ItemImagePath)])
      merged_image_path <- if (length(image_paths) > 0) {
        montage_path <- paste0("/var/www/images/", main_order_id, "_montage_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        generate_montage(image_paths, montage_path)
      } else {
        NA
      }
      
      # 获取最早的 `created_at` 时间
      earliest_created_at <- min(possible_sub_orders$created_at, na.rm = TRUE)
      
      # **先删除所有子单（包括可能存在的主单）**
      dbExecute(con, sprintf(
        "DELETE FROM orders WHERE OrderID IN (%s)",
        paste(shQuote(possible_sub_orders$OrderID), collapse = ", ")
      ))
      
      # **插入合并后的主订单**
      merged_order <- tibble(
        OrderID = main_order_id,
        Platform = platforms[1],
        TransactionAmount = max(possible_sub_orders$TransactionAmount),
        UsTrackingNumber = tracking_numbers[1],
        CustomerName = ifelse(length(unique(possible_sub_orders$CustomerName)) > 0,
                              paste(unique(possible_sub_orders$CustomerName), collapse = ", "), NA),
        CustomerNetName = ifelse(length(unique(possible_sub_orders$CustomerNetName)) > 0,
                                 paste(unique(possible_sub_orders$CustomerNetName), collapse = ", "), NA),
        OrderImagePath = merged_image_path,  # 合并图片路径
        OrderNotes = ifelse(length(unique(possible_sub_orders$OrderNotes)) > 0,
                            paste(unique(possible_sub_orders$OrderNotes), collapse = " | "), NA),
        OrderStatus = "备货",
        created_at = earliest_created_at,  # 使用子单中最早的创建时间
        updated_at = Sys.time()
      )
      
      dbWriteTable(
        con, "orders", merged_order,
        append = TRUE, overwrite = FALSE
      )
      
      # 更新子单物品的订单号为主单号
      update_order_id(con, sub_items$UniqueID, main_order_id)
      
      showNotification(paste("订单合并成功！主单号为：", main_order_id, ", 共计", nrow(sub_items), "件物品"), type = "message")
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      
    }, error = function(e) {
      showNotification(paste("合并订单时发生错误：", e$message), type = "error")
    })
  })
  
  ######
  
  # 渲染货架
  output$shelf_table <- renderDT({
    datatable_and_names <- render_table_with_images(shelf_items(), 
                                                    column_mapping = list(
                                                      SKU = "条形码",
                                                      ItemImagePath = "商品图",
                                                      ItemName = "商品名",
                                                      Status = "库存态",
                                                      Defect = "瑕疵态",
                                                      ProductCost = "单价"
                                                    ), 
                                                    selection = "single",
                                                    image_column = "ItemImagePath",
                                                    option = modifyList(table_default_options, list(scrollY = "278px"))
                                                    )
    
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  })
  
  # 渲染箱子
  output$box_table <- renderDT({
    datatable_and_names <- render_table_with_images(box_items(), 
                                                    column_mapping = list(
                                                      SKU = "条形码",
                                                      ItemImagePath = "商品图",
                                                      ItemName = "商品名",
                                                      Status = "库存态",
                                                      Defect = "瑕疵态",
                                                      ProductCost = "单价"
                                                    ), 
                                                    selection = "single",
                                                    image_column = "ItemImagePath",
                                                    option = modifyList(table_default_options, list(scrollY = "220px"))
    )
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  })
  
  # 渲染货架物品数量
  output$shelf_count <- renderText({
    shelf_items <- shelf_items()  # 获取当前货架上的物品
    paste0("(", nrow(shelf_items), ")")  # 返回数量显示
  })
  
  # 渲染发货箱物品数量
  output$box_count <- renderText({
    box_items <- box_items()  # 获取当前发货箱内的物品
    paste0("(", nrow(box_items), ")")  # 返回数量显示
  })
  
  # 点击货架物品，移入箱子
  observeEvent(input$shelf_table_rows_selected, {
    selected_row <- input$shelf_table_rows_selected
    shelf_data <- shelf_items()
    
    if (!is.null(selected_row) && nrow(shelf_data) >= selected_row) {
      selected_item <- shelf_data[selected_row, ]  # 获取选中的物品
      sku <- selected_item$SKU  # 获取SKU
      status <- selected_item$Status  # 获取库存状态

      # 查询当前 SKU 的美国入库库存数量
      us_stock_count <- sum(shelf_data$SKU == sku & shelf_data$Status == "美国入库")
      
      if (status == "美国入库" && us_stock_count <= 1) {
        showModal(modalDialog(
          title = "注意",
          p("此商品在美国库存仅剩一件，请沟通核实后再进行调货"),
          footer = tagList(
            actionButton("verify_and_proceed", "已核实, 继续调货", class = "btn-primary"),
            modalButton("取消")
          ),
          easyClose = FALSE
        ))
      } else {
        # 直接执行入箱操作
        updateBox(selected_item, selected_row, shelf_data)
        runjs("playSuccessSound()")
      }
    }
  })
  
  observeEvent(input$verify_and_proceed, {
    removeModal()  # 移除弹窗
    selected_row <- input$shelf_table_rows_selected
    shelf_data <- shelf_items()
    
    if (!is.null(selected_row) && nrow(shelf_data) >= selected_row) {
      selected_item <- shelf_data[selected_row, ]  # 获取选中的物品
      
      # 执行入箱操作
      updateBox(selected_item, selected_row, shelf_data)
      runjs("playSuccessSound()")
    }
  })
  
  updateBox <- function(selected_item, selected_row, shelf_data) {
    # 更新箱子内容
    current_box <- box_items()
    box_items(bind_rows(selected_item, current_box))
    
    # 更新货架上的物品，移除已选的
    updated_shelf <- shelf_data[-selected_row, ]
    shelf_items(updated_shelf)
  }
  
  
  # 点击箱子物品，还回货架
  observeEvent(input$box_table_rows_selected, {
    selected_row <- input$box_table_rows_selected
    box_data <- box_items()
    
    if (!is.null(selected_row) && nrow(box_data) >= selected_row) {
      selected_item <- box_data[selected_row, ]  # 获取选中的物品
      
      # 更新货架内容
      current_shelf <- shelf_items()
      shelf_items(bind_rows(current_shelf, selected_item))
      
      # 更新箱子内的物品，移除已选的
      updated_box <- box_data[-selected_row, ]
      box_items(updated_box)
    }
  })
  
  # 扫码上架功能
  observeEvent(input$sku_to_shelf, {
    req(input$sku_to_shelf)  # 确保输入框不为空
    
    tryCatch({
      # 获取输入的 SKU
      scanned_sku <- trimws(input$sku_to_shelf)
      
      if (is.null(scanned_sku) || scanned_sku == "") {
        showNotification("请输入有效的 SKU！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      # 如果货架中没有符合条件的物品，提示错误
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        runjs("playErrorSound()")
        updateTextInput(session, "sku_to_shelf", value = "")  # 清空输入框
        return()
      }
      
      # 从箱子中获取当前 SKU 的已选数量
      box_data <- box_items()

      # 更新货架上的物品
      updated_shelf <- all_shelf_items[!all_shelf_items$UniqueID %in% box_data$UniqueID, ]
      shelf_items(updated_shelf)
      
      # 通知用户
      showNotification(paste("物品已上货架！SKU:", scanned_sku), type = "message")
      runjs("playSuccessSound()")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("处理 SKU 时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")
    })
    
    # 清空输入框
    updateTextInput(session, "sku_to_shelf", value = "")
  })
  
  # 扫码入箱功能
  observeEvent(input$sku_to_box, {
    req(input$sku_to_box)  # 确保输入框不为空
    
    tryCatch({
      # 获取输入的 SKU
      scanned_sku <- trimws(input$sku_to_box)
      
      if (is.null(scanned_sku) || scanned_sku == "") {
        showNotification("请输入有效的 SKU！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 从 unique_items_data 获取货架中符合条件的物品
      all_shelf_items <- get_shelf_items(data = unique_items_data(), sku = scanned_sku)
      
      if (is.null(all_shelf_items)) {
        showNotification("货架上未找到对应 SKU 的物品！", type = "error")
        runjs("playErrorSound()")
        updateTextInput(session, "sku_to_box", value = "")  # 清空输入框
        return()
      }
      
      # 检查是否为 "美国入库" 状态且仅剩一件
      us_stock_count <- sum(all_shelf_items$Status == "美国入库")
      
      if (any(all_shelf_items$Status == "美国入库") && us_stock_count <= 1) {
        # 弹出模态框，提醒用户核实后再操作
        showModal(modalDialog(
          title = "注意",
          p("此商品在美国库存仅剩一件，请沟通核实后再进行调货"),
          footer = tagList(
            actionButton("verify_and_proceed_auto", "已核实, 继续调货", class = "btn-primary"),
            modalButton("取消")
          ),
          easyClose = FALSE
        ))
        
        # 监听 "已核实" 按钮事件，确认操作
        observeEvent(input$verify_and_proceed_auto, {
          removeModal()  # 关闭模态框
          process_box_addition(scanned_sku, all_shelf_items)  # 继续处理移入箱子操作
        })
        
        # 清空输入框
        updateTextInput(session, "sku_to_box", value = "")  # 清空输入框
        return()
      }
      
      # 如果不需要弹窗，直接处理入箱
      process_box_addition(scanned_sku, all_shelf_items)
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("处理 SKU 时发生错误：", e$message), type = "error")
      runjs("playErrorSound()")
    })
    
    # 清空输入框
    updateTextInput(session, "sku_to_box", value = "")
  })
  
  # 定义移入箱子的逻辑
  process_box_addition <- function(scanned_sku, all_shelf_items) {
    # 从箱子中获取当前 SKU 的已选数量
    box_data <- box_items()
    box_sku_count <- sum(box_data$SKU == scanned_sku)
    
    # 如果箱子中物品数量 >= 货架中物品总量，则阻止操作
    if (box_sku_count >= nrow(all_shelf_items)) {
      showNotification("该 SKU 的所有物品已移入箱子，无法继续添加！", type = "error")
      runjs("playErrorSound()")
      return()
    }
    
    # 获取优先级最高的物品
    selected_item <- all_shelf_items[box_sku_count + 1, ]
    
    # 更新箱子内容
    current_box <- box_items()
    box_items(bind_rows(selected_item, current_box))
    
    # 更新货架上的物品
    updated_shelf <- all_shelf_items[!all_shelf_items$UniqueID %in% box_items()$UniqueID, ]
    shelf_items(updated_shelf)
    runjs("playSuccessSound()")
  }
  
  zero_stock_items <- reactiveVal(list())  # 用于存储国内库存为零的物品
  
  # 确认售出
  observeEvent(input$confirm_order_btn, {
    req(input$order_id)
    
    tryCatch({
      
      if (nrow(box_items()) == 0) {
        showNotification("箱子内容不能为空！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      if (is.null(input$platform) || input$platform == "") {
        showNotification("电商平台不能为空，请选择一个平台！", type = "error")
        runjs("playErrorSound()")
        return()
      }
      
      # 去除空格和#号
      sanitized_order_id <- gsub("#", "", trimws(input$order_id))
      
      # 确保订单已登记
      order_registered <- register_order(
        order_id = sanitized_order_id,
        customer_name = input$customer_name,
        customer_netname = input$customer_netname,
        platform = input$platform,
        transaction_amount = input$transaction_amount,
        order_notes = input$order_notes,
        tracking_number = input$tracking_number,
        image_data = image_sold,
        con = con,
        orders = orders,
        box_items = box_items,
        unique_items_data = unique_items_data,
        is_transfer_order = input$is_transfer_order,
        is_preorder = input$is_preorder,
        preorder_supplier = input$preorder_supplier,
        preorder_item_name = input$preorder_item_name
      )
      
      # 如果订单登记失败，直接退出
      if (!order_registered) {
        runjs("playErrorSound()")
        return()
      }
      
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 遍历箱子内物品，减库存并更新物品状态
      lapply(1:nrow(box_items()), function(i) {
        item <- box_items()[i, ]
        sku <- item$SKU
        
        # 根据当前状态决定新的状态
        current_status <- item$Status
        new_status <- ifelse(
          current_status %in% c("美国入库", "国内出库"), "美国调货",
          ifelse(current_status == "国内入库", "国内售出", NA)
        )
        
        # 更新 unique_items 表中的状态
        update_status(
          con = con,
          unique_id = item$UniqueID,
          new_status = new_status,
          shipping_method = if (new_status == "国内售出") input$sold_shipping_method else NULL,
          refresh_trigger = NULL
        )
        
        # 更新订单号
        update_order_id(
          con = con,
          unique_id = item$UniqueID,
          order_id = sanitized_order_id
        )
      }) # end of lapply
      
      # 检查库存并记录库存为零的物品
      zero_items <- list()  # 临时列表存储库存为零的物品
      
      for (sku in unique(box_items()$SKU)) {
        # 检查库存
        result <- unique_items_data() %>%
          filter(SKU == sku) %>%
          group_by(SKU, ItemName, ItemImagePath, Maker) %>%
          summarise(
            DomesticStock = sum(Status == "国内入库", na.rm = TRUE),
            .groups = "drop"
          )
        
        if (result$DomesticStock == 0) {
          zero_items <- append(zero_items, list(result))
        }
      }
      
      # 更新 zero_stock_items
      zero_stock_items(zero_items)
      
      # 弹出模态框提示补货和出库请求
      modal_content <- tagList()
      
      if (length(zero_items) > 0) {
        modal_content <- tagAppendChildren(
          modal_content,
          tags$div(
            style = "padding: 10px; background-color: #ffe6e6; border-radius: 8px; margin-bottom: 20px;",
            tags$h4("需要采购补货：", style = "color: red; margin-bottom: 15px;"),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
              lapply(zero_items, function(item) {
                div(
                  style = "background: white; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; display: flex; flex-direction: column; align-items: center;",
                  tags$img(
                    src = ifelse(is.na(item$ItemImagePath), placeholder_150px_path, paste0(host_url, "/images/", basename(item$ItemImagePath))),
                    style = "width: 150px; height: 150px; object-fit: cover; border-radius: 8px; margin-bottom: 10px;"
                  ),
                  tags$p(tags$b("物品名："), item$ItemName, style = "margin: 5px 0;"),
                  tags$p(tags$b("SKU："), item$SKU, style = "margin: 5px 0;"),
                  numericInput(
                    paste0("purchase_qty_", item$SKU),
                    "请求数量",
                    value = 1,
                    min = 1,
                    width = "80%"
                  ),
                  textAreaInput(
                    paste0("purchase_remark_input_", item$SKU),
                    "留言（可选）",
                    placeholder = "输入留言...",
                    width = "100%",
                    rows = 2
                  ),
                  actionButton(
                    paste0("create_request_purchase_", item$SKU),
                    "发出采购请求",
                    class = "btn-primary",
                    style = "margin-top: 10px; width: 100%;"
                  )
                )
              })
            )
          )
        )
        showModal(modalDialog(
          title = "处理库存请求",
          div(style = "max-height: 650px; overflow-y: auto;", modal_content),
          easyClose = FALSE,
          footer = tagList(
            actionButton("complete_requests", "关闭", class = "btn-success")
          )
        ))
      }

      # 清空箱子
      box_items(create_empty_shelf_box())
      
      # 重置所有输入框
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      showNotification("订单已完成售出并更新状态！", type = "message")
      runjs("playSuccessSound()")
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
      runjs("playErrorSound()")
    })
  })
  
  # 用于记录已绑定的请求按钮
  observed_request_buttons <- reactiveValues(registered = character())
  
  # 监听添加请求按钮
  observe({
    # 获取当前所有动态生成的按钮 ID
    request_buttons <- grep("^create_request_", names(input), value = TRUE)
    
    # 筛选出尚未绑定的按钮
    new_buttons <- setdiff(request_buttons, observed_request_buttons$registered)
    
    # 为每个新按钮动态创建监听
    lapply(new_buttons, function(button_id) {
      observeEvent(input[[button_id]], {
        if (grepl("purchase", button_id)) {
          # 采购请求处理逻辑
          sku <- sub("create_request_purchase_", "", button_id)  # 提取 SKU
          items <- zero_stock_items()  # 从 reactiveVal 获取库存为零的物品
          item <- items[[which(sapply(items, function(x) x$SKU == sku))]]  # 找到匹配的物品
          
          # 获取请求数量
          qty <- input[[paste0("purchase_qty_", sku)]]
          request_id <- uuid::UUIDgenerate()
          
          tryCatch({
            # 插入采购请求到数据库
            dbExecute(con,
                      "INSERT INTO requests (RequestID, SKU, Maker, ItemImagePath, ItemDescription, Quantity, RequestStatus, Remarks, RequestType)
                     VALUES (?, ?, ?, ?, ?, ?, '待处理', ?, '采购')",
                      params = list(
                        request_id,
                        sku,
                        item$Maker,
                        item$ItemImagePath,
                        item$ItemName,
                        qty,
                        format_remark(input[[paste0("purchase_remark_input_", sku)]], system_type)
                      ))
            
            # 绑定按钮
            bind_buttons(request_id, requests_data(), input, output, session, con)
            
            # 动态更新按钮文本和样式
            updateActionButton(session, inputId = button_id, label = HTML("<i class='fa fa-check'></i> 采购请求已发送"))
            runjs(sprintf("$('#%s').removeClass('btn-primary').addClass('btn-success');", button_id))
            disable(button_id)
            
            # 提示成功消息
            showNotification(paste0("已发出采购请求，SKU：", sku, "，数量：", qty), type = "message")
          }, error = function(e) {
            # 提示错误消息
            showNotification(paste("发出采购请求失败：", e$message), type = "error")
          })
        }
      }, ignoreInit = TRUE)  # 忽略初始绑定时的触发
    })
    
    # 更新已注册的按钮 ID
    observed_request_buttons$registered <- union(observed_request_buttons$registered, new_buttons)
  })
  
  # 监听 "完成请求" 按钮事件
  observeEvent(input$complete_requests, {
    zero_stock_items(list())        # 清空补货物品列表
    removeModal()                   # 关闭模态框
  })
  
  ############################ 
  #####   订单管理子页   ##### 
  ############################ 
  
  # 订单关联物品容器
  associated_items <- reactiveVal(NULL)
  
  # 用于存储当前选中的订单ID
  selected_order_id <- reactiveVal(NULL)  
  
  # 商品名自动联想
  autocompleteInputServer("sold", get_suggestions = item_names)  # 返回商品名列表
  
  # 监听订单选择事件
  observeEvent(selected_order_row(), {
    # 获取选中行
    selected_row <- selected_order_row()
    req(selected_row)  # 确保有选中行
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    customer_name <- selected_order$CustomerName
    order_status <- selected_order$OrderStatus
    us_tracking_number <- selected_order$UsTrackingNumber
    
    # 存储当前选中的订单ID
    selected_order_id(order_id)
    
    # 设置运单 PDF 文件路径
    label_pdf_file_path(file.path("/var/uploads/shiplabels", paste0(us_tracking_number, ".pdf")))
    
    # 填充左侧订单信息栏
    updateTextInput(session, "order_id", value = order_id)
    
    # 更新关联物品数据（在 renderUI 之前）
    associated_items(unique_items_data() %>% filter(OrderID == order_id))
    
    # 动态更新标题和按钮
    output$associated_items_title <- renderUI({
      req(selected_order_id())
      selected_row <- selected_order_row()
      req(selected_row)
      
      # 获取订单数据
      selected_order <- filtered_orders()[selected_row, ]
      order_id <- selected_order$OrderID
      customer_name <- selected_order$CustomerName
      order_status <- selected_order$OrderStatus
      
      # 获取相关物品
      items <- associated_items()
      has_items <- !is.null(items) && is.data.frame(items) && nrow(items) > 0
      
      # 计算标题后缀（发货时间）
      shipping_suffix <- ""
      if (has_items) {
        all_us_shipping <- all(items$Status == "美国发货", na.rm = TRUE)
        if (all_us_shipping && "UsShippingTime" %in% names(items)) {
          latest_shipping_time <- max(items$UsShippingTime, na.rm = TRUE)
          if (!is.infinite(latest_shipping_time)) {
            shipping_suffix <- sprintf("（发货日期：%s）", latest_shipping_time)
          }
        }
      } else {
        shipping_suffix <- "（无）"
      }
      
      # 统一按钮区域
      button_div <- div(
        style = "display: flex; gap: 10px;",
        actionButton(
          "regen_order_image", 
          label = "重新生成订单拼图", 
          class = "btn btn-info", 
          style = "height: 34px; font-size: 14px; padding: 5px 10px;"
        ),
        if (order_status == "预定") {
          actionButton(
            "complete_preorder", 
            label = "已完成预定", 
            class = "btn-success", 
            style = "font-size: 14px; padding: 5px 10px;"
          )
        },
        if (order_status %in% c("备货", "预定", "调货")) {
          actionButton(
            "cancel_order", 
            label = "取消订单", 
            class = "btn btn-warning", 
            style = "font-size: 14px; padding: 5px 10px;"
          )
        },
        if (selected_order$LabelStatus != "无") {
          downloadButton(
            "download_pdf_manage", 
            label = "下载运单", 
            class = "btn btn-primary", 
            style = "height: 34px; font-size: 14px; padding: 5px 10px;"
          )
        }
      )
      
      # 返回 UI
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        tags$h4(
          sprintf("#%s - %s 的订单物品%s", order_id, customer_name, shipping_suffix),
          style = "color: #007BFF; font-weight: bold; margin: 0;"
        ),
        button_div
      )
    })
    
    # 重置图片上传模块
    image_sold$reset()
  })
  
  # 重新生成订单拼图按钮
  observeEvent(input$regen_order_image, {
    req(selected_order_id())
    success <- update_order_montage(selected_order_id(), con, unique_items_data())
    
    # 只有当拼图生成成功时才刷新订单数据
    if (success) {
      orders_refresh_trigger(!orders_refresh_trigger())
    }
  })
  
  # 定义运单下载处理器
  output$download_pdf_manage <- downloadHandler(
    filename = function() {
      basename(label_pdf_file_path())
    },
    content = function(file) {
      file.copy(label_pdf_file_path(), file, overwrite = TRUE)
    }
  )
  
  # 完成预定订单按钮
  observeEvent(input$complete_preorder, {
    req(selected_order_row())
    
    # 获取选中订单
    selected_row <- selected_order_row()
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    existing_notes <- selected_order$OrderNotes %||% ""  # 若为空，则默认空字符串
    
    # 检查 associated_items 是否为空
    associated_items_data <- associated_items()
    if (is.null(associated_items_data) || nrow(associated_items_data) == 0) {
      showNotification("无法完成预定：订单中未找到关联物品！", type = "error")
      return()  # 提前退出，避免后续逻辑执行
    }
    
    # 在 R 中拼接备注内容
    new_notes <- paste(existing_notes, sprintf("【预定完成 %s】", format(Sys.Date(), "%Y-%m-%d")))
    
    update_order_status(order_id, new_status = "备货", updated_notes = new_notes, refresh_trigger = orders_refresh_trigger, con = con)
  })
  
  # 取消订单按钮
  observeEvent(input$cancel_order, {
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    tryCatch({
      # 获取与订单关联的物品
      associated_items <- unique_items_data() %>% filter(OrderID == order_id)
      
      if (nrow(associated_items) > 0) {
        # 遍历关联物品进行逆向操作
        lapply(1:nrow(associated_items), function(i) {
          item <- associated_items[i, ]
          
          # 查询物品的原始状态
          original_state <- dbGetQuery(con, paste0(
            "SELECT * FROM item_status_history WHERE UniqueID = '", item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
          ))
          
          if (nrow(original_state) > 0) {
            # 恢复物品状态
            update_status(
              con = con,
              unique_id = item$UniqueID,
              new_status = original_state$previous_status,
              clear_status_timestamp = item$Status
            )
            
            # 清空物品的 OrderID
            update_order_id(
              con = con,
              unique_id = item$UniqueID,
              order_id = NULL  # 清空订单号
            )
          } else {
            showNotification(paste0("物品 ", item$UniqueID, " 无状态历史记录，无法恢复。"), type = "error")
          }
        })
      }
      
      # 更新订单状态为"取消"
      update_order_status(order_id, new_status = "取消", refresh_trigger = orders_refresh_trigger, con = con)
      
      # 通知用户操作结果
      message <- if (nrow(associated_items) > 0) {
        paste("订单", order_id, "已取消，订单内物品已返回库存！")
      } else {
        paste("订单", order_id, "已取消，没有关联的物品需要处理！")
      }
      showNotification(message, type = "message")
      
      # 重置输入
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      # 清空关联物品表
      output$associated_items_title <- renderUI({ NULL }) # 清空标题
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
    }, error = function(e) {
      showNotification(paste("取消订单时发生错误：", e$message), type = "error")
    })
  })
  
  # 渲染物品信息卡片  
  observe({
    req(associated_items())
    if (nrow(associated_items()) == 0) {
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
      return()
    }
    renderOrderItems(output, "order_items_cards", associated_items(), con, deletable = TRUE)
  })

  # 订单物品删除逻辑
  observeEvent(input$delete_card, {
    req(input$delete_card, associated_items())  # 确保输入和物品列表存在
    
    # 当前物品列表
    current_items <- associated_items()

    # 移除对应的物品
    deleted_item <- current_items %>% filter(UniqueID == input$delete_card)
    updated_items <- current_items %>% filter(UniqueID != input$delete_card)
    associated_items(updated_items)  # 更新物品列表

    # 查询物品原始状态
    original_state <- dbGetQuery(con, paste0(
      "SELECT * FROM item_status_history WHERE UniqueID = '", deleted_item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
    ))
    
    if (nrow(original_state) > 0) {
      # 恢复物品状态到原始状态
      update_status(
        con = con,
        unique_id = deleted_item$UniqueID,
        new_status = original_state$previous_status,
        clear_status_timestamp = deleted_item$Status
      )
    } else {
      showModal(modalDialog(
        title = "错误",
        paste0("未找到物品 (SKU: ", deleted_item$SKU, ") 之前的库存状态记录，请联系管理员手动更改物品库存状态"),
        footer = modalButton("关闭"),
        easyClose = TRUE
      ))    
    }
    
    # 清空物品的 OrderID
    update_order_id(
      con = con,
      unique_id = deleted_item$UniqueID,
      order_id = NULL  # 清空订单号
    )
    
    # 提示删除成功
    showNotification("物品已删除, 库存已归还。", type = "message")

    # 更新数据并触发 UI 刷新
    orders_refresh_trigger(!orders_refresh_trigger())
    
    # 检查并更新订单拼图
    order_id <- deleted_item$OrderID
    order_info <- dbGetQuery(con, paste0(
      "SELECT OrderImagePath FROM orders WHERE OrderID = '", order_id, "'"
    ))
    
    if (nrow(order_info) > 0) {
      order_image_path <- order_info$OrderImagePath
      # 检查 OrderImagePath 是否为空或包含“montage” <- 只有原图是拼图的时候才能更新
      if (is.na(order_image_path) || grepl("montage", order_image_path)) {
        update_order_montage(order_id, con, unique_items_data())
      }
    }
  })
  
  # 清空筛选条件逻辑
  observeEvent(input$reset_filter_btn, {
    tryCatch({
      # 重置所有输入框和选择框
      updateTextInput(session, "filter_combined", value = "")  # 重置合并的搜索框
      updateSelectInput(session, "filter_platform", selected = "")  # 重置电商平台选择
      updateSelectInput(session, "filter_order_status", selected = "")  # 重置订单状态选择
      updateDateRangeInput(session, "filter_order_date", 
                           start = Sys.Date() - 90, 
                           end = Sys.Date() + 1)  # 重置日期范围到默认值
      
      # 显示成功通知
      showNotification("筛选条件已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并显示通知
      showNotification(paste("清空筛选条件时发生错误：", e$message), type = "error")
    })
  })
  
  # 删除订单逻辑
  observeEvent(input$delete_order_btn, {
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    # 显示确认弹窗
    showModal(
      modalDialog(
        title = "确认删除订单",
        paste0("您确定要删除订单 ", order_id, " 吗？此操作无法撤销！"),
        footer = tagList(
          modalButton("取消"),  # 关闭弹窗按钮
          actionButton("confirm_delete_order_btn", "确认删除", class = "btn-danger")
        )
      )
    )
  })
  
  # 确认删除订单逻辑
  observeEvent(input$confirm_delete_order_btn, {
    removeModal()  # 关闭确认弹窗
    
    req(selected_order_row())  # 确保用户选择了一行订单
    selected_row <- selected_order_row()
    
    # 获取选中的订单数据
    selected_order <- filtered_orders()[selected_row, ]
    order_id <- selected_order$OrderID
    
    tryCatch({
      # 获取与订单关联的物品
      associated_items <- unique_items_data() %>% filter(OrderID == order_id)
      
      if (nrow(associated_items) > 0) {
        # 遍历关联物品进行逆向操作
        lapply(1:nrow(associated_items), function(i) {
          item <- associated_items[i, ]
          
          # 查询物品的原始状态
          original_state <- dbGetQuery(con, paste0(
            "SELECT * FROM item_status_history WHERE UniqueID = '", item$UniqueID, "' ORDER BY change_time DESC LIMIT 1"
          ))
          
          if (nrow(original_state) > 0) {
            # 恢复物品状态
            update_status(
              con = con,
              unique_id = item$UniqueID,
              new_status = original_state$previous_status,
              clear_status_timestamp = item$Status
            )
            
            # 清空物品的 OrderID
            update_order_id(
              con = con,
              unique_id = item$UniqueID,
              order_id = NULL  # 清空订单号
            )
          } else {
            showNotification(paste0("物品 ", item$UniqueID, " 无状态历史记录，无法恢复。"), type = "error")
          }
        })
      }
      
      # 删除订单记录
      dbExecute(con, "DELETE FROM orders WHERE OrderID = ?", params = list(order_id))
      
      # 通知用户操作结果
      message <- if (nrow(associated_items) > 0) {
        paste("订单", order_id, "已成功删除，订单内物品已返回库存！")
      } else {
        paste("订单", order_id, "已成功删除，没有关联的物品需要处理！")
      }
      showNotification(message, type = "message")
      
      # 更新数据并触发 UI 刷新
      orders_refresh_trigger(!orders_refresh_trigger())
      
      # 重置输入
      reset_order_form(session, image_sold)
      
      # 重置库存商品名列表
      updateSelectizeInput(session, "preorder_item_name_db", choices = c("", inventory()$ItemName), selected = NULL, server = TRUE)
      
      # 清空关联物品表
      output$associated_items_title <- renderUI({ NULL }) # 清空标题
      renderOrderItems(output, "order_items_cards", data.frame(), con)  # 清空物品卡片
    }, error = function(e) {
      showNotification(paste("删除订单时发生错误：", e$message), type = "error")
    })
  })
  
  # order信息筛选清除
  observeEvent(input$clear_filter_combined, {
    updateTextInput(session, "filter_combined", value = "")
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
  ## 瑕疵商品分页                                               ##
  ##                                                            ##
  ################################################################
  
  # 物品表过滤模块
  itemFilterServer(
    id = "defect_filter",
    makers_items_map = makers_items_map
  )
  
  # 处理登记为瑕疵品
  observeEvent(input$register_defective, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品的状态符合要求（Defect == "无瑕" 或 Defect == "修复"）
      invalid_items <- selected_data[!selected_data$Defect %in% c("无瑕", "修复"), ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘无瑕’或‘修复’状态的物品可以登记为瑕疵品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为瑕疵
        update_status(con, unique_id, defect_status = "瑕疵", refresh_trigger = NULL)
        
        # 添加备注
        defect_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = defect_notes,
          status_label = "瑕疵",
          refresh_trigger = NULL
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为瑕疵品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  # 处理登记为修复品
  observeEvent(input$register_repair, {
    selected_rows <- unique_items_table_defect_selected_row()  # 获取选中行索引
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的数据
      selected_data <- filtered_unique_items_data_defect()[selected_rows, ]
      
      # 检查是否所有选中物品都满足条件（Defect == "瑕疵"）
      invalid_items <- selected_data[selected_data$Defect != "瑕疵", ]
      if (nrow(invalid_items) > 0) {
        showNotification("只有‘瑕疵’状态的物品可以登记为修复品！", type = "error")
        return()
      }
      
      # 遍历每个选中物品，进行状态更新和备注添加
      lapply(selected_data$UniqueID, function(unique_id) {
        # 更新状态为修复
        update_status(con, unique_id, defect_status = "修复", refresh_trigger = NULL)
        
        # 添加备注
        repair_notes <- trimws(input$manage_defective_notes)
        add_defective_note(
          con = con,
          unique_id = unique_id,
          note_content = repair_notes,
          status_label = "修复",
          refresh_trigger = NULL
        )
      })
      
      # 清空备注栏
      updateTextAreaInput(session, "manage_defective_notes", value = "")
      
      showNotification("所选物品已成功登记为修复品！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  # 监听“仅显示无瑕品”开关的状态变化
  observeEvent(input$show_perfects_only, {
    if (input$show_perfects_only && input$show_defects_only) {
      updateSwitchInput(session, "show_defects_only", value = FALSE)
    }
  })
  
  # 监听“仅显示瑕疵品”开关的状态变化
  observeEvent(input$show_defects_only, {
    if (input$show_defects_only && input$show_perfects_only) {
      updateSwitchInput(session, "show_perfects_only", value = FALSE)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 国际物流管理分页                                           ##
  ##                                                            ##
  ################################################################
  
  # 筛选逻辑
  itemFilterServer(
    id = "logistic_filter",
    makers_items_map = makers_items_map)
  
  ######################
  ### 国际运单登记分页
  ######################
  
  # 登记运单信息
  observeEvent(input$register_shipment_btn, {
    req(input$intl_tracking_number, input$intl_shipping_method, input$intl_total_shipping_cost)
    
    # 获取用户输入的值
    tracking_number <- trimws(input$intl_tracking_number)
    shipping_method <- input$intl_shipping_method
    total_cost <- as.numeric(input$intl_total_shipping_cost)
    
    tryCatch({
      # 更新或插入运单记录
      dbExecute(
        con,
        "INSERT INTO intl_shipments (TrackingNumber, ShippingMethod, TotalCost, Status)
       VALUES (?, ?, ?, '运单创建')
       ON DUPLICATE KEY UPDATE 
         ShippingMethod = VALUES(ShippingMethod), 
         TotalCost = VALUES(TotalCost),
         UpdatedAt = CURRENT_TIMESTAMP",
        params = list(tracking_number, shipping_method, total_cost)
      )
      
      # # 生成交易记录的备注
      # remarks <- paste0("[国际运费登记]", " 运单号：", tracking_number, " 运输方式：", shipping_method)
      # 
      # # 生成交易记录的 ID
      # transaction_id <- generate_transaction_id("一般户卡", total_cost, remarks, Sys.time())
      # 
      # # 插入交易记录到“一般户卡”
      # dbExecute(
      #   con,
      #   "INSERT INTO transactions (TransactionID, AccountType, Amount, Remarks, TransactionTime) 
      #  VALUES (?, ?, ?, ?, ?)",
      #   params = list(
      #     transaction_id,
      #     "一般户卡", 
      #     -total_cost,  # 转出金额为负值
      #     remarks,
      #     Sys.time()
      #   )
      # )
      # 
      # showNotification("国际运单登记成功，相关费用已记录到'一般户卡（541）'！", type = "message")
      # 
      # # 重新计算所有balance记录
      # update_balance("一般户卡", con)
      
      enable("link_tracking_btn")  # 启用挂靠运单按钮
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  # 查询运单逻辑
  observeEvent(input$intl_tracking_number, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      # 如果运单号为空，清空相关输入字段并禁用按钮
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      disable("link_tracking_btn")  # 禁用挂靠运单按钮
      output$intl_status_display <- renderText({ "" })  # 清空状态显示
      return()
    }
    
    tryCatch({
      # 查询运单号对应的信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod, TotalCost, Status FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) > 0) {
        # 如果运单号存在，回填信息
        updateSelectInput(session, "intl_shipping_method", selected = shipment_info$ShippingMethod[1])
        updateNumericInput(session, "intl_total_shipping_cost", value = shipment_info$TotalCost[1])
        enable("link_tracking_btn")  # 启用挂靠运单按钮
        
        # 显示物流状态
        output$intl_status_display <- renderText({
          paste("物流状态:", shipment_info$Status[1])
        })
        
      } else {
        # 如果运单号不存在，清空相关字段并禁用按钮
        updateSelectInput(session, "intl_shipping_method", selected = "空运")
        updateNumericInput(session, "intl_total_shipping_cost", value = 0)
        disable("link_tracking_btn")  # 禁用挂靠运单按钮
        
        # 提示未找到状态
        output$intl_status_display <- renderText({
          "未找到对应的运单信息，可以登记新运单！"
        })
      }
    }, error = function(e) {
      # 遇到错误时禁用按钮并清空状态显示
      disable("link_tracking_btn")
      output$intl_status_display <- renderText({
        paste("查询失败：", e$message)
      })
      showNotification(paste("加载运单信息失败：", e$message), type = "error")
    })
  })
  
  # 货值汇总显示
  observeEvent(input$batch_value_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询与运单号相关的汇总信息
      summary_info <- dbGetQuery(
        con,
        "
      SELECT 
        COUNT(*) AS TotalQuantity,
        SUM(ProductCost) AS TotalValue,
        SUM(DomesticShippingCost) AS TotalDomesticShipping,
        SUM(IntlShippingCost) AS TotalIntlShipping
      FROM unique_items
      WHERE IntlTracking = ?
      ",
        params = list(tracking_number)
      )
      
      # 查询运单号的运输方式
      shipping_method_info <- dbGetQuery(
        con,
        "SELECT ShippingMethod FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(summary_info) == 0 || is.na(summary_info$TotalQuantity[1])) {
        showNotification("未找到与当前运单号相关的货物信息！", type = "warning")
        return()
      }
      
      # 确定运输方式
      shipping_method <- ifelse(nrow(shipping_method_info) > 0, shipping_method_info$ShippingMethod[1], "未知")
      
      # 计算总价值合计
      total_value_sum <- summary_info$TotalValue[1] + summary_info$TotalDomesticShipping[1] + summary_info$TotalIntlShipping[1]
      
      # 格式化汇总信息
      summary_text <- HTML(paste0(
        "<div style='font-family: Arial, sans-serif; line-height: 2;'>",  # 调整行间距
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left; width: 30%;'>运单号:</td>",
        "<td style='text-align: left; color: #000;'>", tracking_number, " <span style='color: #28A745;'>(", shipping_method, ")</span></td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物数量:</td>",
        "<td style='text-align: left;'>", summary_info$TotalQuantity[1], "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总货物价值:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalValue[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国内运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalDomesticShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>总国际运费:</td>",
        "<td style='text-align: left;'>￥", formatC(summary_info$TotalIntlShipping[1], format = "f", digits = 2), "</td>",
        "</tr>",
        "<tr>",
        "<td style='color: #007BFF; font-weight: bold; text-align: left;'>合计总价值:</td>",
        "<td style='text-align: left; font-size: 18px; font-weight: bold;'>￥", formatC(total_value_sum, format = "f", digits = 2), "</td>",
        "</tr>",
        "</table>",
        "</div>"
      ))
      
      
      # 创建模态对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #007BFF;'>运单货值汇总</strong>"),
        HTML(summary_text),
        easyClose = TRUE,
        footer = modalButton("关闭")
      ))
    }, error = function(e) {
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })

  # 删除运单逻辑
  observeEvent(input$delete_shipment_btn, {
    tracking_number <- input$intl_tracking_number
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("请输入运单号后再执行此操作！", type = "error",  )
      return()
    }
    
    tryCatch({
      # 检查运单是否存在于 intl_shipments 表中
      shipment_exists <- dbGetQuery(
        con,
        "SELECT COUNT(*) AS count FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (shipment_exists$count == 0) {
        showNotification("运单号不存在，无法删除！", type = "warning")
        return()
      }
      
      # 如果运单存在，弹出确认对话框
      showModal(modalDialog(
        title = HTML("<strong style='color: #C70039;'>确认删除国际运单</strong>"),
        HTML(paste0(
          "<p>您确定要删除国际运单号 <strong>", tracking_number, "</strong> 吗？关联物品的国际运单信息也会被同时清空。此操作不可逆！</p>"
        )),
        easyClose = FALSE,
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_delete_shipment_btn", "确认删除", class = "btn-danger")
        )
      ))
    }, error = function(e) {
      showNotification(paste("检查运单时发生错误：", e$message), type = "error")
    })
  })
  
  
  # 监听确认删除运单按钮的点击事件
  observeEvent(input$confirm_delete_shipment_btn, {
    tracking_number <- trimws(input$intl_tracking_number)
    
    tryCatch({
      # 开始事务
      dbBegin(con)
      
      # 清空 unique_items 表中与运单号相关的运费
      dbExecute(con, "UPDATE unique_items SET IntlShippingCost = 0.00 WHERE IntlTracking = ?", params = list(tracking_number))
      
      # 从 intl_shipments 表中删除对应的运单号 (unique_items表会同时触发运单删除操作)
      dbExecute(con, "DELETE FROM intl_shipments WHERE TrackingNumber = ?", params = list(tracking_number))
      
      # # 删除 transactions 表中与运单号相关的记录
      # dbExecute(con, "DELETE FROM transactions WHERE Remarks LIKE ?", params = list(paste0("%[国际运费登记] 运单号：", tracking_number, "%")))
      
      # 提示删除成功
      showNotification("运单与关联的物品信息已成功删除！", type = "message")
      
      # 重新计算所有balance记录
      update_balance("一般户卡", con)
      
      # 清空输入框和相关字段
      updateTextInput(session, "intl_tracking_number", value = "")
      updateSelectInput(session, "intl_shipping_method", selected = "空运")
      updateNumericInput(session, "intl_total_shipping_cost", value = 0)
      
      # 提交事务
      dbCommit(con)
    }, error = function(e) {
      # 捕获错误并提示用户，回滚事务
      dbRollback(con)
      showNotification(paste("删除失败：", e$message), type = "error")
    })
    
    # 禁用挂靠按钮
    disable("link_tracking_btn")
    
    # 关闭确认对话框
    removeModal()
  })
  
  # 清空填写按钮逻辑
  observeEvent(input$clean_shipment_btn, {
    # 清空输入字段
    updateTextInput(session, "intl_tracking_number", value = "")  # 清空国际运单号
    updateSelectInput(session, "intl_shipping_method", selected = "空运")  # 重置国际运输方式为默认值
    updateNumericInput(session, "intl_total_shipping_cost", value = 0)  # 重置国际物流总运费为 0
    output$intl_status_display <- renderText({ "" })  # 清空状态显示
    
    # 提示用户清空完成
    showNotification("填写内容已清空！", type = "message")
  })
  
  # 点击行自动填写运单号
  observeEvent(unique_items_table_logistics_selected_row(), {
    selected_rows <- unique_items_table_logistics_selected_row()
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      # 如果没有选中行，清空运单号输入框，并禁用挂靠按钮
      updateTextInput(session, "intl_tracking_number", value = "")
      disable("link_tracking_btn")  # 禁用按钮
      disable("unlink_tracking_btn")  # 禁用按钮
      return()
    }
    
    tryCatch({
      # 获取选中行的数据
      selected_data <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 提取所有选中行的国际物流单号（IntlTracking）
      unique_tracking_numbers <- unique(selected_data$IntlTracking)
      
      # 检查选中的物品是否已经挂靠国际运单
      if (any(!is.na(selected_data$IntlTracking))) {
        # 如果所有物品都未挂靠国际运单
        if (length(unique_tracking_numbers) == 1 && !is.na(unique_tracking_numbers)) {
          # 如果只有一个唯一的物流单号，填写到输入框
          updateTextInput(session, "intl_tracking_number", value = unique_tracking_numbers)
          showNotification("已根据选中行填写运单号！", type = "message")
        } else {
          # 如果没有唯一物流单号，取最新点击的那个
          updateTextInput(session, "intl_tracking_number", value = selected_data$IntlTracking[nrow(selected_data)])
        }
        # 如果选中物品中存在已挂靠国际运单的物品
        disable("link_tracking_btn")  # 禁用按钮
        enable("unlink_tracking_btn")  # 启用按钮
      } else {
        # 如果所有物品都未挂靠国际运单
        enable("link_tracking_btn")  # 启用按钮
        disable("unlink_tracking_btn")  # 禁用按钮
      }
    }, error = function(e) {
      # 捕获错误并提示
      disable("link_tracking_btn")  # 禁用按钮
      disable("unlink_tracking_btn")  # 禁用按钮
      showNotification(paste("操作失败：", e$message), type = "error")
    })
  })
  
  
  
  ######################
  ### 挂靠管理分页
  ######################
  
  # 监听页面切换事件
  observeEvent(input$intl_shipment_tabs, {
    if (input$intl_shipment_tabs == "link_management") {
      tryCatch({
        # 查询数据库中状态为“运单新建”的最新运单
        latest_shipment <- dbGetQuery(
          con,
          "SELECT TrackingNumber
         FROM intl_shipments
         WHERE Status = '运单创建'
         ORDER BY CreatedAt DESC
         LIMIT 1"
        )

        if (nrow(latest_shipment) > 0) {
          # 填写到 intl_link_tracking_number
          updateTextInput(session, "intl_link_tracking_number", value = latest_shipment$TrackingNumber[1])
          showNotification("已自动填充最新的‘运单创建’状态的运单号！", type = "message")
        } else {
          # 未找到符合条件的运单
          updateTextInput(session, "intl_link_tracking_number", value = "")
          showNotification("未找到状态为‘运单创建’的运单！", type = "warning")
        }
      }, error = function(e) {
        # 捕获错误并提示
        showNotification(paste("检查运单状态时发生错误：", e$message), type = "error")
      })
    }
  })

  # 监听待挂靠运单号输入
  observeEvent(input$intl_link_tracking_number, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    if (is.null(tracking_number) || tracking_number == "") {
      disable("link_tracking_btn")  # 禁用按钮
      disable("unlink_tracking_btn")  # 禁用按钮
      
      output$intl_link_display <- renderText({
        "请输入运单号以查看运单信息"
      })
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT Status, TotalCost, ShippingMethod, CreatedAt FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        disable("link_tracking_btn")  # 禁用按钮
        disable("unlink_tracking_btn")  # 禁用按钮
        
        output$intl_link_display <- renderText({
          "未找到对应的运单信息，请检查"
        })
        return()
      }
      
      # 显示运单状态和运费
      output$intl_link_display <- renderUI({
        HTML(paste0(
          "物流状态:   ", shipment_info$Status[1], "<br>",
          "运输方式：  ", shipment_info$ShippingMethod[1], "<br>",
          "国际运费:   ￥", format(shipment_info$TotalCost[1], big.mark = ",", nsmall = 2), "<br>",
          "创建日期:   ", format(as.Date(shipment_info$CreatedAt[1]), "%Y-%m-%d")
        ))
      })
    }, error = function(e) {
      output$intl_link_display <- renderText({
        paste("查询运单信息失败：", e$message)
      })
    })
  })
  
  # 挂靠运单号逻辑
  observeEvent(input$link_tracking_btn, {
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要挂靠的物品行！", type = "error")
      return()
    }
    
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      
      # 更新挂靠信息
      dbBegin(con)
      for (i in seq_len(nrow(selected_items))) {
        dbExecute(
          con,
          "UPDATE unique_items SET IntlTracking = ? WHERE UniqueID = ?",
          params = list(tracking_number, selected_items$UniqueID[i])
        )
      }
      
      # 查询挂靠到该运单的所有物品
      related_items <- dbGetQuery(
        con,
        "SELECT UniqueID FROM unique_items WHERE IntlTracking = ?",
        params = list(tracking_number)
      )
      
      if (nrow(related_items) == 0) {
        showNotification("当前运单号没有关联的物品！", type = "warning")
        dbRollback(con)
        return()
      }
      
      # 计算平摊运费
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      total_cost <- as.numeric(shipment_info$TotalCost)
      per_item_cost <- total_cost / nrow(related_items)
      
      # 更新平摊运费
      dbExecute(
        con,
        "UPDATE unique_items SET IntlShippingCost = ? WHERE IntlTracking = ?",
        params = list(per_item_cost, tracking_number)
      )
      dbCommit(con)
      
      showNotification("运单号挂靠成功，平摊运费已更新！", type = "message")
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("挂靠失败：", e$message), type = "error")
    })
  })
  
  # 解除运单号挂靠逻辑
  observeEvent(input$unlink_tracking_btn, {
    selected_rows <- unique_items_table_logistics_selected_row()  # 获取用户选择的物品行
    tracking_number <- input$intl_link_tracking_number  # 获取用户输入的运单号
    
    # 校验用户选择的物品行
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择需要解除挂靠的物品行！", type = "error")
      return()
    }
    
    # 校验运单号
    if (is.null(tracking_number) || tracking_number == "") {
      showNotification("运单号不能为空！", type = "error")
      return()
    }
    
    tryCatch({
      # 查询运单信息
      shipment_info <- dbGetQuery(
        con,
        "SELECT TotalCost FROM intl_shipments WHERE TrackingNumber = ?",
        params = list(tracking_number)
      )
      
      if (nrow(shipment_info) == 0) {
        showNotification("未找到对应的运单信息，请检查输入的运单号！", type = "error")
        return()
      }
      
      # 获取选中行的物品数据
      selected_items <- filtered_unique_items_data_logistics()[selected_rows, ]
      selected_tracking_numbers <- unique(na.omit(selected_items$IntlTracking))
      
      # 开启事务处理
      dbBegin(con)
      
      # 批量解除挂靠并清零运费
      dbExecute(
        con,
          "UPDATE unique_items 
         SET IntlTracking = NULL, IntlShippingCost = 0.00 
         WHERE UniqueID IN (?)",
        params = list(selected_items$UniqueID)
      )
      
      # 重新计算剩余挂靠物品的平摊运费
      dbExecute(
        con, "
        UPDATE unique_items ui
        JOIN (
          SELECT IntlTracking, TotalCost / COUNT(*) AS PerItemCost
          FROM unique_items
          JOIN intl_shipments ON unique_items.IntlTracking = intl_shipments.TrackingNumber
          WHERE IntlTracking IN (?)
          GROUP BY IntlTracking
        ) calc ON ui.IntlTracking = calc.IntlTracking
        SET ui.IntlShippingCost = calc.PerItemCost",

        params = list(selected_tracking_numbers)
      )
      
      # 提交事务
      dbCommit(con)
      
      showNotification("运单号已成功解除挂靠，相关物品的平摊运费已重新计算！", type = "message")
    }, error = function(e) {
      # 回滚事务
      dbRollback(con)
      showNotification(paste("解除挂靠失败：", e$message), type = "error")
    })
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
      label = "选择供应商:",
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

  
  
  ################################################################
  ##                                                            ##
  ## 管理员分页                                                 ##
  ##                                                            ##
  ################################################################
  
  # 管理员登录状态
  admin_logged_in <- reactiveVal(FALSE)
  
  # 监听登录按钮
  observeEvent(input$admin_login_btn, {
    if (input$admin_password == admin_password) {
      admin_logged_in(TRUE)
      showNotification("登录成功！", type = "message")
    } else {
      showNotification("密码错误，请重试！", type = "error")
      admin_logged_in(FALSE)
    }
  })
  
  # 渲染管理员控制
  output$admin_controls <- renderUI({
    if (admin_logged_in()) {
      tagList(
        
        tags$h4("修改库存状态", style = "font-weight: bold; color: #28A745;"),
        
        # 目标状态选择
        selectInput("admin_target_status", "目标库存状态改为：", 
                    choices = c('采购','国内入库','国内出库','国内售出','美国入库','美国发货','美国调货','交易完毕'), 
                    selected = NULL, width = "100%"),
        
        # 是否记录修改时间
        checkboxInput("admin_record_timestamp", "记录修改时间", value = FALSE),
        
        # 更新选中物品状态
        actionButton("admin_update_status_btn", "更新库存状态", class = "btn-success", style = "width: 100%; margin-top: 10px;"),
        
        tags$hr(),
        
        tags$h4("修改瑕疵品状态", style = "font-weight: bold; color: #007BFF;"),
        
        # 目标状态选择
        selectInput("admin_target_defect", "目标瑕疵状态改为：", 
                    choices = c('未知','无瑕','瑕疵','修复'), 
                    selected = NULL, width = "100%"),
        
        # 更新选中物品瑕疵品状态
        actionButton("admin_update_defect_btn", "更新瑕疵品状态", class = "btn-info", style = "width: 100%; margin-top: 10px;"),
        
        div(
          class = "card shadow-sm",
          style = "padding: 15px; border: 1px solid #007BFF; border-radius: 8px; margin-top: 20px;",
          tags$h4("历史库存状态流转记录", style = "color: #007BFF; font-weight: bold; margin-bottom: 10px;"),
          textOutput("selected_item_unique_id"),  # 显示 UniqueID
          DTOutput("item_status_history_table")   # 渲染状态流转表
        )
      )
    } else {
      div(tags$p("请输入密码以访问管理员功能", style = "color: red; font-weight: bold; text-align: center;"))
    }
  })
  
  # 使用 uniqueItemsTableServer 渲染表格
  unique_items_table_admin_selected_row <- callModule(uniqueItemsTableServer, "admin_items_table", 
                                                      column_mapping = c(common_columns, list(
                                                        Defect = "瑕疵态",
                                                        urchaseTime = "采购日",
                                                        DomesticEntryTime = "入库日",
                                                        DomesticExitTime = "出库日",
                                                        DomesticSoldTime = "售出日",
                                                        UsEntryTime = "美入库日",
                                                        UsRelocationTime = "美调货日",
                                                        UsShippingTime = "美发货日",
                                                        OrderID = "订单号"
                                                      )), 
                                                      selection = "multiple",
                                                      option = modifyList(table_default_options, list(searching = TRUE)),
                                                      data = unique_items_data)
  
  # 更新库存状态按钮
  observeEvent(input$admin_update_status_btn, {
    req(input$admin_target_status, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取是否记录修改时间的选项
      record_timestamp <- input$admin_record_timestamp
      
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        new_status <- input$admin_target_status
        
        # 调用 update_status 更新物品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = new_status,
          refresh_trigger = NULL,
          update_timestamp = record_timestamp  # 使用用户选择的值
        )
      })
      
      # 通知成功并刷新数据
      showNotification("库存状态更新成功！", type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("库存状态更新失败：", e$message), type = "error")
    })
  })
  
  # 更新瑕疵状态按钮
  observeEvent(input$admin_update_defect_btn, {
    req(input$admin_target_defect, unique_items_table_admin_selected_row())
    
    # 获取选中行的索引
    selected_rows <- unique_items_table_admin_selected_row()
    selected_items <- unique_items_data()[selected_rows, ]
    
    # 确保有选中物品
    if (nrow(selected_items) == 0) {
      showNotification("请选择至少一个物品进行瑕疵品状态更新！", type = "error")
      return()
    }
    
    tryCatch({
      # 遍历选中物品
      lapply(1:nrow(selected_items), function(i) {
        unique_id <- selected_items$UniqueID[i]
        target_defect <- input$admin_target_defect  # 获取目标瑕疵品状态
        
        # 调用 update_status 更新瑕疵品状态
        update_status(
          con = con,
          unique_id = unique_id,
          new_status = NULL,  # 不更新物品状态
          defect_status = target_defect,  # 更新瑕疵品状态
          refresh_trigger = NULL
        )
      })
      
      # 通知成功并刷新数据
      showNotification("瑕疵品状态更新成功！", type = "message")
      
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification(paste("瑕疵品状态更新失败：", e$message), type = "error")
    })
  })
  
  # 监听表格选中行，获取 UniqueID
  observeEvent(unique_items_table_admin_selected_row(), {
    selected_rows <- unique_items_table_admin_selected_row()
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      updateTextOutput(session, "selected_item_unique_id", value = "未选择")
      output$item_status_history_table <- renderDT({ data.frame() })  # 清空表格
      return()
    }
    
    selected_item <- unique_items_data()[selected_rows, ]
    unique_id <- selected_item$UniqueID[length(selected_rows)]
    
    # 显示 UniqueID
    output$selected_item_unique_id <- renderText({ unique_id })
    
    # 查询该物品的状态历史
    status_history <- dbGetQuery(con, 
                                 "SELECT previous_status AS 'Status', previous_status_timestamp AS 'Time' 
                                  FROM item_status_history 
                                  WHERE UniqueID = ? 
                                  ORDER BY previous_status_timestamp",
                                 params = list(unique_id))
    
    # 格式化时间列
    if (nrow(status_history) > 0) {
      status_history$Time <- format(as.POSIXct(status_history$Time, format = "%Y-%m-%dT%H:%M:%SZ"))
    }
    
    # 渲染状态历史表格
    output$item_status_history_table <- renderDT({
      datatable(status_history, rownames = FALSE, options = list(
        searching = FALSE, paging = FALSE, info = FALSE
      ))
    })
  })
  
  #########################################################################################################################
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}
