# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shiny.fluent)

library(DBI)
library(RMariaDB)
library(tidyverse)
library(DT)
library(stringi)
library(baRcodeR)
library(plotly)
library(networkD3)
library(openxlsx)
library(future)

# Source shared module R file
files <- list.files("/srv/shiny-server/erp-module", pattern = "\\.R$", full.names = TRUE)
lapply(files, function(f) {
  source(f, local = FALSE)  # 确保加载到全局环境
})

system_type <<- "cn"

# 定义轮询间隔（以毫秒为单位）
poll_interval <<- 5000  # 每 5 秒检查一次

# 主机URL
host_url <<- "https://www.goldenbeanllc.com/"

# 通用物品表的列名
placeholder_300px_path <<- "https://dummyimage.com/300x300/cccccc/000000.png&text=No+Image"
placeholder_200px_path <<- "https://dummyimage.com/200x200/cccccc/000000.png&text=No+Image"
placeholder_150px_path <<- "https://dummyimage.com/150x150/cccccc/000000.png&text=No+Image"
placeholder_50px_path <<- "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image"

# Size of barcode paper (in cm)
page_width <<- 4
page_height <<- 2
size_unit <<- "cm"

status_levels <<- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国调货", "美国发货", "交易完毕")
status_colors <<- c("#D3D3D3", "#C7E89B", "#9CA695", "#46A80D", "#6F52FF", "#529AFF", "#FAF0D4", "#F4C7FC")

# 协作页面映射关系
request_types <<- list(
  "新品" = "new_product_board",
  "采购" = "purchase_request_board",
  "安排" = "provider_arranged_board",
  "完成" = "done_paid_board",
  "出库" = "outbound_request_board"
)

# 通用物品表显示列
common_columns <<- list(
  SKU = "条形码",
  ItemName = "商品名",
  ItemImagePath = "商品图",
  Maker = "供应商",
  ProductCost = "单价",
  Status = "库存态"
)

# 通用账务表列名映射
transaction_common_columns <<- list(
  TransactionTime = "转账时间",
  TransactionType = "转账类别",
  AmountIn = "转入金额",
  AmountOut = "转出金额",
  Balance = "当前余额",
  TransactionImagePath = "转账截图",
  Remarks = "备注"
)

# 通用订单表列名映射
orders_table_columns <<- list(
  OrderID = "订单号",
  OrderImagePath = "订单图",
  CustomerName = "姓名",
  CustomerNetName = "网名",
  Platform = "平台",
  UsTrackingNumber = "运单号",
  LabelStatus = "运单PDF",
  OrderStatus = "状态",
  OrderNotes = "备注",
  created_at = "创建时间"
)

# 定义需要记录时间的状态
status_columns <<- list(
  "采购" = "PurchaseTime",
  "国内入库" = "DomesticEntryTime",
  "国内出库" = "DomesticExitTime",
  "国内售出" = "DomesticSoldTime",
  "美国入库" = "UsEntryTime",
  "美国发货" = "UsShippingTime",
  "美国调货" = "UsRelocationTime",
  "交易完毕" = "CompleteTime"
)

# 定义默认表格渲染选项
table_default_options <<- list(
  scrollY = "730px",
  scrollX = TRUE,
  fixedHeader = TRUE,
  paging = TRUE,
  pageLength = 30,
  dom = 'ftip',
  searching = FALSE,
  language = list(search = "搜索：")
)

# 账户余额卡片
accounts <<- list(
  list(name = "买货卡 (139)", outputId = "purchase_balance", gradient = "linear-gradient(135deg, #FFC107, #FF9800)"),
  list(name = "一般户卡 (541)", outputId = "general_balance", gradient = "linear-gradient(135deg, #6C757D, #495057)"),
  list(name = "工资卡 (567)", outputId = "salary_balance", gradient = "linear-gradient(135deg, #007BFF, #0056b3)"),
  list(name = "美元卡 (553)", outputId = "dollar_balance", gradient = "linear-gradient(135deg, #28A745, #1E7E34)")
)

# 定义瑕疵和修复的状态
defect_statuses <<- c("瑕疵", "修复", "无瑕")

# 定义管理员密码
admin_password <<- "1029"
