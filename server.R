#source("util/snowflake_connection.R")

function(input, output, session){
  vals<-reactiveValues()
  vals$bankaccounts<-data.table()
  vals$breakdown<-data.table()
  vals$amort_detail<-data.table()
  vals$series_detail<-data.table()
  vals$schedule<-data.table()
  vals$all_detail<-data.table()
  vals$frequency<-data.table()
  
  output$season <- renderUI({
    req(input$tvshow)
    selectInput("seasonnumber", "Season Number:", choices =c("ALL",unique(showproject[TITLE==input$tvshow][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  observeEvent(input$breakdown_table_cell_clicked,{
    req(length(input$breakdown_table_cell_clicked)>0)
    row<-input$breakdown_table_cell_clicked$row
    col<-input$breakdown_table_cell_clicked$col
    dat<-vals$breakdown
    # print(dat)
    # print(row)
    # print(col)
    if(col==0){
      sign<-dat[row][[(col+1)]]
      print(sign)
      if(sign=="+"){
        # groupname<-vals$breakdown[row]$Category
        # BudgetDetail_account <- merge(vals$series_detail[GROUPNAME==groupname][,.(Pattern=sum(AMOUNT)), by =.(ACCOUNTCODE,ACCOUNTNAME)],
        #                               vals$amort_detail[GROUPNAME==groupname][,.(Amorts=sum(AMOUNT)), by =.(ACCOUNTCODE,ACCOUNTNAME)],
        #                               all=TRUE)
        # BudgetDetail_account[,Total:=Pattern*input$episode+Amorts]
        # colnames(BudgetDetail_account)[1]<-'AccountCode'
        # colnames(BudgetDetail_account)[2]<-'Category'
        # BudgetDetail_account<-cbind(" "="", BudgetDetail_account,vals$bankaccounts)
        dat[row][[1]]<-"-"
        code<-as.numeric(dat[row]$AccountCode)
        code_next<-as.numeric(dat[row+1]$AccountCode)
        if(is.na(code_next)){code_next<-as.numeric(dat[row+2]$AccountCode)}
        vals$breakdown<-rbind(dat[1:row],vals$all_detail[AccountCode>code&AccountCode<code_next],dat[(row+1):nrow(dat)])
      }
      else if(sign=='-'){
        dat[row][[1]]<-"+"
        code<-as.numeric(dat[row]$AccountCode)
        code_next<-as.numeric(dat[row+1]$AccountCode)
        group_next<-code+99
        row_next<-min(dat[as.numeric(AccountCode)>group_next,which = TRUE])
        if(dat[row_next-1]$AccountCode==""){row_next<-row_next-1}
        print(row_next)
        vals$breakdown<-rbind(dat[1:row],dat[row_next:nrow(dat)])
      }
      else{}
    }
    else{}
  })
  
  observe({
    validate(need(input$seasonnumber!="ALL", "Please choose a season"))
    vals$amort_detail<-getAmort(input$tvshow,input$seasonnumber)
    vals$series_detail<-getSeries(input$tvshow,input$seasonnumber)
  })
  
  observeEvent(input$nextstep_1,{
    updateTabItems(session, "tab", selected = "budget_breakdown")
  })
  
  observeEvent(input$laststep_2,{
    updateTabItems(session, "tab", selected = "amorts")
  })
  
  observeEvent(input$nextstep_2,{
    validate(need(input$start!=input$end, "End Date and Start Date can't be the same"))
    updateTabItems(session, "tab", selected = "cashflow_forecast")
  })
  
  observeEvent(input$laststep_3,{
    updateTabItems(session, "tab", selected = "budget_breakdown")
  })
  
  output$amort_table<-renderDT({
    validate(need(input$seasonnumber!="ALL", "Please choose a season"))
    #vals$amort_detail<-getAmort(input$tvshow,input$seasonnumber)
    dat<-spread(vals$amort_detail[,.(AMOUNT=sum(AMOUNT)), by =.(AMORTTYPE,ROLENAME)],AMORTTYPE,AMOUNT)
    amort_type<-unique(vals$amort_detail[order(AMORTTYPE)]$AMORTTYPE)
    add_type<-setNames(data.table(matrix(nrow = 0, ncol = length(amort_type))), amort_type)
    dat<-select(dat,c("ROLENAME",amort_type))
    dat[,TotalAmort:=rowSums(dat[,-1],na.rm = TRUE)]
    subdat_list<-list()
    #dat$ROLENAME<-factor(dat$ROLENAME,levels=c("Above The Line","Production","Post Production","Other Costs","Other Costs 2"))
    rolenames<-intersect(c("Above The Line","Production","Post Production","Other Costs","Other Costs 2"), unique(dat$ROLENAME))
    for(rolename in rolenames){
      BudgetDetail_sum_group <- vals$amort_detail[ROLENAME==rolename][,.(AMOUNT=sum(AMOUNT)), by =.(AMORTTYPE,GROUPNAME)][order(AMORTTYPE)]
      BudgetDetail_group <- spread(BudgetDetail_sum_group,AMORTTYPE,AMOUNT)
      BudgetDetail_group<-rbind(BudgetDetail_group, add_type,fill=TRUE)
      BudgetDetail_group<-select(BudgetDetail_group,c("GROUPNAME",amort_type))
      BudgetDetail_group[,TotalAmort:=rowSums(BudgetDetail_group[,-1],na.rm = TRUE)]
      subsubdat_list<-list()
      for(groupname in unique(BudgetDetail_group$GROUPNAME)){
        BudgetDetail_sum_account <- vals$amort_detail[GROUPNAME==groupname][,.(AMOUNT=sum(AMOUNT)), by =.(AMORTTYPE,ACCOUNTNAME)][order(AMORTTYPE)]
        BudgetDetail_account <- spread(BudgetDetail_sum_account,AMORTTYPE,AMOUNT)
        BudgetDetail_account<-rbind(BudgetDetail_account, add_type,fill=TRUE)
        BudgetDetail_account<-select(BudgetDetail_account,c("ACCOUNTNAME",amort_type))
        BudgetDetail_account[,TotalAmort:=rowSums(BudgetDetail_account[,-1],na.rm = TRUE)]
        subsubdat_list<-list.append(subsubdat_list,BudgetDetail_account)
      }
      subsubdats<-lapply(subsubdat_list,purrr::transpose)
      subdat_i<-cbind(" " = "&oplus;", BudgetDetail_group, "_details" = I(subsubdats))
      subdat_list<-list.append(subdat_list,subdat_i)
    }
    subdats <- lapply(subdat_list, purrr::transpose)
    Dat <- cbind(" " = "&oplus;", dat, "_details" = I(subdats))
    callback = JS(
      "table.column(1).nodes().to$().css({cursor: 'pointer'});",
      "",
      "// make the table header of the nested table",
      "var format = function(d, childId){",
      "  if(d != null){",
      "    var html = ", 
      "      '<table class=\"display compact hover\" id=\"' + childId + '\"><thead><tr>';",
      "    for (var key in d[d.length-1][0]) {",
      "      html += '<th>' + key + '</th>';",
      "    }",
      "    html += '</tr></thead></table>'",
      "    return html;",
      "  } else {",
      "    return '';",
      "  }",
      "};",
      "",
      "// row callback to style the rows of the child tables",
      "var rowCallback = function(row, dat, displayNum, index){",
      "  if($(row).hasClass('odd')){",
      "    $(row).css('background-color', 'papayawhip');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#E6FF99');",
      "    }, function() {",
      "      $(this).css('background-color', 'papayawhip');",
      "    });",
      "  } else {",
      "    $(row).css('background-color', 'lemonchiffon');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#DDFF75');",
      "    }, function() {",
      "      $(this).css('background-color', 'lemonchiffon');",
      "    });",
      "  }",
      "};",
      "",
      "// header callback to style the header of the child tables",
      "var headerCallback = function(thead, data, start, end, display){",
      "  $('th', thead).css({",
      "    'border-top': '3px solid indigo',", 
      "    'color': 'indigo',",
      "    'background-color': '#fadadd'",
      "  });",
      "};",
      "",
      "// make the datatable",
      "var format_datatable = function(d, childId){",
      "  var dataset = [];",
      "  var n = d.length - 1;",
      "  for(var i = 0; i < d[n].length; i++){",
      "    var datarow = $.map(d[n][i], function (value, index) {",
      "      return [value];",
      "    });",
      "    dataset.push(datarow);",
      "  }",
      "  var id = 'table#' + childId;",
      "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
      "    var subtable = $(id).DataTable({",
      "                 'data': dataset,",
      "                 'autoWidth': true,",
      "                 'deferRender': true,",
      "                 'info': false,",
      "                 'lengthChange': false,",
      "                 'ordering': d[n].length > 1,",
      "                 'order': [],",
      "                 'paging': false,",
      "                 'scrollX': false,",
      "                 'scrollY': false,",
      "                 'searching': false,",
      "                 'sortClasses': false,",
      "                 'rowCallback': rowCallback,",
      "                 'headerCallback': headerCallback,",
      "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
      "               });",
      "  } else {",
      "    var subtable = $(id).DataTable({",
      "            'data': dataset,",
      "            'autoWidth': true,",
      "            'deferRender': true,",
      "            'info': false,",
      "            'lengthChange': false,",
      "            'ordering': d[n].length > 1,",
      "            'order': [],",
      "            'paging': false,",
      "            'scrollX': false,",
      "            'scrollY': false,",
      "            'searching': false,",
      "            'sortClasses': false,",
      "            'rowCallback': rowCallback,",
      "            'headerCallback': headerCallback,",
      "            'columnDefs': [", 
      "              {targets: -1, visible: false},", 
      "              {targets: 0, orderable: false, className: 'details-control'},", 
      "              {targets: '_all', className: 'dt-center'}",
      "             ]",
      "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
      "  }",
      "};",
      "",
      "// display the child table on click",
      "table.on('click', 'td.details-control', function(){",
      "  var tbl = $(this).closest('table'),",
      "      tblId = tbl.attr('id'),",
      "      td = $(this),",
      "      row = $(tbl).DataTable().row(td.closest('tr')),",
      "      rowIdx = row.index();",
      "  if(row.child.isShown()){",
      "    row.child.hide();",
      "    td.html('&oplus;');",
      "  } else {",
      "    var childId = tblId + '-child-' + rowIdx;",
      "    row.child(format(row.data(), childId)).show();",
      "    td.html('&CircleMinus;');",
      "    format_datatable(row.data(), childId);",
      "  }",
      "});")
    datatable(Dat, callback = callback, escape = -2, selection="none",
              options = list(
                columnDefs = list(
                  list(visible = FALSE, targets = ncol(Dat)),
                  list(orderable = FALSE, className = 'details-control', targets = 1),
                  list(className = "dt-center", targets = "_all")
                )
              )
    )
  })
  
  observe({
    validate(need(input$seasonnumber!="ALL", "Please choose a season"))
    vals$amort_detail[,AccountCode:=paste0(substr(ACCOUNTCODE,1,2),'00')]
    vals$series_detail[,AccountCode:=paste0(substr(ACCOUNTCODE,1,2),'00')]
    BudgetDetail_role<-merge(vals$series_detail[,.(Pattern=sum(AMOUNT)), by =.(ROLENAME)],
                             vals$amort_detail[,.(Amorts=sum(AMOUNT)), by =.(ROLENAME)],
                             all = TRUE)
    BudgetDetail_role[,Total:=Pattern*input$episode+Amorts]
    colnames(BudgetDetail_role)[1]<-'Category'
    BudgetDetail_role<-cbind(" " = "","AccountCode" = "",BudgetDetail_role,vals$bankaccounts)
    dat<-data.table()
    rolenames<-intersect(c("Above The Line","Production","Post Production","Other Costs","Other Costs 2"), unique(BudgetDetail_role$Category))
    for (rolename in rolenames){
      dat<-rbind(dat,cbind(BudgetDetail_role[Category==rolename]))
      BudgetDetail_group <- merge(vals$series_detail[ROLENAME==rolename][,.(Pattern=sum(AMOUNT)), by =.(AccountCode,GROUPNAME)],
                                  vals$amort_detail[ROLENAME==rolename][,.(Amorts=sum(AMOUNT)), by =.(AccountCode,GROUPNAME)],
                                  all=TRUE)
      BudgetDetail_group[is.na(BudgetDetail_group$Pattern)]$Pattern<-0      
      BudgetDetail_group[is.na(BudgetDetail_group$Amorts)]$Amorts<-0
      BudgetDetail_group[,Total:=Pattern*input$episode+Amorts]
      colnames(BudgetDetail_group)[2]<-'Category'
      BudgetDetail_group<-cbind(" " = "+",BudgetDetail_group,vals$bankaccounts)
      dat<-rbind(dat,BudgetDetail_group)
      for(groupname in unique(BudgetDetail_group$Category)){
        BudgetDetail_account <- merge(vals$series_detail[GROUPNAME==groupname][,.(Pattern=sum(AMOUNT)), by =.(ACCOUNTCODE,ACCOUNTNAME)],
                                      vals$amort_detail[GROUPNAME==groupname][,.(Amorts=sum(AMOUNT)), by =.(ACCOUNTCODE,ACCOUNTNAME)],
                                      all=TRUE)
        BudgetDetail_account[is.na(BudgetDetail_account$Pattern)]$Pattern<-0      
        BudgetDetail_account[is.na(BudgetDetail_account$Amorts)]$Amorts<-0
        BudgetDetail_account[,Total:=Pattern*input$episode+Amorts]
        colnames(BudgetDetail_account)[1:2]<-c('AccountCode','Category')
        BudgetDetail_account<-cbind(" " = "",BudgetDetail_account,vals$bankaccounts)
        dat<-rbind(dat,BudgetDetail_account)
      }
    }
    vals$all_detail<-dat
    vals$breakdown<-dat[dat$` `=="+" | dat$AccountCode==""]
  })
  
  output$breakdown_table<-renderDT(
    vals$breakdown, rownames=FALSE, selection="none", 
    editable = list(target = 'cell', disable = list(columns = seq(0,5))),
    callback=JS(js),
    extensions="KeyTable",
    options=list(keys=TRUE))
  
  js <- c(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  var keys = [9,13,37,38,39,40];",
    "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  if(type == 'keydown' && targetName == 'input'){",
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
  )
  
  proxy = dataTableProxy('breakdown_table')
  
  observeEvent(input$breakdown_table_cell_edit, {
    info = input$breakdown_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    vals$breakdown[is.na(vals$breakdown)]<-0
    vals$all_detail[is.na(vals$all_detail)]<-0
    accountcode<-vals$breakdown[i]$AccountCode
    row<-vals$all_detail[AccountCode==accountcode,which=TRUE]
    vals$all_detail[row][[j+1]] <- as.numeric(v)
    vals$breakdown[i][[j+1]] <- as.numeric(v)
    vals$all_detail[AccountCode==paste0(as.numeric(accountcode)%/%100,"00")][[j+1]]<-vals$all_detail[AccountCode==paste0(as.numeric(accountcode)%/%100,"00")][[j+1]]+as.numeric(v)
    vals$breakdown[AccountCode==paste0(as.numeric(accountcode)%/%100,"00")][[j+1]]<-vals$breakdown[AccountCode==paste0(as.numeric(accountcode)%/%100,"00")][[j+1]]+as.numeric(v)
    vals$all_detail[1:row][AccountCode==''][[j+1]]<-vals$all_detail[1:row][AccountCode==''][[j+1]]+as.numeric(v)
    vals$breakdown[1:row][AccountCode==''][[j+1]]<-vals$breakdown[1:row][AccountCode==''][[j+1]]+as.numeric(v)
    # print(vals$breakdown)
    replaceData(proxy, vals$breakdown, resetPaging = FALSE)
  })
  # observe({
  #   dat_all<-cbind(vals$breakdown[,1:3],vals$breakdown[,.(Total)],"Frequency"='', matrix(nrow=nrow(vals$breakdown),ncol=nrow(vals$schedule)))
  #   dat<-dat_all[!is.na(Total)]
  #   colnames(dat)[6:ncol(dat)]<-as.Date(vals$schedule$Week)
  #   for(col in 6:ncol(dat)){
  #     dat[,col]<-dat$Total/(ncol(dat)-6)*dat$Frequency
  #   }
  #   dat
  # })
  
  observeEvent(input$add,{
    dt<-data.table(matrix(nrow=1,ncol=1))
    colnames(dt)<-paste(input$account,input$currency)
    vals$bankaccounts<-cbind(vals$bankaccounts,dt)
    appendTab(inputId = "mainTabset_sum",
              select = FALSE,
              tabPanel(paste(input$account,input$currency), DTOutput(paste0("table_", input$account))
              )
    )
    account<-input$account
    currency<-input$currency
    output[[paste0("table_", account)]] <- renderDT({
      dat_all<-cbind(vals$all_detail[,1:3],"Frequency"=as.numeric(),vals$all_detail[[paste(account,currency)]], matrix(0,nrow=nrow(vals$all_detail),ncol=nrow(vals$schedule)))
      print(dat_all)
      colnames(dat_all)[5]<-"Total"
      colnames(dat_all)[6:ncol(dat_all)]<-as.character(vals$schedule$Week)
      dat<-dat_all[!is.na(Total)][AccountCode!=""&` `!="+"][,2:ncol(dat_all)]
      dat$Frequency<-1
      #dat[,5:ncol(dat)]<-round(dat$Total/(ncol(dat)-5)*dat$Frequency,0)
      for(row in 1:nrow(dat)){
        for(col in 5:ncol(dat)){
          if((col-4)%%dat[row]$Frequency==0){
            dat[row][[col]]<-round(dat[row]$Total/(ncol(dat)-5)*dat[row]$Frequency,0)
          }
        }
      }
      vals[[account]]<-rbind(cbind(data.table(AccountCode="",Category="Total",Frequency="",Total=""),t(colSums(dat[,5:ncol(dat)]))),dat)
      vals[[account]][1]$Frequency<-""
      #print(vals[[account]])
      vals[[account]]
    },rownames=FALSE, selection="none", 
    editable = list(target = 'cell', disable = list(columns = c(0,1,3))),
    callback=JS(js),
    extensions="KeyTable",
    options=list(keys=TRUE,scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  # proxy1<- dataTableProxy(paste0("table_",input$account))
  # 
  # observeEvent(input[[paste0("table_",input$account,"_cell_edit")]], {
  #   info = input[[paste0(input$account,"_cell_edit")]]
  #   str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   vals[[input$account]][i][[j+1]] <- as.numeric(v)
  #   # print(vals$breakdown)
  #   replaceData(proxy1, vals[[input$account]], resetPaging = FALSE)
  # })

  
  # observeEvent(input[[paste0("table_",input$account,"_cell_clicked")]],{
  #   req(length(input[[paste0("table_",input$account,"_cell_clicked")]])>0)
  #   print("Yes")
  #   vals[[input$account]]<-cbind(vals$all_detail[,1:3],vals$all_detail[[input$account]],"Frequency"='', matrix(nrow=nrow(vals$breakdown),ncol=nrow(vals$schedule)))
  #   row<-input$breakdown_table_cell_clicked$row
  #   col<-input$breakdown_table_cell_clicked$col
  #   dat<-vals$breakdown
  #   print(dat)
  #   print(row)
  #   print(col)
  #   if(col==0){
  #     sign<-dat[row][[(col+1)]]
  #     print(sign)
  #     if(sign=="+"){
  #       dat[row][[1]]<-"-"
  #       code<-as.numeric(dat[row]$AccountCode)
  #       code_next<-as.numeric(dat[row+1]$AccountCode)
  #       if(is.na(code_next)){code_next<-as.numeric(dat[row+2]$AccountCode)}
  #       vals$breakdown<-rbind(dat[1:row],vals$all_detail[AccountCode>code&AccountCode<code_next],dat[(row+1):nrow(dat)])
  #     }
  #     else if(sign=='-'){
  #       dat[row][[1]]<-"+"
  #       code<-as.numeric(dat[row]$AccountCode)
  #       code_next<-as.numeric(dat[row+1]$AccountCode)
  #       print(code)
  #       print(code_next)
  #       group_next<-code+99
  #       row_next<-min(dat[as.numeric(AccountCode)>group_next,which = TRUE])
  #       if(dat[row_next-1]$AccountCode==""){row_next<-row_next-1}
  #       print(row_next)
  #       vals$breakdown<-rbind(dat[1:row],dat[row_next:nrow(dat)])
  #     }
  #     else{}
  #   }
  #   else{}
  # })
  
  output$schedule<-renderDT({
    vals$schedule<-data.table(Week=seq(as.Date(input$start), by=7, len=as.integer(difftime(input$end,input$start,units = "weeks"))),
                              Type="")
    vals$schedule
    #print(vals$schedule)
  },rownames=FALSE,editable=TRUE)
  
  # output$frequency<-renderDT({
  #   vals$frequency<-
  #   vals$frequency
  # },rownames=FALSE,editable=TRUE)
  
  }