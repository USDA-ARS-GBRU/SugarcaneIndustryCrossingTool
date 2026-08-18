properties_server <- function(input, output, session, reactive_iid, inventory_init, clone_assignments) {
  properties_init <- eventReactive(input$makeproperties, {
    withProgress(message = "Pulling Property Data", {
      tryCatch({
        req(inventory_init())

        germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
        germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

        if (nrow(germplasm) == 0) {
          stop("No inventory data available")
        }

        display <- c(input$male_list, input$female_list)
        if (length(display) == 0) {
          stop("No clones sorted into male/female categories")
        }

        germplasm <- germplasm[which(germplasm$Clone %in% display), ]
        
        studies_list<-jsonlite::fromJSON(ba_studies(brap2, trialDbId = reactive_iid(), pageSize=2000, rclass="json"))$result$data
        
        tmp=data.frame()
        
        for(i in 1:dim(studies_list)[1]){
          
          studyDbId=as.character(studies_list[i,"studyDbId"])
          
        tmp <- stripClass(
          as.data.frame(
            ba_germplasm_details2(con = brap2, germplasmQuery = as.character(paste0("?studyDbId=", reactive_iid(), "&pageSize=2000")), rclass = "data.frame")
          ),
          classString = "ba_germplasm_details"
        )
        
        }
        col_additional_props<-grep("additionalProps", colnames(tmp), value=TRUE)
        
        properties <- tmp[tmp$data.germplasmName %in% germplasm$Clone, c("data.germplasmName", "data.germplasmDbId", col_additional_props)] %>%
          dplyr::rename(Clone = data.germplasmName) %>% 
          dplyr::select(!data.germplasmDbId)
        
       colnames(properties)<-gsub("data.additionalInfo.additionalProps.","", colnames(properties))
        
        return(properties)
        
      } , error = function(e) {
        showNotification(paste("Error getting property data:", e$message), type = "error", duration = NULL)
        return(data.frame(Clone = character(), Property = character()))
      }, warning = function(w) {
        showNotification(paste("Warning:", w$message), type = "warning")
      })
    })
  })


  output$propertiesTable <- renderDT(
    {
      tryCatch(
        {
          req(properties_init())
          properties_init()
        },
        error = function(e) {
          data.frame()
        }
      )
    },
    options = list(language = list(
      zeroRecords = "No clone properties records available. Check inventory records for selected date."
    ))
  )
}