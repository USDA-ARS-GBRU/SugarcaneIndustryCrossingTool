#Pedigree.R

## Pedigree and Progeny ----
pedigree_server <- function(input, output, session, reactive_iid, selectedClone, inventory_init, clone_assignments) {

  pedigree_init <- eventReactive(input$makepedigree, {
    withProgress(message = "Pulling Progeny Data", {
      tryCatch({
        # Validate inputs
        req(inventory_init())
           germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
           germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
    
        if(nrow(germplasm) == 0) {
          stop("No inventory data available")
        }
        
        # Only show sorted clones
        display <- c(input$male_list, input$female_list)
        if(length(display) == 0) {
          stop("No clones sorted into male/female categories")
        }
        
        germplasm <- germplasm[which(germplasm$Clone %in% display),]
        
        studies_list<-jsonlite::fromJSON(ba_studies(brap2, trialDbId = reactive_iid(), pageSize=2000, rclass="json"))$result$data
        
        tmp=data.frame()
        
        for(i in 1:dim(studies_list)[1]){
          studyDbId=as.character(studies_list[i,"studyDbId"])
          
         germ_details <- stripClass(
          as.data.frame(
            ba_germplasm_details2(con = brap2, germplasmQuery = as.character(paste0("?studyDbId=", studyDbId, "&pageSize=2000")), rclass = "data.frame")
          ),
          classString = "ba_germplasm_details"
        )
          
          if(inherits(germ_details$x, "try-error")){
            tmp<-tmp
          } else {
            tmp<-bind_rows(tmp, germ_details )
          } 
          
        }

        pedigree <- tmp[tmp$data.germplasmName %in% germplasm$Clone, c("data.germplasmName", "data.germplasmDbId", "data.pedigree")] %>%
          dplyr::rename(Clone = data.germplasmName, Pedigree = data.pedigree)

        pedigree<-pedigree[unique(pedigree$Clone), ]
        
        #note: could rewrite ba_germplam_progeny to speed performance
        
        for (i in 1:dim(pedigree)[1]) {
          pedigree[i, 4] <-
            fromJSON(brapi::ba_germplasm_progeny(con = brap, germplasmDbId = as.character(pedigree[i, 2]), rclass = "json"))$metadata$pagination$totalCount
        }

        colnames(pedigree)[4] <- "Number.Progeny"
      
      
        pedigree<-pedigree[,-which(colnames(pedigree) == "data.germplasmDbId")]
        
        return(pedigree)
      }, error = function(e) {
        showNotification(paste("Error getting pedigree data:", e$message), type = "error", duration = NULL)
        return(data.frame(Clone = character(), Pedigree = character(), Number.Progeny = numeric()))
      }, warning = function(w) {
        showNotification(paste("Warning:", w$message), type = "warning")
      })
    })
  })

  deeppedigree_init <- eventReactive(input$selectedClone, {
    tryCatch({
      req(input$selectedClone)
      
      # Get pedigree data
      germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
      clone_id <- germplasm[which(germplasm$Clone == input$selectedClone), "germplasmDbId"]
      
      if (length(clone_id) == 0) {
        showNotification("Selected clone not found in inventory", type = "warning")
        return(NULL)
      }
      
      tmp <- jsonlite::fromJSON(
        ba_germplasm_pedigree(
          con = brap2, 
          germplasmDbId = as.character(clone_id), 
          rclass = "json"
        )
      )$result$data
      
      # Validate pedigree data
      if (is.null(tmp) || nrow(tmp) == 0 || 
          (is.list(tmp$parents) && length(tmp$parents[[1]]) == 0)) {
        showNotification("No pedigree data available for selected clone", type = "warning")
        return(NULL)
      }
      
      return(tmp)
    }, error = function(e) {
      showNotification(paste("Error getting detailed pedigree:", e$message), type = "error")
      return(NULL)
    })
  })

  pedmatrix_init <- eventReactive(input$makepedigree, {
    tryCatch({
      req(input$male_list, input$female_list)
      germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
      germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

      # Add validation checks
      if (length(input$male_list) == 0 || length(input$female_list) == 0) {
        showNotification("Please select both male and female parents", type = "warning")
        return(data.frame())
      }

      # Create relationship matrix with error handling
      mat <- tryCatch({
        parent_amat
      }, error = function(e) {
        showNotification(paste("Error creating relationship matrix:", e$message), type = "error")
        return(matrix(nrow = 0, ncol = 0))
      })

      if (nrow(mat) == 0) {
        return(data.frame())
      }

      # Check if selected parents exist in the matrix
      valid_males <- input$male_list[input$male_list %in% rownames(mat)]
      valid_females <- input$female_list[input$female_list %in% colnames(mat)]

      if (length(valid_males) == 0 || length(valid_females) == 0) {
        showNotification("Selected parents not found in pedigree data", type = "warning")
        return(data.frame())
      }

      # Filter matrix for selected parents and ensure numeric values
      mat2 <- tryCatch({
        # Subset matrix using only valid parents
        result <- round(as.numeric(mat[valid_males, valid_females]), 2)
        
        # Convert to data frame and add Clone column
        if (length(valid_males) == 1 || length(valid_females) == 1) {
          # Handle case when result is a vector
          result_df <- data.frame(
            Clone = valid_males,
            stringsAsFactors = FALSE
          )
          result_df[[valid_females[1]]] <- result
        } else {
          result_df <- as.data.frame(matrix(
            result,
            nrow = length(valid_males),
            ncol = length(valid_females),
            dimnames = list(valid_males, valid_females)
          ))
          result_df$Clone <- valid_males
        }
        
        # Ensure Clone is first column
        result_df <- result_df[, c("Clone", setdiff(names(result_df), "Clone"))]
        
        result_df
      }, error = function(e) {
        showNotification(paste("Error processing relationship matrix:", e$message), type = "error")
        return(data.frame())
      })

      return(mat2)
    }, error = function(e) {
      showNotification(paste("Error in pedmatrix_init:", e$message), type = "error")
      return(data.frame())
    })
  })

  #get rid of this for now - LA secific
  # output$pedigreeTable <- ({
  #   renderDT(merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
  #     by = "Clone"
  #   ) %>% rename(Rel.2.LCP850384 = "LCP85-0384"), options = list(language = list(
  #     zeroRecords = "There are no pedigree records to display. Double check that there are inventory records for the date you selected"
  #   )))
    
    
    output$pedigreeTable <- renderDT({
      tryCatch({
        req(pedigree_init())
        pedigree_init()
      }, error = function(e) {
        data.frame()
      })
    }, options = list(language = list(
      zeroRecords = "No pedigree records available. Check inventory records for selected date."
    )))

  selectedClone <- reactiveVal()

  output$cloneDropdown <- renderUI({
    selectInput("selectedClone", "Select a Clone", choices = unique(pedigree_init()$Clone))
  })

  output$pedigreeGraph <- renderVisNetwork({
    tryCatch({
      req(input$selectedClone)
      pedigree_data <- deeppedigree_init()
      
      validate(
        need(!is.null(pedigree_data), "No pedigree data available for selected clone"),
        need(nrow(pedigree_data) > 0, "Empty pedigree data returned")
      )
      
      createPedigreeGraph(pedigree_data)
    }, error = function(e) {
      showNotification(paste("Error displaying pedigree graph:", e$message), type = "error")
      NULL
    })
  })

  output$pedigreeMatrix <- renderPlotly({
    tryCatch({
      req(pedmatrix_init())
      
      # Validate matrix data
      matrix_data <- pedmatrix_init()
      if (nrow(matrix_data) == 0 || ncol(matrix_data) <= 1) {
        showNotification("No valid relationship data to display", type = "warning")
        return(plotly_empty())
      }
      
      # Convert data to numeric matrix for heatmap
      matrix_for_plot <- as.matrix(matrix_data[,-1, drop = FALSE]) # exclude Clone column, preserve matrix structure

      
      # Force conversion to numeric and handle any NA values
      matrix_for_plot <- apply(matrix_for_plot, 2, as.numeric)
      matrix_for_plot[is.na(matrix_for_plot)] <- 0
      
      rownames(matrix_for_plot) <- matrix_data$Clone
      
      # Create heatmap using plot_ly
      plot_ly(
        x = colnames(matrix_for_plot),
        y = rownames(matrix_for_plot),
        z = matrix_for_plot,
        type = "heatmap",
        colors = viridis::viridis(100),
        hoverongaps = FALSE
      ) %>%
        layout(
          title = "Relationship Matrix",
          xaxis = list(
            title = "Female Parent",
            tickangle = 45
          ),
          yaxis = list(
            title = "Male Parent"
          ),
          margin = list(
            l = 100,
            r = 50,
            b = 100,
            t = 50
          )
        )
    }, error = function(e) {
      showNotification(paste("Error displaying pedigree matrix:", e$message), type = "error")
      plotly_empty()
    })
  })
}