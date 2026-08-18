# Flowering.R

flowering_server <- function(input, output, session, reactive_date, reactive_iid, dataSource) {
  inventory_init <<- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
    tryCatch({
        # Validate inputs
        req(reactive_date(), reactive_iid())
     
      #get list of studies for input$location
      
      studies_list<-jsonlite::fromJSON(ba_studies(brap2, trialDbId = reactive_iid(), pageSize=2000, rclass="json"))$result$data
      
      inven=data.frame()
     
       for(i in 1:dim(studies_list)[1]){
        studyDbId=as.character(studies_list[i,"studyDbId"])
        
        tmp<-data.frame(brapi::ba_studies_observations_brapi2(con = brap2, studyDbId = studyDbId , rclass="data.frame"))
          
        if(dim(tmp)[1]==0){
          inven<-inven 
          } else{
          tmp<-tmp %>% filter(grepl("Tassel Count", observationVariableName))
          inven<-rbind(inven, tmp )
          } 
        
      }
    

      inven_date<-filter(inven, grepl(reactive_date(), observationTimeStamp)) %>% 
          select(germplasmName, germplasmDbId, observationVariableName, value, observationTimeStamp) %>% 
          group_by(germplasmName)
      
      inven_merge<-aggregate(as.numeric(value)~germplasmName+germplasmDbId,inven_date, sum) #takes into account possibility of multiple plots of same genotype
      
      colnames(inven_merge)<-c("Clone", "germplasmDbId", "FlowerCount")
      
      #####temp placeholder for sex
      inven_merge$tempSex<-sample(c("M", "F"), nrow(inven_merge), replace = TRUE)
      
      male<-filter(inven_merge, tempSex=="M")
      female<-filter(inven_merge, tempSex=="F")
      
      inven2<-list(male, female)
      names(inven2)<-c("male", "female")
      
    dataSource("Data pulled from BrAPI")
      inven2
    }, error = function(e) {
      dataSource("Saved data is being rendered")
      data.frame(Clone = character(), germplasmDbId=character(), FloweringCount = numeric(), Location = character()) # Return an empty data frame with the expected columns
    })
  }))
  
  
  output$inventoryTableMale <- 
  
    renderDT({
      tryCatch({
        req(inventory_init())
        inventory_init()$male %>% select(!germplasmDbId)}, error = function(e) {
          showNotification("Error displaying inventory table", type = "error")
          data.frame()
        })
      },options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
      You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  
  
  output$inventoryTableFemale <-

    renderDT({
      tryCatch({
        req(inventory_init())
        inventory_init()$female%>% select(!germplasmDbId)}, error = function(e) {
      showNotification("Error displaying inventory table", type = "error")
      data.frame()
    })
},options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
      You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  

  
  return(inventory_init)
}