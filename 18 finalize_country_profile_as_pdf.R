library(tidyverse)
library(KeyboardSimulator)

# microsoft word shortcutes
# https://support.microsoft.com/en-us/office/use-the-keyboard-to-work-with-the-ribbon-in-word-467089f1-b4f9-4407-a80a-6a1f351fd44e

# list of keyboardSimulator commands
# ?keyboard_value

# create finalize_country_profile_as_pdf()
finalize_country_profile_as_pdf <- function(current_country, current_fy) {
        
        # delete old blank_doc if it exists
        if("output/charts/country_profile_blank_doc.docx" %in% dir_ls("output/charts")) {
                
                file_delete(path = "output/charts/country_profile_blank_doc.docx")
        }
        
        # create blank_doc
        read_docx() %>% print(target = "output/charts/country_profile_blank_doc.docx")
        
        # sleep 
        Sys.sleep(time = 3)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # if necessary, delete old pdf output 
        old_pdf_path <- str_c("output/charts/Malign Influence Resilience Index - ", current_country, " FY ", as.character(current_fy), " Country Profile.pdf")
        if(old_pdf_path %in% dir_ls("output/charts")) {
                
                file_delete(path = old_pdf_path)
        }
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # open doc 
        
        # get filename 
        # filename <- str_c("C:\\Users\\sdevine\\Desktop\\usaid\\mcp\\malign_influence\\",
        #                 "output\\charts\\country_profile_", current_country, ".docx")
        filename <- str_c("C:\\Users\\sdupre\\Desktop\\usaid\\mcp\\malign_influence\\",
                          "output\\charts\\country_profile_", current_country, ".docx")
        
        # open word doc 
        shell(shQuote(string = filename), wait = FALSE)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # center image horizontally and vertically
        
        # sleep 
        Sys.sleep(time = 7)
        
        # click on image in doc
        # mouse.move(x = 960, y = 540)
        mouse.move(x = 1280, y = 720)
        mouse.click(button = "left")
        
        # bring image in front of text so it can be moved
        keybd.press(button = "Shift+F10")
        keybd.press(button = "w")
        keybd.press(button = "n")
        
        # move image randomly/slightly so that align center/vertical will work
        # mouse.move(x = 960, y = 540)
        mouse.move(x = 1280, y = 720)
        keybd.press(button = "left")
        keybd.press(button = "up")
        
        
        # center image vertically
        keybd.press(button = "Alt")
        keybd.press(button = "p")
        keybd.press(button = "a")
        keybd.press(button = "a")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        
        # center image horizontally
        keybd.press(button = "Alt")
        keybd.press(button = "p")
        keybd.press(button = "a")
        keybd.press(button = "a")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        keybd.press(button = "right")
        keybd.press(button = "right")
        keybd.press(button = "right")
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
 
        # save as pdf and close pdf
        
        # sleep 
        Sys.sleep(time = 5)
        
        # get output_name
        output_name <- str_c("Malign Influence Resilience Index - ", current_country, " FY ", as.character(current_fy), " Country Profile.pdf")
        
        # name output (on usaid laptop)
        keybd.press(button = "Alt+f")
        keybd.press(button = "a")
        Sys.sleep(time = 2)
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.type_string(string = output_name)
        
        # sleep 
        Sys.sleep(time = 2)
        
        # select pdf as output format
        keybd.press(button = "Tab")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # save (on usaid laptop)
        keybd.press(button = "Tab")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 5)
        
        # close pdf
        keybd.press(button = "Ctrl+w")
        keybd.press(button = "Enter")

        # sleep 
        Sys.sleep(time = 3)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # close current_country word doc
        
        # get blank_doc_filename
        # blank_doc_filename <- str_c("C:\\Users\\sdevine\\Desktop\\usaid\\mcp\\malign_influence\\",
        #                   "output\\charts\\country_profile_blank_doc.docx")
        blank_doc_filename <- str_c("C:\\Users\\sdupre\\Desktop\\usaid\\mcp\\malign_influence\\",
                                    "output\\charts\\country_profile_blank_doc.docx")
        
        # open blank_doc
        shell(shQuote(string = blank_doc_filename), wait = FALSE)
        
        # sleep 
        Sys.sleep(time = 5)
        
        # switch view to current country_profile word doc and close it
        keybd.press(button = "Ctrl+F6")
        keybd.press(button = "Alt+f")
        keybd.press(button = "c")
        keybd.press(button = "right")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 3)
        
        # close blank_doc
        keybd.press(button = "Alt+f")
        keybd.press(button = "c")
        
        # sleep
        Sys.sleep(time = 3)
        
        # delete blank_doc
        file_delete(path = "output/charts/country_profile_blank_doc.docx")
        
        # sleep
        Sys.sleep(time = 3)
}


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
