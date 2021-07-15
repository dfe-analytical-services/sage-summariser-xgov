# sage-summariser-xgov
## Overview
The SAGE summariser is a search engine for papers discussed at meetings of the Scientific Advisory Group for Emergencies (SAGE) and published on gov.uk 
(https://www.gov.uk/government/collections/scientific-evidence-supporting-the-government-response-to-coronavirus-covid-19). 
It was built and is maintained by the Department for Education but has been made publicly accessible for other interested Government analysts to use. 

The SAGE summariser is an R Shiny application. It relies on Github actions to automatically check for new SAGE papers being published. It then reads in their content and uses
machine learning techniques to extract key information about them. That information is stored in a data object that is searchable via a User Interface. 

## Functionalities
From https://department-for-education.shinyapps.io/sage-summariser-xgov/, users can: 
* Scroll through a list of all papers discussed at SAGE with links to full text and landing pages
* Search papers by SAGE meeting and publication date
* Search papers by key words
* Search papers by themes
* Consult summaries (i.e. example sentences that are representative of the overall content)
* Upload and process papers stored on their local machine (these are not added to the database and are available to the user for the duration of their current session only)

## Updating process
Currently the summariser checks for new publications on a weekly basis (on Tuesdays). This means new content may not be immediately picked up as it is published. 
While daily updates would have been preferable, this was a trade-off we had to make because of Github's limitations in terms of data storage.  

## Machine Learning
The app uses topic modelling to automatically generate theme labels for each paper. Example sentences are selected using extractive summarising relying
on Google's TextRank algorithm. 

## Limitations
Many different types of documents are discussed at SAGE meetings, with some easier to extract information from than others. For instance, extracting representative sentences
from slide packs, or from papers that mainly contain charts, can be a challenging task even for human readers. In addition, reading from pdfs is inherently difficult for machines
because of the unstructured nature of this format. As a consequence, the SAGE summariser sometimes produces unconvincing summaries. 
