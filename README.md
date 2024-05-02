A new version of the E4 dashboard is available here: https://github.com/PCdLf/wearalyze 

Note that the new Wearalyze dashboard also incorporates the Empatica EmbracePlus and Nowatch devices. The new Wearalyze dashboard is currently in development (latest update 2-5-2024). 

# e4dashboard

Functions and modules to visualize and analyze empatica e4 data. For more information see:
- de Looff, P., Duursma, R., Noordzij, M., Taylor, S., Jaques, N., Scheepers, F., de Schepper, K., Koldijk, S. (2022). Wearables: An R Package With Accompanying Shiny Application for Signal Analysis of a Wearable Device Targeted at Clinicians and Researchers. <i> Frontiers in Behavioral Neuroscience, 16.</i> [https://www.frontiersin.org/article/10.3389/fnbeh.2022.856544](https://www.frontiersin.org/articles/10.3389/fnbeh.2022.856544) 


## How to get started

1) Get the E4dashboard code on your computer:
  - via GIT (version control): git clone https://github.com/PCdLf/e4dashboard.git
  - alternatively: download as zip and unpack

2) Run the E4 dashboard:
  - open the file global.r in RStudio
  - click "Run App" (in Rstudio)
  - (note: the package [wearables](https://github.com/PCdLf/wearables) will be installed in the process.)

## Functionality

After recoring data with the Empatica E4 watch, the data will be synchronized to the [Empatica cloud](https://www.empatica.com/connect/login.php) (directly via the smartphone app or after extracting the data from the watch via cable and empatica software). Download this data.


Our E4 dashboard can visualize and further process this data. There are several tabs:

1) Data
- Select the downloaded Empatica zip file. The data will be loaded.

2) Calendar
- Optionally: select a file with annotations that can be shown in the data plot. (The format is: Date, Start, End, Text, Color; as Excel or plain text)

3) Visualization
- Adjust the settings of the plot as desired an click "Make plot". A new tab will appear with the interactive plot.

4) Analysis
- An analysis can be run for a selected time period. The resulting report can be downloaded as .html, which can be opened in any broswer. It includes: an interactive plot of the data and annotations, two plots on data quality, a list of calendar events, the output of the analysis (several parameters calculated over the selected time period).

6) Data cutter
- If desired, the original data file can be cut into shorter analysis frames, eg 8 hours of data can be split into 5 minute data frames for further analysis.

8) Batch analysis
- The resulting shorter data frames can be loaded. In a batch analysis several parameters will be determined per frame. The resulting data is stored as .rds which can be opened in RStudio.
