VF5 Ultimate Shordown / eSports data visualization and analytics.

## Dependencies
* [R](https://cran.r-project.org/)
* [VF5 US/ES Match Data](https://t.co/H6JnOKsIQj)

## Data
VirtuAnalytics dashboard visualizes Virtua Fighter 5 US/ES match data.
This is provided via Google Drive instead of GIT to minimize repo size.

Windows
Browse [VF5 US/ES Match Data](https://t.co/H6JnOKsIQj) in web browser and save to `virtuanalytics_dashboard/data` directory

Linux
```
wget "https://drive.usercontent.google.com/u/0/uc?id=1ZFwbOWkIxUrxT6wT6FVq2fkP5uDlfDpZ&export=download" -O data/vf_match_data.csv

```

## Getting Started
Once installed, run the following command to launch the web application

```
Rscript run_app.R
```

You can then access the application through web browser [http://127.0.0.1:8080](http://127.0.0.1:8080)