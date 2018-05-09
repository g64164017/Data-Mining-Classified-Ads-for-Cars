# Data-Mining-Classified-Ads-for-Cars
Context  The data was scraped from several websites in Czech Republic and Germany over a period of more than a year. Originally I wanted to build a model for estimating whether a car is a good buy or a bad buy based on the posting. But I was unable to create a model I could be satisfied with and now have no use for this data. I'm a great believer in open data, so here goes. Content  The scrapers were tuned slowly over the course of the year and some of the sources were completely unstructured, so as a result the data is dirty, there are missing values and some values are very obviously wrong (e.g. phone numbers scraped as mileage etc.)  There are roughly 3,5 Million rows and the following columns:      maker - normalized all lowercase     model - normalized all lowercase     mileage - in KM     manufacture_year     engine_displacement - in ccm     engine_power - in kW     body_type - almost never present, but I scraped only personal cars, no motorcycles or utility vehicles     color_slug - also almost never present     stk_year - year of the last emission control     transmission - automatic or manual     door_count     seat_count     fuel_type - gasoline, diesel, cng, lpg, electric     date_created - when the ad was scraped     date_last_seen - when the ad was last seen. Our policy was to remove all ads older than 60 days     price_eur - list price converted to EUR  Inspiration      Which factors determine the price of a car?     With what accuracy can the price be predicted?     Can a model trained on all cars be used to accurately predict prices of models with only a few samples?  In my analysis, there is too much variance even within individual models to reliably predict the price, can you prove me wrong? I would love to understand what I did wrong if you can.
aaaa