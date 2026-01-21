This repository contains the data, functions and scripts to model traffic volume at different temporal resolutions (so far only AADT). 

```
xdt-modelling/
├── README.md
└── 20xx/
    ├── data-raw/                                    
    |   ├── directed-traffic-links-20xx.json         
    |   ├── traffic-nodes-20xx.geojson               
    |   ├── trafikklenker_med_holdeplasser_20xx.csv  
    |   └── holdeplasspasseringer_fra_entur.csv      
    ├── data-prepared/                               
    |   ├── adjacency_matrix20xx.rds                 
    |   ├── clusters20xx.rds
    |   ├── prepared_nodes20xx.rds
    |   └── prepared_traffic_links20xx.rds
    ├── results/            
    └── scripts/            
```
