This repository contains the data, functions and scripts to model traffic volume at different temporal resolutions (so far only AADT). 

```
xdt-modelling/
├── README.md
└── 20xx/
    ├── data-raw/                                    
    |   ├── directed-traffic-links-20xx.json          # Retta trafikklenker uten geometri
    |   ├── directed_traffic_links_20xx.geojson       # Retta trafikklenker med geometri
    |   ├── traffic-nodes-20xx.geojson                # Trafikknoder med geometri
    |   ├── traffic-links-aadt-data-20xx.csv          # Uretta trafikklenker med korrigert ÅDT (tilgjengelig etter ÅDT-belegginga)
    |   ├── trafikklenker_med_holdeplasser_20xx.csv   # Retta trafikklenker som bare har kollektivtrafikk, med holdeplass-id'er
    |   └── holdeplasspasseringer_entur_20xx.csv      # Antall passeringer på alle holdeplasser i Norge for angitt år.
    ├── data-prepared/                               
    |   ├── adjacency_matrix20xx.rds                  # Naboskapsmatrise for romlig ledd i INLA
    |   ├── clusters20xx.rds                          # Grupper av trafikklenker avgrenset av TRP'er som brukes i balanseringa
    |   ├── prepared_nodes20xx.rds                    # Noder med ekstra kolonner som angir hvilke som skal balanseres
    |   └── prepared_traffic_links20xx.rds            # Preparerte trafikklenker, klar for modellering.
    ├── results/            
    └── scripts/
        └── modeling_script_20xx.R                    # Modelleringsscriptet som brukes til ÅDT-belegginga
```
