# Data Directory

The data used for SESYNC training events is freely available online. The note
following each group of files below describes the data and where to access it
online. Not all files listed are likely to be present among the handouts,
depending on the training event.


## Portal Teaching Database

### Files

- plots.csv
- animals.csv
- species.csv
- portal.xlsx
- portal.sqlite

### Description

The Portal Project Teaching Database is a simplified version of the Portal
Project Database designed for teaching. It provides a real world example of
life-history, population, and ecological data, with sufficient complexity to
teach many aspects of data analysis and management, but with many complexities
removed to allow students to focus on the core ideas and skills being taught.

The database is currently available in csv, xlsx, and sqlite files, and possibly
within a PostgreSQL database called 'portal' presented in the lesson.

This database is not designed for research as it intentionally removes some of
the real-world complexities. The original database is published as
[ecol/E090/118] on the Ecological Archives, and that version should be used for
research purposes.

Use of this dataset should cite http://dx.doi.org/10.6084/m9.figshare.1314459

[ecol/E090/118]: http://esapubs.org/archive/ecol/E090/118/


## National Land Cover Database

### Files

- nlcd_agg.grd
- nlcd_agg.gri
- nlcd_proj.grd
- nlcd_proj.gri

### Description

A portion of the [National Land Cover Database] that has been cropped and
reduced to a lower resolution in order to speed up processing time for this
tutorial. The *_agg* raster is in the original Albers equal-area projection,
whereas the *_proj* raster has been reprojected to Web Mercator for use with the `leaflet` package.

[National Land Cover Database]: http://www.mrlc.gov/nlcd2011.php


## US Census Cartographic Boundary Shapefile

### Files

- cb_2016_us_county_5m/cb_2016_us_county_5m.shp
- cb_2016_us_county_5m/cb_2016_us_county_5m.prj
- cb_2016_us_county_5m/cb_2016_us_county_5m.dbf
- cb_2016_us_county_5m/cb_2016_us_county_5m.shx
- cb_2016_md_county_5m/cb_2016_md_county_5m.shp
- cb_2016_md_county_5m/cb_2016_md_county_5m.prj
- cb_2016_md_county_5m/cb_2016_md_county_5m.dbf
- cb_2016_md_county_5m/cb_2016_md_county_5m.shx

### Description

The [cartographic boundary files] are simplified representations of selected
geographic areas from the Census Bureau's MAF/TIGER geographic database. These
boundary files are specifically designed for small scale thematic mapping. The
`cb_2016_md_county_5m` is the subset of US features where `STATEFP == 24`.

[cartographic boundary files]: https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_5m.zip


## US Census County Business Patterns

### Files

- cbp15co.txt
- national_county.txt

### Description

[County business patterns data from 2015]. One [record] per county per [NAICS] industry code in cbp15co.txt. Data on number of establishments, payroll, etc.
national_county.txt is a list of FIPS codes and county names.

[County business patterns data from 2015]: https://www2.census.gov/programs-surveys/cbp/datasets/2015/cbp15co.zip
[record]: https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/county_layout_2015.txt
[NAICS]: https://www.naics.com/search/


## Hydrologic Unit Shapefile

### Files

- huc250k/huc250k.shp
- huc250k/huc250k.shp.xml
- huc250k/huc250k.shx
- huc250k/huc250k.dbf
- huc250k/huc250k.prj
- huc250k/huc250k.sbn
- huc250k/huc250k.sbx

### Description

[1:250,000-scale Hydrologic Units] of the United States published by the
U.S. Geological Survey as a shapefile with associated metadata.

[1:250,000-scale Hydrologic Units]: https://water.usgs.gov/GIS/metadata/usgswrd/XML/huc250k.xml


## Enron Email Dataset

### Files

- enron/*.txt

### Description

The [Enron Email Dataset] was collected and prepared by the CALO
Project (A Cognitive Assistant that Learns and Organizes). It contains
data from about 150 users, mostly senior management of Enron,
organized into folders. The corpus contains a total of about 0.5M
messages that were originally made public, and posted to the web,
by the Federal Energy Regulatory Commission during its investigation.

The subset of the corpus re-distriubuted with this lesson is limited
to email sent betwen 2001-08-13 and 2001-11-29, roughly the period of
the company's demise.

[Enron Email Dataset]: https://www.cs.cmu.edu/~./enron/

## Ebola data for Sierra Leon

### Files

- Casees_at_Admin2_Level
- Ebola_Treatment_Centers
- Sierra_Leone_Roads
- SL_Admin01
- SL_pop.csv
- SL_pop.ods
- gis-abm-lesson.prj

### Description

Not availble.