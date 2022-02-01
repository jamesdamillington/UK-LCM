# UK-LCM

Notebooks used in the development and teaching of land use/cover change analysis using Python. Data used are from [UK CEH LCM](https://www.ceh.ac.uk/ukceh-land-cover-maps), the licence for which does not permit sharing on publicly.

## Teaching
Notebooks used directly in teaching:

- [`uk-lcm-wrangling.ipynb`](uk-lcm-wrangling.ipynb): data manipulation needed prior to `uk-lcm-wales-analysis.ipynb`
- [`uk-lcm-wales-analysis.ipynb`](uk-lcm-wales-analysis.ipynb): demonstrating the main land cover change analysis using UK CEH LCM data
- [`uk-lcm-wales-analysis-presplit.ipynb`](uk-lcm-wales-analysis-presplit.ipynb): single notebook containing combined code of `uk-lcm-wrangling.ipynb` and `uk-lcm-wales-analysis.ipynb`  
- [`uk-lcm-wales-analysis-exercises.ipynb`](uk-lcm-wales-analysis-exercises.ipynb): `uk-lcm-wales-analysis.ipynb` with exercises appended
- [`uk-lcm-wales-analysis-exercises-answers.ipynb`](uk-lcm-wales-analysis-exercises-answers.ipynb): `uk-lcm-wales-analysis-exercises.ipynb` with answers appended (not shared on GitHub)

## Demos
Supporting notebooks that might be useful for students

- [`convert-tif-nc.ipynb`](convert-tif-nc.ipynb): demo of basic conversion tif -> nc using gdal
- [`combine-shp.ipynb`](combine-shp.ipynb): demo of combining multiple shapefiles into a single shapefile
