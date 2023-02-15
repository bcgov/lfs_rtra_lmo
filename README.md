
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)


lfs_rtra_lmo
============================

### Usage

-Submit 17!!! sas scripts found in folder `sas_code` to https://www75.statcan.gc.ca/eft-tef/en/login-connexion.
-Grab a coffee.
-Parse through the 102 emails Stats Can sent you to find the names of the 17 csv files you need to download.
-place the csv files who's name contains "retire" or "stat" in the folder `by_noc`
-place the csv files who's name contains "naics" in the folder `by_naics`.
-change the min and max years in the file  `00_source_me.R`
-source the file `00_source_me.R`
-output can be found in folder `out`.


### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/lfs_rtra_lmo/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
