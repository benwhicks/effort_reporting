# Guide to effort reporting process

The following documents using the [Effort Tracking](https://efforttracking.com/) framework along side with interfacing with Edumate as a school information system. The relevant parts of this repository are:

* _code_ folder. This contains relevant __sql__ and __r__ scripts to use, either for wrangling data or performing analysis.
* _data_ folder. Directory for storing data, including sample data. School data is not uploaded to repo.
* _reports_ folder. Directory for outputting reports, including sample reports. School reports and analysis not uploaded to repo.

## Wrangling data for _Effort Tracking_ system

The _Effort Tracking_ system specifies a template for the csv upload that it needs. Note that all files should be saved into the *data* folder with sensible names. The process is as follows:

1. Extract enrolment data from Eduamte. This is done with the script called *edumate_enrolment_oxley.sql* but can also be found in *Reports [WHERE EXACTLY?]* on Edumate.
2. Extract student information from Edumate (primarily to get House / Pastoral data). This is done with the script called *FINDNAMEOFSCRIPT* but can also be found in *FINDSCRIPTONEDUMATE*
3. ~~Extract academic data.~~ This is currently not possible.
4. Merge Edumate csv files into relevant format for _Effort Tracking_ system. This is done by running the script *edumate_data_merger.R*. You will need to read the script before running it. This script will output a file in the *data* folder.
5. Upload the output csv file from step 4 to the _Effort Tracking_ site.


