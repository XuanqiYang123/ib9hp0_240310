# E-Commerce Database Management System
This repository houses the R scripts and GitHub Actions workflows necessary to automate the data validation, database updates, and basic data analysis tasks for an e-commerce platform.
## Objective
To establish an automated system that validates incoming data, updates the e-commerce database, and performs fundamental data analysis through GitHub Actions. The workflows are designed to be triggered by specific events such as pushes or pull requests to ensure that the database is consistently up-to-date and accurate.
## Requirements
The system is configured with the following features:
- GitHub Workflows: Automated workflows that trigger on push or pull request events to the main branch of the repository.
- Data Validation: Scripts to validate data integrity before database insertion.
- Database Updates: Automation of database update tasks to include new and modified data.
- Data Analysis: Execution of scripts to perform basic data analysis after each update.
## Repository Contents
- 'IB9HP0_9_synth_1.R': Script for generating synthetic customer data.
- 'IB9HP0_9_synth_2.R': Script for generating synthetic product data.
- 'IB9HP0_9_Table_Creation.R': Script for creating initial database schema.
- 'IB9HP0_9_Data_Validation_Insertion.R': Script for inserting data into the database and executing initial analysis.
- 'IB9HP0_9_Analysis.R': Script for data analysis.
- 'workflow.yml': GitHub Actions workflow for orchestrating the automation process.
## Automated Workflow
1. Push/Pull Request: The workflow is triggered whenever changes are pushed to the main branch or a pull request is made.
2. Installing, Saving and Restoring of various dependencies(Packages).
3. Validate the new data and store the records to 'error_log.txt'.
4. Runs Multiple basic analysis on any new data updated on the repository.
