# 📊 Insurance Dashboard - Insurance Market Analysis

### 🚀 [View Live Demo](https://rnaufal.shinyapps.io/insurance_dashboard/) 

---

## 📋 Table of Contents

- [About The Project](#about-the-project)
- [Key Features](#key-features)
- [Main Components](#main-components)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Usage](#usage)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)

---

## 🎯 About The Project

The Cyber Dashboard is an interactive web application built with R and the Shiny framework, designed for in-depth analysis of the cyber insurance market. It provides a comprehensive suite of tools for users to explore, visualize, and compare financial and operational metrics across different Insurance Groups, individual Firms, and Managing General Agents (MGAs). 

Key functionalities include side-by-side company comparisons, deep-dive trend analysis for single entities, and the ability to export custom reports and underlying data to both PDF and Excel formats. The application serves as a central hub for market intelligence, combining quantitative data with qualitative information like company news and background details.

---

## ✨ Key Features

- **📈 Interactive Data Exploration:** View and filter aggregated financial data for Insurance Groups and individual Firms in customizable tables
- **🔄 Side-by-Side Company Comparison:** Compare up to three Groups or Firms against each other across a wide range of performance metrics (e.g., Written Premiums, Loss Ratio, Number of Policies)
- **📊 In-Depth Trend Analysis:** Select a single Group or Firm and visualize its performance over time across more than 14 different financial and operational metrics
- **📤 Powerful Exporting Options:**
  - **📄 Export to PDF:** Generate multi-page, branded PDF reports of single-entity or comparison analyses, complete with graphs and summary data tables
  - **📑 Export to Excel:** Download the underlying numerical data for any analysis into a well-organized Excel (`.xlsx`) file with multiple worksheets
- **🏢 MGA Intelligence Hub:** Browse, search, and view detailed profiles for Managing General Agents (MGAs), including their background, funding status, and latest news
- **📚 Integrated Wiki:** Access company-specific overviews and Frequently Asked Questions (FAQs) directly within the app to get context behind the data

---

## 🏗️ Main Components

The application is organized into several key panels:

### 1. **Company Table**
The main entry point to view high-level financial data, aggregated by Insurance Group or by individual Firm. From here, users can navigate to more detailed views.

### 2. **Single Graph**
A deep-dive visualization suite for a single selected company, showing historical trends across numerous metrics. This is where PDF/Excel exporting is performed.

### 3. **Comparison**
A powerful tool to compare up to three companies side-by-side on selected metrics. This panel also supports exporting comparison data to PDF and Excel.

### 4. **MGA Table & Single MGA**
A dedicated section to browse and search for Managing General Agents and view their detailed profiles, including background info and recent news.


---

## 🚀 Getting Started

To run this application locally, follow these steps.

### Prerequisites

You will need R and RStudio (recommended) installed. You must also install the required R packages.

**Required R version:** R >= 4.0.0

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/your-username/cyber-dashboard.git
   cd cyber-dashboard
   ```

2. **Install required R packages:**
   ```r
   # Install all required packages for the app
   install.packages(c(
     "shiny", 
     "bs4Dash", 
     "leaflet", 
     "dplyr", 
     "purrr", 
     "glue",
     "DT", 
     "plotly", 
     "rmarkdown", 
     "knitr"
   ))
   ```

3. **Set up the project directory:**
   Ensure your project folder contains the necessary `data/` and `www/` sub-directories as required by the application.

### Usage

1. **Run the application:**
   - Open the `global.R`, `ui.R`, or `server.R` file in RStudio
   - Click the "Run App" button that appears at the top of the editor
   
   OR
   
   - Run from R console:
   ```r
   shiny::runApp()
   ```

2. **Access the dashboard:**
   The application will open in your default web browser, typically at `http://127.0.0.1:port`

---

## 📁 Project Structure

```
cyber-dashboard/
├── global.R          # Global variables and package loading
├── ui.R              # User interface definition
├── server.R          # Server logic
├── data/             # Data files
├── www/              # Static web assets (CSS, images, etc.)
├── R/                # Additional R scripts and functions
├── reports/          # Report templates
└── README.md         # This file
```

---

## 🤝 Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

---

## 📄 License

Distributed under the MIT License. See `LICENSE` file for more information.

---
