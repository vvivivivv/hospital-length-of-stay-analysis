# hospital-length-of-stay-analysis

**Course**: BC2406 Analytics I: Visual and Predictive Analytics  
**Semester**: AY24/25 Sem 1  

**Project**: Hospital Length of Stay Estimation at Admission

**Assessment Type**: Computer-Based Assessment (Individual Project)

---

## 📘 Project Description
This project focuses on improving the accuracy of Length of Stay (LOS) prediction for hospital inpatients upon admission relative to the mean LOS. The goal is to support better hospital resource planning (e.g., beds, medical staff, equipment), cost estimation, and insurance approvals.

---

## 📁 Structure
- `code/` - R script for preprocessing and modeling
- `question/` - Case questions and background
- `report/` - Final report and summary of findings
- `data/` - Raw dataset

---

## 🧪 Project Tasks Overview

### 🔍 Data Exploration
- Identified notable findings via correlation analysis
- Prepared the data by removing outliers, applying one-hot encoding, and addressing multicollinearity.

### ⚙️ Model Building and Evaluation
- Trained models using 70/30 train/test split.
- Compared Linear Regression and CART models.
- Evaluated using RMSE and R².
- Linear Regression was optimised using stepwise selection; CART was pruned and benchmarked against Random Forest to assess model robustness.

### 🧠 Insights & Business Implications
- Both models highlight similar key predictors and were more accurate for shorter LOS predictions.
- CART outperformed Linear Regression with a lower RMSE and wider prediction range due to its ability to capture non-linear relationships and handle mixed variable types.
- These insights can support more efficient resource allocation and improve operational decision-making in hospitals.

### 💡 Suggested Improvements
- Incorporate additional data (e.g. national health policies, bed occupancy rates, time-based factors) to enhance model performance.
- Explore advanced ensemble models like XGBoost to enhance accuracy.
- Develop diagnosis-specific models using CCSR Diagnosis Codes for scalable deployment and better cost management.

---

## 🛠 Technologies Used
- R (with `rpart`, `caret`, `randomForest`)
- RStudio

---

## 🚀 How to Run
1. Open `code/hospital_length_of_stay_analysis.R` in RStudio.
2. Install required packages.
3. Retrieve raw input dataset in `data/`.
4. Run the script to reproduce the analysis and models.

---

## 📌 Notes
- All identifying student information has been removed for privacy.
- Dataset and question/information PDF was provided as part of the BC2406 assessment and is not open-source.
- Final report is stored in `report/`.
