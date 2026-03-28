# 📊 RPT5000 – YTD Sales Report

## 👨‍💻 Program Information
- **Program Name:** RPT5000  
- **Programmer:** Dhruv Patel  
- **Course:** CIS 352 – Intro to Enterprise Computing  
- **Date:** 03/26/2026  
- **Language:** COBOL  

---

## 📌 Description
This program reads customer sales data from a sequential file and generates a formatted Year-To-Date (YTD) Sales Report.

The report includes:
- Branch Number  
- Sales Representative Number  
- Customer Number and Name  
- Sales This Year  
- Sales Last Year  
- Change Amount  
- Change Percentage  

The program also calculates and prints:
- Sales Representative Totals  
- Branch Totals  
- Grand Totals  

---

## ⚙️ Features
- ✔ Reads input from CUSTOMER-FILE  
- ✔ Writes formatted report to REPORT-FILE  
- ✔ Calculates:
  - Change Amount = This Year – Last Year  
  - Change % = (Change / Last Year) × 100  
- ✔ Handles divide-by-zero:
  - Displays 999.9 when last year sales = 0  
- ✔ Implements control break logic:
  - SalesRep totals  
  - Branch totals  
- ✔ Includes:
  - Page headings  
  - Date & Time  
  - Page numbers  
- ✔ Uses formatted numeric output with commas and decimals  

---

## 🧮 Sample Output
<img width="1155" height="933" alt="image" src="https://github.com/user-attachments/assets/dd4aab9e-893c-43b5-9221-296da9e03571" />
<img width="890" height="184" alt="image" src="https://github.com/user-attachments/assets/f2ce05dc-c232-43c0-a993-40f479ed78d1" />


---

## 📂 File Structure
- RPT5000.cbl → Main COBOL program  
- CUSTMAST → Input customer data file  
- RPT5000 → Output report file  

---

## 🔁 Program Flow
1. Initialize program  
2. Open input/output files  
3. Read first record  
4. Loop through records:
   - Detect control breaks (SalesRep / Branch)  
   - Calculate values  
   - Print detail line  
5. Print totals  
6. Print grand total  
7. Close files  

---

## ⚠️ Notes
- Input file must match record length (130 bytes)  
- If mismatch occurs → File Status 39 error  
- Dataset must be properly cataloged before execution  

---

## 👍 GBU Reflection

### ✅ Good
- Program correctly calculates totals and percentages  
- Control-break logic works for both salesrep and branch levels  
- Output is clean and formatted like a real business report  
- Handles division by zero safely  

### ❌ Bad
- Code is long and repetitive in some sections  
- Formatting logic could be modularized better  
- Hardcoded report layout makes changes difficult  

### ⚠️ Ugly
- Debugging file errors (like record length mismatch) was confusing  
- COBOL syntax is strict and small mistakes cause big errors  
- Control-break logic is tricky to understand at first  

---

## 🚀 Future Improvements
- Use tables instead of hardcoded logic  
- Improve modular design with more reusable paragraphs  
- Add sorting before processing  
- Enhance formatting with better spacing/alignment  
