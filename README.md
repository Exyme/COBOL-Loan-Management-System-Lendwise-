# COBOL-Loan-Management-System-Lendwise-
[![COBOL](https://img.shields.io/badge/COBOL-Developer-blue)](https://github.com/Exyme/COBOL-Loan-Management-System-Lendwise-)
[![DB2](https://img.shields.io/badge/DB2-SQL-green)](https://github.com/Exyme/COBOL-Loan-Management-System-Lendwise-)


A group-developed COBOL CRUD application for loan payments with DB2 integration. Showcases modular design, SQL embedding, and JCL for execution.

## Project Overview
- **Technologies**: COBOL, DB2/SQL, JCL (z/OS environment).
- **Modules**:
  - **Create Module** (WONA): Generates payment plans with interest calculations and inserts into PAYPLAN.
  - **Read & Update Module** (LNDWISE4): Fetches due payments, handles partial/overdue/overpayments, updates status, and generates reports.
  - **Payment Module** (PAYMENT): Validates and inserts payment records from input files.
  - **Delete Module** (DLTPAYPL): Deletes paid-off loan plans from PAYPLAN.
- **Database Model**: Relational schema with LOAN as central table, supporting transactions and queries (see below).

[ER Diagram]:

<img width="1738" height="916" alt="er_diagram" src="https://github.com/user-attachments/assets/9136ba2a-99bd-4e68-b3f8-00254e0d8f0f" />


Sample loan data (from database/loan_sample_data.txt):
| LOAN_ID | CUSTOMER_ID | TYPE_ID | LOAN_AMOUNT | LOAN_STATUS | INTEREST_RATE | INTEREST_TYPE | CREATION_DATE | DOWN_PAYMENT | PAYMENT_PERIOD |
|---------|-------------|---------|-------------|-------------|---------------|---------------|---------------|--------------|----------------|
| 1       | 1           | 2       | 5000000.00  | A           | 4.65          | F             | 2024-01-05    | 1000000.00   | 360            |
| 2       | 3           | 1       | 500000.00   | A           | 3.25          | F             | 2024-06-20    | 100000.00    | 60             |
(Truncated—full in repo)

## My Contributions
As a COBOL developer in the group:
- Implemented the Read & Update module, including overpayment logic and interest calculations.
- Designed JCL for compile/bind/run, ensuring DB2 integration.
- Contributed to creating the ER diagram and sample data for testing.
- Debugged versions of the Read & Update module.

Collaborators: [Teammate1, wonalee1a@gmail.com], [Teammate2, malene.folkvord@hotmail.com].

## How to Run
1. **Setup**: Upload .cbl files to SRCLIB, DCLGENs to DCLGLIB.
2. **Compile & Link**: Submit /jcl/compile_link_lndwise4.jcl (adjust MEMBER).
3. **Bind**: Submit /jcl/bind_plan.jcl.
4. **Execute**: Submit /jcl/run_db2_with_files.jcl (outputs to OUTFILE).

Example code snippet from Read & Update (handling overdue):
```cobol
740-PAYMENT-NOT-FOUND.
    IF PLAN_DUE-DATE < WS-DATE-FOR-CALC
       IF PLAN_PLAN-STATUS NOT = 'PARTIAL'
          MOVE 'OVERDUE' TO PLAN_PLAN-STATUS
          MOVE PLAN_PAYMENT-AMOUNT TO LEFTOVER-PAYMENT
          PERFORM 800-UPDATE-PAYMENT-PLAN-STATUS
       END-IF
    END-IF.
```

## Lessons Learned
- Optimizing COBOL PERFORM loops for efficient SQL cursors.
- Handling DB2 error codes in production JCL.
- COBOL's relevance in finance for reliable transaction processing.
