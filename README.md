# Financial Analysis in Common Lisp

This project is an exploration of financial modeling techniques using **Common Lisp**. It follows the progression found in classic AI and financial engineering literature (specifically inspired by Miller's work), moving from functional/procedural implementations to a sophisticated **Frame-based Object System** using the Meta-Object Protocol (MOP).

The goal is to demonstrate how Lisp's malleability allows you to build not just financial models, but the *domain-specific languages* (DSLs) required to represent them elegantly.

## Project Structure

The system is divided into two distinct evolutionary stages:

1.  **Classic (`financial-analysis/classic`):** * Implements financial logic using standard functions and **Property Lists** (plists).
    * Demonstrates the "pre-CLOS" style of Knowledge Representation.
    * Includes a Bisection solver for Yield-to-Maturity (YTM).
    * Includes a recursive Decision Tree evaluator.

2.  **CLOS & MOP (`financial-analysis/clos`):**
    * Implements a **Frame System** on top of the Common Lisp Object System.
    * Uses the **Meta-Object Protocol (MOP)** to inject custom behaviors into class definitions.
    * **Features:**
        * **Smart Slots:** Support for `:units`, `:range` validation, and `:default` values.
        * **Lazy Evaluation:** Slots with `:if-needed` daemons that calculate their own values on demand (spreadsheet-like behavior).
        * **Memoization:** Caches calculated results automatically.

---

## Prerequisites

* **SBCL** (Steel Bank Common Lisp)
* **Quicklisp** (Package Manager)
* **Editor:** Emacs with SLIME (Recommended) or VS Code with Alive.

## Installation

1.  Clone this repository into your Quicklisp local projects folder:
    ```bash
    cd ~/quicklisp/local-projects/
    git clone [https://github.com/mandeluna/financial-analysis.git](https://github.com/mandeluna/financial-analysis.git)
    ```

2.  Start your Lisp REPL (`sbcl` or via your editor).

3.  Register the project (if necessary):
    ```lisp
    (ql:register-local-projects)
    ```

---

## Module 1: The Classic Approach

This module uses direct function calls and symbol property lists. It forces the user to manage data "manually" but offers high transparency.

### Getting Started

Load the system:
```lisp
(ql:quickload :financial-analysis/classic)
(in-package :financial-analysis.classic)

```

### Examples

**1. Bond Valuation & Yields**
Unlike Excel, we have exposed the numerical solvers (Bisection Search) directly.

```lisp
;; Calculate Price (5% Coupon, $1000 Par, 10 Years, 6% Market Yield)
(bond-price 0.05 1000 10 0.06)
;; -> 926.399

;; Calculate Yield to Maturity (If price is 926.39)
(yield-to-maturity 926.399 0.05 1000 10)
;; -> 0.06 (6.0%)

```

**2. Decision Trees (AI Style)**
We use symbols as nodes in a graph to evaluate probabilistic outcomes.

```lisp
;; Define nodes
(create-node 'drill-decision :type 'decision :successors '(drill sell))
(create-node 'drill :type 'chance :successors '(oil dry))

;; Define outcomes
(create-node 'oil :prob 0.2 :payoff 500000)
(create-node 'dry :prob 0.8 :payoff 0)
(create-node 'sell :payoff 40000)

;; Evaluate the tree
(tree-value 'drill-decision)
;; -> 100000 (Expected Value of Drilling)

```

---

## Module 2: The Frame System (CLOS/MOP)

This module builds a higher-level abstraction. Instead of calling functions on data, we define **Smart Objects** that enforce consistency and calculate their own derived data.

### Getting Started

Load the system:

```lisp
(ql:quickload :financial-analysis/clos)
(in-package :financial-analysis.clos)

```

### The "Smart Bond" Example

We define a `Bond` class where the `Price` is not just a number, but a **live calculation**. If the `Yield` is known, the `Price` computes itself.

```lisp
;; 1. Define a Bond with a Market Yield, but NO Price
(defparameter *b* (make-instance 'bond 
                                 :maturity 10 
                                 :yield 0.06))

;; 2. Ask for the Price
;; The system triggers the :if-needed daemon, calculates the PV, 
;; caches it, and returns the result.
(security-price *b*)
;; -> 926.399...

;; 3. Validation Logic
;; The MOP ensures we cannot set invalid data.
(setf (bond-coupon *b*) 0.50)
;; -> ERROR: Value 0.50 is above maximum 0.30 for slot COUPON

```

### How it Works (The Meta-Classes)

The logic resides in `src/clos/meta-classes.lisp`. We override:

* `compute-effective-slot-definition`: To compile raw lambda lists into functions.
* `slot-unbound`: To intercept missing data and trigger calculation daemons.
* `(setf slot-value-using-class)`: To enforce range constraints before data is written.

---

## Directory Layout

```text
.
├── financial-analysis.asd   # System Definition
├── README.md
└── src
    ├── classic
    │   ├── package.lisp
    │   ├── core.lisp        # PV, NPV, basic math
    │   ├── bonds.lisp       # Solvers and functions
    │   └── decision-trees.lisp
    └── clos
        ├── package.lisp
        ├── meta-classes.lisp # The MOP Engine
        └── frames.lisp       # The Domain Model (Bond, Asset)

```

```
