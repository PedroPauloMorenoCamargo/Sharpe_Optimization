# Sharpe Optimization Project (Pedro Paulo Moreno Camargo)

---

## 1. Assignment

| Requirements                                   | Fulfilment in this repo                                 |
| ---------------------------------------------- | ------------------------------------------------------- |
| Use **30 Dow Jones** stocks                    | `data/download_data.py` pulls the full DJIA stocks‑set  |
| Training period: **01 Aug 2024 → 31 Dec 2024** | Default in `Makefile` (`TRAIN_*`)                       |
| Test period: **01 Jan 2025 → 31 Mar 2025**     | Default in `Makefile` (`TEST_*`)                        |
| Choose **k = 25** of 30 ($~142 k$ combos)      | `Simulate.combinations` enumerates all $\binom{30}{25}$ |
| **n = 1000** weight vectors per combo          | `randomWeights`  1000 weight vectors          |
| Long‑only, cap ≤ 20 % per asset                | Enforced in `Weights.validWeights`                      |
| Parallelised *and* pure                        | `parListChunk` + pure math, no mutable state inside parallel function            |

**Extras**  ⬇️ on‑demand data · 🔁 out‑of‑sample Sharpe · ⚡ serial vs parallel benchmarks.

---

## 1. Objective

Find the portfolio **weight vector** $w$ that maximises the **Sharpe Ratio**

$$
  SR = \frac{\mu^{\mathsf T} w}{\sqrt{w^{\mathsf T}\Sigma w}}
$$

subject to

$$
  \sum w_i = 1,\qquad 0 ≤ w_i ≤ 0.20
$$

Because the optimisation is non‑convex we perform an **exhaustive simulation**:

1. Enumerate every 25‑stock subset of the DJIA ($~142 060$).
2. For each subset, simulate 1000 random weights that satisfy the constraints (pre-generated and reused across subsets).
3. Compute Sharpe(μ, Σ, w) and keep the global maximum.

Total evaluations ≈ **142 million**.

## 2. Why functional? Why parallel?

* **Purity** — deterministic functions allow local reasoning and automatic memoisation.
* **Parallel safety** — no locks, no races.

Parallelism strategy:

* Split the combination list into `numCapabilities × 4` chunks.
* Evaluate each chunk with `rdeepseq` so every Sharpe value is fully reduced on its worker.
* Results are folded to a single `(Sharpe, names, weights)` triple.

---

## 3. Project Layout

### Architecture Overview

**Directory Structure:**
```text
app/Main.hs  – 📋 CLI orchestration, IO, and coordination

src/SharpeOptimization/
├── 📄 **DataLoader.hs**        – CSV ingestion and validation
├── 📊 **Statistics.hs**        – Mathematical operations: μ, Σ, Sharpe
├── 🎲 **Weights.hs**           – Random constrained portfolio generation
├── 🧮 **SimulateSequential.hs** – Exhaustive pure single-threaded simulation
├── ⚙️  **SimulateParallel.hs**   – Parallelized search wrapper
└── 🧾 **Types.hs**             – Type aliases and core data definitions

data/download\_data.py – 🗃 Fetches DJIA prices from Yahoo Finance
Makefile              – 🛠 Setup, fetch, and cleanup automation
```
---

### Module Descriptions

#### 📄 DataLoader.hs

Handles all CSV processing and encapsulates I/O safely:

* Reads training (2nd-Semester-2024) and test (1st-Trimester-2025) datasets from CSV files.
* Drops the first column (typically a "Date" field) to isolate numeric stock prices.
* Validates each line to ensure column consistency and that all values are numeric.
* Wraps all I/O, parsing, and validation using `ExceptT IO`.
* Returns stock names and a matrix of price data as a tuple: `(StockNames, [[Double]])`.

#### 📊 Statistics.hs

Implements all core numerical computations in pure form:

- **`toPriceMatrix :: [[Double]] -> PriceMatrix`**  
  Converts a list of lists from CSV parsing into a boxed matrix of unboxed price vectors.

- **`priceMatrixToReturns :: PriceMatrix -> ReturnMatrix`**  
  Converts price matrix into a matrix of daily returns:  
  each return is calculated as `(pₜ₊₁ / pₜ) - 1` for each asset.

- **`mean :: (G.Vector v Double) => v Double -> Double`**  
  Computes the arithmetic mean of a generic vector.  
  Used in μ and Σ calculations.

- **`muVector :: ReturnMatrix -> U.Vector Double`**  
  Computes the mean return (μ) for each asset (column-wise).

- **`covarianceMatrix :: ReturnMatrix -> CovarianceMatrix`**  
  Computes the sample covariance matrix (Σ) by:
  - centralizing each column (subtracting its mean),
  - computing pairwise dot products,
  - dividing by `n - 1`.

- **`dotProductU :: U.Vector Double -> U.Vector Double -> Double`**  
  Computes the dot product between two unboxed vectors.  
  Used heavily in Sharpe ratio and matrix algebra.

- **`matVecU :: V.Vector (U.Vector Double) -> U.Vector Double -> U.Vector Double`**  
  Multiplies a boxed matrix of unboxed vectors by a column vector.  
  Used in variance computation for the Sharpe ratio.

- **`sharpeRatioFast :: U.Vector Double -> CovarianceMatrix -> Weights -> Maybe Sharpe`**  
  Computes the **annualized Sharpe ratio**:
  Returns `Nothing` if variance is near-zero (numerical stability safeguard).

- **`selectByIndexesU :: [Int] -> U.Vector a -> U.Vector a`**  
  Utility to extract a subvector by index list.  
  Used to reduce μ and Σ to k-dimensional subspaces.

- **`transpose :: V.Vector (U.Vector a) -> V.Vector (U.Vector a)`**  
  Transposes a matrix of unboxed vectors (rows ↔ columns).  
  Used for column-wise statistics like μ and Σ.

- **`centralizeMatrix :: ReturnMatrix -> ReturnMatrix`**  
  Centralizes each column of the return matrix (mean = 0).  
  Required before computing Σ.
  
#### 🎲 Weights.hs

Generates random portfolio weights subject to constraints:

* Uses rejection sampling to enforce valid portfolios:

  * All weights ≥ 0, ∑wᵢ ≈ 1, and no weight > 20%.
* All logic is purely functional and deterministic when given a `StdGen`.
* Returns a list of weight vectors along with the updated random generator.
* Ensures compliance with problem constraints for every trial portfolio.

#### ➡️ SimulateSequential.hs

Implements the exhaustive search engine for simulation in a single thread:

* Enumerates all combinations of `k` out of 30 DJIA stocks (≈ 142k portfolios).
* For each combination, generates `n` valid random weights.
* Computes the Sharpe ratio using the subsets of μ and Σ from `Statistics`.
* Tracks the highest-performing portfolio across all evaluated combinations.
* Threads random generator state manually to ensure reproducibility.
* Entirely pure and referentially transparent.

#### 🔄 SimulateParallel.hs

Parallel backend for simulation with multi-core optimization:

* Splits all combinations into `numCapabilities × 4` chunks for load balancing.
* For each chunk:

  * Assigns an independent random generator (via `splitMany`).
  * Evaluates all weight trials in parallel with `parListChunk` and `rdeepseq`.
* Sharpe ratios are computed as in the sequential path.
* All intermediate results are reduced using a custom `better` comparator.
* Fully deterministic, while significantly improving performance on multi-core machines.

#### 🧾 Types.hs

Defines core type aliases to unify the project interface:

* `PriceMatrix`, `ReturnMatrix`, and `CovarianceMatrix` are declared for clarity.
* Uses `Vector` for outer structures and `U.Vector` for inner rows (performance).
* `Weights` is an unboxed vector of Doubles.
* `Best` encapsulates the best portfolio result: `(Sharpe, Stocks, Weights)`.
* Central to the consistency and readability of all math and logic.

#### 🗃 download\_data.py

Fetches historical stock price data using Python and `yfinance`:

* Downloads daily close prices for the 30 DJIA stocks.
* Saves training and testing datasets to `data/training.csv` and `data/result.csv`.
* Dates for training and testing periods are configurable via command-line args or Makefile variables.
* Used as the primary data source for the simulation pipeline.

#### 🛠 Makefile

Facilitates development, testing, and environment setup:

* Automates the creation of a Python virtual environment.
* Installs required Python dependencies: `yfinance`, `pandas`.
* Defines reusable commands for downloading data and cleaning up generated files.
* Includes targets:

  * `python-setup` — setup venv and install dependencies
  * `download-data` — run data downloader with configured dates
  * `clean-data`, `clean-env`, `clean-all` — housekeeping targets

---

### Simulation Flow
```text
CSV → prices → returns → μ, Σ
  │
  ├─ combinations(k, 30)
  │  │
  │  ├─ generate n weight vectors (Weights.hs)
  │  ├─ sharpeRatioFast μ\[subset], Σ\[subset], w
  │  └─ keep best (Sharpe, names, w)
  ↓
best portfolio → re‑evaluated on test CSV
```
**Step-by-step explanation:**

1. **CSV Load (DataLoader.hs)**: The program reads a CSV file of stock closing prices, validates the data, and returns a matrix of prices for each stock.
2. **Transform to Returns (Statistics.hs)**: Converts the price matrix to a return matrix by calculating daily percentage changes.
3. **Compute μ and Σ (Statistics.hs)**: Computes the Global mean returns vector μ and sample covariance matrix Σ from the return matrix.
4. **Generate Combinations (Statistics.hs)**: All 25-stock subsets (out of 30) are enumerated using combinatorics.
5. **Generate Weights (Weights.hs)**: For each combination, 1000 weight vectors are generated that respect the constraints.
6. **Evaluate Portfolios (Simulate\[Sequential|Parallel].hs)**: Each weight vector is evaluated via the Sharpe formula. The best-performing one (highest Sharpe ratio) is tracked.
7. **Out-of-sample Evaluation**: The best in-sample portfolio is evaluated again on a separate test dataset to check generalization.

> *Parallel execution* distributes step 6 across CPU cores while preserving pure and reproducible logic.

> *Parallel path* simply distributes the “combo loop”; math remains pure and deterministic.

## 4. Installation

### 4.1  Quick start

```bash
# clone
$ git clone https://github.com/<you>/sharpe-optimization.git && cd sharpe-optimization

# (optional) fetch market data
$ make python-setup        # .venv + yfinance, pandas
$ source .venv/bin/activate
$ make download-data       # if you want to get another period change variable in Makefile

# build Haskell executable
$ cabal build              # or: stack build
```

## 5. Running the Simulation

How to run:

```bash
$ cabal run sharpe-optimization-exe -- +RTS -N4
```

When running answer the questions:

Interactive run (defaults shown in `[]`):

```bash
Training CSV file path [default: data/training.csv]:
Result   CSV file path [default: data/result.csv]:
Number of assets to choose (k) [default: 25]:
Number of trials per combination (n) [default: 1000]:
Parallel? (1 = yes, 0 = no) [default: 1]:
```
## 6. Expected Output & Benchmarks
```
⏳ Running simulation over 25-asset portfolios (50 trials each)...

✔️  Best Sharpe portfolio found:
    Sharpe Ratio : 0.19133890490130517
    Assets / Weights:
      AAPL  ->  6.61426573572841e-2
      AMGN  ->  5.7662123184381936e-2
      AXP  ->  6.049365088186588e-2
      BA  ->  2.9881444099959686e-2
      CAT  ->  1.6502922939016243e-2
      CRM  ->  4.377581824715346e-2
      CSCO  ->  6.636225408207679e-2
      CVX  ->  1.4271888618188582e-2
      DIS  ->  5.4044253860645264e-2
      DOW  ->  6.60192530454152e-2
      GS  ->  6.763640964414544e-2
      HD  ->  4.833322115709522e-2
      HON  ->  2.7758743908091885e-2
      IBM  ->  5.345353976300215e-2
      INTC  ->  1.387311090131029e-2
      JNJ  ->  2.5924101900068552e-2
      JPM  ->  5.576292711529842e-2
      KO  ->  4.686583069822723e-2
      MCD  ->  3.1999587813132256e-3
      MMM  ->  2.117927048681058e-2
      MRK  ->  1.36148190273245e-3
      MSFT  ->  4.391043299696419e-3
      NKE  ->  4.485089765131787e-2
      PG  ->  5.0749493832663126e-2
      TRV  ->  5.95037026422398e-2

⏱️  Optimization Elapsed time: 22.77 seconds

🔁 Evaluating same portfolio on result dataset...
✅ New Sharpe Ratio on result dataset: -2.3305575327931777e-2
😕 Worse or equal Sharpe in result dataset.
```
## 7. Results

| Cores  | **Best Sharpe Ratio (2024_2nd_Semester)** | **Optimization Time (s)** | **Best Sharpe Ratio (2025_1st_Trimester)** | **Comparison**                          |
| -------------- | ----------------------- | ------------------------- | --------------------------------- | --------------------------------------- |
| 4  | 0.31707951483423114     | 162.4                     | 4.324464935386083e-3              | Worse Sharpe in Trimester |
|  4  | 0.33898489256815745     | 170.43                    | -1.7167632267105295e-2            | Worse Sharpe in Trimester|
|  8  | 0.29796031386257094     | 112.63                    | -1.7389490189320743e-4            | Worse Sharpe in Trimester|
| 8 | 0.36091653571516474     | 113.99                    | -3.790277278120213e-2            | Worse Sharpe in Trimester|

### Key Observations:
- **Best Sharpe Ratio (2024)**: Sharpe ratios range between **0.2979** and **0.3609**, indicating strong portfolio selection during training.
- **Sharpe Ratio (2025)**: Significant drop in Sharpe ratios for the test period (from **4.32e-3** to **-3.79e-2**), indicating poor generalization.
- **Optimization Time**: Optimization time improves with 8 cores (**113 seconds**) vs. 4 cores (**162-170 seconds**), showing efficient parallel scaling.

### Optimal Portfolio Visualization:
The chart below shows the **optimal portfolio weights** based on the highest Sharpe ratio of **0.3609** found during the simulation:

![Optimal Portfolio Weights](img/optimal_portfolio_weights.png)

## 8. AI Usage Disclosure

* Creating and Optimizing Functions
* Writing Readme
* Cabal Structure
* Commenting Code

