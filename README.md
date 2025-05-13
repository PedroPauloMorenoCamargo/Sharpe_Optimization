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

Extras implemented for **bonus marks**:

* ⬇️ Data fetched **on‑demand** via python script that uses `yfinance` lib.
* 🔁 Sharpe re‑evaluated on first trimester of **2025**, test set.
* 🚦 Benchmarks include **serial vs parallel**.

---

## 1  Objective

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

# 2  Why functional? Why parallel?

* **Purity** — deterministic functions allow local reasoning and automatic memoisation.
* **Parallel safety** — no locks, no races.

Parallelism strategy:

* Split the combination list into `numCapabilities × 4` chunks.
* Evaluate each chunk with `rdeepseq` so every Sharpe value is fully reduced on its worker.
* Results are folded to a single `(Sharpe, names, weights)` triple.

---

## 3  Project layout

```text
app/Main.hs                  CLI, IO & wall‑clock timing

src/SharpeOptimization/
├── DataLoader.hs            Robust CSV ingestion, validation (ExceptT IO)
├── Statistics.hs            Pure: prices→returns, μ, Σ, Sharpe_fast, vector algebra
├── Weights.hs               Rejection‑sampled Dirichlet ≤ 20 %
└── Simulate.hs              Sequential & parallel search cores

data/
└── download_data.py         Pulls DJIA closes via yfinance  # optional
Makefile                     Turn‑key venv + data + clean targets
```

## 4  Installation

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

## 5  Running the simulation

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
## 6  Expected output & benchmarks
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
## 7 Results

| Cores  | **Best Sharpe Ratio (2024_2nd_Semester)** | **Optimization Time (s)** | **Best Sharpe Ratio (2025_1st_Trimester)** | **Comparison**                          |
| -------------- | ----------------------- | ------------------------- | --------------------------------- | --------------------------------------- |
| 4  | 0.31707951483423114     | 162.4                     | 4.324464935386083e-3              | Worse or equal Sharpe in result dataset |
|  4  | 0.33898489256815745     | 170.43                    | -1.7167632267105295e-2            | Worse or equal Sharpe in result dataset |
|  8  | 0.29796031386257094     | 112.63                    | -1.7389490189320743e-4            | Worse or equal Sharpe in result dataset |
| 8 | 0.36091653571516474     | 113.99                    | -3.790277278120213e-2             | Worse or equal Sharpe in result dataset |



## 8  AI usage disclosure

* Creating and Optimizing Functions;
* Writing Readme
* Cabal Structure

