# Sharpe Optimization Project (Pedro Paulo Moreno Camargo)

---

## 1. Assignmen

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
