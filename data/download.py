import yfinance as yf
import pandas as pd

tickers = [
    "AAPL", "AMGN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS", "DOW",
    "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
    "MRK", "MSFT", "NKE", "PG", "TRV", "UNH", "V", "VZ", "WBA", "WMT"
]

periods = {
    "dow_jones_2sem_2024.csv": ("2024-08-01", "2024-12-31"),
    "dow_jones_1tri_2025.csv": ("2025-01-01", "2025-03-31"),
}

for filename, (start, end) in periods.items():
    print(f"Baixando período {start} a {end}...")
    data = yf.download(tickers, start=start, end=end)["Close"]
    data.to_csv(filename)
    print(f"Salvo: {filename}")
