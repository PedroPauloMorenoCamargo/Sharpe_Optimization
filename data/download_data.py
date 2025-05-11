# data/download_data.py
import yfinance as yf
import pandas as pd
import sys

if len(sys.argv) != 5:
    print("Uso: python download_data.py <train_start> <train_end> <test_start> <test_end>")
    sys.exit(1)

train_start, train_end, test_start, test_end = sys.argv[1:]

tickers = [
    "AAPL", "AMGN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS", "DOW",
    "GS", "HD", "HON", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
    "MRK", "MSFT", "NKE", "PG", "TRV", "UNH", "V", "VZ", "WBA", "WMT"
]

periods = {
    "data/training.csv": (train_start, train_end),
    "data/result.csv": (test_start, test_end),
}

for filename, (start, end) in periods.items():
    print(f"Baixando período {start} a {end}...")
    data = yf.download(tickers, start=start, end=end)["Close"]
    data.to_csv(filename)
    print(f"Salvo: {filename}")
print("Download concluído.")