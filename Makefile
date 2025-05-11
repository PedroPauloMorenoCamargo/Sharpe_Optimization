.PHONY: python-setup download-data clean-data clean-env clean-all

# Default dates
TRAIN_START ?= 2024-07-01
TRAIN_END   ?= 2024-12-31
TEST_START  ?= 2025-01-01
TEST_END    ?= 2025-03-31


# Create virtualenv and install dependencies
python-setup:
	@python3 -m venv .venv
	@.venv/bin/pip install --upgrade pip
	@.venv/bin/pip install -r requirements.txt

# Download data using the venv's python
download-data:
	@mkdir -p data
	@.venv/bin/python data/download_data.py $(TRAIN_START) $(TRAIN_END) $(TEST_START) $(TEST_END)

# Clean data only
clean-data:
	rm -f data/*.csv

# Clean virtual environment only
clean-env:
	rm -rf .venv

# Clean everything
clean-all: clean-data clean-env
