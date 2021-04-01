# Exosphere browser integration tests

## Setup

### 1. Install geckodriver

On MacOS:

```bash
brew install geckodriver 
export PATH=/Applications/Firefox.app/Contents/MacOS:$PATH
```

On Linux:

- Download geckodriver from [this page](https://github.com/mozilla/geckodriver/releases), the latest linux64.tar.gz
- Un-archive it (e.g. `tar -xvzf geckodriver-v0.28.0-linux64.tar.gz`)
- Make the binary executable (e.g. `chmod +x geckodriver`)
- Move the binary to a directory in your system path (e.g. `mv geckodriver /usr/local/bin/`)

### 2. Install Python dependencies

Do this in a Python virtual environment:

```bash
python3 -m pip install --requirement requirements.txt 
```

## Usage

Set your TACC credentials as environment variables:

```bash
read -p "Enter your TACC username: " taccusername
read -p "Enter your TACC password: " -s taccpass
export taccusername
export taccpass
```

Run all the Exosphere scenarios in order: 

```bash
behave features/exosphere.feature
```

Run selected Exosphere scenarios by tag:

```bash
behave --tags @add-allocation features/exosphere.feature
behave --tags @launch features/exosphere.feature
behave --tags @delete features/exosphere.feature
```

You can use a custom URL for Exosphere:

```bash
behave -D EXOSPHERE_BASE_URL=http://app.exosphere.localhost:8000 features/exosphere.feature 
```
