#!/bin/bash

# Define paths
INPUT_FILE="R/etn_owf_data_exploration.qmd"
OUTPUT_DIR="./../docs"

# Check if the input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: File '$INPUT_FILE' not found."
    exit 1
fi

# Ensure the output directory exists
mkdir -p "$OUTPUT_DIR"

# Render the file
echo "Rendering $INPUT_FILE to $OUTPUT_DIR..."
quarto render "$INPUT_FILE" --output-dir "$OUTPUT_DIR"

# Check if rendering was successful
if [ $? -eq 0 ]; then
    echo "Success! Output saved to $OUTPUT_DIR/etn_owf_data_exploration.html"
else
    echo "Error: Rendering failed."
    exit 1
fi

#2. Make it Executable
# Run this command once in your terminal to give the script permission to run:
# 
# chmod +x render_quarto.sh
# 
# 3. How to Run It
# Simply execute the script from your root directory:
# 
# ./render_quarto.sh