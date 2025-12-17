#!/usr/bin/env python3
"""
Plot results from Fortran calculation
Creates a log-scale plot of n_prz_kmode.txt data
"""

import sys
import os
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt

def plot_n_prz_kmode(data_file, output_file):
    """
    Plot n_prz_kmode.txt with log scale on y-axis
    
    Args:
        data_file: Path to n_prz_kmode.txt
        output_file: Path to save the plot image
    """
    try:
        # Read data from file
        # Expected format: column 1 = k mode (x values), column 2 = n_prz (y values)
        # This matches gnuplot: p "n_prz_kmode.txt" u 1:2 w l
        data = np.loadtxt(data_file)
        
        if data.ndim == 1:
            # If only one row, reshape
            data = data.reshape(1, -1)
        
        # Extract columns: column 1 = k mode, column 2 = n_prz
        x = data[:, 0]  # k mode
        y = data[:, 1]  # n_prz
        
        # Filter out invalid values (zeros, negatives, NaN, inf)
        valid_mask = (y > 0) & np.isfinite(y) & np.isfinite(x)
        x = x[valid_mask]
        y = y[valid_mask]
        
        if len(x) == 0:
            print("Error: No valid data points to plot", file=sys.stderr)
            return False
        
        # Create plot with log scale on y-axis (matching gnuplot: set logscale y)
        plt.figure(figsize=(8, 5))
        plt.semilogy(x, y, color='teal', linewidth=2)
        plt.xlabel('$N(efold)$', fontsize=14, fontweight='bold')
        plt.ylabel('$P_R$', fontsize=14, fontweight='bold')
        plt.title('Power Spectrum', fontsize=16, fontweight='bold')
        plt.grid(True, which='both', linestyle='--', alpha=0.7)
        plt.legend(fontsize=12)
        plt.tight_layout()
        
        # Save plot with high quality
        plt.savefig(output_file, dpi=150, bbox_inches='tight', facecolor='white')
        plt.close()
        
        print(f"Plot saved to: {output_file}")
        print(f"Plotted {len(x)} data points")
        return True
        
    except FileNotFoundError:
        print(f"Error: Data file not found: {data_file}", file=sys.stderr)
        return False
    except Exception as e:
        print(f"Error plotting data: {e}", file=sys.stderr)
        return False

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python plot_results.py <data_file> <output_image>", file=sys.stderr)
        sys.exit(1)
    
    data_file = sys.argv[1]
    output_file = sys.argv[2]
    
    success = plot_n_prz_kmode(data_file, output_file)
    sys.exit(0 if success else 1)

