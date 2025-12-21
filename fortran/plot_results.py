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
        # Expected format from Fortran: write(93,*) t(k) +n_back,prk(k),ks_norm
        # Column 0 = N(efold) = t(k) + n_back (x-axis)
        # Column 1 = P_R = prk(k) (y-axis)
        # Column 2 = ks_norm (k mode, not used for this plot)
        print(f"Reading data from: {data_file}", file=sys.stderr)
        data = np.loadtxt(data_file)
        
        if data.ndim == 1:
            # If only one row, reshape
            data = data.reshape(1, -1)
        
        print(f"Data shape: {data.shape}, Number of rows: {len(data)}", file=sys.stderr)
        
        # Extract columns: column 0 = N(efold), column 1 = P_R
        x = data[:, 0]  # N(efold) - x-axis
        y = data[:, 1]  # P_R - y-axis
        
        print(f"X range: [{np.min(x):.2f}, {np.max(x):.2f}]", file=sys.stderr)
        print(f"Y range: [{np.min(y):.6e}, {np.max(y):.6e}]", file=sys.stderr)
        
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

