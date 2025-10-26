#!/bin/bash

# checks.sh - Run all quality checks on Emacs Lisp files
# Usage: ./checks.sh

set -e  # Exit on error

echo "=== ORG-SOCIAL.EL QUALITY CHECKS ==="
echo ""

# Track overall status
OVERALL_STATUS=0

# 1. Run checkdoc
echo "[1/5] Running checkdoc..."
checkdoc_file() {
    local file="$1"
    echo "  Checking $file..."
    emacs --batch --eval "(progn (require 'checkdoc) (checkdoc-file \"$file\"))" 2>&1
}

# Check all org-social*.el files
for file in org-social*.el; do
    [ -f "$file" ] && checkdoc_file "$file"
done

# Check ui/*.el files
for file in ui/*.el; do
    [ -f "$file" ] && checkdoc_file "$file"
done

# Check ui/buffers/*.el files
for file in ui/buffers/*.el; do
    [ -f "$file" ] && checkdoc_file "$file"
done

echo "✓ Checkdoc completed"
echo ""

# 2. Run package-lint
echo "[2/5] Running package-lint..."

# Only lint the main file (org-social.el) as it contains Package-Requires
echo "  Linting org-social.el (main file)..."
emacs --batch \
      --eval "(require 'package)" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
      --eval "(package-initialize)" \
      --eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
      --eval "(require 'package-lint)" \
      -f package-lint-batch-and-exit org-social.el 2>&1

echo "✓ Package-lint completed"

echo ""

# 3. Run melpazoid
echo "[3/5] Running melpazoid..."

if command -v docker >/dev/null 2>&1 && command -v python3 >/dev/null 2>&1; then
    if [ ! -d "melpazoid" ]; then
        echo "  Cloning melpazoid..."
        git clone https://github.com/riscy/melpazoid.git 2>&1 | grep -v "Cloning" || true
    fi

    echo "  Running melpazoid checks with MELPA recipe..."
    RECIPE='(org-social :fetcher github :repo "tanrax/org-social.el" :files (:defaults "ui/*.el" "ui/buffers/*.el"))'
    # Use LOCAL_REPO to test local files instead of cloning
    if RECIPE="$RECIPE" LOCAL_REPO="$(pwd)" make -C melpazoid 2>&1; then
        echo "✓ Melpazoid completed"
    else
        echo "⚠ Melpazoid had warnings or errors"
        OVERALL_STATUS=1
    fi
else
    if ! command -v docker >/dev/null 2>&1; then
        echo "⚠ docker not found, skipping melpazoid"
    elif ! command -v python3 >/dev/null 2>&1; then
        echo "⚠ python3 not found, skipping melpazoid"
    fi
fi

echo ""

# 4. Format all files
echo "[4/5] Formatting all Emacs Lisp files..."
format_file() {
    local file="$1"
    echo "  Formatting $file..."
    if emacs --batch "$file" --eval "(progn (emacs-lisp-mode) (indent-region (point-min) (point-max)) (save-buffer))" 2>&1 | grep -q "error"; then
        echo "  ✗ Failed to format $file"
        OVERALL_STATUS=1
    else
        echo "  ✓ Formatted $file"
    fi
}

# Format all org-social*.el files
for file in org-social*.el; do
    [ -f "$file" ] && format_file "$file"
done

# Format ui/*.el files
for file in ui/*.el; do
    [ -f "$file" ] && format_file "$file"
done

# Format ui/buffers/*.el files
for file in ui/buffers/*.el; do
    [ -f "$file" ] && format_file "$file"
done

echo ""

# 5. Compile all files
echo "[5/5] Compiling all Emacs Lisp files..."
compile_file() {
    local file="$1"
    echo "  Compiling $file..."
    if emacs --batch -L . -L ui -L ui/buffers --eval "(setq byte-compile-error-on-warn nil)" -f batch-byte-compile "$file" 2>&1 | grep -i "error\|warning" | grep -v "Loading"; then
        echo "  ⚠ Compilation warnings/errors in $file"
        OVERALL_STATUS=1
    else
        echo "  ✓ Compiled $file"
    fi
}

# Compile all org-social*.el files
for file in org-social*.el; do
    [ -f "$file" ] && compile_file "$file"
done

# Compile ui/*.el files
for file in ui/*.el; do
    [ -f "$file" ] && compile_file "$file"
done

# Compile ui/buffers/*.el files
for file in ui/buffers/*.el; do
    [ -f "$file" ] && compile_file "$file"
done

echo ""
echo "=== ALL CHECKS COMPLETED ==="

if [ $OVERALL_STATUS -eq 0 ]; then
    echo "✓ All checks passed!"
else
    echo "✗ Some checks failed or had warnings"
fi

exit $OVERALL_STATUS
