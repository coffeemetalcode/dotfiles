# gitbash.zsh

export SRC="/c/Users/coffe/src/dotfiles/zsh/windows"

source "$SRC/common.zsh"

# Test that you've reached the right source
export WINDOWS_ZSH="windows.zsh"

# Add winfetch to PATH
# export PATH="/c/tools/winfetch:$PATH"

# Create winfetch alias to call via PowerShell (NoProfile to avoid double-run)
alias winfetch='powershell.exe -ExecutionPolicy Bypass -NoProfile -File "C:\tools\winfetch\winfetch.ps1" 2>/dev/null'

# Auto-run winfetch on shell startup
if [[ -f "/c/tools/winfetch/winfetch.ps1" ]]; then
    winfetch
fi 