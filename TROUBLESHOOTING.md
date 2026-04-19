# Troubleshooting

This document captures solutions to issues encountered with this Emacs configuration.

## GPG File Encoding Prompts (2026-04)

### Problem
Opening `.gpg` files in Emacs showed repeated "Select coding system" prompts, even though the decrypted content was correct and displayed properly.

### Symptoms
- Interactive Emacs (GUI or terminal) showed encoding selection prompts
- Batch mode worked fine (`emacs --batch`)
- `emacs -Q` (no config) worked fine - files opened cleanly
- Issue occurred across different Emacs versions and platforms (macOS, Linux)
- Content was actually correct - no byte corruption
- Command-line GPG decryption worked perfectly (`gpg -d file.gpg`)

### Root Cause
**git-gutter** configuration with aggressive update hooks was triggering `select-safe-coding-system` calls on GPG files.

Specifically, these hooks in `config/init-version-control.el`:
```elisp
:hook
((focus-in . git-gutter:update-all-windows)
 (after-save . git-gutter:update-all-windows)
 (magit-post-refresh . git-gutter:update-all-windows)
 (window-configuration-change . git-gutter:update-all-windows)
 (find-file . lgreen/setup-git-gutter-if-in-vcs))
```

The `find-file` and `window-configuration-change` hooks were particularly problematic, causing git-gutter to run operations that triggered encoding detection on encrypted files.

### Solution
Simplified git-gutter configuration to use just the default mode without aggressive update hooks.

**File:** `config/init-version-control.el`

Removed the problematic `:hook` section and the custom `lgreen/setup-git-gutter-if-in-vcs` function. Kept visual customizations and theme integration which work fine:

```elisp
(use-package git-gutter
  :after general
  :custom
  ;; Visual customizations (safe)
  (git-gutter:update-interval 0.5)
  (git-gutter:window-width 2)
  (git-gutter:modified-sign "┃")
  (git-gutter:added-sign "┃")
  (git-gutter:deleted-sign "▁")
  (git-gutter:hide-gutter t)
  :init
  ;; Keybindings
  (lgreen/leader-define-key
    "g p" '(git-gutter:previous-hunk :wk "previous hunk")
    "g n" '(git-gutter:next-hunk :wk "next hunk")
    "g r" '(git-gutter:revert-hunk :wk "revert hunk")
    "g s" '(git-gutter:stage-hunk :wk "stage hunk")
    "t g" '(global-git-gutter-mode :wk "toggle git-gutter")
    "t G" '(git-gutter :wk "refresh git-gutter"))
  :config
  (global-git-gutter-mode 1)

  ;; Theme integration (safe)
  (defun lgreen/update-git-gutter-colors ()
    "Update git-gutter colors to match current theme."
    (set-face-foreground 'git-gutter:modified (face-foreground 'diff-changed))
    (set-face-foreground 'git-gutter:added (face-foreground 'diff-added))
    (set-face-foreground 'git-gutter:deleted (face-foreground 'diff-removed)))

  (lgreen/update-git-gutter-colors)
  (add-hook 'after-load-theme-hook #'lgreen/update-git-gutter-colors))
```

Git-gutter still works perfectly with `global-git-gutter-mode`, just without the aggressive hooks that caused issues with GPG files.

### Debugging Methodology

The issue took weeks to solve. Key lessons learned:

1. **Start with `emacs -Q`**: Always test with no config first to isolate user config vs Emacs core issues
   ```bash
   emacs -Q /path/to/problem-file.gpg
   ```

2. **Binary search config**: Comment out half your requires in `init.el`, test, repeat until you find the culprit module
   - Used `emacs -Q` baseline to confirm it wasn't an Emacs bug
   - Systematically disabled config modules until prompt disappeared
   - Found: worked without `init-version-control.el`
   - Then bisected within that file to find git-gutter

3. **Use proper tools**:
   - Use `rg "pattern" ~/.emacs.d/config/` instead of `grep -r` for faster, gitignore-aware searches
   - Use `ps aux | grep -i emacs` to find actual process names before killing
   - Found process was `Emacs` (capital E) not `emacs`, so `pkill emacs` was failing

4. **Launch GUI Emacs correctly on macOS**:
   ```bash
   # Wrong - causes input issues
   /path/to/Emacs.app/Contents/MacOS/Emacs file.gpg &

   # Correct - use open command
   open -a /path/to/Emacs.app --args file.gpg
   ```

5. **Red herrings exist**:
   - Initially suspected undo-tree (it calls `sha1` which calls `select-safe-coding-system`)
   - Suspected EPA/EPG encryption handling
   - Suspected GPG version differences
   - All were actually fine - the real issue was git-gutter hooks

6. **Trust simple tests**: If command-line `gpg -d file.gpg` works and shows correct content, the encryption/decryption/content is fine

7. **Test incrementally**: When simplifying config, add back customizations piece by piece:
   - Default config worked ✓
   - + Visual customizations worked ✓
   - + Theme integration worked ✓
   - + Hooks = FAILED ✗

### Timeline
- Issue first noticed: Multiple weeks before resolution, intermittent occurrence
- Systematic debugging session: 2026-04-19
- Root cause identified: git-gutter hooks in `init-version-control.el`
- Solution applied: Removed aggressive update hooks, kept visual customizations

### Why This Was Rare
This specific issue combination was uncommon because it required:
1. Using git-gutter (many use diff-hl, magit's built-in diff, or built-in VC)
2. Having aggressive custom update hooks (not the default git-gutter config)
3. Opening GPG files (less common use case)
4. Having the GPG files in a git repository (common for dotfiles/notes, less common generally)

Most users either:
- Use default git-gutter config (which works fine with GPG files)
- Don't customize with aggressive update hooks
- Don't use git-gutter at all
- Don't work with GPG encrypted files regularly
- Store sensitive GPG files outside git repos

### Technical Details

The problematic hooks caused this call chain:
```
find-file (opening .gpg file)
  → lgreen/setup-git-gutter-if-in-vcs
    → git-gutter operations
      → buffer analysis/hashing
        → select-safe-coding-system with accept-default-p=nil
          → Interactive prompt "Select coding system"
```

The `accept-default-p=nil` parameter forces an interactive prompt even when the content is valid UTF-8. This was unnecessary because EPA had already correctly decrypted and decoded the file.

### Alternative Solutions (Not Used)

Other approaches that would have worked but were more invasive:

1. **Disable git-gutter entirely** - too heavy-handed, loses useful feature
2. **Disable undo-tree for GPG files** - didn't fix it (not the root cause)
3. **Advice `select-safe-coding-system` to auto-accept for GPG files** - treats symptom not cause
4. **Disable git-gutter only for GPG files** - more complex config, unnecessary

The chosen solution (simplify git-gutter config) was cleanest: removes the problem while keeping all desired functionality.

### Environment
- Emacs: emacs-mac-exp@31 (HEAD-f7a1f58) via Homebrew
- Platform: macOS (Darwin 25.4.0)
- Package manager: Elpaca
- Affected files: Any `.gpg` files in git repositories
- Test file: `~/dev/my/org/work/private.org.gpg`

### Related Configuration Files
- `init.el` - Main init file with module requires
- `config/init-version-control.el` - Git-gutter and magit configuration (where fix was applied)
- `config/init-undo.el` - Undo-tree configuration (initially suspected, was innocent)

### See Also
- Git-gutter documentation: https://github.com/emacsorphanage/git-gutter
- EPA (EasyPG) documentation: https://www.gnu.org/software/emacs/manual/html_mono/epa.html
- Emacs debugging: https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugging.html
