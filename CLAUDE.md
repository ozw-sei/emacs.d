# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a modular Emacs configuration repository using straight.el for package management and init-loader for organized configuration loading.

## Key Architecture

### Configuration Loading Pattern
- `init.el`: Main entry point that bootstraps straight.el and init-loader
- `inits/`: Numbered configuration files loaded in order:
  - 00-09: Core Emacs settings and fundamentals
  - 10-19: Navigation and completion frameworks (currently migrating from Helm to Vertico)
  - 20-29: Project management and editing tools
  - 30-39: Additional editing features
  - 50-59: Programming language support
  - 60-69: Utilities
  - 80-89: Major modes (Org, vterm)
  - 99: Global keybindings



### Package Management
- **straight.el**: Git-based package manager (packages cloned to `straight/`)
- **use-package**: Declarative package configuration
- Local elisp files stored in `elisp/` directory

### Current Migration Status
The repository has completed its migration from Helm to Vertico/Consult:
- **Completion Framework**: Vertico + Consult + Orderless + Marginalia + Embark (in `10-vertico.el`)
- **Completion UI**: Migrated from Company-mode to Corfu + Cape (in `20-corfu.el`)
- **Prescient Integration**: Smart sorting with `11-prescient.el` for history-based completion
- **Undo System**: Transitioning from built-in undo to undo-fu (see `10-undo-fu.el.example`)

## Development Commands

This is a personal Emacs configuration without build/test commands. Common development tasks:

```elisp
;; Reload configuration
M-x eval-buffer (in init.el)
M-x init-loader-load

;; Update packages
M-x straight-pull-all
M-x straight-rebuild-all

;; Check for errors
M-x toggle-debug-on-error
```

## Dependencies

External tools required (from README.org):
- Emacs 26+ (minimum version enforced in init.el)
- hunspell (spell checking)
- ag/rg/ack (search tools)
- ghq (repository management)  
- git
- cmigemo (Japanese search - optional, migemo integration commented out)
- direnv
- cmake

## Key Integrations

- **Projectile + Perspective**: Project and workspace management
- **Eglot**: LSP client for programming languages
- **Corfu + Cape**: Modern completion-at-point UI with extensions
- **Magit**: Git interface
- **YASnippet**: Template system with custom snippets in `snippets/`
- **Treemacs**: File tree sidebar
- **Hydra**: Modal command interfaces

## Important Configuration Details

- Font configuration expects Microsoft Consolas for ASCII and Google Noto for Japanese
- Spell checking requires DICPATH and DICTIONARY environment variables on Windows  
- The configuration supports Windows, Linux, and macOS
- Japanese input and search capabilities through migemo (currently commented out)
- Terminal support: Corfu includes terminal-mode compatibility for non-GUI usage
- Configuration persists history in `savehist` and prescient files for intelligent completion ordering

## File Organization Patterns

When making changes:
- Core functionality files (00-09): Basic Emacs behavior, paths, fonts
- Navigation/completion (10-19): Vertico, Corfu, undo systems, prescient
- Project tools (20-29): Projectile, LSP (eglot), version control integration  
- Editing enhancements (30-39): Text manipulation, navigation aids, visual improvements
- Language support (50-59): Major modes and language-specific configurations
- Utilities (60-69): Helper tools like quickrun, REST client
- Major applications (80-89): Org-mode, terminal emulation
