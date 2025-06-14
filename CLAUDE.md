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

### Current Migration
The repository is undergoing a Helm to Vertico/Consult migration:
- `10-counsel.el.bak` contains the old Ivy/Counsel configuration
- `10-vertico.el` implements the new completion system with Consult, Orderless, Marginalia, and Embark

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
- Emacs 27+
- hunspell (spell checking)
- ag/rg/ack (search tools)
- ghq (repository management)
- git
- cmigemo (Japanese search)
- direnv
- cmake

## Key Integrations

- **Projectile + Perspective**: Project and workspace management
- **Eglot**: LSP client for programming languages
- **Company-mode**: Auto-completion
- **Magit**: Git interface
- **YASnippet**: Template system with custom snippets in `snippets/`
- **Treemacs**: File tree sidebar
- **Hydra**: Modal command interfaces

## Important Configuration Details

- Font configuration expects Microsoft Consolas for ASCII and Google Noto for Japanese
- Spell checking requires DICPATH and DICTIONARY environment variables on Windows
- The configuration supports Windows, Linux, and macOS
- Japanese input and search capabilities through migemo