# Modern Emacs Configuration

A comprehensive, modular Emacs configuration optimized for modern software development with multi-language LSP support.

## 🎯 Target Environment

- **Emacs**: 29+ (with native compilation support)
- **Platforms**: macOS, Linux, Windows
- **Package Manager**: straight.el + use-package
- **Configuration Loader**: init-loader (modular file organization)

## 🚀 Key Features

### Multi-Language LSP Orchestration
- **Go**: gopls + goimports-reviser + gofumpt formatting chain
- **TypeScript/React**: tsserver + prettier with pnpm workspace support
- **Python**: pylsp + uv project integration
- **Protobuf**: buf-language-server + buf toolchain
- **GraphQL**: graphql-lsp + codegen integration

### Modern Development Tools
- **Tree-sitter**: Fast, accurate syntax highlighting for all languages
- **Eglot**: Built-in LSP client with comprehensive language support
- **Vertico/Consult**: Modern completion framework (migrated from Helm)
- **Projectile**: Advanced project management
- **Magit**: Git interface
- **Treemacs**: File tree sidebar
- **vterm**: Fast terminal emulator

### Enhanced Editing
- **Company**: Intelligent completion
- **YASnippet**: Template system with custom snippets
- **Hydra**: Modal command interfaces
- **Multiple cursors**: Multi-cursor editing
- **Smart parens**: Balanced parentheses editing

## 📁 Configuration Structure

```
~/.emacs.d/
├── init.el                 # Main entry point
├── CLAUDE.md              # AI assistant instructions
├── inits/                 # Modular configuration files
│   ├── 00-09/            # Core Emacs settings
│   ├── 10-19/            # Navigation and completion
│   ├── 20-29/            # Project management and editing
│   ├── 30-39/            # Additional editing features
│   ├── 50-59/            # Programming language support
│   ├── 60-69/            # Utilities
│   └── 80-89/            # Major modes (Org, vterm)
├── snippets/              # YASnippet templates
└── elisp/                # Local elisp files (minimal)
```

## 🛠 Prerequisites

### Core Dependencies
```bash
# Package managers and tools
brew install git ghq direnv cmake

# Search tools
brew install ripgrep the_silver_searcher ack

# Spell checking
brew install hunspell

# Japanese input support
brew install cmigemo
```

### Language-Specific Tools

#### Go Development
```bash
go install golang.org/x/tools/gopls@latest
go install github.com/incu6us/goimports-reviser/v3@latest
go install mvdan.cc/gofumpt@latest
```

#### TypeScript/JavaScript
```bash
npm install -g typescript-language-server prettier pnpm
```

#### Python
```bash
pip install python-lsp-server[all] uv
```

#### Protobuf
```bash
brew install buf
```

#### GraphQL
```bash
npm install -g @graphql-lsp/cli graphql-language-service-cli
```

## 🎨 Fonts

### ASCII Font
- **Microsoft Consolas** (recommended)
- Download: [Microsoft Typography](https://www.microsoft.com/en-us/download/details.aspx?id=17879)

### Japanese Font
- **Google Noto Sans CJK JP**
- Download: [Google Noto Fonts](https://www.google.com/get/noto/#sans-jpan)

## ⚙️ Environment Variables

### Windows-Specific
```bash
# Spell checking
set DICPATH=C:\path\to\hunspell\share\hunspell
set DICTIONARY=en_US

# Add to PATH
set PATH=%PATH%;%HOME%\.emacs.d\hunspell\bin
```

### macOS/Linux
Environment variables are automatically configured through the configuration.

## 🚀 Installation

1. **Clone the repository**:
   ```bash
   git clone <your-repo-url> ~/.emacs.d
   ```

2. **Install external dependencies** (see Prerequisites section)

3. **Start Emacs**: The configuration will automatically install packages on first run

4. **Install Tree-sitter grammars**:
   ```
   M-x treesit-auto-install-all
   ```

## 🔧 Key Bindings

### Navigation
- `C-;` - Flycheck hydra (syntax checking)
- `C-:` - Flymake hydra (diagnostics)
- `M-SPC` - Major mode hydra
- `C-c p` - Projectile commands

### Completion
- Uses Vertico/Consult for modern completion interface
- `C-x b` - Switch buffer with preview
- `C-c f` - Find files in project

### LSP Integration
- `M-.` - Go to definition
- `M-?` - Find references
- `C-c C-r` - Rename symbol
- `C-c C-a` - Code actions

## 🏗 Architecture Highlights

### Package Management
- **straight.el**: Git-based package manager for reproducible builds
- **use-package**: Declarative package configuration
- All packages pinned to specific commits for stability

### Performance Optimizations
- Native compilation enabled (Emacs 28+)
- Garbage collection tuning for faster startup
- Lazy loading for non-essential packages
- Tree-sitter for efficient syntax highlighting

### Multi-Language Support
- Unified LSP configuration across all languages
- Language-specific formatting and build tool integration
- Schema-driven development for Protobuf and GraphQL

## 🔍 Error Handling & Debugging

### Claude Code Integration
The configuration includes experimental Claude Code integration:
- `e` in flymake hydra - Explain current error
- `f` in flymake hydra - Request error fix
- Error context automatically gathered and formatted

### Debug Mode
```elisp
M-x toggle-debug-on-error
```

## 📚 Development Workflow

### Testing Configuration Changes
```elisp
M-x eval-buffer          ; Reload current file
M-x init-loader-load     ; Reload all configuration
```

### Package Management
```elisp
M-x straight-pull-all    ; Update all packages
M-x straight-rebuild-all ; Rebuild packages
```

## 🤝 Contributing

This is a personal configuration, but contributions and suggestions are welcome:

1. Follow the existing file organization pattern
2. Use `use-package` for all package configurations
3. Add appropriate documentation for new features
4. Test on multiple platforms when possible

## 📄 License

Personal configuration - use and modify as needed.

---

*Generated and maintained with Claude Code assistance*