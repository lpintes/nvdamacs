# nvdamacs - NVDA Integration for Emacs

## Project Overview

This project provides integration between Emacs and the NVDA screen reader. It consists of two parts:
- **Emacs Lisp**: JSON-RPC server exposing Emacs text navigation
- **Python NVDA addon**: Client that communicates with Emacs via TCP

## Architecture

```
Emacs (nvda-mode)  <--[JSON-RPC/TCP]-->  NVDA (addon/appModules/emacs.py)
     ^                                              |
     |                                              v
  Port file: ~/.emacs.d/.nvda-server-port    NVDA speech output
```

## Module Structure (nvda/)

The Emacs Lisp code is organized into 6 modules:

### 1. nvda-textinfo.el
**Purpose**: TextInfo API implementation
**When to edit**: Adding new text navigation methods, offset calculations, or buffer inspection functions

Key functions:
- `nvda--get-*-offsets` - Get text unit boundaries (line/word/sentence/paragraph/page)
- `nvda--get-text-range` - Extract text between offsets
- `nvda--minibuffer-*` - Minibuffer-specific text access

### 2. nvda-server.el
**Purpose**: TCP server and JSON-RPC infrastructure
**When to edit**: Rarely - only for protocol changes or server lifecycle

Key components:
- Server startup/shutdown
- JSON-RPC message encoding/decoding
- Method dispatch table (`nvda--method-handlers`)
- Debug logging system

### 3. nvda-speech.el
**Purpose**: Speech output and event system
**When to edit**: Adding new speaking commands or reading functions

Key functions:
- `nvda-speak` - Basic speech output
- `nvda-speak-*` - Reading commands (character/word/line/sentence/paragraph/buffer/etc.)
- `nvda-speak-*-info` - Informative commands (buffer info, position, mode line)

### 4. nvda-hooks.el
**Purpose**: Hooks and advice for automatic speech
**When to edit**: Changing automatic speech behavior, adding new hooks

Key components:
- Message interception (`nvda--advice-message`)
- Auto-speak insertions tracking
- Delete character feedback
- Comint mode support
- Auto-speak buffers (Help, Completions)

### 5. nvda-commands.el
**Purpose**: Command registration and navigation bindings
**When to edit**: Adding speech handlers for new navigation commands

Key components:
- `nvda-on-command` / `nvda-on-commands` macros
- Navigation command speech bindings (forward-char, next-line, etc.)
- Unhandled command announcement

### 6. nvda-mode.el
**Purpose**: Main entry point, customization, keymaps
**When to edit**: Adding new keybindings, customization options, or changing mode behavior

Key components:
- Customization variables (`defcustom`)
- `nvda-speak-map` keymap (C-e prefix)
- Minor mode definition with hook registration

## Development Workflow

### Quick reload during development:
```elisp
;; After editing a module:
M-x load-file RET ~/.emacs.d/nvda/nvda-speech.el RET

;; Or if file is open:
M-x eval-buffer
```

### Copy to Emacs:
```bash
./c  # Copies addon/*.py to NVDA and nvda/ to ~/.emacs.d/
```

### Server restart (if needed):
```elisp
M-x nvda-mode  ; disable
M-x nvda-mode  ; enable (restarts server)
```

## Common Tasks

| Task | Module to edit |
|------|---------------|
| Add new speaking command | nvda-speech.el |
| Add speech for navigation command | nvda-commands.el |
| Change insertion announcement | nvda-hooks.el |
| Add new TextInfo method | nvda-textinfo.el → nvda-server.el (method handlers) |
| Add new keybinding | nvda-mode.el |
| Change server protocol | nvda-server.el + addon/appModules/*.py |

## Python Side

- `addon/appModules/emacs.py` - NVDA app module for Emacs integration
- `addon/appModules/rpc.py` - JSON-RPC client implementation

## Dependencies

- Emacs 24.4+ (for `lexical-binding`)
- NVDA (for speech output)
- Built-in: bindat, json

## Module Dependencies

```
nvda-textinfo.el (no dependencies)
     ↓
nvda-server.el (requires: nvda-textinfo)
     ↓
nvda-speech.el (requires: nvda-server, nvda-textinfo)
     ↓
nvda-hooks.el (requires: nvda-speech, nvda-server)
nvda-commands.el (requires: nvda-speech)
     ↓
nvda-mode.el (requires: all above)
```
